/*
USE master
*/


IF (OBJECT_ID(N'dbo.sp_ctrl3') IS NULL)
	--- Placeholder:
	EXEC(N'CREATE PROCEDURE dbo.sp_ctrl3 AS ---');
GO
/*

Copyright Daniel Hutmacher under Creative Commons 4.0 license with attribution.
http://creativecommons.org/licenses/by/4.0/

Source: https://github.com/sqlsunday/sp_ctrl3

DISCLAIMER: This script may not be suitable to run in a production
            environment. I cannot assume any responsibility regarding
            the accuracy of the output information, performance
            impacts on your server, or any other consequence. If
            your juristiction does not allow for this kind of
            waiver/disclaimer, or if you do not accept these terms,
            you are NOT allowed to store, distribute or use this
            code in any way.

USAGE:      EXECUTE sp_ctrl3 {object name}

SHORTCUT:   In SQL Server Management Studio, go to Tools -> Options
            -> Environment -> Keyboard -> Query Shortcuts.
            
            On a shortcut location of your choice, enter the following
            code, with the trailing space, without the quotes:
            "EXECUTE sp_ctrl3 ". To use, highlight the name of an object
            and press that keyboard shortcut. You may have to open a new
            query for the change to take effect. Also, objects denoted by
            schema (with a dot) need to be enclosed in quotes for this
            to work in older versions of SSMS.

VERSION:    2024-02-24

*/

ALTER PROCEDURE dbo.sp_ctrl3
	@objname	sysname
WITH EXECUTE AS CALLER
AS

SET NOCOUNT ON;
SET STATISTICS XML, TIME, IO OFF;
SET DEADLOCK_PRIORITY LOW;
SET LOCK_TIMEOUT 500;

DECLARE @object_id              int,
        @object_id_str          nvarchar(20),
        @type                   char(2),
        @database_id            int,
        @database               sysname,
		@compatibility_level	tinyint,
        @rowcount               bigint=0,
        @has_cols_or_params     bit,
        @has_indexes            bit,
	    @has_foreign_keys       bit,
	    @has_references         bit,
	    @has_permissions        bit,
	    @has_sql_module         bit,
	    @has_data               bit,
        @has_policies           bit,
		@is_azure_sql_db		bit=(CASE WHEN CAST(SERVERPROPERTY(N'Edition') AS varchar(100)) LIKE N'%Azure%' THEN 1 ELSE 0 END),
	    @is_tempdb              bit=0,
        @synonym_references     nvarchar(1035)='',
        @module_definition      nvarchar(max),
        @uses_ansi_nulls        bit,
	    @uses_quoted_identifier bit,
        @temp                   nvarchar(max),
        @default_fill_factor    tinyint=(SELECT TOP (1) CAST((CASE [value] WHEN 100 THEN 0 ELSE [value] END) AS tinyint)
                                         FROM sys.configurations
                                         WHERE [name] LIKE N'%fill factor%');

--- These are special (unicode) characters, used to display the graph output:
DECLARE @inf  nchar(1)=NCHAR(8734),          --- Infinity symbol
        @hyph nchar(1)=NCHAR(8722),          --- Hyphen
        @pipe nchar(1)=NCHAR(8739),          --- Pipe
	    @zero nchar(1)=NCHAR(176),           --- Superscript "0".
	    @one  nchar(1)=NCHAR(185),           --- Superscript "1".
        @lf   nchar(1)=NCHAR(10),            --- Line feed
        @cr   nchar(1)=NCHAR(13);            --- Carriage return

--- If this is a synonym, follow it until we reach a base object.
--- (SQL Server does not currently allow recursive synonyms, but just in case.)
WHILE (@synonym_references IS NOT NULL) BEGIN;

    SET @object_id=OBJECT_ID(@objname);
    IF (@objname LIKE N'#%')
        SELECT @is_tempdb=1, @object_id=OBJECT_ID(N'tempdb.dbo.'+@objname);

    IF (@object_id IS NULL)
        SELECT @object_id=tt.type_table_object_id
        FROM sys.table_types AS tt
        WHERE tt.user_type_id=TYPE_ID(@objname);

    IF (@object_id IS NULL)
        SET @synonym_references=NULL;

    SELECT @database_id=database_id, @database=QUOTENAME([name]), @compatibility_level=[compatibility_level]
    FROM sys.databases
    WHERE @objname LIKE N'#%' AND [name]=N'tempdb' OR
        @objname LIKE N'\[%\].%' ESCAPE N'\' AND @objname LIKE N'%.%.%' AND [name]=SUBSTRING(@objname, 2, NULLIF(CHARINDEX(N'].', @objname), 0)-2) OR
        @objname NOT LIKE N'[\[#]%' ESCAPE N'\' AND @objname LIKE N'%.%.%' AND [name]=LEFT(@objname, NULLIF(CHARINDEX(N'.', @objname), 0)-1) OR
        @objname NOT LIKE N'%.%.%' AND @objname NOT LIKE N'#%' AND database_id=DB_ID();

    SET @object_id_str=CAST(@object_id AS nvarchar(20));

    --- Is this a synonym, and if so, what base object does it reference?
    SET @synonym_references=NULL;
    SET @temp=N'SELECT @synonym_references=base_object_name FROM '+@database+N'.sys.synonyms WHERE [object_id]=@object_id;';
    EXECUTE sys.sp_executesql
        @temp,
        N'@object_id int, @synonym_references nvarchar(1035) OUTPUT',
        @object_id=@object_id,
        @synonym_references=@synonym_references OUTPUT;

    --- If yes, show header, update @objname, and restart the loop.
    IF (@synonym_references IS NOT NULL) BEGIN;
        SELECT OBJECT_SCHEMA_NAME(@object_id, @database_id) AS [Schema],
               OBJECT_NAME(@object_id, @database_id) AS [Object],
               N'Synonym' AS [Type],
               @object_id AS [object_id],
               @synonym_references AS [References];

        SET @objname=@synonym_references COLLATE database_default;
    END;

END;

-------------------------------------------------------------------------------
--- If database object isn't found, try a plaintext search instead.

IF (@object_id IS NULL) BEGIN;

    DECLARE @search_results TABLE (
        [type_desc]         nvarchar(60) NOT NULL,
        [schema_id]         int NOT NULL,
        major_id            int NOT NULL,
        minor_id            int NULL,
        index_id            int NULL,
        line                int NULL,
        [Definition]        nvarchar(max) NULL,
        row_count           bigint NULL,
        line_count          int NULL,
        _id                 int IDENTITY(1, 1) NOT NULL,
        PRIMARY KEY CLUSTERED (major_id, _id)
    );

    --- T-SQL modules like stored procedures, views, function, triggers, etc.
    WITH rcte AS (
        SELECT [object_id], 1 AS line, CAST(NULL AS nvarchar(max)) AS [sql], REPLACE([definition], NCHAR(13)+NCHAR(10), NCHAR(13)) AS remain,
               LEN([definition])-LEN(REPLACE([definition], CHAR(13), N'')) AS line_count
        FROM sys.sql_modules
        WHERE [definition] LIKE N'%'+@objname+N'%'

        UNION ALL

        SELECT rcte.[object_id],
               CAST(rcte.line+LEN(x2.left_of_keyword)-LEN(REPLACE(REPLACE(x2.left_of_keyword, NCHAR(10), N''), NCHAR(13), N'')) AS int) AS line,
               CAST(RIGHT(x2.left_of_keyword, PATINDEX(N'%['+NCHAR(10)+NCHAR(13)+N']%', REVERSE(x2.left_of_keyword)+NCHAR(10))-1)+
                    SUBSTRING(rcte.remain, x1.keyword_offset, x2.offset_to_next_line) AS nvarchar(max)) AS [sql],
               CAST(SUBSTRING(rcte.remain, x1.keyword_offset+x2.offset_to_next_line, LEN(rcte.remain)) AS nvarchar(max)) AS remain,
               rcte.line_count
        FROM rcte
        CROSS APPLY (
            VALUES (
                PATINDEX(N'%'+@objname+N'%', rcte.remain)
            )) AS x1(keyword_offset)
        CROSS APPLY (
            VALUES (
                LEFT(rcte.remain, x1.keyword_offset-1),
                PATINDEX(N'%['+NCHAR(10)+NCHAR(13)+N']%', SUBSTRING(rcte.remain, x1.keyword_offset, LEN(rcte.remain))+NCHAR(10))-1
            )) AS x2(left_of_keyword, offset_to_next_line)                
        WHERE x1.keyword_offset>0)

    INSERT INTO @search_results ([type_desc], [schema_id], major_id, line, [Definition], line_count)
    SELECT o.[type_desc], o.[schema_id], o.[object_id] AS major_id, rcte.line, rcte.[sql] AS [Definition], rcte.line_count
    FROM rcte
    INNER JOIN sys.all_objects AS o ON rcte.[object_id]=o.[object_id]
    WHERE rcte.[sql] IS NOT NULL
    OPTION (MAXRECURSION 0);


    --- Columns or computed column definitions:
    INSERT INTO @search_results ([type_desc], [schema_id], major_id, minor_id, [Definition])
    SELECT t.[type_desc], t.[schema_id], t.[object_id] AS major_id, c.column_id AS minor_id,
           COALESCE(cc.[name] COLLATE database_default+N' AS '+cc.[definition], c.[name], N'') AS [Definition]
    FROM sys.tables AS t
    INNER JOIN sys.all_columns AS c ON t.[object_id]=c.[object_id] AND c.[name] LIKE N'%'+@objname+N'%'
    LEFT JOIN sys.computed_columns AS cc ON t.[object_id]=cc.[object_id] AND cc.[definition] LIKE N'%'+@objname+N'%'
    LEFT JOIN sys.extended_properties AS ep ON ep.class=1 AND ep.major_id=t.[object_id] AND ep.minor_id=c.column_id AND ep.[name]=N'Description'
    WHERE c.[name] LIKE N'%'+@objname+N'%' OR
          cc.[definition] LIKE N'%'+@objname+N'%' OR
          CAST(ep.[value] AS nvarchar(max)) LIKE N'%'+@objname+N'%';


    --- Default constraints:
    INSERT INTO @search_results ([type_desc], [schema_id], major_id, minor_id, [Definition])
    SELECT c.[type_desc], o.[schema_id], o.[object_id] AS major_id, oc.column_id AS minor_id,
           ISNULL(oc.[name]+N' ', N'')+N'CONSTRAINT '+c.[name]+N' DEFAULT '+c.[definition] AS [Definition]
    FROM sys.default_constraints AS c
    INNER JOIN sys.schemas AS s ON c.[schema_id]=s.[schema_id]
    LEFT JOIN sys.all_objects AS o ON c.parent_object_id=o.[object_id]
    LEFT JOIN sys.schemas AS os ON o.[schema_id]=os.[schema_id]
    LEFT JOIN sys.all_columns AS oc ON c.parent_object_id=oc.[object_id] AND c.parent_column_id=oc.column_id
    WHERE c.[name] LIKE N'%'+@objname+N'%' OR
          c.[definition] LIKE N'%'+@objname+N'%';


    --- Check constraints:
    INSERT INTO @search_results ([type_desc], [schema_id], major_id, minor_id, [Definition])
    SELECT c.[type_desc], o.[schema_id], o.[object_id] AS major_id, oc.column_id AS minor_id,
           ISNULL(oc.[name]+N' ', N'')+N'CONSTRAINT '+c.[name]+N' CHECK '+c.[definition] AS [Definition]
    FROM sys.check_constraints AS c
    INNER JOIN sys.schemas AS s ON c.[schema_id]=s.[schema_id]
    LEFT JOIN sys.all_objects AS o ON c.parent_object_id=o.[object_id]
    LEFT JOIN sys.schemas AS os ON o.[schema_id]=os.[schema_id]
    LEFT JOIN sys.all_columns AS oc ON c.parent_object_id=oc.[object_id] AND c.parent_column_id=oc.column_id
    WHERE c.[name] LIKE N'%'+@objname+N'%' OR
          c.[definition] LIKE N'%'+@objname+N'%';


    --- Indexes and index filter definitions:
    INSERT INTO @search_results ([type_desc], [schema_id], major_id, index_id, [Definition], row_count)
    SELECT N'INDEX' AS [type_desc], o.[schema_id], o.[object_id] AS major_id, i.index_id,
           (CASE WHEN i.is_unique=1 THEN N'UNIQUE ' ELSE N'' END)+i.[type_desc]+
           (CASE WHEN i.index_id>0 THEN N' INDEX' ELSE N'' END)+
           ISNULL(N' '+i.[name], N'')+
           ISNULL(N' WHERE '+i.filter_definition, N'') AS [Definition],
           (SELECT SUM(p.[rows])
            FROM sys.partitions AS p
            WHERE p.[object_id]=i.[object_id] AND p.index_id=i.index_id) AS row_count
    FROM sys.indexes AS i
    INNER JOIN sys.all_objects AS o ON i.[object_id]=o.[object_id]
    INNER JOIN sys.schemas AS s ON o.[schema_id]=s.[schema_id]
    WHERE i.[name] LIKE N'%'+@objname+N'%' OR
          i.filter_definition LIKE N'%'+@objname+N'%';






    --- Tables and objects:
    INSERT INTO @search_results ([type_desc], [schema_id], major_id, row_count, line_count)
    SELECT o.[type_desc], o.[schema_id], o.[object_id] AS major_id,
           (SELECT SUM(p.[rows])
            FROM sys.partitions AS p
            WHERE p.[object_id]=o.[object_id] AND p.index_id IN (0, 1)) AS row_count,
           (SELECT TOP (1) line_count
            FROM @search_results
            WHERE major_id=o.[object_id] AND line_count IS NOT NULL) AS line_count
    FROM sys.all_objects AS o
    LEFT JOIN sys.extended_properties AS ep ON ep.class=1 AND ep.major_id=o.[object_id] AND ep.minor_id=0 AND ep.[name]=N'Description'
    WHERE o.parent_object_id=0 AND (
          o.[object_id] IN (SELECT major_id FROM @search_results)
      AND o.[object_id] NOT IN (SELECT major_id FROM @search_results WHERE major_id IS NOT NULL AND COALESCE(minor_id, index_id, line) IS NULL)
       OR o.[name] LIKE N'%'+@objname+N'%'
       OR CAST(ep.[value] AS nvarchar(max)) LIKE N'%'+@objname+N'%');


    --- Schema
    INSERT INTO @search_results ([type_desc], [schema_id], major_id, minor_id)
    SELECT N'SCHEMA', s.[schema_id], 0, 0
    FROM sys.schemas AS s
    LEFT JOIN sys.extended_properties AS ep ON ep.class=3 AND ep.major_id=s.[schema_id] AND ep.minor_id=0 AND ep.[name]=N'Description'
    WHERE s.[name] LIKE N'%'+@objname+N'%' OR
          CAST(ep.[value] AS nvarchar(max)) LIKE N'%'+@objname+N'%'
       OR s.[schema_id] IN (SELECT [schema_id] FROM @search_results)
      AND s.[schema_id] NOT IN (SELECT [schema_id] FROM @search_results WHERE [type_desc]=N'SCHEMA');







    --- Output the results in a fancypants ASCII graph:
    SELECT (CASE WHEN o._ordinal=0 THEN s.[name] ELSE N'' END) AS [Schema],
           ISNULL((CASE WHEN x._ordinal=0 THEN o.[type_desc] ELSE N'' END), N'') AS [Object type],
           ISNULL((CASE WHEN x._ordinal=0 THEN s.[name]+N'.'+o.[name] WHEN x._ordinal=1 AND x._count>1 THEN N'/' WHEN x._ordinal=x._count THEN N'\' ELSE N'|' END), N'') AS [Object],
           (CASE WHEN x._ordinal=0           THEN COALESCE(REPLACE(CONVERT(varchar(20), CAST(o.line_count AS money), 1), '.00', '')+' lines',
                                                           REPLACE(CONVERT(varchar(20), CAST(o.row_count  AS money), 1), '.00', '')+' rows', '')
                 WHEN x.index_id IS NOT NULL THEN COALESCE(REPLACE(CONVERT(varchar(20), CAST(x.row_count  AS money), 1), '.00', '')+' rows', '')
                 ELSE '' END) AS [Size],
           ISNULL(STR(x.line, 10, 0), N'') AS [Line no],
           ISNULL(x.[Definition], N'') AS [Definition],
           ISNULL((CASE WHEN x._ordinal>0 THEN x.[Description]
                        WHEN x._ordinal=0 THEN o.[Description]
                        WHEN o._ordinal=0 THEN ep.[value] END), N'') AS [Description]
    FROM @search_results AS x1
    INNER JOIN sys.schemas AS s ON x1.[schema_id]=s.[schema_id]
    LEFT JOIN sys.extended_properties AS ep ON ep.class=3 AND ep.major_id=s.[schema_id] AND ep.minor_id=0 AND ep.[name]=N'Description'

    --- For each schema, return one blank offset row and the objects for that schema:
    OUTER APPLY (
        SELECT ROW_NUMBER() OVER (PARTITION BY s.[schema_id] ORDER BY o.[name]) AS _ordinal,
               COUNT(*) OVER (PARTITION BY s.[schema_id]) AS _count,
               o.[object_id], o.[name], o.[type_desc], ep.[value] AS [Description],
               row_count, line_count
        FROM @search_results AS res
        INNER JOIN sys.objects AS o ON res.major_id=o.[object_id]
        LEFT JOIN sys.extended_properties AS ep ON ep.class=1 AND ep.major_id=res.major_id AND ep.minor_id=0 AND ep.[name]=N'Description'
        WHERE res.[schema_id]=s.[schema_id] AND COALESCE(res.minor_id, res.index_id, res.line) IS NULL

        UNION ALL

        SELECT 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL
        ) AS o

    --- .. and for each object, return a blank offset row and all of the lines/columns/etc for that object:
    OUTER APPLY (
        SELECT ROW_NUMBER() OVER (PARTITION BY res.major_id ORDER BY res.minor_id) AS _ordinal,
               COUNT(*) OVER (PARTITION BY res.major_id) AS _count,
               res.line,
               res.minor_id, res.index_id, res.row_count,
               res.[Definition], ep.[value] AS [Description]
        FROM @search_results AS res
        LEFT JOIN sys.extended_properties AS ep ON ep.class=1 AND ep.major_id=res.major_id AND ep.minor_id=res.minor_id AND ep.[name]=N'Description'
        WHERE res.major_id=o.[object_id] AND COALESCE(res.minor_id, res.index_id, res.line) IS NOT NULL

        UNION ALL

        SELECT 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL
        ) AS x
    WHERE x1.major_id=0
    ORDER BY s.[name], o._ordinal, x._ordinal;




    RETURN;






/*        ,

    search AS (
)

    SELECT class, parent_class, parent_id, major_id, minor_id, ordinal, [Type], [Line no], [Definition], Size
    INTO #search_results
    FROM search
    OPTION (MAXRECURSION 0);

    INSERT INTO #search_results (class, parent_class, parent_id, major_id, minor_id, ordinal, [Type], [Line no], [Definition], Size)
    SELECT 1 AS class, 3 AS parent_class, [schema_id] AS parent_id, [object_id] AS major_id, 0 AS minor_id, 0 AS ordinal,
           t.[type_desc] AS [Type], '' AS [Line no],
           NULL AS [Definition],
           ISNULL((SELECT REPLACE(CONVERT(varchar(20), CAST(NULLIF(SUM(p.[rows]), 0) AS money), 1), '.00', '')+' rows'
                   FROM sys.partitions AS p
                   WHERE p.[object_id]=t.[object_id] AND p.index_id IN (0, 1)), '(empty)') AS [Size]
    FROM sys.tables AS t
    WHERE t.[object_id] IN (SELECT major_id FROM #search_results WHERE class IN (-1, 51))
      AND t.[object_id] NOT IN (SELECT major_id FROM #search_results WHERE class=1);

    INSERT INTO #search_results (class, parent_id, major_id, minor_id, ordinal, [Type], [Line no], [Definition], Size)
    SELECT 3 AS class, NULL AS parent_id, s.[schema_id] AS major_id, 0 AS minor_id, 0 AS ordinal,
           N'SCHEMA' AS [Type], '' AS [Line no],
           N'' AS [Definition], '' AS [Size]
    FROM sys.schemas AS s
    WHERE s.[schema_id] IN (SELECT parent_id FROM #search_results WHERE class=1)
      AND s.[schema_id] NOT IN (SELECT major_id FROM #search_results WHERE class=3);
*/


    RETURN;
END;

-------------------------------------------------------------------------------
--- Table variables to hold copies of system DMVs. The reason we use temp tables
--- is to be able to collect this data from the current database or from
--- tempdb (if it's a temp table)

DECLARE @sysobjects TABLE (
    [schema_id]	        int NOT NULL,
    [object_id]	        int NOT NULL,
    principal_id        int NULL,
    [type]		        char(2) COLLATE database_default NOT NULL,
    [type_desc]         nvarchar(60) COLLATE database_default NOT NULL,
    [name]              sysname COLLATE database_default NOT NULL,
	is_memory_optimized bit NOT NULL,
	durability_desc     nvarchar(60) COLLATE database_default NULL,
    temporal_type_desc  nvarchar(60) COLLATE database_default NULL,
    history_table_id    int NULL,
    is_change_tracked   bit NOT NULL,
    is_track_columns_updated_on bit NOT NULL,
    min_valid_version   bigint NULL,
    PRIMARY KEY CLUSTERED ([object_id])
);

DECLARE @sysschemas TABLE (
    [schema_id]       int NOT NULL,
    principal_id      int NOT NULL,
    name              sysname COLLATE database_default NOT NULL,
    PRIMARY KEY CLUSTERED ([schema_id])
);

DECLARE @syscolumns TABLE (
    [object_id]       int NOT NULL,
    column_id         int NOT NULL,
    [name]            sysname COLLATE database_default NOT NULL,
    user_type_id      int NOT NULL,
    system_type_id    int NOT NULL,
    max_length        smallint NOT NULL,
    [precision]       tinyint NOT NULL,
    scale             tinyint NOT NULL,
    is_sparse         bit NULL,
    is_nullable       bit NULL,
    collation_name    sysname COLLATE database_default NULL,
    is_ansi_padded    bit NOT NULL,
    xml_collection_id int NOT NULL,
    default_object_id int NOT NULL,
    seed_value        sql_variant NULL,
    increment_value   sql_variant NULL,
    [definition]      nvarchar(max) COLLATE database_default NULL,
    is_persisted      bit NULL,
    [type_name]       sysname COLLATE database_default NOT NULL,
    default_name      sysname COLLATE database_default NULL,
    default_is_system_named bit NULL,
    current_value     sql_variant NULL,
    max_alloc_size    int NULL,
    generated_always_type_desc nvarchar(60) COLLATE database_default NULL,
    is_hidden         bit NULL,
    PRIMARY KEY CLUSTERED ([object_id], column_id)
);

DECLARE @sysparameters TABLE (
    parameter_id      int NOT NULL,
    name              sysname COLLATE database_default NOT NULL,
    user_type_id      int NOT NULL,
    system_type_id    int NOT NULL,
    max_length        smallint NOT NULL,
    [precision]       tinyint NOT NULL,
    scale             tinyint NOT NULL,
    is_nullable       bit NULL,
    xml_collection_id int NOT NULL,
    is_output         bit NOT NULL,
    is_readonly       bit NOT NULL,
    is_table_type     bit NOT NULL,
    [type_name]       sysname COLLATE database_default NOT NULL,
    tbl_type_cols     varchar(max) COLLATE database_default NULL,
    PRIMARY KEY CLUSTERED (parameter_id)
);

DECLARE @sysindexes TABLE (
    [object_id]          int NOT NULL,
    index_id             int NOT NULL,
    [name]               sysname COLLATE database_default NULL,
    [type]               tinyint NOT NULL,
    [type_desc]          nvarchar(120) COLLATE database_default NULL,
    data_space_id        int NULL,
    is_primary_key       bit NULL,
    is_unique_constraint bit NULL,
    is_unique            bit NULL,
    filter_definition    nvarchar(max) COLLATE database_default NULL,
    fill_factor          tinyint NOT NULL,
    [allow_row_locks]    bit NULL,
    [allow_page_locks]	 bit NULL,
    is_padded	      	 bit NULL,
    has_filter	     	 bit NULL,
    is_system_named      bit NOT NULL,
    [bucket_count]       bigint NULL,
    [compression_delay]  int NULL,
    PRIMARY KEY CLUSTERED ([object_id], index_id)
);

DECLARE @sysindexcolumns TABLE (
    [object_id]        int NOT NULL,
    index_id           int NOT NULL,
    index_column_id    int NOT NULL,
    column_id          int NOT NULL,
    key_ordinal        tinyint NOT NULL,
    partition_ordinal  tinyint NOT NULL,
    is_descending_key  bit NULL,
    is_included_column bit NULL,
    PRIMARY KEY CLUSTERED ([object_id], index_id, key_ordinal, index_column_id)
);

DECLARE @sysforeignkeys TABLE (
    [object_id]                    int NOT NULL,
    name                           sysname COLLATE database_default NOT NULL,
    parent_object_id               int NOT NULL,
    referenced_object_id           int NOT NULL,
    delete_referential_action_desc nvarchar(120) COLLATE database_default NULL,
    update_referential_action_desc nvarchar(120) COLLATE database_default NULL,
    is_system_named                bit NOT NULL,
    is_disabled                    bit NOT NULL,
    is_not_trusted                 bit NOT NULL,
    PRIMARY KEY CLUSTERED ([object_id])
);

DECLARE @sysforeignkeycols TABLE (
    constraint_object_id  int NOT NULL,
    constraint_column_id  int NOT NULL,
    parent_object_id      int NOT NULL,
    parent_column_id      int NOT NULL,
    referenced_object_id  int NOT NULL,
    referenced_column_id  int NOT NULL,
    PRIMARY KEY CLUSTERED (constraint_object_id, constraint_column_id)
);

DECLARE @xmlschemacollections TABLE (
    xml_collection_id    int NOT NULL,
    [schema_id]          int NOT NULL,
    name                 sysname COLLATE database_default NOT NULL,
    PRIMARY KEY CLUSTERED (xml_collection_id)
);

DECLARE @sysdataspaces TABLE (
    data_space_id    int NOT NULL,
    name             sysname COLLATE database_default NOT NULL,
    [type]           char(2) COLLATE database_default NOT NULL,
    is_default       bit NOT NULL,
    PRIMARY KEY CLUSTERED (data_space_id)
);

DECLARE @sysdatabaseprincipals TABLE (
    principal_id     int NOT NULL,
    name             sysname COLLATE database_default NOT NULL
);

DECLARE @sysexprdependencies TABLE (
    referencing_id            int NOT NULL,
    referenced_id             int NOT NULL,
    is_schema_bound_reference bit NOT NULL,
    PRIMARY KEY CLUSTERED (referencing_id, referenced_id)
);

DECLARE @syspartitions TABLE (
    [partition_id]   bigint NULL,
    [object_id]      int NOT NULL,
    index_id         int NOT NULL,
    partition_number int NOT NULL,
    [rows]           bigint NULL,
    data_compression_desc nvarchar(120) COLLATE database_default NOT NULL,
    xml_compression_desc varchar(3) COLLATE database_default NULL,
    boundary_value_on_right bit NULL,
    boundary         nvarchar(max) NULL,
    boundary_type    sysname NULL,
    discrete_boundary bit NULL,
    PRIMARY KEY CLUSTERED ([object_id], index_id, partition_number)
);

DECLARE @syspartitionstats TABLE (
    [partition_id]               bigint NOT NULL,
    row_count                    bigint NULL,
    in_row_used_page_count       bigint NULL,
    reserved_page_count          bigint NULL,
    row_overflow_used_page_count bigint NULL,
    lob_used_page_count          bigint NULL,
    used_page_count              bigint NULL,
    PRIMARY KEY CLUSTERED ([partition_id])
);

DECLARE @destination_data_spaces TABLE (
    partition_scheme_id         int NOT NULL,
    partition_number            int NOT NULL,
    data_space_id               int NOT NULL,
    PRIMARY KEY CLUSTERED (partition_scheme_id, partition_number)
);

DECLARE @columnstore_rowgroups TABLE (
    [object_id]                 int NOT NULL,
    index_id                    int NOT NULL,
    partition_number            int NOT NULL,
    [state]                     tinyint NOT NULL,
    total_rows                  bigint NOT NULL,
    size_in_bytes               bigint NOT NULL,
    PRIMARY KEY CLUSTERED ([object_id], index_id, partition_number, [state])
);

DECLARE @sysdatabasepermissions TABLE (
    class                tinyint NOT NULL,
    class_desc           nvarchar(120) COLLATE database_default NULL,
    major_id             int NOT NULL,
    minor_id             int NOT NULL,
    grantee_principal_id int NOT NULL,
    grantor_principal_id int NOT NULL,
    [type]               char(4) COLLATE database_default NOT NULL,
    [permission_name]    nvarchar(256) COLLATE database_default NULL,
    [state]              char(1) COLLATE database_default NOT NULL,
    state_desc           nvarchar(120) COLLATE database_default NULL,
    PRIMARY KEY CLUSTERED (class, major_id, minor_id, [type], [state], grantee_principal_id)
);

DECLARE @signatures TABLE (
    src                 nvarchar(20) COLLATE database_default NOT NULL,
    [name]              sysname COLLATE database_default NOT NULL,
    encryption_type_desc varchar(20) COLLATE database_default NULL,
    [type_desc]         nvarchar(60) COLLATE database_default NOT NULL,
    major_id            int NOT NULL
);

DECLARE @references TABLE (
    parent_id      int NOT NULL,         --- object_id of referencing object
    parent_name    varchar(255) COLLATE database_default NOT NULL, -- Name of referencing object
    child_id       int NOT NULL,         --- object_id of referenced object
    child_name     varchar(255) COLLATE database_default NOT NULL, -- Name of referenced object
    is_schemabound bit,                  --- Is this relation schemabound?
    is_foreign_key bit,                  --- Is this relation a foreign key constraint? (If not, it's an SQL module)
    parent_row     smallint NOT NULL,    --- Parent ordinal (this child)
    parent_count   smallint NOT NULL,    --- Number of parents (this child)
    child_row      smallint NOT NULL,    --- Child ordinal (this parent)
    child_count    smallint NOT NULL,    --- Number of children (this parent)
    is_unique      bit NOT NULL,         --- If the combination of referencing columns is unique
    is_nullable    bit NOT NULL,         --- If the referencing column allows nulls
    PRIMARY KEY CLUSTERED (parent_id, child_id),
    UNIQUE (parent_id, child_row),
    UNIQUE (child_id, parent_row)
);

DECLARE @syssecuritypolicies TABLE (
    security_policy_id          int NOT NULL,
    security_predicate_id       int NOT NULL,
    predicate_type_desc         nvarchar(60) COLLATE database_default NOT NULL,
    predicate_definition        nvarchar(max) COLLATE database_default NOT NULL,
    is_enabled                  bit NOT NULL,
    is_schema_bound             bit NOT NULL,
    PRIMARY KEY CLUSTERED (security_policy_id, security_predicate_id)
);

DECLARE @syssqlmodules TABLE (
    [definition]        nvarchar(max) COLLATE database_default NULL,
    uses_ansi_nulls     bit NULL,
    uses_quoted_identifier bit NULL,
    is_schema_bound     bit NULL,
	uses_native_compilation bit NULL
);

DECLARE @systriggers TABLE (
    [object_id]     int NOT NULL,
    name            sysname COLLATE database_default NOT NULL,
    is_disabled     bit NOT NULL,
    is_instead_of_trigger bit NOT NULL,
    [trigger_events]  sysname COLLATE database_default NOT NULL,
    PRIMARY KEY CLUSTERED ([object_id])
);
/*
DECLARE @plans TABLE (
    _id                         int IDENTITY(1, 1) NOT NULL,
    plan_generation_num         bigint NOT NULL,
    query_plan                  xml NULL,
    execution_count             bigint NOT NULL,
    last_execution_time         datetime NULL,
    PRIMARY KEY CLUSTERED (plan_generation_num, _id)
);
*/
DECLARE @spt_values_O9T TABLE (
	[name]		nvarchar(50) COLLATE database_default NOT NULL,
	PRIMARY KEY CLUSTERED ([name])
);

DECLARE @definition TABLE (
	id			int NOT NULL,
	[definition] nvarchar(max) COLLATE database_default NOT NULL,
	PRIMARY KEY CLUSTERED (id)
);

DECLARE @reserved_keywords TABLE (
    keyword     sysname COLLATE database_default NOT NULL,
    PRIMARY KEY CLUSTERED (keyword)
);

DECLARE @extended_properties TABLE (
    [object_id] int NOT NULL,
    column_id   int NOT NULL,
    [name]      sysname COLLATE database_default NOT NULL,
    [value]     sql_variant NULL
    PRIMARY KEY CLUSTERED ([object_id], column_id, [name])
);

-------------------------------------------------------------------------------
--- Populate DMV table variables:

IF (@is_azure_sql_db=1)
	INSERT INTO @spt_values_O9T ([name])
	EXEC(N'
		SELECT DISTINCT [type]+N'': ''+LOWER([type_desc])
		FROM sys.objects');

IF (@is_azure_sql_db=0)
	INSERT INTO @spt_values_O9T ([name])
	EXEC(N'
		SELECT [name]
		FROM master.dbo.spt_values
		WHERE [type]=N''O9T''');

SET @temp=N'
SELECT ISNULL(tt.[schema_id], o.[schema_id]), o.[object_id], o.principal_id, o.[type], o.[type_desc], ISNULL(tt.[name], o.[name]),
       '+(CASE WHEN @compatibility_level>=120 THEN N'ISNULL(t.is_memory_optimized, 0), t.durability_desc' ELSE N'0, NULL' END)+N',
       '+(CASE WHEN @compatibility_level>=130 THEN N't.temporal_type_desc, t.history_table_id' ELSE N'NULL, NULL' END)+N',
       (CASE WHEN ct.[object_id] IS NOT NULL THEN 1 ELSE 0 END), ISNULL(ct.is_track_columns_updated_on, 0), ct.min_valid_version
FROM '+@database+N'.sys.all_objects AS o
LEFT JOIN '+@database+N'.sys.tables AS t ON o.[object_id]=t.[object_id]
LEFT JOIN '+@database+N'.sys.table_types AS tt ON tt.type_table_object_id=o.[object_id]
LEFT JOIN '+@database+N'.sys.change_tracking_tables AS ct ON t.[object_id]=ct.[object_id]'

INSERT INTO @sysobjects
EXEC(@temp);

INSERT INTO @sysschemas
EXEC(N'
SELECT [schema_id], principal_id, name
FROM '+@database+N'.sys.schemas');

SET @temp=(CASE
       WHEN @compatibility_level>=130
       THEN N'c.generated_always_type_desc, c.is_hidden'
       ELSE N'NULL, NULL' END);

INSERT INTO @syscolumns
EXEC(N'
SELECT c.[object_id], c.column_id, c.[name], c.user_type_id, c.system_type_id,
       c.max_length, c.[precision], c.scale, c.is_sparse, c.is_nullable,
       c.collation_name, c.is_ansi_padded, c.xml_collection_id, c.default_object_id,
       ic.seed_value, ic.increment_value, ISNULL(cc.[definition], d.[definition]), cc.is_persisted,
       t.[name] AS [type_name], d.[name] AS default_name,
       d.is_system_named AS default_is_system_named, NULL AS current_value,
       (CASE WHEN st.[name]IN (N''bit'', N''tinyint'') THEN 1
             WHEN st.[name]=N''smallint'' THEN 2
             WHEN st.[name]=N''date'' THEN 3
             WHEN st.[name] IN (N''int'', N''smalldatetime'', N''smallmoney'') THEN 4
             WHEN st.[name] IN (N''bigint'', N''money'', N''timestamp'', N''datetime'') THEN 8
             WHEN st.[name]=N''datetime2'' AND c.scale BETWEEN 1 AND 2 THEN 6
             WHEN st.[name]=N''datetime2'' AND c.scale BETWEEN 3 AND 4 THEN 7
             WHEN st.[name]=N''datetime2'' AND c.scale BETWEEN 5 AND 7 THEN 8
             WHEN st.[name]=N''datetimeoffset'' AND c.scale BETWEEN 0 AND 2 THEN 8
             WHEN st.[name]=N''datetimeoffset'' AND c.scale BETWEEN 3 AND 4 THEN 9
             WHEN st.[name]=N''datetimeoffset'' AND c.scale BETWEEN 5 AND 7 THEN 10
             WHEN st.[name] IN (N''decimal'', N''numeric'') AND c.[precision] BETWEEN 1 AND 9 THEN 5
             WHEN st.[name] IN (N''decimal'', N''numeric'') AND c.[precision] BETWEEN 10 AND 19 THEN 9
             WHEN st.[name] IN (N''decimal'', N''numeric'') AND c.[precision] BETWEEN 20 AND 28 THEN 13
             WHEN st.[name] IN (N''decimal'', N''numeric'') AND c.[precision] BETWEEN 29 AND 38 THEN 17
             WHEN st.[name] IN (N''real'', N''float'') AND c.[precision]<=24 THEN 4
             WHEN st.[name] IN (N''real'', N''float'') AND c.[precision]>24 THEN 8
             WHEN st.[name]=N''time'' AND c.[scale] BETWEEN 0 AND 2 THEN 3
             WHEN st.[name]=N''time'' AND c.[scale] BETWEEN 3 AND 4 THEN 4
             WHEN st.[name]=N''time'' AND c.[scale] BETWEEN 5 AND 7 THEN 5
             WHEN st.[name] IN (N''binary'', N''varbinary'', N''char'', N''nchar'', N''varchar'', N''nvarchar'', N''sysname'') THEN NULLIF(c.max_length, -1)
             WHEN st.[name]=N''uniqueidentifier'' THEN 16
             END) AS max_alloc_size,
       '+@temp+N'
FROM '+@database+N'.sys.all_columns AS c
LEFT JOIN '+@database+N'.sys.identity_columns AS ic ON c.[object_id]=ic.[object_id] AND c.column_id=ic.column_id
LEFT JOIN '+@database+N'.sys.computed_columns AS cc ON c.[object_id]=cc.[object_id] AND c.column_id=cc.column_id
LEFT JOIN '+@database+N'.sys.types AS t ON c.user_type_id=t.user_type_id
LEFT JOIN '+@database+N'.sys.types AS st ON c.system_type_id=st.user_type_id
LEFT JOIN '+@database+N'.sys.default_constraints AS d ON d.[object_id]=c.default_object_id');

BEGIN TRY;
    INSERT INTO @syscolumns
    EXEC(N'
    SELECT s.[object_id], 1 AS column_id, s.[name], s.user_type_id, s.system_type_id,
           8 AS max_length, s.[precision], s.scale, 0 AS is_sparse, 0 AS is_nullable,
           NULL AS collation_name, 0 AS is_ansi_padded, 0 AS xml_collection_id, 0 AS default_object_id,
           s.start_value AS seed_value, s.increment AS increment_value,
       
           ISNULL(N'' MINVALUE ''+CAST(NULLIF(s.minimum_value, (CASE st.[name]
                WHEN N''tinyint'' THEN 0
                WHEN N''smallint'' THEN -32768
                WHEN N''int'' THEN -2147483648
                WHEN N''bigint'' THEN -9223372036854775808
            END)) AS nvarchar(40)), N'''')+
            ISNULL(N'' MAXVALUE ''+CAST(NULLIF(s.maximum_value, (CASE st.[name]
                WHEN N''tinyint'' THEN 255
                WHEN N''smallint'' THEN 32767
                WHEN N''int'' THEN 2147483647
                WHEN N''bigint'' THEN 9223372036854775807
            END)) AS nvarchar(40)), N'''')+
            (CASE WHEN s.is_cycling=1 THEN N'' CYCLE'' ELSE N'''' END)+
            (CASE WHEN s.is_cached=0 THEN N'' NOCACHE''
                  WHEN s.is_cached=1 THEN ISNULL(N''CACHE ''+CAST(s.cache_size AS nvarchar(10)), '''')
                  END) AS [definition], 0 AS is_persisted,
           t.[name] AS [type_name], NULL AS default_name, 1 AS default_is_system_named, s.current_value, NULL, NULL, NULL
    FROM '+@database+N'.sys.sequences AS s
    LEFT JOIN '+@database+N'.sys.types AS t ON s.user_type_id=t.user_type_id
    LEFT JOIN '+@database+N'.sys.types AS st ON s.system_type_id=st.user_type_id
    ');
END TRY
BEGIN CATCH;
    PRINT 'sys.sequences could not be loaded.';
END CATCH;

SET @temp=(CASE WHEN SERVERPROPERTY('ProductVersion')>=N'12' THEN N'p.is_nullable' ELSE N'1' END);

INSERT INTO @sysparameters
EXEC(N'
SELECT p.parameter_id, p.[name], p.user_type_id, p.system_type_id, p.max_length, p.[precision],
       p.scale, '+@temp+N', p.xml_collection_id, p.is_output, p.is_readonly, t.is_table_type,
       ISNULL(s.[name]+N''.'', N'''')+t.[name] AS [type_name],
	   N''(''+SUBSTRING(CAST((SELECT N'', ''+ttc.[name]
	                          FROM '+@database+N'.sys.all_columns AS ttc
	                          WHERE ttc.[object_id]=tt.type_table_object_id
	                          ORDER BY ttc.column_id
	                          FOR XML PATH(N''''), TYPE) AS varchar(max)), 3, 8000)+N'')'' AS tbl_type_cols
FROM '+@database+N'.sys.all_parameters AS p
LEFT JOIN '+@database+N'.sys.types AS t ON p.user_type_id=t.user_type_id
LEFT JOIN '+@database+N'.sys.table_types AS tt ON t.user_type_id=tt.user_type_id
LEFT JOIN '+@database+N'.sys.schemas AS s ON t.is_table_type=1 AND t.[schema_id]=s.[schema_id]
WHERE p.[object_id]='+@object_id_str);

SET @temp=(CASE WHEN SERVERPROPERTY('ProductVersion')>=N'13' THEN N'ix.[compression_delay]' ELSE N'NULL' END);

INSERT INTO @sysindexes
EXEC(N'
SELECT ix.[object_id], ix.index_id, ix.[name], ix.[type], ix.[type_desc], ix.data_space_id,
       ix.is_primary_key, ix.is_unique_constraint, ix.is_unique, ix.filter_definition,
       ix.fill_factor, ix.[allow_row_locks], ix.[allow_page_locks], ix.is_padded, ix.has_filter,
	   ISNULL(kc.is_system_named, 0), NULL, '+@temp+N'
FROM '+@database+N'.sys.indexes AS ix
LEFT JOIN '+@database+N'.sys.key_constraints AS kc ON ix.[object_id]=kc.parent_object_id AND ix.[name]=kc.[name]
WHERE ix.is_hypothetical=0 AND ix.[type_desc] NOT LIKE N''%HASH%''');

IF (@compatibility_level>=120)
    INSERT INTO @sysindexes
    EXEC(N'
    SELECT ix.[object_id], ix.index_id, ix.[name], ix.[type], ix.[type_desc], ix.data_space_id,
           ix.is_primary_key, ix.is_unique_constraint, ix.is_unique, ix.filter_definition,
           ix.fill_factor, ix.[allow_row_locks], ix.[allow_page_locks], ix.is_padded, ix.has_filter,
	       ISNULL(kc.is_system_named, 0), ix.[bucket_count], NULL
    FROM '+@database+N'.sys.hash_indexes AS ix
    LEFT JOIN '+@database+N'.sys.key_constraints AS kc ON ix.[object_id]=kc.parent_object_id AND ix.[name]=kc.[name]
    WHERE ix.is_hypothetical=0');

INSERT INTO @sysindexcolumns
EXEC(N'
SELECT [object_id], index_id, index_column_id, column_id, key_ordinal,
        partition_ordinal, is_descending_key, is_included_column
FROM '+@database+N'.sys.index_columns');

INSERT INTO @sysforeignkeys
EXEC(N'
SELECT [object_id], name, parent_object_id, referenced_object_id,
        delete_referential_action_desc, update_referential_action_desc,
        is_system_named, is_disabled, is_not_trusted
FROM '+@database+N'.sys.foreign_keys');

INSERT INTO @sysforeignkeycols
EXEC(N'
SELECT constraint_object_id, constraint_column_id, parent_object_id,
        parent_column_id, referenced_object_id, referenced_column_id
FROM '+@database+N'.sys.foreign_key_columns');

INSERT INTO @extended_properties
EXEC(N'
SELECT major_id, minor_id, [name], [value]
FROM '+@database+N'.sys.extended_properties
WHERE class=1;');

INSERT INTO @xmlschemacollections
EXEC(N'
SELECT xml_collection_id, [schema_id], name
FROM '+@database+N'.sys.xml_schema_collections');

INSERT INTO @sysdataspaces
EXEC(N'
SELECT data_space_id, name, [type], is_default
FROM '+@database+N'.sys.data_spaces');

INSERT INTO @sysdatabaseprincipals
EXEC(N'
SELECT principal_id, name
FROM '+@database+N'.sys.database_principals');

SET @temp=N'
SELECT [definition], uses_ansi_nulls, uses_quoted_identifier, is_schema_bound'+(CASE WHEN @compatibility_level>=120 THEN N', uses_native_compilation' ELSE N', NULL' END)+N'
FROM '+@database+N'.sys.sql_modules
WHERE [object_id]='+@object_id_str

INSERT INTO @syssqlmodules
EXEC(@temp);

SELECT @module_definition=[definition],
       @uses_ansi_nulls=uses_ansi_nulls,
       @uses_quoted_identifier=uses_quoted_identifier
FROM @syssqlmodules;

BEGIN TRY;
	INSERT INTO @sysexprdependencies
	EXEC(N'
    SELECT referencing_id,
           referenced_id,
           is_schema_bound_reference
    FROM (
	    SELECT DISTINCT d.referencing_id,
					    COALESCE(ct.type_table_object_id,
                                 d.referenced_id,
                                 (CASE WHEN d.referenced_server_name IS NULL AND d.referenced_database_name IS NULL
                                       THEN OBJECT_ID(ISNULL(QUOTENAME(referenced_schema_name)+N''.'', N'''')+QUOTENAME(d.referenced_entity_name)) END)) AS referenced_id,
					    (CASE WHEN ct.user_type_id IS NOT NULL THEN 1 ELSE d.is_schema_bound_reference END) AS is_schema_bound_reference
	    FROM '+@database+N'.sys.sql_expression_dependencies AS d
	    LEFT JOIN '+@database+N'.sys.table_types AS ct ON d.referenced_class=6  AND d.referenced_id=ct.user_type_id
	    WHERE d.referencing_class=1 AND
		      d.referenced_class IN (1, 6)
        ) AS sub
    WHERE referenced_id IS NOT NULL;');
END TRY
BEGIN CATCH;
	PRINT 'Problem compiling expression dependencies: '+ERROR_MESSAGE();
END CATCH;

SET @temp=N'
SELECT p.[partition_id], p.[object_id], p.index_id, p.partition_number, p.[rows], p.data_compression_desc,
       '+(CASE WHEN @compatibility_level>=160 THEN N'p.xml_compression_desc' ELSE N'NULL' END)+N', pf.boundary_value_on_right, prv.boundary, prv.boundary_type, 0
FROM '+@database+N'.sys.partitions AS p
LEFT JOIN '+@database+N'.sys.indexes AS i ON p.[object_id]=i.[object_id] AND p.index_id=i.index_id AND p.[object_id]='+@object_id_str+N'
LEFT JOIN '+@database+N'.sys.partition_schemes AS ps ON i.data_space_id=ps.data_space_id
LEFT JOIN '+@database+N'.sys.partition_functions AS pf ON ps.function_id=pf.function_id
LEFT JOIN (
    SELECT function_id, boundary_id, CAST(SQL_VARIANT_PROPERTY([value], N''BaseType'') AS sysname) AS boundary_type,
           (CASE 
            WHEN CAST(SQL_VARIANT_PROPERTY([value], N''BaseType'') AS sysname)=N''date'' THEN LEFT(CONVERT(nvarchar(max), [value], 120), 10)
            WHEN CAST(SQL_VARIANT_PROPERTY([value], N''BaseType'') AS sysname) LIKE N''%datetime%'' THEN CONVERT(nvarchar(max), [value], 120)
            ELSE CAST([value] AS nvarchar(max)) END) AS boundary
    FROM '+@database+N'.sys.partition_range_values
    WHERE parameter_id=1
    ) AS prv ON pf.function_id=prv.function_id AND p.partition_number=prv.boundary_id'
    
INSERT INTO @syspartitions
EXEC(@temp);

-- Does this partition have a discrete boundary, i.e. can it only contain a single boundary value?
-- This applies to discrete datatypes, like integers, dates, etc.
UPDATE sub
SET sub.discrete_boundary=1
FROM (
    SELECT discrete_boundary,
           TRY_CAST(LAG(boundary, 1) OVER (PARTITION BY [object_id], index_id ORDER BY partition_number) AS bigint) AS a,
           TRY_CAST(boundary AS bigint) AS b,
           TRY_CAST(LEAD(boundary, 1) OVER (PARTITION BY [object_id], index_id ORDER BY partition_number) AS bigint) AS c
    FROM @syspartitions
    WHERE boundary_type IN (N'bit', N'tinyint', N'smallint', N'int', N'bigint')
    ) AS sub
WHERE a+1=b AND b+1=ISNULL(c, b+1);

UPDATE sub
SET sub.discrete_boundary=1
FROM (
    SELECT discrete_boundary,
           TRY_CAST(LAG(boundary, 1) OVER (PARTITION BY [object_id], index_id ORDER BY partition_number) AS date) AS a,
           TRY_CAST(boundary AS date) AS b,
           TRY_CAST(LEAD(boundary, 1) OVER (PARTITION BY [object_id], index_id ORDER BY partition_number) AS date) AS c
    FROM @syspartitions
    WHERE boundary_type IN (N'date')
    ) AS sub
WHERE DATEADD(day, 1, a)=b AND DATEADD(day, 1, b)=ISNULL(c, DATEADD(day, 1, b));

BEGIN TRY;
	INSERT INTO @syspartitionstats
	EXEC(N'
	SELECT ps.[partition_id], ps.row_count, ps.in_row_used_page_count, ps.reserved_page_count,
		   ps.row_overflow_used_page_count, ps.lob_used_page_count, ps.used_page_count
	FROM '+@database+N'.sys.dm_db_partition_stats AS ps
	INNER JOIN '+@database+N'.sys.partitions AS p ON ps.[partition_id]=p.[partition_id]');
END TRY
BEGIN CATCH;
	PRINT 'Problem compiling partition stats: '+ERROR_MESSAGE();
END CATCH;




BEGIN TRY;
    INSERT INTO @destination_data_spaces
    EXEC(N'
    SELECT partition_scheme_id, destination_id AS partition_number, data_space_id
    FROM '+@database+N'.sys.destination_data_spaces');
END TRY
BEGIN CATCH;
    PRINT 'Could not view sys.destination_data_spaces.';
END CATCH;




BEGIN TRY;
    INSERT INTO @columnstore_rowgroups
    EXEC(N'
    SELECT [object_id], index_id, partition_number, [state], SUM(total_rows), SUM(size_in_bytes)
    FROM '+@database+N'.sys.column_store_row_groups
    WHERE [object_id]='+@object_id_str+N'
    GROUP BY [object_id], index_id, partition_number, [state]');
END TRY
BEGIN CATCH;
	PRINT 'There''s no sys.column_store_row_groups.';
END CATCH;


INSERT INTO @sysdatabasepermissions
EXEC(N'
SELECT class, class_desc, major_id, minor_id, grantee_principal_id,
       grantor_principal_id, [type], [permission_name], [state], state_desc
FROM '+@database+N'.sys.database_permissions');

BEGIN TRY;
    INSERT INTO @signatures
    EXEC(N'
    SELECT N''ASYMMETRIC KEY'' AS src, ask.[name], (CASE ask.pvt_key_encryption_type WHEN N''PW'' THEN ''PASSWORD=N''''*****'''''' END) AS encryption_type_desc, sg.[type] AS [type_desc], sg.[entity_id] AS major_id
    FROM '+@database+N'.sys.asymmetric_keys AS ask
    CROSS APPLY '+@database+N'.sys.fn_check_object_signatures (N''asymmetric key'', ask.thumbprint) AS sg
    INNER JOIN '+@database+N'.sys.objects AS o ON sg.[entity_id]=o.[object_id] AND sg.[type]=o.[type_desc]
    WHERE sg.is_signed=1
    AND o.[object_id]='+@object_id_str+N';

    SELECT N''CERTIFICATE'' AS src, crt.[name], (CASE crt.pvt_key_encryption_type WHEN N''PW'' THEN ''PASSWORD=N''''*****'''''' END) AS encryption_type_desc, sg.[type] AS [type_desc], sg.[entity_id] AS major_id
    FROM '+@database+N'.sys.certificates AS crt
    CROSS APPLY '+@database+N'.sys.fn_check_object_signatures (N''certificate'', crt.thumbprint) AS sg
    INNER JOIN '+@database+N'.sys.objects AS o ON sg.[entity_id]=o.[object_id] AND sg.[type]=o.[type_desc]
    WHERE sg.is_signed=1
    AND o.[object_id]='+@object_id_str+';
    ');
END TRY
BEGIN CATCH;
    PRINT 'Could not view signatures DMV.';
END CATCH;

BEGIN TRY;
    INSERT INTO @syssecuritypolicies
    EXEC(N'
	    SELECT p.[object_id] AS security_policy_id,
               sp.security_predicate_id,
               sp.predicate_type_desc COLLATE database_default,
               sp.predicate_definition COLLATE database_default,
               p.is_enabled,
               p.is_schema_bound
	    FROM '+@database+N'.sys.security_policies AS p
	    INNER JOIN '+@database+N'.sys.security_predicates AS sp ON p.[object_id]=sp.[object_id]
	    INNER JOIN '+@database+N'.sys.objects AS o ON sp.target_object_id=o.[object_id]
	    INNER JOIN '+@database+N'.sys.schemas AS os ON o.[schema_id]=os.[schema_id]
        WHERE sp.target_object_id='+@object_id_str+';');

END TRY
BEGIN CATCH;
    PRINT 'Could not view sys.security_policies.';
END CATCH;


INSERT INTO @systriggers
EXEC(N'
SELECT t.[object_id], t.[name], t.is_disabled, t.is_instead_of_trigger,
       SUBSTRING(CAST((SELECT N'', ''+te.[type_desc]
                       FROM '+@database+N'.sys.trigger_events AS te
                       WHERE te.[object_id]=t.[object_id]
                       ORDER BY te.[type] FOR XML PATH(N''''), TYPE) AS sysname), 3, 256)
FROM '+@database+N'.sys.triggers AS t
WHERE parent_id='+@object_id_str);

-- https://docs.microsoft.com/en-us/sql/t-sql/language-elements/reserved-keywords-transact-sql
INSERT INTO @reserved_keywords (keyword)
VALUES ('ABSOLUTE'), ('ACTION'), ('ADA'), ('ADD'), ('ADMIN'), ('ADDRESS'), ('AFTER'), ('AGGREGATE'), ('ALIAS'), ('ALL'), ('ALLOCATE'), ('ALTER'), ('ALWAYS'), ('AND'), ('ANY'), ('ARE'), ('ARRAY'),
       ('AS'), ('ASC'), ('ASENSITIVE'), ('ASSERTION'), ('ASYMMETRIC'), ('AT'), ('ATOMIC'), ('AUTHORIZATION'), ('AVG'), ('BEFORE'), ('BEGIN'), ('BETWEEN'), ('BINARY'),
       ('BIT'), ('BIT_LENGTH'), ('BLOB'), ('BOOLEAN'), ('BOTH'), ('BREADTH'), ('BY'), ('CALL'), ('CALLED'), ('CARDINALITY'), ('CASCADE'), ('CASCADED'), ('CASE'), ('CAST'),
       ('CATALOG'), ('CHAR'), ('CHAR_LENGTH'), ('CHARACTER'), ('CHARACTER_LENGTH'), ('CHECK'), ('CLASS'), ('CLOB'), ('CLOSE'), ('COALESCE'), ('COLLATE'), ('COLLATION'),
       ('COLLECT'), ('COLUMN'), ('COMMIT'), ('COMPLETION'), ('CONDITION'), ('CONNECT'), ('CONNECTION'), ('CONSTRAINT'), ('CONSTRAINTS'), ('CONSTRUCTOR'), ('CONTINUE'),
       ('CONVERT'), ('CORR'), ('CORRESPONDING'), ('COUNT'), ('COVAR_POP'), ('COVAR_SAMP'), ('CREATE'), ('CROSS'), ('CUBE'), ('CUME_DIST'), ('CURRENT'), ('CURRENT_CATALOG'),
       ('CURRENT_DATE'), ('CURRENT_DEFAULT_TRANSFORM_GROUP'), ('CURRENT_PATH'), ('CURRENT_ROLE'), ('CURRENT_SCHEMA'), ('CURRENT_TIME'), ('CURRENT_TIMESTAMP'),
       ('CURRENT_TRANSFORM_GROUP_FOR_TYPE'), ('CURRENT_USER'), ('CURSOR'), ('CYCLE'), ('DATA'), ('DATE'), ('DAY'), ('DEALLOCATE'), ('DEC'), ('DECIMAL'), ('DECLARE'),
       ('DEFAULT'), ('DEFERRABLE'), ('DEFERRED'), ('DELETE'), ('DEPTH'), ('DEREF'), ('DESC'), ('DESCRIBE'), ('DESCRIPTOR'), ('DESTROY'), ('DESTRUCTOR'), ('DETERMINISTIC'),
       ('DIAGNOSTICS'), ('DICTIONARY'), ('DISCONNECT'), ('DISTINCT'), ('DOMAIN'), ('DOUBLE'), ('DROP'), ('DYNAMIC'), ('EACH'), ('ELEMENT'), ('ELSE'), ('END'), ('END-EXEC'),
       ('EQUALS'), ('ESCAPE'), ('EVERY'), ('EXCEPT'), ('EXCEPTION'), ('EXEC'), ('EXECUTE'), ('EXISTS'), ('EXTERNAL'), ('EXTRACT'), ('FALSE'), ('FETCH'), ('FILTER'), ('FIRST'),
       ('FLOAT'), ('FOR'), ('FOREIGN'), ('FORTRAN'), ('FOUND'), ('FREE'), ('FROM'), ('FULL'), ('FULLTEXTTABLE'), ('FUSION'), ('GENERAL'), ('GENERATED'), ('GET'), ('GLOBAL'), ('GO'), ('GOTO'),
       ('GRANT'), ('GROUP'), ('GROUPING'), ('HAVING'), ('HIDDEN'), ('HOLD'), ('HOST'), ('HOUR'), ('IDENTITY'), ('IGNORE'), ('IMMEDIATE'), ('IN'), ('INCLUDE'), ('INDEX'), ('INDICATOR'),
       ('INITIALIZE'), ('INITIALLY'), ('INNER'), ('INOUT'), ('INPUT'), ('INSENSITIVE'), ('INSERT'), ('INT'), ('INTEGER'), ('INTERSECT'), ('INTERSECTION'), ('INTERVAL'),
       ('INTO'), ('IS'), ('ISOLATION'), ('ITERATE'), ('JOIN'), ('KEY'), ('LANGUAGE'), ('LARGE'), ('LAST'), ('LATERAL'), ('LEADING'), ('LEFT'), ('LESS'), ('LEVEL'), ('LIKE'),
       ('LIKE_REGEX'), ('LIMIT'), ('LN'), ('LOCAL'), ('LOCALTIME'), ('LOCALTIMESTAMP'), ('LOCATOR'), ('LOWER'), ('MAP'), ('MATCH'), ('MAX'), ('MEMBER'), ('METHOD'), ('MIN'),
       ('MINUTE'), ('MOD'), ('MODIFIES'), ('MODIFY'), ('MODULE'), ('MONTH'), ('MULTISET'), ('NAME'), ('NAMES'), ('NATIONAL'), ('NATURAL'), ('NCHAR'), ('NCLOB'), ('NEW'), ('NEXT'),
       ('NO'), ('NONE'), ('NORMALIZE'), ('NOT'), ('NULL'), ('NULLIF'), ('NUMERIC'), ('OBJECT'), ('OCCURRENCES_REGEX'), ('OCTET_LENGTH'), ('OF'), ('OLD'), ('ON'), ('ONLY'),
       ('OPEN'), ('OPERATION'), ('OPTION'), ('OR'), ('ORDER'), ('ORDINALITY'), ('OUT'), ('OUTER'), ('OUTPUT'), ('OVERLAPS'), ('OVERLAY'), ('PAD'), ('PARAMETER'), ('PARAMETERS'),
       ('PARTIAL'), ('PARTITION'), ('PASCAL'), ('PATH'), ('PERCENT_RANK'), ('PERCENTILE_CONT'), ('PERCENTILE_DISC'), ('POSITION'), ('POSITION_REGEX'), ('POSTFIX'), ('PRECISION'),
       ('PREFIX'), ('PREORDER'), ('PREPARE'), ('PRESERVE'), ('PRIMARY'), ('PRIOR'), ('PRIVILEGES'), ('PROCEDURE'), ('PUBLIC'), ('RANGE'), ('READ'), ('READS'), ('REAL'),
       ('RECURSIVE'), ('REF'), ('REFERENCES'), ('REFERENCING'), ('REGR_AVGX'), ('REGR_AVGY'), ('REGR_COUNT'), ('REGR_INTERCEPT'), ('REGR_R2'), ('REGR_SLOPE'), ('REGR_SXX'),
       ('REGR_SXY'), ('REGR_SYY'), ('RELATIVE'), ('RELEASE'), ('RESTRICT'), ('RESULT'), ('RETURNS'), ('REVOKE'), ('RIGHT'), ('ROLE'), ('ROLLBACK'), ('ROLLUP'), ('ROUTINE'),
       ('ROW'), ('ROWS'), ('SAVEPOINT'), ('SCHEMA'), ('SCOPE'), ('SCROLL'), ('SEARCH'), ('SECOND'), ('SECTION'), ('SELECT'), ('SENSITIVE'), ('SEQUENCE'), ('SESSION'),
       ('SESSION_USER'), ('SET'), ('SETS'), ('SIMILAR'), ('SIZE'), ('SMALLINT'), ('SOME'), ('SPACE'), ('SPECIFIC'), ('SPECIFICTYPE'), ('SQL'), ('SQLCA'), ('SQLCODE'),
       ('SQLERROR'), ('SQLEXCEPTION'), ('SQLSTATE'), ('SQLWARNING'), ('START'), ('STATE'), ('STATEMENT'), ('STATIC'), ('STDDEV_POP'), ('STDDEV_SAMP'), ('STRUCTURE'),
       ('SUBMULTISET'), ('SUBSTRING'), ('SUBSTRING_REGEX'), ('SUM'), ('SYMMETRIC'), ('SYSTEM'), ('SYSTEM_USER'), ('TABLE'), ('TEMPORARY'), ('TERMINATE'), ('THAN'), ('THEN'),
       ('TIME'), ('TIMESTAMP'), ('TIMEZONE_HOUR'), ('TIMEZONE_MINUTE'), ('TO'), ('TRAILING'), ('TRANSACTION'), ('TRANSLATE'), ('TRANSLATE_REGEX'), ('TRANSLATION'), ('TREAT'),
       ('TRIM'), ('TRUE'), ('TYPE'), ('UESCAPE'), ('UNDER'), ('UNION'), ('UNIQUE'), ('UNKNOWN'), ('UNNEST'), ('UPDATE'), ('UPPER'), ('USAGE'), ('USER'), ('USING'), ('VALUE'), ('VALUES'),
       ('VAR_POP'), ('VAR_SAMP'), ('VARCHAR'), ('VARIABLE'), ('VARYING'), ('WHEN'), ('WHENEVER'), ('WHERE'), ('WIDTH_BUCKET'), ('VIEW'), ('WINDOW'), ('WITH'), ('WITHIN'),
       ('WITHOUT'), ('WORK'), ('WRITE'), ('XMLAGG'), ('XMLATTRIBUTES'), ('XMLBINARY'), ('XMLCAST'), ('XMLCOMMENT'), ('XMLCONCAT'), ('XMLDOCUMENT'), ('XMLELEMENT'), ('XMLEXISTS'),
       ('XMLFOREST'), ('XMLITERATE'), ('XMLNAMESPACES'), ('XMLPARSE'), ('XMLPI'), ('XMLQUERY'), ('XMLSERIALIZE'), ('XMLTABLE'), ('XMLTEXT'), ('XMLVALIDATE'), ('YEAR'), ('ZONE');

INSERT INTO @reserved_keywords (keyword)
SELECT [name] COLLATE database_default FROM sys.types WHERE system_type_id=user_type_id
EXCEPT SELECT keyword FROM @reserved_keywords;


-------------------------------------------------------------------------------
--- Depending on object type and what relevant attributes an object has, we're
--- displaying as few recordsets as possible.


SELECT TOP 1 @has_cols_or_params=(CASE WHEN col.[object_id] IS NOT NULL OR par.parameter_id IS NOT NULL THEN 1 ELSE 0 END),
             @has_indexes=       (CASE WHEN ix.[object_id] IS NOT NULL THEN 1 ELSE 0 END),
             @has_foreign_keys=  (CASE WHEN fk.parent_object_id IS NOT NULL THEN 1 ELSE 0 END),
             @has_references=    (CASE WHEN fk.parent_object_id IS NOT NULL OR dep.referencing_id IS NOT NULL THEN 1 ELSE 0 END),
             @has_permissions=   (CASE WHEN per.major_id IS NOT NULL THEN 1 ELSE 0 END),
             @has_sql_module=    (CASE WHEN @module_definition IS NOT NULL THEN 1 ELSE 0 END),
             @has_data=          (CASE WHEN obj.[type] IN ('IT', 'U', 'S') OR obj.[type]='V' AND ix.index_id IS NOT NULL THEN 1 ELSE 0 END),
             @has_policies=      (CASE WHEN pol.security_policy_id IS NOT NULL THEN 1 ELSE 0 END),
             @rowcount=          (SELECT SUM([rows]) FROM @syspartitions WHERE [object_id]=@object_id AND index_id IN (0, 1)),
             @type=              obj.[type]
FROM @sysobjects AS obj
LEFT JOIN @syscolumns AS col ON col.[object_id]=@object_id
LEFT JOIN @sysparameters AS par ON 1=1
LEFT JOIN @sysindexes AS ix ON ix.[object_id]=@object_id AND ix.index_id!=0 -- no heaps
LEFT JOIN @sysforeignkeys AS fk ON @object_id IN (fk.parent_object_id, fk.referenced_object_id)
LEFT JOIN @sysexprdependencies AS dep ON @object_id IN (dep.referencing_id, dep.referenced_id)
LEFT JOIN @sysdatabasepermissions AS per ON per.class=1 AND per.major_id=@object_id
LEFT JOIN @syssecuritypolicies AS pol ON 1=1
WHERE obj.[object_id]=@object_id;

IF (EXISTS (SELECT NULL FROM @signatures)) SET @has_permissions=1;



--- SECURITY POLICY, introduced in SQL Server 2016 to implement row-based security:
IF (@type='SP') BEGIN;
	BEGIN TRY;
		SET @has_sql_module=1;

		INSERT INTO @definition (id, [definition])
		EXEC(N'
			SELECT 1, N''CREATE SECURITY POLICY [''+s.[name] COLLATE database_default+N''].[''+p.[name] COLLATE database_default+N'']''
			FROM '+@database+N'.sys.schemas AS s
			INNER JOIN '+@database+N'.sys.security_policies AS p ON s.[schema_id]=p.[schema_id]
			WHERE p.[object_id]='+@object_id_str+N'

			UNION ALL

			SELECT 1+ROW_NUMBER() OVER (ORDER BY sp.security_predicate_id),
				    N''   ADD ''+sp.predicate_type_desc COLLATE database_default+N'' PREDICATE ''+sp.predicate_definition COLLATE database_default+N'' ON [''+os.[name]+N''].[''+o.[name]+N'']''+
					(CASE WHEN LEAD(1, 1, 0) OVER (ORDER BY sp.security_predicate_id)=1
							THEN N'',''
							ELSE N'' WITH (STATE=''+(CASE WHEN p.is_enabled=1 THEN N''ON'' ELSE N''OFF'' END)+
												(CASE WHEN p.is_schema_bound=1 THEN N'', SCHEMABINDING=ON'' ELSE N'''' END)+N'')'' END) AS [Definition]
			FROM '+@database+N'.sys.security_policies AS p
			INNER JOIN '+@database+N'.sys.security_predicates AS sp ON p.[object_id]=sp.[object_id]
			INNER JOIN '+@database+N'.sys.objects AS o ON sp.target_object_id=o.[object_id]
			INNER JOIN '+@database+N'.sys.schemas AS os ON o.[schema_id]=os.[schema_id]
			WHERE p.[object_id]='+@object_id_str+N';');

		SET @module_definition=N'';
		SET @has_sql_module=1;

		SELECT @module_definition=@module_definition+[definition]+@lf
		FROM @definition
		ORDER BY id;

	END TRY
	BEGIN CATCH
		PRINT 'That''s strange... '+ERROR_MESSAGE();
	END CATCH;
END;





--- Header: Name, owner, type, data space, options/schemabinding/compression
SELECT (CASE WHEN @is_tempdb=1 THEN '' ELSE sch.[name] END) AS [Schema],
       obj.[name] AS [Object],
       SUBSTRING(REPLACE(t.[name], '_', ' ') COLLATE catalog_default, 5, LEN(t.[name]))+ISNULL((CASE
			WHEN obj.is_memory_optimized=0 AND ix.[type_desc] IS NOT NULL THEN ' ('+LOWER(ix.[type_desc])+')'
			WHEN m.uses_native_compilation=1 THEN ' (native compilation)'
            WHEN m.is_schema_bound=1 THEN ' (with schemabinding)'
			WHEN obj.is_memory_optimized=1 THEN ' (memory optimized, '+REPLACE(LOWER(obj.durability_desc), '_', ' ')+')'
			ELSE '' END), '') AS [Type],
       obj.[object_id],
       own.[name]+(CASE WHEN obj.principal_id IS NULL THEN N' (schema owner)' ELSE N'' END) AS [Owner],
       (CASE WHEN ix.index_id IS NOT NULL THEN 'ON '+QUOTENAME(ds.[name])+ISNULL(N'('+c.[name]+N')', N'') ELSE N'' END) AS [Data space],
       ISNULL(N'WITH ('+NULLIF(SUBSTRING(
                (CASE WHEN obj.history_table_id IS NOT NULL
                      THEN ', SYSTEM_VERSIONING=ON (HISTORY_TABLE='+ISNULL(
                            (SELECT hs.[name]+'.'+ho.[name]
                             FROM @sysschemas AS hs
                             INNER JOIN @sysobjects AS ho ON hs.[schema_id]=ho.[schema_id]
                             WHERE ho.[object_id]=obj.history_table_id), N'##missing##')+')'
                      ELSE '' END)+
                (CASE WHEN obj.is_memory_optimized=1
                      THEN N', MEMORY_OPTIMIZED=ON, DURABILITY='+obj.durability_desc
                      ELSE N'' END)+
                ISNULL(N', DATA_COMPRESSION='+NULLIF(NULLIF(p.data_compression_desc, N'NONE'), N'COLUMNSTORE'), N'')+
                ISNULL(N', XML_COMPRESSION='+NULLIF(p.xml_compression_desc, 'OFF'), N''),
        	3, 1000), N'')+N')', N'') AS [Options],
       (CASE WHEN obj.is_change_tracked=1 THEN (CASE WHEN obj.is_track_columns_updated_on=1 THEN 'Column updates' ELSE 'Table' END) ELSE '' END) AS [Change tracking],
       ISNULL(CAST(NULLIF(obj.min_valid_version, 0) AS varchar(20)), '') AS [CT version],
       (CASE WHEN obj.[type] IN ('IT', 'U', 'S') OR obj.[type]='V' AND ix.index_id=1 THEN
           (SELECT ISNULL(REPLACE(REPLACE(CONVERT(varchar(100), CAST(NULLIF(SUM(sub.[rows]), 0) AS money), 1), ',', ' '), '.00', '')+
                          (CASE WHEN COUNT(*)>1 THEN ' rows in '+CAST(COUNT(*) AS varchar(10))+' partitions' ELSE '' END),
    		      '(empty)')
            FROM @syspartitions AS sub
            WHERE sub.[object_id]=@object_id AND sub.index_id IN (0, 1))
         ELSE ''
	 END) AS [Row count],
     (SELECT ISNULL(STR(NULLIF(SUM(1.0*ps.used_page_count)*8/1024, 0), 12, 2)+' MB', '')
      FROM @syspartitionstats AS ps
      RIGHT JOIN @syspartitions AS p ON ps.[partition_id]=p.[partition_id]
      LEFT JOIN @sysindexes AS ix ON p.[object_id]=ix.[object_id] AND p.index_id=ix.index_id
      WHERE p.[object_id]=@object_id) AS [Total size],
     ISNULL(CAST(ep.[value] AS nvarchar(max)), N'') AS [Description]
FROM @sysschemas AS sch
INNER JOIN @sysobjects AS obj ON sch.[schema_id]=obj.[schema_id]
LEFT JOIN @sysdatabaseprincipals AS own ON ISNULL(obj.principal_id, sch.principal_id)=own.principal_id
LEFT JOIN @spt_values_O9T AS t ON RTRIM(LEFT(t.[name], 2)) COLLATE catalog_default=obj.[type]
LEFT JOIN @sysindexes AS ix ON obj.[object_id]=ix.[object_id] AND ix.index_id IN (0, 1)
LEFT JOIN @sysdataspaces AS ds ON ix.data_space_id=ds.data_space_id
LEFT JOIN @sysindexcolumns AS pc ON ds.[type]='PS' AND ix.[object_id]=pc.[object_id] AND ix.index_id=pc.index_id AND pc.partition_ordinal>0
LEFT JOIN @syscolumns AS c ON pc.[object_id]=c.[object_id] AND pc.column_id=c.column_id
LEFT JOIN @syspartitions AS p ON ds.[type]!='PS' AND ix.[object_id]=p.[object_id] AND ix.index_id=p.index_id
LEFT JOIN @syssqlmodules AS m ON 1=1
LEFT JOIN @extended_properties AS ep ON ep.[object_id]=@object_id AND ep.column_id=0 AND ep.[name]=N'MS_Description'
WHERE obj.[object_id]=@object_id;










--- Columns: Name/computed/persisted, type/length/prec/scale, identity/default, null/sparse, collation
IF (@has_cols_or_params=1) BEGIN;
	SELECT (CASE WHEN col.parameter_id IS NOT NULL AND
			  col.[name]=N'' AND
			  col.is_output=1 THEN N'RETURNS ' ELSE N'' END)+
	       (CASE WHEN col.[name] LIKE N'[0-9]%' OR col.[name] LIKE N'%[^0-9a-z\_@]%' ESCAPE N'\' OR col.[name] IN (SELECT keyword FROM @reserved_keywords) THEN QUOTENAME(col.[name]) ELSE col.[name] END)+
                 (CASE WHEN obj.[type]!='SO' THEN ISNULL(N' AS '+col.[definition]+
				 (CASE col.is_persisted
				  WHEN 0 THEN N''
				  WHEN 1 THEN N' PERSISTED' END), N'') ELSE N'' END) AS [Column],
	       (CASE WHEN obj.[type]='SO' THEN N'AS ' ELSE N'' END)+
	       (CASE WHEN col.is_persisted IS NULL THEN
               col.[type_name]+(CASE
			       WHEN col.user_type_id!=col.system_type_id THEN ''
			       WHEN col.[type_name] LIKE N'n%char%' THEN N'('+ISNULL(CAST(NULLIF(col.max_length, -1)/2 AS varchar(max)), N'max')+N')'
			       WHEN col.[type_name] LIKE N'%char%' OR col.[type_name] LIKE N'%binary%' THEN N'('+ISNULL(CAST(NULLIF(col.max_length, -1)   AS varchar(max)), N'max')+N')'
			       WHEN col.[type_name] IN (N'numeric', N'decimal') THEN N'('+CAST(col.[precision] AS varchar(max))+N', '+CAST(col.scale AS varchar(max))+N')'
			       WHEN col.[type_name]=N'datetime2' THEN N'('+CAST(col.scale AS varchar(max))+N')'
			       WHEN col.[type_name]=N'xml' THEN ISNULL(N'('+xsc_sch.[name]+N'.'+xsc.[name]+N')', N'')
			       ELSE N''
			       END) ELSE N'' END) AS [Datatype],
	       (CASE WHEN obj.[type]='SO' THEN N'START WITH '+CAST(col.seed_value AS nvarchar(40))+
                                           N' INCREMENT BY '+CAST(col.increment_value AS nvarchar(40))
                 WHEN col.generated_always_type_desc=N'AS_ROW_START' THEN N'GENERATED ALWAYS AS ROW START'
                 WHEN col.generated_always_type_desc=N'AS_ROW_END' THEN N'GENERATED ALWAYS AS ROW END'
                 WHEN col.seed_value IS NOT NULL THEN ISNULL(N'IDENTITY('+CAST(col.seed_value AS nvarchar(40))+N', '+CAST(col.increment_value AS nvarchar(40))+N')', N'')
	             WHEN col.default_name IS NOT NULL THEN (CASE WHEN col.default_is_system_named=0 THEN N'CONSTRAINT '+col.default_name+N' ' ELSE N'' END)+
	                                                    (CASE WHEN col.default_name IS NOT NULL THEN ISNULL(N'DEFAULT '+col.[definition], N'') ELSE N'' END)
                 ELSE N''
                 END)+
           (CASE WHEN col.is_hidden=1 THEN N' HIDDEN'
                 ELSE N''
                 END) AS [Ident/default],
	       ISNULL(N'COLLATE '+NULLIF(col.collation_name, CAST(DATABASEPROPERTYEX(DB_NAME(), N'collation') AS nvarchar(255))), N'') AS [Collation],
	       (CASE WHEN obj.[type]='SO' THEN N''
                 WHEN col.column_id IS NULL THEN N''
	             WHEN col.is_sparse=1 THEN N'SPARSE NULL'
		         WHEN col.is_nullable=1 THEN N'NULL'
		         ELSE N'NOT NULL' END) AS [NULL],
	       ISNULL(NULLIF(SUBSTRING(
		   (CASE WHEN (col.[type_name] LIKE N'%char%' OR col.[type_name] LIKE N'%bin%') AND col.is_ansi_padded=0 THEN N'  /* ANSI_PADDING OFF */' ELSE N'' END)+
		   (CASE WHEN col.is_output=1 THEN N', OUTPUT' ELSE N'' END)+
		   (CASE WHEN col.is_readonly=1 THEN N', READONLY' ELSE N'' END)+
		   ISNULL(N'  /* '+col.tbl_type_cols+N' */', ''),
		   3, 8000), N''), N'')+
           (CASE WHEN obj.[type]='SO' THEN ISNULL(N' '+[definition], N'') ELSE N'' END) AS [Options],
		   (CASE WHEN obj.[type]='SO' THEN N'Current value '+CAST(col.current_value AS nvarchar(40))
                 WHEN ROW_NUMBER() OVER (ORDER BY col.is_output, COALESCE(col.parameter_id, col.column_id+999))=COUNT(*) OVER (PARTITION BY (SELECT NULL)) THEN N'' ELSE N',' END) AS [ ],
           ISNULL(N'--> '+ref.[name], N'') AS [References],
           ISNULL(N'-- '+CAST(ep.[value] AS nvarchar(max)), N'') AS [Description],
           ISNULL(N'-- '+CAST(col.max_alloc_size AS nvarchar(20))+N' bytes', N'') AS [Max storage]
	FROM (SELECT column_id, CAST(NULL AS int) AS parameter_id, name, user_type_id, system_type_id,
		         max_length, [precision], scale, is_sparse, is_nullable, collation_name,
		         is_ansi_padded, xml_collection_id, default_object_id, CAST(NULL AS bit) AS is_output,
                 CAST(NULL AS bit) AS is_readonly, CAST(NULL AS bit) AS is_table_type,
		         seed_value, increment_value, [definition], is_persisted, [type_name],
		         default_name, default_is_system_named, CAST(NULL AS varchar(max)) AS tbl_type_cols,
                 current_value, max_alloc_size, generated_always_type_desc, is_hidden
	      FROM @syscolumns
	      WHERE [object_id]=@object_id
          UNION ALL
	      SELECT CAST(NULL AS int) AS column_id, parameter_id, [name], user_type_id, system_type_id,
		         max_length, [precision], scale, NULL, is_nullable, NULL,
		         NULL, xml_collection_id, NULL, is_output, is_readonly,
                 is_table_type, NULL, NULL, NULL, NULL, [type_name], NULL, NULL, tbl_type_cols,
                 NULL AS current_value, NULL AS max_alloc_size, NULL AS generated_always_type_desc, NULL AS is_hidden
	      FROM @sysparameters
	      ) AS col
    OUTER APPLY (
        SELECT TOP (1) fks.[name]+N'.'+fko.[name]+N'('+fkc.[name]+N')' AS [name]
        FROM @sysforeignkeycols AS fk
        INNER JOIN @sysobjects AS fko ON fk.referenced_object_id=fko.[object_id]
        INNER JOIN @syscolumns AS fkc ON fk.referenced_object_id=fkc.[object_id] AND fk.referenced_column_id=fkc.column_id
        INNER JOIN @sysschemas AS fks ON fko.[schema_id]=fks.[schema_id]
        WHERE fk.parent_object_id=@object_id AND fk.parent_column_id=col.column_id
        ) AS ref
	LEFT JOIN @xmlschemacollections AS xsc ON col.xml_collection_id=xsc.xml_collection_id
    LEFT JOIN @extended_properties AS ep ON ep.[object_id]=@object_id AND ep.column_id=col.column_id AND ep.[name]=N'MS_Description'
	LEFT JOIN @sysschemas AS xsc_sch ON xsc.[schema_id]=xsc_sch.[schema_id]
    LEFT JOIN @sysobjects AS obj ON obj.[object_id]=@object_id

    UNION ALL

    --- Add space potential space requirement for uniqifiers:
    SELECT N'', N'', N'', N'', N'', N'', N'', N'', N'', N'-- 4 bytes (uniquifier)'
    FROM @sysindexes
    WHERE [object_id]=@object_id AND index_id IN (0, 1) AND is_unique=0

    UNION ALL

    --- Add temporal table syntax:
    SELECT N'', N'', N', PERIOD FOR SYSTEM_TIME ('+
        MAX((CASE WHEN generated_always_type_desc='AS_ROW_START' THEN [name] ELSE N'' END))+N', '+
        MAX((CASE WHEN generated_always_type_desc='AS_ROW_END' THEN [name] ELSE N'' END))+N')', N'', N'', N'', N'', N'', N'', N''
	FROM @syscolumns
	WHERE [object_id]=@object_id
      AND generated_always_type_desc IN (N'AS_ROW_START', N'AS_ROW_END')
    HAVING COUNT(*)>0;
END;



--- Indexes: Name, constraint type, type/unique, data space, columns, included, options/compression
IF (@has_indexes=1)
	WITH ixc AS (
		SELECT ic.index_id, ic.is_included_column,
		       ROW_NUMBER() OVER (
			   PARTITION BY ic.index_id, ic.is_included_column
			   ORDER BY ic.key_ordinal) AS ordinal,
		       (CASE WHEN c.[name] LIKE N'[0-9]%' OR c.[name] LIKE N'%[^0-9a-z\_@]%' ESCAPE N'\' OR c.[name] IN (SELECT keyword FROM @reserved_keywords) THEN QUOTENAME(c.[name]) ELSE c.[name] END)+(CASE WHEN ic.is_descending_key=1 THEN N' DESC' ELSE N'' END) AS [name],
		       (CASE WHEN c.[name] LIKE N'[0-9]%' OR c.[name] LIKE N'%[^0-9a-z\_@]%' ESCAPE N'\' OR c.[name] IN (SELECT keyword FROM @reserved_keywords) THEN QUOTENAME(c.[name]) ELSE c.[name] END) AS name_plain
		FROM @sysindexcolumns AS ic
		INNER JOIN @syscolumns AS c ON ic.[object_id]=c.[object_id] AND ic.column_id=c.column_id
		WHERE ic.[object_id]=@object_id
          AND (key_ordinal>0 OR is_included_column=1)),

	     rcte AS (
		SELECT index_id, is_included_column, ordinal,
               CAST([name] AS nvarchar(max)) AS list,
               CAST(name_plain AS nvarchar(max)) AS list_plain
		FROM ixc
		WHERE ordinal=1
        
		UNION ALL
        
		SELECT rcte.index_id,
		       rcte.is_included_column,
		       ixc.ordinal,
		       CAST(rcte.list+N', '+ixc.[name] AS nvarchar(max)),
		       CAST(rcte.list_plain+N', '+ixc.name_plain AS nvarchar(max))
		FROM rcte
		INNER JOIN ixc ON
		    rcte.index_id=ixc.index_id AND
		    rcte.is_included_column=ixc.is_included_column AND
		    rcte.ordinal+1=ixc.ordinal),

    --- Partition compression
    part AS (
        SELECT [object_id], index_id, [group], data_compression_desc,
               (CASE WHEN partition_count=COUNT(*) THEN N'ALL' ELSE CAST(MIN(partition_number) AS varchar(20))+ISNULL(N' TO '+CAST(NULLIF(MAX(partition_number), MIN(partition_number)) AS varchar(20)), N'') END) AS partition_list
        FROM (
            SELECT [object_id], index_id, partition_number, data_compression_desc, partition_count, SUM(segment) OVER (PARTITION BY [object_id], index_id ORDER BY partition_number ROWS UNBOUNDED PRECEDING) AS [group]
            FROM (
                SELECT [object_id], index_id, partition_number, data_compression_desc,
                       (CASE WHEN LAG(data_compression_desc, 1, N'') OVER (PARTITION BY [object_id], index_id ORDER BY partition_number)!=data_compression_desc THEN 1 ELSE 0 END) AS segment,
                       COUNT(*) OVER (PARTITION BY index_id) AS partition_count,
                       (CASE WHEN MIN(data_compression_desc) OVER (PARTITION BY index_id)
                                !=MAX(data_compression_desc) OVER (PARTITION BY index_id) THEN 1 ELSE 0 END) AS different_compression_settings
                FROM @syspartitions
                WHERE [object_id]=@object_id
                ) AS x
            WHERE partition_count>1
              AND different_compression_settings=1
            ) AS x
        WHERE data_compression_desc!=N'NONE'
        GROUP BY [object_id], index_id, [group], data_compression_desc, partition_count),

    part2 AS (
        SELECT [object_id], index_id, N'DATA_COMPRESSION='+data_compression_desc+ISNULL(N' ON PARTITIONS ('+
               NULLIF(SUBSTRING(CAST((SELECT N', '+partition_list
                                      FROM part AS p2
                                      WHERE p2.[object_id]=p2.[object_id] AND p2.index_id=p1.index_id AND p2.data_compression_desc=p1.data_compression_desc
                                      ORDER BY [group] FOR XML PATH(''), TYPE) AS varchar(max)), 3, 1000), N'ALL')+N')', N'') AS [definition]
        FROM part AS p1
        GROUP BY [object_id], index_id, data_compression_desc),

    xpart AS (
        SELECT [object_id], index_id, [group], xml_compression_desc,
               (CASE WHEN partition_count=COUNT(*) THEN N'ALL' ELSE CAST(MIN(partition_number) AS varchar(20))+ISNULL(N' TO '+CAST(NULLIF(MAX(partition_number), MIN(partition_number)) AS varchar(20)), N'') END) AS partition_list
        FROM (
            SELECT [object_id], index_id, partition_number, xml_compression_desc, partition_count, SUM(segment) OVER (PARTITION BY [object_id], index_id ORDER BY partition_number ROWS UNBOUNDED PRECEDING) AS [group]
            FROM (
                SELECT [object_id], index_id, partition_number, xml_compression_desc,
                       (CASE WHEN LAG(xml_compression_desc, 1, N'') OVER (PARTITION BY [object_id], index_id ORDER BY partition_number)!=xml_compression_desc THEN 1 ELSE 0 END) AS segment,
                       COUNT(*) OVER (PARTITION BY index_id) AS partition_count,
                       (CASE WHEN MIN(xml_compression_desc) OVER (PARTITION BY index_id)
                                !=MAX(xml_compression_desc) OVER (PARTITION BY index_id) THEN 1 ELSE 0 END) AS different_compression_settings
                FROM @syspartitions
                WHERE [object_id]=@object_id
                ) AS x
            WHERE partition_count>1
              AND different_compression_settings=1
            ) AS x
        WHERE xml_compression_desc!=N'OFF'
        GROUP BY [object_id], index_id, [group], xml_compression_desc, partition_count),

    xpart2 AS (
        SELECT [object_id], index_id, N'XML_COMPRESSION='+xml_compression_desc+ISNULL(N' ON PARTITIONS ('+
               NULLIF(SUBSTRING(CAST((SELECT N', '+partition_list
                                      FROM xpart AS p2
                                      WHERE p2.[object_id]=p2.[object_id] AND p2.index_id=p1.index_id AND p2.xml_compression_desc=p1.xml_compression_desc
                                      ORDER BY [group] FOR XML PATH(''), TYPE) AS varchar(max)), 3, 1000), N'ALL')+N')', N'') AS [definition]
        FROM xpart AS p1
        GROUP BY [object_id], index_id, xml_compression_desc)

    SELECT (CASE WHEN ix.is_primary_key=0 AND ix.is_unique_constraint=0 AND ix.is_unique=1 THEN N'UNIQUE ' ELSE N'' END)+
           (CASE WHEN ix.is_primary_key=0 AND ix.is_unique_constraint=0 AND ix.[type]=1 THEN N'CLUSTERED ' ELSE N'' END)+
           (CASE WHEN ix.[type]>=3 THEN REPLACE(ix.[type_desc], N'NONCLUSTERED ', N'')+' ' ELSE N'' END)+
           (CASE WHEN 1 IN (ix.is_primary_key, ix.is_unique_constraint) THEN N'CONSTRAINT' ELSE N'INDEX' END) AS [Type],
           (CASE WHEN ix.is_system_named=0 THEN ix.[name] ELSE '' END) AS [Index/constraint name],
           (CASE WHEN ix.is_primary_key=1 THEN N'PRIMARY KEY '+ix.[type_desc]
    	     WHEN ix.is_unique_constraint=1 THEN N'UNIQUE CONSTRAINT '+ix.[type_desc]
    	     ELSE N'' END) AS [Constraint type],
           ISNULL(N'('+(SELECT TOP 1 rcte.list FROM rcte WHERE rcte.index_id=ix.index_id AND (rcte.is_included_column=0 OR ix.[type]=6) ORDER BY rcte.ordinal DESC)+N')', N'') AS [Index columns],
           ISNULL(N'INCLUDE ('+(SELECT TOP 1 rcte.list_plain FROM rcte WHERE rcte.index_id=ix.index_id AND rcte.is_included_column=1 AND ix.[type] IN (1, 2) ORDER BY rcte.ordinal DESC)+N')', N'') AS [Includes],
           ISNULL(N' WHERE '+ix.filter_definition COLLATE database_default, N'') AS [Filter],
	   ISNULL(N'WITH ('+NULLIF(SUBSTRING(
	              ISNULL(', DATA_COMPRESSION='+NULLIF(NULLIF(p.data_compression_desc, N'NONE'), N'COLUMNSTORE'), N'')+
                  ISNULL(CAST((SELECT N', '+[definition] FROM part2 WHERE part2.[object_id]=ix.[object_id] AND part2.index_id=ix.index_id FOR XML PATH(N''), TYPE) AS varchar(max)), N'')+
                  ISNULL(', XML_COMPRESSION='+NULLIF(p.xml_compression_desc, N'OFF'), N'')+
                  ISNULL(CAST((SELECT N', '+[definition] FROM xpart2 WHERE xpart2.[object_id]=ix.[object_id] AND xpart2.index_id=ix.index_id FOR XML PATH(N''), TYPE) AS varchar(max)), N'')+
                  ISNULL(N', COMPRESSION_DELAY='+CAST(ix.[compression_delay] AS varchar(10))+N' MINUTES', '')+
            (CASE WHEN ix.[type] IN (1, 2)
	              THEN (CASE WHEN ix.fill_factor!=@default_fill_factor THEN N', FILLFACTOR='+ISNULL(NULLIF(CAST(ix.fill_factor AS varchar(max)), N'0'), N'100') ELSE N'' END)+
	                   N', ALLOW_ROW_LOCKS='+(CASE ix.[allow_row_locks] WHEN 1 THEN N'ON' ELSE N'OFF' END)+
	                   N', ALLOW_PAGE_LOCKS='+(CASE ix.[allow_page_locks] WHEN 1 THEN N'ON' ELSE N'OFF' END)+
	                   (CASE WHEN ix.fill_factor!=0 THEN N', PAD_INDEX='+(CASE ix.is_padded WHEN 1 THEN N'ON' ELSE N'OFF' END) ELSE N'' END)
                  ELSE N'' END)+
                  ISNULL(N', BUCKET_COUNT='+CAST(ix.[bucket_count] AS varchar(10)), N'')
            , 3, 10000), N'')+N')', N'') AS [Options],
	   (CASE WHEN ix.index_id IS NOT NULL AND ds.is_default=0 THEN N'ON '+QUOTENAME(ds.[name])+ISNULL(N'('+c.[name]+N')', '') ELSE N'' END) AS [Data space],
           (SELECT ISNULL(REPLACE(REPLACE(CONVERT(nvarchar(100), CAST(SUM(sub.[rows]) AS money), 1), N',', N' '), N'.00', N'')+
	           ISNULL(N' rows in '+CAST(NULLIF(COUNT(*), 1) AS varchar(10))+N' partitions', N''), N'')
            FROM @syspartitions AS sub
            WHERE sub.[object_id]=@object_id AND sub.index_id=ix.index_id AND ix.has_filter=1) AS [Filtered rows]
	FROM @sysindexes AS ix
	LEFT JOIN @sysdataspaces AS ds ON ix.data_space_id=ds.data_space_id
	LEFT JOIN @sysindexcolumns AS pc ON ds.[type]='PS' AND ix.[object_id]=pc.[object_id] AND ix.index_id=pc.index_id AND pc.partition_ordinal>0
	LEFT JOIN @syscolumns AS c ON pc.[object_id]=c.[object_id] AND pc.column_id=c.column_id
	LEFT JOIN @syspartitions AS p ON ds.[type]!='PS' AND ix.[object_id]=p.[object_id] AND ix.index_id=p.index_id
	WHERE ix.[object_id]=@object_id AND ix.index_id>0
	ORDER BY (CASE WHEN ix.is_primary_key=1 THEN 1
		       WHEN ix.is_unique_constraint=1 THEN 2
		       ELSE 3 END), ix.[type], ix.[name]
    OPTION (MAXRECURSION 0);






--- Foreign keys constraints: Constraint name, columns, references, ref.columns, on update/delete, options (enabled, for replication)
IF (@has_foreign_keys=1)
	WITH cols AS (
		SELECT fkc.constraint_object_id, fkc.constraint_column_id,
		       CAST((CASE WHEN pc.[name] LIKE N'[0-9]%' OR pc.[name] LIKE N'%[^0-9a-z\_@]%' ESCAPE N'\' OR pc.[name] IN (SELECT keyword FROM @reserved_keywords) THEN QUOTENAME(pc.[name]) ELSE pc.[name] END) AS varchar(max)) AS parent_cols,
		       CAST((CASE WHEN rc.[name] LIKE N'[0-9]%' OR rc.[name] LIKE N'%[^0-9a-z\_@]%' ESCAPE N'\' OR rc.[name] IN (SELECT keyword FROM @reserved_keywords) THEN QUOTENAME(rc.[name]) ELSE rc.[name] END) AS varchar(max)) AS referenced_cols
		FROM @sysforeignkeycols AS fkc
		INNER JOIN @syscolumns AS pc ON fkc.parent_object_id=pc.[object_id] AND fkc.parent_column_id=pc.column_id
		INNER JOIN @syscolumns AS rc ON fkc.referenced_object_id=rc.[object_id] AND fkc.referenced_column_id=rc.column_id
		WHERE fkc.constraint_column_id=1 AND
		      @object_id IN (fkc.parent_object_id, fkc.referenced_object_id)

		UNION ALL

		SELECT fkc.constraint_object_id, fkc.constraint_column_id,
		       CAST(cols.parent_cols+', '+pc.[name] AS varchar(max)) AS parent_cols,
		       CAST(cols.referenced_cols+', '+rc.[name] AS varchar(max)) AS referenced_cols
		FROM cols
		INNER JOIN @sysforeignkeycols AS fkc ON cols.constraint_object_id=fkc.constraint_object_id AND cols.constraint_column_id+1=fkc.constraint_column_id
		INNER JOIN @syscolumns AS pc ON fkc.parent_object_id=pc.[object_id] AND fkc.parent_column_id=pc.column_id
		INNER JOIN @syscolumns AS rc ON fkc.referenced_object_id=rc.[object_id] AND fkc.referenced_column_id=rc.column_id)

	SELECT ps.[name]+N'.'+p.[name] AS [Referencing object],
	       fk.[name] AS [Foreign key constraint],
	       N'FOREIGN KEY ('+(SELECT TOP 1 parent_cols FROM cols WHERE cols.constraint_object_id=fk.[object_id] ORDER BY cols.constraint_column_id DESC)+N')' AS [Referencing columns],
	       N'REFERENCES '+rs.[name]+'.'+r.[name] AS [Referenced object],
	       N'('+(SELECT TOP 1 referenced_cols FROM cols WHERE cols.constraint_object_id=fk.[object_id] ORDER BY cols.constraint_column_id DESC)+N')' AS [Referenced columns],
	       SUBSTRING(ISNULL(N' ON DELETE '+NULLIF(REPLACE(fk.delete_referential_action_desc, N'_', N' '), N'NO ACTION'), N'')+
			 ISNULL(N' ON UPDATE '+NULLIF(REPLACE(fk.update_referential_action_desc, N'_', N' '), N'NO ACTION'), N''), 2, 100)+
             (CASE WHEN 1 IN (fk.is_disabled, fk.is_not_trusted) THEN N' -- ' ELSE N'' END)+
             (CASE WHEN fk.is_disabled=1 THEN N' DISABLED'
                   WHEN fk.is_not_trusted=1 THEN N' NOT TRUSTED' ELSE N'' END) AS [Options]
	FROM @sysforeignkeys AS fk
	INNER JOIN @sysobjects AS p ON fk.parent_object_id=p.[object_id]
	INNER JOIN @sysschemas AS ps ON p.[schema_id]=ps.[schema_id]
	INNER JOIN @sysobjects AS r ON fk.referenced_object_id=r.[object_id]
	INNER JOIN @sysschemas AS rs ON r.[schema_id]=rs.[schema_id]
	WHERE @object_id IN (fk.parent_object_id, fk.referenced_object_id)
	ORDER BY (CASE WHEN fk.parent_object_id=@object_id THEN 1 ELSE 2 END), fk.[name];





--- Security policies
IF (@has_policies=1)
	SELECT N'ALTER SECURITY POLICY '+ps.[name]+N'.'+po.[name] AS [Security policy],
           N'ADD '+pol.predicate_type_desc+N' PREDICATE' AS [Predicate],
           pol.predicate_definition AS [Definition],
           N'ON ['+s.[name]+N'].['+o.[name]+N']' AS [Target],
           N'WITH (STATE='+(CASE WHEN pol.is_enabled=1 THEN N'ON' ELSE N'OFF' END)+
               (CASE WHEN pol.is_schema_bound=1 THEN N', SCHEMABINDING=ON' ELSE N'' END)+N');' AS [Options]
    FROM @syssecuritypolicies AS pol
	INNER JOIN @sysobjects AS po ON pol.security_policy_id=po.[object_id]
	INNER JOIN @sysschemas AS ps ON po.[schema_id]=ps.[schema_id]
	INNER JOIN @sysobjects AS o ON o.[object_id]=@object_id
	INNER JOIN @sysschemas AS s ON o.[schema_id]=s.[schema_id]
    ORDER BY ps.[name], po.[name], pol.security_predicate_id;




--- Triggers: name, type and actions
IF (EXISTS (SELECT NULL FROM @systriggers))
    SELECT s.[name]+N'.'+t.[name] AS [Trigger],
           (CASE WHEN t.is_instead_of_trigger=1 THEN N'INSTEAD OF ' ELSE N'FOR ' END)+t.[trigger_events] AS [Trigger action(s)],
           (CASE WHEN t.is_disabled=1 THEN N'Disabled' ELSE N'' END) AS [ ]
    FROM @systriggers AS t
    LEFT JOIN @sysobjects AS o ON t.[object_id]=o.[object_id]
    LEFT JOIN @sysschemas AS s ON o.[schema_id]=s.[schema_id]
    ORDER BY t.[name];






--- Relations between objects (referencing or referenced) including foreign key constraints:
IF (@has_references=1) BEGIN;
	WITH refs1
	AS (--- Expression dependencies (i.e. modules that refer to other modules,
	    --- tables, functions, check constraints, etc:
	    SELECT DISTINCT dep.referencing_id,
		   dep.referenced_id,
		   dep.is_schema_bound_reference AS is_schemabound,
		   0 AS is_foreign_key,
		   CAST(NULL AS varchar(255)) AS column_list,
		   0 AS is_nullable
	    FROM @sysexprdependencies AS dep
	    WHERE dep.referencing_id!=dep.referenced_id

	    UNION

	    --- ... and foreign key constraints that define dependencies between tables:    
	    SELECT fk.parent_object_id AS referencing_id,
		   fk.referenced_object_id,
		   0 AS is_schemabound,
		   1 AS is_foreign_key,
		   CAST(c.list AS varchar(255)) AS column_list,
		   (SELECT MIN(CAST(n.is_nullable AS int))
		    FROM @sysforeignkeycols AS fkc
		    INNER JOIN @syscolumns AS n ON
			fkc.parent_object_id=n.[object_id] AND
			fkc.parent_column_id=n.column_id
		    WHERE fk.[object_id]=fkc.constraint_object_id) AS is_nullable
	    FROM @sysforeignkeys AS fk
	    CROSS APPLY (
		SELECT CAST(fkc.parent_column_id AS varchar(4))+N';'
		FROM @sysforeignkeycols AS fkc
		WHERE fkc.constraint_object_id=fk.[object_id]
		ORDER BY fkc.parent_column_id
		FOR XML PATH(N''), TYPE) AS c(list)
	    WHERE fk.parent_object_id!=fk.referenced_object_id),

	     uqix
	AS (--- These are all the unique column combinations in each table.
	    SELECT DISTINCT ix.[object_id], CAST(c.list AS varchar(255)) AS column_list
	    FROM @sysindexes AS ix
	    CROSS APPLY (
		SELECT CAST(ixc.column_id AS varchar(4))+N';'
		FROM @sysindexcolumns AS ixc
		WHERE ixc.[object_id]=ix.[object_id] AND
		      ixc.index_id=ix.index_id AND
		      ixc.is_included_column=0
		ORDER BY ixc.column_id
		FOR XML PATH(N''), TYPE) AS c(list)
	    WHERE ix.is_unique=1),

	     refs2
	AS (--- Check for matching unique indexes on the referencing table columns. Also, if
	    --- there's more than one reference between the two tables, or more than one unique
	    --- index on the referencing table, make the result set distinct using GROUP BY.
	    SELECT refs1.referencing_id AS parent_id,
		   ps.[name]+N'.'+p.[name] AS parent_name,
		   refs1.referenced_id AS child_id,
		   cs.[name]+N'.'+c.[name] AS child_name,
		   CAST(MAX(refs1.is_schemabound) AS bit) AS is_schemabound,
		   CAST(MAX(refs1.is_foreign_key) AS bit) AS is_foreign_key,
		   CAST(MAX((CASE WHEN uqix.[object_id] IS NOT NULL THEN 1 ELSE 0 END)) AS bit) AS is_unique,
		   CAST(MAX(refs1.is_nullable) AS bit) AS is_nullable
	    FROM refs1
	    INNER JOIN @sysobjects AS p  ON p.[object_id]=refs1.referencing_id
	    INNER JOIN @sysschemas AS ps ON p.[schema_id]=ps.[schema_id]
	    INNER JOIN @sysobjects AS c  ON c.[object_id]=refs1.referenced_id
	    INNER JOIN @sysschemas AS cs ON c.[schema_id]=cs.[schema_id]
	    LEFT JOIN uqix ON uqix.[object_id]=p.[object_id] AND refs1.column_list=uqix.column_list
	    GROUP BY refs1.referencing_id, refs1.referenced_id,
		     ps.[name], p.[name], cs.[name], c.[name]),
    
	     refs3
	AS (--- Add ROW_NUMBER() and COUNT(*) OVER to count number of children
	    --- per parent and number of parents per child. This is used to
	    --- format the output properly.
	    SELECT parent_id, parent_name,
		   ROW_NUMBER() OVER (PARTITION BY child_id ORDER BY parent_name) AS parent_row,
		   COUNT(*) OVER (PARTITION BY child_id) AS parent_count,
		   child_id, child_name,
		   ROW_NUMBER() OVER (PARTITION BY parent_id ORDER BY child_name) AS child_row,
		   COUNT(*) OVER (PARTITION BY parent_id) AS child_count,
		   is_schemabound, is_foreign_key, is_unique, is_nullable
	    FROM refs2)

	--- Store everything from refs3 in a work table for performance:    
	INSERT INTO @references
	      (parent_id, parent_name, child_id, child_name, is_schemabound, is_nullable,
	       is_foreign_key, parent_row, parent_count, child_row, child_count, is_unique)
	SELECT parent_id, parent_name, child_id, child_name, is_schemabound, is_nullable,
	       is_foreign_key, parent_row, parent_count, child_row, child_count, is_unique
	FROM refs3 AS r;

	--- "parents" contains the first and second-order parent levels (r2 and r1),
	--- where "parents" are referencing objects and "children" are the referenced objects.
	WITH [rowcount] AS (
		SELECT [object_id], N' ('+CAST(SUM([rows]) AS nvarchar(10))+N')' AS [rowcount]
		FROM @syspartitions
		WHERE index_id IN (0, 1)
		GROUP BY [object_id], index_id
		HAVING SUM([rows])!=0
		),

	     parents AS (
		SELECT --- Ordinal
		       ROW_NUMBER() OVER (ORDER BY r2.parent_name, r1.parent_name)-COUNT(*) OVER (PARTITION BY NULL)/2 AS ordinal,
        
		       --- Level -2
		       ISNULL(r1.parent_name+ISNULL(+c1.[rowcount], N''), N'') AS obj2,
		       r1.is_foreign_key AS fk2,
		       r1.is_schemabound AS sb2,
		       r1.is_unique AS uq2,
		       r1.is_nullable AS n2,

		       --- Spacer
		       (CASE WHEN r1.parent_count=0 THEN N''
			     WHEN r1.parent_count=1 THEN @hyph
			     WHEN r1.parent_row=1 THEN N'\'
			     WHEN r1.parent_row=r1.parent_count THEN N'/'
			     WHEN r1.child_id IS NOT NULL THEN @pipe
			     ELSE N'' END) AS spacer,

		       --- Level -1
		       (CASE WHEN r1.parent_row=r1.parent_count/2+1 OR r1.child_id IS NULL THEN r2.parent_name+ISNULL(+c2.[rowcount], N'') ELSE N'' END) AS obj1,
		       r2.is_foreign_key AS fk1,
		       r2.is_schemabound AS sb1,
		       r2.is_unique AS uq1,
		       r2.is_nullable AS n1
		FROM @references AS r1
		RIGHT JOIN @references AS r2 ON r1.child_id=r2.parent_id
		LEFT JOIN [rowcount] AS c1 ON r1.parent_id=c1.[object_id]
		LEFT JOIN [rowcount] AS c2 ON r2.parent_id=c2.[object_id]
		WHERE r2.child_id=@object_id),

	--- "children" contains the first and second-order child levels (r3 and r4):
	     children AS (
		SELECT --- Ordinal
		       ROW_NUMBER() OVER (ORDER BY r3.child_name, r4.child_name)-COUNT(*) OVER (PARTITION BY NULL)/2 AS ordinal,

		       --- Level +1
		       (CASE WHEN r4.child_row=r4.child_count/2+1 OR r4.parent_id IS NULL THEN r3.child_name+ISNULL(+c3.[rowcount], N'') ELSE N'' END) AS obj1,
		       r3.is_foreign_key AS fk1,
		       r3.is_schemabound AS sb1,
		       r3.is_unique AS uq1,
		       r3.is_nullable AS n1,

		       --- Spacer
		       (CASE WHEN r4.child_count=0 THEN N''
			     WHEN r4.child_count=1 THEN @hyph
			     WHEN r4.child_row=1 THEN N'/'
			     WHEN r4.child_row=r4.child_count THEN N'\'
			     WHEN r4.parent_id IS NULL THEN N''
			     ELSE @pipe END) AS spacer,

		       --- Level +2
		       ISNULL(r4.child_name+ISNULL(+c4.[rowcount], N''), N'') AS obj2,
		       r4.is_foreign_key AS fk2,
		       r4.is_schemabound AS sb2,
		       r4.is_unique AS uq2,
		       r4.is_nullable AS n2
		FROM @references AS r3
		LEFT JOIN @references AS r4 ON r3.child_id=r4.parent_id
		LEFT JOIN [rowcount] AS c3 ON r3.child_id=c3.[object_id]
		LEFT JOIN [rowcount] AS c4 ON r4.child_id=c4.[object_id]
		WHERE r3.parent_id=@object_id)

	--- Putting it all together:
	SELECT --- Second-order parent and spacer:
	       ISNULL(p.obj2, '') AS [Referencing...],

	       (CASE WHEN p.obj2!=N'' AND p.fk2=1 AND p.n2=1 THEN @zero
		         WHEN p.obj2!=N'' AND p.fk2=1 AND p.n2=0 THEN @one ELSE N'' END)+
	       (CASE WHEN p.obj2!=N'' AND p.fk2=1 AND p.uq2=0 THEN @inf ELSE N'' END)+
	       (CASE WHEN p.obj2!=N'' AND p.fk2=1 THEN @hyph+@one
		         WHEN p.sb2=1 THEN N'*' ELSE N'' END) AS [ ],
	       ISNULL(p.spacer, N'') AS [ ],

	       --- First-order parent and spacer:
	       ISNULL(p.obj1, N'') AS [ ],
	       (CASE WHEN p.obj1!=N'' AND p.fk1=1 AND p.n1=1 THEN @zero
		         WHEN p.obj1!=N'' AND p.fk1=1 AND p.n1=0 THEN @one ELSE N'' END)+
	       (CASE WHEN p.obj1!=N'' AND p.fk1=1 AND p.uq1=0 THEN @inf ELSE N'' END)+
	       (CASE WHEN p.obj1!=N'' AND p.fk1=1 THEN @hyph+@one
		         WHEN p.sb1=1 THEN N'*' ELSE N'' END) AS [ ],
	       (CASE WHEN COUNT(p.ordinal) OVER(PARTITION BY NULL)=1 AND p.ordinal=MIN(p.ordinal) OVER (PARTITION BY NULL) THEN @hyph
		         WHEN p.ordinal=MIN(p.ordinal) OVER (PARTITION BY NULL) THEN N'\'
		         WHEN p.ordinal=MAX(p.ordinal) OVER (PARTITION BY NULL) THEN N'/'
		         WHEN p.ordinal IS NOT NULL THEN @pipe ELSE N'' END) AS [ ],

	       --- The object itself:
	       (CASE WHEN n.ordinal=1 THEN sch.[name]+N'.'+obj.[name]+ISNULL(rc.[rowcount], N'') ELSE N'' END) AS [Object],

	       --- First-order spacer and child:
	       (CASE WHEN COUNT(c.ordinal) OVER(PARTITION BY NULL)=1 AND c.ordinal=MIN(c.ordinal) OVER (PARTITION BY NULL) THEN @hyph
		         WHEN c.ordinal=MIN(c.ordinal) OVER (PARTITION BY NULL) THEN N'/'
		         WHEN c.ordinal=MAX(c.ordinal) OVER (PARTITION BY NULL) THEN N'\'
		         WHEN c.ordinal IS NOT NULL THEN @pipe ELSE '' END) AS [ ],
	       (CASE WHEN c.obj1!=N'' AND c.fk1=1 AND c.n1=1 THEN @zero
		         WHEN c.obj1!=N'' AND c.fk1=1 AND c.n1=0 THEN @one ELSE N'' END)+
	       (CASE WHEN c.obj1!=N'' AND c.fk1=1 AND c.uq1=0 THEN @inf ELSE N'' END)+
	       (CASE WHEN c.obj1!=N'' AND c.fk1=1 THEN @hyph+@one
		         WHEN c.sb1=1 THEN N'*' ELSE N'' END) AS [ ],

	       ISNULL(c.obj1, '') AS [ ],

	       --- Second-order spacer and child:
	       ISNULL(c.spacer, N'') AS [ ],
	       (CASE WHEN c.obj2!=N'' AND c.fk2=1 AND c.n2=1 THEN @zero
		         WHEN c.obj2!=N'' AND c.fk2=1 AND c.n2=0 THEN @one ELSE N'' END)+
	       (CASE WHEN c.obj2!=N'' AND c.fk2=1 AND c.uq2=0 THEN @inf ELSE N'' END)+
	       (CASE WHEN c.obj2!=N'' AND c.fk2=1 THEN @hyph+@one
		         WHEN c.sb2=1 THEN N'*' ELSE N'' END) AS [ ],

	       ISNULL(c.obj2, N'') AS [... referenced]
	FROM (--- This is a list of all ordinals, used as a "frame" for the
	      --- recordset, to which we join "parents" and "children":
	      SELECT ordinal FROM parents UNION
	      SELECT ordinal FROM children) AS n
	INNER JOIN @sysobjects AS obj ON obj.[object_id]=@object_id
	INNER JOIN @sysschemas AS sch ON obj.[schema_id]=sch.[schema_id]
	LEFT JOIN [rowcount] AS rc ON rc.[object_id]=@object_id
	LEFT JOIN parents  AS p ON p.ordinal=n.ordinal
	LEFT JOIN children AS c ON c.ordinal=n.ordinal
	--- ... and order the output:
	ORDER BY n.ordinal;
END;







--- Permissions
IF (@has_permissions=1)
	WITH list AS (
		SELECT p.class, (CASE p.class
		        WHEN 1 THEN s.[name]+N'.'+o.[name]
			WHEN 3 THEN N'SCHEMA::'+s.[name]
			WHEN 0 THEN N'DATABASE::'+DB_NAME() END) AS securable,
		       ROW_NUMBER() OVER (
			   PARTITION BY p.class, p.major_id, p.grantee_principal_id, p.grantor_principal_id, p.[state]
			   ORDER BY p.[permission_name], p.minor_id) AS ordinal,
		       p.grantee_principal_id,
		       p.grantor_principal_id,
		       p.[state],
		       p.[permission_name] COLLATE database_default+ISNULL(N'('+c.[name]+N')', N'') AS [permission_name]
		FROM @sysdatabasepermissions AS p
		INNER JOIN @sysobjects AS o ON o.[object_id]=@object_id
		INNER JOIN @sysschemas AS s ON o.[schema_id]=s.[schema_id]
		LEFT JOIN @syscolumns AS c ON c.[object_id]=o.[object_id] AND p.minor_id=c.column_id
		WHERE (p.class=1 AND p.major_id=o.[object_id] OR
		       p.class=3 AND p.major_id=o.[schema_id] OR
		       p.class=0) AND
		       p.[type] IN (SELECT [type] COLLATE database_default FROM sys.fn_builtin_permissions(N'OBJECT'))),

	rcte AS (
		SELECT class, securable, ordinal, grantee_principal_id, grantor_principal_id, [state], CAST([permission_name] AS nvarchar(max)) AS list
		FROM list
		WHERE ordinal=1

		UNION ALL

		SELECT list.class, list.securable, list.ordinal, rcte.grantee_principal_id, rcte.grantor_principal_id, rcte.[state], CAST(rcte.list+N', '+list.[permission_name] AS nvarchar(max))
		FROM rcte
		INNER JOIN list ON
		    list.securable=rcte.securable AND
		    list.grantee_principal_id=rcte.grantee_principal_id AND
		    list.grantor_principal_id=rcte.grantor_principal_id AND
		    list.[state]=rcte.[state] AND
		    list.ordinal=rcte.ordinal+1)

	SELECT (CASE p.[state] WHEN 'D' THEN N'DENY'
		                   WHEN 'G' THEN N'GRANT'
		                   WHEN 'W' THEN N'GRANT' END) AS [Grant/Deny],
	       (CASE WHEN obj.[type]='FN' AND px.list=N'EXECUTE, REFERENCES' THEN N'ALL'
		         WHEN obj.[type] IN ('IF', 'TF', 'U', 'V') AND px.list=N'DELETE, INSERT, REFERENCES, SELECT, UPDATE' THEN N'ALL'
		         ELSE px.list END) AS [Permission],
	       N'ON '+p.securable AS [Object],
	       N'TO '+QUOTENAME(grantee.[name]) AS [Principal],
	       LTRIM((CASE WHEN p.[state]='W' THEN N' WITH GRANT OPTION' ELSE N'' END)+
		         (CASE WHEN grantor.[name]!=N'dbo' THEN N' AS '+QUOTENAME(grantor.[name]) ELSE N'' END)) AS [Options]
	FROM rcte AS p
	INNER JOIN @sysobjects AS obj ON obj.[object_id]=@object_id
	INNER JOIN @sysschemas AS sch ON obj.[schema_id]=sch.[schema_id]
	LEFT JOIN @sysdatabaseprincipals AS grantee ON p.grantee_principal_id=grantee.principal_id
	LEFT JOIN @sysdatabaseprincipals AS grantor ON p.grantor_principal_id=grantor.principal_id
	CROSS APPLY (SELECT TOP 1 list
		     FROM rcte
		     WHERE rcte.[state]=p.[state] AND
			   rcte.grantee_principal_id=p.grantee_principal_id AND
			   rcte.grantor_principal_id=p.grantor_principal_id
		     ORDER BY rcte.ordinal DESC) AS px
	WHERE p.ordinal=1

    UNION ALL

    SELECT N'ADD SIGNATURE' AS [Grant/Deny], N'' AS [Permission],
           N'TO '+sch.[name]+N'.'+obj.[name] AS [Object],
           N'' AS [Principal],
           N'BY '+sig.src+N' '+sig.[name]+ISNULL(N' WITH '+sig.encryption_type_desc, N'')+';' AS [Options]
    FROM @signatures AS sig
    INNER JOIN @sysobjects AS obj ON sig.major_id=obj.[object_id] AND sig.[type_desc]=obj.[type_desc]
	INNER JOIN @sysschemas AS sch ON obj.[schema_id]=sch.[schema_id];




IF (@has_data=1 AND @rowcount>0)
	SELECT ISNULL(ix.[name], N'') AS [Index/heap],
/*
	       --- Partition number, if there are partitions:
	       (CASE COUNT(*) OVER (PARTITION BY p.[object_id], p.index_id)
		     WHEN 1 THEN ''
		     ELSE CAST(p.partition_number AS varchar(10))
		     END) AS [Partition],
*/
           (CASE WHEN p.discrete_boundary=0
                 THEN ISNULL(LAG(p.boundary, 1) OVER (PARTITION BY ix.index_id ORDER BY p.partition_number), N'')+
                 (CASE WHEN p.boundary_value_on_right=1 AND p.partition_number=1 THEN pc.[name]
                       WHEN p.boundary_value_on_right=1 THEN N' <= '+pc.[name]
                       WHEN p.boundary_value_on_right=0 AND p.partition_number=1 THEN pc.[name]
                       WHEN p.boundary_value_on_right=0 THEN N' < '+pc.[name]
                       ELSE N'' END)+
                 LEAD((CASE WHEN p.boundary_value_on_right=1 THEN N' < '
                            WHEN p.boundary_value_on_right=0 THEN N' <= '
                            ELSE N'' END), 1, N'') OVER (PARTITION BY ix.index_id ORDER BY p.partition_number)+
                 ISNULL(p.boundary, N'')
                 ELSE pc.[name]+N'='+ISNULL(p.boundary, N'NULL') END) AS [Partition],

	       --- Storage properties:
	       ISNULL(NULLIF(NULLIF(p.data_compression_desc, N'NONE'), N'COLUMNSTORE'), N'') AS [Compression],
	       ds.[name]+ISNULL('('+pc.[name]+N')', N'') AS [Data space],
           ISNULL(ds2.[name], ds.[name]) AS [Filegroup],
	       (CASE WHEN ix.[type_desc]!=N'HEAP' THEN STR(ISNULL(NULLIF(ix.fill_factor, 0), 100), 4, 0)+'%' ELSE '' END) AS [Fill factor],

	       --- The raw numbers:
	       REPLACE(REPLACE(CONVERT(varchar(100), CAST(ISNULL(ps.row_count, p.[rows]) AS money), 1), ',', ' '), '.00', '') AS [Row count],
	       (CASE WHEN ps.[partition_id] IS NULL THEN 'n/a' ELSE '' END)+ISNULL(STR(NULLIF(1.0*ps.reserved_page_count*8/1024, 0), 12, 2)+' MB', '') AS [Reserved],
	       (CASE WHEN ps.[partition_id] IS NULL THEN 'n/a' ELSE '' END)+ISNULL(STR(NULLIF(1.0*ps.in_row_used_page_count*8/1024, 0), 12, 2)+' MB', '') AS [In-row used],
	       (CASE WHEN ps.[partition_id] IS NULL THEN 'n/a' ELSE '' END)+ISNULL(STR(NULLIF(1.0*ps.row_overflow_used_page_count*8/1024, 0), 12, 2)+' MB', '') AS [Row-overflow used],
	       (CASE WHEN ps.[partition_id] IS NULL THEN 'n/a' ELSE '' END)+ISNULL(STR(NULLIF(1.0*ps.lob_used_page_count*8/1024, 0), 12, 2)+' MB', '') AS [Out-of-row used],
	       (CASE WHEN ps.[partition_id] IS NULL THEN 'n/a' ELSE '' END)+ISNULL(STR(NULLIF(1.0*ps.used_page_count*8/1024, 0), 12, 2)+' MB', '') AS [Total used],
	       (CASE WHEN ps.[partition_id] IS NULL THEN 'n/a' ELSE '' END)+ISNULL(STR(NULLIF(100.0*ps.used_page_count/NULLIF(ps.reserved_page_count, 0), 0), 12, 2)+' %', '') AS [Total used %],
	       (CASE WHEN ps.[partition_id] IS NULL THEN 'n/a' ELSE '' END)+ISNULL(STR(NULLIF(1.0*ps.used_page_count*8*1024/NULLIF(ISNULL(ps.row_count, p.[rows]), 0), 0), 12, 1)+' B', '') AS [Avg. row size],
           ISNULL(STR(NULLIF(1.0*cs1.size_in_bytes/1024/1024, 0), 12, 2)+' MB', '') AS [CS open],
           ISNULL(STR(NULLIF(1.0*cs2.size_in_bytes/1024/1024, 0), 12, 2)+' MB', '') AS [CS closed],
           ISNULL(STR(NULLIF(1.0*cs3.size_in_bytes/1024/1024, 0), 12, 2)+' MB', '') AS [CS compressed]
	FROM @syspartitionstats AS ps
	RIGHT JOIN @syspartitions AS p ON ps.[partition_id]=p.[partition_id]
	LEFT JOIN @sysindexes AS ix ON p.[object_id]=ix.[object_id] AND p.index_id=ix.index_id
	--- Data space is either a file group or a partition function:
	LEFT JOIN @sysdataspaces AS ds ON ix.data_space_id=ds.data_space_id
    LEFT JOIN @destination_data_spaces AS dds ON ds.data_space_id=dds.partition_scheme_id AND p.partition_number=dds.partition_number
    LEFT JOIN @sysdataspaces AS ds2 ON dds.data_space_id=ds2.data_space_id
	--- This is the partitioning column:
	LEFT JOIN @sysindexcolumns AS ixc ON ix.[object_id]=ixc.[object_id] AND
	    ix.index_id=ixc.index_id AND ixc.partition_ordinal>0
	LEFT JOIN @syscolumns AS pc ON pc.[object_id]=@object_id AND pc.column_id=ixc.column_id
    LEFT JOIN @columnstore_rowgroups AS cs1 ON cs1.[object_id]=@object_id AND cs1.index_id=ix.index_id AND cs1.partition_number=p.partition_number AND cs1.[state]=1
    LEFT JOIN @columnstore_rowgroups AS cs2 ON cs2.[object_id]=@object_id AND cs2.index_id=ix.index_id AND cs2.partition_number=p.partition_number AND cs1.[state]=2
    LEFT JOIN @columnstore_rowgroups AS cs3 ON cs3.[object_id]=@object_id AND cs3.index_id=ix.index_id AND cs3.partition_number=p.partition_number AND cs1.[state]=3
	WHERE p.[object_id]=@object_id
	ORDER BY ix.index_id, p.partition_number;




IF (@has_sql_module=1) BEGIN;
    WITH r AS (
        SELECT -3 AS [row], CAST(N'SET '+NULLIF(SUBSTRING(
                    (CASE WHEN @uses_ansi_nulls=1 THEN N', ANSI_NULLS' ELSE N'' END)+
                    (CASE WHEN @uses_quoted_identifier=1 THEN N', QUOTED_IDENTIFIER' ELSE N'' END), 3, 1000), '')+
	            N' ON;' AS nvarchar(max)) AS rowData,
            CAST(NULL AS nvarchar(max)) AS remain
        UNION ALL
        SELECT -3 AS [row], CAST('SET '+NULLIF(SUBSTRING(
                    (CASE WHEN @uses_ansi_nulls=0 THEN N', ANSI_NULLS' ELSE '' END)+
                    (CASE WHEN @uses_quoted_identifier=0 THEN N', QUOTED_IDENTIFIER' ELSE N'' END), 3, 1000), '')+
	            N' OFF;' AS nvarchar(max)) AS rowData,
	        CAST(NULL AS nvarchar(max)) AS remain
        UNION ALL

        SELECT -2 AS [row], CAST(N'GO' AS nvarchar(max)), CAST(NULL AS nvarchar(max))
        UNION ALL
        SELECT -1 AS [row], CAST(N'' AS nvarchar(max)), CAST(NULL AS nvarchar(max))
        UNION ALL

        SELECT 0 AS [row], CAST(NULL AS nvarchar(max)) AS rowData, @module_definition AS remain
	    UNION ALL
	    SELECT [row]+1, LEFT(remain, CHARINDEX(@lf, remain+@lf)-1),
	           SUBSTRING(remain, CHARINDEX(@lf, remain+@lf)+1, LEN(remain))
	    FROM r
	    WHERE NULLIF(remain, N'') IS NOT NULL AND [row]<32)

    SELECT (CASE WHEN [row]<0 THEN N'' ELSE STR([row], 4, 0) END) AS Line,
           REPLACE(rowData, @cr, N'') AS [Source code]
    FROM r
    WHERE [row]>=(SELECT MIN([row]) FROM r WHERE [row]>0 AND REPLACE(rowData, @cr, N'')!=N'') AND [row]<32 OR [row]<0 AND rowData IS NOT NULL
    UNION ALL
    SELECT TOP 1 N' ...' AS [row], N''
    FROM r
    WHERE [row]>=15
    ORDER BY 1
    OPTION (MAXRECURSION 0);
END;





/*
--- Cached execution plans (stored procedures only)
IF (@type='P' AND EXISTS (SELECT NULL FROM @plans))

    SELECT plan_generation_num AS [Generation],
           query_plan AS [Query plan],
           execution_count AS [Exec count],
           last_execution_time AS [Last executed]
    FROM @plans
    ORDER BY plan_generation_num DESC;
*/



/*

--- Remove Query Store plans pertaining to sp_ctrl3:
BEGIN TRY;

    SET @temp=N'';
    SELECT @temp=@temp+N'
        BEGIN TRY;
            EXECUTE sys.sp_query_store_remove_query @query_id='+CAST(qt.query_text_id AS varchar(20))+N';
        END TRY
        BEGIN CATCH
            -- Do nothing
        END CATCH;'
    FROM sys.query_store_query AS q
    INNER JOIN sys.query_store_query_text AS qt ON q.query_text_id = qt.query_text_id
    WHERE q.[object_id]=OBJECT_ID('['+DB_NAME()+']'+'.dbo.sp_ctrl3')

    EXECUTE sys.sp_executesql @statement=@temp;

END TRY
BEGIN CATCH

    -- Not using query store or not enough permissions.

END CATCH;
*/


GO








--- This marks master.dbo.sp_ctrl3 as a "system object", which makes
--- it accessible from any database without the "master." prefix. It
--- also sets the execution context of the procedure to the database
--- where it was called.
---
--- Not available on Azure SQL Database.
---
--- NOTE: sp_MS_marksystemobject is an undocumented, unsupported
---       feature of SQL Server.

IF (DB_NAME()=N'master' AND CAST(SERVERPROPERTY(N'Edition') AS varchar(100)) NOT LIKE N'%Azure%')
	EXECUTE sys.sp_MS_marksystemobject @objname=N'sp_ctrl3';
GO

--- Apply extended properties to the stored procedure. The values
--- for these properties are collected from the code comment in the
--- installation script.
---
DECLARE @object_id int=OBJECT_ID(N'dbo.sp_ctrl3'), @src nvarchar(max), @val nvarchar(4000);
SELECT @src=[definition] FROM sys.sql_modules WHERE [object_id]=@object_id;

BEGIN TRANSACTION;

    IF (EXISTS (SELECT NULL FROM sys.extended_properties WHERE major_id=@object_id AND [name]=N'Disclaimer'))
        EXECUTE sys.sp_dropextendedproperty @name=N'Disclaimer', @level0type=N'SCHEMA', @level0name=N'dbo', @level1type=N'PROCEDURE', @level1name=N'sp_ctrl3';

    IF (EXISTS (SELECT NULL FROM sys.extended_properties WHERE major_id=@object_id AND [name]=N'Usage'))
        EXECUTE sys.sp_dropextendedproperty @name=N'Usage', @level0type=N'SCHEMA', @level0name=N'dbo', @level1type=N'PROCEDURE', @level1name=N'sp_ctrl3';

    IF (EXISTS (SELECT NULL FROM sys.extended_properties WHERE major_id=@object_id AND [name]=N'Shortcut'))
        EXECUTE sys.sp_dropextendedproperty @name=N'Shortcut', @level0type=N'SCHEMA', @level0name=N'dbo', @level1type=N'PROCEDURE', @level1name=N'sp_ctrl3';

    IF (EXISTS (SELECT NULL FROM sys.extended_properties WHERE major_id=@object_id AND [name]=N'Version'))
        EXECUTE sys.sp_dropextendedproperty @name=N'Version', @level0type=N'SCHEMA', @level0name=N'dbo', @level1type=N'PROCEDURE', @level1name=N'sp_ctrl3';

    -- DISCLAIMER:
    SET @src=SUBSTRING(@src, CHARINDEX(N'DISCLAIMER: ', @src)+12, LEN(@src));
    SELECT @val=LTRIM(RTRIM(REPLACE(LEFT(@src, CHARINDEX(NCHAR(13)+NCHAR(10)+NCHAR(13)+NCHAR(10), @src)-1), NCHAR(13)+NCHAR(10), N' ')));
    WHILE (@val LIKE N'%  %') SET @val=REPLACE(@val, N'  ', N' ');

    EXECUTE sys.sp_addextendedproperty @name=N'Disclaimer', @value=@val, @level0type=N'SCHEMA', @level0name=N'dbo', @level1type=N'PROCEDURE', @level1name=N'sp_ctrl3';

    -- USAGE:
    SET @src=SUBSTRING(@src, CHARINDEX(N'USAGE: ', @src)+12, LEN(@src));
    SELECT @val=LTRIM(RTRIM(REPLACE(LEFT(@src, CHARINDEX(NCHAR(13)+NCHAR(10)+NCHAR(13)+NCHAR(10), @src)-1), NCHAR(13)+NCHAR(10), N' ')));
    WHILE (@val LIKE N'%  %') SET @val=REPLACE(@val, N'  ', N' ');

    EXECUTE sys.sp_addextendedproperty @name=N'Usage', @value=@val, @level0type=N'SCHEMA', @level0name=N'dbo', @level1type=N'PROCEDURE', @level1name=N'sp_ctrl3';

    -- SHORTCUT:
    SET @src=SUBSTRING(@src, CHARINDEX(N'SHORTCUT: ', @src)+12, LEN(@src));
    SELECT @val=LTRIM(RTRIM(REPLACE(LEFT(@src, CHARINDEX(NCHAR(13)+NCHAR(10)+NCHAR(13)+NCHAR(10), @src)-1), NCHAR(13)+NCHAR(10), N' ')));
    WHILE (@val LIKE N'%  %') SET @val=REPLACE(@val, N'  ', N' ');

    EXECUTE sys.sp_addextendedproperty @name=N'Shortcut', @value=@val, @level0type=N'SCHEMA', @level0name=N'dbo', @level1type=N'PROCEDURE', @level1name=N'sp_ctrl3';

    -- VERSION:
    SET @src=SUBSTRING(@src, CHARINDEX(N'VERSION: ', @src)+12, LEN(@src));
    SELECT @val=LTRIM(RTRIM(REPLACE(LEFT(@src, CHARINDEX(NCHAR(13)+NCHAR(10)+NCHAR(13)+NCHAR(10), @src)-1), NCHAR(13)+NCHAR(10), N' ')));
    WHILE (@val LIKE N'%  %') SET @val=REPLACE(@val, N'  ', N' ');

    EXECUTE sys.sp_addextendedproperty @name=N'Version', @value=@val, @level0type=N'SCHEMA', @level0name=N'dbo', @level1type=N'PROCEDURE', @level1name=N'sp_ctrl3';

COMMIT TRANSACTION;
GO
