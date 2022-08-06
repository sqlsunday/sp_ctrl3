# sp_ctrl3

Copyright Daniel Hutmacher under [Creative Commons 4.0 license with attribution](http://creativecommons.org/licenses/by/4.0/).

Source: https://github.com/sqlsunday/sp_ctrl3

# Installing it

You can place the object in the master database to make it available as a global shortcut on the instance. The procedure can
also be put in a single user database if you only plan to use it there.

For ease of use, add a keyboard shortcut in SSMS:

![image](https://user-images.githubusercontent.com/20098483/183245763-5b5e798b-60af-46ba-89db-3e386affb891.png)

* Go to "Tools" -> "Options"
* Under "Environment" -> "Keyboard" -> "Query Shortcuts"
* Enter the text "sp_ctrl3" under any keyboard shortcut you like

Keyboard shortcuts will only work in new query windows, so you'll have to open a new one to use the shortcut.

# What it does

## Display detailed database object information

<img alt="image" src="https://user-images.githubusercontent.com/20098483/183244884-7908adae-817d-4f64-98fa-2a39c4ac28aa.png">

Most of the data displayed is formatted in a way that facilitates copying and pasting directly as a T-SQL statement like CREATE TABLE.

**Object-level information**

* Object schema, name and type
* Data space (filegroup or partition scheme)
* Options (is it system-versioned, any ANSI options we need to know about)
* Change tracking
* Row count and allocated size on disk
* Description from extended properties

**Columns/parameters**

* Name and datatype of the column/parameter
* Defaults, identity column definition
* Collation, if not database default
* Whether or not the column is nullable
* Any non-standard ANSI options
* If this column is constrained by a foreign key
* Description from extended properties

**Indexes and primary key/unique constraints**

* Type, name and list of columns
* Included columns
* Filter expression
* Scripting options used
* Data space assigned (filegroup or partition scheme)
* Number of filtered rows, if filtering is applied

**Foreign key constraints**

* Foreign key constraints to other objects
* Foreign keys constraints on other objects that reference this object
* Options used to create the constraint, like if it's disabled, etc.

**Security policies**

* Name
* Predicate and definition
* Scripting options

**Triggers**

* List of triggers and some information on them

**Permissions**

**Code or object dependencies from/to this object**

A simple ASCII graph that shows you other objects that reference this object,
or are referenced by this object. This could be views, functions, etc. I've
made an attempt at showing if it's a one-to-many, one-to-one, or many-to-one
relationship.

**Storage**

* Clustered/non-clustered indexes and/or heaps
* How the index is stored (what partitions, what compression settings, etc)
* Partition scheme, partition function, as well as the boundaries of each partition
* Fill factor, row count of each partition
* Reserved and used space on- and off-row, as well as a computed average row width
* Number of open, closed and compressed columnstore segments

**Code preview**

A short preview of the first couple of rows. Handy for instance if your company
keeps a description header in each object.

## Plaintext schema search

<img width="1365" alt="image" src="https://user-images.githubusercontent.com/20098483/183245501-e4f13ec9-4740-4b42-b3c3-30c2bfe0abfe.png">

You can search for any string used in the database schema, including

* T-SQL code used in modules (procedures, triggers, views, functions)
* Tables and columns
* Constraints
* Indexes and filter definitions
* Schemas
* Descriptions from extended properties

The search string can include any valid T-SQL wildcard expression. The output includes

* The object schema, object type and name
* Row count, if it's a table or an index
* Line count for T-SQL modules
* Line numbers of the matching results
* Object description from extended properties

**DISCLAIMER:** This script may not be suitable to run in a production
environment. I cannot assume any responsibility regarding
the accuracy of the output information, performance
impacts on your server, or any other consequence. If
your jurisdiction does not allow for this kind of
waiver/disclaimer, or if you do not accept these terms,
you are NOT allowed to store, distribute or use this
code in any way.

**USAGE:**

    EXECUTE sp_ctrl3 {object name}

Blog post: https://sqlsunday.com/2016/11/28/sp_ctrl3/

**SHORTCUT:**   In SQL Server Management Studio, go to Tools -> Options
-> Environment -> Keyboard -> Query Shortcuts.

On a shortcut location of your choice, enter the following
code, with the trailing space, without the quotes:
"EXECUTE sp_ctrl3 ". To use, highlight the name of an object
and press that keyboard shortcut. You may have to open a new
query for the change to take effect. Also, objects denoted by
schema (with a dot) need to be enclosed in quotes for this
to work in older versions of SSMS.

