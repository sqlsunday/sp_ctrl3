# sp_ctrl3

Copyright Daniel Hutmacher under [Creative Commons 4.0 license with attribution](http://creativecommons.org/licenses/by/4.0/).

Source: http://sqlsunday.com/downloads/

**DISCLAIMER:** This script may not be suitable to run in a production
environment. I cannot assume any responsibility regarding
the accuracy of the output information, performance
impacts on your server, or any other consequence. If
your juristiction does not allow for this kind of
waiver/disclaimer, or if you do not accept these terms,
you are NOT allowed to store, distribute or use this
code in any way.

**USAGE:**

    EXECUTE sp_ctrl3 {object name}

How to install and use it: https://sqlsunday.com/2016/11/28/sp_ctrl3/

**SHORTCUT:**   In SQL Server Management Studio, go to Tools -> Options
-> Environment -> Keyboard -> Query Shortcuts.

On a shortcut location of your choice, enter the following
code, with the trailing space, without the quotes:
"EXECUTE sp_ctrl3 ". To use, highlight the name of an object
and press that keyboard shortcut. You may have to open a new
query for the change to take effect. Also, objects denoted by
schema (with a dot) need to be enclosed in quotes for this
to work in older versions of SSMS.
