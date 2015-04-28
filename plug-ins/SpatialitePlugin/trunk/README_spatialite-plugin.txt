The OpenJUMP SpatiaLite Plugin is a read-only driver for reading 
attribute and vector data from SQLite databases which have geometries
encoded either as SpatiaLite BLOBs or according to the Feature Data 
Objects (FDO) specification. The plugin can be used for entering 
arbitrary SQL queries. The results of the queries are presented as a
a table ot, if the result set contains geometries, alternatively on 
a map.

License
-------
The JUMP DB Query plugin is licensed under the GNU General License, 
version 2, or a later version of the license. See the file 
LICENSE_spatialite-plugin.txt for more information.

Installation 
------------
  1) Quick install on Windows
  
  Download a Spatialite-plugin package that suits your Jave version:
  32-bit version for 32-bit Java or 64-bit version for 64-bit Java.
  Notice that 64-bit Windows may still run 32-bit Java. If you do not
  know which Java version OpenJUMP is using you can check it by 
  starting OpenJUMP and reading the system info from Help - About menu.
  
  Unzip the package and place all the files in the zipped folder 
  in the JUMP_HOME/lib/ext folder.
  
  2) Custom install for all operating systems
  
  SpatiaLite Plugin needs three components:
  
   a) spatialite-plugin jar file
   b) SQLite JDBC driver
   c) Native SpatiaLite binaries
   
   a) Download a platform independent spatialite-plugin.jar from from 
      the plugin download page. Place the file into 
      JUMP_HOME/lib/ext folder.
   b) Download a platform independent Xerial JDBC driver from 
      https://bitbucket.org/xerial/sqlite-jdbc.
      Driver versions jdbc-3.8.6, jdbc-3.8.7 and jdbc4-3.8.2-SNAPSHOT
      are known to work with SpatiaLite Plugin. 
   c) Acquire native SpatiaLite extension "mod_spatialite" which must
      match both the Java architecture (32-bit or 64-bit) and your 
      operation system. In addition to mod_spatialite you will need 
      all the shared libraries (.dll or .so files) which are used for
      compiling mod_spatialite.
      Ready made binaries for 32-bit and 64-bit Windows can be 
      downloaded through the SpatiaLite web site
      https://www.gaia-gis.it/gaia-sins/ but Linux and OS X users
      are supposed to build mod_spatialite from sources.
      
   The spatialite-plugin.jar and SQLite JDBC driver can also be unzipped
   from the plugin package for Windows. Notice, that OpenJUMP Plus 
   comes with DB Query Plugin which installs a suitable JDBC driver 
   into JUMP_HOME/lib/ext/jumpdbquery-1.1.0. That can be moved to 
   JUMP_HOME/lib/ext/ directory where it can be found by both these
   SpatiaLite utilities.
      
   
Instructions
------------
Start OpenJUMP, and go to Layer --> Import Spatialite Layer. Press the
"Open SpatiaLite DB" button and select the SpatiaLite database with file
browser. After successful connection to database the "Execute SQL Query"
and "Add Layer" buttons are activated. The Tables tab in the left side
panel is also updated to list the tables of the selected SpatiaLite DB.

Enter your SQL SELECT statement to the upper right panel. The left side
panel gives syntax help about the SpatiaLite functions and some basic 
SQL commands. When the "Execute SQL Query" button is pressed the result
of the query appears into the lower right text panel. By pressing the
"Add layer" button the resultset is converted into a layer to the 
OpenJUMP project. It is not necessary to run "Execute SQL Query" first.
However, the text view is the only way to show the resultsets which do
not contain geometries. With "LIMIT 1" it is a perfect way for testing
your queries.

Notice that anything that can be done with SQL can be performed with 
Execute SQL button: CREATE/DROP TABLE/INDEX/VIEW, VACUUM, PRAGMA...

OpenJUMP teem thanks Jorge Alvaraz for this plugin and hopes you find
this plugin useful.  If you have problems, contact me, 
Jukka Rahkonen at jukka.rahkonen@latuviitta.fi.

