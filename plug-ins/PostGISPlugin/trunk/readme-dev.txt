--- Informations about the OpenJUMP PostGIS PlugIn (2012-01-21)

The JumpPostGIS PlugIn is an extension of the original PostGIS driver 
created by Refractions Research (www.refractions.net) for Jump.

HISTORY
********
It has been succcessively developped and maintained by
- Chris Hodgson and Paul Ramsey
    original developpers
- Uwe Dalluege (uwe.dalluege@rzcn.haw-hamburg.de)
    feature schema and srid management
    better exception handling
- Eric Lemesre (eric.lemesre@gmail.com)
    integration to new open wizard framework
    internationalization
- Michael Michaud (michael.michaud@free.fr)
    bug fixes, merge of uwe and eric improvements, i18n error messages

FEATURES
********
The plugin can
- read features from a table
- create a new table (managing insert in geometry column table)
- update a table using a primary key

TODO (Michaël)
********
The plugin is partially redundant with core OpenJUMP PostGIS features
Would be useful
- to use core OpenJUMP connection facilities
- to use core OpenJUMP data types mapping facilities
- to add the plugin update/insert feature to OpenJUMP
- to add an option to create an index on feature id and/or on geometry column

RELEASE (Michaël)
********
Last release is named 1.5.0 as it is synchronized with OpenJUMP 1.5.x
(but it is compatible but previous releases).
Eric upgraded the build system to maven, and used a maven for ant plugin
I just came back to a plain ant build file called build-mm.xml for 1.5.0 release

