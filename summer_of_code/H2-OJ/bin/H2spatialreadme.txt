Erwan Bocher

The H2_spatialExtension1.1.jar is under cecill license see : http://www.cecill.info/
More information on : http://geosysin.iict.ch/irstv-trac/wiki/H2spatial/Download

Run h2spatial.bat or h2spatial.sh


Create a new H2 database see H2 documentation.

Load geoSpatial.sql script (copy-paste) and execute it.

Display available functions : SELECT * FROM INFORMATION_SCHEMA.FUNCTION_ALIASES


Create a spatial table

CREATE TABLE myFirstGeoTable(gid INT primary key, the_geom geometry);


Populate table

INSERT INTO myFirstGeoTable VALUES(1, GeomFromText('POINT(0 1)','1')

Where GeomFromText(arg0,arg1)

 arg0 = geometry in WKT format
 arg1 = EPSG code
 


Test buffer function using :

SELECT buffer(the_geom, 20) FROM myFirstGeoTable;
 



10/03/2007

add a geometry dataType in H2-DATABASE.
This datatype is a blob value.

Ecole Centrale School project. 
Students :Christophe TARDELLA and Benoît ROGEZ
Coordination : Erwan Bocher, Thomas Leduc

21/01/2007

Create H2-spatial Extension.




