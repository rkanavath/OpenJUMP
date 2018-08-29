---- OpenKLEM  -----

OpenKlem is an extension of OpenJUMP for hydrological analysis, 
it includes also a module for simulating flood hydrograph (Kinematik Local Excess Model, KLEM).

Source code is available here:
https://bitbucket.org/geomaticaeambiente/openklem

OpenKLEM is distributed under GNU https://www.gnu.org/licenses/gpl-2.0.html


--- Set up ---

OpenJUMP plus: OpenKLEM is already integrated into OpenJUMP PLUS.  

OpenJUMP Core: To install OpenKLEM into OpenJUMP CORE, copy OpenKLEM-**.jar and OpenKLEMOJ-**.jar into OpenJUMP/LIB/EXT folder. 
It requires the following libraries to be located into OJ/LIB/EXT folder
    jfreechart-1.0.13.jar (*)  
    jcommon-1.0.16.jar (*)  
    jep-2.4.1.jar (**)  

(*) available at https://sourceforge.net/p/jump-pilot/code/HEAD/tree/plug-ins/OpenKLEM/OpenKLEMOpenJUMP/trunk/lib/
(**) available at https://sourceforge.net/p/jump-pilot/code/HEAD/tree/plug-ins/OpenKLEM/OpenKLEMCore/trunk/lib/



--- Extra ---

To activate the optional tool "Export to OpenOffice"  you need to have "OpenDocument libraries" installed into
your OpenJUMP:
    commons-validator-1.4.1.jar
    java-rdfa-o.4.2.jar
    jena-core-2.13.0.jar
    jena-iri-1.1.2,jar
    odfdom-java-0.8.10-incubating.jar
    simple-odf-0.8.1-incubating.jar

1) Download OpenDocument libraries from OpenJUMP repository, located ether at OpenJUMP SVN:
   https://sourceforge.net/p/jump-pilot/code/HEAD/tree/plug-ins/OpenKLEM/OpenKLEMOpenJUMP/trunk/lib/
Or at GeoArbores sourceforge site:
 https://sourceforge.net/projects/opensit/files/Openjump/OpenKLEM/OpenKLEM20180726/OpenDocument%20libraries/

2) Create a folder OpenJUMP/lib/ext/Opendocument and copy the selected libraries into it




--- Change log ---

OpenKLEM Base

2018-13-07 
  * Class: com.geomaticaeambiente.openjump.klem.slope.SlopeStripe
    Added radians output
  * General Added gpl_2 and ChangeLog files
  
2018-07-26  Ricompiled the libraries:
  * Enabled project specific setting on Project/Java compiler/Error-Warning  
     to deactivated some warning on compiling
  * Delate class it.geomaticaambiente.klem.klemDA as creating error on compiling

OpenKLEMOJ

2018-06-18 
  * General. Changed icons to famfamfam or OJ ones. Added gpl_2 file
  * General. Optimized icon view on Windows.
  * Kinematic Local Excess Model - Output. Added option to export tables to
    .csv file
  * Kinematic Local Excess Model - Output. 
    Option "Export to .ods" (to libreoffice) activated only if OpenDoc libs
    are located into OJ/LIB/EXT
  * Kinematic Local Excess Model - Output. 
     Output is controlled by embedded OJ libs so that the process can 
    be stopped if it takes a long time
  
2018-07-12 Giuseppe Aruta. Kinematic Local Excess Model.
  * Option: "Basin area km2", substituted scientific notation (2E-3) with 
    numeric one (0.002)
  * Sub-basin warning now shows also the range of area within the algorithm can 
    be applied
  * Substituted [m s.m.m.] with:  [m s.l.m]-Italian (metri sul livello marino)
     and [m a.s.l.]-English (metre above sea level)

2018-07-26 Giuseppe Aruta. PluginUtils class - Kinematic Local Excess Model.
   * Loading project JFileChooser: added method oiption to load project raster files 
     into OpenJUMP view when loading project file
   * Added ChangeLog file.