CLASSPATH=H2_withgeometry.jar:H2_spatialExtension1.1.jar:jts-1.8.jar
java -Xms512M -Xmx512M -cp $CLASSPATH org.h2.tools.Server
