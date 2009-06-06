@java -cp "H2_withgeometry.jar;H2_spatialExtension1.1.jar;jts-1.8.jar;%H2DRIVERS%;%CLASSPATH%" org.h2.tools.Server
@if errorlevel 1 pause