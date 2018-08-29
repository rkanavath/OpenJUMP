package com.geomaticaeambiente.klemgui.utils;

import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.io.DriverProperties;
import com.vividsolutions.jump.io.ShapefileReader;
import com.vividsolutions.jump.io.ShapefileWriter;
import com.vividsolutions.jump.workbench.model.Layer;
import java.io.File;

/**
 *
 * @author AdL
 */
public class ShapefileUtils {
    
    public static File saveLayerAsShapefile(Layer layer, File ouutputFolder) throws Exception{
        
        String name = layer.getName();
        String outFile = ouutputFolder.getAbsolutePath() + File.separator + name + ".shp";
       
        
        DriverProperties dp = new DriverProperties();
        dp.set("File", outFile); 
        
        ShapefileWriter shpWriter = new ShapefileWriter();
        shpWriter.write(layer.getFeatureCollectionWrapper(), dp);
        
        return new File(outFile);
    }
    
    public static FeatureCollection loadShapefile(File shapefile) throws Exception {
        
        DriverProperties driverProps = new DriverProperties();
        driverProps.setProperty("File", shapefile.getAbsolutePath());
        ShapefileReader shpReader = new ShapefileReader();
        return shpReader.read(driverProps);
        
    }
    
}
