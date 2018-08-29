package com.geomaticaeambiente.klemgui.plugin.rastertools;

import com.geomaticaeambiente.klemgui.exceptions.WarningException;
import com.geomaticaeambiente.klemgui.utils.FileUtils;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.vividsolutions.jts.geom.Envelope;
import java.awt.Point;
import java.awt.image.Raster;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.ByteOrder;
import org.openjump.core.rasterimage.GridAscii;
import org.openjump.core.rasterimage.GridFloat;
import org.openjump.core.rasterimage.RasterImageIO;
import org.openjump.core.rasterimage.RasterImageIO.CellSizeXY;
import org.openjump.core.rasterimage.TiffTags;

/**
 *
 * @author AdL
 */
public class RasterConverter {
    
    public static void convert(File input,
            File output, Format outputFormat)
            throws IOException, FileNotFoundException, TiffTags.TiffReadingException, Exception {
        
        // Just copy
        Format inputFormat = findFormat(input);
        if(inputFormat == null) {
            throw new WarningException(PluginUtils.getResources().getString("RasterConverter.UnknownInputFormat"));
        }
        if(inputFormat == outputFormat) {
            FileUtils.copy(input, output);
        }
        
        Point imageDimensions = RasterImageIO.getImageDimensions(input.getAbsolutePath());
        Envelope envelope = RasterImageIO.getGeoReferencing(input.getAbsolutePath(), true, imageDimensions);
        CellSizeXY cellSize = RasterImageIO.getCellSize(input.getAbsolutePath());
        Double noData = RasterImageIO.getNoData(input.getAbsolutePath());
        if(noData == null) {
            noData = -9999.;
        }
        
        if(outputFormat == Format.ESRI_ASCII) {
            
            GridAscii ga = new GridAscii(output.getAbsolutePath(),
                    imageDimensions.x, imageDimensions.y,
                    true, envelope.getMinX(), envelope.getMinY(),
                    cellSize.getAverageCellSize(), noData);
            ga.setRas(RasterImageIO.loadRasterData(input.getAbsolutePath(), null));
            ga.writeGrid();
            
        } else if(outputFormat == Format.ESRI_FLT) {
            
            GridFloat gf = new GridFloat(output.getAbsolutePath(),
                    imageDimensions.x, imageDimensions.y,
                    true, envelope.getMinX(), envelope.getMinY(),
                    cellSize.getAverageCellSize(), noData, ByteOrder.LITTLE_ENDIAN);
            gf.setRas(RasterImageIO.loadRasterData(input.getAbsolutePath(), null));
            gf.writeGrid();
            
        } else if (outputFormat == Format.TIFF) {
        
            RasterImageIO rasterImageIO = new RasterImageIO();
            Raster raster = RasterImageIO.loadRasterData(input.getAbsolutePath(), null);
            
            rasterImageIO.writeImage(output, raster, envelope, cellSize, noData);
            
        }
        
    }
    
    public static Format findFormat(File file) {
        
        String fileName = file.getName().toLowerCase();
        if(fileName.endsWith(".asc") || fileName.endsWith(".txt")) {
            return Format.ESRI_ASCII;
        } else if(fileName.endsWith(".flt")) {
            return Format.ESRI_FLT;
        } else if(fileName.endsWith(".tif") || fileName.endsWith(".tiff")) {
            return Format.TIFF;
        }
        return null;
        
    }
    
    public enum Format {
        
        ESRI_ASCII, ESRI_FLT, TIFF;
        
    }
    
}
