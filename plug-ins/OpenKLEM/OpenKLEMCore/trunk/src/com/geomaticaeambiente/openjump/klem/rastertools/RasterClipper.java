package com.geomaticaeambiente.openjump.klem.rastertools;

import com.geomaticaeambiente.openjump.klem.grid.ByteBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import java.util.Arrays;

/**
 *
 * @author AdL
 */
public class RasterClipper {
    
    public static DoubleBasicGrid clip(DoubleBasicGrid inputGrid, DoubleBasicGrid clipperGrid) {
        
        DoubleBasicGrid outGrid = new DoubleBasicGrid(new double[clipperGrid.getRowCount()][clipperGrid.getColumnCount()],
                clipperGrid.getCellSize(), inputGrid.getNoData(), clipperGrid.getLowerLeftCoord());
        for(int r=0; r<clipperGrid.getRowCount(); r++){
            Arrays.fill(outGrid.getData()[r], inputGrid.getNoData());
        }
        
        
        // Compare cutter and ras in
        for(int r=0; r<clipperGrid.getRowCount(); r++){
            for(int c=0; c<clipperGrid.getColumnCount(); c++){
                
                java.awt.Point inputCell = inputGrid.fromCoordinateToCell(clipperGrid.fromCellToCoordinate(new java.awt.Point(c, r)));

                // Check cell1 inside ras2
                if(inputCell.x >= 0 && inputCell.y > 0 &&
                        inputCell.x < inputGrid.getColumnCount()
                        && inputCell.y < inputGrid.getRowCount()){
                    if(!clipperGrid.isNoData(clipperGrid.getValue(c, r))){
                        outGrid.setValue(c, r, inputGrid.getValue(inputCell));
                    }
                }
            }
        }        
        
        
        return outGrid;
    }
    
    public static ByteBasicGrid clip(ByteBasicGrid inputGrid, DoubleBasicGrid clipperGrid) {
        
        ByteBasicGrid outGrid = new ByteBasicGrid(new byte[clipperGrid.getRowCount()][clipperGrid.getColumnCount()],
                clipperGrid.getCellSize(), inputGrid.getNoData(), clipperGrid.getLowerLeftCoord());
        for(int r=0; r<clipperGrid.getRowCount(); r++){
            Arrays.fill(outGrid.getData()[r], inputGrid.getNoData());
        }
        
        
        // Compare cutter and ras in
        for(int r=0; r<clipperGrid.getRowCount(); r++){
            for(int c=0; c<clipperGrid.getColumnCount(); c++){
                
                java.awt.Point inputCell = inputGrid.fromCoordinateToCell(clipperGrid.fromCellToCoordinate(new java.awt.Point(c, r)));

                // Check cell1 inside ras2
                if(inputCell.x >= 0 && inputCell.y > 0 &&
                        inputCell.x < inputGrid.getColumnCount()
                        && inputCell.y < inputGrid.getRowCount()){
                    if(!clipperGrid.isNoData(clipperGrid.getValue(c, r))){
                        outGrid.setValue(c, r, inputGrid.getValue(inputCell));
                    }
                }
            }
        }        
        
        
        return outGrid;
    }    
    
    public static FlowDirBasicGrid clip(FlowDirBasicGrid inputGrid, DoubleBasicGrid clipperGrid) {
        
        FlowDirBasicGrid outGrid = new FlowDirBasicGrid(
                clipperGrid.getColumnCount(), clipperGrid.getRowCount(),               
                clipperGrid.getCellSize(), clipperGrid.getLowerLeftCoord());
        
//        for(int r=0; r<clipperGrid.getRowCount(); r++){
//            Arrays.fill(outGrid.getByteFlowDirData()[r], inputGrid.getNoData());
//        }
        
        
        // Compare cutter and ras in
        for(int r=0; r<clipperGrid.getRowCount(); r++){
            for(int c=0; c<clipperGrid.getColumnCount(); c++){
                
                java.awt.Point inputCell = inputGrid.fromCoordinateToCell(clipperGrid.fromCellToCoordinate(new java.awt.Point(c, r)));

                // Check cell1 inside ras2
                if(inputCell.x >= 0 && inputCell.y > 0 &&
                        inputCell.x < inputGrid.getColumnCount()
                        && inputCell.y < inputGrid.getRowCount()){
                    if(!clipperGrid.isNoData(clipperGrid.getValue(c, r))){
                        outGrid.setByteFlowDirValue(c, r, inputGrid.getByteFlowDirValue(inputCell));
                        outGrid.setNoData(c, r, inputGrid.isNoData(inputCell));
                    }
                }
            }
        }        
        
        
        return outGrid;
    }  
    
    public static DoubleBasicGrid clip(DoubleBasicGrid inputGrid, Geometry clipperGeom,
            Coordinate lowerLeftCorner) throws Exception {

        if(lowerLeftCorner == null) {
            
            lowerLeftCorner = new Coordinate();
            
            lowerLeftCorner.x = clipperGeom.getEnvelopeInternal().getMinX();
            lowerLeftCorner.y = clipperGeom.getEnvelopeInternal().getMinY();
            
            // Snap origin to cell
            lowerLeftCorner.x = Math.floor(lowerLeftCorner.x / inputGrid.getCellSize()) * inputGrid.getCellSize();
            lowerLeftCorner.y = Math.floor(lowerLeftCorner.y / inputGrid.getCellSize()) * inputGrid.getCellSize();

        }
        
        int columnCount = ((int) Math.ceil(
                (clipperGeom.getEnvelopeInternal().getMaxX() - lowerLeftCorner.x)
                        / inputGrid.getCellSize()));
        int rowCount = ((int) Math.ceil(
                (clipperGeom.getEnvelopeInternal().getMaxY() - lowerLeftCorner.y)
                        / inputGrid.getCellSize()));  
        
        DoubleBasicGrid rasterizedGrid = Rasterizer.rasterize(
                clipperGeom,
                lowerLeftCorner,
                columnCount, rowCount,
                inputGrid.getCellSize(), Double.NaN);
        
        return rasterizedGrid;
    }
    
}
