package com.geomaticaeambiente.openjump.klem.parallel;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.vividsolutions.jts.geom.Coordinate;

/**
 *
 * @author AdL
 */
public class GridClipper {
    
    public static DoubleBasicGrid extractValues(DoubleBasicGrid inGrid, double[] selectedValues) {
        
        // Clipping
        int colMin = Integer.MAX_VALUE;
        int colMax = -colMin;
        int rowMin = Integer.MAX_VALUE;
        int rowMax = -rowMin;
   
        // Find bounding box
        int rowCount = inGrid.getRowCount();
        int colCount = inGrid.getColumnCount();
        for(int r=0; r<rowCount; r++){
            for(int c=0; c<colCount; c++){
                if(!inGrid.isNoData(inGrid.getData()[r][c])){
                    
                    boolean cellIsValid = false;
                    if(selectedValues != null && selectedValues.length > 0) {                    
                        for(double value : selectedValues) {
                            if(inGrid.getData()[r][c] == value) {
                                cellIsValid = true;
                                break;
                            } 
                        }
                    } else {
                        if(inGrid.getData()[r][c] > 0) {
                            cellIsValid = true;
                        }
                    }

                    if(cellIsValid) {
                    
                        colMin = Math.min(colMin, c);
                        colMax = Math.max(colMax, c);
                        rowMin = Math.min(rowMin, r);
                        rowMax = Math.max(rowMax, r);
                    
                    }
                }
            }
        }

        int outColCount = colMax - colMin + 1;
        int outRowCount = rowMax - rowMin + 1;

        double cellSize = inGrid.getCellSize();
        double xllCornerOut = inGrid.getLowerLeftCoord().x + (colMin) * cellSize;
        double yllCornerOut = inGrid.getLowerLeftCoord().y + (rowCount - rowMin - outRowCount) * cellSize;
        Coordinate outllCorner = new Coordinate(xllCornerOut, yllCornerOut);
        
        double[][] outGrid = new double[outRowCount][outColCount];
        for(int r=0; r<outRowCount; r++){
            for(int c=0; c<outColCount; c++){
                
                outGrid[r][c] = inGrid.getNoData();
                if(selectedValues != null) {
                    for(double value : selectedValues) {
                        if(inGrid.getData()[r+rowMin][c+colMin] == value) {
                            outGrid[r][c] = inGrid.getData()[r+rowMin][c+colMin];
                        }
                    }
                } else {
                    outGrid[r][c] = inGrid.getData()[r+rowMin][c+colMin];
                }
                
                
            }
        }
        
        return new DoubleBasicGrid(outGrid, cellSize, inGrid.getNoData(), outllCorner);
        
    }
    
}
