package com.geomaticaeambiente.openjump.klem.rastertools;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;

/**
 *
 * @author AdL
 */
public class RasterAverager {
    
    public double average(DoubleBasicGrid inputGrid, DoubleBasicGrid mask) {
        
        double sum = 0;
        long cellCount = 0;

        double xurCornerP = inputGrid.getLowerLeftCoord().x + inputGrid.getColumnCount() * inputGrid.getCellSize();
        double yurCornerP = inputGrid.getLowerLeftCoord().y + inputGrid.getRowCount() * inputGrid.getCellSize();

        
        if(mask == null) {
            
            for(int r=0; r<inputGrid.getRowCount(); r++) {
                for(int c=0; c<inputGrid.getColumnCount(); c++) {
                    if(!inputGrid.isNoData(inputGrid.getData()[r][c])) {
                        sum += inputGrid.getData()[r][c];
                        cellCount++;
                    }
                }
            }
            
        } else {
        
            // Calc average parameters over mask
            for(int rM=0; rM<mask.getRowCount(); rM++){
                for(int cM=0; cM<mask.getColumnCount(); cM++){

                    double xM = mask.getLowerLeftCoord().x + mask.getCellSize() * cM - mask.getCellSize() / 2;
                    double yM = mask.getLowerLeftCoord().y + mask.getCellSize() * (mask.getRowCount() - rM) - mask.getCellSize() / 2;

                    if (xM >= inputGrid.getLowerLeftCoord().x && xM < xurCornerP
                            && yM >= inputGrid.getLowerLeftCoord().y && yM < yurCornerP
                            && !mask.isNoData(mask.getData()[rM][cM])) {

                        int cP = (int)((xM - inputGrid.getLowerLeftCoord().x) / inputGrid.getCellSize()) + 1;
                        int rP = inputGrid.getRowCount() - (int)((yM - inputGrid.getLowerLeftCoord().y) / inputGrid.getCellSize());

                        if(!inputGrid.isNoData(inputGrid.getData()[rP][cP])) {
                            sum += inputGrid.getData()[rP][cP];
                            cellCount++;
                        }
                    }
                }
            }
            
        }
        if(cellCount > 0) {
            return sum / cellCount;
        } else {
            return Double.NaN;
        }

    }
    
    
}
