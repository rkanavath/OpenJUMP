package com.geomaticaeambiente.openjump.klem.grid;

import com.vividsolutions.jts.geom.Coordinate;
import java.util.Arrays;

/**
 *
 * @author AdL
 */
public class GridStriper {
    
    public static DoubleBasicGrid[] stripeDoubleGrid(DoubleBasicGrid grid,
            int stripeRowCount) {
        
        int rowCount = grid.getRowCount();
        int stripeCount = (int) Math.ceil((double) rowCount / stripeRowCount);
        
        DoubleBasicGrid[] outStripes = new DoubleBasicGrid[stripeCount];
        for(int s=0; s<stripeCount; s++) {
            
            int stripeEffectiveRows;
            if(s != stripeCount - 1) {
                stripeEffectiveRows = stripeRowCount;
            } else {
                stripeEffectiveRows = rowCount - (s * stripeRowCount);                
            }

            double[][] stripeData = new double[stripeEffectiveRows][grid.getColumnCount()];
            int offset = s * stripeRowCount;

            for(int r=0; r<stripeEffectiveRows; r++) {
                stripeData[r] = Arrays.copyOf(grid.getData()[r+offset], grid.getData()[r].length);
            }
            
            outStripes[s] = new DoubleBasicGrid(
                    stripeData,
                    grid.getCellSize(),
                    grid.getNoData(),
                    calcLowerLeftCoord(
                            grid.getLowerLeftCoord(), rowCount, grid.getCellSize(), offset, stripeEffectiveRows));

        }   
        
        return outStripes;
        
    }
    
    public static ByteBasicGrid[] stripeByteGrid(ByteBasicGrid grid,
            int stripeRowCount) {
        
        int rowCount = grid.getRowCount();
        int stripeCount = (int) Math.ceil((double) rowCount / stripeRowCount);
        
        ByteBasicGrid[] outStripes = new ByteBasicGrid[stripeCount];
        for(int s=0; s<stripeCount; s++) {
            
            int stripeEffectiveRows;
            if(s != stripeCount - 1) {
                stripeEffectiveRows = stripeRowCount;
            } else {
                stripeEffectiveRows = rowCount - (s * stripeRowCount);                
            }

            byte[][] stripeData = new byte[stripeEffectiveRows][grid.getColumnCount()];
            int offset = s * stripeRowCount;

            for(int r=0; r<stripeEffectiveRows; r++) {
                stripeData[r] = Arrays.copyOf(grid.getData()[r+offset], grid.getData()[r].length);
            }
            
            outStripes[s] = new ByteBasicGrid(
                    stripeData,
                    grid.getCellSize(),
                    grid.getNoData(),
                    calcLowerLeftCoord(grid.getLowerLeftCoord(), rowCount, grid.getCellSize(), offset, stripeRowCount));

        }   
        
        return outStripes;
        
    }    

    public static FlowDirBasicGrid[] stripeFlowDirBasicGrid(FlowDirBasicGrid grid,
            int stripeRowCount) {
        
        int rowCount = grid.getRowCount();
        int stripeCount = (int) Math.ceil((double) rowCount / stripeRowCount);
        
        FlowDirBasicGrid[] outStripes = new FlowDirBasicGrid[stripeCount];
        for(int s=0; s<stripeCount; s++) {
            
            int stripeEffectiveRows;
            if(s != stripeCount - 1) {
                stripeEffectiveRows = stripeRowCount;
            } else {
                stripeEffectiveRows = rowCount - (s * stripeRowCount);                
            }

            byte[][] flowDirData = new byte[stripeEffectiveRows][grid.getColumnCount()];
            boolean[][] noDataData = new boolean[stripeEffectiveRows][grid.getColumnCount()];
            int offset = s * stripeRowCount;

            for(int r=0; r<stripeEffectiveRows; r++) {
                flowDirData[r] = Arrays.copyOf(grid.getByteFlowDirData()[r+offset], grid.getByteFlowDirData()[r].length);
                noDataData[r] = Arrays.copyOf(grid.getNoDataData()[r+offset], grid.getNoDataData()[r].length);
            }
            
            outStripes[s] = new FlowDirBasicGrid(
                    flowDirData,
                    noDataData,
                    grid.cellSize,
                    calcLowerLeftCoord(grid.getLowerLeftCoord(), rowCount, grid.getCellSize(), offset, stripeRowCount));
            
        }   
        
        return outStripes;
        
    } 
    
//    public static FlowDirBasicGrid[] stripeFlowDirGrid(FlowDirBasicGrid grid,
//            int stripeRowCount) {
//        
//        int rowCount = grid.getRowCount();
//        int stripeCount = (int) Math.ceil((double) rowCount / stripeRowCount);
//        
//        FlowDirBasicGrid[] outStripes = new FlowDirBasicGrid[stripeCount];
//        for(int s=0; s<stripeCount; s++) {
//            
////            int stripeEffectiveRows;
////            if(s != stripeCount - 1) {
////                stripeEffectiveRows = stripeRowCount;
////            } else {
////                stripeEffectiveRows = rowCount - (s * stripeRowCount);                
////            }
////
////            byte[][] stripeData = new byte[stripeEffectiveRows][grid.getColumnCount()];
//            int offset = s * stripeRowCount;
//
//            outStripes[s] =  grid.getStripe(offset, stripeRowCount);
//            
////            for(int r=0; r<stripeEffectiveRows; r++) {
////                stripeData[r] = Arrays.copyOf(grid.getData()[r+offset], grid.getData()[r].length);
////            }
////            
////            outStripes[s] = new FlowDirBasicGrid(
////                    stripeData,
////                    grid.getCellSize(),
////                    grid.getNoData(),
////                    calcLowerLeftCoord(grid.getLowerLeftCoord(), rowCount, grid.getCellSize(), offset, stripeRowCount));
//
//        }   
//        
//        return outStripes;
//        
//    }    
    
    public static Coordinate calcLowerLeftCoord(
            Coordinate lowerLeftCoord, int rowCount, double cellSize,
            int offset, int stripeRowCount) {
        
        return new Coordinate(
                            lowerLeftCoord.x,
                            lowerLeftCoord.y
                                + rowCount * cellSize
                                - (offset * cellSize)
                                - (stripeRowCount * cellSize));
        
    }
    
    
}
