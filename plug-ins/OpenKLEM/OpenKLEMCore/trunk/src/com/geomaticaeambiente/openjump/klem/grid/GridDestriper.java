package com.geomaticaeambiente.openjump.klem.grid;

import java.util.Arrays;
import java.util.List;

/**
 *
 * @author AdL
 */
public class GridDestriper {
    
    public static DoubleBasicGrid assembleDoubleStrips (List<DoubleBasicGrid> grids_l) {
        
        /* Calculate total rows */
        int rowCount = 0;
        
        for(DoubleBasicGrid grid : grids_l) {
            rowCount += grid.getRowCount();
        }
            
        double[][] outGrid = new double[rowCount][];
        int row = 0;
        for (DoubleBasicGrid inGrid : grids_l) {
            for(int r=0; r<inGrid.getRowCount(); r++) {
                outGrid[row] = Arrays.copyOf(inGrid.getData()[r], inGrid.getData()[r].length);
                row++;
            }
        }

        return new DoubleBasicGrid(
                outGrid,
                grids_l.get(0).getCellSize(),
                grids_l.get(0).getNoData(),
                grids_l.get(grids_l.size()-1).getLowerLeftCoord());
            
    }
    
    public static FloatBasicGrid assembleFloatStrips (List<FloatBasicGrid> grids_l) {
        
        /* Calculate total rows */
        int rowCount = 0;
        
        for(FloatBasicGrid grid : grids_l) {
            rowCount += grid.getRowCount();
        }
            
        float[][] outGrid = new float[rowCount][];
        int row = 0;
        for (FloatBasicGrid inGrid : grids_l) {
            for(int r=0; r<inGrid.getRowCount(); r++) {
                outGrid[row] = Arrays.copyOf(inGrid.getData()[r], inGrid.getData()[r].length);
                row++;
            }
        }

        return new FloatBasicGrid(
                outGrid,
                grids_l.get(0).getCellSize(),
                grids_l.get(0).getNoData(),
                grids_l.get(grids_l.size()-1).getLowerLeftCoord());
            
    }
    
    public static LongBasicGrid assembleLongStrips (List<LongBasicGrid> grids_l) {
        
        /* Calculate total rows */
        int rowCount = 0;
        
        for(LongBasicGrid grid : grids_l) {
            rowCount += grid.getRowCount();
        }
            
        long[][] outGrid = new long[rowCount][];
        int row = 0;
        for (LongBasicGrid inGrid : grids_l) {
            for(int r=0; r<inGrid.getRowCount(); r++) {
                outGrid[row] = Arrays.copyOf(inGrid.getData()[r], inGrid.getData()[r].length);
                row++;
            }
        }

        return new LongBasicGrid(
                outGrid,
                grids_l.get(0).getCellSize(),
                grids_l.get(0).getNoData(),
                grids_l.get(grids_l.size()-1).getLowerLeftCoord());
            
    }
    
    public static IntBasicGrid assembleIntStrips (List<IntBasicGrid> grids_l) {
        
        /* Calculate total rows */
        int rowCount = 0;
        
        for(IntBasicGrid grid : grids_l) {
            rowCount += grid.getRowCount();
        }
            
        int[][] outGrid = new int[rowCount][];
        int row = 0;
        for (IntBasicGrid inGrid : grids_l) {
            for(int r=0; r<inGrid.getRowCount(); r++) {
                outGrid[row] = Arrays.copyOf(inGrid.getData()[r], inGrid.getData()[r].length);
                row++;
            }
        }

        return new IntBasicGrid(
                outGrid,
                grids_l.get(0).getCellSize(),
                grids_l.get(0).getNoData(),
                grids_l.get(grids_l.size()-1).getLowerLeftCoord());
            
    }
    
    public static ByteBasicGrid assembleByteStrips (List<ByteBasicGrid> grids_l) {
        
        /* Calculate total rows */
        int rowCount = 0;
        
        for(ByteBasicGrid grid : grids_l) {
            rowCount += grid.getRowCount();
        }
            
        byte[][] outGrid = new byte[rowCount][];
        int row = 0;
        for (ByteBasicGrid inGrid : grids_l) {
            for(int r=0; r<inGrid.getRowCount(); r++) {
                outGrid[row] = Arrays.copyOf(inGrid.getData()[r], inGrid.getData()[r].length);
                row++;
            }
        }

        return new ByteBasicGrid(
                outGrid,
                grids_l.get(0).getCellSize(),
                grids_l.get(0).getNoData(),
                grids_l.get(grids_l.size()-1).getLowerLeftCoord());
            
    }

    public static FlowDirBasicGrid assembleFlowDirStrips (List<FlowDirBasicGrid> grids_l) {
        
        /* Calculate total rows */
        int rowCount = 0;
        
        for(FlowDirBasicGrid grid : grids_l) {
            rowCount += grid.getRowCount();
        }
            
        byte[][] flowDirData = new byte[rowCount][];
        boolean[][] noDataData = new boolean[rowCount][];
        int row = 0;
        for (FlowDirBasicGrid inGrid : grids_l) {
            for(int r=0; r<inGrid.getRowCount(); r++) {
                flowDirData[row] = Arrays.copyOf(inGrid.getByteFlowDirData()[r], inGrid.getByteFlowDirData()[r].length);
                noDataData[row] = Arrays.copyOf(inGrid.getNoDataData()[r], inGrid.getNoDataData()[r].length);
                row++;
            }
        }

        return new FlowDirBasicGrid(
                flowDirData,
                noDataData,
                grids_l.get(0).cellSize,
                grids_l.get(grids_l.size()-1).getLowerLeftCoord());
            
    }
    
}
