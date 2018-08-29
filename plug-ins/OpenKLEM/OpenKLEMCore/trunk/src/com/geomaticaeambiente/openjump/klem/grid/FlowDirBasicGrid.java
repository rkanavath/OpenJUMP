package com.geomaticaeambiente.openjump.klem.grid;

import com.geomaticaeambiente.openjump.klem.flowdir.BitOps;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;
import com.vividsolutions.jts.geom.Coordinate;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * @author AdL
 */
public class FlowDirBasicGrid extends BasicGrid {

    public FlowDirBasicGrid(int columnsCount, int rowsCount, double cellSize, Coordinate lowerLeftCoord) {
        super(columnsCount, rowsCount, cellSize, lowerLeftCoord);
        
        flowDirData = new byte[rowCount][columnCount];
        noDataData = new boolean[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                noDataData[r][c] = false;
            }
        }
        
        initialize();
        
    }

    public FlowDirBasicGrid(byte[][] flowDirData, boolean[][] noDataData, double cellSize, Coordinate lowerLeftCoord) {
        super(flowDirData[0].length, flowDirData.length, cellSize, lowerLeftCoord);
        
        this.flowDirData = flowDirData;
        this.noDataData = noDataData;
        
        initialize();
        
    }
    
    private void initialize() {

        /**
         *  8  4    2
         * 16  *    1 
         * 32 64 -128
         * 
         */
        
        byteToTarbotonian.put((byte) 0, -1);
        byteToTarbotonian.put((byte) 1, 0);
        byteToTarbotonian.put((byte) 2, 1);
        byteToTarbotonian.put((byte) 4, 2);
        byteToTarbotonian.put((byte) 8, 3);
        byteToTarbotonian.put((byte) 16, 4);
        byteToTarbotonian.put((byte) 32, 5);
        byteToTarbotonian.put((byte) 64, 6);
        byteToTarbotonian.put((byte) -128, 7);        
        
        byteToCazorzian.put((byte) 0, -1);
        byteToCazorzian.put((byte) 1, 7);
        byteToCazorzian.put((byte) 2, 0);
        byteToCazorzian.put((byte) 4, 1);
        byteToCazorzian.put((byte) 8, 2);
        byteToCazorzian.put((byte) 16, 3);
        byteToCazorzian.put((byte) 32, 4);
        byteToCazorzian.put((byte) 64, 5);
        byteToCazorzian.put((byte) -128, 6);
        
        byteToEsri.put((byte) 0, -1);
        byteToEsri.put((byte) 1, 1);
        byteToEsri.put((byte) 2, 128);
        byteToEsri.put((byte) 4, 64);
        byteToEsri.put((byte) 8, 32);
        byteToEsri.put((byte) 16, 16);
        byteToEsri.put((byte) 32, 8);
        byteToEsri.put((byte) 64, 4);
        byteToEsri.put((byte) -128, 2);
        
    }
    
    public byte getByteFlowdirValue(int c, int r) {
        return flowDirData[r][c];
    }
    
    public void setByteFlowDirValue(int col, int row, byte flowDirValue) {
        this.flowDirData[row][col] = flowDirValue;
    }
    
    public byte getByteFlowDirValue(Point cell) {
        return getByteFlowdirValue(cell.x, cell.y);
    }
    
    public void setByteFlowDirValue(Point cell, byte flowDirValue) {
        FlowDirBasicGrid.this.setByteFlowDirValue(cell.x, cell.y, flowDirValue);
    }    
    
    public byte[][] getByteFlowDirData() {
        return flowDirData;
    }
    
    public void setFlowDirData(byte[][] data) {
        this.flowDirData = data;
    }    
    
    public boolean isOutlet(int col, int row) {
        return flowDirData[row][col] == 0;
    }
    
    public boolean isOutlet(Point cell) {
        return isOutlet(cell.x, cell.y);
    }
    
    public void setOutlet(int col, int row) {
        flowDirData[row][col] = 0;
        setNoData(col, row, false);
    }
    
    public void setOutlet(Point cell) {
        setOutlet(cell.x, cell.y);
    }    
    
    public boolean[][] getNoDataData() {
        return noDataData;
    }    
    
    public void setNoDataData(boolean[][] noData) {
        this.noDataData = noData;
    }
    
    public boolean isNoData(int col, int row) {
        return noDataData[row][col];
    }
    
    public void setNoData(int col, int row, boolean isNoData) {
        noDataData[row][col] = isNoData;
    }    
    
    public boolean isNoData(Point cell) {
        return isNoData(cell.x, cell.y);
    }
    
    public void setNoData(Point cell, boolean isNoData) {
        setNoData(cell.x, cell.y, isNoData);
    }    
    
    public int getTarbotonianFlowDirValue(int col, int row) {
        return byteToTarbotonian.get(flowDirData[row][col]);
    }
    
    public int getTarbotonianFlowDirValue(Point cell) {
        return getTarbotonianFlowDirValue(cell.x, cell.y);
    }
    
    public int getCazorzianFlowDirValue(int col, int row) {
        return byteToCazorzian.get(flowDirData[row][col]);
    }
    
    public int getCarorzianFlowDirValue(Point cell) {
        return getCazorzianFlowDirValue(cell.x, cell.y);
    }
    
    public int getEsriFlowDirValue(int col, int row) {
        return byteToEsri.get(flowDirData[row][col]);
    }
    
    public int getEsriFlowDirValue(Point cell) {
        return getEsriFlowDirValue(cell.x, cell.y);
    }
    
    public Point[] sinkCells(int col, int row) {
        
        boolean[] vals = BitOps.readBitValues(flowDirData[row][col]);
        List<Point> sinks_l = new ArrayList<Point>();
        for(int i=0; i<8; i++) {
            if(vals[i]) {
                sinks_l.add(
                        new Point(
                                col + Shifter.getColShift(i),
                                row + Shifter.getRowShift(i)));
            }
        }
        return sinks_l.toArray(new Point[sinks_l.size()]);
        
    }
    
    public boolean flowsTowards(int col, int row, int direction) {
        return BitOps.readBitValue(flowDirData[row][col], direction);
    }

    public boolean flowsTowards(Point cell, int direction) {
        return FlowDirBasicGrid.this.flowsTowards(cell.x, cell.y, direction);
    }
    
    public boolean flowsTowards(byte flowDirsByte, int direction) {
        return BitOps.readBitValue(flowDirsByte, direction);
    }       
    
    public boolean flowsInto(Point sourceCell, Point destCell) {
        
        int shiftIndex = Shifter.getShiftIndex(sourceCell, destCell);
        return FlowDirBasicGrid.this.flowsTowards(sourceCell, shiftIndex);
        
    } 

//    public int getFlowDirValue(int col, int row) {
//        for(int i=0; i<8; i++) {
//            if(BitOps.readBitValue(flowDirData[row][col], i)) {
//                return i;
//            }
//        }
//        return -1;
//    }
//
//    public int getFlowDirValue(Point cell) {
//        return getFlowDirValue(cell.x, cell.y);
//    }
    
//    public boolean[] getFlowDirValues(int col, int row) {
//        return BitOps.readBitValues(flowDirData[row][col]);
//    }
//
//    public boolean[] getFlowDirValues(Point cell) {
//        return getFlowDirValues(cell.x, cell.y);
//    }
    
    public FlowDirBasicGrid getStripe(int startRow, int stripeRowCount) {
        
        if(startRow + stripeRowCount > rowCount) {
            stripeRowCount = stripeRowCount - (startRow + stripeRowCount - rowCount); 
        }
        
        FlowDirBasicGrid flowDirStripe = new FlowDirBasicGrid(
                    columnCount,
                    stripeRowCount,
                    cellSize,
                    GridStriper.calcLowerLeftCoord(
                            getLowerLeftCoord(), rowCount, getCellSize(), startRow, stripeRowCount));

        for(int r=startRow; r<=startRow+stripeRowCount; r++) {
            flowDirStripe.getByteFlowDirData()[r] = Arrays.copyOf(flowDirData[r], flowDirData[r].length);
        }
        
        return flowDirStripe;
        
    }
    
    public final Map<Byte,Integer> byteToTarbotonian = new HashMap<Byte,Integer>();
    public final Map<Byte,Integer> byteToCazorzian = new HashMap<Byte,Integer>();
    public final Map<Byte,Integer> byteToEsri = new HashMap<Byte,Integer>();
    
    protected byte[][] flowDirData;
    protected boolean[][] noDataData;
    
}
