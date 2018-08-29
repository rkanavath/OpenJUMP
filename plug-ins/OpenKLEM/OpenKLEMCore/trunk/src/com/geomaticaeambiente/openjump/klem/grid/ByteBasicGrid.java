package com.geomaticaeambiente.openjump.klem.grid;

import com.vividsolutions.jts.geom.Coordinate;

public class ByteBasicGrid extends BasicGrid {
    
    public ByteBasicGrid(double[][] data, double cellSize, byte noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length,  cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new byte[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = (byte) data[r][c];
            }
        }
        this.noData = noData;
        
    }

    public ByteBasicGrid(float[][] data, double cellSize, byte noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new byte[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = (byte) data[r][c];
            }
        }
        this.noData = noData;
        
    }
    
    public ByteBasicGrid(long[][] data, double cellSize, byte noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new byte[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = (byte) data[r][c];
            }
        }
        this.noData = noData;
        
    }
    
    public ByteBasicGrid(int[][] data, double cellSize, byte noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new byte[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = (byte) data[r][c];
            }
        }
        this.noData = noData;
        
    }
    
    public ByteBasicGrid(byte[][] data, double cellSize, byte noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = data;
        this.noData = noData;
        
    }

    public byte[][] getData() {
        return data;
    }

    public void setData(byte[][] data) {
        this.data = data;
    }
    
    public byte getValue(java.awt.Point cell) {
        return getValue(cell.x, cell.y);
    }
    
    public byte getValue(int col, int row) {
        return data[row][col];
    }
    
    public void setValue(int col, int row, byte value) {
        data[row][col] = value;
    }
    
    public byte getNoData() {
        return noData;
    }

    public void setNoData(byte noData) {
        this.noData = noData;
    }

    public boolean isNoData(byte value) {
        return(value == noData);
    }
    
    private byte noData;
    protected byte[][] data;
    
    
}

