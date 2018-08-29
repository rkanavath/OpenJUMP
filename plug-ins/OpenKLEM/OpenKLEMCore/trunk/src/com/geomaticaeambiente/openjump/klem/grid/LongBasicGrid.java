package com.geomaticaeambiente.openjump.klem.grid;

import com.vividsolutions.jts.geom.Coordinate;

public class LongBasicGrid extends BasicGrid {
    
    public LongBasicGrid(double[][] data, double cellSize, long noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new long[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = (long) data[r][c];
            }
        }
        this.noData = noData;
        
    }

    public LongBasicGrid(float[][] data, double cellSize, long noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new long[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = (long) data[r][c];
            }
        }
        this.noData = noData;
        
    }
    
    public LongBasicGrid(long[][] data, double cellSize, long noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = data;
        this.noData = noData;
        
    }
    
    public LongBasicGrid(int[][] data, double cellSize, long noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new long[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = data[r][c];
            }
        }
        this.noData = noData;
        
    }
    
    public LongBasicGrid(byte[][] data, double cellSize, long noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new long[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = data[r][c];
            }
        }
        this.noData = noData;
        
    }

    public long[][] getData() {
        return data;
    }

    public void setData(long[][] data) {
        this.data = data;
    }
    
    public void setValue(int col, int row, long value) {
        data[row][col] = value;
    }
    
    public long getNoData() {
        return noData;
    }

    public void setNoData(long noData) {
        this.noData = noData;
    }

    public boolean isNoData(long value) {
        return(value == noData);
    }
    
    private long noData;
    protected long[][] data;
    
}

