package com.geomaticaeambiente.openjump.klem.grid;

import com.vividsolutions.jts.geom.Coordinate;

public class IntBasicGrid extends BasicGrid {
    
    public IntBasicGrid(double[][] data, double cellSize, int noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new int[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = (int) data[r][c];
            }
        }
        this.noData = noData;
        
    }

    public IntBasicGrid(float[][] data, double cellSize, int noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new int[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = (int) data[r][c];
            }
        }
        this.noData = noData;
        
    }
    
    public IntBasicGrid(long[][] data, double cellSize, int noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new int[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = (int) data[r][c];
            }
        }
        this.noData = noData;
        
    }
    
    public IntBasicGrid(int[][] data, double cellSize, int noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = data;
        this.noData = noData;
        
    }
    
    public IntBasicGrid(byte[][] data, double cellSize, int noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new int[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = data[r][c];
            }
        }
        this.noData = noData;
        
    }

    public int[][] getData() {
        return data;
    }

    public void setData(int[][] data) {
        this.data = data;
    }
    
    public int getValue(java.awt.Point cell) {
        return getValue(cell.x, cell.y);
    }
    
    public int getValue(int col, int row) {
        return data[row][col];
    }
    
    public void setValue(int col, int row, int value) {
        data[row][col] = value;
    }
    
    public void setValue(java.awt.Point cell, int value) {
        data[cell.y][cell.x] = value;
    }
    
    public int getNoData() {
        return noData;
    }

    public void setNoData(int noData) {
        this.noData = noData;
    }

    public boolean isNoData(int value) {
        return(value == noData);
    }
    
    private int noData;
    protected int[][] data;
    
}

