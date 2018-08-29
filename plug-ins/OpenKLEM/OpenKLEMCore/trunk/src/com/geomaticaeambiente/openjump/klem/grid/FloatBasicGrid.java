package com.geomaticaeambiente.openjump.klem.grid;

import com.vividsolutions.jts.geom.Coordinate;

public class FloatBasicGrid extends BasicGrid {
    
//    public FloatBasicGrid(double[][] data, double cellSize, float noData,
//            Coordinate lowerLeftCoord) {
//        super(cellSize, lowerLeftCoord);
//        
//        if(data.length > 0) {
//            this.columnCount = data[0].length;
//            this.rowCount = data.length;
//        }
//        
//        this.data = new float[rowCount][columnCount];
//        for(int r=0; r<rowCount; r++) {
//            for(int c=0; c<columnCount; c++) {
//                this.data[r][c] = (float) data[r][c];
//            }
//        }
//        this.noData = noData;
//        
//    }

    public FloatBasicGrid(float[][] data, double cellSize, float noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = data;
        this.noData = noData;
        
    }
    
    public FloatBasicGrid(long[][] data, double cellSize, float noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new float[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = data[r][c];
            }
        }
        this.noData = noData;
        
    }
    
    public FloatBasicGrid(int[][] data, double cellSize, float noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new float[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = data[r][c];
            }
        }
        this.noData = noData;
        
    }
    
    public FloatBasicGrid(byte[][] data, double cellSize, float noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new float[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = data[r][c];
            }
        }
        this.noData = noData;
        
    }

    public float[][] getData() {
        return data;
    }

    public void setData(float[][] data) {
        this.data = data;
    }
    
    public float getValue(java.awt.Point cell) {
        return getValue(cell.x, cell.y);
    }

    public float getValue(int col, int row) {
        return data[row][col];
    }
    
    public void setValue(int col, int row, float value) {
        data[row][col] = value;
    }
    
    public void setValue(java.awt.Point cell, float value) {
        data[cell.y][cell.x] = value;
    }
    
    public float getNoData() {
        return noData;
    }

    public void setNoData(float noData) {
        this.noData = noData;
    }

    public boolean isNoData(float value) {
        if(Float.isInfinite(noData) && Float.isInfinite(value)) {
            return true;
        }
        if(Float.isNaN(noData) && Float.isNaN(value)) {
            return true;
        }
        return(value == noData);
    }
    
    private float noData;
    protected float[][] data;
    
}

