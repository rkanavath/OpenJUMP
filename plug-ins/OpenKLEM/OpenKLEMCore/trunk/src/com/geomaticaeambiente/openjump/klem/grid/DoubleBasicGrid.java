package com.geomaticaeambiente.openjump.klem.grid;

import com.vividsolutions.jts.geom.Coordinate;

public class DoubleBasicGrid extends BasicGrid {
    
    public DoubleBasicGrid(double[][] data, double cellSize, double noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        this.data = data;
        this.noData = noData;
        
    }

    public DoubleBasicGrid(float[][] data, double cellSize, double noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new double[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = data[r][c];
            }
        }
        this.noData = noData;
        
    }
    
    public DoubleBasicGrid(long[][] data, double cellSize, double noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new double[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = data[r][c];
            }
        }
        this.noData = noData;
        
    }
    
    public DoubleBasicGrid(int[][] data, double cellSize, double noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new double[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = data[r][c];
            }
        }
        this.noData = noData;
        
    }
    
    public DoubleBasicGrid(byte[][] data, double cellSize, double noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new double[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = data[r][c];
            }
        }
        this.noData = noData;
        
    }
    
    public DoubleBasicGrid(boolean[][] data, double cellSize, double noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        if(data.length > 0) {
            this.columnCount = data[0].length;
            this.rowCount = data.length;
        }
        
        this.data = new double[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                
                if(data[r][c]) {
                    this.data[r][c] = 1;
                } else {
                    this.data[r][c] = 0;
                }
                
                //this.data[r][c] = data[r][c] ? 1 : 0;
            }
        }
        this.noData = noData;
        
    }    
    
    public double[][] getData() {
        return data;
    }

    public void setData(double[][] data) {
        this.data = data;
    }
    
    public double getValue(java.awt.Point cell) {
        return getValue(cell.x, cell.y);
    }

    public double getValue(int col, int row) {
        return data[row][col];
    }
    
    public void setValue(int col, int row, double value) {
        data[row][col] = value;
    }
    
    public double getNoData() {
        return noData;
    }

    public void setNoData(double noData) {
        this.noData = noData;
    }

    public boolean isNoData(double value) {
        if(Double.isInfinite(noData) && Double.isInfinite(value)) {
            return true;
        }
        if(Double.isNaN(noData) && Double.isNaN(value)) {
            return true;
        }
        return(value == noData);
    }
    
    private double noData;
    protected double[][] data;
    
}