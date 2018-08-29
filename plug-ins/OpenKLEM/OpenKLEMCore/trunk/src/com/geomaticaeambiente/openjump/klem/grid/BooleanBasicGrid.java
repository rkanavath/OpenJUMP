package com.geomaticaeambiente.openjump.klem.grid;

import com.vividsolutions.jts.geom.Coordinate;

public class BooleanBasicGrid extends BasicGrid {
    
    public BooleanBasicGrid(boolean[][] data, double cellSize, boolean noData,
            Coordinate lowerLeftCoord) {
        super(data[0].length, data.length, cellSize, lowerLeftCoord);
        
        this.data = new boolean[rowCount][columnCount];
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                this.data[r][c] = data[r][c];
            }
        }
        this.noData = noData;
        
    }

    public boolean[][] getData() {
        return data;
    }

    public void setData(boolean[][] data) {
        this.data = data;
    }
    
    public void setValue(int col, int row, boolean value) {
        data[row][col] = value;
    }
    
    public boolean getNoData() {
        return noData;
    }

    public void setNoData(boolean noData) {
        this.noData = noData;
    }

    public boolean isNoData(boolean value) {
        return(value == noData);
    }
    
    private boolean noData;
    protected boolean[][] data;
    
}

