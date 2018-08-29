package com.geomaticaeambiente.openjump.klem.grid;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 *
 * @author Adl
 */
public abstract class BasicGrid {
    
    protected BasicGrid(int columnCount, int rowCount, double cellSize, Coordinate lowerLeftCoord) {
        this.columnCount = columnCount;
        this.rowCount = rowCount;
        this.cellSize = cellSize;
        this.lowerLeftCoord = lowerLeftCoord;
    }    
    
    public boolean isSpatiallyConsistentWith(BasicGrid otherGrid) {
        
        if(this.cellSize != otherGrid.cellSize) {
            return false;
        }
        
        if(this.lowerLeftCoord.x != otherGrid.lowerLeftCoord.x) {
            return false;
        }
        
        if(this.lowerLeftCoord.y != otherGrid.lowerLeftCoord.y) {
            return false;
        }
        
        if(this.columnCount != otherGrid.columnCount) {
            return false;
        }
        
        if(this.rowCount != otherGrid.rowCount) {
            return false;
        }
        
        return true;
            
    }
    
    public int getColumnCount() {
        return columnCount;
    }

    public void setColumnCount(int columnCount) {
        this.columnCount = columnCount;
    }

    public int getRowCount() {
        return rowCount;
    }

    public void setRowCount(int rowCount) {
        this.rowCount = rowCount;
    }

    public double getCellSize() {
        return cellSize;
    }

    public void setCellSize(double cellSize) {
        this.cellSize = cellSize;
    }
    
    public Coordinate getLowerLeftCoord() {
        return lowerLeftCoord;
    }

    public void setLowerLeftCoord(Coordinate lowerLeftCoord) {
        this.lowerLeftCoord = lowerLeftCoord;
    }
    
    public Envelope getEnvelope() {
        Envelope envelope = new Envelope(lowerLeftCoord, new Coordinate(
                lowerLeftCoord.x + columnCount * cellSize, lowerLeftCoord.y + rowCount * cellSize));
        
        return envelope;        
    }
    
    public java.awt.Point fromCoordinateToCell(Coordinate coord) {
        
        java.awt.Point point = new java.awt.Point();
        point.x = (int)Math.floor((coord.x - lowerLeftCoord.x) / cellSize);
        point.y = rowCount - (int)Math.floor((coord.y - lowerLeftCoord.y) / cellSize) - 1;

        return point; 
        
    }
    
    public Coordinate fromCellToCoordinate(java.awt.Point cell) {
        
        Coordinate coord = new Coordinate();
        coord.x = lowerLeftCoord.x + cell.x * cellSize + 0.5 * cellSize;
        coord.y = lowerLeftCoord.y + (rowCount - cell.y) * cellSize - 0.5 * cellSize;
        return coord;
        
    }
    
    public boolean belongsToGrid(int col, int row) {
        return (col >=0 && col < columnCount && row >= 0 && row < rowCount);
    }
    
    public boolean belongsToGrid(java.awt.Point cell) {
        return belongsToGrid(cell.x, cell.y);
    }
    
    protected int columnCount;
    protected int rowCount;
    protected double cellSize;
    protected Coordinate lowerLeftCoord;    
//    protected Envelope envelope;
    
}
