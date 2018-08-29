package com.geomaticaeambiente.openjump.klem.parallel;

/**
 *
 * @author AdL
 */
public class Shifter {

    public static java.awt.Point getShiftedCell(java.awt.Point cell, int shiftPos) {
        return new java.awt.Point(cell.x + colShifts[shiftPos], cell.y + rowShifts[shiftPos]);
    }
    
    public static int getColShift(int position) {
        return colShifts[position];
    }
    
    public static int getRowShift(int position) {
        return rowShifts[position];
    }
    
    public static int getReverseShiftIndex(int index) {
        
        int cVal = colShifts[index];
        int rVal = rowShifts[index];
        
        for(int i=0; i<8; i++) {
            if(colShifts[i] == -cVal && rowShifts[i] == -rVal) {
                return i;
            }
        }
        return -1;
        
        
    }
    
    public static int getShiftIndex(java.awt.Point sourceCell, java.awt.Point destCell) {
        
        int colDiff = destCell.x - sourceCell.x;
        int rowDiff = destCell.y - sourceCell.y;
        
        for(int i=0; i<8; i++) {
            if(colShifts[i] == colDiff && rowShifts[i] == rowDiff) {
                return i;
            }
        }
        return -1;
        
    }
    
    /* Cazorzian */
    public static final int[] colShiftsCz = {  1,  0, -1, -1, -1,  0,  1, 1};
    public static final int[] rowShiftsCz = { -1, -1, -1,  0,  1,  1,  1, 0};
    
    /*
       2   1   0  
        \  |  /
      3 -     - 7 
        /  |  \
       4   5   6 
    */    
    
    /* Tarbotonian */
    public static final int[] colShifts = { 1, 1, 0,-1,-1,-1, 0, 1};
    public static final int[] rowShifts = { 0,-1,-1,-1, 0, 1, 1, 1};    
    
    /*
       3   2   1  
        \  |  /
      4 -     - 0 
        /  |  \
       5   6   7 
    */
    
}