package com.geomaticaeambiente.openjump.klem.parallel;

import com.geomaticaeambiente.openjump.klem.grid.BasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.vividsolutions.jts.geom.Coordinate;

/**
 *
 * @author deluca
 */
public class Utils {
    
    
//    /**
//     * Checks whether a source cell flows into a destination cell.
//     * @param flowDirsGrid
//     * @param sourceCell The source cell.
//     * @param destCell The destination cell.
//     * @return 
//     */
//    public static boolean flowsInto(FlowDirBasicGrid flowDirsGrid, java.awt.Point sourceCell, java.awt.Point destCell) {
//        
//        if(flowDirsGrid.isOutlet(sourceCell)) {
//            return false;
//        }
//        int shiftIndex = Shifter.getShiftIndex(sourceCell, destCell);
//        return flowDirsGrid.flowsTowards(sourceCell, shiftIndex);
//        
//        
////        if(sourceCell.x + Shifter.getColShift(flowDirsGrid.getFlowDirValue(sourceCell)) == destCell.x &&
////                sourceCell.y + Shifter.getRowShift(flowDirsGrid.getFlowDirValue(sourceCell)) == destCell.y) {
////            return true;
////        } else {
////            return false;
////        }
//    }     
    
    
    public static java.awt.Point getColRow(Coordinate coord, BasicGrid grid){

        return getColRow(coord, grid.getRowCount(), grid.getLowerLeftCoord(), grid.getCellSize());
    
    }    
    
    public static java.awt.Point getColRow(Coordinate coord, int nRows,
            Coordinate llCorner, double cellSize){
    
        java.awt.Point point = new java.awt.Point();
        point.x = (int)Math.floor((coord.x - llCorner.x) / cellSize);
        point.y = nRows - (int)Math.floor((coord.y - llCorner.y) / cellSize) - 1;

        return point;        
    
    }
    
}
