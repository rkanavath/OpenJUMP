package com.geomaticaeambiente.openjump.klem.parallel;

import com.geomaticaeambiente.openjump.klem.grid.ByteBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import java.util.concurrent.Callable;

/**
 *
 * @author AdL
 */
public class DependencyStripe implements Callable<ByteBasicGrid>{

    public DependencyStripe(int id, Origin position,
            FlowDirBasicGrid flowDirsGrid, FlowDirBasicGrid upRow, FlowDirBasicGrid bottomRow) {
        this.id = id;
        this.position = position;
        this.flowDirsGrid = flowDirsGrid;
        this.upRow = upRow;
        this.bottomRow = bottomRow;
    }
    
    @Override
    public ByteBasicGrid call() throws Exception {
        return calcDependencyGrid();
    }
    
    private ByteBasicGrid calcDependencyGrid() {
        
        int nRows = flowDirsGrid.getRowCount();
        int nCols = flowDirsGrid.getColumnCount();
        
        byte[][] dependencyGrid = new byte[nRows][nCols];
        
        for(int r=0; r<nRows; r++) {
            for(int c=0; c<nCols; c++) {
                
                if(!flowDirsGrid.isNoData(c, r)) {
                    
                    byte contributorsCount = 0;
                    for(int i=0; i<8; i++) {
                        
                        int c1 = c + Shifter.getColShift(i);
                        int r1 = r + Shifter.getRowShift(i);
                        
                        if(position == Origin.OUTLET) {
                            if(flowDirsGrid.isOutlet(c, r)) {
                                contributorsCount = 1;
                                break;
                            }
                        } else if(position == Origin.WATERSHED) {
                        
                            /* Check domain */
                            if(c1>=0 && c1<nCols && r1>=0 && r1<nRows) {
                                if(!flowDirsGrid.isNoData(c1, r1) &&
                                        !flowDirsGrid.isOutlet(c1, r1)) {

                                    /* Upslope cell contributes only if its upslope
                                     area has not been calculated yet */
                                    java.awt.Point sourceCell = new java.awt.Point(c1, r1);
                                    
                                    if(flowDirsGrid.flowsTowards(flowDirsGrid.getByteFlowDirValue(sourceCell),
                                            Shifter.getReverseShiftIndex(i))) {
                                        contributorsCount++;
                                        
                                    }
                                }
                            } else if(c1>=0 && c1<nCols && r1<0) {
                                if(!upRow.isNoData(c1, 0)) {
                                    if(upRow.flowsTowards(upRow.getByteFlowdirValue(c1, 0), Shifter.getReverseShiftIndex(i))) {
                                        contributorsCount++;
                                    }
                                }
                            } else if(c1>=0 && c1<nCols && r1==nRows) {
                                if(!bottomRow.isNoData(c1, 0)) {
                                    if(bottomRow.flowsTowards(bottomRow.getByteFlowdirValue(c1, 0), Shifter.getReverseShiftIndex(i))) {
                                        contributorsCount++;
                                    }
                                }
                            }                   
                        }
                    }
                    
                    dependencyGrid[r][c] = contributorsCount;

                }
            }
        }
        
        return new ByteBasicGrid(dependencyGrid, flowDirsGrid.getCellSize(),
                (byte) -9, flowDirsGrid.getLowerLeftCoord());
        
    }
    
    private final int id;
    private final Origin position;
    private final FlowDirBasicGrid flowDirsGrid;
    private final FlowDirBasicGrid upRow;
    private final FlowDirBasicGrid bottomRow;
    
    public enum Origin {
        
        WATERSHED, OUTLET;
        
    }
    
}
