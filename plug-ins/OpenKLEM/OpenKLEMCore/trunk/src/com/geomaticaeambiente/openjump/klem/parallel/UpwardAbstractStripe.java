package com.geomaticaeambiente.openjump.klem.parallel;

import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import java.util.concurrent.Callable;

public abstract class UpwardAbstractStripe extends DependencyAbstractStripe implements Callable<Boolean> {

    public UpwardAbstractStripe(int id, FlowDirBasicGrid flowDirData,
            byte[][] dependencyStripe, double[][] outputData, int yOffset) {
        super(id, flowDirData, dependencyStripe, outputData, yOffset);
        
    }
    
    @Override
    protected void updateDependencyMaps(java.awt.Point cell, int valueToAdd) {
        
        for(int i=0; i<8; i++) {
            
            java.awt.Point sourceCell = new java.awt.Point(
                    cell.x + Shifter.getColShift(i), cell.y + Shifter.getRowShift(i));
            
            if(sourceCell.x >= 0 && sourceCell.x < gridNCols &&
                    sourceCell.y + yOffset >= 0 && sourceCell.y + yOffset < gridNRows &&
                    !flowDirsGrid.isNoData(sourceCell.x, sourceCell.y + yOffset)) {
            
                if(flowDirsGrid.flowsInto(
                        new java.awt.Point(sourceCell.x, sourceCell.y + yOffset),
                        new java.awt.Point(cell.x, cell.y + yOffset))) {

                    // If downslope cell pertains to processor above
                    if (sourceCell.y < 0) {

                        dependencyBuff_Above[cell.x] = 1;

                    } else if(sourceCell.y == stripeNRows) {

                        dependencyBuff_Below[cell.x] = 1;

                    } else if (sourceCell.y >=0 && sourceCell.y < stripeNRows) {

                        if(valueToAdd == 0) {
                            return;
                        }

                        dependencyStripe[sourceCell.y][sourceCell.x] = 1;

                        if(dependencyStripe[sourceCell.y][sourceCell.x] == 1) {
                            
                            Qs_l.add(sourceCell);
                        }
                    }                
                }
            }
            
        }
        
    }
    
}
