package com.geomaticaeambiente.openjump.klem.upslopearea;

import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;
import java.util.concurrent.Callable;

/**
 * Algorithm from: Wallis C., Watson D., Tarboton D., Wallace R. 2009. Parallel
 * Flow-Direction and Contributing Area Calculation for Hydrology Analysis in
 * Digital Elevation Models. 2009 International Conference on Parallel and
 * Distributed Processing Techniques and Applications. Las Vegas, Nevada, USA,
 * July 13-16.
 * @author AdL
 */
public class UpslopeAreaStripeD8 extends UpslopeAreaStripe implements Callable<Boolean> {
    
    /**
     * Instantiates a stripe.
     * @param stripeId The stripe ID. Should be unique.
     * @param flowDirGrid The 
     * @param dependencyStripe
     * @param upslopeAreaGrid
     * @param yOffset 
     */
    public UpslopeAreaStripeD8(int stripeId, FlowDirBasicGrid flowDirGrid, byte[][] dependencyStripe,
            double[][] upslopeAreaGrid, int yOffset) {
        super(stripeId, flowDirGrid, dependencyStripe, upslopeAreaGrid, yOffset);        
        
    }

    
    @Override
    public Boolean call() throws Exception {
        process();
        return true;
    }
    
    /**
     * Updates the upslope area grid for the given cell.
     * @param cell The cell.
     */
    @Override
    protected void updateUa(java.awt.Point cell) {
        
        java.awt.Point cellGrid = new java.awt.Point(cell.x, cell.y + yOffset);
        
        /* Find cells contributing to cell */
        for(int i=0; i<8; i++) {
            
            int contribCellCol = cellGrid.x + Shifter.getColShift(i);
            int contribCellRow = cellGrid.y + Shifter.getRowShift(i);
            
            java.awt.Point contribCell = new java.awt.Point(
                    contribCellCol, contribCellRow);
            if(contribCell.x >= 0 && contribCell.x < gridNCols &&
                    contribCell.y >= 0 && contribCell.y < gridNRows &&
                    !flowDirsGrid.isNoData(contribCell) &&
                    flowDirsGrid.flowsInto(contribCell, cellGrid)) {
                
                upslopeAreaGrid[cellGrid.y][cellGrid.x] += upslopeAreaGrid[contribCell.y][contribCell.x];
                
            }
            
        }
        
    }
    
    /**
     * Updates the dependency maps. Given the source cell, the downslope cell
     * is updated. If the downslope cell falls inside the stripe, the value valueToAdd
     * is added. If the downslope cell falls outside the stripe (i.e.: on the buffers)
     * the value 1 is added.
     * @param sourceCell The source cell.
     * @param skipBuffers
     */
    @Override
    protected void updateDependencyMaps(java.awt.Point sourceCell, boolean skipBuffers) {

        if(flowDirsGrid.isNoData(sourceCell.x, sourceCell.y + yOffset) ||
                flowDirsGrid.isOutlet(sourceCell.x, sourceCell.y + yOffset)) {
            return;
        }
        
        java.awt.Point downslopeCell = new java.awt.Point(
                sourceCell.x + Shifter.getColShift(flowDirsGrid.getTarbotonianFlowDirValue(sourceCell.x, sourceCell.y + yOffset)),
                sourceCell.y + Shifter.getRowShift(flowDirsGrid.getTarbotonianFlowDirValue(sourceCell.x, sourceCell.y + yOffset)));
        
        // If downslope cell pertains to processor above
        if (downslopeCell.y < 0) {
            if(skipBuffers) return;
            dependencyBuff_Above[downslopeCell.x] ++;
            
        // If downslope cell pertains to processor below
        } else if(downslopeCell.y == stripeNRows) {
            if(skipBuffers) return;            
            dependencyBuff_Below[downslopeCell.x] ++;
            
        // If downslope cell pertains to this stripe
        } else if (downslopeCell.y >=0 && downslopeCell.y < stripeNRows) {
            
            dependencyStripe[downslopeCell.y][downslopeCell.x] -= 1;
            
            if(dependencyStripe[downslopeCell.y][downslopeCell.x] == 0 &&
                    upslopeAreaGrid[downslopeCell.y + yOffset][downslopeCell.x] == 0) {
                Qs_l.add(downslopeCell);
            }
        }
        
    }
    
}
