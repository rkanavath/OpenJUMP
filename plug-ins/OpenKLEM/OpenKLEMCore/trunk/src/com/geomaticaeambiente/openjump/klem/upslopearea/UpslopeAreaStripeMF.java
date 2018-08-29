package com.geomaticaeambiente.openjump.klem.upslopearea;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.IntBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;
import com.geomaticaeambiente.openjump.klem.parallel.Utils;
import java.util.concurrent.Callable;

/**
 * Algorithm from: Wallis C., Watson D., Tarboton D., Wallace R. 2009. Parallel
 * Flow-Direction and Contributing Area Calculation for Hydrology Analysis in
 * Digital Elevation Models. 2009 International Conference on Parallel and
 * Distributed Processing Techniques and Applications. Las Vegas, Nevada, USA,
 * July 13-16.
 * @author AdL
 */
public class UpslopeAreaStripeMF extends UpslopeAreaStripe implements Callable<Boolean> {
    
    /**
     * Instantiates a stripe.
     * @param stripeId The stripe ID. Should be unique.
     * @param flowDirGrid The 
     * @param demGrid 
     * @param dispersionGrid 
     * @param dependencyStripe
     * @param upslopeAreaGrid
     * @param yOffset 
     */
    public UpslopeAreaStripeMF(
            int stripeId, FlowDirBasicGrid flowDirGrid, DoubleBasicGrid demGrid,
            byte[][] dependencyStripe,
            double[][] upslopeAreaGrid, int yOffset) {
        super(stripeId, flowDirGrid, dependencyStripe, upslopeAreaGrid, yOffset);

        this.demGrid = demGrid;
        
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
        
        for(int i=0; i<8; i++) {
            
            int contribCellCol = cellGrid.x + Shifter.getColShift(i);
            int contribCellRow = cellGrid.y + Shifter.getRowShift(i);
            java.awt.Point contribCell = new java.awt.Point(contribCellCol, contribCellRow);
            
            if(contribCell.x >= 0 && contribCell.x < gridNCols &&
                    contribCell.y >= 0 && contribCell.y < gridNRows &&
                    !flowDirsGrid.isNoData(contribCell) &&
                    flowDirsGrid.flowsInto(contribCell, cellGrid)) {

                /* Calc % of contribution */
                upslopeAreaGrid[cellGrid.y][cellGrid.x] += upslopeAreaGrid[contribCell.y][contribCell.x] *
                        calcDirWeight(cellGrid, contribCell, i) / calcRasAngle(contribCell);
                
            }
        }

        
    }
    
    private double calcDirWeight(java.awt.Point destCell, java.awt.Point contribCell, int i) {
        
        double heightDiff = demGrid.getValue(contribCell) - demGrid.getValue(destCell);
        if(heightDiff == 0) {
            heightDiff = 1;
        }
        
        double slope = heightDiff / (weightS[i] * demGrid.getCellSize());
        double dirWeight = slope * (weightL[i] * demGrid.getCellSize());
        return dirWeight;
        
    }
    
    private double calcRasAngle(java.awt.Point cell) {
        
        double denm = 0;
        for(int i=0; i<8; i++) {
            
            java.awt.Point destCell = Shifter.getShiftedCell(cell, i);
            if(destCell.x < 0 || destCell.x >= demGrid.getColumnCount() ||
                    destCell.y < 0 || destCell.y >= demGrid.getRowCount()) {
                continue;
            }
      
            if(!demGrid.isNoData(demGrid.getValue(destCell.x, destCell.y))) {
                //if(demGrid.getValue(destCell.x, destCell.y) < demGrid.getValue(cell.x, cell.y)) {
                if(flowDirsGrid.flowsInto(cell, destCell)) {
                    denm += calcDirWeight(destCell, cell, i);
                }
            }
            
        }
        
        if(denm > 0) {
            return denm;
        } else {
            return -1;
        }
        
    }    
    
    /**
     * Updates the dependency maps. Given the source cell, the downslope cell
     * is updated. If the downslope cell falls inside the stripe, the value valueToAdd
     * is added. If the downslope cell falls outside the stripe (i.e.: on the buffers)
     * the value 1 is added.
     * @param sourceCell The source cell.
     * @param valueToAdd The value to add when the downslope cell falls inside
     * the stripe.
     * @param skipBuffers
     */
    @Override
    protected void updateDependencyMaps(java.awt.Point sourceCell, boolean skipBuffers) {

        if(flowDirsGrid.isNoData(sourceCell.x, sourceCell.y + yOffset)
                || flowDirsGrid.isOutlet(sourceCell.x, sourceCell.y + yOffset)) {
            return;
        }
        
        for(int i=0; i<8; i++) {
            if(flowDirsGrid.flowsTowards(sourceCell.x, sourceCell.y + yOffset, i)) {
                
                java.awt.Point downslopeCell = new java.awt.Point(
                        sourceCell.x + Shifter.getColShift(i),
                        sourceCell.y + Shifter.getRowShift(i));
 
                        // If downslope cell pertains to processor above
                        if (downslopeCell.y < 0) {
                            if(skipBuffers) continue;
                            dependencyBuff_Above[ sourceCell.x + Shifter.getColShift(i)] ++;

                        // If downslope cell pertains to processor below
                        } else if(downslopeCell.y == stripeNRows) {
                            if(skipBuffers) continue;
                            dependencyBuff_Below[ sourceCell.x + Shifter.getColShift(i)] ++;

                        // If downslope cell pertains to this stripe
                        } else if (downslopeCell.y >=0 && downslopeCell.y < stripeNRows) {
                            dependencyStripe[downslopeCell.y][downslopeCell.x] -= 1;

                            if(dependencyStripe[downslopeCell.y][downslopeCell.x] <= 0 &&
                                    upslopeAreaGrid[downslopeCell.y + yOffset][downslopeCell.x] == 0) {
                                Qs_l.add(downslopeCell);
                            }
                        }
                
                
            }
        }
        
    }
    
    private final DoubleBasicGrid demGrid;
    
}
