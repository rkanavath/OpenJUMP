package com.geomaticaeambiente.openjump.klem.drainageTime;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;
import com.geomaticaeambiente.openjump.klem.parallel.UpwardAbstractStripe;
import java.awt.Point;

/**
 *
 * @author AdL
 */
public class DrainageTimeStripe extends UpwardAbstractStripe {

    public DrainageTimeStripe(int id, DoubleBasicGrid slopeGridRads, FlowDirBasicGrid flowDirData,
            byte[][] dependencyStripe, double[][] outputData, int yOffset,
            double maxDrainageTime, VelocityCalculator velCalc) {
        super(id, flowDirData, dependencyStripe, outputData, yOffset);
        
        this.slopeGridRads = slopeGridRads;
        this.maxDrainageTime = maxDrainageTime;
        this.orthoDistance = flowDirData.getCellSize();
        this.diagDistance = orthoDistance * Math.sqrt(2);
        this.velCalc = velCalc;
        
    }
    
    @Override
    protected void findQs () {
        
        for(int r=0; r<stripeNRows; r++) {
            for(int c=0; c<stripeNCols; c++) {                
                if(dependencyStripe[r][c] == 1 &&
                        outputData[r + yOffset][c] == 0) {
                    Qs_l.add(new java.awt.Point(c, r));
                }
            }
        }
        
    }
    
    @Override
    public void process() {
        
        while(!Qs_l.isEmpty()) {
            
            java.awt.Point cell = Qs_l.get(Qs_l.size()-1);            
            
            Qs_l.remove(Qs_l.size()-1);
            if(flowDirsGrid.isOutlet(cell)) {
                outputData[cell.y][cell.x] = -9;
            }

            updateCell(cell);
            
            // Update dependency maps
            updateDependencyMaps(cell, 1);

        }
        
    }    
    
    @Override
    public void updateCell(Point cell)  {
        
        java.awt.Point cellGrid = new java.awt.Point(cell.x, cell.y + yOffset);
        
        int noData = -9;
        
        if(flowDirsGrid.isOutlet(cellGrid)) {
            outputData[cellGrid.y][cellGrid.x] = orthoDistance;
            return;
        }

        /* Find the sink cell */
        int colShift = Shifter.getColShift(flowDirsGrid.getTarbotonianFlowDirValue(cellGrid));
        int rowShift = Shifter.getRowShift(flowDirsGrid.getTarbotonianFlowDirValue(cellGrid));
        
        java.awt.Point sinkCell = new java.awt.Point(
                cellGrid.x + colShift,
                cellGrid.y + rowShift);
        
        if(outputData[sinkCell.y][sinkCell.x] == noData) {
            outputData[cellGrid.y][cellGrid.x] = noData;
            return;
        }
        
        double distance = orthoDistance;
        if(sinkCell.x - cellGrid.x != 0 && sinkCell.y - cellGrid.y != 0) {
            distance = diagDistance;
        }
        
        double slope = slopeGridRads.getValue(cellGrid);
        double cellDrainageTime = distance / velCalc.calcVelocity(slope);
        
        double drainageTime = outputData[sinkCell.y][sinkCell.x] + cellDrainageTime;
        
        outputData[cellGrid.y][cellGrid.x] =
                drainageTime <= maxDrainageTime ?
                outputData[sinkCell.y][sinkCell.x] + cellDrainageTime : noData;   
        
    }
    
    private final DoubleBasicGrid slopeGridRads;
    private final double maxDrainageTime;
    private final double orthoDistance;
    private final double diagDistance;
    private final VelocityCalculator velCalc;
    
}
