package com.geomaticaeambiente.openjump.klem.routing;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.units.Length;
import com.geomaticaeambiente.openjump.klem.units.Speed;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;
import com.geomaticaeambiente.openjump.klem.parallel.UpwardAbstractStripe;
import java.awt.Point;

/**
 *
 * @author AdL
 */
public class RoutingTimeStripe extends UpwardAbstractStripe {

    public RoutingTimeStripe(
            int id, FlowDirBasicGrid flowDirGrid, DoubleBasicGrid upslopeAreaGrid,
            byte[][] dependencyStripe, double[][] outputData, int yOffset,
            RoutingTimeParameters routingParameters) {
        super(id, flowDirGrid, dependencyStripe, outputData, yOffset);
        
        this.routingParams = routingParameters;
        this.upslopeAreaData = upslopeAreaGrid.getData();
        orthoDistance = flowDirGrid.getCellSize();
        diagonalDistance = orthoDistance * Math.sqrt(2);        

        minThreshold = routingParameters.getMinThreshold().getArea(Length.LengthUnit.m);
        maxThreshold = routingParameters.getMaxThreshold().getArea(Length.LengthUnit.m);
        channelVel = routingParameters.getChannelVelocity().getSpeed(Speed.SpeedUnit.m_s);
        slopeVel = routingParameters.getSlopeVelocity().getSpeed(Speed.SpeedUnit.m_s);
        
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
                double upslopeAreaM2 = upslopeAreaData[cell.y + yOffset][cell.x] * Math.pow(orthoDistance, 2);
                outputData[cell.y][cell.x] = calcTime(upslopeAreaM2, orthoDistance) / 3600; // Output in h
            }

            updateCell(cell);
            
            // Update dependency maps
            updateDependencyMaps(cell, 1);

        }
        
    }    
    
    @Override
    public void updateCell(Point cell)  {
        
        java.awt.Point cellGrid = new java.awt.Point(cell.x, cell.y + yOffset);
        
        double upslopeAreaM2 = upslopeAreaData[cell.y + yOffset][cell.x] * Math.pow(orthoDistance, 2);
        if(flowDirsGrid.isOutlet(cellGrid)) {
            outputData[cellGrid.y][cellGrid.x] = calcTime(upslopeAreaM2, orthoDistance) / 3600; // Output in h
            return;
        }

        /* Find the sink cell */
        int colShift = Shifter.getColShift(flowDirsGrid.getTarbotonianFlowDirValue(cellGrid));
        int rowShift = Shifter.getRowShift(flowDirsGrid.getTarbotonianFlowDirValue(cellGrid));
        java.awt.Point sinkCell = new java.awt.Point(
                cellGrid.x + colShift,
                cellGrid.y + rowShift);
        
        double distance;
        if(colShift * rowShift == 0) {
            distance = orthoDistance;
        } else {
            distance = diagonalDistance;
        }        
        
        
        outputData[cellGrid.y][cellGrid.x] =
                    outputData[sinkCell.y][sinkCell.x] + calcTime(upslopeAreaM2, distance) / 3600;
        
    }
    
    private double calcTime(double upslopeArea, double distance) {
        
        if (upslopeArea > maxThreshold) {
            double timeSec =  
                    distance / (channelVel * Math.pow(upslopeArea  / maxThreshold, routingParams.getExponent()));
            return timeSec;
        } else if (upslopeArea > minThreshold){
            return distance / channelVel;
        } else {
            return distance / slopeVel;
        }
        
    }
    
    private final double[][] upslopeAreaData;
    private final double orthoDistance;
    private final double diagonalDistance;
    private final RoutingTimeParameters routingParams;
    private final double maxThreshold;
    private final double minThreshold;
    private final double channelVel;
    private final double slopeVel;
    
}
