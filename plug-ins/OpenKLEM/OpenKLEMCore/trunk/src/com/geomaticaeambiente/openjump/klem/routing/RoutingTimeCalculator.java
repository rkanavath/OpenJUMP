package com.geomaticaeambiente.openjump.klem.routing;

import com.geomaticaeambiente.openjump.klem.Log;
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsCalculator;
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsStripe;
import com.geomaticaeambiente.openjump.klem.grid.ByteBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.upslopearea.UpslopeAreaCalculator;
import com.geomaticaeambiente.openjump.klem.parallel.DependencyCalculator;
import com.geomaticaeambiente.openjump.klem.parallel.DependencyStripe;
import com.geomaticaeambiente.openjump.klem.parallel.ExecutorBuilder;
import com.vividsolutions.jts.geom.LineString;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;

/**
 * Calculated the hydrological distance using parallel computing.
 * @author AdL
 */
public class RoutingTimeCalculator {

    public DoubleBasicGrid calcD8(
            DoubleBasicGrid demGrid,
            LineString[] bluelines,
            Double bluelinesWeight,
            RoutingTimeParameters routingTimeParameters) throws Exception {

        if(demGrid == null) {
            throw new Exception ("The flow directions grid cannot be null.");
        }    
        
        FlowDirsCalculator flowDirsCalc = new FlowDirsCalculator(
                demGrid, FlowDirsStripe.FlowDirAlgorithm.D8, bluelines, bluelinesWeight);
        FlowDirBasicGrid flowDirsGrid = flowDirsCalc.calculate(); 
        
        /* Striping */
        int rowCount = flowDirsGrid.getRowCount();
        ExecutorBuilder execBuilder = new ExecutorBuilder(rowCount);
        int stripeRowCount = execBuilder.getRowPerStripeCount();
        int stripeCount = execBuilder.getStripeCount();
        
        /* Calculate upslopea area */
        if(Log.log) System.out.println("Calc upslope area...");
        UpslopeAreaCalculator upslopeAreaCalc = new UpslopeAreaCalculator(demGrid, bluelines, bluelinesWeight);
        DoubleBasicGrid upslopeAreaGrid = upslopeAreaCalc.calc(FlowDirsStripe.FlowDirAlgorithm.D8);
        
        /* Calculate dependency grid */
        if(Log.log) System.out.println("Calc dependency grid...");
        DependencyCalculator depGridCalculator = new DependencyCalculator(
                DependencyStripe.Origin.OUTLET, flowDirsGrid);
        ByteBasicGrid dependencyGrid = depGridCalculator.calculate();

        /* Calculate hydrological distance */
        if(Log.log) System.out.println("Calculate routing time...");
        double[][] routingTimeGrid = new double[flowDirsGrid.getRowCount()][flowDirsGrid.getColumnCount()];
        for(int r=0; r<routingTimeGrid.length; r++) {
            for(int c=0; c<routingTimeGrid[r].length; c++) {
                if(flowDirsGrid.isNoData(c, r)) {
                    routingTimeGrid[r][c] = -9999;
                }
            }
        }
        
        /* Striping */
        RoutingTimeStripe[] stripes = new RoutingTimeStripe[stripeCount];

        for(int s=0; s<stripes.length; s++) {
            
            int stripeEffectiveRows;
            if(s != stripes.length -1) {
                stripeEffectiveRows = stripeRowCount;
            } else {
                stripeEffectiveRows = rowCount - (s * stripeRowCount);                
            }
        
            byte[][] dependencyStripe = new byte[stripeEffectiveRows][flowDirsGrid.getColumnCount()];
        
            int offset = s * stripeRowCount;
            
            for(int r=0; r<stripeEffectiveRows; r++) {
                dependencyStripe[r] = Arrays.copyOf(dependencyGrid.getData()[r+offset], flowDirsGrid.getColumnCount());
            }
            
            stripes[s] = new RoutingTimeStripe(
                    s, flowDirsGrid,
                    upslopeAreaGrid,
                    dependencyStripe,
                    routingTimeGrid,
                    offset,
                    routingTimeParameters);
            
        }

        /* Process stripes */
        if(Log.log) System.out.println("Process stripes...");
        boolean terminate = false;        
        while(!terminate) {
            
            // This part can be parallelized
            List<Callable<Boolean>> todos_l = new ArrayList<Callable<Boolean>>();
            
            for(int s=0; s<stripes.length; s++) {
                if(Log.log) System.out.println("Processing stripe: " + (s+1));
                todos_l.add(stripes[s]);
            }
            
            List<Future<Boolean>> futures_l = execBuilder.getExecutorService().invokeAll(todos_l);
            
            /* Swap buffers */
            if(stripes.length == 1) {
                break;
            }
            
            terminate = true;
            for(int s=0; s<stripes.length-1; s++) {
                
                byte[] s2above = stripes[s+1].getAboveBuffer();
                byte[] s1below = stripes[s].getBelowBuffer();                
                
                stripes[s].plugNewBelowBuffer(s2above);
                stripes[s+1].plugNewAboveBuffer(s1below);
                
                if(stripes[s].getQsCount() != 0 || stripes[s+1].getQsCount() != 0) {
                    terminate = false;
                }              
                
            }
            
        }

        return new DoubleBasicGrid(routingTimeGrid, flowDirsGrid.getCellSize(),
                -9999, flowDirsGrid.getLowerLeftCoord());
        
    }
   
    
}
