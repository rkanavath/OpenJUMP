package com.geomaticaeambiente.openjump.klem.upslopearea;

import com.geomaticaeambiente.openjump.klem.Log;
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsCalculator;
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsStripe.FlowDirAlgorithm;
import com.geomaticaeambiente.openjump.klem.grid.ByteBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
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
 *
 * @author AdL
 */
public class UpslopeAreaCalculator {
    
    public UpslopeAreaCalculator(DoubleBasicGrid demGrid, LineString[] bluelines, Double bluelinesWeight) {
        this.demGrid = demGrid;
        this.bluelines = bluelines;
        this.bluelinesWeight = bluelinesWeight;
    }

    public DoubleBasicGrid calc(FlowDirAlgorithm algo) throws Exception {

        FlowDirsCalculator flowDirsCalc = new FlowDirsCalculator(demGrid, algo, bluelines, bluelinesWeight);
        FlowDirBasicGrid flowDirsGrid = flowDirsCalc.calculate();   
        
        /* Striping */
        int rowCount = flowDirsGrid.getRowCount();
        ExecutorBuilder execBuilder = new ExecutorBuilder(rowCount);
        int stripeRowCount = execBuilder.getRowPerStripeCount();
        int stripeCount = execBuilder.getStripeCount();        
        
        /* Calculate dependency grid */
        if(Log.log) System.out.println("Calc dependency grid...");
        DependencyCalculator depGridCalculator = new DependencyCalculator(
                DependencyStripe.Origin.WATERSHED, flowDirsGrid);
        ByteBasicGrid dependencyGrid = depGridCalculator.calculate();

        /* Calculate upslope area */
        if(Log.log) System.out.println("Calculate upslope area...");
        double[][] upslopeAreaGrid = new double[flowDirsGrid.getRowCount()][flowDirsGrid.getColumnCount()];
        for(int r=0; r<upslopeAreaGrid.length; r++) {
            for(int c=0; c<upslopeAreaGrid[r].length; c++) {
                if(flowDirsGrid.isNoData(c, r)) {
                    upslopeAreaGrid[r][c] = -9999;
                }
            }
        }
        
        UpslopeAreaStripe[] stripes = null;
        switch(algo) {
            
            case D8:
                
                stripes = new UpslopeAreaStripeD8[stripeCount];
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

                    stripes[s] = new UpslopeAreaStripeD8(s, flowDirsGrid, dependencyStripe, upslopeAreaGrid, offset);

                }
                
                break;
                
            case MultiFlow:
                
                stripes = new UpslopeAreaStripeMF[stripeCount];
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

                    stripes[s] = new UpslopeAreaStripeMF(s, flowDirsGrid, demGrid,
                            dependencyStripe, upslopeAreaGrid, offset);

                }
                
                break;
            
        }
        
        

        /* Process stripes */
        if(Log.log) System.out.println("Process stripes...");
        boolean terminate = false;        
        while(!terminate) {
            
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
        
        return new DoubleBasicGrid(upslopeAreaGrid, flowDirsGrid.getCellSize(),
                -9999, flowDirsGrid.getLowerLeftCoord());
        
        
    }
    
    private final DoubleBasicGrid demGrid;
    private final LineString[] bluelines;
    private final Double bluelinesWeight;
    
}
