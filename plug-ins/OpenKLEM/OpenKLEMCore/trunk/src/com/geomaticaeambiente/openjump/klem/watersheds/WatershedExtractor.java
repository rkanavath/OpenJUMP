package com.geomaticaeambiente.openjump.klem.watersheds;

import com.geomaticaeambiente.openjump.klem.Log;
import com.geomaticaeambiente.openjump.klem.grid.ByteBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.DependencyCalculator;
import com.geomaticaeambiente.openjump.klem.parallel.DependencyStripe;
import com.geomaticaeambiente.openjump.klem.parallel.GridClipper;
import com.geomaticaeambiente.openjump.klem.parallel.Utils;
import com.geomaticaeambiente.openjump.klem.parallel.ExecutorBuilder;
import com.vividsolutions.jts.geom.Coordinate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;

/**
 * 
 * @author AdL
 */
public class WatershedExtractor {

    public DoubleBasicGrid extract(FlowDirBasicGrid flowDirsGrid,
            Coordinate[] outlets, boolean clipOutput) throws Exception {

        if(flowDirsGrid == null) {
            throw new Exception ("The flow directions grid cannot be null.");
        }    
        
        /* Striping */
        int rowCount = flowDirsGrid.getRowCount();
        ExecutorBuilder execBuilder = new ExecutorBuilder(rowCount);
        int stripeRowCount = execBuilder.getRowPerStripeCount();
        int stripeCount = execBuilder.getStripeCount();   
        
        /* Plug custom outlets */
        if(outlets != null) {
            for(int r=0; r<flowDirsGrid.getRowCount(); r++) {
                for(int c=0; c<flowDirsGrid.getColumnCount(); c++) {
                    if(flowDirsGrid.isOutlet(c, r)) {
                        flowDirsGrid.setNoData(c, r, true);
                    }
                }
            }
            
            for(Coordinate outlet : outlets) {
                java.awt.Point outletColRow = Utils.getColRow(outlet, flowDirsGrid);
                flowDirsGrid.setOutlet(outletColRow);
            }
        }        
        
        /* Calculate dependency grid */
        if(Log.log) System.out.println("Calc dependency grid...");
        DependencyCalculator depGridCalculator = new DependencyCalculator(
                DependencyStripe.Origin.OUTLET, flowDirsGrid);
        ByteBasicGrid dependencyGrid = depGridCalculator.calculate();

        /* Extract watershed */
        if(Log.log) System.out.println("Extract watershed...");
        int watershedValue = 0;
        double[][] watershedData = new double[flowDirsGrid.getRowCount()][flowDirsGrid.getColumnCount()];
        for(int r=0; r<watershedData.length; r++) {
            for(int c=0; c<watershedData[r].length; c++) {
                if(flowDirsGrid.isNoData(c, r)) {
                    watershedData[r][c] = -9999;    
                } else {
                    if(!flowDirsGrid.isNoData(c, r) && flowDirsGrid.isOutlet(c, r)) {
                        watershedData[r][c] = ++watershedValue;
                    }
                }
            }
        }
        
        /* Striping */
        WatershedStripe[] stripes = new WatershedStripe[stripeCount];

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
            
            stripes[s] = new WatershedStripe(
                    s, flowDirsGrid, dependencyStripe,
                    watershedData, offset);
            
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

        DoubleBasicGrid watershedGrid =  new DoubleBasicGrid(watershedData, flowDirsGrid.getCellSize(),
                -9999, flowDirsGrid.getLowerLeftCoord());
        
        if(outlets != null && clipOutput) {
            // Clip output            
            if(Log.log) System.out.println("Clipping output...");
            return GridClipper.extractValues(watershedGrid, new double[]{1});
        } else {
            // Replace 0s with nodatas
            for(int r=0; r<watershedGrid.getRowCount(); r++) {
                for(int c=0; c<watershedGrid.getColumnCount(); c++) {
                    if(watershedGrid.getValue(c, r) == 0) {
                        watershedGrid.setValue(c, r, watershedGrid.getNoData());
                    }
                }
            }
            return watershedGrid;
        }

    }
   
    
}
