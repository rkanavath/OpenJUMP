package com.geomaticaeambiente.openjump.klem.fill;

import com.geomaticaeambiente.openjump.klem.Log;
import com.geomaticaeambiente.openjump.klem.grid.BooleanBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.GridDestriper;
import com.geomaticaeambiente.openjump.klem.grid.GridStriper;
import com.geomaticaeambiente.openjump.klem.parallel.ExecutorBuilder;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

/**
 *
 * @author AdL
 */
public class DemFiller {
    
    public DemFiller(DoubleBasicGrid demGrid) {
        this.demGrid = demGrid;
    }
    
    public DoubleBasicGrid calculate() throws InterruptedException, ExecutionException, Exception {
        
        int rowCount = demGrid.getRowCount();
        ExecutorBuilder execBuilder = new ExecutorBuilder(rowCount);
        int stripeRowCount = execBuilder.getRowPerStripeCount();
        int stripeCount = execBuilder.getStripeCount();
        
        DoubleBasicGrid[] demGrids = GridStriper.stripeDoubleGrid(demGrid, stripeRowCount);
        DemFillerStripe[] demStripes = new DemFillerStripe[stripeCount];
        
        /* Create stripes */
        BooleanBasicGrid borderGrid = new BooleanBasicGrid(
                    new boolean[demGrid.getRowCount()][demGrid.getColumnCount()],
                    demGrid.getCellSize(), false, demGrid.getLowerLeftCoord());
        
        for(int s=0; s<stripeCount; s++) {

            int stripeEffectiveRows;
            if(s != stripeCount -1) {
                stripeEffectiveRows = stripeRowCount;
            } else {
                stripeEffectiveRows = rowCount - (s * stripeRowCount);                
            }
            
            double[] upRow = new double[demGrid.getColumnCount()];
            Arrays.fill(upRow, demGrid.getNoData());
            double[] bottomRow = new double[demGrid.getColumnCount()];
            Arrays.fill(bottomRow, demGrid.getNoData());
            
            if(s > 0) {
                upRow = Arrays.copyOf(
                        demGrid.getData()[s*stripeRowCount-1], demGrid.getColumnCount());
            }
           
            if(s < stripeCount - 1) {
                bottomRow = Arrays.copyOf(
                        demGrid.getData()[s*stripeRowCount+stripeEffectiveRows], demGrid.getColumnCount());
            }
                        
            DemFillerStripe demFillerStripe = new DemFillerStripe(
                    s, demGrids[s], s*stripeRowCount, upRow, bottomRow, demGrid, borderGrid);

            demStripes[s] = demFillerStripe;

        }   

        /* Process stripes */
        if(Log.log) System.out.println("Process stripes...");
        boolean terminate = false;

        while(!terminate) {
            
            List<Callable<DoubleBasicGrid>> demFillToDos_l = new ArrayList<Callable<DoubleBasicGrid>>();
            demFillToDos_l.addAll(Arrays.asList(demStripes));            
            
            List<Future<DoubleBasicGrid>> flowDirFutures_l = execBuilder.getExecutorService().invokeAll(demFillToDos_l);

            terminate = true;
            
            for (DemFillerStripe demStripe : demStripes) {
                if (!demStripe.isTerminated()) {
                    terminate = false;
                }
            }
            
            // Swap buffers
            for(int s=0; s<demStripes.length-1; s++) {

                double[] s2above = demStripes[s+1].getAboveBuffer();
                double[] s1below = demStripes[s].getBelowBuffer();                

                demStripes[s].plugNewBelowBuffer(s2above);
                demStripes[s+1].plugNewAboveBuffer(s1below);

            }
            
            if(terminate == true) {
                
                List<DoubleBasicGrid> demFillGrids_l = new ArrayList<DoubleBasicGrid>();
                for(Future<DoubleBasicGrid> demFillFuture : flowDirFutures_l) {
                    demFillGrids_l.add(demFillFuture.get());
                }
                DoubleBasicGrid filledDem =  GridDestriper.assembleDoubleStrips(demFillGrids_l);
                return filledDem;
                
//                // Remove flat areas
//                // TODO: parallelise flat area remover
//                FlatAreasProcessor2 flatAreasProcessor2 = new FlatAreasProcessor2();
//                return flatAreasProcessor2.process(filledDem);
                
                
            }
            
        }

        return null;
        
    }
        
    private final DoubleBasicGrid demGrid;
    
    
}
