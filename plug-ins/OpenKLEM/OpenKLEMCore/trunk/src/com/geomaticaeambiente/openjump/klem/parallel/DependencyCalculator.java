package com.geomaticaeambiente.openjump.klem.parallel;

import com.geomaticaeambiente.openjump.klem.grid.ByteBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.GridDestriper;
import com.geomaticaeambiente.openjump.klem.grid.GridStriper;
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
public class DependencyCalculator {
    
    public DependencyCalculator(DependencyStripe.Origin position, FlowDirBasicGrid flowDirGrid) {
        this.position = position;
        this.flowDirsGrid = flowDirGrid;
    }
    
    public ByteBasicGrid calculate() throws InterruptedException, ExecutionException {
        
        int rowCount = flowDirsGrid.getRowCount();
        ExecutorBuilder execBuilder = new ExecutorBuilder(rowCount);
        int stripeRowCount = execBuilder.getRowPerStripeCount();
        int stripeCount = execBuilder.getStripeCount();
        
        List<Callable<ByteBasicGrid>> depGridToDo_l = new ArrayList<Callable<ByteBasicGrid>>();
       
        /* Stripe flow directions */
        FlowDirBasicGrid[] flowDirStripes = GridStriper.stripeFlowDirBasicGrid(flowDirsGrid, stripeRowCount);

        /* Create the dependency stripes */
        for(int s=0; s<stripeCount; s++) {

            int stripeEffectiveRows;
            if(s != stripeCount -1) {
                stripeEffectiveRows = stripeRowCount;
            } else {
                stripeEffectiveRows = rowCount - (s * stripeRowCount);                
            }
            
            byte[][] upRow = new byte[1][stripeEffectiveRows];
            boolean[][] upRowNoData = new boolean[1][flowDirsGrid.getColumnCount()];
            Arrays.fill(upRowNoData[0], true);
            byte[][] bottomRow = new byte[1][stripeEffectiveRows];
            boolean[][] bottomRowNoData = new boolean[1][flowDirsGrid.getColumnCount()];
            Arrays.fill(bottomRowNoData[0], true);
            
            if(s > 0) {
                upRow[0] = Arrays.copyOf(
                        flowDirsGrid.getByteFlowDirData()[s*stripeRowCount-1], flowDirsGrid.getColumnCount());
                upRowNoData[0] = Arrays.copyOf(
                        flowDirsGrid.getNoDataData()[s*stripeRowCount-1], flowDirsGrid.getColumnCount());
            }
           
            if(s < stripeCount - 1) {
                bottomRow[0] = Arrays.copyOf(
                        flowDirsGrid.getByteFlowDirData()[s*stripeRowCount+stripeEffectiveRows], flowDirsGrid.getColumnCount());
                bottomRowNoData[0] = Arrays.copyOf(
                        flowDirsGrid.getNoDataData()[s*stripeRowCount+stripeEffectiveRows], flowDirsGrid.getColumnCount());
            }
            
            FlowDirBasicGrid upRowFlowDirGrid = new FlowDirBasicGrid(upRow, upRowNoData, flowDirsGrid.getCellSize(), null);
            FlowDirBasicGrid downRowFlowDirGrid = new FlowDirBasicGrid(bottomRow, bottomRowNoData, flowDirsGrid.getCellSize(), null);
            
            DependencyStripe dependencyStripe = new DependencyStripe(
                    s, position, flowDirStripes[s], upRowFlowDirGrid, downRowFlowDirGrid);
            
            depGridToDo_l.add(dependencyStripe);

        }   

        /* Calculate */
        List<Future<ByteBasicGrid>> depGridFutures_l = execBuilder.getExecutorService().invokeAll(depGridToDo_l);
        
        List<ByteBasicGrid> depGrids_l = new ArrayList<ByteBasicGrid>();
        for(Future<ByteBasicGrid> flowDirFuture : depGridFutures_l) {
            depGrids_l.add(flowDirFuture.get());
        }
        
        return GridDestriper.assembleByteStrips(depGrids_l);
        
    }
    
    private final DependencyStripe.Origin position;
    private final FlowDirBasicGrid flowDirsGrid;
    
}
