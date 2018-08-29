package com.geomaticaeambiente.openjump.klem.flowdir;

import com.geomaticaeambiente.openjump.klem.fill.FlatAreasProcessor;
import com.geomaticaeambiente.openjump.klem.fill.FlatAreasProcessor.Results;
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsStripe.FlowDirAlgorithm;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.GridDestriper;
import com.geomaticaeambiente.openjump.klem.grid.GridStriper;
import com.geomaticaeambiente.openjump.klem.grid.IntBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.ExecutorBuilder;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.LineString;
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
public class FlowDirsCalculator {
    
    public FlowDirsCalculator(DoubleBasicGrid demGrid, FlowDirAlgorithm algo, LineString[] bluelines, Double bluelinesWeight) {
        this.demGrid = demGrid;
        this.algo = algo;
        this.bluelinesLineStrings = bluelines;
        this.bluelinesWeight = bluelinesWeight;
    }
    
    public FlowDirBasicGrid calculate() throws InterruptedException, ExecutionException, Exception {
        
        int rowCount = demGrid.getRowCount();
        int colCount = demGrid.getColumnCount();
        ExecutorBuilder execBuilder = new ExecutorBuilder(rowCount);
        int stripeRowCount = execBuilder.getRowPerStripeCount();
        int stripeCount = execBuilder.getStripeCount();
        
        List<Callable<FlowDirBasicGrid>> flowDirToDos_l = new ArrayList<Callable<FlowDirBasicGrid>>();
        
        DoubleBasicGrid[] demStripes = GridStriper.stripeDoubleGrid(demGrid, stripeRowCount);

        /* Rasterize bluelines */
        boolean[][] bluelines = null;
        if(bluelinesLineStrings != null && bluelinesLineStrings.length != 0) {
            Envelope bluelinesEnvelope = new Envelope();
            for(LineString lineString : bluelinesLineStrings) {
                bluelinesEnvelope.expandToInclude(lineString.getEnvelopeInternal());
            }
            
            bluelines = rasterize(bluelinesLineStrings,
                    demGrid.getColumnCount(), demGrid.getRowCount(),
                    demGrid.getCellSize(), demGrid.getLowerLeftCoord());
            
        }
        
        /* Stripe flow directions */
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
            
            FlowDirsStripe flowDirsStripe = new FlowDirsStripe(algo, demStripes[s],
                    upRow, bottomRow, bluelines, bluelinesWeight, null, null, null);
            
            flowDirToDos_l.add(flowDirsStripe);

        }   

        /* Calculate */
        List<Future<FlowDirBasicGrid>> flowDirFutures_l = execBuilder.getExecutorService().invokeAll(flowDirToDos_l);
        List<FlowDirBasicGrid> flowDirGrids_l = new ArrayList<FlowDirBasicGrid>();
        for(Future<FlowDirBasicGrid> flowDirFuture : flowDirFutures_l) {
            flowDirGrids_l.add(flowDirFuture.get());
        }
        
        FlowDirBasicGrid flowDirsGrid = GridDestriper.assembleFlowDirStrips(flowDirGrids_l);
        
        /* Process flat areas */
        FlatAreasProcessor flatAreaProc = new FlatAreasProcessor();
        Results flatAreasResults = flatAreaProc.process(demGrid, flowDirsGrid);

        DoubleBasicGrid flatsGradientGrid = flatAreasResults.getGradientGrid();
        IntBasicGrid flatLabelsGrid = flatAreasResults.getLabelsGrid();
        
        /* Calc flow dirs on flat areas */      
        double[] upRow = new double[demGrid.getColumnCount()];
        Arrays.fill(upRow, flatsGradientGrid.getNoData());
        int[] upFlatsRow = new int[demGrid.getColumnCount()];
        Arrays.fill(upFlatsRow,flatLabelsGrid.getNoData());
        double[] downRow = new double[demGrid.getColumnCount()];
        Arrays.fill(downRow, flatsGradientGrid.getNoData());
        int[] bottomFlatsRow = new int[demGrid.getColumnCount()];
        Arrays.fill(bottomFlatsRow,flatLabelsGrid.getNoData());
        FlowDirsStripe flatsStripe = new FlowDirsStripe(
                algo, flatsGradientGrid, upRow, downRow, bluelines, bluelinesWeight,
                flatLabelsGrid, upFlatsRow, bottomFlatsRow);
        FlowDirBasicGrid flatsFlowDirGrid = flatsStripe.call();
        
        /* Integrate flow directions*/
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<colCount; c++) {
                if(!demGrid.isNoData(demGrid.getValue(c, r)) && flowDirsGrid.isOutlet(c, r)) {
                    flowDirsGrid.setByteFlowDirValue(c, r, flatsFlowDirGrid.getByteFlowdirValue(c, r));
                }
            }
        }
        
//        GridAscii outGa = new GridAscii(
//                "D:\\Temp\\Moscardo\\flowdirs_MF_wFlats.asc",
//                flatsGradientGrid.getColumnCount(),
//                flatsGradientGrid.getRowCount(),
//                true, 
//                flatsGradientGrid.getLowerLeftCoord().x,
//                flatsGradientGrid.getLowerLeftCoord().y,
//                flatsGradientGrid.getCellSize(),
//                (float)flatsGradientGrid.getNoData());
//        
//        double[][] outData = new double[outGa.getnRows()][outGa.getnCols()];
//        for(int r=0; r<outData.length; r++) {
//            for(int c=0; c<outData[r].length; c++) {
//                outData[r][c] = flowDirsGrid.getByteFlowDirValue(new java.awt.Point(c, r));
//            }
//        }
//        
//        outGa.setRas(outData);
//        outGa.writeGrid();
        
        return flowDirsGrid;
        
    }
    
    public boolean[][] rasterize(LineString[] lineStrings,
            int colCount, int rowCount, double cellSize, Coordinate llCorner) {

        int n_step;

        boolean[][] bluelines = new boolean[rowCount][colCount];
        for(int r=0; r<rowCount; r++){
            for(int c=0; c<colCount; c++){
                bluelines[r][c] = false;
            }
        }

        for(LineString lineString : lineStrings) {
            
            if(lineString.getCoordinates().length > 1){
                Coordinate coord1 = lineString.getCoordinates()[0];
                for(int p=1; p<lineString.getCoordinates().length; p++){
                    Coordinate coord2 = lineString.getCoordinates()[p];

                    n_step = (int)Math.round(Math.max(
                            Math.abs(coord2.x-coord1.x) / (cellSize/2.0),
                            Math.max(Math.abs(coord2.y-coord1.y)/(cellSize/2.0), 1.0)));
                    for(int k=0; k<=n_step; k++){
                        double x = coord1.x + k * (coord2.x - coord1.x) / n_step;
                        double y = coord1.y + k * (coord2.y - coord1.y) / n_step;
                        int ic = (int)((x - llCorner.x) / cellSize + 0.9999 - 1);
                        int ir = rowCount - (int)((y - llCorner.y) / cellSize + 0.9999);
//                            System.out.println(ic + "," + ir);
                        if (ic > 0 && ic < colCount && ir > 0 && ir < rowCount) {
                            bluelines[ir][ic] = true;
                        }
                    }
                    coord1 = coord2;
                }
            }
        }
        return bluelines;
    }    
    
    private final DoubleBasicGrid demGrid;
    private final FlowDirAlgorithm algo;
    private final LineString[] bluelinesLineStrings;
    private final Double bluelinesWeight;
    
}
