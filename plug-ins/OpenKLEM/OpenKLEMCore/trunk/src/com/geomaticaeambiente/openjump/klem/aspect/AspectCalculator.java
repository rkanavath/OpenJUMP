package com.geomaticaeambiente.openjump.klem.aspect;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.GridDestriper;
import com.geomaticaeambiente.openjump.klem.grid.GridStriper;
import com.geomaticaeambiente.openjump.klem.parallel.ExecutorBuilder;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.LineString;
import java.io.IOException;
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
public class AspectCalculator {
    
    /**
     * Calculates the aspect
     * @param demGrid
     * @param bluelines Optional
     * @param bluelinesWeight Optional
     */
    public AspectCalculator(DoubleBasicGrid demGrid, LineString[] bluelines,
            Double bluelinesWeight) {
        this.demGrid = demGrid;
        this.bluelinesLineStrings = bluelines;
        this.bluelinesWeight = bluelinesWeight;
    }
    
    /**
     * Calculates the aspect (in radians).
     * @return The aspect (radians).
     * @throws InterruptedException
     * @throws ExecutionException
     * @throws IOException 
     */
    public DoubleBasicGrid calculate() throws InterruptedException, ExecutionException, IOException {
        
        int rowCount = demGrid.getRowCount();
        ExecutorBuilder execBuilder = new ExecutorBuilder(rowCount);
        int stripeRowCount = execBuilder.getRowPerStripeCount();
        int stripeCount = execBuilder.getStripeCount();
        
        List<Callable<DoubleBasicGrid>> asepectToDos_l = new ArrayList<Callable<DoubleBasicGrid>>();
        
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
            
            AspectStripe aspectStripe = new AspectStripe(
                    demStripes[s], upRow, bottomRow, bluelines, bluelinesWeight);
            
            asepectToDos_l.add(aspectStripe);

        }   

        /* Calculate */
        List<Future<DoubleBasicGrid>> aspectFutures_l = execBuilder.getExecutorService().invokeAll(asepectToDos_l);
        List<DoubleBasicGrid> aspectGrids_l = new ArrayList<DoubleBasicGrid>();
        for(Future<DoubleBasicGrid> asepctFuture : aspectFutures_l) {
            aspectGrids_l.add(asepctFuture.get());
        }
        
        return GridDestriper.assembleDoubleStrips(aspectGrids_l);
        
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
    private final LineString[] bluelinesLineStrings;
    private final Double bluelinesWeight;
    
}
