package com.geomaticaeambiente.openjump.klem.flowdir;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.IntBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;
import java.io.IOException;
import java.util.Arrays;
import java.util.concurrent.Callable;

/**
 *
 * @author AdL
 */
public class FlowDirsStripe implements Callable<FlowDirBasicGrid> {

    public FlowDirsStripe(FlowDirAlgorithm algo, DoubleBasicGrid demStripe, double[] upRow,
            double[] bottomRow, boolean[][] bluelinesGrid, Double bluelinesWeight,
            IntBasicGrid flatMaskGrid, int[] flatMaskUpRow, int[] flatMaskBottomRow) {

        this.algo = algo;
        this.demStripe = demStripe;
        this.upRow = upRow;
        this.bottomRow = bottomRow;
        this.bluelinesGrid = bluelinesGrid;
        this.bluelinesWeight = bluelinesWeight;
        
        this.flatMaskGrid = flatMaskGrid;
        this.flatMaskUpRow = flatMaskUpRow;
        this.flatMaskBottomRow = flatMaskBottomRow;
        
        for(int i=0; i<8; i++) {
            distanceWeight[i] = (float) (
                    Math.sqrt(Math.abs((double)(Shifter.getColShift(i)))
                    + Math.abs((double)(Shifter.getRowShift(i)))));
        }   
        
    }
    
    @Override
    public FlowDirBasicGrid call() throws Exception {
        
        return calcFlowDir();
        
    }
    
    private FlowDirBasicGrid calcFlowDir() throws IOException {
        
        int nRows = demStripe.getRowCount();
        int nCols = demStripe.getColumnCount();

        FlowDirBasicGrid flowDirGrid = new FlowDirBasicGrid(nCols, nRows,
                demStripe.getCellSize(), demStripe.getLowerLeftCoord());
        
        for(int r=0; r<nRows; r++) {
            for(int c=0; c<nCols; c++) {
                
                flowDirGrid.setNoData(c, r, true);
                
                if(!demStripe.isNoData(demStripe.getData()[r][c])) {
                    
                    flowDirGrid.setNoData(c, r, false);
                                 
                    double[] elev = new double[8];
                    Arrays.fill(elev, demStripe.getNoData());
                    for(int i=0; i<8; i++) {
                        
                        int c1 = c + Shifter.getColShift(i);
                        int r1 = r + Shifter.getRowShift(i);                         
                        if(c1>=0 && c1<nCols && r1>=0 && r1<nRows) {
                            if(!demStripe.isNoData(demStripe.getData()[r1][c1]) &&
                                    (flatMaskGrid == null || flatMaskGrid.getValue(c1, r1) == flatMaskGrid.getValue(c, r))) {
                                elev[i] = demStripe.getData()[r1][c1];
                            }
                        } else if(c1>=0 && c1<nCols && r1<0) {
                            if(!demStripe.isNoData(upRow[c1]) &&
                                    (flatMaskUpRow == null || flatMaskUpRow[c] == flatMaskGrid.getValue(c, r))) {
                                elev[i] = upRow[c1];
                            }
                        } else if(c1>=0 && c1<nCols && r1==nRows) {
                            if(!demStripe.isNoData(bottomRow[c1]) &&
                                    (flatMaskBottomRow == null || flatMaskBottomRow[c] == flatMaskGrid.getValue(c, r))) {
                                elev[i] = bottomRow[c1];
                            }
                        }
                        
                        if(bluelinesGrid != null) {
                            if(c1>0 && c1<nCols && r1>0 && r1<nRows && bluelinesGrid[r1][c1]) {
                                if(elev[i] < demStripe.getData()[r][c] && !demStripe.isNoData(elev[i])) {
                                    elev[i] -= bluelinesWeight;
                                }
                            }
                        }
                        
                    }
                    
                    // Return a byte
                    switch(algo) {
                        case D8:
                            flowDirGrid.setByteFlowDirValue(c, r, calcD8(c, r, nCols, nRows, elev));
                            break;
                        case MultiFlow:
                            flowDirGrid.setByteFlowDirValue(c, r, calcMultiFlow(c, r, nCols, nRows, elev));
                            break;
                    }
                    
                }
            }
        }
        
        return flowDirGrid;
        
    }
    
    private byte calcD8(int c, int r, int nCols, int nRows, double[] elev) {
        
        // Find max slope
        double maxDelta = -Double.MAX_VALUE;
        int maxPos = -1;
        
        byte flowDirsByte = 0;
        for(int i=0; i<8; i++) {
            int c1 = c + Shifter.getColShift(i);
            int r1 = r + Shifter.getRowShift(i);   

            /* Check domain */
            Double delta = null;
            if(c1>=0 && c1<nCols && r1>=0 && r1<nRows) {
                if(!demStripe.isNoData(elev[i])) {
                    delta = (demStripe.getData()[r][c] - elev[i]) / distanceWeight[i];
                }
            } else if(c1>=0 && c1<nCols && r1<0) {
                if(!demStripe.isNoData(elev[i])) {
                    delta = (demStripe.getData()[r][c] - elev[i]) / distanceWeight[i];
                }
            } else if(c1>=0 && c1<nCols && r1==nRows) {
                if(!demStripe.isNoData(elev[i])) {
                    delta = (demStripe.getData()[r][c] - elev[i]) / distanceWeight[i];
                }
            }
            if(delta != null && delta > maxDelta && delta > 0) {
                maxDelta = delta;
                maxPos = i;
            }
        }

        if(maxDelta > 0) {         
            flowDirsByte = BitOps.setBitValue(flowDirsByte, maxPos, true);
        } 
        return flowDirsByte;
        
    }
    
    private byte calcMultiFlow(int c, int r, int nCols, int nRows, double[] elev) {
        
        byte flowDirsByte = 0;
        for(int i=0; i<8; i++) {
            int c1 = c + Shifter.getColShift(i);
            int r1 = r + Shifter.getRowShift(i);
            
            /* Check domain */
            Double delta = null;
            if(c1>=0 && c1<nCols && r1>=0 && r1<nRows) {
                if(!demStripe.isNoData(elev[i])) {
                    delta = (demStripe.getData()[r][c] - elev[i]);
                }
            } else if(c1>=0 && c1<nCols && r1<0) {
                if(!demStripe.isNoData(upRow[c1])) {
                    delta = (demStripe.getData()[r][c] - elev[i]);
                }
            } else if(c1>=0 && c1<nCols && r1==nRows) {
                if(!demStripe.isNoData(bottomRow[c1])) {
                    delta = (demStripe.getData()[r][c] - elev[i]);
                }
            }
            
            if(delta != null && delta > 0) {
                flowDirsByte = BitOps.setBitValue(flowDirsByte, i, true);
            }
            
        }

        return flowDirsByte;
        
    }
    
    public enum FlowDirAlgorithm {
        D8, MultiFlow
    }
    
    private final FlowDirAlgorithm algo;
    private final DoubleBasicGrid demStripe;
    private final double[] upRow;
    private final double[] bottomRow;
    private final IntBasicGrid flatMaskGrid;
    private final int[] flatMaskUpRow;
    private final int[] flatMaskBottomRow;  
    private final float[] distanceWeight = new float[8];
    private final boolean[][] bluelinesGrid;
    private final Double bluelinesWeight;
    
}
