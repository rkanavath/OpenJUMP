package com.geomaticaeambiente.openjump.klem.fill;

import com.geomaticaeambiente.openjump.klem.Log;
import com.geomaticaeambiente.openjump.klem.grid.BooleanBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.AbstractStripe;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;

/**
 *
 * @author AdL
 */
public class DemFillerStripe extends AbstractStripe implements Callable<DoubleBasicGrid> {
    
    public DemFillerStripe (
            int stripeId, DoubleBasicGrid demStripe, int yOffset,
            double[] aboveBuffer, double[] belowBuffer,
            DoubleBasicGrid wholeDem,
            BooleanBasicGrid wholeBorder) {
        super(stripeId, demStripe, yOffset, aboveBuffer, belowBuffer);
        
        this.wholeDem = wholeDem;
        this.wholeBorder = wholeBorder;
        
        this.outputGrid = demStripe;
        this.aboveBuffer = aboveBuffer;
        this.belowBuffer = belowBuffer;
        stripeNCols = demStripe.getColumnCount();
        stripeNRows = demStripe.getRowCount();

        aboveOutBuffer = new double[stripeNCols];
        belowOutBuffer = new double[stripeNCols];
        
        gridNRows = wholeDem.getRowCount();    
        
    }
       
    @Override
    protected void process() {   
        
        /* Initialization is done only once */
        if(initialize) {
            /* Initialize output grids */
            outputGrid = new DoubleBasicGrid(new double[stripeNRows][stripeNCols],
                    outputGrid.getCellSize(), outputGrid.getNoData(), outputGrid.getLowerLeftCoord());
        
            for(int r=0; r<stripeNRows; r++) {
                Arrays.fill(outputGrid.getData()[r], outputGrid.getNoData());
            }
            findBorder();
            findPits();
            
            initialize = false;
        }
        
        
        terminated = false;
        if(!theSwitch) {
            if(Log.log) System.out.println(stripeId + " Qs: " + Qs_l.size());
            process(Qs_l, Qs1_l);
        } else {
            if(Log.log) System.out.println(stripeId + " Qs1: " + Qs1_l.size());
            process(Qs1_l, Qs_l);
        }
        theSwitch = !theSwitch;
 
    }
    
    private void findBorder() {
        
        // Needs to calcualte border for the buffer too
        for (int r=0; r<stripeNRows; r++) {
            for (int c=0; c<stripeNCols; c++) {
                
                if(r+yOffset < 0 || r+yOffset >= gridNRows) {
                    continue;
                }

                boolean border = false;
                double dValue;
                
                if(!wholeDem.isNoData(wholeDem.getData()[r + yOffset][c])) {
                    for (int i = 0; i < 8; i++) {
                        
                        int ic = c + Shifter.getColShift(i);
                        int ir = r + Shifter.getRowShift(i);
                    
                        if(ic < 0 || ic >= stripeNCols ||
                                ir+yOffset < 0 || ir+yOffset >= gridNRows) {
                            border = true;
                            break;
                        }
                        
                        dValue = wholeDem.getData()[ir+yOffset][ic];
                        
                        if (wholeDem.isNoData(dValue)) {
                            border = true;
                            break;
                        }
                        
                    }
                    
                    wholeBorder.getData()[r+yOffset][c] = border;
                    
                    if(r == 0) {
                        outputGrid.getData()[r][c] = border ? wholeDem.getData()[r+yOffset][c] : Double.MAX_VALUE;
                        aboveOutBuffer[c] = border ? aboveBuffer[c] : Double.MAX_VALUE;
                    } else if(r == stripeNRows-1) {
                        outputGrid.getData()[r][c] = border ? wholeDem.getData()[r+yOffset][c] : Double.MAX_VALUE;
                        belowOutBuffer[c] = border ? belowBuffer[c] : Double.MAX_VALUE;
                    } else {
                        outputGrid.getData()[r][c] = border ? wholeDem.getData()[r+yOffset][c] : Double.MAX_VALUE;
                    }
                    
                }
            }
        }
        
    }
    
    private void findPits() {

        for (int r=0; r<stripeNRows; r++) {
            for (int c=0; c<stripeNCols; c++) {
                
                if(r+yOffset < 0 || r+yOffset >= gridNRows) {
                    continue;
                }
                
                if(!wholeBorder.getData()[r+yOffset][c]) {
                    
                    if(r < 0) {
                        if(aboveOutBuffer[c] <= wholeDem.getData()[r+yOffset][c]) {
                            continue;
                        }
                    } else if (r >= stripeNRows) {
                        if(belowOutBuffer[c] <= wholeDem.getData()[r+yOffset][c]) {
                            continue;
                        }
                    } else {
                        if(outputGrid.getData()[r][c] <= wholeDem.getData()[r+yOffset][c]) {
                            continue;
                        }
                    }
                    
                    if(!wholeDem.isNoData(wholeDem.getData()[r+yOffset][c])) {
                        
                        double neighbourDouble = Double.MAX_VALUE;
                        
                        for(int i=0; i<8; i++) {
                            int ic = c + Shifter.getColShift(i);
                            int ir = r + Shifter.getRowShift(i);
                           
                            if(ir < 0) {
                                
                                if(!wholeDem.isNoData(aboveOutBuffer[ic]) &&
                                        aboveOutBuffer[ic] < neighbourDouble) {

                                    neighbourDouble = aboveOutBuffer[ic];
                                }
                                
                            } else if(ir >= stripeNRows) {
                                
                                if(!wholeDem.isNoData(belowOutBuffer[ic]) &&
                                        belowOutBuffer[ic] < neighbourDouble) {

                                    neighbourDouble = belowOutBuffer[ic]; 
                                }
                                
                            } else {
                            
                                if(!wholeDem.isNoData(outputGrid.getData()[ir][ic]) &&
                                        outputGrid.getData()[ir][ic] < neighbourDouble) {

                                    neighbourDouble = outputGrid.getData()[ir][ic];

                                }
                                
                            }
                        }
                        
                        if(r == 0) {                            
                            if(wholeDem.getData()[r+yOffset][c] >= neighbourDouble) {
                                outputGrid.getData()[r][c] = wholeDem.getData()[r+yOffset][c];
                                aboveOutBuffer[c] = wholeDem.getData()[r+yOffset][c];
                            } else {
                                Qs_l.add(new java.awt.Point(c, r));
                                outputGrid.getData()[r][c] = neighbourDouble;
                                aboveOutBuffer[c] = neighbourDouble;
                            } 
                        } else if(r == stripeNRows-1) {
                            if(wholeDem.getData()[r+yOffset][c] >= neighbourDouble) {
                                outputGrid.getData()[r][c] = wholeDem.getData()[r+yOffset][c];
                                belowOutBuffer[c] = wholeDem.getData()[r+yOffset][c];
                            } else {
                                Qs_l.add(new java.awt.Point(c, r));
                                outputGrid.getData()[r][c] = neighbourDouble;
                                belowOutBuffer[c] = neighbourDouble;
                            }                             
                        } else {
                            if(wholeDem.getData()[r+yOffset][c] >= neighbourDouble) {
                                outputGrid.getData()[r][c] = wholeDem.getData()[r+yOffset][c];
                            } else {
                                Qs_l.add(new java.awt.Point(c, r));
                                outputGrid.getData()[r][c] = neighbourDouble;
                            }  
                        }
                    }
                    
                }
            }
        }
        
    }

    private void process(List<java.awt.Point> fromCue, List<java.awt.Point> toCue) {
        
        terminated = true;
        while(!fromCue.isEmpty()) {

            java.awt.Point cell = fromCue.get(fromCue.size()-1);
            fromCue.remove(fromCue.size()-1);
            
            double neighbourDouble = Double.MAX_VALUE;

            int r = cell.y;
            int c = cell.x;
            
            for(int i=0; i<8; i++) {
                int ic = c + Shifter.getColShift(i);
                int ir = r + Shifter.getRowShift(i);

                if(ic < 0 || ic >= stripeNCols) {
                    continue;
                }
                
                if(ir < 0) {
                    if(!wholeDem.isNoData(aboveOutBuffer[ic]) &&
                            aboveOutBuffer[ic] < neighbourDouble) {
                        neighbourDouble = aboveOutBuffer[ic];
                    }
                } else if (ir == stripeNRows) {
                    if(!wholeDem.isNoData(belowOutBuffer[ic]) &&
                            belowOutBuffer[ic] < neighbourDouble) {
                        neighbourDouble = belowOutBuffer[ic];
                    }
                } else {
                    if(!wholeDem.isNoData(outputGrid.getData()[ir][ic]) &&
                            outputGrid.getData()[ir][ic] < neighbourDouble) {
                        neighbourDouble = outputGrid.getData()[ir][ic];
                    }
                }

            }
            
            if(wholeDem.getData()[r+yOffset][c] >= neighbourDouble) {
                outputGrid.getData()[r][c] = wholeDem.getData()[r+yOffset][c];
                terminated = false;
            } else {
                toCue.add(new java.awt.Point(c, r));
                if(outputGrid.getData()[r][c] > neighbourDouble) {
                    outputGrid.getData()[r][c] = neighbourDouble;
                    terminated = false;
                }
            }
            
            // Update buffers if on first or last row
            if(r==0) {
                if(!wholeDem.isNoData(aboveOutBuffer[c])) {
                    aboveOutBuffer[c] = outputGrid.getData()[r][c];
                }
            } else if (r==stripeNRows-1) {
                if(!wholeDem.isNoData(belowOutBuffer[c])) {
                    belowOutBuffer[c] = outputGrid.getData()[r][c];
                }
            }
            
        }
        
    }    

    @Override
    public double[] getBelowBuffer() {
        return belowOutBuffer;
    }

    @Override
    public double[] getAboveBuffer() {
        return aboveOutBuffer;
    }

    @Override
    public void plugNewBelowBuffer(double[] newBuffer) {
        belowOutBuffer = newBuffer;
    }

    @Override
    public void plugNewAboveBuffer(double[] newBuffer) {
        aboveOutBuffer = newBuffer;
    }
    
    public boolean isTerminated() {
        return terminated;
    }
    
    private boolean initialize = true;
    
    private final DoubleBasicGrid wholeDem;
    private final BooleanBasicGrid wholeBorder;
    
    private final int gridNRows;
        
    private final List<java.awt.Point> Qs1_l = new ArrayList<java.awt.Point>();
    private boolean terminated = false;
    private boolean theSwitch = false;
    
    private double[] aboveOutBuffer;
    private double[] belowOutBuffer;
    
}
