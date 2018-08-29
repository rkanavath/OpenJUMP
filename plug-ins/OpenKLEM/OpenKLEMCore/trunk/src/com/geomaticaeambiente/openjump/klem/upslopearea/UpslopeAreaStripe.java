package com.geomaticaeambiente.openjump.klem.upslopearea;

import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;

/**
 *
 * @author AdL
 */
public abstract class UpslopeAreaStripe implements Callable<Boolean> {
    
    public UpslopeAreaStripe(int stripeId, FlowDirBasicGrid flowDirGrid, byte[][] dependencyStripe,
            double[][] upslopeAreaGrid, int yOffset) {
        
        this.stripeId = stripeId;
        this.flowDirsGrid = flowDirGrid;

        this.dependencyStripe = dependencyStripe;
        this.upslopeAreaGrid = upslopeAreaGrid;
        this.yOffset = yOffset;
        
        initialize();    
        
    }
    
    private void initialize() {
    
        gridNRows = flowDirsGrid.getRowCount();
        gridNCols = flowDirsGrid.getColumnCount();
        
        stripeNRows = dependencyStripe.length;
        stripeNCols = dependencyStripe[0].length;
        
        dependencyBuff_Above = new byte[flowDirsGrid.getColumnCount()];
        dependencyBuff_Below = new byte[flowDirsGrid.getColumnCount()];
        
        for(int i=0; i<weightS.length; i++) {
            
            double dist = Math.abs(Shifter.getColShift(i)) + Math.abs(Shifter.getRowShift(i));
            weightS[i] = Math.sqrt(dist);
            weightL[i] = weightS[i] / (dist * 2.);
            
        }

        findQs();
        
    }
    
    /**
    * Finds all the cells without upslope contributing cells.
    */
    private void findQs() {
        
        for(int r=0; r<stripeNRows; r++) {
            for(int c=0; c<stripeNCols; c++) {  
                
                if(dependencyStripe[r][c] <= 0 &&
                        upslopeAreaGrid[r + yOffset][c] == 0) {
                    Qs_l.add(new java.awt.Point(c, r));
                }
            }
        }
        
    }
    
    /**
    * Processes the stripe.
    */
    public void process() {
        
        while(!Qs_l.isEmpty()) {
            
            java.awt.Point cell = Qs_l.get(Qs_l.size()-1);            
            
            Qs_l.remove(Qs_l.size()-1);
            upslopeAreaGrid[cell.y + yOffset][cell.x] = 1;
                      
            // Add areas of contributing cells (they should be all calculated)
            updateUa(cell);
            
            // Update dependency maps
            updateDependencyMaps(cell, false);
            
        }
        
    }
    
    protected abstract void updateUa(Point cell);
    
    public byte[][] getDependencyStripe() {
        return dependencyStripe;
    }
    
    public byte[] getAboveBuffer() {
        return dependencyBuff_Above;
    }
    
    public byte[] getBelowBuffer() {
        return dependencyBuff_Below;
    }

    public List<Point> getQs_l() {
        return Qs_l;
    }
    
    public int getQsCount() {
        return Qs_l.size();
    }
    
    /**
     * Plugs in a new above buffer and updates the first row of the dependency
     * stripe. The x coordinate of the buffer cells are the same as the
     * last row of the upper stripe.
     * @param newBuffer The new buffer to be plugged in.
     */
    protected void plugNewAboveBuffer(byte[] newBuffer) {
        
        dependencyBuff_Above = newBuffer;
        for(int c=0; c<newBuffer.length; c++) {
//            updateDependencyMaps(new java.awt.Point(c, -1),
//                    dependencyBuff_Above[c], true);
            
            dependencyStripe[0][c] -= dependencyBuff_Above[c];
            if(dependencyStripe[0][c] == 0 && upslopeAreaGrid[0 + yOffset][c] == 0) {
                Qs_l.add(new Point(c, 0));
            }
            
        }
        
        Arrays.fill(dependencyBuff_Above, (byte) 0);
        
    }
    
    /**
     * Plugs in a new below buffer and updates the last row of the dependency
     * stripe. The x coordinate of the buffer cells are the same as the
     * first row of the lower stripe.
     * @param newBuffer The new buffer to be plugged in.
     */
    public void plugNewBelowBuffer(byte[] newBuffer) {
        
        dependencyBuff_Below = newBuffer;
        for(int c=0; c<newBuffer.length; c++) {
            
            dependencyStripe[dependencyStripe.length - 1][c] -= dependencyBuff_Below[c];
            if(dependencyStripe[dependencyStripe.length - 1][c] == 0 && upslopeAreaGrid[dependencyStripe.length - 1 + yOffset][c] == 0) {
                Qs_l.add(new Point(c, dependencyStripe.length - 1));
            }
//            updateDependencyMaps(new java.awt.Point(c, dependencyStripe.length),
//                    dependencyBuff_Below[c], true);
            
        }
        
        Arrays.fill(dependencyBuff_Below, (byte) 0);
        
    }    
 
    protected abstract void updateDependencyMaps(java.awt.Point sourceCell, boolean skipBuffers);

    protected final int stripeId;
    protected final FlowDirBasicGrid flowDirsGrid;
    protected int gridNRows, gridNCols;
    protected int stripeNRows, stripeNCols;
    protected final int yOffset;
    
    protected final byte[][] dependencyStripe;
    protected byte[] dependencyBuff_Above;
    protected byte[] dependencyBuff_Below;
    
    protected final double[][] upslopeAreaGrid;
    
    protected final List<java.awt.Point> Qs_l = new ArrayList<java.awt.Point>();
        
    protected final double[] weightS = new double[8];
    protected final double[] weightL = new double[8];
    
    
}
