package com.geomaticaeambiente.openjump.klem.parallel;

import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;

public abstract class DependencyAbstractStripe implements Callable<Boolean> {
    
    /**
     * Instantiates a stripe.
     * @param id The stripe ID. Should be unique.
     * @param flowDirGrid The 
     * @param dependencyStripe
     * @param outputData
     * @param yOffset
     */
    public DependencyAbstractStripe(
            int id, FlowDirBasicGrid flowDirGrid, byte[][] dependencyStripe, double[][] outputData, int yOffset) {
        
        this.stripeId = id;
        this.flowDirsGrid = flowDirGrid;
        this.dependencyStripe = dependencyStripe;
        this.outputData = outputData;
        this.yOffset = yOffset;
        
        initialize();    
        
    }
    
    protected final void initialize() {
        
        gridNRows = flowDirsGrid.getRowCount();
        gridNCols = flowDirsGrid.getColumnCount();
        
        stripeNRows = dependencyStripe.length;
        stripeNCols = dependencyStripe[0].length;
        
        dependencyBuff_Above = new byte[flowDirsGrid.getColumnCount()];
        dependencyBuff_Below = new byte[flowDirsGrid.getColumnCount()];
                
        findQs();
        
    }
    
    @Override
    public Boolean call() throws Exception {
        process();
        return true;
    }
    
    protected abstract void process();
    
    protected abstract void findQs();
    
    /**
     * Updates the property distance grid for the given cell.
     * @param cell The cell.
     */
    protected abstract void updateCell(java.awt.Point cell);

    protected abstract void updateDependencyMaps(java.awt.Point cell, int valueToAdd);
    
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
    public void plugNewAboveBuffer(byte[] newBuffer) {
        
        dependencyBuff_Above = newBuffer;
        for(int c=0; c<newBuffer.length; c++) {
            updateDependencyMaps(new java.awt.Point(c, -1),
                    dependencyBuff_Above[c]);
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
            
            updateDependencyMaps(
                    new java.awt.Point(c, dependencyStripe.length),
                    dependencyBuff_Below[c]);
            
        }
        
        Arrays.fill(dependencyBuff_Below, (byte) 0);
        
    }

    protected final int stripeId;
    protected final FlowDirBasicGrid flowDirsGrid;
    protected int gridNRows, gridNCols;
    protected int stripeNRows, stripeNCols;
    protected final int yOffset;
    
    protected final byte[][] dependencyStripe;
    protected byte[] dependencyBuff_Above;
    protected byte[] dependencyBuff_Below;
    protected final double[][] outputData;
    
    protected List<java.awt.Point> Qs_l = new ArrayList<java.awt.Point>();
    
}
