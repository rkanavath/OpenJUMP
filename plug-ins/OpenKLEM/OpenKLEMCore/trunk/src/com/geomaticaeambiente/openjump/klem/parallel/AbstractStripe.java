package com.geomaticaeambiente.openjump.klem.parallel;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.vividsolutions.jts.geom.Coordinate;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

public abstract class AbstractStripe implements Callable<DoubleBasicGrid> {
    
    public AbstractStripe(int id, DoubleBasicGrid stripeGrid, int yOffset,
            double[] aboveBuffer, double[] belowBuffer) {
        
        this.stripeId = id;
        this.stripeGrid = stripeGrid;
        this.yOffset = yOffset;
        this.aboveBuffer = aboveBuffer;
        this.belowBuffer = belowBuffer;
        
        initialize();    
        
    }
    
    protected final void initialize() {
        
        stripeNRows = stripeGrid.getRowCount();
        stripeNCols = stripeGrid.getColumnCount();
        
    }
    
    @Override
    public DoubleBasicGrid call() throws Exception {
        process();
        return outputGrid;
    }
    
    protected abstract void process();
  
    public double[] getAboveBuffer() {
        return aboveBuffer;
    }
    
    public double[] getBelowBuffer() {
        return belowBuffer;
    }

    public List<Point> getQs_l() {
        return Qs_l;
    }
    
    public int getQsCount() {
        return Qs_l.size();
    }
    
    public void plugNewAboveBuffer(double[] newBuffer) {
        aboveBuffer = newBuffer;
    }
    
    public void plugNewBelowBuffer(double[] newBuffer) {
        belowBuffer = newBuffer;
    }

    protected final int stripeId;
    protected final DoubleBasicGrid stripeGrid;
    protected int stripeNRows, stripeNCols;
    protected final int yOffset;
    
    protected double[] aboveBuffer;
    protected double[] belowBuffer;
    protected DoubleBasicGrid outputGrid;
    
    protected List<java.awt.Point> Qs_l = new ArrayList<java.awt.Point>();
    
}
