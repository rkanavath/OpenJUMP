package com.geomaticaeambiente.openjump.klem.parallel2;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;

/**
 *
 * @author AdL
 */
public class DoubleStripeGrid2 extends DoubleBasicGrid{
    
    public DoubleStripeGrid2(DoubleBasicGrid stripeData,
            DoubleBasicGrid topBuffer, DoubleBasicGrid bottomBuffer) {
        super(stripeData.getData(), stripeData.getCellSize(), stripeData.getNoData(), stripeData.getLowerLeftCoord());
        this.stripeData = stripeData;
        this.topBuffer = topBuffer;
        this.bottomBuffer = bottomBuffer;
    }

    @Override
    public double getValue(java.awt.Point cell) {
        return this.getValue(cell.x, cell.y);
    }
    
    @Override
    public double getValue(int col, int row) {
        
        if(col < 0 || col >= stripeData.getColumnCount()) {
            return stripeData.getNoData();
        }

        if(row == -1) {
            return topBuffer.getValue(col, 0);
        } else if(row >= 0 || row < stripeData.getRowCount()) {
            return stripeData.getValue(col, row);
        } else if(row == stripeData.getRowCount()) {
            return bottomBuffer.getValue(col, 0);
        } else {
            return stripeData.getNoData();
        }
        
    }

    public DoubleBasicGrid getStripeData() {
        return stripeData;
    }

    public void setStripeData(DoubleBasicGrid stripeData) {
        this.stripeData = stripeData;
    }

    public DoubleBasicGrid getTopBuffer() {
        return topBuffer;
    }

    public void setTopBuffer(DoubleBasicGrid topBuffer) {
        this.topBuffer = topBuffer;
    }

    public DoubleBasicGrid getBottomBuffer() {
        return bottomBuffer;
    }

    public void setBottomBuffer(DoubleBasicGrid bottomBuffer) {
        this.bottomBuffer = bottomBuffer;
    }
    
    
    private DoubleBasicGrid stripeData;
    private DoubleBasicGrid topBuffer;
    private DoubleBasicGrid bottomBuffer;
    
}
