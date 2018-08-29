package com.geomaticaeambiente.openjump.klem.parallel2;

import com.geomaticaeambiente.openjump.klem.grid.IntBasicGrid;

/**
 *
 * @author AdL
 */
public class IntStripeGrid2 extends IntBasicGrid {
    
    public IntStripeGrid2(IntBasicGrid stripeData,
            IntBasicGrid topBuffer, IntBasicGrid bottomBuffer) {
        super(stripeData.getData(), stripeData.getCellSize(), stripeData.getNoData(), stripeData.getLowerLeftCoord());
        this.stripeData = stripeData;
        this.topBuffer = topBuffer;
        this.bottomBuffer = bottomBuffer;
    }

    @Override
    public int getValue(java.awt.Point cell) {
        return this.getValue(cell.x, cell.y);
    }
    
    @Override
    public int getValue(int col, int row) {
        
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

    public IntBasicGrid getStripeData() {
        return stripeData;
    }

    public void setStripeData(IntBasicGrid stripeData) {
        this.stripeData = stripeData;
    }

    public IntBasicGrid getTopBuffer() {
        return topBuffer;
    }

    public void setTopBuffer(IntBasicGrid topBuffer) {
        this.topBuffer = topBuffer;
    }

    public IntBasicGrid getBottomBuffer() {
        return bottomBuffer;
    }

    public void setBottomBuffer(IntBasicGrid bottomBuffer) {
        this.bottomBuffer = bottomBuffer;
    }
    
    private IntBasicGrid stripeData;
    private IntBasicGrid topBuffer;
    private IntBasicGrid bottomBuffer;
    
}
