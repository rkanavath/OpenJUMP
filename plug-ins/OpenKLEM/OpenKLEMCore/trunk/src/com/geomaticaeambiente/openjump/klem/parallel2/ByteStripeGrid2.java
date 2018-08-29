package com.geomaticaeambiente.openjump.klem.parallel2;

import com.geomaticaeambiente.openjump.klem.grid.ByteBasicGrid;

/**
 *
 * @author AdL
 */
public class ByteStripeGrid2 extends ByteBasicGrid {
    
    public ByteStripeGrid2(ByteBasicGrid stripeData,
            ByteBasicGrid topBuffer, ByteBasicGrid bottomBuffer) {
        super(stripeData.getData(), stripeData.getCellSize(), stripeData.getNoData(), stripeData.getLowerLeftCoord());
        this.stripeData = stripeData;
        this.topBuffer = topBuffer;
        this.bottomBuffer = bottomBuffer;
    }

    @Override
    public byte getValue(java.awt.Point cell) {
        return this.getValue(cell.x, cell.y);
    }
    
    @Override
    public byte getValue(int col, int row) {
        
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

    public ByteBasicGrid getStripeData() {
        return stripeData;
    }

    public void setStripeData(ByteBasicGrid stripeData) {
        this.stripeData = stripeData;
    }

    public ByteBasicGrid getTopBuffer() {
        return topBuffer;
    }

    public void setTopBuffer(ByteBasicGrid topBuffer) {
        this.topBuffer = topBuffer;
    }

    public ByteBasicGrid getBottomBuffer() {
        return bottomBuffer;
    }

    public void setBottomBuffer(ByteBasicGrid bottomBuffer) {
        this.bottomBuffer = bottomBuffer;
    }

    private ByteBasicGrid stripeData;
    private ByteBasicGrid topBuffer;
    private ByteBasicGrid bottomBuffer;    
    
}
