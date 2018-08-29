package com.geomaticaeambiente.openjump.klem.rastertools;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;

/**
 *
 * @author AdL
 */
public class RasterReclassifier {
    
    public RasterReclassifier() {}
        
    public DoubleBasicGrid reclassify(DoubleBasicGrid inputGrid, ReclassTuple[] reclassPairs) {
        
        DoubleBasicGrid outputGrid = new DoubleBasicGrid(
                new double[inputGrid.getRowCount()][inputGrid.getColumnCount()],
                inputGrid.getCellSize(), inputGrid.getNoData(), inputGrid.getLowerLeftCoord());
        
        for(int r=0; r<inputGrid.getRowCount(); r++) {
            for(int c=0; c<inputGrid.getColumnCount(); c++) {
                
                double val = inputGrid.getValue(c, r);
                if(!inputGrid.isNoData(val)) {
                    
                    boolean valueChanged = false;
                    for (ReclassTuple reclassPair : reclassPairs) {
                        if (val >= reclassPair.getOldRangeMin() &&
                                val <= reclassPair.getOldRangeMax()) {
                            outputGrid.setValue(c, r, reclassPair.getNewValue());
                            valueChanged = true;
                            break;
                        }
                    }
                    
                    if(!valueChanged) {
                        outputGrid.setValue(c, r, val);
                    }
                    
                } else {
                    outputGrid.setValue(c, r, inputGrid.getNoData());
                }
                
            }
        }
        return outputGrid;
        
    }    
    
}
