package com.geomaticaeambiente.openjump.klem.rastertools.classifiers;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;

/**
 *
 * @author deluca
 */
public class Autoscale implements ClassAlgo {

    public Autoscale(DoubleBasicGrid grid) {
        this.grid = grid;
    }
    
    @Override
    public double[] getBreakValues() {
        
        // Find min and max vals
        double minVal = Double.MAX_VALUE;
        double maxVal = -Double.MIN_VALUE;
        
        for(int r=0; r<grid.getRowCount(); r++) {
            for(int c=0; c<grid.getColumnCount(); c++) {
                double value = grid.getValue(c, r);
                if(!grid.isNoData(value)) {
                    if(value < minVal) {
                        minVal = value;
                    }
                    if(value > maxVal) {
                        maxVal = value;
                    }
                }
            }
        }
        
        int intvCount = 24;
        if(minVal == maxVal){
            maxVal = maxVal + intvCount * Double.MIN_VALUE;
        }

        double valSpan = (maxVal - minVal) / intvCount;
        if(valSpan <=0){
            valSpan = maxVal / intvCount;
        }

        double rMod = Math.log10(Math.max(valSpan, Double.MIN_VALUE));
        double aMod = Math.floor(rMod);

        valSpan = (int)(0.9999 + Math.pow(10,(rMod - aMod)));
        if(valSpan >= 8.9){
            valSpan = 10;
        }

        valSpan = valSpan * Math.pow(10, aMod);

        minVal = valSpan * (int)(minVal / valSpan);
        maxVal = valSpan * (int)(maxVal / valSpan); //minVal + valSpan * (intvCount);

        intvCount = (int)((maxVal - minVal) / valSpan);

        if(intvCount > 100){
            double valSpan2 = (maxVal - minVal) / 24;
            rMod = Math.log10(valSpan2);
            valSpan2 = (int)(0.999 + Math.pow(10, (rMod-(int)rMod))) * Math.pow(10, (int)rMod);
            maxVal = minVal + valSpan2 * 24;
            intvCount = 10 + (int)((maxVal - minVal) / valSpan2);
        }else if(intvCount ==0){
            intvCount = 1;
        }

        double valStart;
        double valEnd = minVal;

        double[] breaks = new double[intvCount];
        for(int i=0;i<intvCount;i++){
            valStart = valEnd;
            valEnd = valStart + valSpan;
            breaks[i] = valEnd;
        }

        return breaks;
        
    }
    
    private final DoubleBasicGrid grid;
    
}
