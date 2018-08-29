package com.geomaticaeambiente.openjump.klem.rastertools;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author deluca
 */
public class Utils {
    
    public static double[] findUniqueVals(DoubleBasicGrid grid){

        Set<Double> uniqueVals_s = new HashSet<Double>();
        for(int r=0; r<grid.getRowCount(); r++) {
            for(int c=0; c<grid.getColumnCount(); c++) {
                
                double val = grid.getValue(c, r);
                if(!grid.isNoData(val)) {
                    uniqueVals_s.add(val);
                }
                
            }
        }

        double[] uniqueVals = new double[uniqueVals_s.size()];
        int pos = 0;
        for(Double uniqueVal : uniqueVals_s) {
            uniqueVals[pos++] = uniqueVal;
        }

        return uniqueVals;
        
    }
    
    
}
