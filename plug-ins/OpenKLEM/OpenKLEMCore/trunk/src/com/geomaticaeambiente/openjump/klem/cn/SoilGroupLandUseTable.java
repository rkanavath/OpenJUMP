package com.geomaticaeambiente.openjump.klem.cn;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author AdL
 */
public class SoilGroupLandUseTable {

    
    public void addTuple(LandUseSoilGroupTuple landUseSoilGroupTuple) {
        
        tuples_l.add(landUseSoilGroupTuple);
        
    }
    
    public Double getCnValue(double landUseValue, double soilGroupValue) {
        
        Double cnValue = null;
        
        for(LandUseSoilGroupTuple tuple : tuples_l) {
            if(tuple.getLandUseRange().getMinValue() <= landUseValue &&
                    tuple.getLandUseRange().getMaxValue() >= landUseValue) {

                if(tuple.getSoilGroupRange().getMinValue() >= soilGroupValue &&
                        tuple.getSoilGroupRange().getMaxValue() <= soilGroupValue) {
                    
                    return tuple.getCnValue();
                }
            }
        }
        
        
        return cnValue;
        
    }

    private final List<LandUseSoilGroupTuple> tuples_l = new ArrayList<LandUseSoilGroupTuple>();
    
    
}
