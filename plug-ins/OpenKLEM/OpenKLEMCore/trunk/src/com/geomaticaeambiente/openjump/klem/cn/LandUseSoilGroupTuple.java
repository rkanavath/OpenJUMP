package com.geomaticaeambiente.openjump.klem.cn;

/**
 *
 * @author AdL
 */
public class LandUseSoilGroupTuple {

    public LandUseSoilGroupTuple(
            ValuesRange landUseRange,
            ValuesRange soilGroupRange,
            double cnValue) {
        
        this.landUseRange = landUseRange;
        this.soilGroupRange = soilGroupRange;
        this.cnValue = cnValue;
        
    }
    
    public ValuesRange getLandUseRange() {
        return landUseRange;
    }

    public ValuesRange getSoilGroupRange() {
        return soilGroupRange;
    }
    
    public double getCnValue() {
        return cnValue;
    }
    
    private final ValuesRange landUseRange;
    private final ValuesRange soilGroupRange;
    private final double cnValue;
    
}
