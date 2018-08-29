package com.geomaticaeambiente.klemgui.utils;

import com.geomaticaeambiente.openjump.klem.cn.ValuesRange;

/**
 *
 * @author Geomatica
 */
public class ValueRangeGroup extends ValuesRange{

    public ValueRangeGroup(double minValue, double maxValue, String name) {
        super(minValue, maxValue);
        this.name = name;
    }
    
    public String getName(){
        return name;
}
 
    public ValuesRange getValueRange(){
        
        return new ValuesRange(getMinValue(), getMaxValue());
        
    }
    
    private String name;
    
}
