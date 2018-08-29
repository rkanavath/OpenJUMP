package com.geomaticaeambiente.openjump.klem.cn;

/**
 *
 * @author AdL
 */
public class ValuesRange {

    public ValuesRange(double minValue, double maxValue) {
        this.minValue = minValue;
        this.maxValue = maxValue;
    }

    public double getMinValue() {
        return minValue;
    }

    public double getMaxValue() {
        return maxValue;
    }

    private final double minValue;
    private final double maxValue;
    
}
