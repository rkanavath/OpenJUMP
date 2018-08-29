package com.geomaticaeambiente.openjump.klem.rastertools;

/**
 *
 * @author deluca
 */
public class ReclassTuple {

    public ReclassTuple(double oldRangeMin, double oldRangeMax, double newValue) {

        this.oldRangeMin = oldRangeMin;
        this.oldRangeMax = oldRangeMax;
        this.newValue = newValue;

    }

    @Override
    public String toString() {
        return Double.toString(oldRangeMin) + "  " + Double.toString(oldRangeMax) + "  " + Double.toString(newValue);
    }

    public double getOldRangeMin() {
        return oldRangeMin;
    }

    public double getOldRangeMax() {
        return oldRangeMax;
    }

    public double getNewValue() {
        return newValue;
    }

    double oldRangeMin;
    double oldRangeMax;
    double newValue;

}
