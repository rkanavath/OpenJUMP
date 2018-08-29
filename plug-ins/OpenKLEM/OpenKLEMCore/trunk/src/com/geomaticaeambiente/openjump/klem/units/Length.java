package com.geomaticaeambiente.openjump.klem.units;

/**
 *
 * @author AdL
 */
public class Length {

    public Length(double length, LengthUnit unit) {
        
        // Convert to m (internally storage is m)
        switch(unit) {
            case m:
                this.length = length;
                break;
            case km:
                this.length = length * M_IN_KM;
                break;
        }

        
    }

    public Double getLength(LengthUnit unit) {
        
        switch(unit) {
            case m:
                return length;
            case km:
                return length / M_IN_KM;
        }
        return null;
        
    }

    public enum LengthUnit {
        m, km;
    }
    
    private double length;
    private final double M_IN_KM = 1E3;
    
}
