package com.geomaticaeambiente.openjump.klem.units;

/**
 *
 * @author AdL
 */
public class Angle {

    public Angle(double value, AngleUnit unit) {
        this.value = value;

        // Convert to radians (internally storage is radians)
        switch(unit) {
            case DEGREE:
                this.value = Math.toRadians(value);
                break;
            case RADIAN:
                this.value = value;
                break;
        }
        
        
    }    
    
    public Double getAngle(AngleUnit unit) {
        
        switch(unit) {
            case DEGREE:
                return Math.toDegrees(value);
            case RADIAN:
                return value;
        }
        return null;
        
    }
    
    
    public enum AngleUnit {
        DEGREE, RADIAN;
    }
    
    private double value;
    
}
