package com.geomaticaeambiente.openjump.klem.units;

import com.geomaticaeambiente.openjump.klem.units.Length.LengthUnit;

/**
 *
 * @author AdL
 */
public class Area {

    public Area(double area, LengthUnit unit) {
        
        // Convert to m2 (internally storage is m2)
        switch(unit) {
            case m:
                this.area = area;
                break;
            case km:
                this.area = area * M2_IN_KM2;
                break;
        }

        
    }

    public Double getArea(LengthUnit unit) {
        
        switch(unit) {
            case m:
                return area;
            case km:
                return area / M2_IN_KM2;
        }
        return null;
        
    }

    private double area;
    private final double M2_IN_KM2 = 1E6;
    
}
