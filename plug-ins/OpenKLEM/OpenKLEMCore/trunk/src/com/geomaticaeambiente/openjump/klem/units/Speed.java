package com.geomaticaeambiente.openjump.klem.units;

/**
 *
 * @author AdL
 */
public class Speed {
    
    public Speed(Length length, Time time) {
        
        // Convert to m/s (internal storage is m/s
        double lengthMeter = length.getLength(Length.LengthUnit.m);
        double timeSecond = time.getInterval(Time.TimeIntervalUnit.s);
        this.speed = lengthMeter / timeSecond; 
        
    }

    public Double getSpeed(SpeedUnit speedUnit) {
        
        switch(speedUnit) {
            case m_s:
                return speed;
        }
        return null;
    }
    
    private final double speed;
    
    public enum SpeedUnit {
        
        m_s;
        
    }
    
}
