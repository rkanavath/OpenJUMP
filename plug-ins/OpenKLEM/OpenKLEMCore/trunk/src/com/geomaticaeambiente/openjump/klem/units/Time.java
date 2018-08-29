package com.geomaticaeambiente.openjump.klem.units;

/**
 * A class that models a time interval. Units supported are: seconds, minutes,
 * hours, days.
 * @author AdL
 */
public class Time {

    /**
     * Constructs a time interval.
     * @param interval The value of the time interval.
     * @param unit The unit of the time interval.
     */
    public Time(double interval, TimeIntervalUnit unit) {
        
        // Convert to seconds (internally storage is seconds)
        switch(unit) {
            case D:
                this.interval = interval *= SECS_IN_DAY;
                break;
            case h:
                this.interval = interval *= SECS_IN_HOUR;
                break;
            case m:
                this.interval = interval *= SECS_IN_MIN;
                break;
            case s:
                this.interval = interval;
                break;
        }
        
    }

    /**
     * Returns the time interval value in the given time unit.
     * @param unit The time unit.
     * @return The time interval value in the give unit.
     */
    public Double getInterval(TimeIntervalUnit unit) {
        
        switch(unit) {
            case D:
                return interval / SECS_IN_DAY;
            case h:
                return interval / SECS_IN_HOUR;
            case m:
                return interval / SECS_IN_MIN;
            case s:
                return interval;
        }
        return null;
        
    }

    public enum TimeIntervalUnit {
        s, m, h, D;
    }
    
    private double interval;
    private final int SECS_IN_MIN = 60;
    private final int SECS_IN_HOUR = 3600;
    private final int SECS_IN_DAY = 86400;
    
}