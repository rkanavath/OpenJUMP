package it.geomaticaeambiente.klem;

/**
 * A class that models a time interval. Units supported are: seconds, minutes,
 * hours, days.
 * @author AdL
 */
public class TimeInterval {

    /**
     * Constructs a time interval.
     * @param interval The value of the time interval.
     * @param unit The unit of the time interval.
     */
    public TimeInterval(double interval, TimeIntervalUnit unit) {
        
        // Convert to seconds (internally storage is seconds)
        if (unit == TimeIntervalUnit.DAY) {
            this.interval = interval *= SECS_IN_DAY;
        }else if (unit == TimeIntervalUnit.HOUR) {
            this.interval = interval *= SECS_IN_HOUR;
        } else if (unit == TimeIntervalUnit.MINUTE) {
            this.interval = interval *= SECS_IN_MIN;
        } else if (unit == TimeIntervalUnit.SECOND) {
            this.interval = interval;
        }
        
    }

    /**
     * Returns the time interval value in the given time unit.
     * @param unit The time unit.
     * @return The time interval value in the give unit.
     */
    public double getInterval(TimeIntervalUnit unit) {
        
        if (unit == TimeIntervalUnit.DAY) {
            return interval / SECS_IN_DAY;
        } else if(unit == TimeIntervalUnit.HOUR) {
            return interval / SECS_IN_HOUR;
        } else if(unit == TimeIntervalUnit.MINUTE) {
            return interval / SECS_IN_MIN;
        } else if(unit == TimeIntervalUnit.SECOND) {
            return interval;
        }
        
        return -1;
        
    }

    public enum TimeIntervalUnit {
        SECOND, MINUTE, HOUR, DAY;
    }
    
    private double interval;
    private final int SECS_IN_MIN = 60;
    private final int SECS_IN_HOUR = 3600;
    private final int SECS_IN_DAY = 86400;
    
}