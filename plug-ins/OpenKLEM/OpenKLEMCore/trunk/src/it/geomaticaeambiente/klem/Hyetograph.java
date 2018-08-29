package it.geomaticaeambiente.klem;

/**
 * A hyetograph is a series of rainfall values [mm] for every time interval.
 * @author AdL
 */
public class Hyetograph {

    /**
     * Constructs a new hyetograph.
     * @param timeInterval The time step between rainfall pulses.
     * @param rainfall The array containing the rainfall values [mm/time interval] for every
     * time interval.
     * @throws Exception 
     */
    public Hyetograph(TimeInterval timeInterval, double[] rainfall) throws Exception {
        
        if(timeInterval == null || rainfall == null || rainfall.length < 2) {
            //throw new Exception("Invalid input parameters.");
        }
        
        this.timeInterval = timeInterval;
        this.rainfall = rainfall;
        
    }

    /**
     * Return the rainfall values.
     * @return The rainfall values [mm/time interval].
     */
    public double[] getRainfall(){
        return rainfall;
    }
    
    /**
     * Returns the time interval between rainfall pulses.
     * @return The time interval.
     */    
    public TimeInterval getStep(){
        return timeInterval;
    }

    /**
     * Return the number of steps of the hyetograph.
     * @return The number of steps.
     */
    public int getStepsCount(){
        return rainfall.length;
    }
    
    /**
     * Returns the total rain in mm.
     * @return The cumulative rain [mm].
     */
    public double getTotalRain(){
        
        double totalRain = 0;
        if(rainfall.length !=0){
            for(int s=0; s<rainfall.length; s++){
                totalRain += (double)Math.round(rainfall[s] * 100) / 100d;
            }
            return totalRain;
        }else{
            return 0;
        }
        
    }

    private TimeInterval timeInterval;
    private double[] rainfall;
    
}
