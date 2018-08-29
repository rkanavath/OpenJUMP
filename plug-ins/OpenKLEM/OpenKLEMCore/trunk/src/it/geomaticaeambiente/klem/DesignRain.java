package it.geomaticaeambiente.klem;

import it.geomaticaeambiente.klem.LsppCalculator.LsppANParameters;

/**
 * A design rain is defined by the LSPP a and n parameters, and by the watershed
 * where the rainfall will occur.
 * @author AdL
 */
public class DesignRain {

    /**
     * Constructs a design rainfall.
     * @param duration The rainfall duration.
     * @param lsppANParameters THe LSPP a and n parameters.
     * @param watershed The watershed where the rainfall takes place.
     */
    public DesignRain(TimeInterval duration,
            LsppANParameters lsppANParameters,
            Watershed watershed){

        this.duration = duration;
        this.lsppANParameters = lsppANParameters;
        this.watershed = watershed;

        calcTotalRain();
        
    }
    
    /**
     * Calculates the total rain.
     */
    private void calcTotalRain(){

        double geomorphoFactor = watershed.getGeomorphology().getGeomorphoFactor();        
        
        double fattAttenSoglia = watershed.getGeomorphology().getGeomorphoFactorThrehsold();
        geomorphoFactor /= 1000d;
        
        double maxAreaThreshold = 200;
        double attenValue;
        if(watershed.getArea() <= maxAreaThreshold) {
            attenValue = 1.0 - geomorphoFactor * Math.max((watershed.getArea() - fattAttenSoglia), 0.0);
        } else {
            // For big watersheds the value becomes constant
            attenValue = 1.0 - geomorphoFactor * Math.max((maxAreaThreshold - fattAttenSoglia), 0.0);
        }
        
        if(duration.getInterval(TimeInterval.TimeIntervalUnit.HOUR) >= 1){
            totalRain = lsppANParameters.getParamA() *
                    Math.pow(duration.getInterval(TimeInterval.TimeIntervalUnit.HOUR), lsppANParameters.getParamN());
        }else{
            totalRain = lsppANParameters.getParamA() *
                    Math.pow(duration.getInterval(TimeInterval.TimeIntervalUnit.HOUR), lsppANParameters.getParamNLess1Hour());
        }
        
        // ARF: auto
        areaReductionFactor = watershed.getAreaReductionFactor();
        if(areaReductionFactor == -1){
            areaReductionFactor = calcFattAtten(watershed.getArea(), duration);
        }
        totalRain *= areaReductionFactor * attenValue;

    }

    public static double calcFattAtten(double watershedArea, TimeInterval duration) {
        double fattAtten = (1.) - Math.exp(-2.472 * Math.pow(watershedArea,-0.242) *
                    Math.pow((duration.getInterval(TimeInterval.TimeIntervalUnit.HOUR)),
                    (0.6-Math.exp(-0.643*Math.pow(watershedArea,0.235)))));
        return fattAtten;
    }
    
    /**
     * Returns the total rain.
     * @return The total rain.
     */
    public final double getTotalRain(){
        return totalRain;
    }
    
    /**
     * Return the time interval of the design rainfall.
     * @return The time interval of the design rainfall.
     */
    public TimeInterval getDuration(){
        return duration;
    }

    /**
     * Returns the LSPP a parameter.
     * @return The LSPP a parameter.
     */
    public double getLsppParamA() {
        return lsppANParameters.getParamA();
    }

    /**
     * Returns the LSPP n parameter.
     * @return The LSPP n parameter.
     */    
    public double getLsppParamN() {
        return lsppANParameters.getParamN();
    }

    /**
     * Returns the LSPP n parameter for rainfall duration less than 1 hour.
     * @return The LSPP n parameter for rainfall duration less than 1 hour.
     */      
    public double getLsppParamNLess1Hour() {
        return lsppANParameters.getParamNLess1Hour();
    }

    /**
     * Sets the duration of the rainfall.
     * @param duration
     * @return The rainfall duration.
     */      
    public void setDuration(TimeInterval duration) {
        this.duration = duration;
    }

    /**
     * Returns the Area Reduction Factor (ARF) [-].
     * @return The Area Reduction Factor (ARF) [-].
     */      
    public double getAreaReductionFactor() {
        return areaReductionFactor;
    }
    
    private TimeInterval duration;
    private LsppANParameters lsppANParameters = null;
    private Watershed watershed = null;
    private double totalRain = 0;
    private double areaReductionFactor = 0;
    
}