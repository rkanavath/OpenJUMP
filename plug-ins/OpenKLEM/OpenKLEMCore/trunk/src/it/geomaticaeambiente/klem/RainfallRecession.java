package it.geomaticaeambiente.klem;

public class RainfallRecession {
    
    /**
     * Constructs a new rainfall recession parameter set
     * @param rainfallRecession The rainfall recession [h-1]
     * @param rainfallRecessionThreshold The precipitation threshold [mm]: recession
     * takes places only below this threshold.
     */
    public RainfallRecession(double rainfallRecession, double rainfallRecessionThreshold) {
        this.rainfallRecession = rainfallRecession;
        this.rainfallRecessionThreshold = rainfallRecessionThreshold;
    }

    public double getRainfallRecession() {
        return rainfallRecession;
    }

    public void setRainfallRecession(double rainfallRecession) {
        this.rainfallRecession = rainfallRecession;
    }

    public double getRainfallRecessionThreshold() {
        return rainfallRecessionThreshold;
    }

    public void setRainfallRecessionThreshold(double rainfallRecessionThreshold) {
        this.rainfallRecessionThreshold = rainfallRecessionThreshold;
    }
    
    private double rainfallRecession;
    private double rainfallRecessionThreshold;
    
}
