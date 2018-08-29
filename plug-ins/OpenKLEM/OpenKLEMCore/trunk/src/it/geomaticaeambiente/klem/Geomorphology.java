package it.geomaticaeambiente.klem;

/**
 * The geomorphological parameters of the watershed.
 * @author AdL
 */
public class Geomorphology {

    /**
     * Constructs the geomorphology of a watershed.
     * @param geomorphoFactor The geomorphological factor, in the 0-1 range.
     * @param geomorphoFactorThrehsold  The geomorphological factor is applied
     * only above this area threshold [km2].
     */
    public Geomorphology(double geomorphoFactor, double geomorphoFactorThrehsold) {
        this.geomorphoFactor = geomorphoFactor;
        this.geomorphoFactorThrehsold = geomorphoFactorThrehsold;
    }

    /**
     * Returns the geomorphological factor.
     * @return The geomorphological factor in the 0-1 range.
     */
    public double getGeomorphoFactor() {
        return geomorphoFactor;
    }

    /**
     * Returns the area threshold [km2] above which the geomorphological factor is
     * applied.
     * @return 
     */
    public double getGeomorphoFactorThrehsold() {
        return geomorphoFactorThrehsold;
    }
    
    private double geomorphoFactor;
    private double geomorphoFactorThrehsold;
    
}
