package it.geomaticaeambiente.klem;

import java.awt.Point;

/**
 * A drainage area is an area inside a watershed that does not provide a part of its
 * discharge to the watershed outlet. The drainage area generates a secondary hydrograph
 * that can be used a input in an another watershed.
 * @author AdL
 */
public class DrainageArea {

    /**
     * Constructs a new drainage area.
     * @param rasDrainageAreas The raster representing the drainage area.
     * @param drainageAreaCellValue The cell value identifying the cells
     * belonging to the drainage area.
     * @param uptakeFraction The fraction of discharge that is diverted by the
     * drainage area and will not reach the watershed outlet.
     * @param intakeHydrograph An optional intake hydrograph.
     * @param lag (Optional) The lag between the drainage area peak time and the main
     * watershed peak time.
     * @param intakePoint The intake hydrograph injection point as raster coordinates.
     */
    public DrainageArea(
            double[][] rasDrainageAreas,
            double drainageAreaCellValue,
            double uptakeFraction, SimulationOutput intakeHydrograph,
            TimeInterval lag, Point intakePoint) {

        this.rasDrainageAreas = rasDrainageAreas;
        this.drainageAreaCellValue = drainageAreaCellValue;
        this.uptakeFraction = uptakeFraction;
        this.intakeHydrograph = intakeHydrograph;
        this.lag = lag;
        this.intakePoint = intakePoint;
        
    }

    /**
     * Returns the uptake fraction of the drainage area.
     * @return The uptake fraction.
     */
    public double getUptakeFraction() {
        return uptakeFraction;
    }

    /**
     * Return the (optional) intake hydrograph.
     * @return The intake hydrograph. Can be null.
     */
    public SimulationOutput getIntakeHydrograph() {
        return intakeHydrograph;
    }

    /**
     * Return the intake hydrograph injection point.
     * @return The injection point (as raster coordinates).
     */
    public Point getIntakePoint() {
        return intakePoint;
    }

    /**
     * Return the lag.
     * @return Returns the lag.
     */
    public TimeInterval getLag() {
        return lag;
    }

    /**
     * Returns the drainage area raster.
     * @return The drainage area raster.
     */
    public double[][] getRasDrainageAreas() {
        return rasDrainageAreas;
    }

    /**
     * Returns the drainage area cell value.
     * @return The drainage area cell value.
     */
    public double getDrainageAreaCellValue() {
        return drainageAreaCellValue;
    }
    
    /**
     * Sets the lag.
     * @param lag The lag.
     */
    public void setLag(TimeInterval lag) {
        this.lag = lag;
    }
    
    private double[][] rasDrainageAreas;
    private double drainageAreaCellValue;
    private double uptakeFraction;
    private SimulationOutput intakeHydrograph;
    private TimeInterval lag;
    private Point intakePoint;
    
}
