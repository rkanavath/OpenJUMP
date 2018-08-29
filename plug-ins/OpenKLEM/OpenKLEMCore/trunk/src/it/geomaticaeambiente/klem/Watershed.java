package it.geomaticaeambiente.klem;

/**
 * This class models a watershed.
 * @author deluca
 */
public class Watershed {

    /**
     * Constructs a new watershed from its parameters.
     * @param area The watershed area (km2).
     * @param rasCurveNumber The raster with the Curve Number values for the 
     * watershed.
     * @param rasRoutingTime The raster with the routing time (hours) values for
     * the watershed.
     * @param cellSize The cell size.
     * @param noData The no data values.
     * @param AMC The AMC value.
     * @param initialAbstraction
     * @param baseflowParams
     * @param subsurfaceDrainLoss The subsurface drainage loss (0-1).
     * @param rainfallRecession The rainfall recession.
     * @param areaReductionFactor The area reduction factor (ARF).
     * @param geomorphology The watershed geomorphology.
     */
    public Watershed(
            double area,
            double[][] rasCurveNumber,
            double[][] rasRoutingTime,
            double cellSize, double noData,
            double AMC,
            InitialAbstraction initialAbstraction,
            BaseflowParams baseflowParams,
            double subsurfaceDrainLoss,
            RainfallRecession rainfallRecession,
            double areaReductionFactor,
            Geomorphology geomorphology){
        
        this.area = area;
        this.rasCurveNumber = rasCurveNumber;
        this.rasRoutingTime = rasRoutingTime;
        this.cellSize = cellSize;
        this.noData = noData;
        this.initialAbstraction = initialAbstraction;
        this.AMC = AMC;
        this.baseflowParams =  baseflowParams;
        this.subsurfaceDrainLoss = subsurfaceDrainLoss; // subsurface drainage loss
        this.rainfallRecession = rainfallRecession;
        this.areaReductionFactor = areaReductionFactor;
        this.geomorphology = geomorphology;
        
        if(baseflowParams.getInitialBaseflow() == null ||
                baseflowParams.getInitialBaseflow() < 0) {
            calcInitialBaseflow();
        }
    }

    private void calcInitialBaseflow() {
        double initialBaseflow = area * 0.05;
        baseflowParams.setInitialBaseflow(initialBaseflow);
    }

    public double getArea() {
        return area;
    }

    public void setArea(double area) {
        this.area = area;
    }

    public double[][] getRasCurveNumber() {
        return rasCurveNumber;
    }

    public void setRasCurveNumber(double[][] rasCurveNumber) {
        this.rasCurveNumber = rasCurveNumber;
    }

    public double[][] getRasRoutingTime() {
        return rasRoutingTime;
    }

    public void setRasRoutingTime(double[][] rasRoutingTime) {
        this.rasRoutingTime = rasRoutingTime;
    }

    public double getCellSize() {
        return cellSize;
    }

    public void setCellSize(double cellSize) {
        this.cellSize = cellSize;
    }

    public double getNoData() {
        return noData;
    }

    public void setNoData(double noData) {
        this.noData = noData;
    }

    public InitialAbstraction getInitialAbstraction() {
        return initialAbstraction;
    }

    public void setInitialAbstraction(InitialAbstraction initialAbstraction) {
        this.initialAbstraction = initialAbstraction;
    }

    public double getAMC() {
        return AMC;
    }

    public void setAMC(double AMC) {
        this.AMC = AMC;
    }

    public BaseflowParams getBaseflowParams() {
        return baseflowParams;
    }

    public void setBaseflowParams(BaseflowParams baseflowParams) {
        this.baseflowParams = baseflowParams;
    }

    public double getSubsurfaceDrainLoss() {
        return subsurfaceDrainLoss;
    }

    public void setSubsurfaceDrainLoss(double subsurfaceDrainLoss) {
        this.subsurfaceDrainLoss = subsurfaceDrainLoss;
    }

    public RainfallRecession getRainfallRecession() {
        return rainfallRecession;
    }

    public void setRainfallRecession(RainfallRecession rainfallRecession) {
        this.rainfallRecession = rainfallRecession;
    }

    public double getAreaReductionFactor() {
        return areaReductionFactor;
    }

    public void setAreaReductionFactor(double areaReductionFactor) {
        this.areaReductionFactor = areaReductionFactor;
    }

    public Geomorphology getGeomorphology() {
        return geomorphology;
    }

    public void setGeomorphology(Geomorphology geomorphology) {
        this.geomorphology = geomorphology;
    }
    
    
    
    private double area = 0;
    private double[][] rasCurveNumber = null;
    private double[][] rasRoutingTime = null;
    private double cellSize = 0;
    private double noData = 0;
    private InitialAbstraction initialAbstraction;
    private double AMC = 0;
    private BaseflowParams baseflowParams;
    private double subsurfaceDrainLoss;
    private RainfallRecession rainfallRecession;
    private double areaReductionFactor;
    private Geomorphology geomorphology;
    
}
