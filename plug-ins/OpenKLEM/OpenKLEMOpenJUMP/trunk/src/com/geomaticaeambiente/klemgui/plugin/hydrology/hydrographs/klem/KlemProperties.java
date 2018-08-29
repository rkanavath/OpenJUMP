package com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.vividsolutions.jts.geom.Coordinate;
import it.geomaticaeambiente.klem.BaseflowParams;
import it.geomaticaeambiente.klem.Geomorphology;
import it.geomaticaeambiente.klem.HyetographGenerator;
import it.geomaticaeambiente.klem.InitialAbstraction;
import it.geomaticaeambiente.klem.LsppCalculator;
import it.geomaticaeambiente.klem.LsppCalculator.LsppModel;
import it.geomaticaeambiente.klem.RainfallRecession;
import java.io.File;
import java.util.Locale;

public class KlemProperties {

    public KlemProperties() {
    }
      
    //initial params
    public File getCNFile() {
        return new File(projectFolder + File.separator + inputFolderName + File.separator + cnFileName);
    }
    
    public File getDemFile() {
        return new File(projectFolder + File.separator + inputFolderName + File.separator + demFileName);
    }

    public boolean isWithBluelines() {
        return withBluelines;
    }

    public void setWithBluelines(boolean withBluelines) {
        this.withBluelines = withBluelines;
    }
    
    public File getBluelinesFile() {
        if(withBluelines) {
            return new File(projectFolder + File.separator + inputFolderName + File.separator + bluelinesFileName);
        } else {
            return null;
        }
    }
    
    public File getProjectFolder() {
        return projectFolder;
    }

    public void setProjectFolder(File projectFolder) {
        if (projectFolder == null) {
            this.projectFolder = null;
        } else {
            this.projectFolder = projectFolder;
        }
    }
    
    public String getProjectName() {
        return projectName;
    }

    public void setProjectName(String projectName) {
        if (projectName.equals("")) {
            this.projectName = null;
        } else {
            this.projectName = projectName;
        }
    }
    
    //project params
    public File getUpslopeAreaFile() {
        return new File(projectFolder + File.separator + inputFolderName + File.separator + upslopeAreaFileName);
    }

    public File getFlowDir() {
        return new File(projectFolder + File.separator + inputFolderName + File.separator + flowDirFileName);
    }
    
    public void setOutletXCoord(double outletXCoord) {
        this.outletXCoord = outletXCoord;
    }

    public void setOutletYCoord(double outletYCoord) {
        this.outletYCoord = outletYCoord;
    }

    public Coordinate getOutletCoords() {
        if (outletXCoord != null && outletYCoord != null) {
            return new Coordinate(outletXCoord, outletYCoord);
        } else {
            return null;
        }
    }

    public Double getOutletElevation() {
        return outletElevation;
    }

    public void setOutletElevation(double outletElevation) {
        this.outletElevation = outletElevation;
    }

    public Double getCatchmentArea() {
        return catchmentArea;
    }
    
    public void setCatchmentArea(double area) {
        this.catchmentArea = area;
    }
    
    //rainfall type: point, historical, distributed
    public RainfallType getRainfallType() {
        return rainfallType;
    }

    public void setRainfallType(RainfallType rainfallType) {
        this.rainfallType = rainfallType;
    }
    
    //point rainfall
    public Double getAParam() {
        return aParam;
    }

    public void setAParam(double aParam) {
        this.aParam = aParam;
    }

    public Double getNParam() {
        return nParam;
    }

    public void setNParam(double nParam) {
        this.nParam = nParam;
    }

    //distributed
    public LsppModel getLsppModel() {
        return lsppModel;
    }

    public void setLsppModel(LsppModel lsppModel) {
        this.lsppModel = lsppModel;
    }

    public File getAFile() {
        return aFile;
    }

    public void setAFile(String aFile) {
        if (!aFile.equals("")) {
            this.aFile = new File(aFile);
        } else {
            this.aFile = null;
        }
    }

    public File getNFile() {
        return nFile;
    }

    public void setNFile(String nFile) {
        if (!nFile.equals("")) {
            this.nFile = new File(nFile);
        } else {
            this.nFile = null;
        }
    }

    public File getCvFile() {
        return cvFile;
    }

    public void setCvFile(String cvFile) {
        if (!cvFile.equals("")) {
            this.cvFile = new File(cvFile);
        } else {
            this.cvFile = null;
        }
    }

    public Integer getReturnPeriod() {
        return returnPeriod;
    }

    public void setReturnPeriod(int returnTime) {
        this.returnPeriod = returnTime;
    }
    
    public Double getnLessHourParam() {
        return nLessHourParam;
    }

    public void setnLessHourParam(Double nLessHourParam) {
        this.nLessHourParam = nLessHourParam;
    }

    public boolean isNLessHourParamAuto() {
        return nLessHourParamAuto;
    }

    public void setnLessHourParamAuto(boolean nLessHourParamAuto) {
        this.nLessHourParamAuto = nLessHourParamAuto;
    }
    
    public double getAutoNLessHour() {
        return 0.366;
    }
    
    
    //historical rainfall
    public File getHistoricalRainfallFile() {
        return historicalRainfallFile;
    }

    public void setHistoricalRainfallFile(String historicalRainfallFile) {
        if (!historicalRainfallFile.equals("")) {
            this.historicalRainfallFile = new File(historicalRainfallFile);
        } else {
            this.historicalRainfallFile = null;
        }

    }

    //advanced parameters
    public KinematicsType getKinematicsType() {
        return kinematicsType;
    }

    public void setKinematicsType(KinematicsType kinematicsType) {
        this.kinematicsType = kinematicsType;
    }
    
    //simple kinematics
    public Double getSlopeVelocity() {
        return slopeVelocity;
    }

    public void setSlopeVelocity(Double slopeVelocity) {
        this.slopeVelocity = slopeVelocity;
    }

    public Double getChannelVelocity() {
        return channelVelocity;
    }

    public void setChannelVelocity(Double channelVelocity) {
        this.channelVelocity = channelVelocity;
    }
    
    public Double getThresholdValue() {
        return thresholdValue;
    }

    public void setThresholdValue(Double thresholdValue) {
        this.thresholdValue = thresholdValue;
    }
    
    //complex kinematics
    public Double getMaxSlopeVelocity() {
        return maxSlopeVelocity;
    }

    public void setMaxSlopeVelocity(Double maxSlopeVelocity) {
        this.maxSlopeVelocity = maxSlopeVelocity;
    }

    public Double getMinSlopeVelocity() {
        return minSlopeVelocity;
    }

    public void setMinSlopeVelocity(Double minSlopeVelocity) {
        this.minSlopeVelocity = minSlopeVelocity;
    }

    public Double getSlopeConstant() {
        return slopeCost;
    }

    public void setSlopeConstant(Double slopeCost) {
        this.slopeCost = slopeCost;
    }

    public Double getMinThreshold() {
        return minThreshold;
    }

    public void setMinThreshold(Double minThreshold) {
        this.minThreshold = minThreshold;
    }

    public Double getMaxThreshold() {
        return maxThreshold;
    }

    public void setMaxThreshold(Double maxThreshold) {
        this.maxThreshold = maxThreshold;
    }

    public Double getThresholdConstant() {
        return thresholdCost;
    }

    public void setThresholdConstant(Double thresholdCost) {
        this.thresholdCost = thresholdCost;
    }

    //hyetograph
    public static String[] getHyetoTypeAsArray() {

        HyetographGenerator.HyetographShape[] hydroTypes = HyetographGenerator.HyetographShape.values();
        String[] values = new String[hydroTypes.length];

        for (int n = 0; n < hydroTypes.length; n++) {
            values[n] = hydroTypes[n].toString().toLowerCase(Locale.ENGLISH);
        }

        return values;
    }
    
    public HyetographGenerator.HyetographShape getHyetoShape() {
        return hyetoShape;
    }
    
    public void setHyetoType(HyetographGenerator.HyetographShape hyetoShape) {
        this.hyetoShape = hyetoShape;
    }
    
    public Double getHyetoPeakPosition() {
        return hyetoPeakPosition;
    }

    public void setHyetoPeakPosition(Double hyetoPeakPosition) {
        this.hyetoPeakPosition = hyetoPeakPosition;
    }

    public Double getRainfallStep() {
        return rainfallStep;
    }

    public void setRainfallStep(Double rainfallStep) {
        this.rainfallStep = rainfallStep;
    }

    public Double getRainfallRecessionValue() {
        return rainfallRecessionValue;
    }

    public void setRainfallRecessionValue(Double rainfallRecessionValue) {
        this.rainfallRecessionValue = rainfallRecessionValue;
    }

    public Double getRainfallRecessionThreshold() {
        return rainfallRecessionThreshold;
    }

    public void setRainfallRecessionThreshold(Double rainfallRecessionThreshold) {
        this.rainfallRecessionThreshold = rainfallRecessionThreshold;
    }

    public RainfallRecession getRainfallRecession() {
        return new RainfallRecession(rainfallRecessionValue, rainfallRecessionThreshold);
    }

    public InitialAbstraction getInitialAbstraction() {
        return new InitialAbstraction(initAbsUnit, initialAbstractionValue);
    }

    public void setInitialAbstractionValue(Double initialAbstractionValue) {
        this.initialAbstractionValue = initialAbstractionValue;
    }

    public Double getInitialAbstractionValue() {
        return initialAbstractionValue;
    }

    public InitialAbstraction.AbstractionUnits getInitAbsUnit() {
        return initAbsUnit;
    }

    public void setInitAbsUnit(InitialAbstraction.AbstractionUnits initAbsUnit) {
        this.initAbsUnit = initAbsUnit;
    }
   
    //hydrograph
    public Double getHydroStepOutput() {
        return hydroStepOutput;
    }

    public void setHydroStepOutput(Double hydroStepOutput) {
        this.hydroStepOutput = hydroStepOutput;
    }

    public Double getAmcValue() {
        
        if(amcType == Amc_Type.VARIABLE) {
            if(returnPeriod > 0) {
                amcValue = 2.15 + 0.2*Math.log(returnPeriod);
            }
        }
        
        return amcValue;
    }

    public void setAmcValue(Double amcValue) {
        this.amcValue = amcValue;
    }
    
    public Amc_Type getAmcType() {
        return amcType;
    }

    public void setAmcType(Amc_Type amcType) {
        this.amcType = amcType;
    }

    public BaseflowParams.BaseflowType getBaseFlowType() {
        return baseFlowType;
    }

    public void setBaseFlowType(BaseflowParams.BaseflowType baseFlowType) {
        this.baseFlowType = baseFlowType;
    }
    
    public Double getQ0() {
        return q0;
    }

    public void setQ0(Double q0) {
        this.q0 = q0;
    }

    public boolean isQ0Auto() {
        return baseFlowAuto;
    }

    public void setQ0Auto(boolean q0Auto) {
        this.baseFlowAuto = q0Auto;
    }
    
    public double getAutoQ0() {
        return 0.05;
    }
    
    public Double getBasefklowRecession() {
        return baseflowRecession;
    }

    public void setBaseflowRecession(Double baseflowRecession) {
        this.baseflowRecession = baseflowRecession;
    }

    public Double getArfValue() {
        return arfValue;
    }

    public void setArf(Double arf) {
        this.arfValue = arf;
    }
    
    public boolean isArfAuto() {
        return arfAuto;
    }

    public void setArfAuto(boolean arfAuto) {
        this.arfAuto = arfAuto;
    }
    
    public double getAutoArf() {
        return -1d;
    }    

    public Double getPeakFraction() {
        return peakFraction;
    }

    public void setPeakFraction(Double peakFraction) {
        this.peakFraction = peakFraction;
    }

    public Double getGeomorphFactor() {
        return geomorphFactor;
    }

    public void setGeomorphFactor(Double geomorphFactor) {
        this.geomorphFactor = geomorphFactor;
    }

    public Double getGeomorphoFactorThreshold() {
        return geomorphoFactorThreshold;
    }

    public void setGeomorphoFactorThreshold(Double thresholdGeomorphFactor) {
        this.geomorphoFactorThreshold = thresholdGeomorphFactor;
    }

    public Geomorphology getGeomorphology() {
        return new Geomorphology(geomorphFactor, geomorphoFactorThreshold);
    }

    public Double getSubSurfaceDrainageLoss() {
        return subSurfaceDrainageLoss;
    }

    public void setSubSurfaceDrainageLoss(Double subSurfaceDrainageLoss) {
        this.subSurfaceDrainageLoss = subSurfaceDrainageLoss;
    }

    public BaseflowParams getBaseflowParams() {
        if(q0 == -1) return new BaseflowParams(baseFlowType, getAutoQ0(), baseflowRecession * 1E-6);
        else return new BaseflowParams(baseFlowType, q0, baseflowRecession * 1E-6);
    }
    
    public void deleteData() {

        projectFolder = null;
        projectName = null;
        outletElevation = 0.0;
        catchmentArea = 0.0;
        outletXCoord = 0.0;
        outletYCoord = 0.0;
        aParam = 0.0;
        aFile = null;
        nParam = 0.0;
        nFile = null;
        lsppModel = null;
        cvFile = null;
        historicalRainfallFile = null;
        nLessHourParam = 0.0;
        nLessHourParamAuto = true;
        
    }

    public File getOutputFolder() {
        return new File(projectFolder + File.separator + outputFolderName);
    }
    
    public File getDemMaskFile(){
        return new File(getOutputFolder().getAbsoluteFile() + File.separator + "demMask.tif");
    }

    public void setLsppCalculator (LsppCalculator lsppCalculator) {
        this.lsppCalculator = lsppCalculator;
    }

    public LsppCalculator getLsppCalculator(){
        return lsppCalculator;
    }


    private File projectFolder;
    private String projectName;
    private Double outletElevation = 0.0;
    private Double catchmentArea = 0.0;
    private Double outletXCoord = 0.0;
    private Double outletYCoord = 0.0;
    private RainfallType rainfallType;
    private LsppModel lsppModel;
    private File aFile;
    private File nFile;
    private File cvFile;
    private Integer returnPeriod = 0;
    private Double aParam = 0.0;
    private Double nParam = 0.0;
    private Double nLessHourParam = 0.366;
    private boolean nLessHourParamAuto = true;
    private File historicalRainfallFile;
    private Double maxSlopeVelocity = 0.05;
    private Double minSlopeVelocity = 0.025;
    private Double slopeCost = 1.0;
    private Double channelVelocity = 2.0;
    private Double minThreshold = 0.0;
    private Double maxThreshold = 0.01;
    private Double thresholdCost = 1.0;
    private Double hyetoPeakPosition = 0.5;
    private Double rainfallStep = 5.0;
    private Double rainfallRecessionValue = 0.0;
    private Double rainfallRecessionThreshold = 0.0;
    private Double initialAbstractionValue = 0.1;
    private Double slopeVelocity = 0.025;
    private Double thresholdValue = 0.02;
    private Double hydroStepOutput = 5.0;
    private Double amcValue = 3.0;
    private Double q0 = -1d;
    private boolean baseFlowAuto = true;
    private Double baseflowRecession = 6.0;
    private Double arfValue = -1d;
    private boolean arfAuto = true;
    private Double peakFraction = 0.9;
    private Double geomorphFactor = 0.8;
    private Double geomorphoFactorThreshold = 20.0;
    private Double subSurfaceDrainageLoss = 0.0;
    private InitialAbstraction.AbstractionUnits initAbsUnit = InitialAbstraction.AbstractionUnits.FRACTION;
    private BaseflowParams.BaseflowType baseFlowType = BaseflowParams.BaseflowType.LUMPED;
    private DoubleBasicGrid watershedGrid;
    private HyetographGenerator.HyetographShape hyetoShape = HyetographGenerator.HyetographShape.WALLINGFORD;
    private LsppCalculator lsppCalculator = null;

    public enum KinematicsType {

        SIMPLE, ADVANCED
    }
    
    private KinematicsType kinematicsType = KinematicsType.SIMPLE;

    public enum Amc_Type {

        COSTANT, VARIABLE
    }
    
    private Amc_Type amcType = Amc_Type.COSTANT;

    public enum RainfallType {

        POINT, DISTRIBUTED, HISTORICAL
    }

    public static final String inputFolderName = "01_Input";
    public static final String outputFolderName = "02_Output";
    
    public static final String cnFileName = "cn.tif";
    public static final String demFileName = "demdepit.tif";
    public static final String flowDirFileName = "FlowDir.tif";
    public static final String upslopeAreaFileName = "Upslope.tif";
    public static final String bluelinesFileName = "bluelines.shp";
    public boolean withBluelines;
    
    protected static String PROJECT_PARAMETERS = "ProjectParameters";
    protected static String PROJECT_FOLDER = "ProjectFolder";
    protected static String INPUT = "Input";
    protected static String DEM = "DEM";
    protected static String CN = "CN";
    protected static String BLUELINES = "Bluelines";
    protected static String UPSLOPE_AREA = "UpslopeArea";
    protected static String FLOW_DIR = "FlowDir";
    protected static String OUTLET = "Outlet";
    protected static String XCOORD = "XCoordinate";
    protected static String YCOORD = "YCoordinate";
    protected static String AREA = "Area";
    protected static String ELEVATION = "Elevation";
    protected static String RAINFALL = "Rainfall";
    protected static String TYPE = "Type";
    protected static String MODEL = "Model";
    protected static String A_FILE = "AParamFile";
    protected static String N_FILE = "NParamFile";
    protected static String CV_FILE = "CVParamFiler";
    protected static String RETURN_TIME = "ReturnTime";
    protected static String N_LESS_HOUR = "NLessHour";
    protected static String A_PARAM = "AParam";
    protected static String N_PARAM = "NParam";
    protected static String HISTORICAL_FILE = "HistoricalFile";
    protected static String ADVANCED_PARAMS = "AdvancedParams";
    protected static String KINEMATICS = "Kinematics";
    protected static String KINEMATICS_TYPE = "KinematicsType";
    protected static String SLOPE_VELOCITY = "SlopeVelocity";
    protected static String CHANNEL_VELOCITY = "ChannelVelocity";
    protected static String THRESHOLD = "Threshold";
    protected static String SLOPE_COST = "SlopeCost";
    protected static String MIN_SLOPE_VELOCITY = "MinVelocity";
    protected static String MAX_SLOPE_VELOCITY = "MaxVelocity";
    protected static String MIN_THRESHOLD = "MinThreshold";
    protected static String MAX_THRESHOLD = "MaxThreshold";
    protected static String THRESHOLD_COST = "ThresholdCost";
    protected static String ROUTING_TIME = "RoutingTime";
    protected static String HYETOGRAPH = "Hyetograph";
    protected static String HYETO_TYPE = "HyetoType";
    protected static String PEAK = "Peak";
    protected static String RAINFALL_DATA_STEP = "RainfallStep";
    protected static String RAINFALL_RECESSION = "RainfallRecession";
    protected static String THRESH_RAINFALL_RECESSION = "ThresholdRainfallRec";
    protected static String INITIAL_ABSTRACTION = "InitialAbstraction";
    protected static String INITIAL_ABSTRACTION_VALUE = "InitAbstrValue";
    protected static String INITIAL_ABSTRACTION_UNIT = "InitAbstrUnit";
    protected static String HYDROGRAH = "Hydrograph";
    protected static String OUTPUT_STEP = "OutputStep";
    protected static String AMC_VALUE = "AMCValue";
    protected static String AMC_TYPE = "AMCType";
    protected static String BASEFLOW_TYPE = "BaseflowType";
    protected static String BASEFLOW_VALUE = "BaseflowValue";
    protected static String DISCHARGE = "Discharge";
    protected static String ARF = "ARF";
    protected static String FRPM_TC = "FRPM";
    protected static String SUB_SURF_DRAIN_LOSS = "SubDrainLoss";
    protected static String GEOMOPRH_FACTOR = "GeomorphFactor";
    protected static String THRESH_GEOMORPH_FACTOR = "ThresholdGeomFactor";

}
