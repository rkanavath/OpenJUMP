package it.geomaticaeambiente.klem;

import it.geomaticaeambiente.klem.BaseflowParams.BaseflowType;
import it.geomaticaeambiente.klem.HyetographGenerator.HyetographShape;
import it.geomaticaeambiente.klem.TimeInterval.TimeIntervalUnit;

/**
 * The Klem (Kinematic Local Excess Model) calculates for a watershed, given an
 * input rainfall and some ancillary data, the hydrograph.
 * @author AdL
 */
public class Klem {
    
    /**
     * Constructs a Klem modeller that uses a design input rainfall. The rainfall
     * duration is set to the one yielding the most critical hydrograph.
     * @param watershed The watershed.
     * @param hyetoShape The shape of the input hyetograph.
     * @param lsppCalculator The lspp calculator.
     * @param peakFraction The fraction of the hydrograph peak to be cut off.
     * @param inputStep The input time step.
     * @param calculationStep The internal calculation time step.
     * @param outputStep The output time step.
     * @param rainfallPeakPosition The rainfall peak position.
     * @param drainageArea The (optional) drainage area.
     */
    public Klem(
            Watershed watershed,
            HyetographShape hyetoShape, LsppCalculator lsppCalculator, double peakFraction,
            TimeInterval inputStep, TimeInterval calculationStep, TimeInterval outputStep,
            double rainfallPeakPosition, DrainageArea drainageArea) {
        
        this.watershed = watershed;
        this.hyetoShape = hyetoShape;
        this.lsppCalculator = lsppCalculator;
        this.peakFraction = peakFraction;
        this.ciclo_in_min = inputStep;
        this.ciclo_cal_min = calculationStep;
        this.ciclo_out_min = outputStep;
        this.rainfallPeakPosition = rainfallPeakPosition;
        this.drainageArea = drainageArea;       
        
        // Convert cicli cal in minutes        
        modellingType = ModellingType.DESIGN;
        
    }
    
    /**
     * Constructs a Klem modeller that uses an an historical input rainfall.
     * @param hyetograph The input hyetograph.
     * @param watershed The watershed.
     * @param calculationStep The internal calculation time step.
     * @param outputStep The output time step.
     * @param drainageArea The (optional) drainage area.
     */
    public Klem(Hyetograph hyetograph, Watershed watershed,
            TimeInterval calculationStep, TimeInterval outputStep, DrainageArea drainageArea) {
        
        this.inputHyetograph = hyetograph;
        this.watershed = watershed;
        this.ciclo_cal_min = calculationStep;
        this.ciclo_out_min = outputStep;
        this.drainageArea = drainageArea;
        
        modellingType = ModellingType.HYSTORICALRAIN;
        
    }
    
    public void run() throws Exception {

        
        if(modellingType == ModellingType.DESIGN) {
            calcCriticalHydrograph();
        } else if (modellingType == ModellingType.HYSTORICALRAIN) {
            calcHydrographFromGivenRain();
        }      
        
    }
    
    private void calcCriticalHydrograph () throws Exception{
        
        // Find critical duration: use LUMPED baseflow
        BaseflowType inputBaseflowType = watershed.getBaseflowParams().getBaseflowType();
        BaseflowParams bfp = watershed.getBaseflowParams();
        bfp.setBaseflowType(BaseflowParams.BaseflowType.LUMPED);
        watershed.setBaseflowParams(bfp);
        criticalDuration = findCriticalDuration(watershed, ciclo_in_min, ciclo_cal_min,
                ciclo_out_min, lsppCalculator);
        DesignRain designRain = new DesignRain(
                criticalDuration,
                lsppCalculator.getParamsAN(),
                watershed);
        
        // Find lag ------------------------------------------------------------
        if(drainageArea != null && drainageArea.getIntakeHydrograph() != null &&
                drainageArea.getLag() == null){          

            HyetographGenerator hyetoGenerator = new HyetographGenerator(rainfallPeakPosition,
                    ciclo_in_min, designRain);
            Hyetograph lagHyetograph = hyetoGenerator.generateHytegraph(
                    hyetoShape, true);

            FlowModeller flowModeller = new FlowModeller(watershed, ciclo_cal_min, ciclo_out_min);
            flowModeller.modelFlow(lagHyetograph, null, true);

            SimulationOutput hydrograph1 = flowModeller.getSimulationOutputs();

            // Lag in seconds
            TimeInterval hydro1TimeInterval = hydrograph1.getSimulationDischarge().getTimeInterval();            
            int lag = getMaxLoc(hydrograph1.getSimulationDischarge().getTflo_out()) *
                    (int) hydrograph1.getSimulationDischarge().getTimeInterval().getInterval(TimeIntervalUnit.MINUTE) -
                    getMaxLoc(drainageArea.getIntakeHydrograph().getSimulationDischarge().getTflo_out()) *
                    (int) drainageArea.getIntakeHydrograph().getSimulationDischarge().getTimeInterval().getInterval(TimeIntervalUnit.MINUTE);

            drainageArea.setLag(new TimeInterval(lag, TimeIntervalUnit.MINUTE));
                
        }
            
        // Get hydrograph
        bfp.setBaseflowType(inputBaseflowType);
        watershed.setBaseflowParams(bfp);
        
        HyetographGenerator hyetoGenerator = new HyetographGenerator(rainfallPeakPosition,
                ciclo_in_min, designRain);
        inputHyetograph = hyetoGenerator.generateHytegraph(hyetoShape, true);

        FlowModeller flowModeller = new FlowModeller(watershed, ciclo_cal_min, ciclo_out_min);
        flowModeller.modelFlow(inputHyetograph, drainageArea, true);
                
        simulationOutput = flowModeller.getSimulationOutputs();
        if(drainageArea != null) {
            drainageAreaSimOutput = flowModeller.getUptakeSimulationOutput(); // OKKIO
        }
        
    }

    private void calcHydrographFromGivenRain() throws Exception{
        
        criticalDuration = new TimeInterval(
                inputHyetograph.getStep().getInterval(TimeIntervalUnit.HOUR) * inputHyetograph.getStepsCount(),
                TimeInterval.TimeIntervalUnit.HOUR);
        
        // Calc flo_ini
        if(watershed.getBaseflowParams().getInitialBaseflow() == null ||
                watershed.getBaseflowParams().getInitialBaseflow() < 0){
            watershed.getBaseflowParams().setInitialBaseflow(watershed.getArea() * 0.05);
        }

        // Find lag ------------------------------------------------------------
        if(drainageArea != null && drainageArea.getIntakeHydrograph() != null && drainageArea.getLag() == null){
            FlowModeller flowModeller = new FlowModeller(watershed, ciclo_cal_min, ciclo_out_min);
            flowModeller.modelFlow(inputHyetograph, null, true);

            SimulationOutput hydrograph1 = flowModeller.getSimulationOutputs();

            // Convert total flow peak time to minutes
            TimeInterval totalFlowPeakTime1 = flowModeller.getSimulationOutputs().getSimulationDischarge().getTotalFlowPeakTime();            
            int lag = (int) Math.round(totalFlowPeakTime1.getInterval(TimeIntervalUnit.MINUTE) -
                    (getMaxLoc(hydrograph1.getSimulationDischarge().getTflo_out()) - 1) *
                    ciclo_cal_min.getInterval(TimeIntervalUnit.MINUTE) + ciclo_cal_min.getInterval(TimeIntervalUnit.MINUTE));
            
            drainageArea.setLag(new TimeInterval(lag, TimeIntervalUnit.MINUTE));
        }

        FlowModeller flowModeller = new FlowModeller(watershed, ciclo_cal_min, ciclo_out_min);
        flowModeller.modelFlow(inputHyetograph, drainageArea, true);

        simulationOutput = flowModeller.getSimulationOutputs();
	if(drainageArea != null) {	
            drainageAreaSimOutput = flowModeller.getUptakeSimulationOutput();
        }  
        
    }

    private TimeInterval findCriticalDuration(Watershed watershed,
            TimeInterval ciclo_in,
            TimeInterval ciclo_cal_min,
            TimeInterval ciclo_out_min,
            LsppCalculator lsppCalculator) throws Exception{

        double durata;
        int imaxval = 0;

        double[] Tflo_max = new double[1000]; // TODO: softcode array length

        /* In the quest for critical duration, baseflow can be ignored only
         * when AMC is big, i.e. when the relative contribution of base flow is
         * low.
         */
        boolean calcBaseflow = watershed.getAMC() < 2;        
        
        // Orario su vettore di 15' --------------------------------------------
        int istart = 4;
        int ipasso = 4;
        
        int ii;
        for(ii = istart; ii<1000; ii+=ipasso){
            durata = ((double)ii/4);
            Tflo_max[ii] = findPeakTime(
                    durata,
                    lsppCalculator,
                    watershed,
                    ciclo_in,
                    calcBaseflow,
                    ciclo_cal_min,
                    ciclo_out_min,
                    HyetographShape.CONSTANT);
            
            if(Tflo_max[ii] < Tflo_max[ii - ipasso]) {
                break;
            }
        }
        
        // 30' su vettore di 15' -----------------------------------------------
        ipasso = 2;
        istart = Math.max(ipasso, (ii-6));
        for(ii = istart; ii<1000; ii+=ipasso){
            durata = ((double)ii/4);
            if(Tflo_max[ii] <= 0.01){

                Tflo_max[ii] = findPeakTime(
                        durata,
                        lsppCalculator,
                        watershed,
                        ciclo_in,
                        calcBaseflow,
                        ciclo_cal_min,
                        ciclo_out_min,
                        HyetographShape.CONSTANT);
                
            }
            
            if(Tflo_max[ii] < Tflo_max[ii - ipasso]) {
                break;
            }
        }

        // 15' su vettore di 15' -----------------------------------------------
        ipasso = 1;
        istart = Math.max(ipasso, (ii-3));
        for(ii = istart; ii<1000; ii+=ipasso){
            durata = ((double)ii/4);
//            System.out.println("Durata: " + durata);
            if(Tflo_max[ii] <= 0.01){

                Tflo_max[ii] = findPeakTime(
                    durata,
                    lsppCalculator,
                    watershed,
                    ciclo_in,
                    calcBaseflow,
                    ciclo_cal_min,
                    ciclo_out_min,
                    HyetographShape.CONSTANT);
                
            }
            if(Tflo_max[ii] < Tflo_max[ii - ipasso]){
                imaxval = ii-ipasso;
                break;
            }
        }

        double criticPeak = peakFraction * Tflo_max[imaxval];

        // Trova posizione di critical peak ridotto
        int inf = 0;
        int sup = imaxval;
        for(int i=0; i<imaxval; i++) {
            if(Tflo_max[i] > 0.01 && Tflo_max[i] < criticPeak) {
                inf = i;
            }
        }
        for(int i=imaxval; i>=0; i--) {
            if(Tflo_max[i] > 0.01 && Tflo_max[i] > criticPeak) {
                sup = i;
            }
        }
        
        // Verifica che entro le posizioni tutti i valori siano calcolati
        for(int i=inf; i<sup; i++) {
            if(i == 0) {
                continue;
            }
            
            if(Tflo_max[i] ==0) {
                durata = ((double)i/4);
                Tflo_max[i] = findPeakTime(
                    durata,
                    lsppCalculator,
                    watershed,
                    ciclo_in,
                    calcBaseflow,
                    ciclo_cal_min,
                    ciclo_out_min,
                    HyetographShape.CONSTANT);
            }
        }
        
        // New max and min
        int jinf3 = 0;
        for(int jinf = imaxval; jinf>=0; jinf--){
            jinf3 = jinf;
            if(Tflo_max[jinf] > 0.01 && Tflo_max[jinf] < criticPeak) break;
        }

        int jsup3 = 0;
        for(int jsup = jinf3+1; jinf3<imaxval; jsup++){   // OKKIO ai contatori 0,1
            jsup3 = jsup;
            if(Tflo_max[jsup] > 0.01) break;
        }        
        
        double deltaT = (jsup3-jinf3) * 0.25;
        double deltaQ = Tflo_max[jsup3] - Tflo_max[jinf3];

        durata = (jinf3 * 0.25) + (criticPeak - Tflo_max[jinf3]) * deltaT / deltaQ;
        durata = Math.round(durata*ciclo_in.getInterval(TimeIntervalUnit.MINUTE)) /
                ciclo_in.getInterval(TimeIntervalUnit.MINUTE);

        if(Utils.DEBUG) {
            int lastNonZero = Tflo_max.length-1;
            for(int t=Tflo_max.length-1; t>=0; t--) {
                if(Tflo_max[t] != 0) {
                    lastNonZero = t;
                    break;
                }
            }
            
            System.out.println("T[min],Q[m3/s]");
            for(int t=0; t<=lastNonZero; t++) {
                System.out.println(t*15 + "," + Tflo_max[t]);
            }
            
        }
        
        
        return new TimeInterval(durata, TimeIntervalUnit.HOUR);
    }


    private double getArrayMaxVal(double[] arrayIn){

        double maxVal = -Double.MAX_VALUE;
        for(int p=0; p<arrayIn.length; p++){
            if(arrayIn[p] > maxVal) maxVal = arrayIn[p];
        }

        return maxVal;
    }
 

    private double findPeakTime(double durata, LsppCalculator lsppCalculator, Watershed watershed,
            TimeInterval ciclo_in, boolean calcBaseflow, TimeInterval ciclo_cal, TimeInterval ciclo_out,
            HyetographShape hyetoShape) throws Exception {
        
        DesignRain designRain = new DesignRain(
                    new TimeInterval(durata, TimeIntervalUnit.HOUR),
                    lsppCalculator.getParamsAN(),
                    watershed);
        HyetographGenerator hyetoGenerator = new HyetographGenerator(
                rainfallPeakPosition, ciclo_in, designRain);
        Hyetograph hyeto = hyetoGenerator.generateHytegraph(hyetoShape, true);

        FlowModeller flowModeller = new FlowModeller(watershed, ciclo_cal, ciclo_out);
        flowModeller.modelFlow(hyeto, null, calcBaseflow);

        return getArrayMaxVal(flowModeller.getSimulationOutputs()
                .getSimulationDischarge().getTflo_out());
        
    }    
    
    private int getMaxLoc(double[] inArray){

        double val = -Double.MAX_VALUE;
        int pos = 0;
        for(int p=0; p<inArray.length; p++)
            if(inArray[p] > val){
                val = inArray[p];
                pos = p;
            }
        return pos;
    }    
    
    /**
     * Returns the simulation watershed.
     * @return The simulation watershed.
     */
    public Watershed getWatershed() {
        return watershed;
    }
    
    /**
     * Return the simulation output: the hydrograph and ancillary results.
     * @return The simulation output.
     */
    public SimulationOutput getSimulationOutput() {
        return simulationOutput;
    }
    
    /**
     * Return the simulation output for the (optional) drainage area: the
     * hydrograph and ancillary results.
     * @return The simulation output for the drainage area.
     */
    public SimulationOutput getDrainageAreaSimulationOutput(){
        return drainageAreaSimOutput;
    }
    
    /**
     * Returns the input hydrograph used to generate the outputs.
     * @return The input hydrograph.
     */
    public Hyetograph getInputHyetograph() {
        return inputHyetograph;
    }

    /**
     * Returns the duration of the most critical rainfall. 
     * @return The critical duration.
     */
    public TimeInterval getCriticalDuration() {
        return criticalDuration;
    }    
    
    private HyetographShape hyetoShape;
    private LsppCalculator lsppCalculator;
    private TimeInterval ciclo_in_min;
    private final TimeInterval ciclo_cal_min;
    private final TimeInterval ciclo_out_min;
    private final DrainageArea drainageArea;    
    private final ModellingType modellingType;
    private Hyetograph inputHyetograph = null;
    private Watershed watershed = null;
    private SimulationOutput simulationOutput = null;
    private SimulationOutput drainageAreaSimOutput = null; 
    private TimeInterval criticalDuration;
    
    
    private double rainfallPeakPosition = 0.5;

    private double peakFraction = 0;
    
    public static final int MAXDATA = 50000;

    private enum ModellingType {
        
        DESIGN, HYSTORICALRAIN;
        
    }
    
}
