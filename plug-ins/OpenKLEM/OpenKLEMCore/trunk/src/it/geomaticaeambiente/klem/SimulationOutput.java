package it.geomaticaeambiente.klem;

/**
 * The output from a simulation run.
 * @author AdL
 */
public class SimulationOutput {

    /**
     * Constructs a new simulation output.
     * @param simulationRainfall The simulation hyetograph.
     * @param simulationSteps The simulation steps.
     * @param simulationDischarge The simulation discharge.
     */
    public SimulationOutput(
            SimulationRainfall simulationRainfall,
            SimulationSteps simulationSteps,
            SimulationDischarge simulationDischarge) {
        this.simulationRainfall = simulationRainfall;
        this.simulationSteps = simulationSteps;
        this.simulationDischarge = simulationDischarge;

    }

    public SimulationRainfall getSimulationRainfall() {
        return simulationRainfall;
    }

    public SimulationDischarge getSimulationDischarge() {
        return simulationDischarge;
    }

    public SimulationSteps getSimulationSteps() {
        return simulationSteps;
    }
    
    public static class SimulationSteps {
        
        public SimulationSteps(int directStepsCount, int baseStepsCount, int totalStepsCount) {
            this.directStepsCount = directStepsCount;
            this.baseStepsCount = baseStepsCount;
            this.totalStepsCount = totalStepsCount;
        }

        public int getDirectStepsCount() {
            return directStepsCount;
        }

        public int getBaseStepsCount() {
            return baseStepsCount;
        }

        public int getTotalStepsCount() {
            return totalStepsCount;
        }

        private int directStepsCount = 0;
        private int baseStepsCount = 0;
        private int totalStepsCount = 0;
        
    }
    
    public static class SimulationRainfall {

        public SimulationRainfall(
                TimeInterval timeInterval,
                double[] totalRain, double[] effectiveRain, double[] outputStore,
                double cumulativeTotalRain, double cumulativeEffectiveRain, double cumulativeStoreRain) {
            this.timeInterval = timeInterval;
            this.totalRain = totalRain;
            this.effectiveRain = effectiveRain;
            this.outputStore = outputStore;
            this.cumulativeTotalRain = cumulativeTotalRain;
            this.cumulativeEffectiveRain = cumulativeEffectiveRain;
            this.cumulativeStoreRain = cumulativeStoreRain;
            
        }

        public TimeInterval getTimeInterval() {
            return timeInterval;
        }
        
        public double[] getTotalRain() {
            return totalRain;
        }

        public double[] getEffectiveRain() {
            return effectiveRain;
        }

        public double[] getOutputStore() {
            return outputStore;
        }
        
        public double getCumulativeTotalRain() {
            return cumulativeTotalRain;
        }

        public double getCumulativeEffectiveRain() {
            return cumulativeEffectiveRain;
        }
        
        public double getCumulativeStoreRain() {
            return cumulativeStoreRain;
        }
        
        private final TimeInterval timeInterval;
        private final double[] totalRain;
        private final double[] effectiveRain;
        private final double[] outputStore;
        private final double cumulativeTotalRain;
        private final double cumulativeEffectiveRain;
        private final double cumulativeStoreRain;
        
    }
    
    public static class SimulationDischarge {

        public SimulationDischarge(TimeInterval timeInterval,
                double[] Dflo_out, double[] Bflo_out,
                Double Dflo_max, Double Bflo_max, Double Tflo_max,
                Double Dflo_med, Double Bflo_med, Double Tflo_med,
                TimeInterval directFlowPeakTime, TimeInterval baseFlowPeakTime, TimeInterval totalFlowPeakTime,
                Double appearingCN, Double contributingAreaPercentage) {
            this.timeInterval = timeInterval;
            this.Dflo_out = Dflo_out;
            this.Bflo_out = Bflo_out;
            this.Tflo_out = calcTotalFlow();
            this.Dflo_max = Dflo_max;
            this.Bflo_max = Bflo_max;
            this.Tflo_max = Tflo_max;
            this.Dflo_med = Dflo_med;
            this.Bflo_med = Bflo_med;
            this.Tflo_med = Tflo_med;
            this.directFlowPeakTime = directFlowPeakTime;
            this.baseFlowPeakTime = baseFlowPeakTime;
            this.totalFlowPeakTime = totalFlowPeakTime;
            this.appearingCN = appearingCN;
            this.contributingAreaPercentage = contributingAreaPercentage;
            
        }

        private double[] calcTotalFlow() {
         
            double[] totalFlow = new double[Dflo_out.length];
            for(int t=0; t<totalFlow.length; t++){
                totalFlow[t] = Dflo_out[t] + Bflo_out[t];
            }
            return totalFlow;
            
        }
        
        public TimeInterval getTimeInterval() {
            return timeInterval;
        }

        public double[] getDflo_out() {
            return Dflo_out;
        }

        public double[] getBflo_out() {
            return Bflo_out;
        }

        public double[] getTflo_out() {
            return Tflo_out;
        }

        public Double getDflo_max() {
            return Dflo_max;
        }

        public Double getBflo_max() {
            return Bflo_max;
        }

        public Double getTflo_max() {
            return Tflo_max;
        }

        public Double getDflo_med() {
            return Dflo_med;
        }

        public Double getBflo_med() {
            return Bflo_med;
        }

        public Double getTflo_med() {
            return Tflo_med;
        }

        public TimeInterval getDirectFlowPeakTime() {
            return directFlowPeakTime;
        }

        public TimeInterval getBaseFlowPeakTime() {
            return baseFlowPeakTime;
        }

        public TimeInterval getTotalFlowPeakTime() {
            return totalFlowPeakTime;
        }

        public Double getAppearingCN() {
            return appearingCN;
        }
        
        public Double getContributingAreaPercentage() {
            return contributingAreaPercentage;
        }
        
        private final TimeInterval timeInterval;
        private final double[] Dflo_out;
        private final double[] Bflo_out;
        private final double[] Tflo_out;      
        private final Double Dflo_max;
        private final Double Bflo_max;
        private final Double Tflo_max;
        private final Double Dflo_med;
        private final Double Bflo_med;
        private final Double Tflo_med;
        private final TimeInterval directFlowPeakTime;
        private final TimeInterval baseFlowPeakTime;
        private final TimeInterval totalFlowPeakTime;        
        private final Double appearingCN;
        private final Double contributingAreaPercentage;
        
    }
    
    private final SimulationRainfall simulationRainfall;
    private final SimulationSteps simulationSteps;
    private final SimulationDischarge simulationDischarge;
    
}
