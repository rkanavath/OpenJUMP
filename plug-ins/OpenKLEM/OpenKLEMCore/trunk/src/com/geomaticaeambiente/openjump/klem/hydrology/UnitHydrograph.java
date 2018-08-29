package com.geomaticaeambiente.openjump.klem.hydrology;

public class UnitHydrograph{
    
    public UnitHydrograph(double[] times, double[] discharges) {

        unitHydrograph = new double[2][discharges.length];
        maxDischarge = 0;
        peakTime = 0;
        volume = 0;

        for(int i=0; i<unitHydrograph[0].length; i++){

            unitHydrograph[0][i] = times[i];
            unitHydrograph[1][i] = discharges[i];

            volume += unitHydrograph[0][i] * unitHydrograph[1][i];

            if(discharges[i] > maxDischarge) {
                maxDischarge = discharges[i];
                peakTime = i * step;
            }
        }            

    }        

    public UnitHydrograph(double step, double[] discharge){

        this.step = step;
        unitHydrograph = new double[2][discharge.length];
        maxDischarge = 0;
        peakTime = 0;
        volume = 0;

        for(int i=0; i<unitHydrograph[0].length; i++){

            unitHydrograph[0][i] = i*step;
            unitHydrograph[1][i] = discharge[i];

            volume += unitHydrograph[0][i] * unitHydrograph[1][i];

            if(discharge[i] > maxDischarge) {
                maxDischarge = discharge[i];
                peakTime = i * step;
            }
        }
    }

    public double[][] getUnitHydrograph() {
        return unitHydrograph;
    }

    public double getMaxDischarge() {
        return maxDischarge;
    }

    public double getDuration() {
        return unitHydrograph.length * step;
    }

    public double getPeakTime() {
        return peakTime;
    }

    public double getVolume() {
        return volume;
    }

    private double step;
    private double[][] unitHydrograph = null;
    private double maxDischarge;
    private double peakTime;
    private double volume;

}
