/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package it.geomaticaeambiente.klem;

import static it.geomaticaeambiente.klem.ParallelFlowModeller.MAXDATA;
import java.util.Arrays;
import java.util.concurrent.Callable;

/**
 *
 * @author deluca
 */
public class FlowModellerWorker implements Callable<Integer> {

    public FlowModellerWorker(int workerID, ParallelFlowModeller caller, int startRow, int rowCount) {
        this.workerID = workerID;
        this.caller = caller;
        this.startRow = startRow;
        this.rowCount = rowCount;
    }

    @Override
    public Integer call() throws Exception {
        
        System.out.println(workerID + " is working.");
        
        /* Cycle through cells */
        double[] Peff_cal_cum = new double[MAXDATA];
        double[] Peff_cal = new double[MAXDATA];
        double[] Pinf_cel = new double[MAXDATA];
        double[] Dflo_cal = new double[MAXDATA];
        
        Watershed watershed = caller.getWatershed();
        double basin_area = caller.getBasinArea();
        double cellArea = caller.getCellArea();
        
        double[][] scs = watershed.getRasCurveNumber();
        double[][] routingTime = watershed.getRasRoutingTime();
        
        int nCols = scs[0].length;
        double noData = watershed.getNoData();
        DrainageArea drainageArea = caller.getDrainageArea();
        
        double initialQin = watershed.getBaseflowParams().getInitialBaseflow() / (basin_area * 1E6 / cellArea); // [m3s-1]
        
        double A_CN_adj = 2.08454 * Math.exp(watershed.getAMC() *
                0.80709) - 0.47225;
        double B_CN_adj = (A_CN_adj - 4.2)/100. - 0.058;
        
        double[] pTotCal = caller.getPTotCal();
        int n_cic_cal = caller.getNCicCal();
        
        double rainfallRecessionThreshold = 0;
        double rainfallRecession = 0;
        if(watershed.getRainfallRecession() != null) {
            rainfallRecessionThreshold = watershed.getRainfallRecession().getRainfallRecessionThreshold();
            rainfallRecession = watershed.getRainfallRecession().getRainfallRecession();
        }
        
        double cicloCal_s = caller.getCicloCalS();
        
        double baseflowRecession = watershed.getBaseflowParams().getRecession(); // [s-1]
        double subsurfaceDrainageLoss = watershed.getSubsurfaceDrainLoss(); // [-]  
        
        for(int row = startRow; row<startRow+rowCount; row++){
            //if(row%10 == 0) System.out.println(row + "/" + nRows);
            for(int col=0; col<nCols; col++){
                
                double qmax = -9e9;
                int num_def = 0;
                if (scs[row][col] != noData && scs[row][col] != 0.0 &&
                        routingTime[row][col] != noData){
                    
                    // Count Drainage Areas cells
                    if(drainageArea != null && drainageArea.getRasDrainageAreas() != null
                            && drainageArea.getUptakeFraction() != 0
                            && drainageArea.getRasDrainageAreas()[row][col] == 1){
                        caller.addDrainageAreaCellCount(1);
                    }						
                    
                    double CN  = (scs[row][col] * A_CN_adj) / (10 + B_CN_adj * scs[row][col]);
                    if(CN < 0.0 || CN > 100) {
                        throw new Exception("CN < 0");
                    }
                    
                    double S = 25.4 * (1000./ CN  - 10.); // [mm]
                    if(CN == 0) {
                        S = Double.MAX_VALUE;
                    }

                    // Take care of initial abstraction
                    double abstraction = 0;
                    if(watershed.getInitialAbstraction().getAbstractionUnits() == InitialAbstraction.AbstractionUnits.FRACTION) {
                        abstraction = watershed.getInitialAbstraction().getAbstractionValue() * S;
                    } else if(watershed.getInitialAbstraction().getAbstractionUnits() == InitialAbstraction.AbstractionUnits.MILLIMETERS) {
                        abstraction = watershed.getInitialAbstraction().getAbstractionValue();
                    }
                    double tmp = abstraction;
                    
                    double[] pTotCal_ia = Arrays.copyOf(pTotCal, n_cic_cal+1); // [mm]
                    for(int jp=0; jp<n_cic_cal; jp++) {
                        if(pTotCal[jp] < tmp) {
                            tmp = tmp - pTotCal_ia[jp];
                            pTotCal_ia[jp] = 0;
                        } else {
                            pTotCal_ia[jp] -= tmp;
                            break;
                        }
                    }

                    // Calculate effective and infiltrated rainfall
                    double p_netta = 0;                    
                    for(int jp=0; jp<=n_cic_cal; jp++){
                        
                        // Runoff reduction parameter      
                        if(pTotCal_ia[jp] < rainfallRecessionThreshold *
                                caller.getHyetograph().getStep().getInterval(TimeInterval.TimeIntervalUnit.HOUR)) { // [mm] // OKKIO: hours?
                            p_netta = p_netta + pTotCal_ia[jp] - p_netta * rainfallRecession * caller.getCicloCalH();
                            if(p_netta < 0) {
                                p_netta = 0;
                            }
                        } else {                            
                            p_netta += pTotCal_ia[jp];
                        }
                        
                        if (p_netta > 0.0){
                            // Explicit account for S==0 and S==100 to avoid rounding errors
                            if(S == 0) {
                                Peff_cal_cum[jp] = p_netta; // [mm]
                            } else if(S == 100) {
                                Peff_cal_cum[jp] = 0;
                            } else {
                                Peff_cal_cum[jp] = (p_netta * p_netta) / (p_netta + S);
                            }
                            num_def++;
                        }else{
                            if(jp==0)  {
                                Peff_cal_cum[jp] = 0;
                            } else {
                                Peff_cal_cum[jp] = Peff_cal_cum[jp-1];
                            }
                        }
                        
                        if(jp==0) {
                            Peff_cal[jp] = 0;
                        } else {
                            Peff_cal[jp] = Peff_cal_cum[jp] - Peff_cal_cum[jp-1]; // [mm]
                        }
                        if(Peff_cal[jp] < 0) {
                            Peff_cal[jp] = 0.0;
                        }
                        
                        caller.addToPeffBac(jp, Peff_cal[jp]); // mm
                        Pinf_cel[jp] = pTotCal_ia[jp] - Peff_cal[jp]; // mm
                        caller.addToPinfBac(jp, Pinf_cel[jp]); // mm
                        
                    }
                    caller.addToNumCel(1);
                        
                    if (num_def > 0.) {
                        caller.addToNumContrib(1);
                    }

                    // Direct flow: routing to outlet                    
                    double routingTime_s = routingTime[row][col] * 3600.; // [s]
                    for(int jp=0; jp<=n_cic_cal; jp++){
                        
                        if(Peff_cal_cum[jp] <= 0) {
                            continue;
                        }
                        
                        int itime = (int)Math.round(routingTime_s / cicloCal_s) + jp;

                        // From runoff [mm] to discharge [m3s-1]
                        double qq = (Peff_cal[jp] * cellArea * 1e-3) / cicloCal_s; // [m3s-1]
                        
                        if (qq > qmax) {
                            qmax = qq;
                        }

                        // Acounts for intake from drainage areas
                        if(drainageArea != null && drainageArea.getIntakeHydrograph() != null &&
                                row==drainageArea.getIntakePoint().y &&
                                col==drainageArea.getIntakePoint().x){
                            qq += caller.getTFlowDA()[jp];
                        }                        
                        
                        // Accounts for uptake from areas uptake
                        if(drainageArea != null && drainageArea.getRasDrainageAreas() != null &&
                                drainageArea.getUptakeFraction() !=0){
                            if(drainageArea.getRasDrainageAreas()[row][col] == 1){
                                Dflo_cal[itime] += (1. - drainageArea.getUptakeFraction()) * qq;
                                caller.addToDflowCalDA(itime, drainageArea.getUptakeFraction() * qq);
                            }else{
                                Dflo_cal[itime] = Dflo_cal[itime] + qq;
                            }
                        }else{
                            Dflo_cal[itime] = Dflo_cal[itime] + qq;
                        }    
                    }

                    // Distributed baseflow
                    if(watershed.getBaseflowParams().getBaseflowType() == BaseflowParams.BaseflowType.DISTRIBUTED) {
                        double qin = initialQin; // [m3s-1]
                        double Galli_Flo = 2d/1000d * basin_area / (basin_area * 1E6 / cellArea); //2E-3 * basin_area / (basin_area * 1E6 / cellArea); // 2m3/s/km2
                        // Initial baseflow
                        if(caller.calcBaseFlow()) {
                            
                            double[] Bflo_cal_temp = new double[MAXDATA];
                            for(int jp=0; jp<MAXDATA-10000; jp++){
                                
                                double qu = (Pinf_cel[jp] * cellArea * 1e-3) / cicloCal_s; // [m3s-1]
                                double Bflo_cal_pre = Bflo_cal_temp[jp];
                                Bflo_cal_temp[jp] = qin * Math.exp(-baseflowRecession * cicloCal_s) + (1. - Math.exp(-baseflowRecession * cicloCal_s)) * qu; // [m3s-1]

                                qin = Bflo_cal_temp[jp]; // [m3s-1]

                                Bflo_cal_temp[jp] = Bflo_cal_pre + (1 - subsurfaceDrainageLoss) * Bflo_cal_temp[jp]; // [m3s-1]

                                int itime = (int)(routingTime_s / cicloCal_s) + (jp-1); //(int)Math.round(tempo / cicloCal_s) + (jp-1);
                                // TODO
                                if (jp >= n_cic_cal && qin < (watershed.getBaseflowParams().getInitialBaseflow() / (basin_area * 1E6 / cellArea))) { // && Dflo_cal[itime] == 0) {
                                //if(jp > n_cic_cal && Dflo_cal[itime] == 0 && Bflo_cal[jp] < Galli_Flo) {
                                    caller.updateNCicBas(jp);
                                    break;
                                }
                            }
                            
                            for(int jp=0; jp<Bflo_cal_temp.length; jp++) {
                                Bflo_cal[jp] += Bflo_cal_temp[jp];
                            }
                        }
                    }
                }                
            }  
        } // End of cell cycles
        
        /* Update shared variables */
        for(int jp=0; jp<Bflo_cal.length; jp++) {
            caller.addToBflowCal(jp, Bflo_cal[jp]);
        }
        
        for(int jp=0; jp<Dflo_cal.length; jp++) {
            caller.addToDfloCal(jp, Dflo_cal[jp]);
        }
        
        return 0;
        
    }
    
    private int workerID;
    private final ParallelFlowModeller caller;
    
//    private double basin_area;
//    private int nRows;
//    private int nCols;
//    private double[][] scs;
//    private double[][] routingTime;
//    private double noData;
//    
//    private DrainageArea drainageArea;
//    
//    private double A_CN_adj;
//    private double B_CN_adj;
//    private Watershed watershed;
//    
//    private double[] pTotCal;
//    private int n_cic_cal;
// 
//    private double rainfallRecessionThreshold;
//    private double rainfallRecession;
//    private Hyetograph hyetograph;
//    
//    private double cicloCal_h;
//    private int cicloCal_s;
//    private double cellArea;
//    
//    private double[] Tflo_DA;
//    
//    private boolean calcBaseFlow;
//    private double baseflowRecession;
//    private double subsurfaceDrainageLoss;
//    
    private final double[] Bflo_cal = new double[MAXDATA];
    private int startRow;
    private int rowCount;
    
    
}
