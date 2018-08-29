package it.geomaticaeambiente.klem;

import it.geomaticaeambiente.klem.SimulationOutput.SimulationDischarge;
import it.geomaticaeambiente.klem.SimulationOutput.SimulationRainfall;
import it.geomaticaeambiente.klem.SimulationOutput.SimulationSteps;
import java.util.Arrays;

/**
 * The flow modeller is in charge of calculating an hydrograph, given a watershed,
 * an hyetograph, and some ancillary data.
 * @author AdL
 */
public class FlowModeller {

    /**
     * Constructs a flow modeler, that is able to generate an hydrograph
     * for the given watershed.
     * @param watershed The watershed where the hydrograph is calculated.
     * @param internalTimeStep The internal time step used by the model (usually:
     * 5 mins)
     * @param outputTimeStep  The output time step (usually: from 5 to 15 mins)
     */
    public FlowModeller(Watershed watershed,
            TimeInterval internalTimeStep,
            TimeInterval outputTimeStep) {
        
        this.watershed = watershed;
        
        this.cicloCal_s = internalTimeStep.getInterval(TimeInterval.TimeIntervalUnit.SECOND); // [s]
        this.cicloCal_h = internalTimeStep.getInterval(TimeInterval.TimeIntervalUnit.HOUR); // [h]
        this.cicloOut_s = (int)Math.round(outputTimeStep.getInterval(TimeInterval.TimeIntervalUnit.SECOND)); // [s]
        
    }
    
    /**
     * Generates the Hydrograph.
     * @param hyetograph The input hyetograph 
     * @param drainageArea An optional drainage area, taking discharge off from
     * the watershed (can be null)
     * @param calcBaseFlow Indicates whether the base flow should be calculated
     * or not.
     * @throws Exception 
     */
    public void modelFlow(Hyetograph hyetograph, DrainageArea drainageArea,
            boolean calcBaseFlow) throws Exception{
        
        int ncicpr = hyetograph.getStepsCount();
        TimeInterval cicloInInterval = hyetograph.getStep();
        
        // Convert interval to seconds
        double cicloIn_s = hyetograph.getStep().getInterval(TimeInterval.TimeIntervalUnit.SECOND); //* 60; // [min] > [s]

        // Retrieve variables and convert them to modelling units: s, m, 
//        double initialAbstraction = watershed.getInitialAbstraction(); // [-]
        double rainfallRecessionThreshold = 0;
        double rainfallRecession = 0;
        if(watershed.getRainfallRecession() != null) {
            rainfallRecessionThreshold = watershed.getRainfallRecession().getRainfallRecessionThreshold();
            rainfallRecession = watershed.getRainfallRecession().getRainfallRecession();
        }
        
        double baseFlowRecession = watershed.getBaseflowParams().getRecession(); // [s-1]
        double subsurfaceDrainageLoss = watershed.getSubsurfaceDrainLoss(); // [-]        
        
        double A_CN_adj = 2.08454 * Math.exp(watershed.getAMC() * 0.80709) - 0.47225;
        double B_CN_adj = (A_CN_adj - 4.2)/100. - 0.058;

        double D_tottim = (int)Math.round(((ncicpr*cicloIn_s/cicloCal_s)+1) * cicloCal_s); // TODO: rifinire con CZ

        //D_tottim = (int4(N_IMM_PIOGGIA*Ciclo_inp/Ciclo_cal) +1) * Ciclo_cal ! Calcolo la durata della precipitazione in cicli di calcolo !CHECK: perchè +1?! '+1) * Ciclo_cal'
        double[] pTotInp;
        double[] pTotCal = new double[MAXDATA];
        double[] pTotCal_ia_tot = new double[MAXDATA];
        double[] pTotOut = new double[MAXDATA];
        double[] Peff_cal = new double[MAXDATA];
        double[] Peff_cal_cum = new double[MAXDATA];
        double[] Pinf_cel = new double[MAXDATA];
        double[] Peff_bac = new double[MAXDATA];
        double[] Pinf_bac = new double[MAXDATA];
        double[] Peff_out = new double[MAXDATA];
        double[] Pinf_out = new double[MAXDATA];
        double[] Dflo_cal = new double[MAXDATA];
        double[] DFlo_calDA = new double[MAXDATA];
        double[] Bflo_cal = new double[MAXDATA];
        
        double[] Dflo_out = new double[MAXDATA];
        double[] Stor_out = new double[MAXDATA];
        double[] Bflo_out = new double[MAXDATA];
        double[] Tflo_out = new double[MAXDATA];

        double B_pektim = 0;
        double D_pektim = 0;
        double T_pektim = 0;
        
        pTotInp = Arrays.copyOf(hyetograph.getRainfall(), MAXDATA);

        // Distribute rain on calculation time step
        
        int n_cic_cal = 0;
        for(double time = cicloCal_s; time <= D_tottim; time += cicloCal_s) {
            int ipnt = (int) Math.floor(time / cicloIn_s - 0.000001);
            pTotCal[n_cic_cal++] = pTotInp[ipnt] * (cicloCal_s / cicloIn_s); // [mm]
        }
        
        n_cic_cal = (int) (D_tottim/cicloCal_s);

        // Takes care of input hydrgraph from drainage areas
        double[] Tflo_DA = null;
        if(drainageArea != null && drainageArea.getIntakeHydrograph() != null){
            
            SimulationOutput intakeHydrograph = drainageArea.getIntakeHydrograph();
            
            // Rescaling of intake hydrograph ----------------------------------
            Tflo_DA = new double[FlowModeller.MAXDATA];
            
            TimeInterval intakeTimeInterval = intakeHydrograph.getSimulationDischarge().getTimeInterval();            
            int intakeStepSec = (int) intakeTimeInterval.getInterval(TimeInterval.TimeIntervalUnit.SECOND);
            
            for(int ki=0; ki<intakeHydrograph.getSimulationDischarge().getTflo_out().length-1; ki++){    
                for(int kc=0; kc<=(int)Math.round(cicloOut_s/cicloCal_s); kc++){
                    
                    Tflo_DA[(ki)*((int)Math.round(cicloOut_s/cicloCal_s))+kc] =
                        ((ki*intakeStepSec + kc*cicloCal_s - ki*intakeStepSec) / ((ki+1)*intakeStepSec - ki*intakeStepSec)) *
                        (intakeHydrograph.getSimulationDischarge().getTflo_out()[ki+1] -
                            intakeHydrograph.getSimulationDischarge().getTflo_out()[ki]) +
                            intakeHydrograph.getSimulationDischarge().getTflo_out()[ki];    
                    
                }
            }
            
            // Adding lag
            TimeInterval lag = drainageArea.getLag();
            
            // Convert lag to seconds
            double lagSeconds = lag.getInterval(TimeInterval.TimeIntervalUnit.SECOND);
            
            double[] Tflo_DATemp = Arrays.copyOf(Tflo_DA, Tflo_DA.length);
            Arrays.fill(Tflo_DA, 0);
            System.arraycopy(
                    Tflo_DATemp, 0, Tflo_DA, (int) lagSeconds/(int)cicloCal_s,
                    Tflo_DATemp.length - (int) lagSeconds/(int)cicloCal_s);
            
        }		
		
        int num_cel = 0;
        int num_contrib = 0;

        double[][] scs = watershed.getRasCurveNumber();
        double[][] routingTime = watershed.getRasRoutingTime();

        int nRows = scs.length;
        int nCols = scs[0].length;
        int n_cic_bas = 0;

        double cellArea = Math.pow(watershed.getCellSize(), 2);
        double basin_area = watershed.getArea(); // cellArea * num_cel * 1e-6;
        //double Galli_flo = 2d/1000d * basin_area / (basin_area * 1E6 / cellArea); //2E-3 * basin_area / (basin_area * 1E6 / cellArea); // 2m3/s/km2
        
        int drainageAreasCellCount = 0;
        
        double noData = watershed.getNoData();
        
        /* Cycle through cells */
        double initialQin = watershed.getBaseflowParams().getInitialBaseflow() * basin_area; //* 1E6 / cellArea); // [m3s-1]  
        
        for(int row = 0; row<nRows; row++){
            //if(row%10 == 0) System.out.println(row + "/" + nRows);
            for(int col=0; col<nCols; col++){
                
                double qmax = -9e9;
                int num_def = 0;
                if (!Double.isNaN(scs[row][col])
                        && scs[row][col] != noData
                        && scs[row][col] != 0.0
                        && !Double.isNaN(routingTime[row][col])
                        && routingTime[row][col] != noData){
                    
                    // Count Drainage Areas cells
                    if(drainageArea != null && drainageArea.getRasDrainageAreas() != null
                            && drainageArea.getUptakeFraction() != 0
                            && drainageArea.getRasDrainageAreas()[row][col] == 1){
                        drainageAreasCellCount++;
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
                    double[] pTotCal_ia_cell = Arrays.copyOf(pTotCal, n_cic_cal+1); // [mm]
                    for(int jp=0; jp<n_cic_cal; jp++) {
                        if(pTotCal[jp] < tmp) {
                            tmp = tmp - pTotCal_ia_cell[jp];
                            pTotCal_ia_cell[jp] = 0;
                        } else {
                            pTotCal_ia_cell[jp] -= tmp;
                            break;
                        }
                    }

                    // Calculate effective and infiltrated rainfall
                    double p_netta = 0;                    
                    for(int jp=0; jp<=n_cic_cal; jp++){
                        
                        // Runoff reduction parameter      
                        if(pTotCal_ia_cell[jp] < rainfallRecessionThreshold *
                                hyetograph.getStep().getInterval(TimeInterval.TimeIntervalUnit.HOUR)) { // [mm] // OKKIO: hours?
                            p_netta = p_netta + pTotCal_ia_cell[jp] - p_netta * rainfallRecession * cicloCal_h;
                            if(p_netta < 0) {
                                p_netta = 0;
                            }
                        } else {                            
                            p_netta += pTotCal_ia_cell[jp];
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
                            Peff_cal[jp] = Peff_cal_cum[jp];
                        } else {
                            Peff_cal[jp] = Peff_cal_cum[jp] - Peff_cal_cum[jp-1]; // [mm]
                        }
                        if(Peff_cal[jp] < 0) {
                            Peff_cal[jp] = 0.0;
                        }
                        
                        Peff_bac[jp] += Peff_cal[jp]; // mm
                        pTotCal_ia_tot[jp] += pTotCal_ia_cell[jp];
                        Pinf_cel[jp] = pTotCal_ia_cell[jp] - Peff_cal[jp]; // mm
                        Pinf_bac[jp] += Pinf_cel[jp]; // mm
                        
                    }
                    num_cel ++;
                        
                    if (num_def > 0.) {
                        num_contrib ++;
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
                            qq += Tflo_DA[jp];
                        }                        
                        
                        // Accounts for uptake from areas uptake
                        if(drainageArea != null && drainageArea.getRasDrainageAreas() != null &&
                                drainageArea.getUptakeFraction() !=0){
                            if(drainageArea.getRasDrainageAreas()[row][col] == 1){
                                Dflo_cal[itime] += (1. - drainageArea.getUptakeFraction()) * qq;
                                DFlo_calDA[jp] += (drainageArea.getUptakeFraction() * qq);
                            }else{
                                Dflo_cal[itime] = Dflo_cal[itime] + qq;
                            }
                        }else{
                            Dflo_cal[itime] = Dflo_cal[itime] + qq;
                        }  
                    }
                    
                    // Distributed baseflow
                    if(watershed.getBaseflowParams().getBaseflowType() == BaseflowParams.BaseflowType.DISTRIBUTED) {
                        double qin = initialQin / (basin_area * 1E6 / cellArea); // [m3s-1]
                        double Galli_Flo = 2d/1000d * basin_area / (basin_area * 1E6 / cellArea); //2E-3 * basin_area / (basin_area * 1E6 / cellArea); // 2m3/s/km2
                        // Initial baseflow
                        double[] Bflo_cal_temp = new double[MAXDATA];
                        if(calcBaseFlow) {
                            for(int jp=0; jp<MAXDATA-10000; jp++){

                                double qu = (Pinf_cel[jp] * cellArea * 1e-3) / cicloCal_s; // [m3s-1]
                                double Bflo_cal_pre = Bflo_cal_temp[jp];
                                Bflo_cal_temp[jp] = qin * Math.exp(-baseFlowRecession * cicloCal_s) + (1. - Math.exp(-baseFlowRecession * cicloCal_s)) * qu; // [m3s-1]

                                qin = Bflo_cal_temp[jp]; // [m3s-1]

                                Bflo_cal_temp[jp] = Bflo_cal_pre + (1 - subsurfaceDrainageLoss) * Bflo_cal_temp[jp]; // [m3s-1]

                                int itime = (int)(routingTime_s / cicloCal_s) + jp; //(int)Math.round(tempo / cicloCal_s) + (jp-1);
                                // TODO
                                if (jp >= n_cic_cal && qin < (watershed.getBaseflowParams().getInitialBaseflow() * (basin_area * 1E6 / cellArea)) && Dflo_cal[itime] == 0) {
                                //if(jp > n_cic_cal && Dflo_cal[itime] == 0 && Bflo_cal[jp] < Galli_Flo) {
                                    if(jp > n_cic_bas) {
                                        n_cic_bas = jp;
                                    }
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
        
        // -----------------------------------------------------------------------
        int k;
        
//        pTotOut = rescaleRainfall(pTotInp, cicloIn_s, cicloOut_s);
        
//        for(k = 0; k<MAXDATA; k++){
//            
//            double time = (k+1) * cicloOut_s;
//            int inPos1 = (int)Math.floor((time - cicloOut_s) / cicloIn_s);
//            int inPos2 = (int)Math.floor(time / cicloIn_s);
//            if(time == cicloIn_s * inPos2) {
//                inPos2--;
//            }

//            int ipnt = (int)Math.floor(time/cicloIn_s);
//            
//            if (ipnt >= MAXDATA -1) {
//                break;
//            }
//            double delta = time - ipnt * cicloIn_s;
//            // Use pTotCal_ia here to exclude Ia from calculation
//            pTotOut[k] = pTotInp[ipnt] * (delta / cicloIn_s);
//            //pTotOut[k] = pTotOut[k] * (cicloOut_s / cicloIn_s);
//        }
//        for(int jp=k; jp<MAXDATA; jp++){
//            pTotOut[jp] = pTotOut[k-1];
//        }
        
        for(int jp = 0; jp<=n_cic_cal; jp++){
            Peff_bac[jp] = Peff_bac[jp] / num_cel;// * (cicloOut_s / cicloCal_s);
            Pinf_bac[jp] = Pinf_bac[jp] / num_cel;// * (cicloOut_s / cicloCal_s);
            // System.out.println(pTotCal[jp] + ", " + Peff_bac[jp] + ", " + Pinf_bac[jp]);
        }
        int jpp;
        for(jpp=MAXDATA-1;jpp>=1; jpp--){
            if(Dflo_cal[jpp] > 0. || Bflo_cal[jpp] > 0) {
                break;
            }
        }

        D_tottim = (int)(Math.ceil(jpp * cicloCal_s / cicloOut_s)+1) * cicloOut_s;

        // Cumulative rainfall
        int n_cic_dir = (int) Math.round(jpp*cicloCal_s / cicloOut_s) + 1;
        double ratio = cicloOut_s / cicloCal_s;
        int n_cic_tot = -1;
        for(int jp=0; jp<=n_cic_dir+1; jp++){
            n_cic_tot++;
            double time = jp * cicloOut_s;
            //int ipnt  = (int)Math.floor(time/ciclo_cal);
            int ipnt = (int) Math.floor(ratio * jp);

            double delta = time - (ipnt) * cicloCal_s;
            
            Dflo_out[n_cic_tot] = Dflo_cal[ipnt] + (Dflo_cal[ipnt+1] - Dflo_cal[ipnt]) * (delta / cicloCal_s);
            Bflo_out[n_cic_tot] = Bflo_cal[ipnt] + (Bflo_cal[ipnt+1] - Bflo_cal[ipnt]) * (delta / cicloCal_s);
            
            // Aggregation via interpolation on rainfall 
//            Peff_out[n_cic_tot] = Peff_bac[ipnt] + (Peff_bac[ipnt+1] - Peff_bac[ipnt]) * (delta / cicloCal_s);
//            Pinf_out[n_cic_tot] = Pinf_bac[ipnt] + (Pinf_bac[ipnt+1] - Pinf_bac[ipnt]) * (delta / cicloCal_s);
            
        }

        double pTotSum = 0;
        double pEffSum = 0;
        double pInfSum = 0;
        for(int p=0; p<Peff_out.length; p++) {
            pTotSum += pTotCal[p];
            pEffSum += Peff_bac[p];
            pInfSum += Pinf_bac[p];
        }
        
        Peff_out = this.rescaleRainfall(Peff_bac, cicloCal_s, cicloOut_s);
        Pinf_out = this.rescaleRainfall(Pinf_bac, cicloCal_s, cicloOut_s);
        pTotOut = this.rescaleRainfall(pTotCal, cicloCal_s, cicloOut_s);
        
        if (n_cic_dir < 1) {
            throw new Exception("Nr of cycles < 1");
        }

        // --------------------------------------------------------------
        double Dflo_tot = 0.;
        double Bflo_tot = 0;
        double Dflo_max = -Double.MAX_VALUE;
        double Bflo_max = -Double.MAX_VALUE;
        
        // Lumped baseflow
        double[] Psto_out = new double[MAXDATA];
        double[] Bvol_out = new double[MAXDATA];
//        Prec_storage = 0.;
        for(k=1; k<=n_cic_dir; k++){
            Psto_out[k] = Pinf_out[k]; //pTotOut[k] - Peff_out[k];
//            /* Accounting for difference between in and out steps */
//            //Psto_out[k] *= (cicloOut_s / cicloIn_s);
//            //Prec_storage += Psto_out[k];
        }
        n_cic_bas = n_cic_dir; // falsa inizializzazione perchè è denominatore
        
        if(watershed.getBaseflowParams().getBaseflowType() == BaseflowParams.BaseflowType.LUMPED &&
                watershed.getBaseflowParams().getRecession() > 0.){
            // Bflo in m3/s, Stor in mm, Bvol in mm, recession in 1/s
            Bflo_max = -1e9;
            double Galli_flo = basin_area * 2e-3; // 2.0 l/(s*km²)
            double recession = Math.max(watershed.getBaseflowParams().getRecession(), 1e-12);
            Stor_out[0] = (initialQin / recession) / (basin_area*1e3);
            Bvol_out[0] = (initialQin * cicloOut_s) / (basin_area*1e3);
            int jp;
            for(jp=1; jp<MAXDATA; jp++){
                Bflo_out[jp] = recession * Stor_out[jp-1] * (basin_area*1e3);
                Bflo_out[jp] = Math.max(Bflo_out[jp], 0.0);
                Bvol_out[jp] = (Bflo_out[jp] * cicloOut_s) / (basin_area*1e3);
                Bvol_out[jp] = Math.min(Bvol_out[jp], Stor_out[jp-1]);
                Stor_out[jp] = Stor_out[jp-1] + Psto_out[jp] - Bvol_out[jp];
                Bflo_tot += Bflo_out[jp];
                if (Bflo_out[jp] > Bflo_max){
                    Bflo_max = Bflo_out[jp];
                    B_pektim = (jp*cicloOut_s)/3600.;
                }
                if (jp >= n_cic_dir && Bflo_out[jp] < Galli_flo) break;
                if (jp >= n_cic_dir && Bflo_out[jp] < watershed.getBaseflowParams().getInitialBaseflow()) break;
  
            }
            Bflo_out[0] = Bflo_out[1];
            n_cic_bas = jp - 1;
        }
        
        //pt=0;
        for(k=1; k<=n_cic_dir; k++){
            double time = k * cicloOut_s;
            //pt += (Tflo_out[k] * cicloOut_s) / (basin_area * 1e3);
            //pe += (Dflo_out[k] * cicloOut_s) / (basin_area * 1e3);
            Dflo_tot += Dflo_out[k];
            Bflo_tot += Bflo_out[k];
            if(Dflo_out[k] > Dflo_max){
                Dflo_max = Dflo_out[k];
                D_pektim = time/3600.;
            }
            if(Bflo_out[k] > Bflo_max) {
                Bflo_max = Bflo_out[k];
                B_pektim = (k*cicloOut_s*60)/3600.;
            }
            
        }
        
        double Tflo_tot = 0;
        double Tflo_max = -1e9;
        n_cic_tot = Math.max(n_cic_bas, n_cic_dir);

        int[] time = new int[MAXDATA];
        for(k=0; k<=n_cic_tot; k++){
            time[k] = k * cicloOut_s;
          
            Tflo_out[k] = Dflo_out[k] + Bflo_out[k];
            Tflo_tot += Tflo_out[k];
            if(Tflo_out[k] > Tflo_max){
                Tflo_max = Tflo_out[k];
                T_pektim = time[k]/3600.;
            }
        }

        // Drainage areas
        double[] ACFlo_out = new double[MAXDATA];
        double[] ACFlo_out2 = new double[MAXDATA];
        double[] ACStor_out = new double[MAXDATA];
        double[] ACVol_out = new double[MAXDATA];   

        if(drainageArea != null && drainageArea.getUptakeFraction() != 0){
            if(drainageAreasCellCount > 0){
                double drainageAreasArea = drainageAreasCellCount * Math.pow(watershed.getCellSize(),2) *1e-6;
                double daKappa = 1d/(2000d*Math.sqrt(drainageAreasArea));
                for(int jp=0; jp<MAXDATA; jp++){
                    ACFlo_out[jp] = daKappa * ACStor_out[jp-1] * (basin_area*1e3 * (drainageAreasArea/basin_area));
                    ACFlo_out[jp] = Math.max(ACFlo_out[jp], 0);

                    ACVol_out[jp] = (ACFlo_out[jp] * cicloOut_s) / (basin_area*1e3 * (drainageAreasArea/basin_area));
                    ACVol_out[jp] = Math.min(ACVol_out[jp], ACStor_out[jp-1]);

                    ACStor_out[jp] = ACStor_out[jp-1] + DFlo_calDA[jp] * cicloOut_s / (basin_area*1e3 * (drainageAreasArea/basin_area)) - ACVol_out[jp];
                }
            }else{
                Arrays.fill(ACFlo_out, 0);
                Arrays.fill(ACVol_out, 0);
            }

            n_cic_dir = 0;
            for(k = cicloOut_s; k<=D_tottim; k+=cicloOut_s){
                n_cic_dir++;
                int ipnt  = (int)Math.round(k/cicloCal_s);
                double delta = k - ipnt * cicloCal_s;
                ACFlo_out2[n_cic_dir] = ACFlo_out[ipnt] + (ACFlo_out[ipnt+1] -
                        ACFlo_out[ipnt]) * (delta/cicloCal_s);
            }
            
            double[] Bflo_outDA = new double[MAXDATA];
            SimulationDischarge simulationDischarge = new SimulationDischarge(
                    new TimeInterval(cicloOut_s, TimeInterval.TimeIntervalUnit.SECOND),
                    ACFlo_out2, Bflo_outDA, null, null, null, null, null, null, null, null, null, null, null);
            
            SimulationSteps simulationSteps = new SimulationSteps(n_cic_dir, n_cic_bas, n_cic_tot);
            
            drainageAreaSimOutput = new SimulationOutput(null, simulationSteps, simulationDischarge);
            
        }

        // Prepare pt array
//        pt = hyetograph.getRainfall()[0];
//        for(int jp=1; jp<hyetograph.getRainfall().length; jp++) {
//            pt += hyetograph.getRainfall()[jp];
//        }        
        
        
        //pt = pTotInp[MAXDATA-1];
        double contributingAreaPercentage = 100.*(double)num_contrib/(double)num_cel;
        double Dflo_med = Dflo_tot / n_cic_dir;
        double Bflo_med = Bflo_tot / n_cic_bas;
        double Tflo_med = Tflo_tot / n_cic_tot;        
        
        TimeInterval directFlowPeakTime = new TimeInterval(D_pektim, TimeInterval.TimeIntervalUnit.HOUR);
        TimeInterval baseFlowPeakTime = new TimeInterval(B_pektim, TimeInterval.TimeIntervalUnit.HOUR);
        TimeInterval totalFlowPeakTime = new TimeInterval(T_pektim, TimeInterval.TimeIntervalUnit.HOUR);        
        
        // Calculate appearing CN
        double appearingCN = calcAppearingCN(pTotSum, pEffSum);
        
        // Create final outputs classes
        double[] storeOut = new double[pTotOut.length];

//        for(int p=0; p<storeOut.length; p++) {
//            Prec_storage+= storeOut[p];
//        }
        
        SimulationRainfall simulationRainfall = new SimulationRainfall(
                new TimeInterval(cicloOut_s, TimeInterval.TimeIntervalUnit.SECOND),
                pTotOut, Peff_out, storeOut, pTotSum, pEffSum, pInfSum); // TODO: StoreOut e ps
        SimulationSteps simulationSteps = new SimulationSteps(n_cic_dir, n_cic_bas, n_cic_tot);
        SimulationDischarge simulationDischarge = new SimulationDischarge(
                new TimeInterval(cicloOut_s, TimeInterval.TimeIntervalUnit.SECOND),
                Dflo_out, Bflo_out,
                Dflo_max, Bflo_max, Tflo_max,
                Dflo_med, Bflo_med, Tflo_med,
                directFlowPeakTime, baseFlowPeakTime, totalFlowPeakTime,
                appearingCN, contributingAreaPercentage);
        
        simulationOutput = new SimulationOutput(
                simulationRainfall,
                simulationSteps,
                simulationDischarge);
        
    }
    
    /**
     * Calculates the appearing CN.
     */
    private double calcAppearingCN(double pt, double pe){
        
        double S;
        double initialAbstraction = 0;
        if(watershed.getInitialAbstraction().getAbstractionUnits() == InitialAbstraction.AbstractionUnits.FRACTION) {
            initialAbstraction = watershed.getInitialAbstraction().getAbstractionValue();
        } else if(watershed.getInitialAbstraction().getAbstractionUnits() == InitialAbstraction.AbstractionUnits.MILLIMETERS) {
//            initialAbstraction = watershed.getInitialAbstraction().getAbstractionValue() * S_MEDIO; // TODO???
        }

        if(initialAbstraction >= 1e-5){
            S = (2*initialAbstraction*pt + (1-initialAbstraction)*pe -
                    Math.sqrt(Math.pow(((1-initialAbstraction)*pe),2) +
                    4*initialAbstraction*pt*pe)) / (2 * Math.pow(initialAbstraction,2));  
        }else{
            S = (Math.pow(pt,2) - pe*pt) / pe;
        }
        
        return 25400. / (254. + S);
    
    }

    /**
     * Returns the simulation output for the main watershed.
     * @return The simulation output.
     */
    public SimulationOutput getSimulationOutputs(){
        return simulationOutput;
    }

    /**
     * Returns the simulation output for the uptake drainage areas (optional).
     * @return 
     */
    public SimulationOutput getUptakeSimulationOutput(){
        return drainageAreaSimOutput;
    }

    public double[] rescaleRainfall(double in[], double cicloIn, double cicloOut) {
        
        //double out[] = new double[(int)Math.ceil(in.length * cicloIn/cicloOut)];
        double out[] = new double[MAXDATA];
        for(int k = 0; k<out.length; k++){
            
            double tOut2 = (k+1) * cicloOut;
            int inPos1 = (int)Math.floor((tOut2 - cicloOut) / cicloIn);
            int inPos2 = (int)Math.floor(tOut2 / cicloIn);
            if(tOut2 == cicloIn * inPos2) {
                inPos2--;
            }
            if(inPos2 > in.length -1) {
                inPos2 = in.length - 1;
            }            
            
            if(inPos1 == inPos2) {
                out[k] = in[inPos1] * cicloOut / cicloIn;
            } else {                
                for(int p=inPos1; p<=inPos2; p++) {
                    out[k] += (in[p]); 
                }
            }
           
        }
        
        return out;
        
    }
    
//    public double[] getBaseFlow(){
//        double[] baseFlow = {Dflo_out[0], Bflo_out[0], Tflo_out[0]};
//        return baseFlow;
//    }
//    
//    public double[] getMaxFlo(){
//        double[] maxFlow = {Dflo_max, Bflo_max, Tflo_max};
//        return maxFlow;
//    }
//
//    public double[] getAvgFlo(){
//        double[] avgFlow = {Dflo_med, Bflo_med, Tflo_med};
//        return avgFlow;
//    }    
//
//    public TimeInterval getDirectFlowPeakTime(){
//        return directFlowPeakTime;
//    }      
//
//    public TimeInterval getBaseFlowPeakTime(){
//        return baseFlowPeakTime;
//    }  
//    
//    public TimeInterval getTotalFlowPeakTime(){
//        return totalFlowPeakTime;
//    }
    
    private final Watershed watershed;
    private double cicloCal_s = 0;
    private double cicloCal_h = 0;
    private int cicloOut_s = 0;

//    private double[] Dflo_out = null;
//    private double[] Bflo_out = null;
//    private double[] Tflo_out = null;
//    private double Dflo_max = 0;
//    private double Bflo_max = 0;
//    private double Tflo_max = 0;
//    private double Dflo_med = 0;
//    private double Bflo_med = 0;
//    private double Tflo_med = 0;
//    private TimeInterval directFlowPeakTime = null;
//    private TimeInterval baseFlowPeakTime = null;
//    private TimeInterval totalFlowPeakTime = null;
    
    private SimulationOutput simulationOutput = null;
    private SimulationOutput drainageAreaSimOutput = null;

    public static final int MAXDATA = 50000;

}
