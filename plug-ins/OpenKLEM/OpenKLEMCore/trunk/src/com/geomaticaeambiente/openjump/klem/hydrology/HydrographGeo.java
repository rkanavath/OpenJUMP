
package com.geomaticaeambiente.openjump.klem.hydrology;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import java.util.Arrays;

/**
 *
 * @author Geomatica
 */
public class HydrographGeo {
    
    public static UnitHydrograph calcHydrograph(double[] Peff_inp, double[] Psto_inp, double ciclo_in,
            DoubleBasicGrid routingTimeGrid, double ciclo_cal, double recession) throws Exception{
        
        
        double flo_ini = 0;
        double[] Dflo_out;
        double[] Bflo_out;
        double[] Tflo_out;
        int n_cic_dir;
        int MAXDATA = 50000;        
        
        int ncicpr = Peff_inp.length -1; // hyetograph.getStepsCount() - 1;

        ciclo_in *= 3600;
        ciclo_cal *= 3600; // from hours to seconds
        
        if(ciclo_cal < ciclo_in) ciclo_cal = ciclo_in;
        
        double ciclo_out = ciclo_in;
        
        double D_tottim = (int)Math.round(((ncicpr*ciclo_in/ciclo_cal)+1) *
                ciclo_cal); // OKKIO

        // pioggia cumulata
        double[] Ptot_inp = new double[MAXDATA];
        double[] Peff_cel = new double[MAXDATA];
        double[] Peff_bac = new double[MAXDATA];
        double[] Peff_out = new double[MAXDATA];
        double[] Psto_out = new double[MAXDATA];
        double[] Dflo_cal = new double[MAXDATA];
        Dflo_out = new double[MAXDATA];
        double[] fMax = new double[MAXDATA];
        double[] Stor_out = new double[MAXDATA];
        double[] Bvol_out = new double[MAXDATA];
        Bflo_out = new double[MAXDATA];
        Tflo_out = new double[MAXDATA];

        Peff_inp = Arrays.copyOf(Peff_inp, MAXDATA);
        Psto_inp = Arrays.copyOf(Psto_inp, MAXDATA);
        
        for(int jp=1; jp<Peff_inp.length; jp++){
            Ptot_inp[jp] = Peff_inp[jp] + Psto_inp[jp];
        }
        for(int jp=1; jp<Peff_inp.length; jp++){
            Ptot_inp[jp] += Ptot_inp[jp-1];
            Peff_inp[jp] += Peff_inp[jp-1];
        }

        int n_cic_cal = 0;
        for(double time=ciclo_cal; time<=D_tottim; time+=ciclo_cal){
          n_cic_cal++;
          int ipnt  = (int)Math.floor(time/ciclo_in);
          double delta = time - ipnt * ciclo_in;
          
          Peff_out[n_cic_cal] = Peff_inp[ipnt]+(Peff_inp[ipnt+1]-Peff_inp[ipnt])*(delta/ciclo_in);
          Psto_out[n_cic_cal] = Psto_inp[ipnt]+(Psto_inp[ipnt+1]-Psto_inp[ipnt])*(delta/ciclo_in);
          
        }

        int num_cel = 0;
        int num_contrib = 0;
        int no_rows = routingTimeGrid.getRowCount();
        int no_columns = routingTimeGrid.getColumnCount();

        double cellArea = Math.pow(routingTimeGrid.getCellSize(), 2);

        for(int ir = 0; ir<no_rows; ir++){
            for(int ic=0; ic<no_columns; ic++){
                double qmax = -9e9;
                int num_def = 0;
                if (!routingTimeGrid.isNoData(routingTimeGrid.getData()[ir][ic])){
                        for(int jp=0; jp<=n_cic_cal; jp++){
                            // deflusso cumulato
                            Peff_cel[jp] = Peff_out[jp];
                        }
                        num_cel ++;
                        if (num_def > 0.) num_contrib ++;
                        // incremento di deflusso
                        for(int jp=n_cic_cal; jp>=2; jp--){
                            Peff_cel[jp] = Peff_cel[jp] - Peff_cel[jp-1];
                        }
                        for(int jp=0; jp<=n_cic_cal; jp++){
                             Peff_bac[jp] = Peff_bac[jp] + Peff_cel[jp];
                        }
                        // lunghezze in metri, tempo in secondi
                        double tempo = routingTimeGrid.getData()[ir][ic] * 3600.;
                        for(int jp=1; jp<=n_cic_cal; jp++){
                            // tempo in cicli
                            int itime = (int)Math.round(tempo / ciclo_cal) + (jp-1);
                            // da deflusso a portata in m3/s
                            double qq = (Peff_cel[jp] * cellArea * 1e-3) / ciclo_cal;
                            
//                            if(qq < 0){
//                                System.out.println(qq);
//                            }
                            
                            if (qq > qmax) qmax = qq;
                            Dflo_cal[itime] = Dflo_cal[itime] + qq;
                        }
                        //  m3/(s*kmý)
                        fMax[ic] = (qmax*1e6/cellArea);
                }else{
                    fMax[ic] = routingTimeGrid.getNoData();
                }
          }
//          if (rasqmax.and.(.not.findduration)) write (18, err=103) (fmax(ic),ic=1,no_columns);
        }
        // fine ciclo celle

        for(int jp = 1; jp<MAXDATA; jp++){
          Peff_bac[jp] = Peff_bac[jp] + Peff_bac[jp-1];
        }
        int jpp;
        for(jpp=MAXDATA-1;jpp>=1; jpp--){
          if(Dflo_cal[jpp] > 0.) break;
        }

        D_tottim = (int)(Math.floor(jpp*ciclo_cal/ciclo_out)+1) * ciclo_out;

        // pioggia cumulata
        n_cic_dir = 0;
        for(double time=ciclo_out; time<=D_tottim; time+=ciclo_out){
          n_cic_dir++;
          int ipnt  = (int)Math.round(time/ciclo_cal);
          double delta = time - ipnt * ciclo_cal;
          Dflo_out[n_cic_dir] = Dflo_cal[ipnt]+(Dflo_cal[ipnt+1]-Dflo_cal[ipnt])*
                  (delta/ciclo_cal);
          Peff_out[n_cic_dir] = Peff_bac[ipnt]+(Peff_bac[ipnt+1]-Peff_bac[ipnt])*
                  (delta/ciclo_cal);
        }

        if (n_cic_dir < 1) throw new Exception("Nr of cycles < 1");
        for(int jp = n_cic_dir + 1; jp<MAXDATA; jp++){
            Peff_out[jp] = Peff_out[n_cic_dir];
        }
        // -----------------------------------------------------------------------
        int k;

        // --------------------------------------------------------------
        double basin_area = cellArea * num_cel * 1e-6;
//        n_cic_bas = n_cic_dir; // falsa inizializzazione perchè è denominatore

        if(recession > 0.){

            double Galli_flo = basin_area * 2e-3; // 2.0 l/(s*km²)
            recession = Math.max(recession, 1e-12);
            Stor_out[0] = (flo_ini /recession) / (basin_area*1e3);
            Bvol_out[0] = (flo_ini * ciclo_out) / (basin_area*1e3);
            int jp;
            for(jp=1; jp<=MAXDATA; jp++){
                Bflo_out[jp] = recession*Stor_out[jp-1]*(basin_area*1e3);
                Bflo_out[jp] = Math.max(Bflo_out[jp], 0.0);
                Bvol_out[jp] = (Bflo_out[jp]*ciclo_out)/(basin_area*1e3);
                Bvol_out[jp] = Math.min(Bvol_out[jp], Stor_out[jp-1]);
                Stor_out[jp] = Stor_out[jp-1] + Psto_out[jp] - Bvol_out[jp];

                if (jp >= n_cic_dir && Bflo_out[jp] < Galli_flo) break;
                if (jp >= n_cic_dir && Bflo_out[jp] < flo_ini  ) break;
  
            }
            Bflo_out[0] = Bflo_out[1];
        }

        for(k=0; k<MAXDATA; k++){          
            Tflo_out[k] = Dflo_out[k] + Bflo_out[k];
//            if(Dflo_out[k] > 0) System.out.println(Tflo_out[k] + "," + Dflo_out[k] + "," + Bflo_out[k]);
        }

        return new UnitHydrograph(ciclo_in/3600, Tflo_out);
        
    }
    
}
