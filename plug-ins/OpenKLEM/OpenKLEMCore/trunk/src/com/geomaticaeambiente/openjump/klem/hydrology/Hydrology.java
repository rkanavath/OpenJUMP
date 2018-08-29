
package com.geomaticaeambiente.openjump.klem.hydrology;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import it.geomaticaeambiente.klem.Hyetograph;
import it.geomaticaeambiente.klem.TimeInterval;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Hydrology {
 
    public static EffectiveRainfall calcEffectiveRainfall(Hyetograph hyetograph,
            double G_CN, double G_FrazioneCDcostante, double G_CD, double G_CDp, double G_Area){
    
        double S = 254d * (100d / G_CN - 1d);
        
        // Cumulated effective rainfall
        double Pcum = 0;
        double[] PeCum = new double[hyetograph.getRainfall().length];
        for(int i=0; i<hyetograph.getRainfall().length;i++){
            Pcum += hyetograph.getRainfall()[i];
            if(Pcum > 0) {
                PeCum[i] = Math.pow(Pcum,2) / (Pcum + S);
            }
        }

        // Incremental effective rainfall       
        double G_VolumeIetogramma = 0;
        double[] G_Pe = new double[hyetograph.getRainfall().length];
        double[] G_Pp = new double[hyetograph.getRainfall().length];

        for(int i=1; i<hyetograph.getRainfall().length;i++){
            G_Pe[i] = (PeCum[i] - PeCum[i - 1]) * (1 - G_FrazioneCDcostante) +
                    hyetograph.getRainfall()[i] * G_FrazioneCDcostante * G_CD;
            G_Pp[i] = hyetograph.getRainfall()[i] * G_CDp;
            G_VolumeIetogramma = G_VolumeIetogramma + G_Pe[i];
        }
        
        double yetographVolume = G_VolumeIetogramma * G_Area * 1000;               

        double[] time = new double[hyetograph.getRainfall().length];
        for(int t=0; t<time.length; t++) {
            time[t] = hyetograph.getStep().getInterval(TimeInterval.TimeIntervalUnit.HOUR) * t;
        }
        
        EffectiveRainfall effectiveRainfall = new EffectiveRainfall(
                time, G_Pe, G_Pp, yetographVolume);    

        return effectiveRainfall;
    }
    
    public static UnitHydrograph calcIuhSCS(double G_TimeStep, double G_Area, double G_QBase, 
            EffectiveRainfall effectiveRainfall, double G_TempoRitardo){
    
        double Tpicco = G_TimeStep / 2 + G_TempoRitardo;
        double Qpicco = 0.75 / Tpicco;
        //   Qpicco = 484 / Tpicco
        int G_Niuh = (int)Math.round(5 * Tpicco / G_TimeStep) - 1;
        int i = 1;
        double tau = i * G_TimeStep / Tpicco;
        
        List<Double> G_IUH_l = new ArrayList<Double>();
        
        while(tau < 0.7){
            G_IUH_l.add((1.3606 * Math.pow(tau, 2) + 0.2445 * tau) * Qpicco);
            i++;
            tau = i * G_TimeStep / Tpicco;
        }
        while(tau <= 2){
            G_IUH_l.add((1.1693 * Math.pow(tau, 3) - 5.366 * Math.pow(tau, 2) + 7.1891 * tau - 1.9857) * Qpicco);
            i++;
            tau = i * G_TimeStep / Tpicco;
        }
        while(tau <= 3.4){
            G_IUH_l.add((0.13155 * Math.pow(tau, 2) - 0.88286 * tau + 1.51486) * Qpicco);
            i++;
            tau = i * G_TimeStep / Tpicco;
        }
        while(tau < 5){
            G_IUH_l.add((0.01081 * Math.pow(tau, 2) - 0.10793 * tau + 0.27) * Qpicco);
            i++;
            tau = i * G_TimeStep / Tpicco;
        }  
         
        double[] G_IUH = new double[G_Niuh+1];
        for(int p=0; p<G_IUH_l.size(); p++){
            G_IUH[p] = G_IUH_l.get(p);
        }
        
        double[] G_Q = convolute(G_Niuh, G_IUH, G_Area, effectiveRainfall, G_QBase, G_TimeStep);
        return (new UnitHydrograph(G_TimeStep, G_Q));
        
    }    
    
    public static UnitHydrograph calcIuhTriangular(double G_TimeStep, double G_Area, double G_QBase,
            EffectiveRainfall effectiveRainfall, double G_TempoBase, double G_TempoPicco){
    
        double Hp = 2 / G_TempoBase;
        int i=1;
        double t = G_TimeStep;
        List<Double> G_IUH_l = new ArrayList<Double>();
        int G_Niuh;
        
        while(t <= G_TempoPicco + G_TimeStep / 2){
            G_IUH_l.add((t - G_TimeStep / 2) * Hp / G_TempoPicco);
            i = i + 1;
            t = i * G_TimeStep;
        }
   
        while( t <= G_TempoBase + G_TimeStep / 2){
            G_IUH_l.add(Hp * (G_TempoBase + G_TimeStep / 2 - t) / (G_TempoBase - G_TempoPicco));
            i = i + 1;
            t = i * G_TimeStep;
        }
        G_Niuh = i - 1;
        
        double[] G_IUH = new double[G_Niuh+1];
        for(int p=0; p<G_IUH_l.size(); p++){
            G_IUH[p] = G_IUH_l.get(p);
        }        
        
        double[] G_Q = convolute(G_Niuh, G_IUH, G_Area, effectiveRainfall, G_QBase, G_TimeStep);
        return (new UnitHydrograph(G_TimeStep, G_Q));
        
    }
    
    public static UnitHydrograph calcIuhHortonTriangular(double G_TimeStep, double G_Area, double G_QBase,
            EffectiveRainfall effectiveRainfall, double G_LunghezzaP, double G_VelocitaP,
            double G_HortonRA, double G_HortonRB, double G_HortonRL){
        
        double TempoBase = 2 * G_LunghezzaP / (0.36 * Math.pow(G_HortonRL,0.43) * G_VelocitaP) / 3600;
        double TempoPicco = 1.58 * Math.pow((G_HortonRA / G_HortonRB),0.55) * Math.pow(G_HortonRL,-0.38) * G_LunghezzaP / G_VelocitaP / 3600;
   
        // Costruisci l'IUH
        int G_Niuh;
        double Hp = 2 / TempoBase;
        int i = 1;
        double t = G_TimeStep;
        List<Double> G_IUH_l = new ArrayList<Double>();
        while(t <= TempoPicco + G_TimeStep / 2){
            G_IUH_l.add((t - G_TimeStep / 2) * Hp / TempoPicco);
            i = i + 1;
            t = i * G_TimeStep;
        }
   
        while(t <= TempoBase + G_TimeStep / 2){
            G_IUH_l.add(Hp * (TempoBase + G_TimeStep / 2 - t) / (TempoBase - TempoPicco));
            i = i + 1;
            t = i * G_TimeStep;
        }
        G_Niuh = i - 1;
    
        double[] G_IUH = new double[G_Niuh+1];
        for(int p=0; p<G_IUH_l.size(); p++){
            G_IUH[p] = G_IUH_l.get(p);
        }        
        
        double[] G_Q = convolute(G_Niuh, G_IUH, G_Area, effectiveRainfall, G_QBase, G_TimeStep);
        return (new UnitHydrograph(G_TimeStep, G_Q));        
        
    }
    
    public static UnitHydrograph calcIuhNash(double G_TimeStep, double G_Area, double G_QBase,
            EffectiveRainfall effectiveRainfall, double G_ParametroForma, double G_ParametroScala){
        
        double G = gamma(G_ParametroForma);
        int G_Niuh = (int)Math.round(G_ParametroForma * G_ParametroScala * 4 / G_TimeStep) - 1;
        
        List<Double> G_IUH_l = new ArrayList<Double>();
        for(int i=1; i<G_Niuh; i++){
            G_IUH_l.add(1 / (G_ParametroScala * G) *
                    Math.pow(((i - 0.5) * G_TimeStep / G_ParametroScala),(G_ParametroForma - 1)) *
                    Math.exp(-(i - 0.5) * G_TimeStep / G_ParametroScala));
        }
    
        double[] G_IUH = new double[G_Niuh+1];
        for(int p=0; p<G_IUH_l.size(); p++){
            G_IUH[p] = G_IUH_l.get(p);
        }        
        
        double[] G_Q = convolute(G_Niuh, G_IUH, G_Area, effectiveRainfall, G_QBase, G_TimeStep);
        
        return (new UnitHydrograph(G_TimeStep, G_Q)); 
        
    }
    
    public static UnitHydrograph calcIuhGeomorpho(EffectiveRainfall effectiveRainfall,
            DoubleBasicGrid routingTimeGrid, double recession) throws Exception{
    
        // Routing time
        double cicloIn = effectiveRainfall.getEffectiveRainfall()[0][1] - 
                effectiveRainfall.getEffectiveRainfall()[0][0];


        UnitHydrograph unitHydrograph = HydrographGeo.calcHydrograph(
                effectiveRainfall.getEffectiveRainfall()[1],
                effectiveRainfall.getDeepRainfall()[1],
                cicloIn,
                routingTimeGrid,
                routingTimeGrid.getCellSize()/0.1/3600,
                recession);
       
        return unitHydrograph;
        
    }
    
    private static double[] convolute(int G_Niuh, double[] G_IUH, double G_Area,
            EffectiveRainfall effectiveRainfall, double G_Qbase, double G_TimeStep){
    
        int Nampl = 4;
        double[] G_IUHp = new double[(G_Niuh)*(Nampl)];
 
        // Ricostruisci l'idrogramma profondo IUHp amplificando IUH
        for(int j=0; j<G_Niuh; j++){
            double diff = G_IUH[j + 1] - G_IUH[j];
            for(int i = 1; i<Nampl; i++){
                G_IUHp[j * Nampl + i] = (G_IUH[j] + diff * (i / Nampl)) / Nampl;
            }
        }

        // Calcola il deflusso superficiale
        double G_VolumeIdrogramma = 0;
        int G_Nts = effectiveRainfall.getEffectiveRainfall()[0].length; // OKKIO
        
        double[] G_Q = new double[G_Nts * G_Niuh]; // OKKIO
        double[] G_Qp = new double[G_Nts * G_Niuh]; // OKKIO
        for(int i=0; i<G_Nts; i++){
            for(int j = 1; j<G_Niuh; j++){
                G_Q[i + j - 1] = G_Q[i + j - 1] + effectiveRainfall.getEffectiveRainfall()[1][i] * G_IUH[j] * G_Area / 3.6;
            }
        }

        // Calcola il deflusso profondo aggiungendo il deflusso di base
        for(int i=0; i<G_Nts; i++){
            for(int j=1; j< G_Niuh; j++){
                G_Qp[i + j - 1] = G_Qp[i + j - 1] + effectiveRainfall.getDeepRainfall()[1][i] * G_IUHp[j] * G_Area / 3.6;
            }
            G_Qp[i] = G_Qp[i] + G_Qbase;
        }

        // Prima calcola il volume dell'idrogramma e poi somma il deflusso profondo
        for(int i=0; i<G_Nts; i++){
            G_VolumeIdrogramma = G_VolumeIdrogramma + G_Q[i] * G_TimeStep * 3600;
            G_Q[i] = G_Q[i] + G_Qp[i];
        }
    
        // Trim G_Q
        boolean dataFound = false;
        int zeroIndex = 0;
        for(int p=0; p<G_Q.length; p++) {
            
            if(G_Q[p] > 0) {
                dataFound = true;
            }
            
            if(dataFound && G_Q[p] == 0) {
                zeroIndex = p;
                break;
            }
            
        }
        
        
        return Arrays.copyOfRange(G_Q, 0, zeroIndex);
        
    }
    
    private static double gamma(double X){

        double G = 1;
        double gamma;
        while(X > 2){
            G = G * (X - 1);
            X = X - 1;
        }
        
        // Funzione approssimante
        if(X == 1 || X == 2){
            gamma = 1;
        }else if(X > 1.4){
            gamma = -0.01851 * Math.pow(X,3) + 0.48302 * X * X - 1.29223 * X + 1.80035;
        }else{
            gamma = -0.41993 * Math.pow(X,3) + 2.14845 * Math.pow(X,2) - 3.60714 * X + 2.87853;
        }
    
        return (gamma * G);
    
    }
    
    public static class EffectiveRainfall{
        
        public EffectiveRainfall(double[] time, double[] effectiveRainfall,
                double[] deepRainfall, double hyetographVolume){
            
            this.time = time;
            this.effectiveRainfall = effectiveRainfall;
            this.deepRainfall = deepRainfall;
            this.hyetographVolume = hyetographVolume;
            
        }
        
        public double[][] getEffectiveRainfall(){
        
            double[][] effectiveRainfallHyeto = new double[2][time.length];
            
            effectiveRainfallHyeto[0] = time;
            effectiveRainfallHyeto[1] = effectiveRainfall;            
            return effectiveRainfallHyeto;
            
        }

        public double[][] getDeepRainfall(){
        
            double[][] effectiveRainfallHyeto = new double[2][time.length];
            
            effectiveRainfallHyeto[0] = time;
            effectiveRainfallHyeto[1] = deepRainfall;
            
            return effectiveRainfallHyeto;
            
        }        
        
        public double getHyetographVolume(){
            return hyetographVolume;
        }
        
        private double[] time = null;
        private double[] effectiveRainfall = null;
        private double[] deepRainfall = null;
        private double hyetographVolume = 0;
        
    }

}
