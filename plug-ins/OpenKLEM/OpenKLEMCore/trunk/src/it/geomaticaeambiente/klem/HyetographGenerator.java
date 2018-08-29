package it.geomaticaeambiente.klem;

import java.util.Arrays;

/**
 * The hyetograph generator creates design hyetographs given some rainfall parameters.
 * It can create hyetographs with five different shapes: constant, Wallingford,
 * triangular, alternating block, instant intensity and constantly increasing.
 * @author AdL
 */
public class HyetographGenerator {

    /**
     * Constructs the hyetograph generator class.
     * @param peakPosition Relative position, inside the hyetograph of the
     * rainfall peak (0-1). This parameter is overridden to 1 when the shape of
     * the hyetograph is "constantly increasing".
     * @param timeInterval The time step between rainfall pulses.
     * @param designRain The design rain parameters.
     */
    public HyetographGenerator(
            double peakPosition,
            TimeInterval timeInterval,
            DesignRain designRain){

        this.peakPosition = peakPosition;
        this.designRain = designRain;
        this.timeStep = timeInterval;
        
        TimeInterval duration = designRain.getDuration();
        
        // Internal calculations are in minutes.
        totalDuration_min = duration.getInterval(TimeInterval.TimeIntervalUnit.MINUTE);
        ciclo_in_min = timeInterval.getInterval(TimeInterval.TimeIntervalUnit.MINUTE);
        
    }

    /**
     * Generates an hyetograph with the given shape.
     * @param hyetographShape The hyetograph shape.
     * @param corr
     * @return An design hyetograph.
     * @throws Exception 
     */
    public Hyetograph generateHytegraph(HyetographShape hyetographShape,
            boolean corr) throws Exception{

        double totale = designRain.getTotalRain();
        double resto = totalDuration_min%ciclo_in_min;
        resto = Math.round(resto/ciclo_in_min)/ciclo_in_min;
        int nStep = (int) Math.round(totalDuration_min/ciclo_in_min);
        totalDuration_min = nStep*ciclo_in_min + resto;

        // Array: first column is time, second column is rainfall
        double[][] prec = new double[2][nStep+1];
        int precipSteps = (int)Math.round(totalDuration_min/ciclo_in_min);
        for(int s=0; s<=nStep; s++){
            prec[0][s] = s * ciclo_in_min;
        }

        switch(hyetographShape){
            case CONSTANT:
                prec[1] = getConst(precipSteps, totale);
                break;
            case WALLINGFORD:
                prec[1] = getWallingford(precipSteps, totale, corr);
                break;
            case TRIANGULAR:
                prec[1] = getTriangular(precipSteps, totale, corr);
                break;
            case ALTERNATINGBLOCK:
                prec[1] = getAlternBlocks(precipSteps, totale, corr);
                break;
            case INSTANT:
                prec[1] = getInstant(precipSteps, totale, corr);
                break;
            case INCREASING:
                prec[1] = getIncreasing(precipSteps, totale, corr);
                break;
        }

        Hyetograph hyetograph = new Hyetograph(timeStep, prec[1]);
        return hyetograph;
    }
    
    /**
     * Creates a constant hyetograph.
     * @param precipSteps
     * @param totale
     * @return 
     */
    private double[] getConst(int precipSteps, double totale){

        double[] prec = new double[precipSteps+1];
        double single = totale / precipSteps;
        for(int k = 1; k <= precipSteps; k += 1){
            prec[k] = single;
        }
        
        return Arrays.copyOfRange(prec, 1, prec.length);

    }

    /**
     * Creates a Wallingford hyetograph.
     * @param precipSteps
     * @param totale
     * @param corr
     * @return 
     */
    private double[] getWallingford(int precipSteps, double totale, boolean corr){

        double[] prec = new double[precipSteps+1];

        //double AVANZAMENTO = 0.50;
        double WB = 0.35;
        double WC = -4.2;
        double WD = 11.0;
        double [] w = new double[5000];
        double resto;
        int k;
// -------------------------- gli intervalli devono essere dispari
        int nn_intv;
        if ((precipSteps % 2) == 0) {
             nn_intv = precipSteps - 1;
        }
        else {
             nn_intv = precipSteps;
        }
        int istep_picco = (int)(nn_intv*peakPosition) + 1;
// ---------------------- l'avanzamento concide con l'asse del picco
        double av_reale = ((float)(istep_picco)-0.5)*ciclo_in_min;
        double somma = 0.;
// ----------------------------------------------- ramo ascendente
        for (k=1; k<=istep_picco; k++) {
             w[k] = WB + Math.exp(WC + WD * ((float)(k-1)/(float)(nn_intv-1)));
             somma = somma + w[k];
        }
// ---------------------------------------------- ramo discendente
        for (k=istep_picco+1; k<=precipSteps; k++) {
             w[k] = WB + Math.exp(WC + WD * ((float)(nn_intv-k)/(float)(nn_intv-1)));
             somma = somma + w[k];
        }
// ----------------------------------------------------------------
        for (k = 1; k<=precipSteps; k++){
             prec[k] = totale * w[k]/somma;
        }
        double uncorr = 0.;
        for( k = 1; k<=precipSteps; k++){
             uncorr = uncorr + prec[k];
        }
        if(corr) {
            resto = 1. + ((totale - uncorr) / uncorr);
            for (k = 1; k<=precipSteps; k++){
                 prec[k] = prec[k] * resto;
            }
        }
        
        return Arrays.copyOfRange(prec, 1, prec.length);

    }

    /**
     * Creates a triangular hyetograph.
     * @param precipSteps
     * @param totale
     * @param corr
     * @return 
     */
    private double[] getTriangular(int precipSteps, double totale, boolean corr){

        double[] prec = new double[precipSteps+1];

        //double AVANZAMENTO = 0.45;
        double tempo;
        double coeff;
        double resto;
        int k;
// ---------------------------------------------------------------------
        double picco  = (2.*totale)/precipSteps;
        int istep_picco = (int)(precipSteps*peakPosition)+1;
// ---------------------- l'avanzamento concide con l'asse del picco
        double avanz_reale = ((float)istep_picco-0.5)*ciclo_in_min;
// ----------------------------------------- ramo ascendente
        for (k = 1; k <= istep_picco; k++){
            tempo = ((float)k-0.5)*ciclo_in_min;
            prec[k] = (picco/avanz_reale) * tempo;
        }
// ----------------------------------------- ramo discendente
        coeff = -picco/(totalDuration_min-avanz_reale);
        for (k = istep_picco+1; k <= precipSteps; k++){
            tempo = ((float)k-0.5)*ciclo_in_min;
            prec[k] = coeff * (tempo - avanz_reale) + picco;
        }
        double uncorr = 0.;
        for (k = 1; k <= precipSteps; k++){
                    uncorr = uncorr + prec[k];
        }
        if (corr) {
            resto = 1. + ((totale - uncorr) / uncorr);
            for (k = 1; k <= precipSteps; k++){
                   prec[k] = prec[k] * resto;
            }
        }
        
        return Arrays.copyOfRange(prec, 1, prec.length);

    }

    /**
     * Creates an alternating blocks hyetograph.
     * @param precipSteps
     * @param totale
     * @param corr
     * @return 
     */
    private double[] getAlternBlocks(int precipSteps, double totale, boolean corr){

        double[] prec = new double[precipSteps+1];
//        double AVANZAMENTO = 0.50;
        double [] pp = new double[precipSteps+1];
        double dummy;
        double resto;

// ---------------------------------------------------------------------
        int istep_picco = (int)Math.round(precipSteps*peakPosition);
//----- dovrebbe essere equivalente a un NINT(real(precipSteps)*AVANZAMENTO
// --------------------------------------- se dispari è centrale
// --------------------------------------- se pari è anticipato
// ---------------------- l'avanzamento concide con l'asse del picco
        double av_reale = ((float)istep_picco-0.5)*ciclo_in_min;
        for (int k = 1; k <= precipSteps; k++){
            if(totalDuration_min >= 60){
                pp[k] = designRain.getLsppParamA() * Math.pow(((float)k*ciclo_in_min/60.), designRain.getLsppParamN());
            }else{
                pp[k] = designRain.getLsppParamA() * Math.pow(((float)k*ciclo_in_min/60.), designRain.getLsppParamNLess1Hour());
            }
        }
        for (int k=precipSteps; k >=2; k--){
            pp[k] = pp[k]-pp[k-1];
        }
// ------------------------------------------ ordinamento decrescente
        int no_gap = (precipSteps+1) / 2;
        while (no_gap > 0){
           for (int i = no_gap; i<=(precipSteps+1); i++){
               for (int j = (i-no_gap-1); j>=1;  j=j-no_gap){
                   if (pp[j] > pp[j+no_gap]) break;
// ..................................................... swap
                   dummy        = pp[j];
                   pp[j]        = pp[j+no_gap];
                   pp[j+no_gap] = dummy;
               }
           }
           no_gap = no_gap / 2;
        }     
// ----------------------------------------------------------------      
        prec[istep_picco] = pp[1];
        int j=1;
        for (int k = 1; k <= precipSteps; k++) {
             if(istep_picco-k >= 1 && istep_picco-k <= precipSteps) {
                 j++;
                 prec[istep_picco-k] = pp[j];
             }
             if(istep_picco+k >= 1 && istep_picco+k <= precipSteps) {
                 j++;
                 prec[istep_picco+k] = pp[j];
             }
        }
        if ((precipSteps % 2) == 0) {
            prec[precipSteps] = pp[precipSteps];
        }    
        double uncorr = 0.;
        for (int k = 1; k<=precipSteps; k++){
             uncorr = uncorr + prec[k];
        }
        if (corr) {
             resto = 1. + ((totale - uncorr) / uncorr);
             for (int k = 1; k<=precipSteps; k++) {
                  prec[k] = prec[k] * resto;
              }
        }

        return Arrays.copyOfRange(prec, 1, prec.length);

    }

    /**
     * Creates an instant intensity hyetograph.
     * @param precipSteps
     * @param totale
     * @param corr
     * @return 
     */
    private double[] getInstant(int precipSteps, double totale, boolean corr){

        // Peak position cannot be 1
        peakPosition = Math.min(peakPosition, 0.99);
        
        double[] prec = new double[precipSteps+1];

        //double AVANZAMENTO = 0.50;
        double [] pm = new double[5000];
        double    resto;
        int k, j;
// --------------------------------- pm vettore di pioggia di un minuto
        int n_minuti = (int)Math.floor(totalDuration_min);
        if ((n_minuti % 2)== 0) n_minuti = n_minuti - 1;
// ---------------------- l'avanzamento concide con l'asse del picco
        double av_reale = peakPosition*(float)n_minuti;
// ---------------------------------------------------------------
        double uncorr = 0.;
        double ta0 = av_reale;
        double td0 = ta0 / peakPosition;
        
        double in0;
        if(totalDuration_min >= 1){
            in0 = designRain.getLsppParamA() * designRain.getLsppParamN() * Math.pow((td0/60.),(designRain.getLsppParamN()-1.0));
        }else{
            in0 = designRain.getLsppParamA() * designRain.getLsppParamNLess1Hour() * Math.pow((td0/60.),(designRain.getLsppParamNLess1Hour()-1.0));
        }
        for ( k = 1;  k<=totalDuration_min; k++){
            double ta1 = Math.abs(av_reale - k);
            double td1 = ta1 / peakPosition ;
            double in1;
            if(totalDuration_min > 1){
                in1 = designRain.getLsppParamA() * designRain.getLsppParamN() * Math.pow((td1/60.),(designRain.getLsppParamN()-1.0));
            }else{
                in1 = designRain.getLsppParamA() * designRain.getLsppParamNLess1Hour() * Math.pow((td1/60.),(designRain.getLsppParamNLess1Hour()-1.0));
            }
            pm[k] = (in0+in1)/120.;					 // 0.5 * (in0+in1) * 1/60
            uncorr = uncorr + pm[k];
            in0	= in1;
         }
         if (corr) {
             resto = 1. + ((totale - uncorr) / uncorr);
             for (k = 1;  k<=totalDuration_min; k++){
                  pm[k] = pm[k] * resto;
             }
         }
// --------------------------------------------------------------------
         for (k=1; k<=precipSteps; k++){
              prec[k] = 0.;
              for (j=1; j<=ciclo_in_min; j++){
                  prec[k] = prec[k] + pm[(k-1)*(int)(ciclo_in_min)+j];
              }
         }

        return prec;

    }

    /**
     * Creates a constantly increasing hyetograph.
     * @param precipSteps
     * @param totale
     * @param corr
     * @return 
     */
    private double[] getIncreasing(int precipSteps, double totale, boolean corr){

        double[] prec = new double[precipSteps+1];

        peakPosition = 1;
        double tempo;
        double coeff;
        double resto;
        int k;
// ---------------------------------------------------------------------
        double picco  = (2.*totale)/precipSteps;
        int istep_picco = (int)(precipSteps*peakPosition)+1;
// ---------------------- l'avanzamento concide con l'asse del picco
        double avanz_reale = ((float)istep_picco-0.5)*ciclo_in_min;
// ----------------------------------------- ramo ascendente
        for (k = 1; k < istep_picco; k++){
            tempo = ((float)k-0.5)*ciclo_in_min;
            prec[k] = (picco/avanz_reale) * tempo;
        }
// ----------------------------------------- ramo discendente
        coeff = -picco/(totalDuration_min-avanz_reale);
        for (k = istep_picco+1; k <= precipSteps; k++){
            tempo = ((float)k-0.5)*ciclo_in_min;
            prec[k] = coeff * (tempo - avanz_reale) + picco;
        }
        double uncorr = 0.;
        for (k = 1; k <= precipSteps; k++){
                    uncorr = uncorr + prec[k];
        }
        if (corr) {
            resto = 1. + ((totale - uncorr) / uncorr);
            for (k = 1; k <= precipSteps; k++){
                   prec[k] = prec[k] * resto;
            }
        }
        
        return Arrays.copyOfRange(prec, 1, prec.length);

    }

    /**
     * Returns the peak position.
     * @return The peak position (0-1).
     */
    public double getPeakPosition(){
        return peakPosition;
    }

    /**
     * Returns the time step.
     * @return The time step.
     */
    public TimeInterval getTimeStep() {
        return timeStep;
    }

    public void setRainfall(DesignRain designRain) {
        this.designRain = designRain;
    }

    public enum HyetographShape{
        
            CONSTANT, WALLINGFORD, TRIANGULAR, ALTERNATINGBLOCK, INSTANT, INCREASING 
        
    }
    
    private double peakPosition = 0;
    private double totalDuration_min;
    private DesignRain designRain = null;
    private final TimeInterval timeStep;
    private final double ciclo_in_min;    
    
}
