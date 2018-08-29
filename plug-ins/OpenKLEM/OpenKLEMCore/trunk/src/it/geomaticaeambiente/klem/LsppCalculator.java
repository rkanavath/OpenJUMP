package it.geomaticaeambiente.klem;

/**
 * This class holds the LSPP parameters: a, n, and n for rainfall durations
 * shorter than 1 hour.
 * @author AdL
 */
public class LsppCalculator {
    
    /**
     * Constructs a set of parameters.
     * @param parA
     * @param parN
     * @param parNLess1Hour 
     */
    public LsppCalculator(double parA, double parN, double parNLess1Hour){
        lsppANParameters = new LsppANParameters(parA, parN, parNLess1Hour);      
    }    
   
    /**
     * Constructs the a, n and n less than an hour from the A, Cv and N parameters
     * derived from the GEV or Gumbel LSPP models.
     * @param parAa The GEV/Gumbel a parameter.
     * @param parCv The GEV/Gumbel cv parameter.
     * @param parNn The GEV/Gumbel n parameter.
     * @param nLessThan1Hour The LSPP n value for rainfall duration shorter than
     * 1 hour.
     * @param lsppModel The LSPP model of the input parameters: GEV or Gumbel.
     * @param returnTime The return time (years).
     */
    public LsppCalculator(double parAa, double parCv, double parNn, double nLessThan1Hour,
            LsppModel lsppModel, double returnTime){
      
        double parA = 0;
        double parN = 0;
        switch(lsppModel){
            case GEV:
                double gamma = 1.058363;
                double rkappa = -0.087;
                double alfa = (parCv * rkappa) / ((1-Math.pow(2, - rkappa))*gamma);
                double epsilon = 1 - ((alfa * (1-gamma))/rkappa);
                parA = parAa * (epsilon + (alfa/rkappa)*
                        (1-Math.pow((Math.log(returnTime/(returnTime-1.))), rkappa)));
                parN = parNn;
                break;
            case GUMBEL:
                double eulero = 0.5772157;
                parA = parAa*(1-(((parCv*Math.sqrt(6.))/3.1416)*
                        (eulero+Math.log(Math.log(returnTime/(returnTime-1.))))));        
                parN = parNn;
            case TCEV:
                // TODO
                break;
        }
        
        lsppANParameters = new LsppANParameters(parA, parN, nLessThan1Hour);
        
    }

    /**
     * Returns the LSPP a and n parameters.
     * @return The LSPP parameters.
     */
    public LsppANParameters getParamsAN(){
        return lsppANParameters;
    }
    
    private LsppANParameters lsppANParameters = null;
    
    public enum LsppModel{
        GEV, GUMBEL, TCEV
    }
    
    public class LsppANParameters {
    
        public LsppANParameters(double paramA, double paramN, double paramNLess1Hour){
        
            this.paramA = paramA;
            this.paramN = paramN;
            this.paramNLess1Hour = paramNLess1Hour;
            
        }
        
        public double getParamA() { return paramA; }
        public double getParamN() { return paramN; }
        public double getParamNLess1Hour() { return paramNLess1Hour; }
        
        private double paramA = 0;
        private double paramN = 0;
        private double paramNLess1Hour = -1;
        
    
    }
    
    
}
