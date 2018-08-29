package com.geomaticaeambiente.openjump.klem.lspp;

import it.geomaticaeambiente.klem.TimeInterval;

/**
 *
 * @author AdL
 */
public class Lspp {
    
    public double calcGumbelParams(double aa, double cv, double n2, Double nLessThan1Hour, double returnPeriod, TimeInterval duration) {
        
        double eulero = 0.5772157;

        double YT = Math.log(Math.log(returnPeriod / (returnPeriod - 1.)));
        double a = aa * (1-(((cv*Math.sqrt(6.))/Math.PI)*(eulero+Math.log(Math.log(returnPeriod / (returnPeriod - 1.))))));

        double durationHours = duration.getInterval(TimeInterval.TimeIntervalUnit.HOUR);
        
        if(durationHours > 1){
            return a *(1 - (((cv * Math.sqrt(6.)) / Math.PI) * (eulero + YT))) * Math.pow(durationHours, n2);
        } else {
            if(nLessThan1Hour != null && nLessThan1Hour > 0){
                return a * (1-(((cv * Math.sqrt(6.)) / Math.PI) * (eulero + YT))) * Math.pow(durationHours, nLessThan1Hour);
            }else{
                return a * (1-(((cv * Math.sqrt(6.)) / Math.PI) * (eulero + YT))) * Math.pow(durationHours, n2);
            }
        }
 
        
    }
    
    public double calcGEVParams(double aa, double cv, double n2, double returnPeriod, TimeInterval duration) {
        
        double gamma = 1.058363;
        double rkappa = -0.087;
        double alfa  = (cv * rkappa) / ((1-Math.pow(2,(-rkappa)))*gamma);
        double epsilon = 1 - ((alfa * (1-gamma))/rkappa);

        
        double YT = 1-Math.pow(Math.log(returnPeriod / (returnPeriod - 1)), rkappa);
        double a = aa * (epsilon + (alfa / rkappa) * YT);


        return a * Math.pow(duration.getInterval(TimeInterval.TimeIntervalUnit.HOUR), n2);
        
    }
    
    public class LsppParams {

        public LsppParams(double a, double n) {
            this.a = a;
            this.n = n;
        }

        public double getA() {
            return a;
        }

        public double getN() {
            return n;
        }  
        
        private final double a;
        private final double n;
        
    }
    
    
}
