package com.geomaticaeambiente.openjump.klem.routing;

import com.geomaticaeambiente.openjump.klem.units.Area;
import com.geomaticaeambiente.openjump.klem.units.Length;
import com.geomaticaeambiente.openjump.klem.units.Speed;
import com.geomaticaeambiente.openjump.klem.units.Time;

/**
 *
 * @author deluca
 */
public class RoutingTimeAdvancedParameters extends RoutingTimeParameters {

    public RoutingTimeAdvancedParameters(
            Speed slopeMinVelocity, Speed slopeMaxVelocity, double velocityK,
            Speed channelVelocity,
            Area lowerThresholdMin, Area lowerThresholdMax, double thresholdK,
            Area upperThrehsold, double exponent, Area watershedArea) {
       super(calcSlopeVelocity(slopeMinVelocity, slopeMaxVelocity, velocityK, watershedArea),
               channelVelocity,
               calcLowerThreshold(lowerThresholdMin, lowerThresholdMax, thresholdK, watershedArea),
               upperThrehsold, exponent);
       
    }
    
    private static Speed calcSlopeVelocity(Speed slopeMinVelocity, Speed slopeMaxVelocity, double velocityK, Area watershedArea) {
        
        double minVel = slopeMinVelocity.getSpeed(Speed.SpeedUnit.m_s);
        double maxVel = slopeMaxVelocity.getSpeed(Speed.SpeedUnit.m_s);
        
        return new Speed(
                new Length(calcYValue(minVel, maxVel, velocityK, watershedArea.getArea(Length.LengthUnit.m), true), Length.LengthUnit.m),
                new Time(1, Time.TimeIntervalUnit.s));
        
    }
    
    private static Area calcLowerThreshold(Area lowerThresholdMin, Area lowerThrehsoldMax, double thresholdK, Area watershedArea) {
        
        return new Area(
                calcYValue(lowerThresholdMin.getArea(Length.LengthUnit.m), lowerThrehsoldMax.getArea(Length.LengthUnit.m), thresholdK, watershedArea.getArea(Length.LengthUnit.m), false),
                Length.LengthUnit.m);
        
    }
    
    
    private static double calcYValue(double minY, double maxY, double k, double x, boolean decreasing) {
    
        if(decreasing) {
            return minY + (maxY - minY) * Math.exp(-k * x);
        } else {
            return maxY + (minY - maxY) * Math.exp(-k * x);
        }        
        
    }
    
}
