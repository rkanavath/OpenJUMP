package com.geomaticaeambiente.openjump.klem.routing;

import com.geomaticaeambiente.openjump.klem.units.Area;
import com.geomaticaeambiente.openjump.klem.units.Speed;

/**
 *
 * @author AdL
 */
public class RoutingTimeParameters {

    public RoutingTimeParameters(
            Speed slopeVelocity, Speed channelVelocity,
            Area lowerThreshold, Area upperThreshold, double exponent) {
        this.slopeVelocity = slopeVelocity;
        this.channelVelocity = channelVelocity;
        this.lowerThreshold = lowerThreshold;
        this.upperThreshold = upperThreshold;
        this.exponent = exponent;
    }

    public Speed getSlopeVelocity() {
        return slopeVelocity;
    }

    public Speed getChannelVelocity() {
        return channelVelocity;
    }

    public Area getMinThreshold() {
        return lowerThreshold;
    }

    public Area getMaxThreshold() {
        return upperThreshold;
    }
    
    public double getExponent() {
        return exponent;
    }
    
    private final Speed slopeVelocity;
    private final Speed channelVelocity;
    private final Area lowerThreshold;
    private final Area upperThreshold;
    private final double exponent;
    
}
