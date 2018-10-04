package com.geomaticaeambiente.openjump.klem.rastertools.classifiers;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;

public class GivenIntervals implements ClassAlgo {
    public final DoubleBasicGrid grid;
    public double interval;
    public final Double baseVal;

    public GivenIntervals(DoubleBasicGrid grid, double interval, Double base) {
        this.grid = grid;
        this.interval = interval;
        baseVal = base;
    }

    @Override
    public double[] getBreakValues() {
        double minVal = Double.MAX_VALUE;
        double maxVal = -4.9E-324D;
        for (int r = 0; r < grid.getRowCount(); r++) {
            for (int c = 0; c < grid.getColumnCount(); c++) {
                final double value = grid.getValue(c, r);
                if (!grid.isNoData(value)) {
                    if (value < minVal) {
                        minVal = value;
                    }
                    if (value > maxVal) {
                        maxVal = value;
                    }
                }
            }
        }
        int intvCount = (int) ((maxVal - minVal) / interval);
        if (intvCount > 100) {
            intvCount = 100;
            interval = ((maxVal - minVal) / 100.0D);
        }
        if (baseVal != null) {
            if (baseVal.doubleValue() == 0.0D) {
                minVal = (int) (minVal / interval) * interval;
            }
            if ((baseVal.doubleValue() > 0.0D) && (minVal > 0.0D)) {
                minVal = (int) (minVal / baseVal.doubleValue())
                        * baseVal.doubleValue();
            } else if ((baseVal.doubleValue() < 0.0D) && (minVal < 0.0D)) {
                minVal = (int) (baseVal.doubleValue() / minVal)
                        * baseVal.doubleValue();
            } else if ((baseVal.doubleValue() < 0.0D) && (minVal > 0.0D)) {
                minVal = (int) (Math.abs(minVal) / Math.abs(baseVal
                        .doubleValue())) * Math.abs(baseVal.doubleValue());
            }
        }
        double endVal = minVal;

        final double[] breaks = new double[intvCount];
        for (int i = 0; i < intvCount; i++) {
            final double startVal = endVal;
            endVal = startVal + interval;
            breaks[i] = endVal;
        }
        return breaks;
    }
}
