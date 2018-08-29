package com.geomaticaeambiente.klemgui.ui;

import java.awt.Color;

import org.openjump.core.rasterimage.RasterSymbology;

public class Symbologies {

    public static RasterSymbology getFlowDirSymb() {

        final RasterSymbology flowDirSymb = new RasterSymbology(
                RasterSymbology.TYPE_SINGLE);

        flowDirSymb.addColorMapEntry(0, Color.GRAY);
        flowDirSymb.addColorMapEntry(1, new Color(67, 187, 0));
        flowDirSymb.addColorMapEntry(2, new Color(255, 178, 0));
        flowDirSymb.addColorMapEntry(4, new Color(238, 71, 18));
        flowDirSymb.addColorMapEntry(8, new Color(204, 153, 255));
        flowDirSymb.addColorMapEntry(16, new Color(0, 134, 208));
        flowDirSymb.addColorMapEntry(32, new Color(0, 166, 255));
        flowDirSymb.addColorMapEntry(64, new Color(158, 212, 255));
        flowDirSymb.addColorMapEntry(128, new Color(0, 125, 59));

        return flowDirSymb;

    }

    public static RasterSymbology getUpslopeAreaSymb(double cellSize)
            throws Exception {

        final RasterSymbology flowDirSymb = new RasterSymbology(
                RasterSymbology.TYPE_INTERVALS);

        final int classesCount = 11;
        // int colorsCount = 2;
        final Color startColor = Color.WHITE;
        final Color endColor = Color.BLUE;

        flowDirSymb.addColorMapEntry(0, startColor);
        for (int c = 1; c < classesCount - 1; c++) {

            final double cellRelDistance = (double) c
                    / (double) (classesCount - 1);
            // double colorRelDistance = cellRelDistance * (colorsCount - 1);

            final Color color = interpolateColor(startColor, endColor,
                    cellRelDistance);
            flowDirSymb.addColorMapEntry(Math.pow(2, c) * cellSize * cellSize,
                    color);

        }
        flowDirSymb.addColorMapEntry(Double.MAX_VALUE, endColor);

        return flowDirSymb;

    }

    private static Color interpolateColor(Color startColor, Color endColor,
            double relDistance) throws Exception {

        if (relDistance < 0 || relDistance > 1) {
            throw new Exception("Relative distance out of range. Must be 0-1.");
        }

        // int red = (int) Math.round((double) c / (double) (colors.length - 1)
        // * 255);
        final int red = interpolate(startColor.getRed(), endColor.getRed(),
                relDistance);
        final int green = interpolate(startColor.getGreen(),
                endColor.getGreen(), relDistance);
        final int blue = interpolate(startColor.getBlue(), endColor.getBlue(),
                relDistance);

        return new Color(red, green, blue);

    }

    private static int interpolate(int startValue, int endValue,
            double relDistance) {

        return (int) Math.round(startValue * (1 - relDistance) + endValue
                * (relDistance));

    }

}
