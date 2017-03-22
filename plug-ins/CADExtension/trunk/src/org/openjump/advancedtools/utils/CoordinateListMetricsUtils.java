package org.openjump.advancedtools.utils;

import java.text.DecimalFormat;

import org.openjump.advancedtools.language.I18NPlug;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jump.workbench.JUMPWorkbench;
import com.vividsolutions.jump.workbench.ui.HTMLFrame;

public class CoordinateListMetricsUtils {
    public static final String Radius = I18NPlug
            .getI18N("org.openjump.core.ui.utils.CoordinateListMetrics.radius")
            + ": ";
    public static final String Circum = I18NPlug
            .getI18N("org.openjump.core.ui.utils.CoordinateListMetrics.circumference")
            + ": ";
    public static final String Center = I18NPlug
            .getI18N("org.openjump.core.ui.utils.CoordinateListMetrics.source")
            + ": ";
    public static final String Target = I18NPlug
            .getI18N("org.openjump.core.ui.utils.CoordinateListMetrics.target")
            + ": ";
    public static final String Coordinates = I18NPlug
            .getI18N("org.openjump.core.ui.utils.CoordinateListMetrics.coordinates");

    public static final String Circle = I18NPlug
            .getI18N("org.openjump.core.ui.utils.CoordinateListMetrics.Circle");

    public static final String Measure = I18NPlug
            .getI18N("org.openjump.core.ui.utils.CoordinateListMetrics.Measure");

    public static DecimalFormat df2 = new DecimalFormat("##0.0##");
    public static DecimalFormat df1 = new DecimalFormat("##0.#");

    public static String circleString(double a, double b, Coordinate start,
            Coordinate target) {
        String all = Coordinates + " [" + Center + df1.format(start.x) + " ; "
                + df1.format(start.y) + " - " + Target + df1.format(target.x)
                + " ; " + df1.format(target.y) + "]  " + Radius + df2.format(a)
                + "  " + Circum + df2.format(b);
        return all;

    }

    public static void setCircleMessage(double a, double b, Coordinate start,
            Coordinate target) {
        String all = Coordinates + " [" + Center + df1.format(start.x) + " ; "
                + df1.format(start.y) + " - " + Target + df1.format(target.x)
                + " ; " + df1.format(target.y) + "]  " + Radius + df2.format(a)
                + "  " + Circum + df2.format(b);
        JUMPWorkbench.getInstance().getFrame().getContext().getLayerViewPanel()
                .getContext().setStatusMessage(all);
    }

    public static void setCircleHTMLFrame(double a, double b, Coordinate start,
            Coordinate target) {
        HTMLFrame out = JUMPWorkbench.getInstance().getContext()
                .createPlugInContext().getOutputFrame();
        out.createNewDocument();
        out.addHeader(1, Measure);
        out.addHeader(2, Circle);
        out.addHeader(
                3,
                Coordinates + " [" + Center + df2.format(start.x) + ";"
                        + df2.format(start.y) + " - " + Target
                        + df2.format(target.x) + ";" + df2.format(target.y)
                        + "]  ");
        out.addHeader(3, Radius + df2.format(a));
        out.addHeader(3, Circum + df2.format(b));
    }

    public static void setMessage(String aString, double a) {
        String all = aString + df2.format(a);
        JUMPWorkbench.getInstance().getFrame().getContext().getLayerViewPanel()
                .getContext().setStatusMessage(all);
    }

}
