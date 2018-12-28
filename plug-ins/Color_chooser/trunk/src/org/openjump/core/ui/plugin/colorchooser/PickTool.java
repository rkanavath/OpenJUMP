package org.openjump.core.ui.plugin.colorchooser;

import images.ColorChooserIconLoader;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import javax.swing.Icon;

import language.I18NPlug;

import org.openjump.core.ui.plugin.colorchooser.utils.ColorUtils;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.geom.EnvelopeUtil;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.ui.cursortool.NClickTool;
import com.vividsolutions.jump.workbench.ui.cursortool.SpecifyFeaturesTool;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;

/*
 * [Giuseppe Aruta 2016_12_4] Pick tool allows to get RGB value (if exist) 
 * by clicking on a feature of a selected layer
 * This tool is distributed under GNU license
 */
public class PickTool extends NClickTool {
    public PickTool() {
        super(1);

    }

    public static final String R_G_B = BasicStyle.RGB_ATTRIBUTE_NAME;

    protected Point getPoint() throws NoninvertibleTransformException {
        return new GeometryFactory().createPoint((Coordinate) getCoordinates()
                .get(0));
    }

    @Override
    protected void gestureFinished() throws Exception {
        reportNothingToUndoYet();
        try {
            final Coordinate coord = (Coordinate) getCoordinates().get(0);
            final Point2D point = getPanel().getViewport().toViewPoint(coord);
            final int PIXEL_BUFFER = 2;
            // if (schema.hasAttribute(R_G_B)) {
            final Map map = SpecifyFeaturesTool.layerToSpecifiedFeaturesMap(
                    panel.getLayerManager().iterator(), EnvelopeUtil.expand(
                            new Envelope(panel.getViewport().toModelCoordinate(
                                    point)), PIXEL_BUFFER
                                    / panel.getViewport().getScale()));

            final String hex = findValue(R_G_B, map);
            final Color color = ColorUtils.hexToColorRGB(hex);
            FeatureColorChooserPlugIn.colorSetbutton.setColor(color);

            final String acad = ColorUtils.getColorIndexRegistry(hex);
            getWorkbench()
                    .getContext()
                    .getWorkbench()
                    .getFrame()
                    .setStatusMessage(
                            I18NPlug.getI18N("color") + " - " + "Index color: "
                                    + acad + "   Hex: " + hex + "   RGB: "
                                    + color.getRed() + "," + color.getGreen()
                                    + "," + color.getBlue(), 5000);

        } catch (final Exception e) {
            getWorkbench().getContext().getWorkbench().getFrame()
                    .setStatusMessage(I18NPlug.getI18N("msg1"), 5000);
        }
    }

    @Override
    public String getName() {
        return I18NPlug.getI18N("picker-color");
    }

    @Override
    public Icon getIcon() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Cursor getCursor() {
        return createCursor(ColorChooserIconLoader.image("pipette-cursor.gif")); //$NON-NLS-1$
    }

    private String findValue(String attributeName,
            Map layerToSpecifiedFeaturesMap) {
        for (final Iterator i = layerToSpecifiedFeaturesMap.keySet().iterator(); i
                .hasNext();) {
            final Layer layer = (Layer) i.next();
            for (int j = 0; j < layer.getFeatureCollectionWrapper()
                    .getFeatureSchema().getAttributeCount(); j++) {
                if ("fid".equalsIgnoreCase(attributeName)) {
                    return ""
                            + ((Feature) ((Collection) layerToSpecifiedFeaturesMap
                                    .get(layer)).iterator().next()).getID();
                }
                if (layer.getFeatureCollectionWrapper().getFeatureSchema()
                        .getAttributeName(j).equalsIgnoreCase(attributeName)) {
                    return ""
                            + ((Feature) ((Collection) layerToSpecifiedFeaturesMap
                                    .get(layer)).iterator().next())
                                    .getAttribute(j);
                }
            }
        }
        return "";
    }

}
