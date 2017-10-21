package org.openjump.core.ui.plugin.colorchooser;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import language.I18NPlug;

import org.openjump.core.ui.plugin.colorchooser.utils.ColorUtils;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollectionWrapper;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.geom.EnvelopeUtil;
import com.vividsolutions.jump.workbench.WorkbenchContext;
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

    protected void gestureFinished() throws Exception {
        reportNothingToUndoYet();
        try {
            final WorkbenchContext wbcontext = this.getWorkbench().getContext();
            Layer layer = wbcontext.createPlugInContext().getSelectedLayer(0);
            FeatureCollectionWrapper fcw = layer.getFeatureCollectionWrapper();
            FeatureSchema schema = fcw.getFeatureSchema();
            Coordinate coord = (Coordinate) getCoordinates().get(0);
            Point2D point = getPanel().getViewport().toViewPoint(coord);
            int PIXEL_BUFFER = 2;
            // if (schema.hasAttribute(R_G_B)) {
            Map map = SpecifyFeaturesTool.layerToSpecifiedFeaturesMap(panel
                    .getLayerManager().iterator(), EnvelopeUtil.expand(
                    new Envelope(panel.getViewport().toModelCoordinate(point)),
                    PIXEL_BUFFER / panel.getViewport().getScale()));

            String hex = findValue(R_G_B, map);
            Color color = ColorUtils.hexToColorRGB(hex);
            FeatureColorChooserPlugIn.colorSetbutton.setColor(color);

            String acad = ColorUtils.getColorIndexRegistry(hex);
            this.getWorkbench()
                    .getContext()
                    .getWorkbench()
                    .getFrame()
                    .setStatusMessage(
                            I18NPlug.getI18N("color") + " - " + "Index color: "
                                    + acad + "   Hex: " + hex + "   RGB: "
                                    + color.getRed() + "," + color.getGreen()
                                    + "," + color.getBlue(), 5000);

        } catch (Exception e) {
            this.getWorkbench().getContext().getWorkbench().getFrame()
                    .setStatusMessage(I18NPlug.getI18N("msg1"), 5000);
        }
    }

    @Override
    public Icon getIcon() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Cursor getCursor() {
        return createCursor(new ImageIcon(getClass().getResource(
                "pick-color-cursor.gif")).getImage()); //$NON-NLS-1$
    }

    private String findValue(String attributeName,
            Map layerToSpecifiedFeaturesMap) {
        for (Iterator i = layerToSpecifiedFeaturesMap.keySet().iterator(); i
                .hasNext();) {
            Layer layer = (Layer) i.next();
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
