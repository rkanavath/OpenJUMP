package org.openjump.advancedtools.tools;

import java.awt.Shape;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;

import javax.swing.Icon;

import org.openjump.advancedtools.config.CADToolsOptionsPanel;
import org.openjump.advancedtools.icon.IconLoader;
import org.openjump.advancedtools.language.I18NPlug;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.util.GeometricShapeFactory;
import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.ui.LayerNamePanelProxy;
import com.vividsolutions.jump.workbench.ui.cursortool.CursorTool;
import com.vividsolutions.jump.workbench.ui.cursortool.DragTool;
import com.vividsolutions.jump.workbench.ui.cursortool.editing.FeatureDrawingUtil;

public class EllipseByDraggingTool extends DragTool {
    private FeatureDrawingUtil featureDrawingUtil;
    static final String drawConstrainedCircle = I18N
            .get("org.openjump.core.ui.plugins.edittoolbox.cursortools.DrawConstrainedCircleTool.Draw-Constrained-Circle");
    static final String theCircleMustHaveAtLeast2Points = I18N
            .get("org.openjump.core.ui.plugins.edittoolbox.cursortools.DrawConstrainedCircleTool.The-circle-must-have-at-least-2-points");

    /** Plugin name */
    public final static String NAME = I18NPlug
            .getI18N("org.openjump.core.ui.plugins.Ellipse");

    protected Coordinate tentativeCoordinate;

    public EllipseByDraggingTool(int n) {
    }

    private EllipseByDraggingTool(FeatureDrawingUtil featureDrawingUtil) {
        this.featureDrawingUtil = featureDrawingUtil;
    }

    public EllipseByDraggingTool(EnableCheckFactory checkFactory) {
    }

    public static CursorTool create(LayerNamePanelProxy layerNamePanelProxy) {
        FeatureDrawingUtil featureDrawingUtil = new FeatureDrawingUtil(
                layerNamePanelProxy);

        return featureDrawingUtil.prepare(new EllipseByDraggingTool(
                featureDrawingUtil), true);
    }

    @Override
    public String getName() {

        String tooltip = "";
        tooltip = "<HTML><BODY>";

        tooltip += "<b>" + NAME + "</b>";

        tooltip += "</BODY></HTML>";
        return tooltip;
    }

    @Override
    public Icon getIcon() {
        return IconLoader.icon("drawEllipse.png");
    }

    @Override
    protected void gestureFinished() throws Exception {
        reportNothingToUndoYet();
        getPanel().setViewportInitialized(true);

        if (CADToolsOptionsPanel.isGetAsPolygon()) {
            execute(featureDrawingUtil.createAddCommand(getEllipsePolygon(),
                    isRollingBackInvalidEdits(), getPanel(), this));
        } else {
            execute(featureDrawingUtil.createAddCommand(getEllipseLineString(),
                    isRollingBackInvalidEdits(), getPanel(), this));
        }

        if (CADToolsOptionsPanel.isGetCentroid()) {
            execute(featureDrawingUtil.createAddCommand(getCenter(),
                    isRollingBackInvalidEdits(), getPanel(), this));
        }

    }

    protected Geometry getEllipsePolygon()
            throws NoninvertibleTransformException {
        Envelope env = new Envelope(getModelSource().x,
                getModelDestination().x, getModelSource().y,
                getModelDestination().y);
        GeometricShapeFactory gsf = new GeometricShapeFactory();
        gsf.setNumPoints(100);
        gsf.setEnvelope(env);

        Geometry geom = gsf.createCircle();

        return geom;
    }

    protected Geometry getEllipseLineString()
            throws NoninvertibleTransformException {
        Envelope env = new Envelope(getModelSource().x,
                getModelDestination().x, getModelSource().y,
                getModelDestination().y);
        GeometricShapeFactory gsf = new GeometricShapeFactory();
        gsf.setNumPoints(100);
        gsf.setEnvelope(env);

        Geometry geom = gsf.createCircle();
        Coordinate[] coords = geom.getCoordinates();
        GeometryFactory factory = new GeometryFactory();

        return factory.createLineString(coords);
    }

    protected Geometry getCenter() throws NoninvertibleTransformException {
        Envelope env = new Envelope(getModelSource().x,
                getModelDestination().x, getModelSource().y,
                getModelDestination().y);
        GeometricShapeFactory gsf = new GeometricShapeFactory();
        gsf.setNumPoints(100);
        gsf.setEnvelope(env);

        Geometry geom = gsf.createCircle().getCentroid();

        return geom;
    }

    @Override
    protected Shape getShape(Point2D source, Point2D destination)
            throws Exception {
        GeneralPath shape = new GeneralPath();
        double minX = Math.min(source.getX(), destination.getX());
        double minY = Math.min(source.getY(), destination.getY());
        double maxX = Math.max(source.getX(), destination.getX());
        double maxY = Math.max(source.getY(), destination.getY());
        double width = Math.max(source.getX(), destination.getX())
                - Math.min(source.getX(), destination.getX());
        double height = Math.max(source.getY(), destination.getY())
                - Math.min(source.getY(), destination.getY());

        shape.append(new Ellipse2D.Double(minX, minY, width, height), true);
        shape.moveTo(minX, (maxY + minY) / 2.0D);
        shape.lineTo(maxX, (maxY + minY) / 2.0D);
        shape.moveTo((maxX + minX) / 2.0D, minY);
        shape.lineTo((maxX + minX) / 2.0D, maxY);
        return shape;
    }
}
