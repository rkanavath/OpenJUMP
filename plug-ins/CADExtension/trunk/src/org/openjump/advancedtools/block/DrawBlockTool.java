package org.openjump.advancedtools.block;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Shape;
import java.awt.event.MouseEvent;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;
import java.io.IOException;

import javax.swing.ImageIcon;

import org.openjump.advancedtools.language.I18NPlug;
import org.openjump.advancedtools.utils.WorkbenchUtils;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jump.geom.CoordUtil;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.LayerNamePanelProxy;
import com.vividsolutions.jump.workbench.ui.LayerViewPanel;
import com.vividsolutions.jump.workbench.ui.cursortool.CursorTool;
import com.vividsolutions.jump.workbench.ui.cursortool.NClickTool;
import com.vividsolutions.jump.workbench.ui.cursortool.editing.FeatureDrawingUtil;

public class DrawBlockTool extends NClickTool {

    public static final String blockFolder = "VertexImages";
    public static final String NAME = I18NPlug
            .getI18N("org.openjump.core.ui.plugins.block.DrawblockTool.description");

    private FeatureDrawingUtil featureDrawingUtil;
    private ImageIcon ICON = org.openjump.advancedtools.icon.IconLoader
            .icon("textblock/block_ins.png");

    public DrawBlockTool(FeatureDrawingUtil featureDrawingUtil) {
        super(1);
        this.featureDrawingUtil = featureDrawingUtil;
        setColor(Color.blue);
        setStroke(new BasicStroke(1.5F));

        allowSnapping();
    }

    public static CursorTool create(LayerNamePanelProxy layerNamePanelProxy) {
        FeatureDrawingUtil featureDrawingUtil = new FeatureDrawingUtil(
                layerNamePanelProxy);

        return featureDrawingUtil.prepare(
                new DrawBlockTool(featureDrawingUtil), true);
    }

    public void initialize(PlugInContext context) throws Exception {
    }

    @Override
    public String getName() {
        return NAME;
    }

    @Override
    @SuppressWarnings({})
    protected void gestureFinished() throws Exception {
        getWorkbench().getContext();
        // Get the click coordinates
        Coordinate cursorPt = (Coordinate) getCoordinates().get(0);

        // Build the geometry at the specific displacement, dimension and
        // rotation
        // Geometry geom2 = BlockUtils.getGeometry(context);
        Geometry geom2 = BlockUtils.getGeometry();

        // Get the centroid coordinates of the geometry
        Coordinate coord = geom2.getEnvelope().getCentroid().getCoordinate();

        // Calculate the displacement of the geometry
        Coordinate displacement = CoordUtil.subtract(cursorPt, coord);
        Integer rotation = (Integer) BlockPanel.rotationSpinner.getValue();
        Integer dimension = (Integer) BlockPanel.dimensionSpinner.getValue();
        GeometryUtils.rotate_clockwise_Geometry(geom2, rotation);
        GeometryUtils.scaleGeometry(geom2, dimension);
        GeometryUtils.centerGeometry(geom2, displacement);

        execute(featureDrawingUtil.createAddCommand(geom2,
                isRollingBackInvalidEdits(), getPanel(), this));

    }

    @Override
    public ImageIcon getIcon() {
        return ICON;
    }

    /****************** other methods ********************************/

    /** Modify using #setDestination */
    protected Coordinate modelDestination = null;
    private Shape selectedFeaturesShape;

    /**
     * overwritten super method to show the block geometry on any mouse move
     */
    @Override
    public void mouseMoved(MouseEvent e) {
        try {
            setViewDestination(e.getPoint());
            redrawShape();
        } catch (Throwable t) {
            getPanel().getContext().handleThrowable(t);
        }
    }

    /**** from drag tool ***/
    protected void setViewDestination(Point2D destination)
            throws NoninvertibleTransformException {
        this.setModelDestination(getPanel().getViewport().toModelCoordinate(
                destination));
    }

    protected void setModelDestination(Coordinate destination) {
        this.modelDestination = snap(destination);
    }

    /**
     * changed to get geometry around mouse pointer
     */
    @Override
    protected Shape getShape() {

        this.setColor(Color.RED.brighter());
        try {
            this.calculateShape(this.modelDestination, this.getPanel());
        } catch (IOException | ParseException e) {
            WorkbenchUtils.Logger(this.getClass(), e);
            e.printStackTrace();
        }
        return this.selectedFeaturesShape;
    }

    /**
     * called from constructor and by mouse move event
     * <p>
     * calculates a geometry around the mouse pointer and converts it to a java
     * shape
     * 
     * @param middlePoint
     *            coordinates of the circle
     * @throws ParseException
     * @throws IOException
     */
    private void calculateShape(Coordinate middlePoint, LayerViewPanel panel)
            throws IOException, ParseException {
        getWorkbench().getContext();
        // Get the click coordinates
        Coordinate cursorPt = this.modelDestination;
        Geometry geom2 = BlockUtils.getGeometry();
        // Geometry geom2 = BlockUtils.getGeometry(context);

        // Get the centroid coordinates of the geometry
        Coordinate coord = geom2.getEnvelope().getCentroid().getCoordinate();

        // Calculate the displacement of the geometry
        Coordinate displacement = CoordUtil.subtract(cursorPt, coord);
        Integer rotation = (Integer) BlockPanel.rotationSpinner.getValue();
        Integer dimension = (Integer) BlockPanel.dimensionSpinner.getValue();
        GeometryUtils.rotate_clockwise_Geometry(geom2, rotation);
        GeometryUtils.scaleGeometry(geom2, dimension);
        GeometryUtils.centerGeometry(geom2, displacement);

        try {
            this.selectedFeaturesShape = panel.getJava2DConverter().toShape(
                    geom2);
        } catch (NoninvertibleTransformException e) {
            System.out.println("DrawCircleWithGivenRadiusTool:Exception " + e);
        }
    }

}