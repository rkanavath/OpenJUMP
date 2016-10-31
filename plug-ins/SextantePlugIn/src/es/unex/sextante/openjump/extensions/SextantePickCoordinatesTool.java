package es.unex.sextante.openjump.extensions;

import java.awt.BasicStroke;
import java.awt.Cursor;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.cursortool.CoordinateListMetrics;
import com.vividsolutions.jump.workbench.ui.cursortool.NClickTool;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.NamedPoint;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.openjump.language.I18NPlug;

public class SextantePickCoordinatesTool extends NClickTool {

    /**
     * @description: Following Victor Oyala Blog
     *               (http://sextantegis.blogspot.it/2009
     *               /05/herramientas-para-usuarios-gvsig.html) this
     *               functionality allows to interactive get coordinates of
     *               points from a view. Thos points can be used later on
     *               Sextante Algorithms,
     * 
     * @author Giuseppe Aruta oct 2016
     *
     **/
    PlugInContext context;
    public static final String LAYER_NAME = I18NPlug
            .getI18N("es.unex.sextante.kosmo.extensions.SextantePickCooridnates.pick-coordinates");

    public SextantePickCoordinatesTool(PlugInContext context) {
        super(1);
        this.context = context;
        setStroke(new BasicStroke(2));
        setMetricsDisplay(new CoordinateListMetrics());
        allowSnapping();
    }

    protected Point getPoint() throws NoninvertibleTransformException {
        return new GeometryFactory().createPoint((Coordinate) getCoordinates()
                .get(0));
    }

    @Override
    public Icon getIcon() {
        return new ImageIcon(getClass().getResource("pick_coordinates.png"));
    }

    public Cursor getCursor() {
        return createCursor(new ImageIcon(getClass().getResource(
                "pick_coordinates_tool.gif")).getImage());
    }

    @Override
    protected void gestureFinished() throws Exception {
        reportNothingToUndoYet();
        final Point2D wcPoint = getPanel().getViewport().toViewPoint(
                getPoint().getCoordinate());
        final String sPointName = JOptionPane.showInputDialog(
                null,
                "X: " + Double.toString(wcPoint.getX()) + "\n" + "Y: "
                        + Double.toString(wcPoint.getY()),
                Sextante.getText("New point"));
        if (sPointName != null) {
            final NamedPoint namedPoint = new NamedPoint(sPointName, wcPoint);
            SextanteGUI.getGUIFactory().getCoordinatesList().add(namedPoint);
        }

    }

}
