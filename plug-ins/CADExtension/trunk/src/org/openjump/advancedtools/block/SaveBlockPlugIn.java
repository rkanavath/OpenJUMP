package org.openjump.advancedtools.block;

import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.io.File;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.openjump.advancedtools.icon.IconLoader;
import org.openjump.advancedtools.language.I18NPlug;
import org.openjump.advancedtools.utils.WorkbenchUtils;
import org.saig.core.gui.swing.sldeditor.util.FormUtils;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.geom.CoordUtil;
import com.vividsolutions.jump.workbench.JUMPWorkbench;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;
import com.vividsolutions.jump.workbench.ui.TaskFrame;

public class SaveBlockPlugIn extends AbstractPlugIn {
    public static ImageIcon ICON = IconLoader.icon("textblock/block_save.png");
    public static final String NAME = I18NPlug
            .getI18N("org.openjump.core.ui.plugins.block.SaveBlockPlugIn");
    public static final String Warning1 = I18NPlug
            .getI18N("org.openjump.core.ui.plugins.block.SaveBlockPlugIn.description-title");
    public static final String Warning2 = I18NPlug
            .getI18N("org.openjump.core.ui.plugins.block.SaveBlockPlugIn.warning-positive");
    public static final String Warning3 = I18NPlug
            .getI18N("org.openjump.core.ui.plugins.block.SaveBlockPlugIn.warning-negative");
    public static final String Description = I18NPlug
            .getI18N("org.openjump.core.ui.plugins.block.SaveBlockPlugIn.description");
    static JTextField jTextField_wktOut = new JTextField();
    static JPanel jpanel = new JPanel(new GridBagLayout());
    static JLabel jLabel = new JLabel();
    private static String OUTPUT_FILE = I18NPlug
            .getI18N("org.openjump.core.ui.plugins.block.SaveBlockPlugIn.dialog");
    public static final String blockFolder = "VertexImages";
    Geometry geomSelected = null;

    @Override
    public String getName() {

        String tooltip = "";
        tooltip = "<HTML><BODY>";
        tooltip += "<DIV style=\"width: 320px; text-justification: justify;\">";
        tooltip += "<b>" + NAME + "</b>" + "<br>";
        tooltip += Warning1 + "<br>";
        tooltip += Description + "<br>";
        tooltip += "</DIV></BODY></HTML>";
        return tooltip;
    }

    public ImageIcon getIcon() {
        return ICON;
    }

    @Override
    public void initialize(PlugInContext context) throws Exception {

    }

    public static MultiEnableCheck createEnableCheck(
            WorkbenchContext workbenchContext) {
        EnableCheckFactory checkFactory = new EnableCheckFactory(
                workbenchContext);

        return new MultiEnableCheck().add(
                checkFactory
                        .createWindowWithSelectionManagerMustBeActiveCheck())
                .add(checkFactory.createAtLeastNItemsMustBeSelectedCheck(1));
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean execute(PlugInContext context) throws Exception {
        reportNothingToUndoYet(context);

        if (!(JUMPWorkbench.getInstance().getFrame().getActiveInternalFrame() instanceof TaskFrame)) {
            JUMPWorkbench
                    .getInstance()
                    .getFrame()
                    .warnUser(
                            I18N.get("com.vividsolutions.jump.workbench.plugin.A-Task-Window-must-be-active"));
            return false;
        } else {
            try {
                Collection<?> selectedFeatures = context.getLayerViewPanel()
                        .getSelectionManager()
                        .createFeaturesFromSelectedItems();
                if (selectedFeatures.size() > 1 || selectedFeatures.size() == 0) {
                    context.getWorkbenchFrame().warnUser(Warning1 + "!");
                    return false;
                }
                File pluginDir = context.getWorkbenchContext().getWorkbench()
                        .getPlugInManager().getPlugInDirectory();
                String wd = pluginDir.getAbsolutePath();
                String filenamedir;
                MultiInputDialog dialog = new MultiInputDialog(
                        context.getWorkbenchFrame(), NAME, true);

                dialog.addRow(MainPanel(context));

                GUIUtil.centreOnWindow(dialog);
                dialog.setVisible(true);

                if (!dialog.wasOKPressed()) {
                    return false;
                }
                if (jTextField_wktOut.getText().isEmpty()) {
                    context.getWorkbenchFrame().warnUser(Warning3);
                    return false;
                }

                else {

                    String filename = jTextField_wktOut.getText();

                    for (Iterator<?> k = selectedFeatures.iterator(); k
                            .hasNext();) {
                        Feature featureSelected = (Feature) k.next();
                        geomSelected = featureSelected.getGeometry();
                    }

                    Geometry targetGeom = new GeometryFactory()
                            .createLineString(new Coordinate[] {
                                    new Coordinate(0, 0),
                                    new Coordinate(20, 20) });
                    double targetLength = targetGeom.getLength();
                    double scale = 0;

                    Envelope envSelected = geomSelected.getEnvelopeInternal();

                    Geometry sourceGeom = new GeometryFactory()
                            .createLineString(new Coordinate[] {
                                    new Coordinate(envSelected.getMinX(),
                                            envSelected.getMinY()),
                                    new Coordinate(envSelected.getMaxY(),
                                            envSelected.getMaxY()) });
                    double sourceLength = sourceGeom.getLength();

                    /*
                     * if (sourceLength>22){ scale =
                     * (targetLength/sourceLength)*100; } else{ scale =
                     * (22/sourceLength)*100; }
                     */
                    Geometry newGeom = (Geometry) geomSelected.clone();
                    // Get the centroid coordinates of the geometry
                    Coordinate coord = newGeom.getEnvelope().getCentroid()
                            .getCoordinate();

                    // Calculate the displacement of the geometry
                    Coordinate displacement = CoordUtil.subtract(
                            new Coordinate(0, 0), coord);

                    scale = (targetLength / sourceLength) * 100;

                    GeometryUtils.centerGeometry(newGeom, displacement);
                    GeometryUtils.scaleGeometry(newGeom, scale);

                    filenamedir = wd + File.separator + blockFolder
                            + File.separator + filename;
                    File f = new File(filenamedir);
                    if (f.exists() && f.isDirectory()) {
                        GeometryUtils.writeToFile(newGeom, filenamedir);
                    } else {
                        File dir = new File(wd + File.separator + blockFolder);
                        dir.mkdir();
                        filenamedir = dir.getAbsolutePath() + File.separator
                                + filename;
                        GeometryUtils.writeToFile(newGeom, filenamedir);
                    }

                    context.getWorkbenchFrame().setStatusMessage(Warning2);
                    File file = new File(filenamedir + ".wkt");
                    BlockPanel.blocks.add(file.getName());
                    BlockPanel.model.removeAllElements();
                    Object name2 = "";
                    for (Iterator<?> i = BlockPanel.listOfBlockFiles(context)
                            .iterator(); i.hasNext();) {
                        name2 = i.next();

                        BlockPanel.model.addElement((String) name2);
                    }

                    BlockPanel.chooseBox.revalidate(); // for JFrame up to Java7
                                                       // is
                    // there only validate()
                    BlockPanel.chooseBox.repaint();
                    Integer index = BlockPanel.model.getIndexOf(name2);
                    BlockPanel.chooseBox.setSelectedItem(index);
                }
            } catch (Exception ex) {
                WorkbenchUtils.Logger(this.getClass(), ex);
            }
            return true;
        }
    }

    public static JPanel MainPanel(PlugInContext context) {
        jLabel.setText(OUTPUT_FILE);
        jTextField_wktOut.setEditable(true);
        jTextField_wktOut.setPreferredSize(new Dimension(250, 20));
        FormUtils.addRowInGBL(jpanel, 3, 0, OUTPUT_FILE, jTextField_wktOut);
        return jpanel;
    }

}
