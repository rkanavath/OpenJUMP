package com.geomaticaeambiente.klemgui.plugin.geomorphometry;

import java.awt.Dialog;
import java.io.File;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import com.geomaticaeambiente.klemgui.exceptions.WarningException;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.GeometryUtils;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.openjump.klem.aspect.AspectCalculator;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.hillshade.HillshadeCalculator;
import com.geomaticaeambiente.openjump.klem.slope.SlopeCalculator;
import com.geomaticaeambiente.openjump.klem.slope.SlopeStripe.SlopeAlgo;
import com.geomaticaeambiente.openjump.klem.slope.SlopeStripe.SlopeUnits;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;

/**
 *
 * @author Geomatica
 */
public class SlopeAspectHillshadePlugIn extends AbstractInputKlemPlugin {

    public SlopeAspectHillshadePlugIn(PlugInContext context,
            InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.initialDialog = initialDialog;
        this.layerablesList = layerablesList;
        initialDialog.setModalityType(Dialog.ModalityType.MODELESS);
    }

    @Override
    public InitialData setInitialData() {

        final InitialData initialData = new InitialData();
        // DEM
        initialData
                .setParam_Label_TextBox(GUIUtils.setGUILabel(DEM_LABEL),
                        PluginUtils.getRasterImageLayers(layerablesList
                                .getLayerables()), GUIUtils.INPUT);// dem
                                                                   // combobox
                                                                   // with
                                                                   // rasterImageLayer

        // Bluelines
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(PluginUtils.getResources().getString(
                        "SlopeAspectHillshadePlugIn.Bluelines.label")),
                PluginUtils.getLayers(layerablesList.getLayerables()),
                GUIUtils.INPUT);

        // Slope algorithm
        initialData.setParam_Labels(new String[] { PluginUtils.getResources()
                .getString("SlopeAspectHillshadePlugIn.SlopeAlgo.label") },
                GUIUtils.OTHER);
        initialData
                .setParam_Action(
                        new ActionObject(
                                new String[] {
                                        PluginUtils
                                                .getResources()
                                                .getString(
                                                        "SlopeAspectHillshadePlugIn.SlopeHorn.label"),
                                        PluginUtils
                                                .getResources()
                                                .getString(
                                                        "SlopeAspectHillshadePlugIn.SlopeLocal.label") }),
                        GUIUtils.OTHER);// radio button da mouse da layer

        // Slope units
        initialData.setParam_Labels(new String[] { PluginUtils.getResources()
                .getString("SlopeAspectHillshadePlugIn.SlopeUnits.label") },
                GUIUtils.OTHER);
        initialData
                .setParam_Action(
                        new ActionObject(
                                new String[] {
                                        PluginUtils
                                                .getResources()
                                                .getString(
                                                        "SlopeAspectHillshadePlugIn.SlopeUnitsPercent.label"),
                                        PluginUtils
                                                .getResources()
                                                .getString(
                                                        "SlopeAspectHillshadePlugIn.SlopeUnitsDegrees.label") }),
                        GUIUtils.OTHER);// radio button da mouse da layer

        // Hillshade parameters;
        initialData.setParam_Labels(
                new String[] { PluginUtils.getResources().getString(
                        "SlopeAspectHillshadePlugIn.HillshadeParms.label") },
                GUIUtils.OTHER);
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(PluginUtils.getResources().getString(
                        "SlopeAspectHillshadePlugIn.HillshadeZenith.label")),
                "", GUIUtils.OTHER);
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(PluginUtils.getResources().getString(
                        "SlopeAspectHillshadePlugIn.HillshadeAzimuth.label")),
                "", GUIUtils.OTHER);

        // Output
        initialData.setParam_Label_TextBox_Button(
                GUIUtils.setGUILabel(SLOPE_LABEL), "", new ActionObject(""),
                GUIUtils.OUTPUT); // JTextField
        initialData.setParam_Label_TextBox_Button(
                GUIUtils.setGUILabel(ASPECT_LABEL), "", new ActionObject(""),
                GUIUtils.OUTPUT); // JTextField
        initialData.setParam_Label_TextBox_Button(
                GUIUtils.setGUILabel(HILLSHADE_LABEL), "",
                new ActionObject(""), GUIUtils.OUTPUT); // JTextField

        return initialData;

    }

    @Override
    public ComponentsTreeMap setComponentsActions(
            final ComponentsTreeMap personalTreeMap) {

        // Radio button slope algo
        final JRadioButton jRadioButton_algo_horn = (JRadioButton) personalTreeMap
                .getComponent("01", GUIUtils.OTHER, 0);
        final JRadioButton jRadioButton_algo_local = (JRadioButton) personalTreeMap
                .getComponent("01", GUIUtils.OTHER, 1);

        // Radio button slope units
        final JRadioButton jRadioButton_units_percent = (JRadioButton) personalTreeMap
                .getComponent("03", GUIUtils.OTHER, 0);
        final JRadioButton jRadioButton_units_degs = (JRadioButton) personalTreeMap
                .getComponent("03", GUIUtils.OTHER, 1);

        // Zenith
        final JTextField jTextField_Zenith = (JTextField) personalTreeMap
                .getComponent("05", GUIUtils.OTHER, 1);
        jTextField_Zenith.setText("45");

        // Azimuth
        final JTextField jTextField_Azimuth = (JTextField) personalTreeMap
                .getComponent("06", GUIUtils.OTHER, 1); // jtext field coord y
        jTextField_Azimuth.setText("315");

        jRadioButton_algo_horn.setSelected(true);
        jRadioButton_units_percent.setSelected(true);

        final JTextField outSlope_TextField = (JTextField) personalTreeMap
                .getComponent("00", GUIUtils.OUTPUT, 1);
        final JButton outSlope_Button = (JButton) personalTreeMap.getComponent(
                "00", GUIUtils.OUTPUT, 2);
        outSlope_Button.setIcon(PluginUtils.getFolderIcon());
        outSlope_Button.addActionListener(GUIUtils
                .setSaveRasterTif(outSlope_TextField));

        final JTextField outAspect_TextField = (JTextField) personalTreeMap
                .getComponent("01", GUIUtils.OUTPUT, 1);
        final JButton outAspect_Button = (JButton) personalTreeMap
                .getComponent("01", GUIUtils.OUTPUT, 2);
        outAspect_Button.setIcon(PluginUtils.getFolderIcon());
        outAspect_Button.addActionListener(GUIUtils
                .setSaveRasterTif(outAspect_TextField));

        final JTextField outHillShade_TextField = (JTextField) personalTreeMap
                .getComponent("02", GUIUtils.OUTPUT, 1);
        final JButton outHillshade_Button = (JButton) personalTreeMap
                .getComponent("02", GUIUtils.OUTPUT, 2);
        outHillshade_Button.setIcon(PluginUtils.getFolderIcon());
        outHillshade_Button.addActionListener(GUIUtils
                .setSaveRasterTif(outHillShade_TextField));

        return personalTreeMap;

    }

    @Override
    public JPanel buildPluginPanel(final ComponentsTreeMap componentsWithActions) {
        if (mainPanel != null) {
            return mainPanel;
        }
        mainPanel = new MainPanel(super.getInitialDialog(),
                componentsWithActions, false, false, true, PluginUtils
                        .getResources().getString(
                                "MainPanel.ExecuteButton.text"), layerablesList) {

            /**
                     * 
                     */
            private static final long serialVersionUID = 1L;

            @Override
            public void rightButton() {
                try {

                    // DEM
                    final String demFileFullName = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "00", GUIUtils.INPUT, 1));

                    // Bluelines
                    final Layer bluelinesLayer = PluginUtils
                            .getLayerSelected((CustomComboBox.LayerComboBox) componentsWithActions
                                    .getComponent("01", GUIUtils.INPUT, 1));
                    LineString[] bluelines = null;
                    if (bluelinesLayer != null) {
                        bluelines = GeometryUtils
                                .getLineStringsFromFeatures(bluelinesLayer
                                        .getFeatureCollectionWrapper());
                    }

                    // Slope algo
                    final boolean slopeAlgoHorn = GUIUtils
                            .componentIsSelected(componentsWithActions
                                    .getComponent("01", GUIUtils.OTHER, 0));

                    // Slope units
                    final boolean slopeUnitsPercent = GUIUtils
                            .componentIsSelected(componentsWithActions
                                    .getComponent("03", GUIUtils.OTHER, 0));

                    // Zenith
                    final String zenith = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "05", GUIUtils.OTHER, 1));

                    // Azimuth
                    final String azimuth = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "06", GUIUtils.OTHER, 1));

                    // Out rasters
                    final String slopeRasterName = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "00", GUIUtils.OUTPUT, 1));
                    final String aspectRasterName = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "01", GUIUtils.OUTPUT, 1));
                    final String hillshadeRasterName = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "02", GUIUtils.OUTPUT, 1));

                    checkValues(demFileFullName, zenith, azimuth,
                            slopeRasterName, aspectRasterName,
                            hillshadeRasterName);

                    // Retrieve inputs
                    final DoubleBasicGrid demGrid = RasterUtils
                            .getDoubleBasicGrid((CustomComboBox.RasterComboBox) componentsWithActions
                                    .getComponent("00", GUIUtils.INPUT, 1));

                    // Execute
                    DoubleBasicGrid slopeGrid = null;
                    DoubleBasicGrid aspectGrid = null;
                    if (!slopeRasterName.equals("")
                            || !hillshadeRasterName.equals("")) {
                        // Calcualte slope: used also for hillshade
                        SlopeAlgo slopeAlgo = SlopeAlgo.HORN;
                        SlopeUnits slopeUnits = SlopeUnits.PERCENT;
                        if (!slopeAlgoHorn) {
                            slopeAlgo = SlopeAlgo.LOCAL;
                        }
                        if (!slopeUnitsPercent) {
                            slopeUnits = SlopeUnits.DEGREES;
                        }
                        final SlopeCalculator sc = new SlopeCalculator(demGrid,
                                bluelines, 100d, slopeAlgo, slopeUnits);
                        slopeGrid = sc.calculate();
                        RasterUtils.saveOutputRasterAsTiff(slopeGrid, new File(
                                slopeRasterName));
                    }

                    if (!aspectRasterName.equals("")
                            || !hillshadeRasterName.equals("")) {
                        // Aspect
                        final AspectCalculator ac = new AspectCalculator(
                                demGrid, bluelines, 100d);
                        aspectGrid = ac.calculate();
                        RasterUtils.saveOutputRasterAsTiff(aspectGrid,
                                new File(aspectRasterName));
                    }

                    if (!hillshadeRasterName.equals("") && slopeGrid != null
                            && aspectGrid != null) {
                        // Hillshade
                        final double zenithDegs = Double.parseDouble(zenith);
                        final double azimuthDegs = Double.parseDouble(azimuth);

                        if (slopeUnitsPercent) {

                            for (int r = 0; r < slopeGrid.getRowCount(); r++) {
                                for (int c = 0; c < slopeGrid.getColumnCount(); c++) {
                                    if (slopeGrid.isNoData(slopeGrid.getValue(
                                            c, r))) {
                                        continue;
                                    }
                                    final double slopeDegs = Math
                                            .toDegrees(Math.atan(slopeGrid
                                                    .getValue(c, r) / 100));
                                    slopeGrid.setValue(c, r, slopeDegs);
                                }
                            }

                        }

                        final HillshadeCalculator hc = new HillshadeCalculator(
                                slopeGrid, aspectGrid, zenithDegs, azimuthDegs);
                        final DoubleBasicGrid hillshadeGrid = hc.calculate();
                        RasterUtils.saveOutputRasterAsTiff(hillshadeGrid,
                                new File(hillshadeRasterName));
                    }

                    // Display raster on OJ from file
                    if (!slopeRasterName.equals("")) {
                        RasterUtils.displayRasterFileOnOJ(context
                                .getWorkbenchContext(), new File(
                                slopeRasterName), null);
                    }
                    if (!aspectRasterName.equals("")) {
                        RasterUtils.displayRasterFileOnOJ(context
                                .getWorkbenchContext(), new File(
                                aspectRasterName), null);
                    }
                    if (!hillshadeRasterName.equals("")) {
                        RasterUtils.displayRasterFileOnOJ(context
                                .getWorkbenchContext(), new File(
                                hillshadeRasterName), null);
                    }

                    JOptionPane.showMessageDialog(
                            super.getInitialDialog(),
                            PluginUtils.getResources().getString(
                                    "SetWorkspacePlugin.Done.message"),
                            PluginUtils.plugInName,
                            JOptionPane.INFORMATION_MESSAGE);

                } catch (final WarningException ex) {
                    JOptionPane.showMessageDialog(super.getInitialDialog(),
                            ex.getMessage(), PluginUtils.plugInName,
                            JOptionPane.WARNING_MESSAGE);
                } catch (final Exception ex) {
                    ErrorDialog.show(super.getInitialDialog(),
                            PluginUtils.plugInName, ex.toString(),
                            StringUtil.stackTrace(ex));
                }
            }

            @Override
            public void leftButton() {
                throw new UnsupportedOperationException("Not supported yet."); // To
                                                                               // change
                                                                               // body
                                                                               // of
                                                                               // generated
                                                                               // methods,
                                                                               // choose
                                                                               // Tools
                                                                               // |
                                                                               // Templates.
            }

            @Override
            public void centerButton() {
                throw new UnsupportedOperationException("Not supported yet."); // To
                                                                               // change
                                                                               // body
                                                                               // of
                                                                               // generated
                                                                               // methods,
                                                                               // choose
                                                                               // Tools
                                                                               // |
                                                                               // Templates.
            }

        };
        return mainPanel;
    }

    @Override
    public String toString() {
        return PluginUtils.getResources().getString(
                "SlopeAspectHillshadePlugIn.PlugInName.label");
    }

    private void checkValues(String dem, String zenith, String azimuth,
            String outSlope, String outAspect, String outHillshade)
            throws IOException, Exception {

        GUIUtils.checkStringValue(dem, DEM_LABEL);

        if (outSlope.equals("") && outAspect.equals("")
                && outHillshade.equals("")) {
            throw new WarningException(PluginUtils.getResources().getString(
                    "Check.AtLeastOneOutputRequired"));
        }

        if (!outSlope.equals("")) {
            GUIUtils.checkFileValue(outSlope, GUIUtils.getOutputRasterString());
        }
        if (!outAspect.equals("")) {
            GUIUtils.checkFileValue(outAspect, GUIUtils.getOutputRasterString());
        }
        if (!outHillshade.equals("")) {
            GUIUtils.checkStringValue(zenith, XCOORD_LABEL);
            GUIUtils.checkStringValue(azimuth, YCOORD_LABEL);
            GUIUtils.checkFileValue(outHillshade,
                    GUIUtils.getOutputRasterString());
        }
    }

    private Object makeObj(final String item) {
        return new Object() {
            @Override
            public String toString() {
                return item;
            }
        };
    }

    private MainPanel mainPanel;
    private final PlugInContext context;
    // private RasterImageLayer[] ar_rasterImageLayers;
    private final InitialDialog initialDialog;
    // private Layer[] layers;
    private final String DEM_LABEL = PluginUtils.getResources().getString(
            "KlemGUI.InputDem.label");
    // private final String OUT_RASTER_LABEL =
    // PluginUtils.getResources().getString("KlemGUI.OutputRaster.label");
    private final String XCOORD_LABEL = "x";
    private final String YCOORD_LABEL = "y";
    private final String SLOPE_LABEL = PluginUtils.getResources().getString(
            "SlopeAspectHillshadePlugIn.SlopeLabel");
    private final String ASPECT_LABEL = PluginUtils.getResources().getString(
            "SlopeAspectHillshadePlugIn.AspectLabel");
    private final String HILLSHADE_LABEL = PluginUtils.getResources()
            .getString("SlopeAspectHillshadePlugIn.HillShadeLabel");

    private final LayerablesList layerablesList;
}
