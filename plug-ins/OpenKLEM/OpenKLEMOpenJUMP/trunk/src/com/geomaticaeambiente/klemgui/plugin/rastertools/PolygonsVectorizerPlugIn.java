package com.geomaticaeambiente.klemgui.plugin.rastertools;

import java.awt.Cursor;
import java.io.File;
import java.io.IOException;

import javax.swing.JOptionPane;
import javax.swing.JPanel;

import org.openjump.core.rasterimage.RasterImageLayer;

import com.geomaticaeambiente.klemgui.exceptions.WarningException;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox.RasterComboBox;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.openjump.klem.rastertools.Vectorizer;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;

/**
 *
 * @author Geomatica
 */
public class PolygonsVectorizerPlugIn extends AbstractInputKlemPlugin {

    public PolygonsVectorizerPlugIn(PlugInContext context,
            InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

        final InitialData initialData = new InitialData();

        // input data
        initialData.setParam_Label_TextBox(GUIUtils
                .setGUILabel(RASTER_IN_LABEL), PluginUtils
                .getRasterImageLayers(layerablesList.getLayerables()),
                GUIUtils.INPUT); // Raster input

        // Check box:
        initialData.setParam_Action(new ActionObject(true, MULTIPOLYGONS),
                GUIUtils.INPUT);

        // output data
        initialData.setParam_Label_TextBox_Label(
                GUIUtils.getOutputVectorLabel(), "VectorLayer", "",
                GUIUtils.OUTPUT);

        return initialData;

    }

    @Override
    public ComponentsTreeMap setComponentsActions(
            ComponentsTreeMap personalTreeMap) {

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

                    super.getInitialDialog().setCursor(
                            new Cursor(Cursor.WAIT_CURSOR));

                    // Input values
                    final String rasterSelected = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "00", GUIUtils.INPUT, 1));

                    final boolean multipolygons = GUIUtils
                            .getBooleanValue(componentsWithActions
                                    .getComponent("01", GUIUtils.INPUT, 0));

                    // Get output raster name
                    final String outLayerName = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "00", GUIUtils.OUTPUT, 1));

                    // Check input and output values
                    checkValues(rasterSelected, outLayerName);

                    // extract raster selected from combobox
                    final RasterImageLayer inputRasterSelected = PluginUtils
                            .getRasterImageLayerSelected((RasterComboBox) componentsWithActions
                                    .getComponent("00", GUIUtils.INPUT, 1));
                    final File inputFile = new File(
                            inputRasterSelected.getImageFileName());

                    final Vectorizer vectorizer = new Vectorizer(
                            RasterUtils.getDoubleBasicGridFromFile(inputFile),
                            multipolygons);
                    final FeatureCollection feats = vectorizer
                            .vectorizePolygons();

                    // Create layer
                    context.getWorkbenchContext()
                            .getLayerManager()
                            .addLayer(StandardCategoryNames.WORKING,
                                    outLayerName, feats);

                    JOptionPane.showMessageDialog(
                            super.getInitialDialog(),
                            PluginUtils.getResources().getString(
                                    "SetWorkspacePlugin.Done.message"),
                            PluginUtils.plugInName,
                            JOptionPane.INFORMATION_MESSAGE);

                } catch (final WarningException ex) {
                    JOptionPane
                            .showMessageDialog(this, ex.getMessage(),
                                    PluginUtils.plugInName,
                                    JOptionPane.WARNING_MESSAGE);
                } catch (final Exception ex) {
                    ErrorDialog.show(super.getInitialDialog(),
                            PluginUtils.plugInName, ex.toString(),
                            StringUtil.stackTrace(ex));
                } finally {
                    super.getInitialDialog().setCursor(
                            new Cursor(Cursor.DEFAULT_CURSOR));
                }
            }

            @Override
            public void leftButton() {

            }

            @Override
            public void centerButton() {

            }

        };

        return mainPanel;

    }

    @Override
    public String toString() {
        return PluginUtils.getResources().getString(
                "Vectorize_Polygons.PlugInName.text");
    }

    private void checkValues(String raster, String rasterOut)
            throws IOException, Exception {

        GUIUtils.checkStringValue(raster, RASTER_IN_LABEL);

    }

    // private RasterImageLayer[] rasterImageLayers;
    private final PlugInContext context;
    private MainPanel mainPanel;
    private final String RASTER_IN_LABEL = PluginUtils.getResources()
            .getString("KlemGUI.InputRaster.label");
    private final String MULTIPOLYGONS = PluginUtils.getResources().getString(
            "KlemGUI.Multipolygons.label");

    private final LayerablesList layerablesList;

}
