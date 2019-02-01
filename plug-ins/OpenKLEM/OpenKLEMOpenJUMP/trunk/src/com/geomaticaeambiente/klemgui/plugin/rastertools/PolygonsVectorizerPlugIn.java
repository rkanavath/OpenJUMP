package com.geomaticaeambiente.klemgui.plugin.rastertools;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.geom.NoninvertibleTransformException;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import javax.swing.JOptionPane;
import javax.swing.JPanel;

import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.rasterimage.algorithms.VectorizeAlgorithm;
import org.openjump.core.rasterimage.sextante.OpenJUMPSextanteRasterLayer;
import org.openjump.core.rasterimage.sextante.rasterWrappers.GridWrapperNotInterpolated;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

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
import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.geom.EnvelopeUtil;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.util.ColorUtil;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.Logger;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.ColorScheme;
import com.vividsolutions.jump.workbench.ui.renderer.style.ColorThemingStyle;
import com.vividsolutions.jump.workbench.ui.task.TaskMonitorManager;

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

        initialData.setParam_Action(new ActionObject(false, APPLYSTYLE),
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
                    AbstractPlugIn.toActionListener(
                            new AbstractThreadedUiPlugIn() {
                                @Override
                                public String getName() {
                                    return null;
                                }

                                @Override
                                public boolean execute(PlugInContext context)
                                        throws Exception {
                                    return true;
                                }

                                @Override
                                public void run(TaskMonitor monitor,
                                        PlugInContext context) throws Exception {
                                    monitor.report(PluginUtils
                                            .getResources()
                                            .getString(
                                                    "OpenKlem.executing-process"));
                                    reportNothingToUndoYet(context);
                                    monitor.allowCancellationRequests();
                                    polygonsVectorizerCommand(componentsWithActions);
                                }
                            }, context.getWorkbenchContext(),
                            new TaskMonitorManager()).actionPerformed(null);
                } catch (final Exception ex) {
                    ErrorDialog.show(super.getInitialDialog(),
                            PluginUtils.plugInName, ex.toString(),
                            StringUtil.stackTrace(ex));
                    Logger.error(PluginUtils.plugInName, ex);
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

    public JPanel buildPluginPanel_old(
            final ComponentsTreeMap componentsWithActions) {

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
    private final String APPLYSTYLE = PluginUtils.getResources().getString(
            "OpenKlem.thematic-style");

    private final LayerablesList layerablesList;

    public void polygonsVectorizerCommand(
            final ComponentsTreeMap componentsWithActions) throws Exception {

        final String rasterSelected = GUIUtils
                .getStringValue(componentsWithActions.getComponent("00",
                        GUIUtils.INPUT, 1));

        final boolean multipolygons = GUIUtils
                .getBooleanValue(componentsWithActions.getComponent("01",
                        GUIUtils.INPUT, 0));

        final boolean style = GUIUtils.getBooleanValue(componentsWithActions
                .getComponent("02", GUIUtils.INPUT, 0));

        // Get output raster name
        final String outLayerName = GUIUtils
                .getStringValue(componentsWithActions.getComponent("00",
                        GUIUtils.OUTPUT, 1));

        // Check input and output values
        checkValues(rasterSelected, outLayerName);

        // extract raster selected from combobox
        final RasterImageLayer rLayer = PluginUtils
                .getRasterImageLayerSelected((RasterComboBox) componentsWithActions
                        .getComponent("00", GUIUtils.INPUT, 1));
        //    final File inputFile = new File(rLayer.getImageFileName());

        final OpenJUMPSextanteRasterLayer rstLayer = new OpenJUMPSextanteRasterLayer();
        // [mmichaud 2013-05-25] false : this is a temporary image not a file based image
        rstLayer.create(rLayer, false);
        final GridWrapperNotInterpolated gwrapper = new GridWrapperNotInterpolated(
                rstLayer, rstLayer.getLayerGridExtent());

        FeatureCollection featDataset = null;

        if (multipolygons) {

            featDataset = VectorizeAlgorithm.toPolygons(gwrapper, false,
                    ATTRIBUTE_NAME, 0);

        } else {
            featDataset = VectorizeAlgorithm.toPolygons(gwrapper, true,
                    ATTRIBUTE_NAME, 0);
        }
        context.getLayerViewPanel().repaint();
        final Layer layer = context
                .getWorkbenchContext()
                .getLayerManager()
                .addLayer(StandardCategoryNames.WORKING, outLayerName,
                        featDataset);
        zoom(context, rLayer);
        if (style) {
            final ColorScheme colorScheme = ColorUtil
                    .createRandomColorSchema(featDataset.size());

            final Map<Object, BasicStyle> attributeToStyleMap = new TreeMap<Object, BasicStyle>();
            for (final Iterator<Feature> i = featDataset.iterator(); i
                    .hasNext();) {
                final Feature feature = i.next();
                attributeToStyleMap.put(feature.getAttribute(ATTRIBUTE_NAME),
                        new BasicStyle(colorScheme.next()));
            }
            layer.getBasicStyle().setEnabled(false);
            final ColorThemingStyle themeStyle = new ColorThemingStyle(
                    ATTRIBUTE_NAME, attributeToStyleMap, new BasicStyle(
                            Color.gray));
            themeStyle.setEnabled(true);
            layer.addStyle(themeStyle);
            ColorThemingStyle.get(layer).setEnabled(true);
            layer.removeStyle(ColorThemingStyle.get(layer));
            ColorThemingStyle.get(layer).setEnabled(true);
            layer.getBasicStyle().setEnabled(false);
            //     context.getWorkbenchFrame().setStatusMessage(PROCESS_FINALIZED); 
        }

    }

    private static final String ATTRIBUTE_NAME = I18N
            .get("org.openjump.core.ui.plugin.raster.RasterQueryPlugIn.value");

    public void zoom(PlugInContext context, RasterImageLayer rLayer)
            throws NoninvertibleTransformException {
        context.getLayerViewPanel()
                .getViewport()
                .zoom(EnvelopeUtil.bufferByFraction(
                        rLayer.getWholeImageEnvelope(), 0.03));
    }

}
