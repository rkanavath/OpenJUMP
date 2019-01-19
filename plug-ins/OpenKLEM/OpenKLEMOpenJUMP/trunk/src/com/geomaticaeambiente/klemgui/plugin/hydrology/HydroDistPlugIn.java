package com.geomaticaeambiente.klemgui.plugin.hydrology;

import java.io.File;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

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
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsCalculator;
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsStripe;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.hydrodistance.HydroDistCalculator;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.Logger;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import com.vividsolutions.jump.workbench.ui.task.TaskMonitorManager;

/**
 *
 * @author Geomatica
 */
public class HydroDistPlugIn extends AbstractInputKlemPlugin {

    public HydroDistPlugIn(PlugInContext context, InitialDialog initialDialog,
            LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

        //        ar_rasterImageLayers = PluginUtils.getRasterImageLayers(context);

        final InitialData initialData = new InitialData();
        //input data        
        initialData.setParam_Label_TextBox(GUIUtils
                .setGUILabel(RASTER_DEM_LABEL), PluginUtils
                .getRasterImageLayers(layerablesList.getLayerables()),
                GUIUtils.INPUT);//combobox with rasterImageLayer        
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Bluelines.label")),
                PluginUtils.getLayers(layerablesList.getLayerables()),
                GUIUtils.INPUT);

        //output data
        initialData.setParam_Label_TextBox_Button(
                GUIUtils.setGUILabel(HYDRODIST_LABEL), "",
                new ActionObject(""), GUIUtils.OUTPUT);

        return initialData;
    }

    @Override
    public ComponentsTreeMap setComponentsActions(
            ComponentsTreeMap personalTreeMap) {

        final JTextField outputTextField = (JTextField) personalTreeMap
                .getComponent("00", GUIUtils.OUTPUT, 1);
        final JButton outputButton = (JButton) personalTreeMap.getComponent(
                "00", GUIUtils.OUTPUT, 2);
        outputButton.setIcon(PluginUtils.getFolderIcon());
        outputButton.addActionListener(GUIUtils
                .setSaveRasterTif(outputTextField));

        return personalTreeMap;
    }

    public void hydroDistCommand(final ComponentsTreeMap componentsWithActions)
            throws Exception {
        //get input raster names
        final String rasterDem = GUIUtils.getStringValue(componentsWithActions
                .getComponent("00", GUIUtils.INPUT, 1));

        //get output raster name
        final String outRasterName = GUIUtils
                .getStringValue(componentsWithActions.getComponent("00",
                        GUIUtils.OUTPUT, 1));

        //check input values
        GUIUtils.checkStringValue(rasterDem, RASTER_DEM_LABEL);

        //check output values
        GUIUtils.checkFileValue(outRasterName, GUIUtils.getOutputRasterString());

        //get input raster as rasterImageLayer from string
        final RasterImageLayer inputRasterSelected = PluginUtils
                .getRasterImageLayerSelected((CustomComboBox.RasterComboBox) componentsWithActions
                        .getComponent("00", GUIUtils.INPUT, 1));
        final DoubleBasicGrid demGrid = RasterUtils
                .getDoubleBasicGrid(inputRasterSelected);

        //Flow direction

        final Layer bluelinesLayer = PluginUtils
                .getLayerSelected((CustomComboBox.LayerComboBox) componentsWithActions
                        .getComponent("01", GUIUtils.INPUT, 1));
        LineString[] bluelines = null;
        if (bluelinesLayer != null) {
            bluelines = GeometryUtils.getLineStringsFromFeatures(bluelinesLayer
                    .getFeatureCollectionWrapper());
        }

        final FlowDirsCalculator flowDirsCalculator = new FlowDirsCalculator(
                demGrid, FlowDirsStripe.FlowDirAlgorithm.D8, bluelines, 100d);
        final FlowDirBasicGrid flowDirsGrid = flowDirsCalculator.calculate();

        //Calculate hydrography distance
        final HydroDistCalculator hydroDistCalc = new HydroDistCalculator();
        final DoubleBasicGrid hydroDistGrid = hydroDistCalc
                .calcD8(flowDirsGrid);

        //Create the output file and display on OJ           
        //Save grid as tiff
        RasterUtils.saveOutputRasterAsTiff(hydroDistGrid, new File(
                outRasterName));
        //Display raster on OJ from file                
        RasterUtils.displayRasterFileOnOJ(context.getWorkbenchContext(),
                new File(outRasterName), null);

        JOptionPane.showMessageDialog(super.getInitialDialog(), PluginUtils
                .getResources().getString("SetWorkspacePlugin.Done.message"),
                PluginUtils.plugInName, JOptionPane.INFORMATION_MESSAGE);

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
                                    hydroDistCommand(componentsWithActions);

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

    @Override
    public String toString() {
        return PluginUtils.getResources().getString(
                "HydroDistancePlugIn.PlugInName.label");
    }

    private MainPanel mainPanel;
    private final PlugInContext context;
    private final String RASTER_DEM_LABEL = PluginUtils.getResources()
            .getString("KlemGUI.InputFilledDem.label");
    private final String HYDRODIST_LABEL = PluginUtils.getResources()
            .getString("HydroDistancePlugIn.PlugInName.label");
    private final LayerablesList layerablesList;

}
