package com.geomaticaeambiente.klemgui.plugin.rastertools;

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
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.rastertools.RasterClipper;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.Logger;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import com.vividsolutions.jump.workbench.ui.task.TaskMonitorManager;

/**
 * Plugin for cutting a raster with another raster. The raster must be loaded on
 * OJ TOC. The output raster will be displayed on OJ.
 *
 * @author Geomatica
 */
public class CutRasterPlugin extends AbstractInputKlemPlugin {

    public CutRasterPlugin(PlugInContext context, InitialDialog initialDialog,
            LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

        //        ar_rasterImageLayers = PluginUtils.getRasterImageLayers(layerablesList.getLayerables());

        final InitialData initialData = new InitialData();
        //input data        
        initialData
                .setParam_Label_TextBox(GUIUtils.setGUILabel(RASTER_IN),
                        PluginUtils.getRasterImageLayers(layerablesList
                                .getLayerables()), GUIUtils.INPUT);//combobox with rasterImageLayer
        initialData
                .setParam_Label_TextBox(GUIUtils.setGUILabel(RASTER_MASK),
                        PluginUtils.getRasterImageLayers(layerablesList
                                .getLayerables()), GUIUtils.INPUT);//combobox with rasterImageLayer

        //output data
        initialData.setParam_Label_TextBox_Button(
                GUIUtils.setGUILabel(RASTER_OUT), "", new ActionObject(""),
                GUIUtils.OUTPUT); //JTextField      

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

    public void cutRasterCommand(final ComponentsTreeMap componentsWithActions)
            throws Exception {
        //get input raster names
        final String rasterSelected1 = GUIUtils
                .getStringValue(componentsWithActions.getComponent("00",
                        GUIUtils.INPUT, 1));
        final String rasterSelected2 = GUIUtils
                .getStringValue(componentsWithActions.getComponent("01",
                        GUIUtils.INPUT, 1));

        //get output raster name
        final String outRasterName = GUIUtils
                .getStringValue(componentsWithActions.getComponent("00",
                        GUIUtils.OUTPUT, 1));

        //check input values
        GUIUtils.checkStringValue(rasterSelected1, RASTER_IN);
        GUIUtils.checkStringValue(rasterSelected2, RASTER_MASK);

        //check output values
        GUIUtils.checkFileValue(outRasterName, RASTER_OUT);

        //get input raster as rasterImageLayer from string
        final RasterImageLayer inputRasterSelected = PluginUtils
                .getRasterImageLayerSelected((CustomComboBox.RasterComboBox) componentsWithActions
                        .getComponent("00", GUIUtils.INPUT, 1));
        final DoubleBasicGrid inputGrid = RasterUtils
                .getDoubleBasicGrid(inputRasterSelected);

        //get mask raster as rasterImageLayer from string
        final RasterImageLayer maskRasterSelected = PluginUtils
                .getRasterImageLayerSelected((CustomComboBox.RasterComboBox) componentsWithActions
                        .getComponent("01", GUIUtils.INPUT, 1));
        final DoubleBasicGrid clipperGrid = RasterUtils
                .getDoubleBasicGrid(maskRasterSelected);

        //Clip raster
        final DoubleBasicGrid clippedRaster = RasterClipper.clip(inputGrid,
                clipperGrid);

        //Create the output file and display on OJ 
        //Save grid as tiff
        RasterUtils.saveOutputRasterAsTiff(clippedRaster, new File(
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
                                    cutRasterCommand(componentsWithActions);
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
                "CutRasterPlugIn.PlugInName.label");
    }

    private final PlugInContext context;
    private MainPanel mainPanel;
    //    private RasterImageLayer[] ar_rasterImageLayers;
    private final String RASTER_IN = PluginUtils.getResources().getString(
            "KlemGUI.InputRaster.label");
    private final String RASTER_MASK = PluginUtils.getResources().getString(
            "CutRasterPlugIn.InputData.MaskRaster");
    private final String RASTER_OUT = PluginUtils.getResources().getString(
            "CutRasterPlugIn.OutputData.OutputRaster");

    private final LayerablesList layerablesList;

}
