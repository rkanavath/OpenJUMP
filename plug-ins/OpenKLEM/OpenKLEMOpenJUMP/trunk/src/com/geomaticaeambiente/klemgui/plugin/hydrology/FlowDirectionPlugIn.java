package com.geomaticaeambiente.klemgui.plugin.hydrology;

import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox.LayerComboBox;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox.RasterComboBox;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.Symbologies;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.GeometryUtils;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsCalculator;
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsStripe;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import java.awt.geom.NoninvertibleTransformException;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.rasterimage.TiffTags;

/**
 *
 * @author Geomatica
 */
public class FlowDirectionPlugIn extends AbstractInputKlemPlugin {

    public FlowDirectionPlugIn(PlugInContext context, InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

//        ar_rasterImageLayers = PluginUtils.getRasterImageLayers(context);

        InitialData initialData = new InitialData();
        //input data        
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(RASTER_IN_LABEL),
                PluginUtils.getRasterImageLayers(layerablesList.getLayerables()), GUIUtils.INPUT);//combobox with rasterImageLayer
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(PluginUtils.getResources().getString("HydrographKlemPlugin.Bluelines.label")),
                PluginUtils.getLayers(layerablesList.getLayerables()), GUIUtils.INPUT);

        //output data
        initialData.setParam_Label_TextBox_Button(GUIUtils.setGUILabel(RASTER_FLOWDIRS), "", new ActionObject(""), GUIUtils.OUTPUT);

        return initialData;
    }

    @Override
    public ComponentsTreeMap setComponentsActions(ComponentsTreeMap personalTreeMap) {

        final JTextField outputTextField = (JTextField) personalTreeMap.getComponent("00", GUIUtils.OUTPUT, 1);
        JButton outputButton = (JButton) personalTreeMap.getComponent("00", GUIUtils.OUTPUT, 2);
        outputButton.setIcon(PluginUtils.getFolderIcon());
        outputButton.addActionListener(GUIUtils.setSaveRasterTif(outputTextField));

        return personalTreeMap;
    }

    @Override
    public JPanel buildPluginPanel(final ComponentsTreeMap componentsWithActions) {
        
        if(this.mainPanel != null) {
            return this.mainPanel;
        }
        this.mainPanel = new MainPanel(super.getInitialDialog(), componentsWithActions, false, false, true,
                PluginUtils.getResources().getString("MainPanel.ExecuteButton.text"), layerablesList) {

            @Override
            public void rightButton() {

                try {

                    //get input raster names
                    String rasterSelected = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));

                    //get output raster name
                    String outRasterName = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.OUTPUT, 1));

                    //check input values
                    GUIUtils.checkStringValue(rasterSelected, RASTER_IN_LABEL);
                    //check output values
                    GUIUtils.checkFileValue(outRasterName, GUIUtils.getOutputRasterString());

                    //get input raster as rasterImageLayer from string
                    RasterImageLayer inputRasterSelected = PluginUtils.getRasterImageLayerSelected((RasterComboBox)componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));
                    DoubleBasicGrid inputGrid = RasterUtils.getDoubleBasicGrid(inputRasterSelected);

                    Layer bluelinesLayer = PluginUtils.getLayerSelected((LayerComboBox) componentsWithActions.getComponent("01", GUIUtils.INPUT, 1));
                    
                    LineString[] bluelines = null;
                    if(bluelinesLayer != null) {   
                        bluelines = GeometryUtils.getLineStringsFromFeatures(bluelinesLayer.getFeatureCollectionWrapper());
                    }
                    
                    //Flow direction
                    FlowDirsCalculator flowDireCalculator = new FlowDirsCalculator(inputGrid, FlowDirsStripe.FlowDirAlgorithm.D8, bluelines, 100d);
                    FlowDirBasicGrid flowDirByteGrid = flowDireCalculator.calculate();

                    //Create the output rasterImageLayer
                    //Save grid as tiff
                    RasterUtils.saveOutputRasterAsTiff(flowDirByteGrid, new File(outRasterName));
                    //Display raster on OJ from file                
                    RasterUtils.displayRasterFileOnOJ(
                            context.getWorkbenchContext(),
                            new File(outRasterName),
                            Symbologies.getFlowDirSymb());

                    JOptionPane.showMessageDialog(super.getInitialDialog(), 
                            PluginUtils.getResources().getString("SetWorkspacePlugin.Done.message"), PluginUtils.plugInName, JOptionPane.INFORMATION_MESSAGE);
                    
                } catch (Exception ex) {
                    ErrorDialog.show(
                            super.getInitialDialog(),
                            PluginUtils.plugInName,
                            ex.toString(),
                            StringUtil.stackTrace(ex));
                }

            }

            @Override
            public void leftButton() {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }

            @Override
            public void centerButton() {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }

        };

        return this.mainPanel;
    }

    @Override
    public String toString() {
        return PluginUtils.getResources().getString("FlowDirectionPlugin.PlugInName.label");
    }

    private MainPanel mainPanel;
    private final PlugInContext context;
//    private RasterImageLayer[] ar_rasterImageLayers;
    private final String RASTER_IN_LABEL = PluginUtils.getResources().getString("KlemGUI.InputFilledDem.label");
    private final String RASTER_FLOWDIRS = PluginUtils.getResources().getString("FlowDirectionPlugin.PlugInName.label");
    private final LayerablesList layerablesList;

}
