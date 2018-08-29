package com.geomaticaeambiente.klemgui.plugin.hydrology;

import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
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
import com.geomaticaeambiente.openjump.klem.hydrodistance.HydroDistCalculator;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import java.awt.geom.NoninvertibleTransformException;
import java.io.File;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import org.openjump.core.rasterimage.RasterImageLayer;

/**
 *
 * @author Geomatica
 */
public class HydroDistPlugIn extends AbstractInputKlemPlugin {

    public HydroDistPlugIn(PlugInContext context, InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

//        ar_rasterImageLayers = PluginUtils.getRasterImageLayers(context);

        InitialData initialData = new InitialData();
        //input data        
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(RASTER_DEM_LABEL), PluginUtils.getRasterImageLayers(layerablesList.getLayerables()), GUIUtils.INPUT);//combobox with rasterImageLayer        
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(PluginUtils.getResources().getString("HydrographKlemPlugin.Bluelines.label")),
                PluginUtils.getLayers(layerablesList.getLayerables()), GUIUtils.INPUT);
        
        //output data
        initialData.setParam_Label_TextBox_Button(GUIUtils.setGUILabel(HYDRODIST_LABEL), "", new ActionObject(""), GUIUtils.OUTPUT);

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
                    String rasterDem = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));

                    //get output raster name
                    String outRasterName = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.OUTPUT, 1));

                    //check input values
                    GUIUtils.checkStringValue(rasterDem, RASTER_DEM_LABEL);

                    //check output values
                    GUIUtils.checkFileValue(outRasterName, GUIUtils.getOutputRasterString());

                    //get input raster as rasterImageLayer from string
                    RasterImageLayer inputRasterSelected = PluginUtils.getRasterImageLayerSelected(
                            (CustomComboBox.RasterComboBox) componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));
                    DoubleBasicGrid demGrid = RasterUtils.getDoubleBasicGrid(inputRasterSelected);

                    //Flow direction
                    
                    Layer bluelinesLayer = PluginUtils.getLayerSelected((CustomComboBox.LayerComboBox) componentsWithActions.getComponent("01", GUIUtils.INPUT, 1));
                    LineString[] bluelines = null;
                    if(bluelinesLayer != null) {   
                        bluelines = GeometryUtils.getLineStringsFromFeatures(bluelinesLayer.getFeatureCollectionWrapper());
                    }
                    
                    FlowDirsCalculator flowDirsCalculator = new FlowDirsCalculator(demGrid, FlowDirsStripe.FlowDirAlgorithm.D8, bluelines, 100d);
                    FlowDirBasicGrid flowDirsGrid = flowDirsCalculator.calculate();
                    
                    //Calculate hydrography distance
                    HydroDistCalculator hydroDistCalc = new HydroDistCalculator();
                    DoubleBasicGrid hydroDistGrid = hydroDistCalc.calcD8(flowDirsGrid);

                    //Create the output file and display on OJ           
                    //Save grid as tiff
                    RasterUtils.saveOutputRasterAsTiff(hydroDistGrid, new File(outRasterName));
                    //Display raster on OJ from file                
                    RasterUtils.displayRasterFileOnOJ(
                            context.getWorkbenchContext(),
                            new File(outRasterName),
                            null);

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
            }

            @Override
            public void centerButton() {
            }

        };
        return this.mainPanel;
    }

    @Override
    public String toString() {
        return PluginUtils.getResources().getString("HydroDistancePlugIn.PlugInName.label");
    }

    private MainPanel mainPanel;
    private final PlugInContext context;
    private final String RASTER_DEM_LABEL = PluginUtils.getResources().getString("KlemGUI.InputFilledDem.label");
    private final String HYDRODIST_LABEL = PluginUtils.getResources().getString("HydroDistancePlugIn.PlugInName.label");
    private final LayerablesList layerablesList;

}
