package com.geomaticaeambiente.klemgui.plugin.hydrology;

import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox;
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
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsStripe.FlowDirAlgorithm;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.upslopearea.UpslopeAreaCalculator;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import java.awt.geom.NoninvertibleTransformException;
import java.io.File;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import org.openjump.core.rasterimage.RasterImageLayer;

/**
 *
 * @author Geomatica
 */
public class UpslopeAreaPlugIn extends AbstractInputKlemPlugin {

    public UpslopeAreaPlugIn(PlugInContext context, InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

        // Input data
        InitialData initialData = new InitialData();       
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(DEM_LABEL),
                PluginUtils.getRasterImageLayers(layerablesList.getLayerables()),
                GUIUtils.INPUT);
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(PluginUtils.getResources().getString("HydrographKlemPlugin.Bluelines.label")),
                PluginUtils.getLayers(layerablesList.getLayerables()), GUIUtils.INPUT);
        
        String[] hyetoTypes = {
            D8_LABEL,
            MF_LABEL
        };
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(FLOWALGO_LABEL), hyetoTypes, GUIUtils.INPUT);
        
        // Output data
        initialData.setParam_Label_TextBox_Button(GUIUtils.setGUILabel(UPSLOPEAREA_LABEL), "", new ActionObject(""), GUIUtils.OUTPUT);

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
                    String flowDirRaster = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));

                    JComboBox jComboBox_Algo = (JComboBox) componentsWithActions.getComponent("02", GUIUtils.INPUT, 1);
                    String selectedAlgo = jComboBox_Algo.getSelectedItem().toString();
                    
                    FlowDirAlgorithm flowDirAlgo = FlowDirAlgorithm.D8;
                    if(selectedAlgo.equals(D8_LABEL)) {
                        flowDirAlgo = FlowDirAlgorithm.D8;
                    } else if(selectedAlgo.equals(MF_LABEL)) {
                        flowDirAlgo = FlowDirAlgorithm.MultiFlow;
                    }
                    
                    
                    //get output raster name
                    String outRasterName = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.OUTPUT, 1));

                    //check input values
                    GUIUtils.checkStringValue(flowDirRaster, DEM_LABEL);

                    //check output values
                    GUIUtils.checkStringValue(outRasterName, GUIUtils.getOutputRasterString());

                    //get input raster as rasterImageLayer from string
                    RasterImageLayer inputRasterSelected = PluginUtils.getRasterImageLayerSelected(
                            (CustomComboBox.RasterComboBox) componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));
                    DoubleBasicGrid demGrid = RasterUtils.getDoubleBasicGrid(inputRasterSelected);

                    Layer bluelinesLayer = PluginUtils.getLayerSelected((CustomComboBox.LayerComboBox) componentsWithActions.getComponent("01", GUIUtils.INPUT, 1));
                    
                    LineString[] bluelines = null;
                    if(bluelinesLayer != null) {   
                        bluelines = GeometryUtils.getLineStringsFromFeatures(bluelinesLayer.getFeatureCollectionWrapper());
                    }
                    
                    //Calculate upslope area
                    UpslopeAreaCalculator upslopeArea = new UpslopeAreaCalculator(demGrid, bluelines, 100d);
                    DoubleBasicGrid upslopeGrid = upslopeArea.calc(flowDirAlgo);

                    //Create the output file and display on OJ 
                    //Save grid as tiff
                    RasterUtils.saveOutputRasterAsTiff(upslopeGrid, new File(outRasterName));
                    //Display raster on OJ from file                
                    RasterUtils.displayRasterFileOnOJ(
                            context.getWorkbenchContext(),
                            new File(outRasterName),
                            Symbologies.getUpslopeAreaSymb(upslopeGrid.getCellSize()));
                    
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
        return PluginUtils.getResources().getString("UpslopeAreaPlugin.PlugInName.label");
    }

    private MainPanel mainPanel;
    private final PlugInContext context;
    private final String DEM_LABEL = PluginUtils.getResources().getString("KlemGUI.InputFilledDem.label");
    private final String FLOWALGO_LABEL = PluginUtils.getResources().getString("KlemGUI.InputFlowDirAlgo.label");
    private final String D8_LABEL = PluginUtils.getResources().getString("FlowDirectionsPlugIn.FlowDirModel.D8.label");
    private final String MF_LABEL = PluginUtils.getResources().getString("FlowDirectionsPlugIn.FlowDirModel.MF.label");
    private final String UPSLOPEAREA_LABEL = PluginUtils.getResources().getString("UpslopeAreaPlugin.PlugInName.label");
    private final LayerablesList layerablesList;
}
