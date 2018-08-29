package com.geomaticaeambiente.klemgui.plugin.rastertools;

import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox.RasterComboBox;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import java.awt.Cursor;
import java.io.File;
import java.io.IOException;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import org.openjump.core.rasterimage.RasterImageLayer;

/**
 *
 * @author Geomatica
 */
public class RasterConversionsPlugIn extends AbstractInputKlemPlugin {

    public RasterConversionsPlugIn(PlugInContext context, InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

        String[] fileFormats = new String[]{
            RasterConverter.Format.ESRI_ASCII.name(),
            RasterConverter.Format.ESRI_FLT.name(),
            RasterConverter.Format.TIFF.name()};
        
        InitialData initialData = new InitialData();

        //input data
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(RASTER_IN_LABEL), PluginUtils.getRasterImageLayers(layerablesList.getLayerables()), GUIUtils.INPUT); //Raster input
//        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(FROM_LABEL), fileFormats, GUIUtils.INPUT); //classes
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(TO_LABEL), fileFormats, GUIUtils.INPUT); //st dev

        //output data
        initialData.setParam_Label_TextBox_Button(GUIUtils.getOutputRasterLabel(), "", new ActionObject(""), GUIUtils.OUTPUT);

        return initialData;

    }

    @Override
    public ComponentsTreeMap setComponentsActions(ComponentsTreeMap personalTreeMap) {

        //output
        final JTextField outputTextField = (JTextField) personalTreeMap.getComponent("00", GUIUtils.OUTPUT, 1);
        JButton outputButton = (JButton) personalTreeMap.getComponent("00", GUIUtils.OUTPUT, 2);
        outputButton.setIcon(PluginUtils.getFolderIcon());
        outputButton.addActionListener(GUIUtils.setSaveFile(outputTextField, "Raster", null));

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

                    super.getInitialDialog().setCursor(new Cursor(Cursor.WAIT_CURSOR));
                    
                    // Input values 
                    String rasterSelected = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));
                    String toSelected = GUIUtils.getStringValue(componentsWithActions.getComponent("01", GUIUtils.INPUT, 1));

                    // Get output raster name
                    String outRasterName = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.OUTPUT, 1));

                    // Check input and output values
                    checkValues(rasterSelected, outRasterName);

                    //extract raster selected from combobox
                    RasterImageLayer inputRasterSelected = PluginUtils.getRasterImageLayerSelected((RasterComboBox) componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));
                    File inputFile = new  File(inputRasterSelected.getImageFileName());
                    
                    File outputFile = new File(outRasterName);
                    
                    RasterConverter.convert(
                            inputFile,
                            outputFile,
                            RasterConverter.Format.valueOf(toSelected));

                    JOptionPane.showMessageDialog(super.getInitialDialog(),
                            PluginUtils.getResources().getString("SetWorkspacePlugin.Done.message"), PluginUtils.plugInName, JOptionPane.INFORMATION_MESSAGE);

                } catch (Exception ex) {
                    super.getInitialDialog().setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                    ex.printStackTrace(System.out);
                    ErrorDialog.show(
                            super.getInitialDialog(),
                            PluginUtils.plugInName,
                            ex.toString(),
                            StringUtil.stackTrace(ex));
                } finally {
                    super.getInitialDialog().setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
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
        return PluginUtils.getResources().getString("ConvertRasterPlugin.PlugInName.text");
    }

    private void checkValues(String raster, String rasterOut) throws IOException, Exception {

        GUIUtils.checkStringValue(raster, RASTER_IN_LABEL);

        if (rasterOut != null) {
            GUIUtils.checkFileValue(rasterOut, GUIUtils.getOutputRasterString());
        }

    }

//    private RasterImageLayer[] rasterImageLayers;
    private final PlugInContext context;
    private MainPanel mainPanel;
    private final String RASTER_IN_LABEL = PluginUtils.getResources().getString("KlemGUI.InputRaster.label");
    private final String FROM_LABEL = PluginUtils.getResources().getString("KlemGUI.From.label");
    private final String TO_LABEL = PluginUtils.getResources().getString("KlemGUI.To.label");

    private final LayerablesList layerablesList;

}
