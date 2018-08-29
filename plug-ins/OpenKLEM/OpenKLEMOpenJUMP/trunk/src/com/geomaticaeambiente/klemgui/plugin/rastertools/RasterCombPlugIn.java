package com.geomaticaeambiente.klemgui.plugin.rastertools;

import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel2.DoubleStripeGrid2;
import com.geomaticaeambiente.openjump.klem.rastertools.RasterComb;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.PersonalRasterCombPanel;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.RasterCombinationComponent;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.openjump.klem.exceptions.NotSpatiallyConsistentGridsException;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import java.io.File;
import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.UUID;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import org.openjump.core.rasterimage.RasterImageLayer;

/**
 *
 * @author Geomatica
 */
public class RasterCombPlugIn extends AbstractInputKlemPlugin {

    public RasterCombPlugIn(PlugInContext context, InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

//        rasterImageLayers = PluginUtils.getRasterImageLayers(context);

        RasterCombinationComponent personalObj = new RasterCombinationComponent(super.getInitialDialog());

        InitialData initialData = new InitialData();
        initialData.setParam_RasterComb(context.getWorkbenchContext(), personalObj, GUIUtils.INPUT);

        //set output
        initialData.setParam_Label_TextBox_Button(GUIUtils.getOutputRasterLabel(), "", new ActionObject(""), GUIUtils.OUTPUT);

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

                    //get input information
                    boolean[] rasterSelected = GUIUtils.getSelectedRasterFromRasterCombo(componentsWithActions.getComponent("00", GUIUtils.INPUT, 0));
                    PersonalRasterCombPanel prc = ((PersonalRasterCombPanel) componentsWithActions.getComponent("00", GUIUtils.INPUT, 0));
                    String expression = GUIUtils.getExprerssionFromRasterCombo(componentsWithActions.getComponent("00", GUIUtils.INPUT, 0));
                    
                    UUID[] uuids = prc.getSelRastersUUIds();
                    
                    //get output raster name
                    String outRasterName = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.OUTPUT, 1));

                    //check
                    checkValues(this, expression, rasterSelected, outRasterName);

                    LinkedHashMap<String, DoubleStripeGrid2> rastersAndNames_m = new LinkedHashMap<String, DoubleStripeGrid2>();                   
                    for (RasterImageLayer rasterImageLayer : PluginUtils.getRasterImageLayers(layerablesList.getLayerables())) {
                        for(UUID uuid : uuids) {
                            if(uuid != null && rasterImageLayer.getUUID().compareTo(uuid) == 0) {
                                rastersAndNames_m.put(rasterImageLayer.getName(), RasterUtils.getDoubleStripeGrid((RasterImageLayer) rasterImageLayer));
                            }
                        }      
//                        rastersAndNames_m.put(rasterImageLayer.getName(), RasterUtils.getDoubleStripeGrid((RasterImageLayer) rasterImageLayer));
                    }

                    RasterComb rasterComb = new RasterComb(rastersAndNames_m, expression);
                    DoubleBasicGrid outGrid = rasterComb.call();

                    //Create the output file and display on OJ
                    //Save grid as tiff
                    RasterUtils.saveOutputRasterAsTiff(outGrid, new File(outRasterName));
                    //Display raster on OJ from the file                
                    RasterUtils.displayRasterFileOnOJ(
                            context.getWorkbenchContext(),
                            new File(outRasterName),
                            null);

                    prc.updateRasterList();
                    
                    JOptionPane.showMessageDialog(super.getInitialDialog(), PluginUtils.getResources().getString("SetWorkspacePlugin.Done.message"), PluginUtils.plugInName, JOptionPane.INFORMATION_MESSAGE);

                } catch (NotSpatiallyConsistentGridsException ex) {
                    JOptionPane.showMessageDialog(super.getInitialDialog(), PluginUtils.getResources().getString("SpatiallyInconsistentRasters"), PluginUtils.plugInName, JOptionPane.WARNING_MESSAGE);
                    
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
        return PluginUtils.getResources().getString("RasterCombPlugin.PlugInName.label");
    }

    private void checkValues(JComponent component, String expression, boolean[] rasterList, String outRaster) throws IOException, Exception {

        GUIUtils.checkStringValue(expression, "Expression");

        boolean foundRaster = false;
        for (int n = 0; n < rasterList.length; n++) {
            if (rasterList[n] == true) {
                foundRaster = true;
                break;
            }
        }
        if (foundRaster == false) {
            throw new NullPointerException(PluginUtils.getResources().getString("RasterComboPlugIn.CheckExpression.message"));
        }

        GUIUtils.checkFileValue(outRaster, "raster out");

    }

    private final PlugInContext context;
    private MainPanel mainPanel;
    private final LayerablesList layerablesList;

}
