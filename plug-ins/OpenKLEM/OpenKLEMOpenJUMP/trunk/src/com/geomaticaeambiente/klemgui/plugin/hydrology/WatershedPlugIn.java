package com.geomaticaeambiente.klemgui.plugin.hydrology;

import com.geomaticaeambiente.klemgui.exceptions.WarningException;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem.WatershedInformation;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox.LayerComboBox;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox.RasterComboBox;
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
import com.geomaticaeambiente.openjump.klem.watersheds.WatershedExtractor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import java.awt.Dialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import org.openjump.core.rasterimage.RasterImageLayer;

/**
 *
 * @author Geomatica
 */
public class WatershedPlugIn extends AbstractInputKlemPlugin {

    public WatershedPlugIn(PlugInContext context, InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.initialDialog = initialDialog;
        this.layerablesList = layerablesList;
        initialDialog.setModalityType(Dialog.ModalityType.MODELESS);
    }

    @Override
    public InitialData setInitialData() {

//        layers = PluginUtils.getLayers(context);
//        ar_rasterImageLayers = PluginUtils.getRasterImageLayers(context);
        String[] attributes = new String[0];

        InitialData initialData = new InitialData();
        //input data        
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(DEM_LABEL),
                PluginUtils.getRasterImageLayers(layerablesList.getLayerables()),
                GUIUtils.INPUT);//dem combobox with rasterImageLayer       
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(PluginUtils.getResources().getString("HydrographKlemPlugin.Bluelines.label")),
                PluginUtils.getLayers(layerablesList.getLayerables()), GUIUtils.INPUT);
        
        //other data
        initialData.setParam_Labels(new String[]{PluginUtils.getResources().getString("WatershedPlugin.CloseCoordSection.label")}, GUIUtils.OTHER);
        initialData.setParam_Action(new ActionObject(
                new String[]{PluginUtils.getResources().getString("WatershedPlugin.MouseRadioButton.label"),
                    PluginUtils.getResources().getString("WatershedPlugin.RadioButtonLayer.label")}), GUIUtils.OTHER);//radio button da mouse da layer

        // coordinata x da mouse
        initialData.setParam_Label_TextBox_Button(GUIUtils.setGUILabel(XCOORD_LABEL), "",
                new ActionObject(PluginUtils.getResources().getString("WatershedPlugin.ChooseButton.label")), GUIUtils.OTHER);
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(YCOORD_LABEL), "", GUIUtils.OTHER);// coordinata y da mouse

        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(LAYER_LABEL), PluginUtils.getLayers(layerablesList.getLayerables()), GUIUtils.OTHER);// lista layer vettoriali
//        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel("Attribute"), attributes);// lista layer vettoriali

        //output data
        // Check box:
        initialData.setParam_Action(new ActionObject(true, CLIP_OUTPUT), GUIUtils.OUTPUT);
        initialData.setParam_Label_TextBox_Button(GUIUtils.setGUILabel(WATERSHED_LABEL), "", new ActionObject(""), GUIUtils.OUTPUT); //JTextField      

        return initialData;

    }

    @Override
    public ComponentsTreeMap setComponentsActions(final ComponentsTreeMap personalTreeMap) {

        //radio button components
        final JRadioButton jRadioButton_mouse = (JRadioButton) personalTreeMap.getComponent("01", GUIUtils.OTHER, 0); //radio button from mouse
        final JRadioButton jRadioButton_layer = (JRadioButton) personalTreeMap.getComponent("01", GUIUtils.OTHER, 1); // radio button from layer

        //x coord components
        final JLabel jLabel_xCoord = (JLabel) personalTreeMap.getComponent("02", GUIUtils.OTHER, 0); // label coord x
        final JTextField jTextField_xCoord = (JTextField) personalTreeMap.getComponent("02", GUIUtils.OTHER, 1); //jtext field coord x
        final JButton jButton_Choose = (JButton) personalTreeMap.getComponent("02", GUIUtils.OTHER, 2); //button choose

        //y coord components
        final JLabel jLabel_yCoord = (JLabel) personalTreeMap.getComponent("03", GUIUtils.OTHER, 0); // label coord y
        final JTextField jTextField_yCoord = (JTextField) personalTreeMap.getComponent("03", GUIUtils.OTHER, 1); //jtext field coord y 

        //layer components
        final JLabel jLabel_layer = (JLabel) personalTreeMap.getComponent("04", GUIUtils.OTHER, 0); // label layer
        jLabel_layer.setEnabled(false);
        final CustomComboBox.LayerComboBox jComboBox_layer = (CustomComboBox.LayerComboBox) (JComboBox) personalTreeMap.getComponent("04", GUIUtils.OTHER, 1); //jtext field layer
        jComboBox_layer.setEnabled(false);

        //attribute components  > NOT USED, ONLY GEOMETRY IS USEFUL
//        final JLabel jLabel_attribute = (JLabel) personalTreeMap.getComponent("Other5", 0); // label attribute
//        jLabel_attribute.setEnabled(false);
//        final JComboBox jComboBox_attribute= (JComboBox) personalTreeMap.getComponent("Other5", 1); //jtext field attribute 
//        jComboBox_attribute.setEnabled(false);
        jRadioButton_mouse.addActionListener(new ActionListener() {
            //enable/disable components
            @Override
            public void actionPerformed(ActionEvent e) {
                enableElements(jRadioButton_mouse, jRadioButton_layer, jLabel_xCoord,
                        jTextField_xCoord, jButton_Choose, jLabel_yCoord, jTextField_yCoord, jLabel_layer,
                        jComboBox_layer);
            }
        });

        jRadioButton_layer.addActionListener(new ActionListener() {
            //enable/disable components
            @Override
            public void actionPerformed(ActionEvent e) {
                enableElements(jRadioButton_mouse, jRadioButton_layer, jLabel_xCoord,
                        jTextField_xCoord, jButton_Choose, jLabel_yCoord, jTextField_yCoord, jLabel_layer,
                        jComboBox_layer);
            }
        });

        //Choose action
        jButton_Choose.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                
                RasterImageLayer inputRasterSelected = PluginUtils.getRasterImageLayerSelected((CustomComboBox.RasterComboBox) personalTreeMap.getComponent("00", GUIUtils.INPUT, 1));
                
                WatershedInformation wi = new WatershedInformation(context, jTextField_xCoord, jTextField_yCoord);
                wi.getCoordinate();
                wi.setRasterEnvelope(inputRasterSelected.getWholeImageEnvelope());
            }
        });

        jRadioButton_mouse.setSelected(true);

        final JTextField outputTextField = (JTextField) personalTreeMap.getComponent("01", GUIUtils.OUTPUT, 1);
        JButton outputButton = (JButton) personalTreeMap.getComponent("01", GUIUtils.OUTPUT, 2);
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

                    //get other information 
                    boolean selectionMouse = GUIUtils.componentIsSelected(componentsWithActions.getComponent("01", GUIUtils.OTHER, 0));
                    String xCoord = null;
                    String yCoord = null;
                    String layerSelected = null;

                    if (selectionMouse) {
                        xCoord = GUIUtils.getStringValue(componentsWithActions.getComponent("02", GUIUtils.OTHER, 1));//xCoord value
                        yCoord = GUIUtils.getStringValue(componentsWithActions.getComponent("03", GUIUtils.OTHER, 1));//yCoord value
                    } else {
                        layerSelected = GUIUtils.getStringValue(componentsWithActions.getComponent("04", GUIUtils.OTHER, 1)); //layer 
                    }

                    //get output raster name
                    boolean clipOutput = GUIUtils.getBooleanValue(componentsWithActions.getComponent("00", GUIUtils.OUTPUT, 0));
                    String outRasterName = GUIUtils.getStringValue(componentsWithActions.getComponent("01", GUIUtils.OUTPUT, 1));//output raster name

                    //ckeck
                    checkValues(this, flowDirRaster, selectionMouse, xCoord, yCoord, layerSelected, outRasterName);

                    List coords = new ArrayList<Coordinate>();

                    //convert string values in correct objects
                    //get input flow dir                    
                    DoubleBasicGrid demGrid = RasterUtils.getDoubleBasicGrid((RasterComboBox) componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));
                    //Calculate flow dir envelope
                    double xMax = demGrid.getLowerLeftCoord().x + (demGrid.getCellSize() * demGrid.getColumnCount());
                    double yMax = demGrid.getLowerLeftCoord().y + (demGrid.getCellSize() * demGrid.getRowCount());
                    
                    Envelope env = new Envelope(demGrid.getLowerLeftCoord().x, xMax, demGrid.getLowerLeftCoord().y, yMax);

                    if (selectionMouse) {

                        coords.add(new Coordinate(Double.parseDouble(xCoord), Double.parseDouble(yCoord)));

                    } else { //get Layer
                        //from name to Layer
                        Layer layer = PluginUtils.getLayerSelected((LayerComboBox)componentsWithActions.getComponent("04", GUIUtils.OTHER, 1));

                        // Get selected features, or all features if none selected, or return error if none present
                        Collection features = context.getLayerViewPanel().getSelectionManager().getFeatureSelection().getFeaturesWithSelectedItems(layer);

                        if (features.isEmpty()) {
                            features = layer.getFeatureCollectionWrapper().getFeatures();
                        }

                        Feature feature;
                        Iterator iter = features.iterator();
                        int count = 0;
                        while (iter.hasNext()) {
                            feature = (Feature) iter.next();
                            // Check feature to be point
                            if (feature.getGeometry().getGeometryType().toUpperCase().equals("POINT")) {
                                Point point = (Point) feature.getGeometry();
                                //check if coordinate is inside raster. Only inside coordinate are added to the list
                                if(env.contains(point.getCoordinate())){
                                    coords.add(new Coordinate(point.getCoordinate().x, point.getCoordinate().y));
                                }
                                break;
                            }
                        }
                    }
                    
                    if(coords.isEmpty()) throw new NullPointerException(PluginUtils.getResources().getString("WatershedPlugin.CoordinateOutside.label"));

                    //convert arrayList to array
                    Coordinate[] ar_coords = new Coordinate[coords.size()];
                    for (int n = 0; n < coords.size(); n++) {
                        ar_coords[n] = (Coordinate) coords.get(n);
                    }

                    //execute
                    WatershedExtractor watershedExtractor = new WatershedExtractor();
                    
                    Layer bluelinesLayer = PluginUtils.getLayerSelected((CustomComboBox.LayerComboBox) componentsWithActions.getComponent("01", GUIUtils.INPUT, 1));
                    LineString[] bluelines = null;
                    if(bluelinesLayer != null) {   
                        bluelines = GeometryUtils.getLineStringsFromFeatures(bluelinesLayer.getFeatureCollectionWrapper());
                    }
                    
                    
                    FlowDirsCalculator flowDirCalc = new FlowDirsCalculator(demGrid, FlowDirsStripe.FlowDirAlgorithm.D8, bluelines, 100d);
                    FlowDirBasicGrid flowDirGrid = flowDirCalc.calculate();
                    
                    DoubleBasicGrid watershedGrid = watershedExtractor.extract(flowDirGrid, ar_coords, clipOutput);

                    //Create the output file and display on OJ 
                    //Save grid as tiff
                    RasterUtils.saveOutputRasterAsTiff(watershedGrid, new File(outRasterName));
                    //Display raster on OJ from file                
                    RasterUtils.displayRasterFileOnOJ(
                            context.getWorkbenchContext(),
                            new File(outRasterName),
                            null);

                    JOptionPane.showMessageDialog(super.getInitialDialog(), 
                            PluginUtils.getResources().getString("SetWorkspacePlugin.Done.message"), PluginUtils.plugInName, JOptionPane.INFORMATION_MESSAGE);
                
                } catch (WarningException ex) {
                    JOptionPane.showMessageDialog(super.getInitialDialog(), ex.getMessage(), PluginUtils.plugInName, JOptionPane.WARNING_MESSAGE);
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
        return PluginUtils.getResources().getString("WatershedPlugin.PlugInName.label");
    }

    private void enableElements(JRadioButton mouse, JRadioButton layer, JLabel xcoordL,
            JTextField xcoordF, JButton choose, JLabel ycoordL, JTextField ycoordF,
            JLabel layerL, JComboBox layerF) {

        xcoordL.setEnabled(mouse.isSelected());
        xcoordF.setEnabled(mouse.isSelected());
        choose.setEnabled(mouse.isSelected());

        ycoordL.setEnabled(mouse.isSelected());
        ycoordF.setEnabled(mouse.isSelected());

        layerL.setEnabled(layer.isSelected());
        layerF.setEnabled(layer.isSelected());
    }

    private void checkValues(JComponent component, String dem, boolean mouseSel, String xcoord, String ycoord,
            String layer, String outRaster) throws IOException, Exception {

        GUIUtils.checkStringValue(dem, DEM_LABEL);
        if (mouseSel) {
            GUIUtils.checkStringValue(xcoord, XCOORD_LABEL);
            GUIUtils.checkStringValue(ycoord, YCOORD_LABEL);
        } else {
            GUIUtils.checkStringValue(layer, LAYER_LABEL);
        }

        GUIUtils.checkFileValue(outRaster, GUIUtils.getOutputRasterString());

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
//    private RasterImageLayer[] ar_rasterImageLayers;
    private final InitialDialog initialDialog;
//    private Layer[] layers;
    private final String DEM_LABEL = PluginUtils.getResources().getString("KlemGUI.InputFilledDem.label");
//    private final String OUT_RASTER_LABEL = PluginUtils.getResources().getString("KlemGUI.OutputRaster.label");
    private final String XCOORD_LABEL = "x";
    private final String YCOORD_LABEL = "y";
    private final String LAYER_LABEL = PluginUtils.getResources().getString("KlemGUI.Layer.label");
    private final String CLIP_OUTPUT = PluginUtils.getResources().getString("WatershedPlugin.ClipOutput");
    private final String WATERSHED_LABEL = PluginUtils.getResources().getString("WatershedPlugin.PlugInName.label");
    
    private final LayerablesList layerablesList;
}
