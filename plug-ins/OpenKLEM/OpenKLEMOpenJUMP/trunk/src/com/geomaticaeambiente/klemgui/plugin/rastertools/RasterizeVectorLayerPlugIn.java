package com.geomaticaeambiente.klemgui.plugin.rastertools;

import com.geomaticaeambiente.klemgui.exceptions.WarningException;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.rastertools.Rasterizer;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import org.openjump.core.rasterimage.RasterImageLayer;

/**
 *
 * @author Geomatica
 */
public class RasterizeVectorLayerPlugIn extends AbstractInputKlemPlugin {

    public RasterizeVectorLayerPlugIn(PlugInContext context, InitialDialog initialDialog, LayerablesList rasterLayersList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = rasterLayersList;
    }

    @Override
    public InitialData setInitialData() {

        layers = PluginUtils.getLayers(layerablesList.getLayerables());

        String[] attributes = new String[0];

        InitialData initialData = new InitialData();
        //input data
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(VECTOR_LAYER_LABEL), layers, GUIUtils.INPUT);
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(ATTRIBUTE_LABEL), attributes, GUIUtils.INPUT);
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(CELL_LABEL), "10", GUIUtils.INPUT);

        // Snapping
        initialData.setParam_Action(new ActionObject(false, PluginUtils.getResources().getString("RasterizeVectorLayerPlugIn.SnapRasterCheckbox.text")), GUIUtils.INPUT);
        
//        initialData.setInputParam_RadioButton(
//                new boolean[]{true,false}, 
//                new ActionObject(), 
//                new String[]{bundle.getString("RasterizeVectorLayerPlugIn.OnCellRadioButton.text"), 
//                    bundle.getString("RasterizeVectorLayerPlugIn.OnRasterRadioButton.text")});
        initialData.setParam_Label_TextBox(SNAP_RASTER_LABEL, PluginUtils.getRasterImageLayers(layerablesList.getLayerables()), GUIUtils.INPUT);
//        initialData.setInputParam_CheckBox(false, new ActionObject(), bundle.getString("RasterizeVectorLayerPlugIn.SameRasterExtentCheckBox.text"));
        

        //output information        
        initialData.setParam_Label_TextBox_Button(GUIUtils.getOutputRasterLabel(), "", new ActionObject(""), GUIUtils.OUTPUT);

        // Visibility        
        return initialData;
    }

    @Override
    public ComponentsTreeMap setComponentsActions(ComponentsTreeMap personalTreeMap) {

        final CustomComboBox.LayerComboBox jComboBox_layer = (CustomComboBox.LayerComboBox) personalTreeMap.getComponent("00", GUIUtils.INPUT, 1);

        final JComboBox jComboBox_attribute = (JComboBox) personalTreeMap.getComponent("01", GUIUtils.INPUT, 1);
        jComboBox_attribute.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
//                listLayerAttribute(jComboBox_attribute, jComboBox_layer);
//                System.out.println("hello");
            }

        });

        jComboBox_layer.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                GUIUtils.listLayerAttribute(context, jComboBox_attribute, jComboBox_layer);
            }
        });

        // IN STAND-BY
        //input data for snap to raster > there aren't in this version of rasterize             
//        final JComboBox rasterComboBox = (JComboBox) personalTreeMap.getComponent("Input5", 1);
//        final JLabel rasterLabel = (JLabel) personalTreeMap.getComponent("Input5", 0);
//                
        final JCheckBox jCheckBox_Extent = (JCheckBox) personalTreeMap.getComponent("03", GUIUtils.INPUT, 0);
        final JComboBox jComboBox_SnapRaster = (JComboBox) personalTreeMap.getComponent("04", GUIUtils.INPUT, 1);
        jComboBox_SnapRaster.setEnabled(false);
        
        jCheckBox_Extent.addActionListener(new ActionListener(){
            @Override
            public void actionPerformed(ActionEvent e) {
                jComboBox_SnapRaster.setEnabled(jCheckBox_Extent.isSelected());
            }
        });
        
        
//        final JRadioButton rbOnCell = (JRadioButton) personalTreeMap.getComponent("Input4", 0);
//        rbOnCell.setEnabled(false);
//        
//        final JRadioButton rbOnRaster = (JRadioButton) personalTreeMap.getComponent("Input4", 1);
//        rbOnRaster.setEnabled(false);
//        rbOnRaster.addActionListener(new ActionListener() {
//
//            @Override
//            public void actionPerformed(ActionEvent e) {
//                rasterLabel.setEnabled(rbOnRaster.isSelected());
//                rasterComboBox.setEnabled(rbOnRaster.isSelected());
//            }
//        });
//        
//        final JCheckBox jCheckBox = (JCheckBox) personalTreeMap.getComponent("Input3", 0);
//        jCheckBox.addActionListener(new ActionListener() {
//
//            @Override
//            public void actionPerformed(ActionEvent e) {
//               rbOnCell.setEnabled(jCheckBox.isSelected());
//               rbOnRaster.setEnabled(jCheckBox.isSelected());
//               checkExtent.setEnabled(jCheckBox.isSelected());
//            }
//        });
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
            @SuppressWarnings("empty-statement")
            public void rightButton() {
                try {

                    super.getInitialDialog().setCursor(new Cursor(Cursor.WAIT_CURSOR));
                    
                    //get input information     
                    String layerSelected = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));
                    String attributeSelected = GUIUtils.getStringValue(componentsWithActions.getComponent("01", GUIUtils.INPUT, 1));
                    double cellDimension = GUIUtils.getDoubleValue(componentsWithActions.getComponent("02", GUIUtils.INPUT, 1));

                    // Snapping
                    Envelope clipEnvelope = null;
                    boolean snap = GUIUtils.getBooleanValue(componentsWithActions.getComponent("03", GUIUtils.INPUT, 0));
                    String snapRasterName = null;
                    if(snap) {
                        snapRasterName = GUIUtils.getStringValue(componentsWithActions.getComponent("04", GUIUtils.INPUT, 1));
                    }
                    
                    //get output raster name
                    String outRasterName = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.OUTPUT, 1));
                    
                    // Check input and output values
                    checksValues(layerSelected, attributeSelected, snapRasterName, outRasterName);

                    if(snap) {
                        RasterImageLayer ril = PluginUtils.getRasterImageLayerSelected((CustomComboBox.RasterComboBox) componentsWithActions.getComponent("04", GUIUtils.INPUT, 1));
                        clipEnvelope = ril.getActualImageEnvelope();
                    }
                    
                    //from name to Layer
                    Layer layer = PluginUtils.getLayerSelected((CustomComboBox.LayerComboBox) componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));

                    // Get selected features, or all features if none selected, or return error if none present
                    Collection features = context.getLayerViewPanel().getSelectionManager().getFeatureSelection().getFeaturesWithSelectedItems(layer);

                    if (features.isEmpty()) {
                        features = layer.getFeatureCollectionWrapper().getFeatures();
                    }

                    Feature[] ar_features = new Feature[features.size()];
                    Iterator iter = features.iterator();
                    int count = 0;
                    while (iter.hasNext()) {
                        ar_features[count] = (Feature) iter.next();
                        count++;
                    }

                    //get information about attribute selected             
                    FeatureSchema featureSchema = layer.getFeatureCollectionWrapper().getFeatureSchema();
                    int fieldsCount = featureSchema.getAttributeIndex(attributeSelected);

                    Geometry[] ar_geoms = new Geometry[ar_features.length];
                    double[] ar_attributes = new double[ar_features.length];
                    
                    for(int f=0; f<ar_features.length; f++) {
                        ar_geoms[f] = ar_features[f].getGeometry();
                        ar_attributes[f] = Double.parseDouble(ar_features[f].getString(fieldsCount));
                    }
                    
                    // Rasterize
                    DoubleBasicGrid rasterized = Rasterizer.rasterize(
                            ar_geoms, ar_attributes, clipEnvelope, cellDimension);

                    //Create the output file and display on OJ  
                    //Save grid as tiff
                    RasterUtils.saveOutputRasterAsTiff(rasterized, new File(outRasterName));
                    //Display raster on OJ from file                
                    RasterUtils.displayRasterFileOnOJ(
                            context.getWorkbenchContext(),
                            new File(outRasterName),
                            null);

                    JOptionPane.showMessageDialog(super.getInitialDialog(), 
                            PluginUtils.getResources().getString("SetWorkspacePlugin.Done.message"), PluginUtils.plugInName, JOptionPane.INFORMATION_MESSAGE);
                    
                } catch (OutOfMemoryError out) {
                    ErrorDialog.show(
                            super.getInitialDialog(),
                            PluginUtils.plugInName,
                            out.toString(),
                            StringUtil.stackTrace(out));
                } catch (WarningException ex) {
                    JOptionPane.showMessageDialog(super.getInitialDialog(), ex.getMessage(), PluginUtils.plugInName, JOptionPane.WARNING_MESSAGE);
                } catch (Exception ex) {
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
        return PluginUtils.getResources().getString("RasterizeVectorLayerPlugin.PlugInName.label");
    }

    private void checksValues(String vectorLayer, String attributes, String snapRaster, String outRaster) throws IOException, Exception {

        GUIUtils.checkStringValue(vectorLayer, VECTOR_LAYER_LABEL);
        GUIUtils.checkStringValue(attributes, ATTRIBUTE_LABEL);
        if(snapRaster != null) {
            GUIUtils.checkStringValue(snapRaster, SNAP_RASTER_LABEL);
        }
        GUIUtils.checkFileValue(outRaster, GUIUtils.getOutputRasterString());

    }

    private MainPanel mainPanel;
    private final PlugInContext context;
    private Layer[] layers;
    private final Map<Integer, Integer> attribHt = new HashMap<Integer, Integer>();

    private final String VECTOR_LAYER_LABEL = PluginUtils.getResources().getString("RasterizeCevtorLayerPlugin.ComboBox1Label.text");
    private final String ATTRIBUTE_LABEL = PluginUtils.getResources().getString("RasterizeCevtorLayerPlugin.ComboBox2Label.text");
    private final String CELL_LABEL = PluginUtils.getResources().getString("RasterizeVectorLayerPlugIn.CellDimensionLabel.text");
    private final String SNAP_RASTER_LABEL = GUIUtils.setGUILabel(PluginUtils.getResources().getString("KlemGUI.Raster.label"));
    
    private final LayerablesList layerablesList;

}
