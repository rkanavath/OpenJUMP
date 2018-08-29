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
import com.geomaticaeambiente.klemgui.utils.Header;
import com.geomaticaeambiente.klemgui.utils.PersonalTable;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.rastertools.RasterReclassifier;
import com.geomaticaeambiente.openjump.klem.rastertools.ReclassTuple;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableModel;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.rasterimage.TiffTags;

/**
 *
 * @author Geomatica
 */
public class HydrologyGroupsPlugin extends AbstractInputKlemPlugin {

    public HydrologyGroupsPlugin(PlugInContext context, InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {
//        rasterImageLayers = PluginUtils.getRasterImageLayers(context);

        InitialData initialData = new InitialData();

        PersonalTable hydrogroupTable = new PersonalTable(setGroupsTable(
                new String[]{FROM_LABEL, TO_LABEL, HYDROLOGY_GROUPS_TABLE_LABEL}),
                new Header(new String[]{FROM_LABEL, TO_LABEL, HYDROLOGY_GROUPS_TABLE_LABEL}),
                true, true, true, false, true, null, null, false);
        FileNameExtensionFilter tableExtFilter = new FileNameExtensionFilter(
                FILE_FILTER_DESCRIPTION,
                new String[]{"table"});

        //input data
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(GEO_LITHO_LABEL), PluginUtils.getRasterImageLayers(layerablesList.getLayerables()), GUIUtils.INPUT);

        initialData.setParam_Labels(new String[]{LITHO_TABLE_LABEL}, GUIUtils.OTHER);//title litho table
        initialData.setParam_PersonalTable(hydrogroupTable, tableExtFilter, GUIUtils.OTHER);
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(HYDRO_DEFAULT_VALUE), "1", GUIUtils.OTHER);

        //output
        initialData.setParam_Label_TextBox_Button(GUIUtils.setGUILabel(HYDRO_GROUPS_OUT_LABEL), "", new ActionObject(""), GUIUtils.OUTPUT);

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

                    String geoRaster = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));
                    double defaultValue = GUIUtils.getDoubleValue(componentsWithActions.getComponent("02", GUIUtils.OTHER, 1));
                    String outRaster = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.OUTPUT, 1));

                    //check input values
                    GUIUtils.checkStringValue(geoRaster, HYDRO_DEFAULT_VALUE);
                    GUIUtils.checkFileValue(outRaster, HYDRO_GROUPS_OUT_LABEL);

                    //get input raster as rasterImageLayer from string
                    RasterImageLayer inputRasterSelected = PluginUtils.getRasterImageLayerSelected((CustomComboBox.RasterComboBox) componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));
                    DoubleBasicGrid geoGrid = RasterUtils.getDoubleBasicGrid(inputRasterSelected);

                    String newValuesTable = GUIUtils.getStringValue(componentsWithActions.getComponent("01", GUIUtils.OTHER, 0));
                    //sort table values

                    ReclassTuple[] pairs = getReclassPairFromString(
                            newValuesTable,
                            inputRasterSelected.getMetadata().getStats().getMin(0),
                            inputRasterSelected.getMetadata().getStats().getMax(0),
                            defaultValue);

                    //exceute reclassification
                    RasterReclassifier reclassifier = new RasterReclassifier();
                    DoubleBasicGrid reclassRaster = reclassifier.reclassify(geoGrid, pairs);

                    ///Create the output rasterImageLayer and display on OJ    
                    //Save grid as tiff
                    RasterUtils.saveOutputRasterAsTiff(reclassRaster, new File(outRaster));
                    //Display raster on OJ from file                
                    RasterUtils.displayRasterFileOnOJ(
                            context.getWorkbenchContext(),
                            new File(outRaster),
                            null);

                    JOptionPane.showMessageDialog(super.getInitialDialog(),
                            PluginUtils.getResources().getString("SetWorkspacePlugin.Done.message"), PluginUtils.plugInName, JOptionPane.INFORMATION_MESSAGE);
                    
                } catch (IOException ex) {
                    Logger.getLogger(HydrologyGroupsPlugin.class.getName()).log(Level.SEVERE, null, ex);
                } catch (TiffTags.TiffReadingException ex) {
                    Logger.getLogger(HydrologyGroupsPlugin.class.getName()).log(Level.SEVERE, null, ex);
                } catch (Exception ex) {
                    Logger.getLogger(HydrologyGroupsPlugin.class.getName()).log(Level.SEVERE, null, ex);
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

    private DefaultTableModel setGroupsTable(String[] col) {
        //empty table version               
        String[][] data = new String[1][col.length];
        for (String[] data1 : data) {
            for (int c = 0; c < data[0].length; c++) {
                data1[c] = null;
            }
        }

        DefaultTableModel dtm = new DefaultTableModel(data, col);

        return dtm;
    }

    public ReclassTuple[] getReclassPairFromString(String values, double minRasValue, double maxRasValue, double defaultValue) throws NullPointerException {

        String[] ar_values = values.split(";");

        String[][] d_ar_values = new String[ar_values.length][];
        for (int n = 0; n < ar_values.length; n++) {
            d_ar_values[n] = ar_values[n].split("_");
        }

        //sort values
        Arrays.sort(d_ar_values, new Comparator<String[]>() {

            @Override
            public int compare(String[] o1, String[] o2) {
                final String value1 = o1[0];
                final String value2 = o2[0];

                return value1.compareTo(value2);
            }
        });

        for (String[] s : d_ar_values) {
            System.out.println(s[0] + " " + s[1]);

        }

        //add default value to array list
        List pairs = new ArrayList<ReclassTuple>();
        if (minRasValue != Double.parseDouble(d_ar_values[0][0])) {
            pairs.add(new ReclassTuple(minRasValue, (Double.parseDouble(d_ar_values[0][0])), defaultValue));
        }
        pairs.add(new ReclassTuple(Double.parseDouble(d_ar_values[0][0]), (Double.parseDouble(d_ar_values[0][1])), Double.parseDouble(d_ar_values[0][2])));

        for (int n = 1; n < d_ar_values.length; n++) {

            if (Double.parseDouble(d_ar_values[n][0]) != Double.parseDouble(d_ar_values[n - 1][1])) {

                pairs.add(new ReclassTuple(Double.parseDouble(d_ar_values[n - 1][1]), (Double.parseDouble(d_ar_values[n][0])), defaultValue));

            }
            pairs.add(new ReclassTuple(Double.parseDouble(d_ar_values[n][0]), (Double.parseDouble(d_ar_values[n][1])), Double.parseDouble(d_ar_values[n][2])));

        }

        if (maxRasValue > Double.parseDouble(d_ar_values[d_ar_values.length - 1][1])) {
            pairs.add(new ReclassTuple(Double.parseDouble(d_ar_values[d_ar_values.length - 1][1]), maxRasValue, defaultValue));
        }

        for (Object pair : pairs) {
            System.out.println(pair.toString());
        }

        ReclassTuple[] reclassPair = new ReclassTuple[pairs.size()];
        for (int n = 0; n < pairs.size(); n++) {

            double minVal = ((ReclassTuple) pairs.get(n)).getOldRangeMin();
            double maxVal = ((ReclassTuple) pairs.get(n)).getOldRangeMax();
            double newVal = ((ReclassTuple) pairs.get(n)).getNewValue();

            reclassPair[n] = new ReclassTuple(minVal, maxVal, newVal);
        }

        return reclassPair;

    }

    @Override
    public String toString() {
        return PluginUtils.getResources().getString("HydrologicalGroups.Hydrology.label");
    }

    private final PlugInContext context;
    private MainPanel mainPanel;
    private final String GEO_LITHO_LABEL = PluginUtils.getResources().getString("HydrologicalGroups.GeoLithoLabel.label");
    private final String LITHO_TABLE_LABEL = PluginUtils.getResources().getString("HydrologicalGroups.LithoTable.label");
    //private final String Hydro_TABLE_LABEL = PluginUtils.getResources().getString("HydrologicalGroups.HydroTable.label");
    private final String HYDRO_GROUPS_OUT_LABEL = PluginUtils.getResources().getString("HydrologicalGroups.HydroRaster.label");
    private final String FROM_LABEL = PluginUtils.getResources().getString("KlemGUI.From.label");
    private final String TO_LABEL = PluginUtils.getResources().getString("KlemGUI.To.label");
    //private final String PERMEABILITY_LABEl = PluginUtils.getResources().getString("HydrologicalGroups.Permeability.label");
    private final String HYDROLOGY_GROUPS_TABLE_LABEL = PluginUtils.getResources().getString("HydrologicalGroups.Hydrology.label");
    private final String FILE_FILTER_DESCRIPTION = PluginUtils.getResources().getString("HydrologicalGroups.TableFileDescription.label");
    //private final String LITHO_DEFAUL_VALUE = PluginUtils.getResources().getString("HydrologicalGroups.DefaultLithoVal.label");
    private final String HYDRO_DEFAULT_VALUE = PluginUtils.getResources().getString("HydrologicalGroups.DefaultHydrogroupVal.label");
    //private final String UPDATE_HYDRO_TABLE = PluginUtils.getResources().getString("HydrographKlemPlugin.UpdateHydroTable.label");
    private final LayerablesList layerablesList;
}
