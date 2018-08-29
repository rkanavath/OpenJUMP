package com.geomaticaeambiente.klemgui.plugin.hydrology;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableModel;

import com.geomaticaeambiente.klemgui.ui.CustomComboBox;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PersonalTable;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.klemgui.utils.ValueRangeGroup;
import com.geomaticaeambiente.openjump.klem.cn.CurveNumberCalculator;
import com.geomaticaeambiente.openjump.klem.cn.LandUseSoilGroupTuple;
import com.geomaticaeambiente.openjump.klem.cn.SoilGroupLandUseTable;
import com.geomaticaeambiente.openjump.klem.cn.ValuesRange;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;

/**
 *
 * @author Geomatica
 */
public class CurveNumberPlugIn extends AbstractInputKlemPlugin {

    public CurveNumberPlugIn(PlugInContext context,
            InitialDialog initialDialog, LayerablesList rasterLayersList) {
        super(context, initialDialog);
        this.context = context;
        layerablesList = rasterLayersList;
    }

    @Override
    public InitialData setInitialData() throws IOException {

        // rasterImageLayers =
        // PluginUtils.getRasterImageLayers(layerablesList.getLayerables());

        final InitialData initialData = new InitialData();
        // input params
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(LAND_USE_LABEL), PluginUtils
                        .getRasterImageLayers(layerablesList.getLayerables()),
                GUIUtils.INPUT);
        initialData.setParam_Label_TextBox(GUIUtils
                .setGUILabel(RASTER_HYDRO_GROUPS_LABEL), PluginUtils
                .getRasterImageLayers(layerablesList.getLayerables()),
                GUIUtils.INPUT);

        // other params
        // initialData.setParam_Labels(new String[]{RECLASS_TABLE_LABEL},
        // GUIUtils.OTHER);

        final PersonalTable hydroGroupTable = new PersonalTable(
                hydroGroupTableModel(), null, false, false, false, false,
                false, null, null, false);
        initialData.setParam_PersonalTable(hydroGroupTable, null,
                GUIUtils.OTHER);

        initialData.setParam_Labels(new String[] { "" }, GUIUtils.OTHER);

        final PersonalTable personalTable = new PersonalTable(cnTableModel(),
                null, true, true, true, false, true, null, null, true);
        final FileNameExtensionFilter table6ExtFilter = new FileNameExtensionFilter(
                PluginUtils.getResources().getString(
                        "CurveNumberPlugIn.CurveNumberTableDescription.label"),
                new String[] { "table6" });
        initialData.setParam_PersonalTable(personalTable, table6ExtFilter,
                GUIUtils.OTHER);

        // table buttons
        // initialData.setParam_Action(new ActionObject[]{new
        // ActionObject(bundle.getString("KlemGUI.LoadButton.label")),
        // new ActionObject(bundle.getString("KlemGUI.SaveButton.label"))},
        // GUIUtils.OTHER);
        // output
        initialData.setParam_Label_TextBox_Button(
                GUIUtils.setGUILabel(CN_LABEL), "", new ActionObject(""),
                GUIUtils.OUTPUT);
        return initialData;

    }

    @Override
    public ComponentsTreeMap setComponentsActions(
            ComponentsTreeMap personalTreeMap) {

        // table
        // final PersonalTableComponents1 personalComp_Table =
        // (PersonalTableComponents1) personalTreeMap.getComponent("03Other",
        // 0); //table
        // personalComp_Table.setFileFilterInfo(
        // bundle.getString("CurveNumberPlugIn.CurveNumberTableDescription.label"),
        // new String[]{"table6"});
        final JTextField outputTextField = (JTextField) personalTreeMap
                .getComponent("00", GUIUtils.OUTPUT, 1);
        final JButton outputButton = (JButton) personalTreeMap.getComponent(
                "00", GUIUtils.OUTPUT, 2);
        outputButton.setIcon(PluginUtils.getFolderIcon());
        outputButton.addActionListener(GUIUtils
                .setSaveRasterTif(outputTextField));

        return personalTreeMap;
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

                    // input values
                    final String landUseRaster = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "00", GUIUtils.INPUT, 1));
                    final String hydroGroupsRaster = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "01", GUIUtils.INPUT, 1));

                    // Other values
                    final String groupsTableValues = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "00", GUIUtils.OTHER, 0));

                    // Output
                    final String cnRasterOut = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "00", GUIUtils.OUTPUT, 1));

                    checksValues(landUseRaster, hydroGroupsRaster, cnRasterOut);

                    // Get values
                    final DoubleBasicGrid landUserGrid = RasterUtils
                            .getDoubleBasicGrid((CustomComboBox.RasterComboBox) componentsWithActions
                                    .getComponent("00", GUIUtils.INPUT, 1));
                    final DoubleBasicGrid hydroGroupGrid = RasterUtils
                            .getDoubleBasicGrid((CustomComboBox.RasterComboBox) componentsWithActions
                                    .getComponent("01", GUIUtils.INPUT, 1));

                    final ValueRangeGroup[] valRangeGroup = getValueRangeGroupFromString(groupsTableValues);

                    final String table = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "02", GUIUtils.OTHER, 0));
                    final SoilGroupLandUseTable soilGroup = fromTableToSoilGroup(
                            table, valRangeGroup);

                    // Calculate raster
                    final CurveNumberCalculator curveNumberCalculator = new CurveNumberCalculator();
                    final DoubleBasicGrid curveNumberGrid = curveNumberCalculator
                            .calculateCn(landUserGrid, hydroGroupGrid,
                                    soilGroup);

                    // display raster on TOC
                    // Save grid as tiff
                    RasterUtils.saveOutputRasterAsTiff(curveNumberGrid,
                            new File(cnRasterOut));
                    // Display raster on OJ from file
                    RasterUtils.displayRasterFileOnOJ(
                            context.getWorkbenchContext(),
                            new File(cnRasterOut), null);

                    JOptionPane.showMessageDialog(
                            super.getInitialDialog(),
                            PluginUtils.getResources().getString(
                                    "SetWorkspacePlugin.Done.message"),
                            PluginUtils.plugInName,
                            JOptionPane.INFORMATION_MESSAGE);

                } catch (final Exception ex) {
                    ErrorDialog.show(super.getInitialDialog(),
                            PluginUtils.plugInName, ex.toString(),
                            StringUtil.stackTrace(ex));
                }
            }

            @Override
            public void leftButton() {
                throw new UnsupportedOperationException("Not supported yet."); // To
                                                                               // change
                                                                               // body
                                                                               // of
                                                                               // generated
                                                                               // methods,
                                                                               // choose
                                                                               // Tools
                                                                               // |
                                                                               // Templates.
            }

            @Override
            public void centerButton() {
                throw new UnsupportedOperationException("Not supported yet."); // To
                                                                               // change
                                                                               // body
                                                                               // of
                                                                               // generated
                                                                               // methods,
                                                                               // choose
                                                                               // Tools
                                                                               // |
                                                                               // Templates.
            }
        };

        return mainPanel;
    }

    private DefaultTableModel cnTableModel()
            throws UnsupportedEncodingException, IOException {

        final InputStream in = getClass().getResourceAsStream(
                "/com/geomaticaeambiente/klemgui/plugin/hydrology/"
                        + "CurveNumberTable" + ".table6");
        final Reader reader = new InputStreamReader(in, "utf-8");
        final BufferedReader buffReader = new BufferedReader(reader);

        final List<String[]> data_l = new ArrayList<String[]>();
        String line = null;
        while ((line = buffReader.readLine()) != null) {

            final String[] lines = line.split(",");
            data_l.add(lines);

        }

        final String[][] data = data_l.toArray(new String[data_l.size()][]);

        final String col[] = { FROM_LABEL, TO_LABEL, "Gr A", "Gr B", "Gr C",
                "Gr D" };

        final DefaultTableModel dtm = new DefaultTableModel(data, col);

        return dtm;
    }

    private DefaultTableModel hydroGroupTableModel() {

        final String col[] = { "Group", FROM_LABEL, TO_LABEL };
        final String[][] data = new String[][] { { A_LABEL, "1", "1" },
                { B_LABEL, "2", "2" }, { C_LABEL, "3", "3" },
                { D_LABEL, "4", "4" } };

        final DefaultTableModel dtm = new DefaultTableModel(data, col);

        return dtm;
    }

    private ValueRangeGroup[] getValueRangeGroupFromString(String tableValues) {

        final String[] rows = tableValues.split(";");
        String[] columns;
        final ValueRangeGroup[] group = new ValueRangeGroup[rows.length];

        for (int v = 0; v < rows.length; v++) {
            columns = rows[v].split("_");

            group[v] = new ValueRangeGroup(Double.parseDouble(columns[1]),
                    Double.parseDouble(columns[2]), columns[0]);// range uso del
                                                                // suolo
        }
        return group;
    }

    private SoilGroupLandUseTable fromTableToSoilGroup(String tableValues,
            ValueRangeGroup[] groupRange) {

        final String[] rows = tableValues.split(";");
        String[] columns;
        final SoilGroupLandUseTable soilGroupLandUseTable = new SoilGroupLandUseTable();
        LandUseSoilGroupTuple lusgt;
        for (final String row : rows) {
            columns = row.split("_");
            for (int g = 0; g < groupRange.length; g++) {
                final ValuesRange landUseValueRange = new ValuesRange(
                        Double.parseDouble(columns[0]),
                        Double.parseDouble(columns[1]));// range uso del suolo
                final ValuesRange soilGroupRange = groupRange[g]
                        .getValueRange();
                final double cn = Double.parseDouble(columns[g + 2]);
                lusgt = new LandUseSoilGroupTuple(landUseValueRange,
                        soilGroupRange, cn);
                soilGroupLandUseTable.addTuple(lusgt);
            }
        }
        return soilGroupLandUseTable;
    }

    private void checksValues(String dem, String flowDir, String outputRaster)
            throws IOException, Exception {

        GUIUtils.checkStringValue(dem, LAND_USE_LABEL);
        GUIUtils.checkStringValue(flowDir, HYDRO_GROUPS_LABEL);
        GUIUtils.checkFileValue(outputRaster, GUIUtils.getOutputRasterString());

    }

    @Override
    public String toString() {
        return PluginUtils.getResources().getString(
                "CurveNumberPlugIn.PlugInName.label");
    }

    private final PlugInContext context;
    private MainPanel mainPanel;
    private final String LAND_USE_LABEL = PluginUtils.getResources().getString(
            "CurveNumberPlugIn.LandUseRaster.label");
    private final String RASTER_HYDRO_GROUPS_LABEL = PluginUtils.getResources()
            .getString("CurveNumberPlugIn.HydroGroupsRaster.label");
    // private final String RECLASS_TABLE_LABEL =
    // PluginUtils.getResources().getString("CurveNumberPlugIn.reclassTable.label");
    private final String HYDRO_GROUPS_LABEL = PluginUtils.getResources()
            .getString("CurveNumberPlugIn.HydroGroupsLabel.label");
    private final String FROM_LABEL = PluginUtils.getResources().getString(
            "KlemGUI.From.label");
    private final String TO_LABEL = PluginUtils.getResources().getString(
            "KlemGUI.To.label");
    private final String A_LABEL = PluginUtils.getResources().getString(
            "CurveNumberPlugIn.HydroGroupsA.label");
    private final String B_LABEL = PluginUtils.getResources().getString(
            "CurveNumberPlugIn.HydroGroupsB.label");
    private final String C_LABEL = PluginUtils.getResources().getString(
            "CurveNumberPlugIn.HydroGroupsC.label");
    private final String D_LABEL = PluginUtils.getResources().getString(
            "CurveNumberPlugIn.HydroGroupsD.label");
    private final String CURVE_NUMBER_DESCRIPTION = PluginUtils.getResources()
            .getString("CurveNumberPlugIn.CurveNumberTableDescription.label");
    private final String CN_LABEL = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.CNRaster.label");

    private final LayerablesList layerablesList;

}
