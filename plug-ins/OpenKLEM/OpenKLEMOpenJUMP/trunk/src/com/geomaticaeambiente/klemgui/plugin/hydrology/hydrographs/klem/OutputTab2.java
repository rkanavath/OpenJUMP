package com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem;

import it.geomaticaeambiente.klem.SimulationOutput;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;

import org.openjump.core.apitools.IOTools;

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
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;

/**
 *
 * @author Geomatica
 */
public class OutputTab2 extends AbstractInputKlemPlugin {

    public OutputTab2(PlugInContext context, InitialDialog initialDialog,
            LayerablesList layerablesList, SimulationOutput simOut) {
        super(context, initialDialog);
        this.simOut = simOut;
        this.context = context;
        this.layerablesList = layerablesList;
    }

    public double[][] tempData() {
        final double[][] initialdata = new double[][] { { 0.0, 0.0 },
                { 0.0, 0.0 } }; // TODO: remove and change with real data
        return initialdata;
    }

    @Override
    public InitialData setInitialData() {

        final double[][] tempdata = tempData(); // TODO: remove and change with
                                                // real data

        final InitialData initialData = new InitialData();

        // Area-Elevation
        initialData
                .setParam_Labels(new String[] { AREA_ELEV }, GUIUtils.OUTPUT); // 00
        final PersonalTable areaElevtable = new PersonalTable(
                setSimulatedFlowParams(tempdata, new String[] {
                        ELEVATION_LABEL, AREA_LABEL }), null, false, false,
                false, false, false, null, null, false);
        initialData
                .setParam_PersonalTable(areaElevtable, null, GUIUtils.OUTPUT); // 01

        // TODO: Area-Elevation chart

        // Area-time
        initialData
                .setParam_Labels(new String[] { AREA_TIME }, GUIUtils.OUTPUT); // 02
        final PersonalTable areaTimeTable = new PersonalTable(
                setSimulatedFlowParams(tempdata, new String[] { TIME_LABEL,
                        AREA_LABEL }), null, false, false, false, false, false,
                null, null, false);

        initialData
                .setParam_PersonalTable(areaTimeTable, null, GUIUtils.OUTPUT); // 03

        // Area-cn
        initialData.setParam_Labels(new String[] { AREA_CN }, GUIUtils.OUTPUT); // 04
        final PersonalTable areaCNTable = new PersonalTable(
                setSimulatedFlowParams(tempdata, new String[] { CN_LABEL,
                        AREA_LABEL }), null, false, false, false, false, false,
                null, null, false);
        initialData.setParam_PersonalTable(areaCNTable, null, GUIUtils.OUTPUT); // 05
        initialData.setParam_Action(new ActionObject(PluginUtils.getResources()
                .getString("HydrographKlemPlugin.Output.ExportTables.button")),
                GUIUtils.OUTPUT);// 06

        return initialData;

    }

    private JTable jTable = new JTable();
    private JTable jTable1 = new JTable();
    private JTable jTable2 = new JTable();

    private KlemProperties klemProps;

    @Override
    public ComponentsTreeMap setComponentsActions(
            ComponentsTreeMap personalTreeMap) {

        setLabelFormat(
                (JLabel) personalTreeMap.getComponent("00", GUIUtils.OUTPUT, 0),
                (JLabel) personalTreeMap.getComponent("02", GUIUtils.OUTPUT, 0),
                (JLabel) personalTreeMap.getComponent("04", GUIUtils.OUTPUT, 0));

        final JButton jButton_Export_Tables = (JButton) personalTreeMap
                .getComponent("06", GUIUtils.OUTPUT, 0); // button choose
        jButton_Export_Tables.setPreferredSize(new Dimension(300, 20));
        jButton_Export_Tables.addActionListener(new ActionListener() {
            double[][] tempdata = tempData();

            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    final String dir = klemProps.getOutputFolder()
                            .getAbsolutePath().concat(File.separator)
                            .concat("tables");
                    final File tableDir = new File(dir);
                    if (!tableDir.exists()) {
                        tableDir.mkdir();
                    }
                    // Output for OpenJUMP AdditionalResult frame
                    jTable = new JTable();
                    jTable.setModel(setSimulatedFlowParams(tempdata,
                            new String[] { ELEVATION_LABEL, AREA_LABEL }));

                    jTable1 = new JTable();
                    jTable1.setModel(setSimulatedFlowParams(tempdata,
                            new String[] { TIME_LABEL, AREA_LABEL }));

                    jTable2 = new JTable();
                    jTable2.setModel(setSimulatedFlowParams(tempdata,
                            new String[] { CN_LABEL, AREA_LABEL }));

                    IOTools.saveCSV(jTable,
                            dir.concat(File.separator).concat(AREA_ELEV));
                    IOTools.saveCSV(jTable1,
                            dir.concat(File.separator).concat(AREA_TIME));
                    IOTools.saveCSV(jTable2,
                            dir.concat(File.separator).concat(AREA_CN));

                    JOptionPane.showMessageDialog(
                            context.getActiveInternalFrame(),
                            PluginUtils
                                    .getResources()
                                    .getString(
                                            "HydrographKlemPlugin.Output.ExportChart.done"),
                            PluginUtils.plugInName,
                            JOptionPane.INFORMATION_MESSAGE);
                } catch (final Exception ex) {
                    ErrorDialog.show(context.getWorkbenchFrame(),
                            PluginUtils.plugInName, ex.toString(),
                            StringUtil.stackTrace(ex));
                }
            }
        });

        return personalTreeMap;
    }

    public void saveTableModelToCSV(DefaultTableModel model, String output) {
        final JTable table = new JTable();
        table.setModel(model);
        try {
            IOTools.saveCSV(table, output);
            JOptionPane.showMessageDialog(
                    context.getActiveInternalFrame(),
                    PluginUtils.getResources().getString(
                            "HydrographKlemPlugin.Output.ExportChart.done"),
                    PluginUtils.plugInName, JOptionPane.INFORMATION_MESSAGE);
        } catch (final Exception e) {
            ErrorDialog.show(context.getWorkbenchFrame(),
                    PluginUtils.plugInName, e.toString(),
                    StringUtil.stackTrace(e));
        }

    }

    @Override
    public JPanel buildPluginPanel(ComponentsTreeMap componentsWithActions) {
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

        mainPanel.setRightButtonText(PluginUtils.getResources().getString(
                "MainPanel.ExecuteButton.text"));

        return mainPanel;
    }

    private DefaultTableModel setSimulatedFlowParams(double[][] data,
            String[] cols) {

        final String[][] dataVal = new String[data.length][cols.length];
        for (int r = 0; r < data.length; r++) {
            dataVal[r][0] = Double.toString(data[r][0]);
            dataVal[r][1] = Double.toString(data[r][1]);
        }

        final DefaultTableModel dtm = new DefaultTableModel(dataVal, cols) {
            @Override
            public boolean isCellEditable(int row, int column) {
                // all cells false
                return false;
            }
        };
        return dtm;
    }

    private void setLabelFormat(JLabel... labels) {
        for (final JLabel label : labels) {
            label.setFont(new Font("Tahoma", 1, 12)); // NOI18N
            label.setForeground(new Color(102, 102, 102));
        }
    }

    public void setKlemProperties(KlemProperties klemPropr) {
        klemProp = klemPropr;
    }

    private MainPanel mainPanel;
    private KlemProperties klemProp;
    private final PlugInContext context;
    private final SimulationOutput simOut;
    private final String AREA_ELEV = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.OutputAreaElev.label");
    private final String AREA_TIME = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.OutputAreaTime.label");
    private final String AREA_CN = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.OutputAreaCn.label");

    private final String ELEVATION_LABEL = PluginUtils.getResources()
            .getString("HydrographKlemPlugin.OutputElevationLabel.label");
    private final String AREA_LABEL = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.OutputAreaLabel.label");
    private final String TIME_LABEL = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.OutputTime.label");
    private final String CN_LABEL = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.OutputCN.label");
    private final LayerablesList layerablesList;
}
