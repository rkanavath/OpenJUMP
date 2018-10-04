package com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem;

import it.geomaticaeambiente.klem.DesignRain;
import it.geomaticaeambiente.klem.InitialAbstraction;
import it.geomaticaeambiente.klem.Klem;
import it.geomaticaeambiente.klem.SimulationOutput;
import it.geomaticaeambiente.klem.TimeInterval.TimeIntervalUnit;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.NoninvertibleTransformException;
import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableModel;

import org.jfree.chart.ChartUtilities;

import org.openjump.core.rasterimage.ImageAndMetadata;
import org.openjump.core.rasterimage.Metadata;
import org.openjump.core.rasterimage.RasterImageIO;
import org.openjump.core.rasterimage.TiffTags;

import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem.KlemProperties.RainfallType;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.FileUtils;
import com.geomaticaeambiente.klemgui.utils.Header;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PersonalChartKlem;
import com.geomaticaeambiente.klemgui.utils.PersonalTable;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.utils.TextUtils;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.Logger;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.ThreadedBasePlugIn;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import com.vividsolutions.jump.workbench.ui.task.TaskMonitorManager;

/**
 *
 * @author Geomatica
 */
public class OutputTab extends AbstractInputKlemPlugin {

    public OutputTab(PlugInContext context, InitialDialog initialDialog,
            LayerablesList layerablesList, SimulationOutput simOut, Klem klem) {
        super(context, initialDialog);
        this.simOut = simOut;
        this.context = context;
        this.layerablesList = layerablesList;
        this.klem = klem;
    }

    @Override
    public InitialData setInitialData() {
        try {

            // Morphometrics parameters
            final PersonalTable morphoBasinTable = new PersonalTable(
                    setMorphoBasinParams(), null, false, false, false, false,
                    false, null, null, false);

            personalChartKlem = new PersonalChartKlem(simOut);

            // Rainfall and discharge values,.
            final PersonalTable hydroTable = new PersonalTable(setHydroTable(),
                    new Header(new String[] { TIME, TOT_FLOW, DIR_FLOW,
                            BASE_FLOW, TOT_RAINFALL, EFF_RAINFALL }), false,
                    false, false, false, false, null, null, false);

            // Simulation params
            final PersonalTable simParamsTable = new PersonalTable(
                    setSimulationParametersTable(), new Header(new String[] {
                            LABEL_SIMOUT_PARAM, VALUE_SIMOUT_PARAM }), false,
                    false, false, false, false, null, null, false);

            // Simulated flow
            final PersonalTable simFlowTable = new PersonalTable(
                    setSimulatedFlowParams(), null, false, false, false, false,
                    false, null, null, false);

            final InitialData initialData = new InitialData();

            // basin data
            initialData.setParam_Labels(new String[] { "Basin data" },
                    GUIUtils.OUTPUT); // 00
            initialData.setParam_PersonalTable(morphoBasinTable, null,
                    GUIUtils.OUTPUT);// 01

            // Chart rainfall flow
            initialData.setParam_Labels(new String[] { "Chart" },
                    GUIUtils.OUTPUT); // 02
            initialData.setParam_ChartPanel(personalChartKlem, GUIUtils.OUTPUT);// 03

            initialData.setParam_Action(
                    new ActionObject(PluginUtils.getResources().getString(
                            "HydrographKlemPlugin.Output.ExportChart.button")),
                    GUIUtils.OUTPUT);// 04

            // Table time flow rainfall
            initialData.setParam_Labels(new String[] { "Table" + " (values:"
                    + setHydroTable().getRowCount() + ")" }, GUIUtils.OUTPUT); // 05
            initialData.setParam_PersonalTable(hydroTable,
                    new FileNameExtensionFilter(FILE_FILTER_DESCRIPTION,
                            new String[] { "txt" }), GUIUtils.OUTPUT);// 06

            // simulation parameters
            initialData.setParam_Labels(
                    new String[] { "Simulation parameters" }, GUIUtils.OUTPUT);// 07
            initialData.setParam_PersonalTable(simParamsTable,
                    new FileNameExtensionFilter(FILE_FILTER_DESCRIPTION,
                            new String[] { "txt" }), GUIUtils.OUTPUT);// 08

            // add deflusso simulato
            initialData.setParam_Labels(
                    new String[] { "Simulated flow parameters" },
                    GUIUtils.OUTPUT);// 09
            initialData.setParam_PersonalTable(simFlowTable, null,
                    GUIUtils.OUTPUT);// 10

            initialData
                    .setParam_Action(
                            new ActionObject(
                                    PluginUtils
                                            .getResources()
                                            .getString(
                                                    "HydrographKlemPlugin.Output.ExportTables.button")),
                            GUIUtils.OUTPUT);// 11

            return initialData;

        } catch (final Exception ex) {
            ErrorDialog.show(getInitialDialog(), PluginUtils.plugInName,
                    ex.toString(), StringUtil.stackTrace(ex));
            return null;
        }
    }

    private JTable jTable = new JTable();
    private JTable jTable1 = new JTable();
    private JTable jTable2 = new JTable();
    private JTable jTable3 = new JTable();

    @Override
    public ComponentsTreeMap setComponentsActions(
            ComponentsTreeMap personalTreeMap) {

        final JLabel basinInfo = (JLabel) personalTreeMap.getComponent("00",
                GUIUtils.OUTPUT, 0);
        final JLabel chart = (JLabel) personalTreeMap.getComponent("02",
                GUIUtils.OUTPUT, 0);
        final JLabel discRainTableLabel = (JLabel) personalTreeMap
                .getComponent("05", GUIUtils.OUTPUT, 0);
        final JLabel simParamLabel = (JLabel) personalTreeMap.getComponent(
                "07", GUIUtils.OUTPUT, 0);
        final JLabel simFlowLabel = (JLabel) personalTreeMap.getComponent("09",
                GUIUtils.OUTPUT, 0);

        final JButton jButton_Export = (JButton) personalTreeMap.getComponent(
                "04", GUIUtils.OUTPUT, 0); // button choose
        jButton_Export.setPreferredSize(new Dimension(300, 20));
        jButton_Export.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    final String dir = klemProps.getOutputFolder()
                            .getAbsolutePath().concat(File.separator)
                            .concat("chart");
                    final File tableDir = new File(dir);
                    if (!tableDir.exists()) {
                        tableDir.mkdir();
                    }
                    int width;
                    int height;
                    if (personalChartKlem.getChartPanel().getWidth() < 640) {
                        width = 640;
                        height = 480;
                    } else {
                        width = personalChartKlem.getChartPanel().getWidth();
                        height = personalChartKlem.getChartPanel().getWidth()
                                - personalChartKlem.getChartPanel().getWidth()
                                / 3;
                    }
                    ChartUtilities.saveChartAsPNG(
                            new File(dir.concat(File.separator).concat(
                                    pngOutputFileName)), personalChartKlem
                                    .getChartPanel().getChart(), width, height);

                    // 640, 480);
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

        final JButton jButton_Export_Tables = (JButton) personalTreeMap
                .getComponent("11", GUIUtils.OUTPUT, 0); // button choose
        jButton_Export_Tables.setPreferredSize(new Dimension(300, 20));
        jButton_Export_Tables.addActionListener(new ActionListener() {

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
                    jTable.setModel(setMorphoBasinParams());

                    jTable1 = new JTable();
                    jTable1.setModel(setHydroTable());

                    jTable2 = new JTable();
                    jTable2.setModel(setSimulationParametersTable());

                    jTable3 = new JTable();
                    jTable3.setModel(setSimulatedFlowParams());

                    TextUtils.saveCSV(jTable,
                            dir.concat(File.separator)
                                    .concat(basinDataFileName));
                    TextUtils.saveCSV(jTable1,
                            dir.concat(File.separator).concat(tableFileName));
                    TextUtils.saveCSV(jTable2,
                            dir.concat(File.separator).concat(simParFileName));
                    TextUtils.saveCSV(
                            jTable3,
                            dir.concat(File.separator).concat(
                                    simFlowParFileName));

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

        setLabelFormat(basinInfo, chart, discRainTableLabel, simParamLabel,
                simFlowLabel);

        return personalTreeMap;
    }

    @Override
    public JPanel buildPluginPanel(ComponentsTreeMap componentsWithActions) {
        if (mainPanel != null) {
            return mainPanel;
        }
        mainPanel = new MainPanel(super.getInitialDialog(),
                componentsWithActions, false, true, true, PluginUtils
                        .getResources().getString(
                                "MainPanel.ExecuteButton.text"), layerablesList) {

            /**
                     * 
                     */
            private static final long serialVersionUID = 1L;

            @Override
            public void rightButton() {
                try {
                    final JTabbedPane mainTabelPane = super.getInitialDialog()
                            .getTabbedPane();

                    final OutputTab2 outputPanel = new OutputTab2(context,
                            super.getInitialDialog(), layerablesList, simOut);
                    outputPanel.setKlemProperties(klemProps);

                    mainTabelPane.setComponentAt(4,
                            outputPanel.getTabPluginComponents());

                    mainTabelPane.setEnabledAt(4, true);
                    mainTabelPane.setSelectedIndex(4);
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

                try {

                    AbstractPlugIn
                            .toActionListener(
                                    new ThreadedBasePlugIn() {
                                        @Override
                                        public String getName() {
                                            return null;
                                        }

                                        @Override
                                        public boolean execute(
                                                PlugInContext context)
                                                throws Exception {
                                            return true;
                                        }

                                        @Override
                                        public void run(TaskMonitor monitor,
                                                PlugInContext context)
                                                throws Exception {
                                            monitor.report(PluginUtils
                                                    .getResources()
                                                    .getString(
                                                            "HydrographKlemPlugin.button.OdsExport.exporting"));
                                            // monitor.allowCancellationRequests();

                                            reportNothingToUndoYet(context);
                                            try {

                                                final InputStream inputOdsStream = getClass()
                                                        .getResourceAsStream(
                                                                "model.ods");

                                                final File odsTempFile = FileUtils
                                                        .stream2file(
                                                                inputOdsStream,
                                                                "odsTemplate",
                                                                ".ods");
                                                final File odsOutputFile = new File(
                                                        klemProps
                                                                .getOutputFolder()
                                                                .getAbsolutePath()
                                                                + File.separator
                                                                + odsOutputFileName);
                                                OdsOutput.createOds(klem,
                                                        klemProps, odsTempFile,
                                                        odsOutputFile);
                                                JOptionPane
                                                        .showMessageDialog(
                                                                component(),
                                                                PluginUtils
                                                                        .getResources()
                                                                        .getString(
                                                                                "HydrographKlemPlugin.OdsExportComplete"),
                                                                PluginUtils.plugInName,
                                                                JOptionPane.INFORMATION_MESSAGE);
                                            } catch (final ThreadDeath td) {
                                                JOptionPane
                                                        .showMessageDialog(
                                                                component(),
                                                                "Processo interrotto",
                                                                PluginUtils.plugInName,
                                                                JOptionPane.INFORMATION_MESSAGE);
                                            } catch (final Exception ex) {
                                                JOptionPane
                                                        .showMessageDialog(
                                                                component(),
                                                                "Processo interrotto",
                                                                PluginUtils.plugInName,
                                                                JOptionPane.INFORMATION_MESSAGE);

                                            }
                                        }
                                    }, context.getWorkbenchContext(),
                                    new TaskMonitorManager())
                            .actionPerformed(null);

                    /*
                     * super.getInitialDialog().setCursor( new
                     * Cursor(Cursor.WAIT_CURSOR));
                     * 
                     * final InputStream inputOdsStream = getClass()
                     * .getResourceAsStream("model.ods");
                     * 
                     * final File odsTempFile = FileUtils.stream2file(
                     * inputOdsStream, "odsTemplate", ".ods"); final File
                     * odsOutputFile = new File(klemProps
                     * .getOutputFolder().getAbsolutePath() + File.separator +
                     * odsOutputFileName); OdsOutput.createOds(klem, klemProps,
                     * odsTempFile, odsOutputFile);
                     * super.getInitialDialog().setCursor( new
                     * Cursor(Cursor.DEFAULT_CURSOR));
                     */

                } catch (final Exception ex) {
                    super.getInitialDialog().setCursor(
                            new Cursor(Cursor.DEFAULT_CURSOR));
                    ErrorDialog.show(super.getInitialDialog(),
                            PluginUtils.plugInName, ex.toString(),
                            StringUtil.stackTrace(ex));
                }

            }

        };

        final ClassLoader cl = this.getClass().getClassLoader();
        Class c = null, c2 = null;
        try {
            c = cl.loadClass("org.odftoolkit.simple.SpreadsheetDocument");
            c2 = cl.loadClass("org.odftoolkit.simple.table.Table");
        } catch (final ClassNotFoundException e) {
            Logger.warn("Could not load ODS classes", e);
        }

        mainPanel.setRightButtonText(PluginUtils.getResources().getString(
                "MainPanel.ExecuteButton.text"));
        mainPanel.setCenterButtonText(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.button.OdsExport"));

        if (c == null || c2 == null) {
            mainPanel.getCenterButton().setEnabled(false);
            mainPanel.getCenterButton().setToolTipText(
                    PluginUtils.getResources().getString(
                            "HydrographKlemPlugin.button.OdsExport.tooltip2"));
        } else {
            mainPanel.getCenterButton().setEnabled(true);
            mainPanel.getCenterButton().setToolTipText(
                    PluginUtils.getResources().getString(
                            "HydrographKlemPlugin.button.OdsExport.tooltip"));
        }

        return mainPanel;
    }

    public Component component() {
        return super.getInitialDialog();
    }

    private DefaultTableModel setHydroTable() {

        final String[] col = new String[] { TIME, TOT_FLOW, DIR_FLOW,
                BASE_FLOW, TOT_RAINFALL, EFF_RAINFALL };

        final String[][] data = new String[simOut.getSimulationRainfall()
                .getTotalRain().length][col.length];
        for (int r = 0; r < data.length; r++) {
            data[r][0] = PluginUtils.getTwoDecimalFormatToString(simOut
                    .getSimulationDischarge().getTimeInterval()
                    .getInterval(TimeIntervalUnit.HOUR)
                    * r);
            data[r][1] = PluginUtils.getThreeDecimalFormatToString(simOut
                    .getSimulationDischarge().getTflo_out()[r]);
            data[r][2] = PluginUtils.getThreeDecimalFormatToString(simOut
                    .getSimulationDischarge().getDflo_out()[r]);
            data[r][3] = PluginUtils.getThreeDecimalFormatToString(simOut
                    .getSimulationDischarge().getBflo_out()[r]);
            data[r][4] = PluginUtils.getOneDecimalFormatToString(simOut
                    .getSimulationRainfall().getTotalRain()[r]);
            data[r][5] = PluginUtils.getOneDecimalFormatToString(simOut
                    .getSimulationRainfall().getEffectiveRain()[r]);
        }

        final DefaultTableModel dtm = new DefaultTableModel(data, col) {
            /**
             * 
             */
            private static final long serialVersionUID = 1L;

            @Override
            public boolean isCellEditable(int row, int column) {
                // all cells false
                return false;
            }
        };

        return dtm;
    }

    private DefaultTableModel setSimulationParametersTable() throws Exception {

        final String[] col = new String[] { LABEL_SIMOUT_PARAM,
                VALUE_SIMOUT_PARAM };
        final String[] labels = getSmulationParamLabels();
        final String[] values = getSmulationParamValues();

        final String[][] data = new String[labels.length][col.length];
        for (int r = 0; r < data.length; r++) {
            data[r][0] = labels[r];
            data[r][1] = values[r];
        }

        final DefaultTableModel dtm = new DefaultTableModel(data, col) {
            /**
             * 
             */
            private static final long serialVersionUID = 1L;

            @Override
            public boolean isCellEditable(int row, int column) {
                // all cells false
                return false;
            }
        };

        return dtm;
    }

    private DefaultTableModel setMorphoBasinParams() throws Exception {

        final String[] col = new String[] { LABEL_SIMOUT_PARAM,
                VALUE_SIMOUT_PARAM };
        final String[] labels = getMorphoBasinParamsLabel();
        final String[] values = getMorphoBasinParamsValues();

        final String[][] data = new String[labels.length][col.length];
        for (int r = 0; r < data.length; r++) {
            data[r][0] = labels[r];
            data[r][1] = values[r];
        }

        final DefaultTableModel dtm = new DefaultTableModel(data, col) {
            /**
             * 
             */
            private static final long serialVersionUID = 1L;

            @Override
            public boolean isCellEditable(int row, int column) {
                // all cells false
                return false;
            }
        };
        return dtm;
    }

    private DefaultTableModel setSimulatedFlowParams() {

        final String[] labels = getSimulatedFlowLabels();
        final double[][] values = getSimulatedFlowValues();

        final String[][] data = new String[labels.length][values[0].length + 1];
        for (int r = 0; r < data.length; r++) {
            data[r][0] = labels[r];
            for (int c = 1; c < data[0].length; c++) {
                data[r][c] = Double.toString(values[r][c - 1]);
            }
        }

        final String[] col = new String[] { LABEL_SIMOUT_PARAM, DIRECT_LABEL,
                BASE_LABEL, TOTAL_LABEL };

        final DefaultTableModel dtm = new DefaultTableModel(data, col) {
            /**
             * 
             */
            private static final long serialVersionUID = 1L;

            @Override
            public boolean isCellEditable(int row, int column) {
                // all cells false
                return false;
            }
        };
        return dtm;
    }

    private String[] getSmulationParamLabels() {

        final List<String> simuParams = new ArrayList<String>();
        simuParams.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputBaseflow.label")); // baseflow
        simuParams.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputBaseflowRecession.label")); // costante
                                                                        // esaurimento
                                                                        // defl
                                                                        // base
        simuParams.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputAMC.label")); // AMC
        simuParams.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputCNApparente.label")); // cn
                                                                  // apparente
        if (klemProps.getInitAbsUnit() == InitialAbstraction.AbstractionUnits.FRACTION) {
            simuParams
                    .add(PluginUtils
                            .getResources()
                            .getString(
                                    "HydrographKlemPlugin.OutputInitialAbstraction.labelFraction")); // initial
                                                                                                     // abstraction
        } else if (klemProps.getInitAbsUnit() == InitialAbstraction.AbstractionUnits.MILLIMETERS) {
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputInitialAbstraction.labelMm")); // initial
                                                                               // abstraction
        }
        simuParams.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputBasinArea.label")); // basin area
        simuParams.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputAreaContribuente.label")); // area
                                                                       // contribuente

        if (klemProps.getKinematicsType() == KlemProperties.KinematicsType.ADVANCED) {
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputChannelVelocity.label")); // channel
                                                                          // velocity
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputMinSlopeVelocity.label")); // min
                                                                           // slope
                                                                           // velocity
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputMaxSlopeVelocity.label")); // max
                                                                           // slope
                                                                           // velocity
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputCostSlopeVelocity.label")); // cost
                                                                            // slope
                                                                            // velocity
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputCostThresholdMin.label")); // min
                                                                           // threshold
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputCostThresholdMax.label")); // soglia
                                                                           // massima
                                                                           // versante
                                                                           // canale
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputCostThreshold.label")); // const.
                                                                        // k
                                                                        // threshold
        } else if (klemProps.getKinematicsType() == KlemProperties.KinematicsType.SIMPLE) {
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputChannelVelocity.label")); // channel
                                                                          // velocity
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.SlopeVelocity.label")); // slope
                                                                  // velocity
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.Threshold.label")); // threshold
        }

        if (klemProps.getRainfallType() == RainfallType.POINT
                || klemProps.getRainfallType() == RainfallType.DISTRIBUTED) {
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputCriticRainfallDuration.label")); // durata
                                                                                 // precipitazione
                                                                                 // critica
            simuParams.add("a"); // a
            simuParams.add("n"); // n
            simuParams.add("n < 1h"); //
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputARF.label")); // ARF
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputHyetoShape.label")); // hyeto
                                                                     // shape
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputRiduzionePioggia.label")); // riduzione
                                                                           // pioggia
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputInitialLsppModel.label")); // LSPP
                                                                           // Model
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputgeomorphFactorlabel")); // geomorph
                                                                        // factor
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputThresholdGeomorphFactorlabel")); // threshold
                                                                                 // geomorph
                                                                                 // factor
            simuParams.add(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.OutputAvanzamentoHyeto.label")); // avanzamento
                                                                           // ietogramma
        }

        return simuParams.toArray(new String[simuParams.size()]);
    }

    private String[] getSmulationParamValues() throws Exception {

        final List<String> simuParams = new ArrayList<String>();
        simuParams.add(PluginUtils.getThreeDecimalFormatToString(simOut
                .getSimulationDischarge().getBflo_out()[1])); // baseflow
        simuParams.add(PluginUtils.getSixDecimalFormatToString(klemProps
                .getBaseflowParams().getRecession())); // costante esaurimento
                                                       // defl base
        simuParams.add(PluginUtils.getOneDecimalFormatToString(klemProps
                .getAmcValue())); // AMC
        simuParams.add(Long.toString(Math.round(simOut.getSimulationDischarge()
                .getAppearingCN()))); // cn apparente
        simuParams.add(PluginUtils.getOneDecimalFormatToString(klemProps
                .getInitialAbstractionValue())); // initial abstraction
        simuParams.add(PluginUtils.getThreeDecimalFormatToString(klemProps
                .getCatchmentArea())); // basin area
        simuParams.add(PluginUtils.getTwoDecimalFormatToString(simOut
                .getSimulationDischarge().getContributingAreaPercentage())); // area
                                                                             // contribuente
        if (klemProps.getKinematicsType() == KlemProperties.KinematicsType.ADVANCED) {
            simuParams.add(PluginUtils.getThreeDecimalFormatToString(klemProps
                    .getChannelVelocity())); // channel velocity
            simuParams.add(PluginUtils.getThreeDecimalFormatToString(klemProps
                    .getMinSlopeVelocity())); // min slope velocity
            simuParams.add(PluginUtils.getThreeDecimalFormatToString(klemProps
                    .getMaxSlopeVelocity())); // max slope velocity
            simuParams.add(PluginUtils.getTwoDecimalFormatToString(klemProps
                    .getSlopeConstant())); // cost slope velocity
            simuParams.add(PluginUtils.getThreeDecimalFormatToString(klemProps
                    .getMinThreshold())); // min threshold
            simuParams.add(PluginUtils.getThreeDecimalFormatToString(klemProps
                    .getMaxThreshold())); // soglia massima versante canale
            simuParams.add(PluginUtils.getTwoDecimalFormatToString(klemProps
                    .getThresholdConstant())); // const. k threshold
        } else if (klemProps.getKinematicsType() == KlemProperties.KinematicsType.SIMPLE) {
            simuParams.add(PluginUtils.getThreeDecimalFormatToString(klemProps
                    .getChannelVelocity())); // channel velocity
            simuParams.add(PluginUtils.getThreeDecimalFormatToString(klemProps
                    .getSlopeVelocity())); // slope velocity
            simuParams.add(PluginUtils.getThreeDecimalFormatToString(klemProps
                    .getThresholdValue())); // min threshold
        }

        if (klemProps.getRainfallType() == RainfallType.POINT
                || klemProps.getRainfallType() == RainfallType.DISTRIBUTED) {

            simuParams.add(PluginUtils.getTwoDecimalFormatToString(klem
                    .getCriticalDuration().getInterval(TimeIntervalUnit.HOUR))); // durata
                                                                                 // precipitazione
                                                                                 // critica
            simuParams.add(PluginUtils.getTwoDecimalFormatToString(klemProps
                    .getLsppCalculator().getParamsAN().getParamA())); // a value
            simuParams.add(PluginUtils.getTwoDecimalFormatToString(klemProps
                    .getLsppCalculator().getParamsAN().getParamN())); // n
            simuParams.add(PluginUtils.getTwoDecimalFormatToString(klemProps
                    .getLsppCalculator().getParamsAN().getParamNLess1Hour())); // n<1h
            simuParams.add(PluginUtils.getTwoDecimalFormatToString(DesignRain
                    .calcFattAtten(klemProps.getCatchmentArea(),
                            klem.getCriticalDuration()))); // ARF
            simuParams.add(klemProps.getHyetoShape().toString()); // hyeto shape
            simuParams.add(PluginUtils.getTwoDecimalFormatToString(klemProps
                    .getPeakFraction())); // riduzione pioggia
            if (klemProps.getLsppModel() != null) {
                simuParams.add(klemProps.getLsppModel().toString()); // LSPP
                                                                     // Model
            } else {
                simuParams.add(""); // LSPP Model
            }
            simuParams.add(PluginUtils.getTwoDecimalFormatToString(klemProps
                    .getGeomorphFactor())); // geomorph factor
            simuParams.add(PluginUtils.getTwoDecimalFormatToString(klemProps
                    .getGeomorphoFactorThreshold())); // threshold geomorph
                                                      // factor
            simuParams.add(PluginUtils.getOneDecimalFormatToString(klemProps
                    .getHyetoPeakPosition())); // avanzamento ietogramma
        }

        return simuParams.toArray(new String[simuParams.size()]);
    }

    private String[] getMorphoBasinParamsLabel() {

        final List<String> morphoBasinList = new ArrayList<String>();
        morphoBasinList.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputOutletNord.label"));
        morphoBasinList.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputOutletEst.label"));
        morphoBasinList.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputMinElevation.label"));
        morphoBasinList.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputMaxElevation.label"));
        morphoBasinList.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputAverageElevation.label"));
        morphoBasinList.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputBasinAverageSlope.label"));
        morphoBasinList.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputBasinArea.label"));

        return morphoBasinList.toArray(new String[morphoBasinList.size()]);

    }

    private String[] getMorphoBasinParamsValues()
            throws TiffTags.TiffReadingException, Exception {

        final List<String> morphoBasinList = new ArrayList<String>();
        morphoBasinList.add(PluginUtils.getThreeDecimalFormatToString(klemProps
                .getOutletCoords().y));
        morphoBasinList.add(PluginUtils.getThreeDecimalFormatToString(klemProps
                .getOutletCoords().x));
        morphoBasinList.add(PluginUtils
                .getThreeDecimalFormatToString(getRasterMetadata(
                        klemProps.getDemMaskFile()).getStats().getMin(0)));
        morphoBasinList.add(PluginUtils
                .getFourDecimalFormatToString(getRasterMetadata(
                        klemProps.getDemMaskFile()).getStats().getMax(0)));
        morphoBasinList.add(PluginUtils
                .getFourDecimalFormatToString(getRasterMetadata(
                        klemProps.getDemMaskFile()).getStats().getMean(0)));
        // morphoBasinList.add(PluginUtils.getFourDecimalFormatToString(0.0));
        // morphoBasinList.add(PluginUtils.getFourDecimalFormatToString(0.0));
        // morphoBasinList.add(PluginUtils.getFourDecimalFormatToString(0.0));
        // morphoBasinList.add(PluginUtils.getFourDecimalFormatToString(0.0));
        morphoBasinList.add(PluginUtils
                .getFourDecimalFormatToString(getRasterMetadata(
                        new File(klemProps.getOutputFolder().getAbsoluteFile()
                                + File.separator + "slopeMask.tif")).getStats()
                        .getMean(0)));
        morphoBasinList.add(PluginUtils.getThreeDecimalFormatToString(klemProps
                .getCatchmentArea()));

        return morphoBasinList.toArray(new String[morphoBasinList.size()]);
    }

    private String[] getSimulatedFlowLabels() {

        final List<String> morphoBasinList = new ArrayList<String>();
        morphoBasinList.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputRainfall.message"));
        morphoBasinList.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputStartDischarge.message"));
        morphoBasinList.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputMaxDischarge.message"));
        morphoBasinList.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputAverageDischarge.message"));
        morphoBasinList.add(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.OutputPeakTime.message"));

        return morphoBasinList.toArray(new String[morphoBasinList.size()]);
    }

    private double[][] getSimulatedFlowValues() {

        final double[][] simValues = new double[5][3];
        simValues[0][0] = PluginUtils.getOneDecimalFormat(simOut
                .getSimulationRainfall().getCumulativeEffectiveRain()); // direct
                                                                        // rainfall
        simValues[0][1] = PluginUtils.getOneDecimalFormat(simOut
                .getSimulationRainfall().getCumulativeStoreRain()); // base
                                                                    // rainfall
        simValues[0][2] = PluginUtils.getOneDecimalFormat(simOut
                .getSimulationRainfall().getCumulativeTotalRain()); // total
                                                                    // rainfall

        simValues[1][0] = PluginUtils.getThreeDecimalFormat(simOut
                .getSimulationDischarge().getDflo_out()[0]); // portata iniziale
                                                             // diretta
        simValues[1][1] = PluginUtils.getThreeDecimalFormat(simOut
                .getSimulationDischarge().getBflo_out()[0]); // portata iniziale
                                                             // di base
        simValues[1][2] = PluginUtils.getThreeDecimalFormat(simOut
                .getSimulationDischarge().getTflo_out()[0]); // portata iniziale
                                                             // totale

        simValues[2][0] = PluginUtils.getThreeDecimalFormat(simOut
                .getSimulationDischarge().getDflo_max()); // portata massima
                                                          // diretta
        simValues[2][1] = PluginUtils.getThreeDecimalFormat(simOut
                .getSimulationDischarge().getBflo_max()); // portata massima di
                                                          // base
        simValues[2][2] = PluginUtils.getThreeDecimalFormat(simOut
                .getSimulationDischarge().getTflo_max()); // portata massima
                                                          // totale

        simValues[3][0] = PluginUtils.getThreeDecimalFormat(simOut
                .getSimulationDischarge().getDflo_med()); // portata media
                                                          // diretta
        simValues[3][1] = PluginUtils.getThreeDecimalFormat(simOut
                .getSimulationDischarge().getBflo_med()); // portata media di
                                                          // base
        simValues[3][2] = PluginUtils.getThreeDecimalFormat(simOut
                .getSimulationDischarge().getTflo_med()); // portata media
                                                          // totale

        simValues[4][0] = PluginUtils.getFourDecimalFormat(simOut
                .getSimulationDischarge().getDirectFlowPeakTime()
                .getInterval(TimeIntervalUnit.HOUR)); // tempo picco diretta
        simValues[4][1] = PluginUtils.getFourDecimalFormat(simOut
                .getSimulationDischarge().getBaseFlowPeakTime()
                .getInterval(TimeIntervalUnit.HOUR)); // tempo picco di base
        simValues[4][2] = PluginUtils.getFourDecimalFormat(simOut
                .getSimulationDischarge().getTotalFlowPeakTime()
                .getInterval(TimeIntervalUnit.HOUR)); // tempo picco totale

        return simValues;
    }

    public void setKlemProperties(KlemProperties klemPropr) {
        klemProps = klemPropr;
    }

    private void setLabelFormat(JLabel... labels) {
        for (final JLabel label : labels) {
            label.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
            label.setForeground(new java.awt.Color(102, 102, 102));
        }
    }

    private Metadata getRasterMetadata(File rasterFile)
            throws NoninvertibleTransformException,
            TiffTags.TiffReadingException, Exception {
        final RasterImageIO rasterImageIO = new RasterImageIO();
        final ImageAndMetadata metadata = rasterImageIO.loadImage(
                context.getWorkbenchContext(), rasterFile.getAbsolutePath(),
                null, null, null);

        return metadata.getMetadata();
    }

    private MainPanel mainPanel;
    private final Klem klem;
    private KlemProperties klemProps;
    private final PlugInContext context;
    private final SimulationOutput simOut;
    private final String TIME = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.TableTime.label");
    private final String TOT_FLOW = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.TotDischarge.label");
    private final String DIR_FLOW = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.DirDischarge.label");
    private final String BASE_FLOW = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.BaseFlow.label");
    private final String TOT_RAINFALL = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.ToTRainfall.label");
    private final String EFF_RAINFALL = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.EffRainfall.label");
    private final String FILE_FILTER_DESCRIPTION = PluginUtils.getResources()
            .getString("HydrologicalGroups.TableFileDescription.label");
    private final String LABEL_SIMOUT_PARAM = PluginUtils.getResources()
            .getString("HydrographKlemPlugin.OutputSimLabelTable.label");
    private final String VALUE_SIMOUT_PARAM = PluginUtils.getResources()
            .getString("HydrographKlemPlugin.OutputSimValueTable.label");
    private final String DIRECT_LABEL = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.OutputDirectLabel.message");
    private final String BASE_LABEL = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.OutputBaseLabel.message");
    private final String TOTAL_LABEL = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.OutputTotalLabel.message");
    private final LayerablesList layerablesList;

    private final String odsOutputFileName = "sim.ods";
    private final String pngOutputFileName = "graph.png";
    private final String basinDataFileName = "basin_data.csv";
    private final String tableFileName = "rainfall.csv";
    private final String simParFileName = "sim_par.csv";
    private final String simFlowParFileName = "sim_flow_par.csv";

    private PersonalChartKlem personalChartKlem;

}
