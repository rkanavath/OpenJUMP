package com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem;

import it.geomaticaeambiente.klem.Klem;
import it.geomaticaeambiente.klem.LsppCalculator;
import it.geomaticaeambiente.klem.SimulationOutput;

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.xml.parsers.ParserConfigurationException;

import com.geomaticaeambiente.klemgui.exceptions.WarningException;
import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem.KlemUtils;
import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem.OutputTab;
import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem.KlemProperties.RainfallType;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.CombinationComponents;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.ThreadedBasePlugIn;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import com.vividsolutions.jump.workbench.ui.task.TaskMonitorManager;

/**
 *
 * @author Geomatica
 */
public class ParamsTab extends AbstractInputKlemPlugin {

    public ParamsTab(PlugInContext context, InitialDialog initialDialog,
            LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

        return setParamsInitialData();
    }

    @Override
    public ComponentsTreeMap setComponentsActions(
            ComponentsTreeMap personalTreeMap) {

        return setParamsTabComponentsTreeMap(personalTreeMap);
    }

    public void calculateParameters(
            final ComponentsTreeMap componentsWithActions)
            throws ParserConfigurationException, IOException, WarningException,
            Exception {
        KlemUtils.checkParams(componentsWithActions, klemProps,
                rainfallParamType);
        final Klem klem = KlemUtils.buildKlem(klemProps);

        //  super.getInitialDialog().setCursor(new Cursor(Cursor.WAIT_CURSOR));
        klem.run();

        final SimulationOutput simOut = klem.getSimulationOutput();

        final JTabbedPane mainTabelPane = super.getInitialDialog()
                .getTabbedPane();
        final OutputTab outputPanel = new OutputTab(context,
                super.getInitialDialog(), layerablesList, simOut, klem);
        outputPanel.setKlemProperties(klemProps);
        mainTabelPane.setComponentAt(3, outputPanel.getTabPluginComponents());

        mainTabelPane.setEnabledAt(3, true);
        // mainTabelPane.setEnabledAt(2, true);
        mainTabelPane.setSelectedIndex(3);
    }
    
    
    
    @Override
    public JPanel buildPluginPanel(final ComponentsTreeMap componentsWithActions) {

        this.componentsWithActions = componentsWithActions;

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
            public void rightButton() { // OUTPUT
                try {

                    AbstractPlugIn.toActionListener(new ThreadedBasePlugIn() {
                        @Override
                        public String getName() {
                            return null;
                        }

                        @Override
                        public boolean execute(PlugInContext context)
                                throws Exception {
                            return true;
                        }

                        @Override
                        public void run(TaskMonitor monitor,
                                PlugInContext context) throws Exception {
                            monitor.report(PluginUtils.getResources()
                                    .getString("OpenKlem.executing-process"));
                            reportNothingToUndoYet(context);
                            try {
                                calculateParameters(componentsWithActions);
                            } catch (final Exception ex) {
                                JOptionPane.showMessageDialog(
                                        getInitialDialog().getTabbedPane(),
                                        PluginUtils.getResources().getString(
                                                "Process interrupted"),
                                        PluginUtils.plugInName,
                                        JOptionPane.INFORMATION_MESSAGE);

                            }
                        }
                    }, context.getWorkbenchContext(), new TaskMonitorManager())
                            .actionPerformed(null);
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
            public void centerButton() { // ADVANCED PARAM
                //
                // try {
                //
                // setAndCheckParams(componentsWithActions);
                //
                // JTabbedPane mainTabelPane =
                // super.getInitialDialog().getTabbedPane();
                //
                // AdvancedParamsOutput advancedParams = new
                // AdvancedParamsOutput(context, super.getInitialDialog(),
                // layerablesList);
                // advancedParams.setKlemProperties(klemProps);
                // advancedParams.setLoadedPrj(loadedPrj);
                // mainTabelPane.setComponentAt(2,
                // advancedParams.getTabPluginComponents());
                // mainTabelPane.setEnabledAt(2, true);
                // mainTabelPane.setSelectedIndex(2);
                //
                //
                // } catch (Exception ex) {
                // JOptionPane.showMessageDialog(super.getInitialDialog(),
                // "Error:"
                // + ex, PluginUtils.plugInName, JOptionPane.ERROR_MESSAGE);
                // }
                //
            }
        };

        mainPanel.setRightButtonText(PluginUtils.getResources().getString(
                "MainPanel.ExecuteButton.text"));
        // mainPanel.setCenterButtonText(ADVANCED_PARAMS);

        return mainPanel;
    }

    private InitialData setParamsInitialData() {

        final String flowDir = klemProps.getFlowDir().getAbsolutePath();
        final String upslope = klemProps.getUpslopeAreaFile().getAbsolutePath();

        final InitialData initialData = new InitialData();

        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(FLOW_DIR),
                flowDir, GUIUtils.INPUT);
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(UPSLOPE),
                upslope, GUIUtils.INPUT);

        initialData.setParam_Labels(new String[] { PluginUtils.getResources()
                .getString("KlemGUI.Outlet.label") }, GUIUtils.OTHER); // 00}

        // X coordinate by mouse
        initialData.setParam_Action(new ActionObject(PluginUtils.getResources()
                .getString("WatershedPlugin.ChooseButton.label")),
                GUIUtils.OTHER); // 01

        if (klemProps.getOutletCoords() != null) {
            initialData.setParam_Label_TextBox_Label(
                    GUIUtils.setGUILabel(XCOORD),
                    klemProps.getOutletCoords().x, "", GUIUtils.OTHER);// 02
            initialData.setParam_Label_TextBox_Label(
                    GUIUtils.setGUILabel(YCOORD),
                    klemProps.getOutletCoords().y, "", GUIUtils.OTHER);// 03
                                                                       // coordinata
                                                                       // y da
                                                                       // mouse
        } else {
            initialData.setParam_Label_TextBox_Label(
                    GUIUtils.setGUILabel(XCOORD), "", "", GUIUtils.OTHER); // 02
            initialData.setParam_Label_TextBox_Label(
                    GUIUtils.setGUILabel(YCOORD), "", "", GUIUtils.OTHER);// 03
                                                                          // coordinata
                                                                          // y
                                                                          // da
                                                                          // mouse
        }

        // area
        if (klemProps.getCatchmentArea() != null) {
            initialData.setParam_Label_TextBox_Label(
                    GUIUtils.setGUILabel(BASIN_AREA),
                    klemProps.getCatchmentArea(), "", GUIUtils.OTHER); // 04
        } else {
            initialData.setParam_Label_TextBox_Label(
                    GUIUtils.setGUILabel(BASIN_AREA), "", "", GUIUtils.OTHER); // 04
        }

        // elevation
        if (klemProps.getOutletElevation() != null) {

            initialData.setParam_Label_TextBox_Label(
                    GUIUtils.setGUILabel(ELEVATION),
                    klemProps.getOutletElevation(), "", GUIUtils.OTHER); // 05
        } else {
            initialData.setParam_Label_TextBox_Label(
                    GUIUtils.setGUILabel(ELEVATION), "", "", GUIUtils.OTHER); // 05
        }

        // Separator
        initialData.setParam_Line(
                CombinationComponents.CombinationElement.LINE, GUIUtils.OTHER); // 06

        // LSPP
        initialData.setParam_Labels(new String[] { RAINFALL, "" },
                GUIUtils.OTHER); // 07

        // radio buttons
        initialData.setParam_Action(new ActionObject(new String[] {
                POINT_RAINFALL, DISTRIBUTED_RAINFALL, HISTORICAL_RAINFALL }),
                GUIUtils.OTHER); // 08

        // LSPP
        // if data exist
        if (klemProps.getRainfallType() != null) {
            if (klemProps.getRainfallType() == RainfallType.POINT) {
                initialData.setParam_Label_TextBox_Label("a:",
                        klemProps.getAParam(), "", GUIUtils.OTHER); // 09
                initialData.setParam_Label_TextBox_Label("n:",
                        klemProps.getNParam(), "", GUIUtils.OTHER);// 10

                initialData.setParam_Label_TextBox_Label(
                        GUIUtils.setGUILabel(MODEL), new String[] { "Gumbel",
                                "GEV" }, "", GUIUtils.OTHER); // 11
                initialData.setParam_Label_TextBox_Button(
                        GUIUtils.setGUILabel(FILE_A), "", new ActionObject(""),
                        GUIUtils.OTHER); // 12
                initialData.setParam_Label_TextBox_Button(
                        GUIUtils.setGUILabel(FILE_N), "", new ActionObject(""),
                        GUIUtils.OTHER); // 13
                initialData.setParam_Label_TextBox_Button(
                        GUIUtils.setGUILabel(FILE_CV), "",
                        new ActionObject(""), GUIUtils.OTHER); // 14
                initialData.setParam_Label_TextBox_Label(
                        GUIUtils.setGUILabel(RETURN_TIME), "", "",
                        GUIUtils.OTHER); // 15

                initialData.setParam_Label_TextBox_Button(
                        GUIUtils.setGUILabel(HISTORICAL_RAINFALL), "",
                        new ActionObject(""), GUIUtils.OTHER); // 16

            } else if (klemProps.getRainfallType() == RainfallType.DISTRIBUTED) {
                initialData.setParam_Label_TextBox_Label("a:", "", "",
                        GUIUtils.OTHER); // 09
                initialData.setParam_Label_TextBox_Label("n:", "", "",
                        GUIUtils.OTHER); // 10

                initialData.setParam_Label_TextBox_Label(
                        GUIUtils.setGUILabel(MODEL), new String[] { "Gumbel",
                                "GEV" }, "", GUIUtils.OTHER); // 11
                if (klemProps.getAFile() != null) {
                    initialData.setParam_Label_TextBox_Button(
                            GUIUtils.setGUILabel(FILE_A), klemProps.getAFile(),
                            new ActionObject(""), GUIUtils.OTHER); // 12
                } else {
                    initialData.setParam_Label_TextBox_Button(GUIUtils
                            .setGUILabel(FILE_A), "", new ActionObject(""),
                            GUIUtils.OTHER);// 12
                }

                if (klemProps.getNFile() != null) {
                    initialData.setParam_Label_TextBox_Button(
                            GUIUtils.setGUILabel(FILE_N), klemProps.getNFile(),
                            new ActionObject(""), GUIUtils.OTHER); // 13
                } else {
                    initialData.setParam_Label_TextBox_Button(GUIUtils
                            .setGUILabel(FILE_N), "", new ActionObject(""),
                            GUIUtils.OTHER); // 13
                }

                if (klemProps.getCvFile() != null) {
                    initialData.setParam_Label_TextBox_Button(
                            GUIUtils.setGUILabel(FILE_CV),
                            klemProps.getCvFile(), new ActionObject(""),
                            GUIUtils.OTHER); // 14
                } else {
                    initialData.setParam_Label_TextBox_Button(
                            GUIUtils.setGUILabel(FILE_CV), "",
                            new ActionObject(""), GUIUtils.OTHER); // 14
                }

                if (klemProps.getReturnPeriod() != null) {
                    initialData.setParam_Label_TextBox_Label(
                            GUIUtils.setGUILabel(RETURN_TIME),
                            klemProps.getReturnPeriod(), "", GUIUtils.OTHER); // 15
                } else {
                    initialData.setParam_Label_TextBox_Label(
                            GUIUtils.setGUILabel(RETURN_TIME), "", "",
                            GUIUtils.OTHER); // 15
                }

                initialData.setParam_Label_TextBox_Button(
                        GUIUtils.setGUILabel(HISTORICAL_RAINFALL), "",
                        new ActionObject(""), GUIUtils.OTHER); // 16

            } else if (klemProps.getRainfallType() == RainfallType.HISTORICAL) {

                initialData.setParam_Label_TextBox_Label("a:", "", "",
                        GUIUtils.OTHER); // 09
                initialData.setParam_Label_TextBox_Label("n:", "", "",
                        GUIUtils.OTHER); // 10

                initialData.setParam_Label_TextBox_Label(
                        GUIUtils.setGUILabel(MODEL), new String[] { "Gumbel",
                                "GEV" }, "", GUIUtils.OTHER); // 11
                initialData.setParam_Label_TextBox_Button(
                        GUIUtils.setGUILabel(FILE_A), "", new ActionObject(""),
                        GUIUtils.OTHER); // 12
                initialData.setParam_Label_TextBox_Button(
                        GUIUtils.setGUILabel(FILE_N), "", new ActionObject(""),
                        GUIUtils.OTHER); // 13
                initialData.setParam_Label_TextBox_Button(
                        GUIUtils.setGUILabel(FILE_CV), "",
                        new ActionObject(""), GUIUtils.OTHER); // 14
                initialData.setParam_Label_TextBox_Label(
                        GUIUtils.setGUILabel(RETURN_TIME), "", "",
                        GUIUtils.OTHER); // 15

                if (klemProps.getHistoricalRainfallFile() != null) {
                    initialData.setParam_Label_TextBox_Button(
                            GUIUtils.setGUILabel(HISTORICAL_RAINFALL),
                            klemProps.getHistoricalRainfallFile(),
                            new ActionObject(""), GUIUtils.OTHER); // 16
                } else {
                    initialData.setParam_Label_TextBox_Button(
                            GUIUtils.setGUILabel(HISTORICAL_RAINFALL), "",
                            new ActionObject(""), GUIUtils.OTHER); // 16
                }
            }
        } else {
            // LSPP point
            initialData.setParam_Label_TextBox_Label("a:", "", "",
                    GUIUtils.OTHER); // 09
            initialData.setParam_Label_TextBox_Label("n:", "", "",
                    GUIUtils.OTHER); // 10

            // LSPP distributed
            initialData.setParam_Label_TextBox_Label(
                    GUIUtils.setGUILabel(MODEL),
                    new String[] { "Gumbel", "GEV" }, "", GUIUtils.OTHER); // 11
            initialData.setParam_Label_TextBox_Button(
                    GUIUtils.setGUILabel(FILE_A), "", new ActionObject(""),
                    GUIUtils.OTHER); // 12
            initialData.setParam_Label_TextBox_Button(
                    GUIUtils.setGUILabel(FILE_N), "", new ActionObject(""),
                    GUIUtils.OTHER); // 13
            initialData.setParam_Label_TextBox_Button(
                    GUIUtils.setGUILabel(FILE_CV), "", new ActionObject(""),
                    GUIUtils.OTHER); // 14
            initialData.setParam_Label_TextBox_Label(
                    GUIUtils.setGUILabel(RETURN_TIME), "", "", GUIUtils.OTHER);// 15

            // Hystory precipitation
            initialData.setParam_Label_TextBox_Button(
                    GUIUtils.setGUILabel(HISTORICAL_RAINFALL), "",
                    new ActionObject(""), GUIUtils.OTHER); // 16
        }

        // if(klemProp.getRainfallType() != null && (klemProp.getRainfallType()
        // == RainfallType.POINT || klemProp.getRainfallType() ==
        // RainfallType.DISTRIBUTED)){
        // initialData.setParam_Label_TextBox_Button(GUIUtils.setGUILabel(NLESSHOUR),
        // klemProp.getnLessHourParam(), new ActionObject(true, "Auto"),
        // GUIUtils.OTHER);//17
        // } else
        initialData.setParam_Label_TextBox_Button(GUIUtils
                .setGUILabel(NLESSHOUR), "Auto",
                new ActionObject(true, "Auto"), GUIUtils.OTHER);// 17

        return initialData;
    }

    private ComponentsTreeMap setParamsTabComponentsTreeMap(
            final ComponentsTreeMap personalTreeMap) {

        // disable flowdir and upslope textfield
        final JTextField jTextField_FlowDir = (JTextField) personalTreeMap
                .getComponent("00", GUIUtils.INPUT, 1);
        jTextField_FlowDir.setEnabled(false);
        final JTextField jTextField_Upslope = (JTextField) personalTreeMap
                .getComponent("01", GUIUtils.INPUT, 1);
        jTextField_Upslope.setEnabled(false);

        final JLabel jlabel_ExclX = (JLabel) personalTreeMap.getComponent("02",
                GUIUtils.OTHER, 2);
        final JLabel jlabel_ExclY = (JLabel) personalTreeMap.getComponent("03",
                GUIUtils.OTHER, 2);
        final JLabel jlabel_ExclArea = (JLabel) personalTreeMap.getComponent(
                "04", GUIUtils.OTHER, 2);
        final JLabel jlabel_ExclElevation = (JLabel) personalTreeMap
                .getComponent("05", GUIUtils.OTHER, 2);

        GUIUtils.setExclamationMark(jlabel_ExclX, jlabel_ExclY,
                jlabel_ExclArea, jlabel_ExclElevation);

        // x coord components
        final JTextField jTextField_xCoord = (JTextField) personalTreeMap
                .getComponent("02", GUIUtils.OTHER, 1); // jtext field coord x
        jTextField_xCoord.setEditable(false);
        if (klemProps.getOutletCoords() != null) {
            GUIUtils.setJTextAction(loadedPrj, jTextField_xCoord,
                    klemProps.getOutletCoords().x, jlabel_ExclX);
        } else {
            GUIUtils.setJTextAction(loadedPrj, jTextField_xCoord, null,
                    jlabel_ExclX);
        }

        final JButton jButton_Choose = (JButton) personalTreeMap.getComponent(
                "01", GUIUtils.OTHER, 0); // button choose

        // y coord components
        final JTextField jTextField_yCoord = (JTextField) personalTreeMap
                .getComponent("03", GUIUtils.OTHER, 1); // jtext field coord y
        jTextField_yCoord.setEditable(false);
        if (klemProps.getOutletCoords() != null) {
            GUIUtils.setJTextAction(loadedPrj, jTextField_yCoord,
                    klemProps.getOutletCoords().y, jlabel_ExclY);
        } else {
            GUIUtils.setJTextAction(loadedPrj, jTextField_yCoord, null,
                    jlabel_ExclY);
        }

        // area component
        final JTextField jTextField_area = (JTextField) personalTreeMap
                .getComponent("04", GUIUtils.OTHER, 1);
        jTextField_area.setEditable(false);
        jTextField_area.setToolTipText(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.CheckArea.message1"));
        GUIUtils.setJTextAction(loadedPrj, jTextField_area,
                klemProps.getCatchmentArea(), jlabel_ExclArea);

        // elevation component
        final JTextField jTextField_elevation = (JTextField) personalTreeMap
                .getComponent("05", GUIUtils.OTHER, 1);
        jTextField_elevation.setEditable(false);
        GUIUtils.setJTextAction(loadedPrj, jTextField_elevation,
                klemProps.getOutletElevation(), jlabel_ExclElevation);

        // Choose action
        jButton_Choose.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    // set xy coords, area and elevation info
                    final WatershedInformation wi = new WatershedInformation(
                            context, jTextField_xCoord, jTextField_yCoord,
                            jTextField_area, jTextField_elevation);
                    wi.getCoordinate();
                    wi.setRasterForArea(klemProps.getUpslopeAreaFile());
                    wi.setRasterForElevation(klemProps.getDemFile());

                } catch (final Exception ex) {
                    ErrorDialog.show(context.getWorkbenchFrame(),
                            PluginUtils.plugInName, ex.toString(),
                            StringUtil.stackTrace(ex));
                }
            }
        });

        // set exclamation mark next to rainfall
        final JLabel rainfallLabel = (JLabel) personalTreeMap.getComponent(
                "07", GUIUtils.OTHER, 1);
        GUIUtils.setExclamationMark(rainfallLabel);

        // set lspp distr. buttons
        // a file
        final JTextField jTextField_aFile = (JTextField) personalTreeMap
                .getComponent("12", GUIUtils.OTHER, 1);
        final JButton jButton_aFile = (JButton) personalTreeMap.getComponent(
                "12", GUIUtils.OTHER, 2);
        jButton_aFile.setIcon(PluginUtils.getFolderIcon());
        jButton_aFile.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {

                final FileNameExtensionFilter fileFilter = new FileNameExtensionFilter(
                        "a Param Grid File *.asc, *.flt, *.tif", new String[] {
                                "asc", "flt", "tif" });
                final File projectPath = PluginUtils.openJChooserDialog(
                        context.getActiveInternalFrame(),
                        JFileChooser.FILES_ONLY, JFileChooser.OPEN_DIALOG,
                        fileFilter, null, false)[0];
                if (projectPath != null) {
                    jTextField_aFile.setText(projectPath.getAbsolutePath());
                }
            }
        });

        // n file
        final JTextField jTextField_nFile = (JTextField) personalTreeMap
                .getComponent("13", GUIUtils.OTHER, 1);
        final JButton jButton_nFile = (JButton) personalTreeMap.getComponent(
                "13", GUIUtils.OTHER, 2);
        jButton_nFile.setIcon(PluginUtils.getFolderIcon());
        jButton_nFile.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {

                final FileNameExtensionFilter fileFilter = new FileNameExtensionFilter(
                        "n Param Grid File *.asc, *.flt, *.tif", new String[] {
                                "asc", "flt", "tif" });
                final File projectPath = PluginUtils.openJChooserDialog(
                        context.getActiveInternalFrame(),
                        JFileChooser.FILES_ONLY, JFileChooser.OPEN_DIALOG,
                        fileFilter, null, false)[0];
                if (projectPath != null) {
                    jTextField_nFile.setText(projectPath.getAbsolutePath());
                }
            }
        });
        // cv file
        final JTextField jTextField_cvFile = (JTextField) personalTreeMap
                .getComponent("14", GUIUtils.OTHER, 1);
        final JButton jButton_cvFile = (JButton) personalTreeMap.getComponent(
                "14", GUIUtils.OTHER, 2);
        jButton_cvFile.setIcon(PluginUtils.getFolderIcon());
        jButton_cvFile.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {

                final FileNameExtensionFilter fileFilter = new FileNameExtensionFilter(
                        "cv Param Grid File *.asc, *.flt, *.tif", new String[] {
                                "asc", "flt", "tif" });
                final File projectPath = PluginUtils.openJChooserDialog(
                        context.getActiveInternalFrame(),
                        JFileChooser.FILES_ONLY, JFileChooser.OPEN_DIALOG,
                        fileFilter, null, false)[0];
                if (projectPath != null) {
                    jTextField_cvFile.setText(projectPath.getAbsolutePath());
                }
            }
        });

        // set historical button
        final JTextField jTextField_hystoric = (JTextField) personalTreeMap
                .getComponent("16", GUIUtils.OTHER, 1);
        final JButton jButton_hystoric = (JButton) personalTreeMap
                .getComponent("16", GUIUtils.OTHER, 2);
        jButton_hystoric.setIcon(PluginUtils.getFolderIcon());
        jButton_hystoric.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {

                final FileNameExtensionFilter extensionFilter = new FileNameExtensionFilter(
                        "Historical rainfall *.txt, *.csv, *.pre",
                        new String[] { "txt", "csv", "pre" });

                final File projectPath = PluginUtils.openJChooserDialog(
                        context.getActiveInternalFrame(),
                        JFileChooser.FILES_ONLY, JFileChooser.OPEN_DIALOG,
                        extensionFilter, null, false)[0];

                if (projectPath != null) {
                    jTextField_hystoric.setText(projectPath.getAbsolutePath());
                }
            }
        });

        final JComboBox distributedModel = (JComboBox) personalTreeMap
                .getComponent("11", GUIUtils.OTHER, 1);
        distributedModel.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {

                if (distributedModel.getSelectedIndex() == 1) {
                    lsppModel = LsppCalculator.LsppModel.GUMBEL;
                } else if (distributedModel.getSelectedIndex() == 2) {
                    lsppModel = LsppCalculator.LsppModel.GEV;
                } else {
                    lsppModel = null;
                }
            }
        });

        // set rainfall visible components
        final JRadioButton jRadiobutton_point = (JRadioButton) personalTreeMap
                .getComponent("08", GUIUtils.OTHER, 0);
        setVisibleComponents(true, false, false, personalTreeMap);
        jRadiobutton_point.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                rainfallParamType = RainfallType.POINT;
                setVisibleComponents(true, false, false, personalTreeMap);
                setRainfallCheckAction(rainfallLabel,
                        (JTextField) personalTreeMap.getComponent("09",
                                GUIUtils.OTHER, 1),
                        (JTextField) personalTreeMap.getComponent("10",
                                GUIUtils.OTHER, 1),
                        (JTextField) personalTreeMap.getComponent("17",
                                GUIUtils.OTHER, 1));
            }
        });

        final JRadioButton jRadiobutton_distributed = (JRadioButton) personalTreeMap
                .getComponent("08", GUIUtils.OTHER, 1);
        jRadiobutton_distributed.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                rainfallParamType = RainfallType.DISTRIBUTED;
                setVisibleComponents(false, true, false, personalTreeMap);
                setRainfallCheckAction(rainfallLabel, jTextField_aFile,
                        jTextField_nFile, jTextField_cvFile,
                        (JTextField) personalTreeMap.getComponent("15",
                                GUIUtils.OTHER, 1),
                        (JTextField) personalTreeMap.getComponent("17",
                                GUIUtils.OTHER, 1));
            }
        });

        final JRadioButton jRadiobutton_historical = (JRadioButton) personalTreeMap
                .getComponent("08", GUIUtils.OTHER, 2);
        jRadiobutton_historical.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                rainfallParamType = RainfallType.HISTORICAL;
                setVisibleComponents(false, false, true, personalTreeMap);
                setRainfallCheckAction(rainfallLabel,
                        (JTextField) personalTreeMap.getComponent("16",
                                GUIUtils.OTHER, 1));
            }
        });

        // set selected radio button if exist in the project
        if (klemProps.getRainfallType() != null) {

            if (klemProps.getRainfallType() == RainfallType.POINT) {
                rainfallParamType = RainfallType.POINT;
                jRadiobutton_point.setSelected(true);
                setVisibleComponents(true, false, false, personalTreeMap);
            } else if (klemProps.getRainfallType() == RainfallType.DISTRIBUTED) {
                rainfallParamType = RainfallType.DISTRIBUTED;
                // set radio button and set distributed combobox
                jRadiobutton_distributed.setSelected(true);
                setVisibleComponents(false, true, false, personalTreeMap);
                if (klemProps.getLsppModel() == LsppCalculator.LsppModel.GUMBEL) {
                    distributedModel.setSelectedIndex(1);
                } else {
                    distributedModel.setSelectedIndex(2);
                }
            } else if (klemProps.getRainfallType() == RainfallType.HISTORICAL) {
                rainfallParamType = RainfallType.HISTORICAL;
                jRadiobutton_historical.setSelected(true);
                setVisibleComponents(false, false, true, personalTreeMap);
            }
        } else {
            jRadiobutton_point.setSelected(true);
            rainfallParamType = RainfallType.POINT;
            setVisibleComponents(true, false, false, personalTreeMap);
        }

        // n less 1hour
        final JTextField jTextField_nLessHour = (JTextField) personalTreeMap
                .getComponent("17", GUIUtils.OTHER, 1);
        final JCheckBox jCheckBox_nLessHour = (JCheckBox) personalTreeMap
                .getComponent("17", GUIUtils.OTHER, 2);
        jCheckBox_nLessHour.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                if (jCheckBox_nLessHour.isSelected()) {
                    jTextField_nLessHour.setText("Auto");
                    jTextField_nLessHour.setEnabled(false);
                    klemProps.setnLessHourParamAuto(true);
                } else {
                    jTextField_nLessHour.setText(Double.toString(0.0));
                    jTextField_nLessHour.setEnabled(true);
                    klemProps.setnLessHourParamAuto(false);
                }
            }

        });

        if (klemProps.isNLessHourParamAuto()) {
            jTextField_nLessHour.setText("Auto");
            jTextField_nLessHour.setEnabled(false);
        } else {
            jTextField_nLessHour.setText(Double.toString(0.0));
            jTextField_nLessHour.setEnabled(true);
        }

        return personalTreeMap;
    }

    public void setAndCheckParams() throws ParserConfigurationException,
            IOException, WarningException, Exception {
        KlemUtils.checkParams(componentsWithActions, klemProps,
                rainfallParamType);
    }

    public void setKlemProperties(KlemProperties klemProp) {
        klemProps = klemProp;
    }

    public void setLoadedPrj(boolean loadedPrj) {
        this.loadedPrj = loadedPrj;
    }

    private void setRainfallCheckAction(JLabel label, JTextField... textField) {

        if (loadedPrj) {
            if (rainfallParamType != klemProps.getRainfallType()) {
                label.setVisible(true);
                return;
            }

            switch (rainfallParamType) {
            case POINT: {
                // check if value are the same as the project

                GUIUtils.setJTextAction(loadedPrj, textField[0],
                        klemProps.getAParam(), label);
                GUIUtils.setJTextAction(loadedPrj, textField[1],
                        klemProps.getNParam(), label);
                break;
            }

            case DISTRIBUTED: {
                // check if value are the same as the project
                if (lsppModel != klemProps.getLsppModel()) {
                    label.setVisible(true);
                    return;
                }

                GUIUtils.setJTextAction(loadedPrj, textField[3],
                        (double) klemProps.getReturnPeriod(), label);
                break;
            }

            }
        }

    }

    private void setVisibleComponents(boolean viewPoint,
            boolean viewDistributed, boolean viewHistorical,
            ComponentsTreeMap personalTreeMap) {

        GUIUtils.setVisibleComponents(viewPoint, GUIUtils
                .getJComponentFromRowRange(personalTreeMap, 9, 10,
                        GUIUtils.OTHER));
        GUIUtils.setVisibleComponents(viewDistributed, GUIUtils
                .getJComponentFromRowRange(personalTreeMap, 11, 15,
                        GUIUtils.OTHER));
        GUIUtils.setVisibleComponents(viewHistorical, GUIUtils
                .getJComponentFromRowRange(personalTreeMap, 16, 16,
                        GUIUtils.OTHER));
        GUIUtils.setVisibleComponents(!viewHistorical, GUIUtils
                .getJComponentFromRowRange(personalTreeMap, 17, 17,
                        GUIUtils.OTHER));
    }

    public KlemProperties.RainfallType getRainfallType() {
        return rainfallParamType;
    }

    private KlemProperties klemProps;
    private final PlugInContext context;
    private boolean loadedPrj = false;
    private MainPanel mainPanel;
    private LsppCalculator.LsppModel lsppModel;
    private KlemProperties.RainfallType rainfallParamType;

    private ComponentsTreeMap componentsWithActions;
    private final String FLOW_DIR = PluginUtils.getResources().getString(
            "KlemGUI.InputFlowDir.label");
    private final String UPSLOPE = PluginUtils.getResources().getString(
            "KlemGUI.InputUpslope.label");
    private final String BASIN_AREA = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.BasinArea.label");
    private final String ELEVATION = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.Elevation.label");
    private final String XCOORD = PluginUtils.getResources().getString(
            "KlemGUI.XCoord.label");
    private final String YCOORD = PluginUtils.getResources().getString(
            "KlemGUI.YCoord.label");
    private final String RAINFALL = PluginUtils.getResources().getString(
            "KlemGUI.Rainfall.label");
    private final String DISTRIBUTED_RAINFALL = PluginUtils.getResources()
            .getString("HydrographKlemPlugin.DistributedRainfall.label");
    private final String POINT_RAINFALL = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.PointRainfall.label");
    public static final String HISTORICAL_RAINFALL = PluginUtils.getResources()
            .getString("HydrographKlemPlugin.HistoricRainfall.label");
    private final String MODEL = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.ModelRainfall.label");
    private final String RETURN_TIME = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.returnTime.label");
    // private final String ADVANCED_PARAMS =
    // PluginUtils.getResources().getString("HydrographKlemPlugin.AdvancedParams.label");
    public static final String FILE_A = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.FileA.label");
    public static final String FILE_N = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.FileN.label");
    public static final String FILE_CV = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.FileCV.label");
    private final String NLESSHOUR = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.nLessHour.label");

    private final LayerablesList layerablesList;

}
