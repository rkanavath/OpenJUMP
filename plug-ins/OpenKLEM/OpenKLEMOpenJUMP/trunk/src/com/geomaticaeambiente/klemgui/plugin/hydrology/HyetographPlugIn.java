package com.geomaticaeambiente.klemgui.plugin.hydrology;

import it.geomaticaeambiente.klem.BaseflowParams;
import it.geomaticaeambiente.klem.DesignRain;
import it.geomaticaeambiente.klem.Geomorphology;
import it.geomaticaeambiente.klem.Hyetograph;
import it.geomaticaeambiente.klem.HyetographGenerator;
import it.geomaticaeambiente.klem.InitialAbstraction;
import it.geomaticaeambiente.klem.LsppCalculator;
import it.geomaticaeambiente.klem.RainfallRecession;
import it.geomaticaeambiente.klem.TimeInterval;
import it.geomaticaeambiente.klem.Watershed;

import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableModel;

import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.Header;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PersonalChart;
import com.geomaticaeambiente.klemgui.utils.PersonalChartHyetograph;
import com.geomaticaeambiente.klemgui.utils.PersonalRadioButtons;
import com.geomaticaeambiente.klemgui.utils.PersonalTable;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;

/**
 *
 * @author Geomatica
 */
public class HyetographPlugIn extends AbstractInputKlemPlugin {

    public HyetographPlugIn(PlugInContext context, InitialDialog initialDialog,
            LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

        final InitialData initialData = new InitialData();
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(PARAM_A), "",
                GUIUtils.INPUT); //param a
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(PARAM_N), "",
                GUIUtils.INPUT); //param n
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(TIME), "",
                GUIUtils.INPUT); // param time
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(STEP), "",
                GUIUtils.INPUT); //param step
        initialData.setParam_Labels(
                new String[] { GUIUtils.setGUILabel(MODEL) }, GUIUtils.INPUT); //label Model

        final PersonalRadioButtons radioButtons = new PersonalRadioButtons(
                PluginUtils.getResources().getString(
                        "HyetographPlugIn.AlternatingBlocks.label"),
                PluginUtils.getResources().getString(
                        "HyetographPlugIn.Costant.label"), PluginUtils
                        .getResources().getString(
                                "HyetographPlugIn.IstantaneusIntensity.label"),
                PluginUtils.getResources().getString(
                        "HyetographPlugIn.Triangular.label"), PluginUtils
                        .getResources().getString(
                                "HyetographPlugIn.Wallingford.labela"));

        initialData.setParam_RadioButtons(radioButtons, GUIUtils.INPUT);

        initialData.setParam_Labels(
                new String[] { GUIUtils.setGUILabel(X_UNITS) }, GUIUtils.INPUT); //label x unit
        initialData.setParam_Action(
                new ActionObject(new String[] {
                        PluginUtils.getResources().getString(
                                "KlemGUI.hours.label"),
                        PluginUtils.getResources().getString(
                                "KlemGUI.minutes.label") }), GUIUtils.INPUT);//radio buttons: hours and minutes

        return initialData;
    }

    @Override
    public ComponentsTreeMap setComponentsActions(
            ComponentsTreeMap personalTreeMap) {

        //set radiobutton selection
        final JRadioButton jradioButton_Units = (JRadioButton) personalTreeMap
                .getComponent("07", GUIUtils.INPUT, 0);
        jradioButton_Units.setSelected(true);

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

                    //input values
                    final double paramA = GUIUtils
                            .getDoubleValue(componentsWithActions.getComponent(
                                    "00", GUIUtils.INPUT, 1));
                    final double paramN = GUIUtils
                            .getDoubleValue(componentsWithActions.getComponent(
                                    "01", GUIUtils.INPUT, 1));
                    final double duration_param = GUIUtils
                            .getDoubleValue(componentsWithActions.getComponent(
                                    "02", GUIUtils.INPUT, 1));
                    step_param = GUIUtils.getDoubleValue(componentsWithActions
                            .getComponent("03", GUIUtils.INPUT, 1));

                    selHyetoType = GUIUtils
                            .getSelectedButton(componentsWithActions
                                    .getComponent("05", GUIUtils.INPUT, 0));
                    if (selHyetoType == -1) {
                        return;
                    }

                    final int units = GUIUtils.getSelectedJRadioButton(
                            (JRadioButton) componentsWithActions.getComponent(
                                    "07", GUIUtils.INPUT, 0), // units: hours
                            (JRadioButton) componentsWithActions.getComponent(
                                    "07", GUIUtils.INPUT, 1)); //units: minutes

                    if (units == 0) {
                        xUnit = TimeInterval.TimeIntervalUnit.HOUR;
                    } else if (units == 1) {
                        xUnit = TimeInterval.TimeIntervalUnit.MINUTE;
                    }

                    //start calculate hyetograph
                    final TimeInterval duration = new TimeInterval(
                            duration_param, TimeInterval.TimeIntervalUnit.HOUR);

                    final LsppCalculator lsppCalculator = new LsppCalculator(
                            paramA, paramN, paramN);
                    final Geomorphology geomorphology = new Geomorphology(0, 0);

                    final InitialAbstraction ia = new InitialAbstraction(
                            InitialAbstraction.AbstractionUnits.MILLIMETERS, 0);
                    final BaseflowParams bfp = new BaseflowParams(
                            BaseflowParams.BaseflowType.DISTRIBUTED, null, 0);
                    final RainfallRecession rr = new RainfallRecession(0, 0);
                    final Watershed watershed = new Watershed(0d, null, null,
                            0, 0, 0, ia, bfp, 0, rr, 1d, geomorphology);

                    final TimeInterval step = new TimeInterval(step_param,
                            TimeInterval.TimeIntervalUnit.MINUTE);
                    final DesignRain designRain = new DesignRain(duration,
                            lsppCalculator.getParamsAN(), watershed);

                    final HyetographGenerator hyetoGenerator = new HyetographGenerator(
                            0.50, step, designRain);

                    //type of hyetograph
                    HyetographGenerator.HyetographShape hyetographShape;

                    switch (selHyetoType) {
                    case 0: {
                        hyetographShape = HyetographGenerator.HyetographShape.ALTERNATINGBLOCK;
                        hyetograph = hyetoGenerator.generateHytegraph(
                                hyetographShape, true);
                        break;
                    }
                    case 1: {
                        hyetographShape = HyetographGenerator.HyetographShape.CONSTANT;
                        hyetograph = hyetoGenerator.generateHytegraph(
                                hyetographShape, true);
                        break;
                    }
                    case 2: {
                        hyetographShape = HyetographGenerator.HyetographShape.INSTANT;
                        hyetograph = hyetoGenerator.generateHytegraph(
                                hyetographShape, true);
                        break;
                    }
                    case 3: {
                        hyetographShape = HyetographGenerator.HyetographShape.TRIANGULAR;
                        hyetograph = hyetoGenerator.generateHytegraph(
                                hyetographShape, true);
                        break;
                    }
                    case 4: {
                        hyetographShape = HyetographGenerator.HyetographShape.WALLINGFORD;
                        hyetograph = hyetoGenerator.generateHytegraph(
                                hyetographShape, true);
                        break;
                    }
                    }

                    final JTabbedPane mainTabelPane = super.getInitialDialog()
                            .getTabbedPane();
                    final HyetographPlugIn.HyetographOutput provaOut = new HyetographPlugIn.HyetographOutput(
                            context, super.getInitialDialog());
                    mainTabelPane.setComponentAt(1,
                            provaOut.getTabPluginComponents());
                    mainTabelPane.setEnabledAt(1, true);
                    mainTabelPane.setSelectedIndex(1);

                } catch (final Exception ex) {
                    ErrorDialog.show(super.getInitialDialog(),
                            PluginUtils.plugInName, ex.toString(),
                            StringUtil.stackTrace(ex));
                }
            }

            @Override
            public void leftButton() {
            }

            @Override
            public void centerButton() {
            }
        };
        return mainPanel;
    }

    @Override
    public String toString() {
        return PluginUtils.getResources().getString(
                "HyetographPlugIn.PlugInName.label");
    }

    class HyetographOutput extends AbstractInputKlemPlugin {

        public HyetographOutput(PlugInContext context,
                InitialDialog initialDialog) {
            super(context, initialDialog);
        }

        @Override
        public InitialData setInitialData() {
            final InitialData initialData = new InitialData();

            //output data   
            final PersonalChart personalChart = new PersonalChartHyetograph(
                    hyetograph, step_param, xUnit);
            initialData.setParam_ChartPanel(personalChart, GUIUtils.OUTPUT); //chart

            final Header header = new Header(new String[] {
                    PluginUtils.getResources().getString(
                            "HyetographPlugin.TotalRainfull.label")
                            + hyetograph.getTotalRain()
                            + ","
                            + PluginUtils.getResources().getString(
                                    "HyetographPlugin.HyetoShpe.label")
                            + getHyetoType(selHyetoType),
                    PluginUtils.getResources().getString(
                            "HyetographPlugin.Graph_TimeHour.label")
                            + ","
                            + PluginUtils.getResources().getString(
                                    "HyetographPlugin.Graph_Rainfall.label") });
            final PersonalTable personalTable = new PersonalTable(
                    setTableModel(fromHyetographToData()), header, false, true,
                    false, false, false, null, null, false);

            final FileNameExtensionFilter filter = new FileNameExtensionFilter(
                    "Hyetograph", new String[] { "txt" });
            initialData.setParam_PersonalTable(personalTable, filter,
                    GUIUtils.OUTPUT); //table

            return initialData;
        }

        @Override
        public ComponentsTreeMap setComponentsActions(
                ComponentsTreeMap personalTreeMap) {
            return personalTreeMap;
        }

        @Override
        public JPanel buildPluginPanel(ComponentsTreeMap componentsWithActions) {
            if (outPanel != null) {
                return outPanel;
            }

            outPanel = new MainPanel(super.getInitialDialog(),
                    componentsWithActions, false, false, false, PluginUtils
                            .getResources().getString(
                                    "MainPanel.ExecuteButton.text"),
                    layerablesList) {

                /**
                         * 
                         */
                private static final long serialVersionUID = 1L;

                @Override
                public void rightButton() {
                }

                @Override
                public void leftButton() {
                }

                @Override
                public void centerButton() {
                }
            };
            return outPanel;
        }

        private MainPanel outPanel;

    }

    private DefaultTableModel setTableModel(double[][] data1) {

        final Double[][] data = new Double[data1[0].length][data1.length];
        int row = 0;
        for (int c = 0; c < data1[0].length; c++) {
            int col = 0;
            for (final double[] data11 : data1) {
                data[row][col] = PluginUtils.getThreeDecimalFormat(data11[c]);
                col++;
            }
            row++;
        }

        String time = "";
        if (xUnit == TimeInterval.TimeIntervalUnit.HOUR) {
            time = PluginUtils.getResources().getString(
                    "HyetographPlugin.Graph_TimeHour.label");
        } else if (xUnit == TimeInterval.TimeIntervalUnit.MINUTE) {
            time = PluginUtils.getResources().getString(
                    "HyetographPlugin.Graph_TimeMin.label");

        }

        final DefaultTableModel dtm = new DefaultTableModel(data, new String[] {
                time,
                PluginUtils.getResources().getString(
                        "HyetographPlugin.Graph_Rainfall.label") });

        return dtm;
    }

    private double[][] fromHyetographToData() {

        final double[][] data = new double[2][hyetograph.getRainfall().length];
        for (int s = 0; s < hyetograph.getRainfall().length; s++) {
            data[0][s] = hyetograph.getStep().getInterval(
                    TimeInterval.TimeIntervalUnit.MINUTE)
                    * s;
            data[1][s] = hyetograph.getRainfall()[s];
        }

        // Units are hours: convert
        if (xUnit == TimeInterval.TimeIntervalUnit.HOUR) {
            step_param /= 60;
            for (int d = 0; d < data[0].length; d++) {
                data[0][d] /= 60;
            }
        }

        return data;

    }

    private String getHyetoType(int selection) {

        switch (selection) {
        case 0: {
            return PluginUtils.getResources().getString(
                    "HyetographPlugIn.AlternatingBlocks.label");
        }
        case 1: {
            return PluginUtils.getResources().getString(
                    "HyetographPlugIn.Costant.label");
        }
        case 2: {
            return PluginUtils.getResources().getString(
                    "HyetographPlugIn.IstantaneusIntensity.label");
        }
        case 3: {
            return PluginUtils.getResources().getString(
                    "HyetographPlugIn.Triangular.label");
        }
        case 4: {
            return PluginUtils.getResources().getString(
                    "HyetographPlugIn.Wallingford.labela");
        }
        }
        return "";
    }

    private final PlugInContext context;
    private MainPanel mainPanel;
    private Hyetograph hyetograph;
    private double step_param;
    int selHyetoType;
    private TimeInterval.TimeIntervalUnit xUnit;
    private final String PARAM_A = PluginUtils.getResources().getString(
            "HyetographPlugIn.ParamA.label");
    private final String PARAM_N = PluginUtils.getResources().getString(
            "HyetographPlugIn.ParamN.label");
    private final String TIME = PluginUtils.getResources().getString(
            "HyetographPlugIn.Time.label");
    private final String STEP = PluginUtils.getResources().getString(
            "HyetographPlugIn.Step.label");
    private final String X_UNITS = PluginUtils.getResources().getString(
            "HyetographPlugIn.XUnits.label");
    private final String MODEL = PluginUtils.getResources().getString(
            "HyetographPlugIn.models.label");

    private final LayerablesList layerablesList;

}
