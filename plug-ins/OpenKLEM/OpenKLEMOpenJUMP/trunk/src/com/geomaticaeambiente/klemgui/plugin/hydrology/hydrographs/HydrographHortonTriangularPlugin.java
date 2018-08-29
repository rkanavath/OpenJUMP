package com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs;

import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.ui.CommonHydroPanel;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.CommonHydrographData;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.HydroUtils;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.openjump.klem.hydrology.Hydrology;
import com.geomaticaeambiente.openjump.klem.hydrology.UnitHydrograph;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import it.geomaticaeambiente.klem.Hyetograph;
import it.geomaticaeambiente.klem.TimeInterval;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

/**
 *
 * @author Geomatica
 */
public class HydrographHortonTriangularPlugin extends AbstractInputKlemPlugin {

    public HydrographHortonTriangularPlugin(PlugInContext context, InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

        InitialData initialData = new InitialData();

        //Input
        CommonHydrographData common = new CommonHydrographData();
        initialData.setParam_CommonHydroData(common, GUIUtils.INPUT);

        //Other
        //Base flow
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(BASE_FLOW), "", GUIUtils.OTHER);
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(FLOW_LENGTH), "", GUIUtils.OTHER);
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(FLOW_VELOCITY), "", GUIUtils.OTHER);
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(HORTON_AREA), "", GUIUtils.OTHER);
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(HORTON_BIFURC), "", GUIUtils.OTHER);
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(HORTON_BIFURC), "", GUIUtils.OTHER);

        //graphic unit
        initialData.setParam_Labels(new String[]{GUIUtils.setGUILabel(PluginUtils.getResources().getString("HyetographPlugIn.XUnits.label"))}, GUIUtils.OTHER); //label x unit
        initialData.setParam_Action(new ActionObject(
                new String[]{PluginUtils.getResources().getString("KlemGUI.hours.label"),
                    PluginUtils.getResources().getString("KlemGUI.minutes.label")}), GUIUtils.OTHER);//radio buttons: hours and minutes
//        
        return initialData;

    }

    @Override
    public ComponentsTreeMap setComponentsActions(ComponentsTreeMap personalTreeMap) {
        //set radiobutton selection
        JRadioButton jradioButton_Units = (JRadioButton) personalTreeMap.getComponent("07", GUIUtils.OTHER, 0);
        jradioButton_Units.setSelected(true);

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

                    //input values
                    CommonHydrographData hydroData = GUIUtils.getCommonHydroDataFromPanel((CommonHydroPanel) componentsWithActions.getComponent("00", GUIUtils.INPUT, 0));

                    double watershedArea = hydroData.getWatershedArea();
                    hyetograph = hydroData.getHyetograph();
                    effectiveRainfall = hydroData.getEffectiveRainfall();

                    double baseFlow = GUIUtils.getDoubleValue(componentsWithActions.getComponent("00", GUIUtils.OTHER, 1));
                    double flowLength = GUIUtils.getDoubleValue(componentsWithActions.getComponent("01", GUIUtils.OTHER, 1)) * 1000; //TODO: sistemare quest aparte
                    double flowVelocity = GUIUtils.getDoubleValue(componentsWithActions.getComponent("02", GUIUtils.OTHER, 1));
                    double hortonAreaRatio = GUIUtils.getDoubleValue(componentsWithActions.getComponent("03", GUIUtils.OTHER, 1));
                    double hortonBiforcRatio = GUIUtils.getDoubleValue(componentsWithActions.getComponent("04", GUIUtils.OTHER, 1));
                    double hortonLengthRatio = GUIUtils.getDoubleValue(componentsWithActions.getComponent("05", GUIUtils.OTHER, 1));

                    checksParameters(baseFlow, flowLength, flowVelocity, hortonAreaRatio, hortonBiforcRatio, hortonLengthRatio);

                    int units = GUIUtils.getSelectedJRadioButton((JRadioButton) componentsWithActions.getComponent("07", GUIUtils.OTHER, 0), // units: hours
                            (JRadioButton) componentsWithActions.getComponent("07", GUIUtils.OTHER, 1)); //units: minutes

                    if (units == 0) {
                        xUnit = TimeInterval.TimeIntervalUnit.HOUR;
                    } else if (units == 1) {
                        xUnit = TimeInterval.TimeIntervalUnit.MINUTE;
                    }

                    timeStep = effectiveRainfall.getEffectiveRainfall()[0][1]
                            - effectiveRainfall.getEffectiveRainfall()[0][0];

                    unitHydrograph = Hydrology.calcIuhHortonTriangular(timeStep, watershedArea, baseFlow,
                            effectiveRainfall, flowLength, flowVelocity, hortonAreaRatio, hortonBiforcRatio, hortonLengthRatio);

                    HydroUtils.addOuputPanel(
                            context, super.getInitialDialog(),
                            hyetograph,
                            effectiveRainfall,
                            unitHydrograph,
                            xUnit,
                            HydroUtils.HydroType.HORTON_TRIANGULAR,
                            layerablesList);

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
            }

            @Override
            public void centerButton() {
            }

        };
        return this.mainPanel;
    }

    public String toString() {
        return PluginUtils.getResources().getString("HydrographHortonTriangular.PluginName.label");
    }

    private void checksParameters(double baseFlow, double flowLength, double flowVelocity,
            double hortonAreaRatio, double hortonBiforcRatio, double hortonLengthRatio) {

        HydroUtils.checkHydroValue(BASE_FLOW, baseFlow, 0d, Double.MAX_VALUE);
        HydroUtils.checkHydroValue(FLOW_LENGTH, flowLength, 0d, Double.MAX_VALUE);
        HydroUtils.checkHydroValue(FLOW_VELOCITY, flowVelocity, 0d, Double.MAX_VALUE);
        HydroUtils.checkHydroValue(HORTON_AREA, hortonAreaRatio, 0d, Double.MAX_VALUE);
        HydroUtils.checkHydroValue(HORTON_BIFURC, hortonBiforcRatio, 0d, Double.MAX_VALUE);
        HydroUtils.checkHydroValue(HORTON_LENGTH, hortonLengthRatio, 0d, Double.MAX_VALUE);

    }

    private PlugInContext context;
    private MainPanel mainPanel;
    private Hyetograph hyetograph;
    private Hydrology.EffectiveRainfall effectiveRainfall;
    private TimeInterval.TimeIntervalUnit xUnit;
    private double timeStep;
    private UnitHydrograph unitHydrograph;
    private final String BASE_FLOW = PluginUtils.getResources().getString("HydrographCommonLabels.BaseFlow.label");
    private final String FLOW_LENGTH = PluginUtils.getResources().getString("HydrographHortonTriangular.FlowLenght.label");
    private final String FLOW_VELOCITY = PluginUtils.getResources().getString("HydrographHortonTriangular.FlowVelocity.label");
    private final String HORTON_AREA = PluginUtils.getResources().getString("HydrographHortonTriangular.HortonAreaRatio.label");
    private final String HORTON_BIFURC = PluginUtils.getResources().getString("HydrographHortonTriangular.HortonBifurcRatio.label");
    private final String HORTON_LENGTH = PluginUtils.getResources().getString("HydrographHortonTriangular.HortonLengthRatio.label");

    private final LayerablesList layerablesList;
}
