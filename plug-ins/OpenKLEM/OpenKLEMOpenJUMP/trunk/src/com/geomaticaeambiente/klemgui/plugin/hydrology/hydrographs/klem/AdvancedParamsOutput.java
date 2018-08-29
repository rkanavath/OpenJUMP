package com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem;

import com.geomaticaeambiente.klemgui.exceptions.WarningException;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.CombinationComponents;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import it.geomaticaeambiente.klem.BaseflowParams;
import it.geomaticaeambiente.klem.HyetographGenerator;
import it.geomaticaeambiente.klem.InitialAbstraction;
import it.geomaticaeambiente.klem.Klem;
import it.geomaticaeambiente.klem.SimulationOutput;
import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

/**
 *
 * @author Geomatica
 */
class AdvancedParamsOutput extends AbstractInputKlemPlugin {

    public AdvancedParamsOutput(PlugInContext context, InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

        InitialData initialData = new InitialData();
        //Routing time
        initialData.setParam_Line(CombinationComponents.CombinationElement.LINE, GUIUtils.OTHER); //0
        initialData.setParam_Labels(new String[]{ROUTING_TIME, ""}, GUIUtils.OTHER); //1 //second label for exclamation mark //NOI18N
        initialData.setParam_Line(CombinationComponents.CombinationElement.LINE, GUIUtils.OTHER); //2

        //simple kinematics or complex kinematics
        initialData.setParam_Action(new ActionObject(new String[]{SIMPLE_KINEMATICS, ADVANCED_KINEMATICS}), GUIUtils.OTHER); //3

        //Simple kinematics          
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(SLOPE_VELOCITY), klemProps.getSlopeVelocity(), GUIUtils.OTHER); //4
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(CHANNEL_VELOCITY), klemProps.getChannelVelocity(), GUIUtils.OTHER); //5
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(THRESHOLD), klemProps.getThresholdValue(), GUIUtils.OTHER); //6

        //Complex kinematics
        //Slope velocity
        initialData.setParam_Labels(new String[]{SLOPE_VELOCITY}, GUIUtils.OTHER); //7       
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(GUIUtils.setLeftSpace(MIN)), Double.toString(klemProps.getMinSlopeVelocity()), GUIUtils.OTHER);//8
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(GUIUtils.setLeftSpace(MAX)), Double.toString(klemProps.getMaxSlopeVelocity()), GUIUtils.OTHER); //9
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(GUIUtils.setLeftSpace(K_V)), Double.toString(klemProps.getSlopeConstant()), GUIUtils.OTHER); //10

        //Channel velocity
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(CHANNEL_VELOCITY), klemProps.getChannelVelocity(), GUIUtils.OTHER); //11

        //Threshold
        initialData.setParam_Labels(new String[]{THRESHOLD}, GUIUtils.OTHER); //12
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(GUIUtils.setLeftSpace(MIN)), klemProps.getMinThreshold(), GUIUtils.OTHER); //13
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(GUIUtils.setLeftSpace(MAX)), klemProps.getMaxThreshold(), GUIUtils.OTHER); //14
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(GUIUtils.setLeftSpace(K_S)), klemProps.getThresholdConstant(), GUIUtils.OTHER); //15

        //Hyetograph
        initialData.setParam_Line(CombinationComponents.CombinationElement.LINE, GUIUtils.OTHER); //16
        initialData.setParam_Labels(new String[]{HYETOGRAPH, ""}, GUIUtils.OTHER); //17  //second label for exclamation mark //NOI18N
        initialData.setParam_Line(CombinationComponents.CombinationElement.LINE, GUIUtils.OTHER); //18

        //Type
        String[] hyetoTypes = KlemProperties.getHyetoTypeAsArray();
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(HYDRO_TYPE), hyetoTypes, GUIUtils.OTHER); //19

        //Peak
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(PEAK), klemProps.getHyetoPeakPosition(), GUIUtils.OTHER);//20

        //Rainfall step
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(RAINFALL_STEP), klemProps.getRainfallStep(), GUIUtils.OTHER); //21

        //Rainfall recession
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(RAINFALL_RECESSION), klemProps.getRainfallRecessionValue(), GUIUtils.OTHER);//22

        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(
                GUIUtils.setLeftSpace(THRESHOLD_RAINFALL_RECESSION)), klemProps.getRainfallRecessionThreshold(), GUIUtils.OTHER);//23

        //Initial abstraction 
        initialData.setParam_Label_TextBox_Button(GUIUtils.setGUILabel(INITIAL_ABSTRACTION), klemProps.getInitialAbstraction().getAbstractionValue(),
                new ActionObject(new String[]{"mm", PluginUtils.getResources().getString("HydrographKlemPlugin.InitialAbstractionUnitsFractionS.label")}), GUIUtils.OTHER);//24 //NOI18N
        initialData.setParam_Labels(new String[]{""}, GUIUtils.OTHER);//25

        //Hydrograph
        initialData.setParam_Line(CombinationComponents.CombinationElement.LINE, GUIUtils.OTHER); //26
        initialData.setParam_Labels(new String[]{HYDROGRAPH, ""}, GUIUtils.OTHER); //27
        initialData.setParam_Line(CombinationComponents.CombinationElement.LINE, GUIUtils.OTHER); //28

        //Step output
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(STEP_OUTPUT), klemProps.getHydroStepOutput(), GUIUtils.OTHER); //29
        initialData.setParam_Label_TextBox_Button(GUIUtils.setGUILabel(AMC), klemProps.getAmcValue(), new ActionObject(new String[]{"Const.", "Var."}), GUIUtils.OTHER); //30
        initialData.setParam_Action(GUIUtils.setGUILabel(BASE_FLOW_TYPE), new ActionObject(new String[]{"Lumped", "Distr."}), GUIUtils.OTHER);//31

        initialData.setParam_Label_TextBox_Button(GUIUtils.setGUILabel(BASE_FLOW), getValue(klemProps.isQ0Auto(), klemProps.getQ0()), new ActionObject(klemProps.isQ0Auto(), "Auto"), GUIUtils.OTHER);//32
        initialData.setParam_Label_TextBox_Label(GUIUtils.setGUILabel(DISCHARGE_CONST), klemProps.getBasefklowRecession(), "*1E-6", GUIUtils.OTHER);//33
        initialData.setParam_Label_TextBox_Button(GUIUtils.setGUILabel(ARF), getValue(klemProps.isArfAuto(), klemProps.getArfValue()), new ActionObject(klemProps.isArfAuto(), "Auto"), GUIUtils.OTHER);//34
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(FRPM_TC), klemProps.getPeakFraction(), GUIUtils.OTHER);//35
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(SUBSURFACE_DRAINAGE_LOSS), klemProps.getSubSurfaceDrainageLoss(), GUIUtils.OTHER);//36
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(GEOMORPH_FACTOR), klemProps.getGeomorphFactor(), GUIUtils.OTHER);//37
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(GUIUtils.setLeftSpace(GEOMORH_THRESHOLD)), klemProps.getGeomorphoFactorThreshold(), GUIUtils.OTHER);//38

        return initialData;
    }

    @Override
    public ComponentsTreeMap setComponentsActions(final ComponentsTreeMap personalTreeMap) {

        //set exclamation marks for labels
        final JLabel jLabel_RoutingTime = (JLabel) personalTreeMap.getComponent("01", GUIUtils.OTHER, 1);
        final JLabel jLabel_Hyetograph = (JLabel) personalTreeMap.getComponent("17", GUIUtils.OTHER, 1);
        JLabel jLabel_Hydrograph = (JLabel) personalTreeMap.getComponent("27", GUIUtils.OTHER, 1);
        GUIUtils.setExclamationMark(jLabel_RoutingTime, jLabel_Hyetograph, jLabel_Hydrograph);

        //routing time components
        //simple kinematics       
        JRadioButton jRadioButton_simpleKinematics = (JRadioButton) personalTreeMap.getComponent("03", GUIUtils.OTHER, 0);
        jRadioButton_simpleKinematics.setSelected(true);
        kinematicsType = KlemProperties.KinematicsType.SIMPLE;
        setVisibilityComponent(true, personalTreeMap);

        //set action to JTextField
        final JTextField text_slopeVelocity = (JTextField) personalTreeMap.getComponent("04", GUIUtils.OTHER, 1);
        final JTextField text_channelVelocity = (JTextField) personalTreeMap.getComponent("05", GUIUtils.OTHER, 1);
        final JTextField text_threshold = (JTextField) personalTreeMap.getComponent("06", GUIUtils.OTHER, 1);
        setRoutingTimeCheckActions(jLabel_RoutingTime, text_slopeVelocity, text_channelVelocity, text_threshold);

        jRadioButton_simpleKinematics.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                kinematicsType = KlemProperties.KinematicsType.SIMPLE;
                setVisibilityComponent(true, personalTreeMap);
                //set textfiekd action for label
                setRoutingTimeCheckActions(jLabel_RoutingTime, text_slopeVelocity, text_channelVelocity, text_threshold);
            }
        });

        //advanced kinematics
        JRadioButton jRadioButton_AndvancedKinematics = (JRadioButton) personalTreeMap.getComponent("03", GUIUtils.OTHER, 1);

        //complex
        final JTextField text_minslopeVelocity = (JTextField) personalTreeMap.getComponent("08", GUIUtils.OTHER, 1);
        final JTextField text_maxslopeVelocity = (JTextField) personalTreeMap.getComponent("09", GUIUtils.OTHER, 1);
        final JTextField text_costSlopeVelocity = (JTextField) personalTreeMap.getComponent("10", GUIUtils.OTHER, 1);
        final JTextField channelVelocity = (JTextField) personalTreeMap.getComponent("11", GUIUtils.OTHER, 1);
        final JTextField text_minthreshold = (JTextField) personalTreeMap.getComponent("13", GUIUtils.OTHER, 1);
        final JTextField text_maxthreshold = (JTextField) personalTreeMap.getComponent("14", GUIUtils.OTHER, 1);
        final JTextField text_costthreshold = (JTextField) personalTreeMap.getComponent("15", GUIUtils.OTHER, 1);
        setRoutingTimeCheckActions(jLabel_RoutingTime, text_minslopeVelocity, text_maxslopeVelocity, text_costSlopeVelocity, channelVelocity,
                text_minthreshold, text_maxthreshold, text_costthreshold);

        jRadioButton_AndvancedKinematics.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                kinematicsType = KlemProperties.KinematicsType.ADVANCED;
                setVisibilityComponent(false, personalTreeMap);
                //set textfiekd action for label
                setRoutingTimeCheckActions(jLabel_RoutingTime, text_minslopeVelocity, text_maxslopeVelocity, text_costSlopeVelocity, channelVelocity,
                        text_minthreshold, text_maxthreshold, text_costthreshold);

            }
        });

        //hyetograph components
        final JComboBox jComboBox_hyetograph = (JComboBox) personalTreeMap.getComponent("19", GUIUtils.OTHER, 1);
        int indexSel = setHyetoSelectedIndex(klemProps.getHyetoShape());
        jComboBox_hyetograph.setSelectedIndex(indexSel);
        hyetoShape = getHyetoSelected(indexSel);
        jComboBox_hyetograph.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                hyetoShape = getHyetoSelected(jComboBox_hyetograph.getSelectedIndex());
                if (hyetoShape != klemProps.getHyetoShape()) {
                    jLabel_Hyetograph.setVisible(true);
                }
            }
        });

        JRadioButton jRadioButton_01_initAbsUnit = (JRadioButton) personalTreeMap.getComponent("24", GUIUtils.OTHER, 3);
        jRadioButton_01_initAbsUnit.setSelected(true);
        initAbsUnit = InitialAbstraction.AbstractionUnits.FRACTION;
        jRadioButton_01_initAbsUnit.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                initAbsUnit = InitialAbstraction.AbstractionUnits.FRACTION;
            }
        });

        JRadioButton jRadioButton_mm_initAbsUnit = (JRadioButton) personalTreeMap.getComponent("24", GUIUtils.OTHER, 2);
        jRadioButton_mm_initAbsUnit.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                initAbsUnit = InitialAbstraction.AbstractionUnits.MILLIMETERS;
            }
        });

        //Hydrograph
        //AMC
        final JRadioButton jRadioButton_AMC_Const = (JRadioButton) personalTreeMap.getComponent("30", GUIUtils.OTHER, 2);
        jRadioButton_AMC_Const.setSelected(true);
        amc_type = KlemProperties.Amc_Type.COSTANT;
        jRadioButton_AMC_Const.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                if (jRadioButton_AMC_Const.isSelected()) {
                    amc_type = KlemProperties.Amc_Type.COSTANT;
                }
            }
        });

        final JRadioButton jRadioButton_AMC_Var = (JRadioButton) personalTreeMap.getComponent("30", GUIUtils.OTHER, 3);
        jRadioButton_AMC_Var.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                if (jRadioButton_AMC_Var.isSelected()) {
                    amc_type = KlemProperties.Amc_Type.VARIABLE;
                }
            }
        });

        if(klemProps.getRainfallType() != KlemProperties.RainfallType.DISTRIBUTED) {
            jRadioButton_AMC_Var.setEnabled(false);
        } else {
            jRadioButton_AMC_Var.setEnabled(true);
        }
        
        //check jtext values for hyetograph section
        setHyetographCheckAction(
                jLabel_Hyetograph,
                personalTreeMap.getComponent("20", GUIUtils.OTHER, 1),
                personalTreeMap.getComponent("21", GUIUtils.OTHER, 1),
                personalTreeMap.getComponent("22", GUIUtils.OTHER, 1),
                personalTreeMap.getComponent("23", GUIUtils.OTHER, 1),
                personalTreeMap.getComponent("24", GUIUtils.OTHER, 1)
        );

        //BaseFlow
        final JRadioButton jRadioButton_baseFlow_Lump = (JRadioButton) personalTreeMap.getComponent("31", GUIUtils.OTHER, 1);
        jRadioButton_baseFlow_Lump.setSelected(true);
        baseflowType = BaseflowParams.BaseflowType.LUMPED;
        jRadioButton_baseFlow_Lump.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                if (jRadioButton_baseFlow_Lump.isSelected()) {
                    baseflowType = BaseflowParams.BaseflowType.LUMPED;
                }
            }
        });

        final JRadioButton jRadioButton_baseflow_Distr = (JRadioButton) personalTreeMap.getComponent("31", GUIUtils.OTHER, 2);
        jRadioButton_baseFlow_Lump.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                if (jRadioButton_baseflow_Distr.isSelected()) {
                    baseflowType = BaseflowParams.BaseflowType.DISTRIBUTED;
                }
            }
        });

        //baseflow value
        final JTextField jTextField_BaseFlow = (JTextField) personalTreeMap.getComponent("32", GUIUtils.OTHER, 1);
        jTextField_BaseFlow.setEnabled(false);
        final JCheckBox jCheckBox_BaseFlow = (JCheckBox) personalTreeMap.getComponent("32", GUIUtils.OTHER, 2);
        jCheckBox_BaseFlow.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                if (jCheckBox_BaseFlow.isSelected()) {
                    jTextField_BaseFlow.setText("Auto");
                    jTextField_BaseFlow.setEnabled(false);
                    klemProps.setQ0Auto(true);
                } else {
                    jTextField_BaseFlow.setText(Double.toString(0.0));
                    jTextField_BaseFlow.setEnabled(true);
                    klemProps.setQ0Auto(false);
                }
            }

        });

        if (klemProps.isQ0Auto() == true) {
            jTextField_BaseFlow.setEnabled(false);
            jCheckBox_BaseFlow.setSelected(true);
        } else {
            jTextField_BaseFlow.setEnabled(true);
            jCheckBox_BaseFlow.setSelected(false);
        }

        //ARF value
        final JTextField jTextField_ARF = (JTextField) personalTreeMap.getComponent("34", GUIUtils.OTHER, 1);
        final JCheckBox jCheckBox_ARF = (JCheckBox) personalTreeMap.getComponent("34", GUIUtils.OTHER, 2);
        jCheckBox_ARF.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                if (jCheckBox_ARF.isSelected()) {
                    jTextField_ARF.setText("Auto");
                    jTextField_ARF.setEnabled(false);
                    klemProps.setArfAuto(true);
                } else {
                    jTextField_ARF.setText(Double.toString(1.0));
                    jTextField_ARF.setEnabled(true);
                    klemProps.setArfAuto(false);
                }
            }
        });

        if (klemProps.isArfAuto()) {
            jTextField_ARF.setEnabled(false);
            jCheckBox_ARF.setSelected(true);
        } else {
            jTextField_ARF.setEnabled(true);
            jCheckBox_ARF.setSelected(false);
        }

        setHydrographCheckAction(
                jLabel_Hydrograph,
                personalTreeMap.getComponent("29", GUIUtils.OTHER, 1),
                personalTreeMap.getComponent("30", GUIUtils.OTHER, 1),
                personalTreeMap.getComponent("32", GUIUtils.OTHER, 1),
                personalTreeMap.getComponent("33", GUIUtils.OTHER, 1),
                personalTreeMap.getComponent("34", GUIUtils.OTHER, 1),
                personalTreeMap.getComponent("35", GUIUtils.OTHER, 1),
                personalTreeMap.getComponent("36", GUIUtils.OTHER, 1),
                personalTreeMap.getComponent("37", GUIUtils.OTHER, 1),
                personalTreeMap.getComponent("38", GUIUtils.OTHER, 1)
        );
        
        //enable disable components when historical rainfall is or not selected
        if(klemProps.getRainfallType() == KlemProperties.RainfallType.HISTORICAL){
            GUIUtils.setEnableComponents(false, GUIUtils.getJComponentFromRowRange(personalTreeMap, 19, 21, GUIUtils.OTHER));
        }
                     
        return personalTreeMap;
    }

    @Override
    public JPanel buildPluginPanel(final ComponentsTreeMap componentsWithActions) {
        
        this.componentsWithActions = componentsWithActions;
        
        if(this.mainPanel != null) {
            return this.mainPanel;
        }
        this.mainPanel = new MainPanel(super.getInitialDialog(), componentsWithActions, false, false, true,
                PluginUtils.getResources().getString("MainPanel.ExecuteButton.text"), layerablesList) {
                    
            @Override
            public void rightButton() {

                try {

                    KlemUtils.checkAdvancedParams(kinematicsType, klemProps,
                            componentsWithActions, hyetoShape, initAbsUnit, amc_type, baseflowType);
                    
                    super.getInitialDialog().setCursor(new Cursor(Cursor.WAIT_CURSOR));
                    
                    Klem klem = KlemUtils.buildKlem(klemProps);
                    klem.run();
                    
                    SimulationOutput simOutput = klem.getSimulationOutput();

                    JTabbedPane mainTabelPane = super.getInitialDialog().getTabbedPane();

                    OutputTab outputPanel = new OutputTab(context, super.getInitialDialog(), layerablesList, simOutput, klem);
                    outputPanel.setKlemProperties(klemProps);
                    mainTabelPane.setComponentAt(3, outputPanel.getTabPluginComponents());
                    mainTabelPane.setEnabledAt(3, true);
                    mainTabelPane.setSelectedIndex(3);
                 

                } catch (WarningException ex) {
                    super.getInitialDialog().setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                    JOptionPane.showMessageDialog(super.getInitialDialog(), ex.getMessage(), PluginUtils.plugInName, JOptionPane.WARNING_MESSAGE);
                } catch (Exception ex) {
                    super.getInitialDialog().setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
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
            }

            @Override
            public void centerButton() {
            }

        };

        return this.mainPanel;
    }

    public void setAndCheckParams() throws WarningException {
        KlemUtils.checkAdvancedParams(kinematicsType, klemProps, componentsWithActions, hyetoShape, initAbsUnit, amc_type, baseflowType);
    }
    
    public void setKlemProperties(KlemProperties klemProp) {
        this.klemProps = klemProp;
    }

    private void setVisibilityComponent(boolean simpleIsVisible, ComponentsTreeMap personalTreeMap) {
        GUIUtils.setVisibleComponents(simpleIsVisible, GUIUtils.getJComponentFromRowRange(personalTreeMap, 4, 6, GUIUtils.OTHER));
        GUIUtils.setVisibleComponents(!simpleIsVisible, GUIUtils.getJComponentFromRowRange(personalTreeMap, 7, 15, GUIUtils.OTHER));
    }

    private HyetographGenerator.HyetographShape getHyetoSelected(int index) {

        switch (index) {

            case 1:
                return HyetographGenerator.HyetographShape.CONSTANT;

            case 2:
                return HyetographGenerator.HyetographShape.WALLINGFORD;

            case 3:
                return HyetographGenerator.HyetographShape.TRIANGULAR;

            case 4:
                return HyetographGenerator.HyetographShape.ALTERNATINGBLOCK;

            case 5:
                return HyetographGenerator.HyetographShape.INSTANT;

            case 6:
                return HyetographGenerator.HyetographShape.INCREASING;
        }
        return null;

    }

    private int setHyetoSelectedIndex(HyetographGenerator.HyetographShape shape) {
        switch (shape) {
            case CONSTANT:
                return 1;
            case WALLINGFORD:
                return 2;
            case TRIANGULAR:
                return 3;
            case ALTERNATINGBLOCK:
                return 4;
            case INSTANT:
                return 5;
            case INCREASING:
                return 6;
        }
        return 0;
    }

    private void setRoutingTimeCheckActions(JLabel label, JComponent... components) {
        if (loadedPrj) {
            if (kinematicsType != klemProps.getKinematicsType()) {
                label.setVisible(true);
                return;
            }

            switch (kinematicsType) {
                case SIMPLE: {
                    GUIUtils.setJTextAction(loadedPrj, (JTextField) components[0], klemProps.getSlopeVelocity(), label);
                    GUIUtils.setJTextAction(loadedPrj, (JTextField) components[1], klemProps.getChannelVelocity(), label);
                    GUIUtils.setJTextAction(loadedPrj, (JTextField) components[2], klemProps.getThresholdValue(), label);
                    break;
                }
                case ADVANCED: {
                    GUIUtils.setJTextAction(loadedPrj, (JTextField) components[0], klemProps.getMinSlopeVelocity(), label);
                    GUIUtils.setJTextAction(loadedPrj, (JTextField) components[1], klemProps.getMaxSlopeVelocity(), label);
                    GUIUtils.setJTextAction(loadedPrj, (JTextField) components[2], klemProps.getSlopeConstant(), label);
                    GUIUtils.setJTextAction(loadedPrj, (JTextField) components[3], klemProps.getChannelVelocity(), label);
                    GUIUtils.setJTextAction(loadedPrj, (JTextField) components[4], klemProps.getMinThreshold(), label);
                    GUIUtils.setJTextAction(loadedPrj, (JTextField) components[5], klemProps.getMaxThreshold(), label);
                    GUIUtils.setJTextAction(loadedPrj, (JTextField) components[6], klemProps.getThresholdConstant(), label);
                    break;
                }
            }

        }
    }

    private void setHyetographCheckAction(JLabel label, JComponent... components) {
        if (loadedPrj) {
            if (hyetoShape != klemProps.getHyetoShape()) {
                label.setVisible(true);
                return;
            }

            GUIUtils.setJTextAction(loadedPrj, (JTextField) components[0], klemProps.getHyetoPeakPosition(), label);
            GUIUtils.setJTextAction(loadedPrj, (JTextField) components[1], klemProps.getRainfallStep(), label);
            GUIUtils.setJTextAction(loadedPrj, (JTextField) components[2], klemProps.getRainfallRecessionValue(), label);
            GUIUtils.setJTextAction(loadedPrj, (JTextField) components[3], klemProps.getRainfallRecessionThreshold(), label);
            GUIUtils.setJTextAction(loadedPrj, (JTextField) components[4], klemProps.getInitialAbstraction().getAbstractionValue(), label);

            if (initAbsUnit != klemProps.getInitAbsUnit()) {
                label.setVisible(true);
            }
        }
    }

    private void setHydrographCheckAction(JLabel label, JComponent... components) {

        if (loadedPrj) {
            if (amc_type != klemProps.getAmcType()) {
                label.setVisible(true);
                return;
            }
            if (baseflowType != klemProps.getBaseFlowType()) {
                label.setVisible(true);
                return;
            }

            GUIUtils.setJTextAction(loadedPrj, (JTextField) components[0], klemProps.getHydroStepOutput(), label);
            GUIUtils.setJTextAction(loadedPrj, (JTextField) components[1], klemProps.getAmcValue(), label);
            if (!klemProps.isQ0Auto()) {
                GUIUtils.setJTextAction(loadedPrj, (JTextField) components[2], klemProps.getQ0(), label);
            }

            GUIUtils.setJTextAction(loadedPrj, (JTextField) components[3], klemProps.getBasefklowRecession(), label);
            if (!klemProps.isArfAuto()) {
                GUIUtils.setJTextAction(loadedPrj, (JTextField) components[4], klemProps.getArfValue(), label);
            }
            GUIUtils.setJTextAction(loadedPrj, (JTextField) components[5], klemProps.getPeakFraction(), label);
            GUIUtils.setJTextAction(loadedPrj, (JTextField) components[6], klemProps.getSubSurfaceDrainageLoss(), label);
            GUIUtils.setJTextAction(loadedPrj, (JTextField) components[7], klemProps.getGeomorphFactor(), label);
            GUIUtils.setJTextAction(loadedPrj, (JTextField) components[8], klemProps.getGeomorphoFactorThreshold(), label);

            if (initAbsUnit != klemProps.getInitAbsUnit()) {
                label.setVisible(true);
            }
        }
    }

    public void setLoadedPrj(boolean loadedPrj) {
        this.loadedPrj = loadedPrj;
    }

    private String getValue(boolean isAuto, double displayValue) {
        if (isAuto) {
            return "Auto";
        } else {
            return Double.toString(displayValue);
        }
    }
    
    public void setHyetoControlsVisibility(boolean visible){
        GUIUtils.setEnableComponents(
                visible, GUIUtils.getJComponentFromRowRange(componentsWithActions, 19, 21, GUIUtils.OTHER));
        GUIUtils.setEnableComponents(
                visible, GUIUtils.getJComponentFromRowRange(componentsWithActions, 34, 34, GUIUtils.OTHER));
    }
    
    private final PlugInContext context;
    private KlemProperties klemProps;
    private KlemProperties.KinematicsType kinematicsType;
    private HyetographGenerator.HyetographShape hyetoShape;
    private InitialAbstraction.AbstractionUnits initAbsUnit;
    private KlemProperties.Amc_Type amc_type;
    private BaseflowParams.BaseflowType baseflowType;
    private boolean loadedPrj = false;

    private ComponentsTreeMap componentsWithActions;
    private MainPanel mainPanel;
    private final String ROUTING_TIME = PluginUtils.getResources().getString("HydrographKlemPlugin.RoutingTime.label");
    private final String SLOPE_VELOCITY = PluginUtils.getResources().getString("HydrographKlemPlugin.SlopeVelocity.label");
    private final String MIN = PluginUtils.getResources().getString("HydrographKlemPlugin.Min.label");
    private final String MAX = PluginUtils.getResources().getString("HydrographKlemPlugin.Max.label");
    private final String K_V = PluginUtils.getResources().getString("HydrographKlemPlugin.CostV.label");
    private final String K_S = PluginUtils.getResources().getString("HydrographKlemPlugin.CostS.label");
    private final String CHANNEL_VELOCITY = PluginUtils.getResources().getString("HydrographKlemPlugin.ChannelVelocity.label");
    private final String THRESHOLD = PluginUtils.getResources().getString("HydrographKlemPlugin.Threshold.label");
    private final String HYETOGRAPH = PluginUtils.getResources().getString("HydrographKlemPlugin.Hetograph.label");
    private final String HYDRO_TYPE = PluginUtils.getResources().getString("HydrographKlemPlugin.HyetoType.label");
    private final String PEAK = PluginUtils.getResources().getString("HydrographKlemPlugin.RunPeak.label");
    private final String RAINFALL_STEP = PluginUtils.getResources().getString("HydrographKlemPlugin.StepRainfallData.label");
    private final String RAINFALL_RECESSION = PluginUtils.getResources().getString("HydrographKlemPlugin.RainfallRecession.label");
    private final String THRESHOLD_RAINFALL_RECESSION = PluginUtils.getResources().getString("HydrographKlemPlugin.ThresholdRainfallRecession.label");
    private final String INITIAL_ABSTRACTION = PluginUtils.getResources().getString("HydrographKlemPlugin.InitialAbstraction.label");
//    private final String INITIAL_ABSTRACTION_UNIT = PluginUtils.getResources().getString("HydrographKlemPlugin.InitialAbstractionUnit.label");
//    private final String INITIAL_ABSTRACTION_VALUE = PluginUtils.getResources().getString("HydrographKlemPlugin.InitialAbstractionValue.label");
    private final String SIMPLE_KINEMATICS = PluginUtils.getResources().getString("HydrographNashPlugin.SimpleKinematics.label");
    private final String ADVANCED_KINEMATICS = PluginUtils.getResources().getString("HydrographKlemPlugin.AdvancedKinematics.label");
    private final String HYDROGRAPH = PluginUtils.getResources().getString("HydrographKlemPlugin.Hydrograph.label");
    private final String STEP_OUTPUT = PluginUtils.getResources().getString("HydrographKlemPlugin.StepOutput.label");
    private final String AMC = PluginUtils.getResources().getString("HydrographKlemPlugin.AMC.label");
    private final String BASE_FLOW_TYPE = PluginUtils.getResources().getString("HydrographKlemPlugin.BaseflowType.label");
    private final String BASE_FLOW = PluginUtils.getResources().getString("HydrographKlemPlugin.BaseFlow.label");
    private final String DISCHARGE_CONST = PluginUtils.getResources().getString("HydrographKlemPlugin.BaseflowRecession.label");
    private final String ARF = PluginUtils.getResources().getString("HydrographKlemPlugin.ARF.label");
    private final String FRPM_TC = PluginUtils.getResources().getString("HydrographKlemPlugin.FRPM-TC.label");
    private final String GEOMORPH_FACTOR = PluginUtils.getResources().getString("HydrographKlemPlugin.GeomorphologicalFactor.label");
    private final String GEOMORH_THRESHOLD = PluginUtils.getResources().getString("HydrographKlemPlugin.ThresholdGeomorphologicalFactor.label");
    private final String SUBSURFACE_DRAINAGE_LOSS = PluginUtils.getResources().getString("HydrographKlemPlugin.SubSurfDrainageLoss.label");

    private final LayerablesList layerablesList;

}
