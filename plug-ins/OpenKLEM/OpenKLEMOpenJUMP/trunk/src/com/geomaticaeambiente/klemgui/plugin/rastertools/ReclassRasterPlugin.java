package com.geomaticaeambiente.klemgui.plugin.rastertools;

import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox.RasterComboBox;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.ui.PersonalTableComponents;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.YourTableCellRenderer1;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.klemgui.utils.PersonalTable;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.openjump.klem.rastertools.RasterReclassifier;
import com.geomaticaeambiente.openjump.klem.rastertools.ReclassTuple;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.Raster;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.table.DefaultTableModel;
import org.openjump.core.rasterimage.RasterImageLayer;

/**
 *
 * @author Geomatica
 */
public class ReclassRasterPlugin extends AbstractInputKlemPlugin {

    public ReclassRasterPlugin(PlugInContext context, InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

//        rasterImageLayers = PluginUtils.getRasterImageLayers(context);

        methods = new String[]{
            ST_DEV_METHOD,
            FIXED_INTERV_METHOD,
            INTERVAL_METHOD,
            UNIQUE_METHOD
        };
        Integer[] classes = new Integer[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        Integer[] stDevClasses = new Integer[]{1, 2, 3, 4, 5};

        PersonalTable personalTable = new PersonalTable(reclassTableModel(), null,
                false, false, true, false, true, null, null, true);

        InitialData initialData = new InitialData();

        //input data
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(RASTER_IN_LABEL), PluginUtils.getRasterImageLayers(layerablesList.getLayerables()), GUIUtils.INPUT); //Raster input
        initialData.setParam_Label_TextBox_Button(GUIUtils.setGUILabel(METHOD_LABEL),
                methods,
                new ActionObject(PluginUtils.getResources().getString("ReclassRasterPlugin.FindValues.button")), GUIUtils.INPUT); //method
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(CLASSES_LABEL), classes, GUIUtils.INPUT); //classes
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(STDEV_LABEL), stDevClasses, GUIUtils.INPUT); //st dev
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(INTERVAL_LABEL), "", GUIUtils.INPUT);//interval  
        initialData.setParam_PersonalTable(personalTable, null, GUIUtils.INPUT);//table

        //output data
        initialData.setParam_Label_TextBox_Button(
                GUIUtils.setGUILabel(GUIUtils.getOutputRasterLabel()), "", new ActionObject(""), GUIUtils.OUTPUT);

        return initialData;

    }

    @Override
    public ComponentsTreeMap setComponentsActions(ComponentsTreeMap personalTreeMap) {

        JButton jButton_findValue = (JButton) personalTreeMap.getComponent("01", GUIUtils.INPUT, 2); //Find values button  // 01 input= row   2=column

        final CustomComboBox.RasterComboBox raster = (CustomComboBox.RasterComboBox) (JComboBox) personalTreeMap.getComponent("00", GUIUtils.INPUT, 1);//raster in combobox action
        raster.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                rasterChanged = true;
            }
        });

        final JComboBox jCombobox_method = (JComboBox) personalTreeMap.getComponent("01", GUIUtils.INPUT, 1); //method combobox
        final JComboBox jCombobox_classes = (JComboBox) personalTreeMap.getComponent("02", GUIUtils.INPUT, 1); //classes combobox
        final JComboBox jcCombobox_StDev = (JComboBox) personalTreeMap.getComponent("03", GUIUtils.INPUT, 1); //st. dev. combobox
        final JTextField jTextFiedl_interval = (JTextField) personalTreeMap.getComponent("04", GUIUtils.INPUT, 1);
        final PersonalTableComponents personalComp_Table = (PersonalTableComponents) personalTreeMap.getComponent("05", GUIUtils.INPUT, 0); //table 
        
        final JLabel jLabel_classes = (JLabel) personalTreeMap.getComponent("02", GUIUtils.INPUT, 0);
        final JLabel jLabel_StDev = (JLabel) personalTreeMap.getComponent("03", GUIUtils.INPUT, 0);
        final JLabel jLabel_interval = (JLabel) personalTreeMap.getComponent("04", GUIUtils.INPUT, 0);

        //find raster values
        jButton_findValue.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    findValues(raster, jCombobox_method, jCombobox_classes, jcCombobox_StDev, jTextFiedl_interval, personalComp_Table);
                } catch (Exception ex) {
                    JOptionPane.showMessageDialog(
                            mainPanel,
                            ex.getMessage(),
                            PluginUtils.plugInName,
                            JOptionPane.WARNING_MESSAGE);
                }
            }
        });

        //enable/disable element when change the method
        jCombobox_method.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                enableElement(jCombobox_method, jLabel_classes, jCombobox_classes, jLabel_StDev, jcCombobox_StDev, jLabel_interval, jTextFiedl_interval);
            }
        });

        //output
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

                    //input values 
                    String rasterSelected = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));
                    String methodSelected = GUIUtils.getStringValue(componentsWithActions.getComponent("01", GUIUtils.INPUT, 1));
                    String classesSelected = GUIUtils.getStringValue(componentsWithActions.getComponent("02", GUIUtils.INPUT, 1));
                    String stDevSelected = GUIUtils.getStringValue(componentsWithActions.getComponent("03", GUIUtils.INPUT, 1));
                    String intervalSelected = GUIUtils.getStringValue(componentsWithActions.getComponent("04", GUIUtils.INPUT, 1));

                    //get output raster name
                    String outRasterName = GUIUtils.getStringValue(componentsWithActions.getComponent("00", GUIUtils.OUTPUT, 1));

                    //check input and output values
                    checkValues(rasterSelected, methodSelected, classesSelected, stDevSelected, intervalSelected, outRasterName);

                    //extract raster selected from combobox
                    RasterImageLayer inputRasterSelected = PluginUtils.getRasterImageLayerSelected((RasterComboBox) componentsWithActions.getComponent("00", GUIUtils.INPUT, 1));
                    DoubleBasicGrid rasterDBG = RasterUtils.getDoubleBasicGrid(inputRasterSelected);

                    //extract values from table     
                    String newValuesTable = GUIUtils.getStringValue(componentsWithActions.getComponent("05", GUIUtils.INPUT, 0)); //TODO: CHECK
                    ReclassTuple[] reclassPair = PluginUtils.getReclassPairFromString(newValuesTable);

                    //exceute reclassification
                    RasterReclassifier reclassifier = new RasterReclassifier();
                    DoubleBasicGrid reclassRaster = reclassifier.reclassify(rasterDBG, reclassPair);

                    ///Create the output rasterImageLayer and display on OJ    
                    //Save grid as tiff
                    RasterUtils.saveOutputRasterAsTiff(reclassRaster, new File(outRasterName));
                    //Display raster on OJ from file                
                    RasterUtils.displayRasterFileOnOJ(
                            context.getWorkbenchContext(),
                            new File(outRasterName),
                            null);

                    JOptionPane.showMessageDialog(super.getInitialDialog(),
                            PluginUtils.getResources().getString("SetWorkspacePlugin.Done.message"), PluginUtils.plugInName, JOptionPane.INFORMATION_MESSAGE);

                } catch (Exception ex) {
                    ErrorDialog.show(
                            super.getInitialDialog(),
                            PluginUtils.plugInName,
                            ex.toString(),
                            StringUtil.stackTrace(ex));
                }
            }

            ;
            
            @Override
            public void leftButton() {

            }

            @Override
            public void centerButton() {

            }

        };

        return this.mainPanel;

    }

    @Override
    public String toString() {
        return PluginUtils.getResources().getString("ReclassRasterPlugin.PlugInName.text");
    }

    private DefaultTableModel reclassTableModel() {

        String col[] = {FROM_LABEL, TO_LABEL, NEW_LABEL};

        String data[][] = {{null, null, null}};

        DefaultTableModel dtm = new DefaultTableModel(data, col);

        // Empty table
        for (int r = dtm.getRowCount() - 1; r >= 0; --r) {
            boolean toRemove = true;
            for (int c = 0; c < dtm.getColumnCount(); c++) {
                Object cellCont = dtm.getValueAt(r, c);
                if (cellCont != null) {
                    toRemove = false;
                    break;
                }
            }
            if (toRemove) {
                dtm.removeRow(r);
            }
        }

        return dtm;

    }

    private void findValues(RasterComboBox raster, JComboBox metodo, JComboBox classi, JComboBox dev, JTextField interval, PersonalTableComponents ptc) throws Exception {
        try {

            String rasterIn = raster.getSelectedItem().toString();
            String methodSelected = metodo.getSelectedItem().toString();
            String classSelected = classi.getSelectedItem().toString();
            String devSelected = dev.getSelectedItem().toString();
            String intervalSelected = interval.getText();

            //check input values
            checkValues(rasterIn, methodSelected, classSelected, devSelected, intervalSelected, null);

            RasterImageLayer ril = PluginUtils.getRasterImageLayerSelected(raster);

            rasterChanged = true;

            // Read values
            ArrayList<Double> valsUnique = new ArrayList<Double>();
            double valSum;
            double valCount;
            double valSumSquare;
            double valMin = 10E12;
            double valMax = -10E12;
            double valStDev = 0d;

            // Read data only if new flt selected
            if (rasterChanged || metodo.getSelectedIndex() == 4) {

                Raster raster_ = ril.getRasterData(null);

                int width = raster_.getWidth();
                int height = raster_.getHeight();

                // Transfer vals to arraylist
                ArrayList<Double> vals = new ArrayList<Double>();
                double val;

                valSum = 0;
                valCount = 0;
                valSumSquare = 0;
                valMin = 10E12;
                valMax = -10E12;

                for (int row = 0; row < height; row++) {
                    for (int col = 0; col < width; col++) {

                        val = raster_.getSampleDouble(col, row, 0);

                        if (!ril.isNoData(val)) {

                            if (metodo.getSelectedIndex() == 4) {
                                vals.add(Math.round(val * 10000000d) / 10000000d);
                            }

                            valSum = valSum + val;
                            valSumSquare += val * val;
                            valCount++;

                            if (val < valMin) {
                                valMin = val;
                            }
                            if (val > valMax) {
                                valMax = val;
                            }

                        }
                    }
                }

                double valMean = valSum / valCount;
                valStDev = Math.sqrt(valSumSquare / valCount - valMean * valMean);

                // Only for unique values
                if (metodo.getSelectedIndex() == 4) {

                    // Find unique values
                    Collections.sort(vals);
                    valsUnique.add(vals.get(0));

                    for (int ii = 1; ii < vals.size(); ii++) {
                        if (!vals.get(ii).equals(vals.get(ii - 1))) {
                            valsUnique.add(vals.get(ii));
                        }
                    }
                }

            }

            // Add values to table
            // Remove all rows
            DefaultTableModel dtm = (DefaultTableModel) ptc.getTabelModel();

            for (int i = dtm.getRowCount() - 1; i >= 0; --i) {
                dtm.removeRow(i);
            }

            // Add rows         
            switch (metodo.getSelectedIndex()) {

                // StandardDev
                case 1:
                    int nrStDev = Integer.parseInt((dev.getSelectedItem()).toString());

                    // Fill up rows
                    double valSpan = valStDev / nrStDev;
                    int intvCount = (int) ((valMax - valMin) / valSpan);

                    if (intvCount > 100) {
                        int res = JOptionPane.showConfirmDialog(
                                super.getInitialDialog(),
                                PluginUtils.getResources().getString("ReclassRasterPlugin.MoreThen100.message"),
                                PluginUtils.plugInName,
                                JOptionPane.YES_NO_OPTION);
                        if (res != 0) {
                            return;
                        }
                    }

                    double valStart;
                    double valEnd;

                    // First interval
                    valEnd = valMin;
                    // Following intervals
                    for (int i = 0; i <= intvCount; i++) {
                        valStart = valEnd;
                        valEnd = valStart + valSpan;
                        if (valEnd >= valMax) {
                            valEnd = valMax;
                        }

                        dtm.addRow(new Object[]{valStart, valEnd, null});

                    }

                    dtm.addRow(new Object[]{-9999.0, -9999.0, -9999.0});
                    setCellRenderer(intvCount + 1, ptc);
                    break;

                // Equal interval
                case 2:

                    try {
                        valSpan = Double.parseDouble(interval.getText());
                    } catch (Exception ex1) {
                        JOptionPane.showMessageDialog(
                                super.getInitialDialog(),
                                PluginUtils.getResources().getString("ReclassRasterPlugin.VerifyIntervalsNo.message"),
                                PluginUtils.plugInName,
                                JOptionPane.WARNING_MESSAGE);
                        break;
                    }

                    valMin = Math.floor(valMin/valSpan) * valSpan;
                    valMax = Math.ceil(valMax/valSpan) * valSpan;
                    intvCount = (int) ((valMax - valMin) / valSpan);
                    if (intvCount > 100) {
                        int res = JOptionPane.showConfirmDialog(
                                super.getInitialDialog(),
                                PluginUtils.getResources().getString("ReclassRasterPlugin.FoundMoreThen100Classes.message"),
                                PluginUtils.plugInName,
                                JOptionPane.YES_NO_OPTION);
                        if (res != 0) {
                            return;
                        }
                    }

                    // Following intervals
                    valEnd = valMin;
                    for (int i = 0; i < intvCount; i++) {
                        valStart = valEnd;
                        valEnd = valStart + valSpan;
                        if (valEnd >= valMax) {
                            valEnd = valMax;
                        }

                        dtm.addRow(new Object[]{valStart, valEnd, null});

                    }

                    dtm.addRow(new Object[]{-9999.0, -9999.0, -9999.0});
                    setCellRenderer(intvCount + 1, ptc);
                    break;

                // Intervals    
                case 3:
                    intvCount = classi.getSelectedIndex() - 1;
                    if (intvCount > 100) {
                        int res = JOptionPane.showConfirmDialog(
                                super.getInitialDialog(),
                                PluginUtils.getResources().getString("ReclassRasterPlugin.FoundMoreThen100Classes.message"),
                                PluginUtils.plugInName,
                                JOptionPane.YES_NO_OPTION);
                        if (res != 0) {
                            return;
                        }
                    }

                    valSpan = (int) ((valMax - valMin) / intvCount);

                    // First interval
                    valEnd = valMin;
                    // Following intervals
                    for (int i = 0; i <= intvCount; i++) {
                        valStart = valEnd;
                        valEnd = valStart + valSpan;
                        if (valEnd >= valMax) {
                            valEnd = valMax;
                        }

                        dtm.addRow(new Object[]{valStart, valEnd, null});
                    }

                    dtm.addRow(new Object[]{-9999.0, -9999.0, -9999.0});
                    setCellRenderer(intvCount + 1, ptc);
                    break;

                case 4:
                    // Unique integer values
                    intvCount = valsUnique.size();
                    if (intvCount > 100) {
                        int res = JOptionPane.showConfirmDialog(
                                super.getInitialDialog(),
                                PluginUtils.getResources().getString("ReclassRasterPlugin.FoundMoreThen100Classes.message"),
                                PluginUtils.plugInName,
                                JOptionPane.YES_NO_OPTION);
                        if (res != 0) {
                            return;
                        }
                    }
                    for (int i = 0; i < intvCount; i++) {
                        dtm.addRow(new Object[]{valsUnique.get(i), valsUnique.get(i), null});
                    }

                    dtm.addRow(new Object[]{-9999.0, -9999.0, -9999.0});

                    //System.out.println(intvCount);
                    setCellRenderer(intvCount, ptc);
                    break;

            }

            rasterChanged = false;

        } catch (Exception ex) {
            ErrorDialog.show(
                    super.getInitialDialog(),
                    PluginUtils.plugInName,
                    ex.toString(),
                    StringUtil.stackTrace(ex));
        }
    }

    private static void setCellRenderer(int row, PersonalTableComponents ptc) {

        YourTableCellRenderer1 ytcr = new YourTableCellRenderer1();
        ytcr.setRow(row);
        ytcr.setForeColour(Color.BLUE);
        for (int c = 0; c < ptc.getTabel().getColumnCount(); c++) {
            ptc.getTabel().getColumnModel().getColumn(c).setCellRenderer(ytcr);
        }

    }

    private void enableElement(JComboBox method, JLabel classesLabel, JComboBox classes, JLabel stdevLabel, JComboBox stDev, JLabel intervalLabel, JTextField intervals) {
        switch (method.getSelectedIndex()) {

            /* Standard deviations */
            case 1:

                classesLabel.setEnabled(false);
                classes.setEnabled(false);
                stdevLabel.setEnabled(true);
                stDev.setEnabled(true);
                intervalLabel.setEnabled(false);
                intervals.setEnabled(false);

                break;
            /* Equal Interval */
            case 2:

                classesLabel.setEnabled(false);
                classes.setEnabled(false);
                stdevLabel.setEnabled(false);
                stDev.setEnabled(false);
                intervalLabel.setEnabled(true);
                intervals.setEnabled(true);

                break;

            /* Intevals */
            case 3:

                classesLabel.setEnabled(true);
                classes.setEnabled(true);
                stdevLabel.setEnabled(false);
                stDev.setEnabled(false);
                intervalLabel.setEnabled(false);
                intervals.setEnabled(false);

                break;
            /* manual */
            case 4:
                classesLabel.setEnabled(false);
                classes.setEnabled(false);
                stdevLabel.setEnabled(false);
                stDev.setEnabled(false);
                intervalLabel.setEnabled(false);
                intervals.setEnabled(false);
                break;

        }
    }

    private void checkValues(String raster, String method, String classes, String stDev, String interval, String rasterOut) throws IOException, Exception {

        GUIUtils.checkStringValue(raster, RASTER_IN_LABEL);
        GUIUtils.checkStringValue(method, METHOD_LABEL);

        if (method.equals(ST_DEV_METHOD)) {
            GUIUtils.checkStringValue(stDev, STDEV_LABEL);
        } else if (method.equals(FIXED_INTERV_METHOD)) {
            GUIUtils.checkStringValue(interval, INTERVAL_LABEL);
            GUIUtils.checkIntegerValue(interval, INTERVAL_LABEL);

        } else if (method.equals(INTERVAL_METHOD)) {

            if (classes == null) {
                throw new NullPointerException();
            }

        }

        if (rasterOut != null) {
            GUIUtils.checkFileValue(rasterOut, GUIUtils.getOutputRasterString());
        }

    }

//    private RasterImageLayer[] rasterImageLayers;
    private final PlugInContext context;
    private MainPanel mainPanel;
    private final String ST_DEV_METHOD = PluginUtils.getResources().getString("ReclassRasterPlugin.StDev.text");
    private final String FIXED_INTERV_METHOD = PluginUtils.getResources().getString("ReclassRasterPlugin.FixedInterval.text");
    private final String INTERVAL_METHOD = PluginUtils.getResources().getString("ReclassRasterPlugin.IntervalsNumber.text");
    private final String UNIQUE_METHOD = PluginUtils.getResources().getString("ReclassRasterPlugin.UniqueValues.text");
    private final String RASTER_IN_LABEL = PluginUtils.getResources().getString("KlemGUI.InputRaster.label");
    private final String METHOD_LABEL = PluginUtils.getResources().getString("ReclassRasterPlugin.MethodLabel.text");
    private final String CLASSES_LABEL = PluginUtils.getResources().getString("ReclassRasterPlugin.ClassesLabel.text");
    private final String STDEV_LABEL = PluginUtils.getResources().getString("ReclassRasterPlugin.StDevClassesLabel.text");
    private final String INTERVAL_LABEL = PluginUtils.getResources().getString("ReclassRasterPlugin.IntervalLabel.text");
    private final String FROM_LABEL = PluginUtils.getResources().getString("KlemGUI.From.label");
    private final String TO_LABEL = PluginUtils.getResources().getString("KlemGUI.To.label");
    private final String NEW_LABEL = PluginUtils.getResources().getString("KlemGUI.NewValue.label");

    private boolean rasterChanged = false;
    private String[] methods;

    private final LayerablesList layerablesList;

}
