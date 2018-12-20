package com.geomaticaeambiente.klemgui.plugin.rastertools;

import java.awt.Cursor;

import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PersonalChartHistogram;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.rastertools.HistogramCalculator;
import com.geomaticaeambiente.openjump.klem.rastertools.HistogramCalculator.Histogram;
import com.geomaticaeambiente.openjump.klem.rastertools.classifiers.Autoscale;
import com.geomaticaeambiente.openjump.klem.rastertools.classifiers.GivenIntervals;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.ThreadedBasePlugIn;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import com.vividsolutions.jump.workbench.ui.task.TaskMonitorManager;

import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

/**
 *
 * @author deluca
 */
public class RasterHistogramPlugIn extends AbstractInputKlemPlugin{

    public RasterHistogramPlugIn(PlugInContext context, InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }
    

    @Override
    public InitialData setInitialData() {
        InitialData initialData = new InitialData();
        
        // Input
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(IN_RAS),
                PluginUtils.getRasterImageLayers(layerablesList.getLayerables()),
                GUIUtils.INPUT);
        
        String[] classModel = {
            AUTO, UNIQUE_VALS, GIVEN_INTERVAL
        };
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(CLASSALGO_LABEL), classModel, GUIUtils.INPUT);
        
        // Interval
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(INTERVAL_LABEL), "", GUIUtils.INPUT);
        
        // Base
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(BASE_LABEL), "10", GUIUtils.INPUT);
        
        return initialData;
        
    }

    @Override
    public ComponentsTreeMap setComponentsActions(ComponentsTreeMap personalTreeMap) {
 
        
        return personalTreeMap;
    }

    @Override
    public JPanel buildPluginPanel(final ComponentsTreeMap componentsWithActions) {
        if(this.mainPanel != null) {
            return this.mainPanel;
        }
        this.mainPanel = new MainPanel(super.getInitialDialog(), componentsWithActions, false, false, true,
                PluginUtils.getResources().getString("MainPanel.ExecuteButton.text"), layerablesList) {

            public void rightButton() {

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
                                                            "RasterHistogram.PlugInName.label"));
                                            // monitor.allowCancellationRequests();

                                            reportNothingToUndoYet(context);
                                            try {

                                                // input values
                                                final String inRaster = GUIUtils
                                                        .getStringValue(componentsWithActions
                                                                .getComponent(
                                                                        "00",
                                                                        GUIUtils.INPUT,
                                                                        1));

                                                final JComboBox jComboBox_Algo = (JComboBox) componentsWithActions
                                                        .getComponent("01",
                                                                GUIUtils.INPUT,
                                                                1);
                                                final String selectedAlgo = jComboBox_Algo
                                                        .getSelectedItem()
                                                        .toString();

                                                final String interval = GUIUtils
                                                        .getStringValue(componentsWithActions
                                                                .getComponent(
                                                                        "02",
                                                                        GUIUtils.INPUT,
                                                                        1));
                                                final String base = GUIUtils
                                                        .getStringValue(componentsWithActions
                                                                .getComponent(
                                                                        "03",
                                                                        GUIUtils.INPUT,
                                                                        1));

                                                checksValues(inRaster,
                                                        selectedAlgo, interval,
                                                        base);

                                                final DoubleBasicGrid rasterGrid = RasterUtils
                                                        .getDoubleBasicGrid((CustomComboBox.RasterComboBox) componentsWithActions
                                                                .getComponent(
                                                                        "00",
                                                                        GUIUtils.INPUT,
                                                                        1));

                                                final HistogramCalculator rasterHisto = new HistogramCalculator();
                                                Histogram histo = null;

                                                if (selectedAlgo
                                                        .equalsIgnoreCase(AUTO)) {
                                                    histo = rasterHisto
                                                            .calcStatsContinuous(
                                                                    rasterGrid,
                                                                    new Autoscale(
                                                                            rasterGrid));
                                                } else if (selectedAlgo
                                                        .equalsIgnoreCase(UNIQUE_VALS)) {
                                                    histo = rasterHisto
                                                            .calcStatsUnique(rasterGrid);
                                                } else if (selectedAlgo
                                                        .equalsIgnoreCase(GIVEN_INTERVAL)) {

                                                    final double intervalVal = Double
                                                            .parseDouble(interval);
                                                    final double baseVal = Double
                                                            .parseDouble(base);

                                                    histo = rasterHisto
                                                            .calcStatsContinuous(
                                                                    rasterGrid,
                                                                    new GivenIntervals(
                                                                            rasterGrid,
                                                                            intervalVal,
                                                                            baseVal));
                                                }

                                                if (histo == null) {
                                                    return;
                                                }

                                                final JTabbedPane mainTabelPane = getInitialDialog()
                                                        .getTabbedPane();

                                                final PersonalChartHistogram persChartHisto = new PersonalChartHistogram();

                                                mainTabelPane
                                                        .setComponentAt(
                                                                1,
                                                                persChartHisto
                                                                        .buildHistogramPanel(
                                                                                getInitialDialog()
                                                                                        .getTabbedPane(),
                                                                                histo));
                                                mainTabelPane.setEnabledAt(1,
                                                        true);
                                                mainTabelPane
                                                        .setSelectedIndex(1);
                                            } catch (final ThreadDeath td) {
                                                JOptionPane
                                                        .showMessageDialog(
                                                                getInitialDialog()
                                                                        .getTabbedPane(),
                                                                "Processo interrotto",
                                                                PluginUtils.plugInName,
                                                                JOptionPane.INFORMATION_MESSAGE);
                                            } catch (final Exception ex) {
                                                JOptionPane
                                                        .showMessageDialog(
                                                                getInitialDialog()
                                                                        .getTabbedPane(),
                                                                "Processo interrotto",
                                                                PluginUtils.plugInName,
                                                                JOptionPane.INFORMATION_MESSAGE);

                                            }
                                        }
                                    }, context.getWorkbenchContext(),
                                    new TaskMonitorManager())
                            .actionPerformed(null);
                } catch (final Exception ex) {
                    super.getInitialDialog().setCursor(
                            new Cursor(Cursor.DEFAULT_CURSOR));
                    ErrorDialog.show(super.getInitialDialog(),
                            PluginUtils.plugInName, ex.toString(),
                            StringUtil.stackTrace(ex));
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
    
    private void checksValues(String inRas, String algo, String interval, String base) throws Exception{
        
        GUIUtils.checkStringValue(inRas, IN_RAS);
        GUIUtils.checkStringValue(algo, CLASSALGO_LABEL);
        
        if(algo.equals(GIVEN_INTERVAL)) {
            GUIUtils.checkDoublePosValue(interval, INTERVAL_LABEL);
            GUIUtils.checkDoubleValue(base, BASE_LABEL);
        }
        
    }
    
    @Override
    public String toString(){
        return PluginUtils.getResources().getString("RasterHistogram.PlugInName.label");
    }
    
    private final PlugInContext context;
    private MainPanel mainPanel;
    
    private final String IN_RAS = PluginUtils.getResources().getString("KlemGUI.InputRaster.label");
    private final String CLASSALGO_LABEL = PluginUtils.getResources().getString("KlemGUI.ClassAlgo.label");
    private final String AUTO = PluginUtils.getResources().getString("KlemGUI.ClassAlgo.Algo.Auto");    
    private final String UNIQUE_VALS = PluginUtils.getResources().getString("KlemGUI.ClassAlgo.Algo.UniqueVals");
    private final String GIVEN_INTERVAL = PluginUtils.getResources().getString("KlemGUI.ClassAlgo.Algo.GivenInterval");
    private final String INTERVAL_LABEL = PluginUtils.getResources().getString("RasterHistogramPlugIn.IntervalLabel.text");
    private final String BASE_LABEL = PluginUtils.getResources().getString("RasterHistogramPlugIn.BaseLabel.text");
    
    private final LayerablesList layerablesList;
}
