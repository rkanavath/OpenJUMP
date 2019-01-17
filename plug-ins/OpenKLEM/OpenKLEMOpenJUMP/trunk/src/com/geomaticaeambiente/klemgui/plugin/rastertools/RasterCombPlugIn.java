package com.geomaticaeambiente.klemgui.plugin.rastertools;

import java.io.File;
import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.UUID;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.openjump.core.rasterimage.RasterImageLayer;

import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.ui.PersonalRasterCombPanel;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.utils.RasterCombinationComponent;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.openjump.klem.exceptions.NotSpatiallyConsistentGridsException;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel2.DoubleStripeGrid2;
import com.geomaticaeambiente.openjump.klem.rastertools.RasterComb;
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
public class RasterCombPlugIn extends AbstractInputKlemPlugin {

    public RasterCombPlugIn(PlugInContext context, InitialDialog initialDialog,
            LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

        //        rasterImageLayers = PluginUtils.getRasterImageLayers(context);

        final RasterCombinationComponent personalObj = new RasterCombinationComponent(
                super.getInitialDialog());

        final InitialData initialData = new InitialData();
        initialData.setParam_RasterComb(context.getWorkbenchContext(),
                personalObj, GUIUtils.INPUT);

        //set output
        initialData.setParam_Label_TextBox_Button(
                GUIUtils.getOutputRasterLabel(), "", new ActionObject(""),
                GUIUtils.OUTPUT);

        return initialData;
    }

    @Override
    public ComponentsTreeMap setComponentsActions(
            ComponentsTreeMap personalTreeMap) {

        final JTextField outputTextField = (JTextField) personalTreeMap
                .getComponent("00", GUIUtils.OUTPUT, 1);
        final JButton outputButton = (JButton) personalTreeMap.getComponent(
                "00", GUIUtils.OUTPUT, 2);
        outputButton.setIcon(PluginUtils.getFolderIcon());
        outputButton.addActionListener(GUIUtils
                .setSaveRasterTif(outputTextField));

        return personalTreeMap;
    }

    public void rasterCombCommand(final ComponentsTreeMap componentsWithActions)
            throws Exception {

        //get input information
        final boolean[] rasterSelected = GUIUtils
                .getSelectedRasterFromRasterCombo(componentsWithActions
                        .getComponent("00", GUIUtils.INPUT, 0));
        final PersonalRasterCombPanel prc = ((PersonalRasterCombPanel) componentsWithActions
                .getComponent("00", GUIUtils.INPUT, 0));
        final String expression = GUIUtils
                .getExprerssionFromRasterCombo(componentsWithActions
                        .getComponent("00", GUIUtils.INPUT, 0));

        final UUID[] uuids = prc.getSelRastersUUIds();

        //get output raster name
        final String outRasterName = GUIUtils
                .getStringValue(componentsWithActions.getComponent("00",
                        GUIUtils.OUTPUT, 1));

        //check
        checkValues(mainPanel, expression, rasterSelected, outRasterName);

        final LinkedHashMap<String, DoubleStripeGrid2> rastersAndNames_m = new LinkedHashMap<String, DoubleStripeGrid2>();
        for (final RasterImageLayer rasterImageLayer : PluginUtils
                .getRasterImageLayers(layerablesList.getLayerables())) {
            for (final UUID uuid : uuids) {
                if (uuid != null
                        && rasterImageLayer.getUUID().compareTo(uuid) == 0) {
                    rastersAndNames_m.put(rasterImageLayer.getName(),
                            RasterUtils.getDoubleStripeGrid(rasterImageLayer));
                }
            }
            //            rastersAndNames_m.put(rasterImageLayer.getName(), RasterUtils.getDoubleStripeGrid((RasterImageLayer) rasterImageLayer));
        }

        final RasterComb rasterComb = new RasterComb(rastersAndNames_m,
                expression);
        final DoubleBasicGrid outGrid = rasterComb.call();

        //Create the output file and display on OJ
        //Save grid as tiff
        RasterUtils.saveOutputRasterAsTiff(outGrid, new File(outRasterName));
        //Display raster on OJ from the file                
        RasterUtils.displayRasterFileOnOJ(context.getWorkbenchContext(),
                new File(outRasterName), null);

        prc.updateRasterList();

        JOptionPane.showMessageDialog(super.getInitialDialog(), PluginUtils
                .getResources().getString("SetWorkspacePlugin.Done.message"),
                PluginUtils.plugInName, JOptionPane.INFORMATION_MESSAGE);

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
                            // monitor.allowCancellationRequests();
                            reportNothingToUndoYet(context);
                            try {
                                rasterCombCommand(componentsWithActions);
                            } catch (final Exception ex) {
                                Logger.error(getName(), ex);
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
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }

            @Override
            public void centerButton() {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }

        };
        return mainPanel;
    }

    public JPanel buildPluginPanel_old(
            final ComponentsTreeMap componentsWithActions) {
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

                    //get input information
                    final boolean[] rasterSelected = GUIUtils
                            .getSelectedRasterFromRasterCombo(componentsWithActions
                                    .getComponent("00", GUIUtils.INPUT, 0));
                    final PersonalRasterCombPanel prc = ((PersonalRasterCombPanel) componentsWithActions
                            .getComponent("00", GUIUtils.INPUT, 0));
                    final String expression = GUIUtils
                            .getExprerssionFromRasterCombo(componentsWithActions
                                    .getComponent("00", GUIUtils.INPUT, 0));

                    final UUID[] uuids = prc.getSelRastersUUIds();

                    //get output raster name
                    final String outRasterName = GUIUtils
                            .getStringValue(componentsWithActions.getComponent(
                                    "00", GUIUtils.OUTPUT, 1));

                    //check
                    checkValues(this, expression, rasterSelected, outRasterName);

                    final LinkedHashMap<String, DoubleStripeGrid2> rastersAndNames_m = new LinkedHashMap<String, DoubleStripeGrid2>();
                    for (final RasterImageLayer rasterImageLayer : PluginUtils
                            .getRasterImageLayers(layerablesList
                                    .getLayerables())) {
                        for (final UUID uuid : uuids) {
                            if (uuid != null
                                    && rasterImageLayer.getUUID().compareTo(
                                            uuid) == 0) {
                                rastersAndNames_m.put(rasterImageLayer
                                        .getName(), RasterUtils
                                        .getDoubleStripeGrid(rasterImageLayer));
                            }
                        }
                        //                        rastersAndNames_m.put(rasterImageLayer.getName(), RasterUtils.getDoubleStripeGrid((RasterImageLayer) rasterImageLayer));
                    }

                    final RasterComb rasterComb = new RasterComb(
                            rastersAndNames_m, expression);
                    final DoubleBasicGrid outGrid = rasterComb.call();

                    //Create the output file and display on OJ
                    //Save grid as tiff
                    RasterUtils.saveOutputRasterAsTiff(outGrid, new File(
                            outRasterName));
                    //Display raster on OJ from the file                
                    RasterUtils.displayRasterFileOnOJ(context
                            .getWorkbenchContext(), new File(outRasterName),
                            null);

                    prc.updateRasterList();

                    JOptionPane.showMessageDialog(
                            super.getInitialDialog(),
                            PluginUtils.getResources().getString(
                                    "SetWorkspacePlugin.Done.message"),
                            PluginUtils.plugInName,
                            JOptionPane.INFORMATION_MESSAGE);

                } catch (final NotSpatiallyConsistentGridsException ex) {
                    JOptionPane
                            .showMessageDialog(
                                    super.getInitialDialog(),
                                    PluginUtils.getResources().getString(
                                            "SpatiallyInconsistentRasters"),
                                    PluginUtils.plugInName,
                                    JOptionPane.WARNING_MESSAGE);

                } catch (final Exception ex) {
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
        return mainPanel;
    }

    @Override
    public String toString() {
        return PluginUtils.getResources().getString(
                "RasterCombPlugin.PlugInName.label");
    }

    private void checkValues(JComponent component, String expression,
            boolean[] rasterList, String outRaster) throws IOException,
            Exception {

        GUIUtils.checkStringValue(expression, "Expression");

        boolean foundRaster = false;
        for (final boolean element : rasterList) {
            if (element == true) {
                foundRaster = true;
                break;
            }
        }
        if (foundRaster == false) {
            throw new NullPointerException(PluginUtils.getResources()
                    .getString("RasterComboPlugIn.CheckExpression.message"));
        }

        GUIUtils.checkFileValue(outRaster, "raster out");

    }

    private final PlugInContext context;
    private MainPanel mainPanel;
    private final LayerablesList layerablesList;

}
