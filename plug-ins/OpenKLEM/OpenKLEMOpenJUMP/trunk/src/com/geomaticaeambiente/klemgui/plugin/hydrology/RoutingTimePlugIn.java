package com.geomaticaeambiente.klemgui.plugin.hydrology;

import java.io.File;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import com.geomaticaeambiente.klemgui.ui.CustomComboBox;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.GeometryUtils;
import com.geomaticaeambiente.klemgui.utils.HydroUtils;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.routing.RoutingTimeCalculator;
import com.geomaticaeambiente.openjump.klem.routing.RoutingTimeParameters;
import com.geomaticaeambiente.openjump.klem.units.Area;
import com.geomaticaeambiente.openjump.klem.units.Length;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import com.vividsolutions.jump.workbench.ui.task.TaskMonitorManager;

/**
 *
 * @author Geomatica
 */
public class RoutingTimePlugIn extends AbstractInputKlemPlugin {

    public RoutingTimePlugIn(PlugInContext context,
            InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

        //        rasterImageLayers = PluginUtils.getRasterImageLayers(context);

        final InitialData initialData = new InitialData();

        //input data
        initialData
                .setParam_Label_TextBox(GUIUtils.setGUILabel(DEM_LABEL),
                        PluginUtils.getRasterImageLayers(layerablesList
                                .getLayerables()), GUIUtils.INPUT);//dem
        initialData.setParam_Label_TextBox(
                GUIUtils.setGUILabel(PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Bluelines.label")),
                PluginUtils.getLayers(layerablesList.getLayerables()),
                GUIUtils.INPUT); // Bluelines
        initialData.setParam_Label_TextBox_Label(
                GUIUtils.setGUILabel(SLOPE_VELOCITY_LABEL), "0.02", M_S,
                GUIUtils.INPUT); //slope velocity
        initialData.setParam_Label_TextBox_Label(
                GUIUtils.setGUILabel(CHANNEL_VELOCITY_LABEL), "2.0", M_S,
                GUIUtils.INPUT); //channel velocity
        initialData.setParam_Label_TextBox_Label(
                GUIUtils.setGUILabel(MIN_THRESHOLD_LABEL), "0.01", KMQ,
                GUIUtils.INPUT); //min threshold 
        initialData.setParam_Label_TextBox_Label(
                GUIUtils.setGUILabel(MAX_THRESHOLD_LABEL), "100000000", KMQ,
                GUIUtils.INPUT); //max threshold
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(EXPONENT), "0",
                GUIUtils.INPUT); //exponent

        //output data
        initialData.setParam_Label_TextBox_Button(GUIUtils
                .setGUILabel(ROUTINGTIME_LABEL), "", new ActionObject(""),
                GUIUtils.OUTPUT); //output raster           

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

    public void routingTimeCommand(final ComponentsTreeMap componentsWithActions)
            throws Exception {
        //get input raster names
        final String demRasterSelected = GUIUtils
                .getStringValue(componentsWithActions.getComponent("00",
                        GUIUtils.INPUT, 1));
        final Layer bluelinesLayer = PluginUtils
                .getLayerSelected((CustomComboBox.LayerComboBox) componentsWithActions
                        .getComponent("01", GUIUtils.INPUT, 1));

        //get input values
        final double slopeVelValue = GUIUtils
                .getDoubleValue(componentsWithActions.getComponent("02",
                        GUIUtils.INPUT, 1));
        final double channelVelValue = GUIUtils
                .getDoubleValue(componentsWithActions.getComponent("03",
                        GUIUtils.INPUT, 1));
        final double minThresholdValue = GUIUtils
                .getDoubleValue(componentsWithActions.getComponent("04",
                        GUIUtils.INPUT, 1));
        final double maxThresholdValue = GUIUtils
                .getDoubleValue(componentsWithActions.getComponent("05",
                        GUIUtils.INPUT, 1));
        final double exponent = GUIUtils.getDoubleValue(componentsWithActions
                .getComponent("06", GUIUtils.INPUT, 1));

        //get output raster name
        final String outRasterName = GUIUtils
                .getStringValue(componentsWithActions.getComponent("00",
                        GUIUtils.OUTPUT, 1));

        //checks
        GUIUtils.checkStringValue(demRasterSelected, DEM_LABEL);
        GUIUtils.checkFileValue(outRasterName, GUIUtils.getOutputRasterString());
        //convert string values in correct objects
        //get input dem                    
        final DoubleBasicGrid demGrid = RasterUtils
                .getDoubleBasicGrid((CustomComboBox.RasterComboBox) componentsWithActions
                        .getComponent("00", GUIUtils.INPUT, 1));

        //get Parameters
        final RoutingTimeParameters routingTimeParameters = new RoutingTimeParameters(
                HydroUtils.calculateSpeed(slopeVelValue),
                HydroUtils.calculateSpeed(channelVelValue), new Area(
                        minThresholdValue, Length.LengthUnit.km), new Area(
                        maxThresholdValue, Length.LengthUnit.km), exponent);

        LineString[] bluelines = null;
        if (bluelinesLayer != null) {
            bluelines = GeometryUtils.getLineStringsFromFeatures(bluelinesLayer
                    .getFeatureCollectionWrapper());
        }

        //Routing time
        final RoutingTimeCalculator routingTimeCalculator = new RoutingTimeCalculator();
        final DoubleBasicGrid routingTimeGrid = routingTimeCalculator.calcD8(
                demGrid, bluelines, 100d, routingTimeParameters);

        //Create the output file and display on OJ 
        //Save grid as tiff
        RasterUtils.saveOutputRasterAsTiff(routingTimeGrid, new File(
                outRasterName));
        //Display raster on OJ from file                
        RasterUtils.displayRasterFileOnOJ(context.getWorkbenchContext(),
                new File(outRasterName), null);

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

                    AbstractPlugIn.toActionListener(
                            new AbstractThreadedUiPlugIn() {
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
                                    monitor.report(PluginUtils
                                            .getResources()
                                            .getString(
                                                    "OpenKlem.executing-process"));
                                    reportNothingToUndoYet(context);
                                    monitor.allowCancellationRequests();
                                    routingTimeCommand(componentsWithActions);

                                }
                            }, context.getWorkbenchContext(),
                            new TaskMonitorManager()).actionPerformed(null);

                    //get input raster names
                    /*      final String demRasterSelected = GUIUtils
                                  .getStringValue(componentsWithActions.getComponent(
                                          "00", GUIUtils.INPUT, 1));
                          final Layer bluelinesLayer = PluginUtils
                                  .getLayerSelected((CustomComboBox.LayerComboBox) componentsWithActions
                                          .getComponent("01", GUIUtils.INPUT, 1));

                          //get input values
                          final double slopeVelValue = GUIUtils
                                  .getDoubleValue(componentsWithActions.getComponent(
                                          "02", GUIUtils.INPUT, 1));
                          final double channelVelValue = GUIUtils
                                  .getDoubleValue(componentsWithActions.getComponent(
                                          "03", GUIUtils.INPUT, 1));
                          final double minThresholdValue = GUIUtils
                                  .getDoubleValue(componentsWithActions.getComponent(
                                          "04", GUIUtils.INPUT, 1));
                          final double maxThresholdValue = GUIUtils
                                  .getDoubleValue(componentsWithActions.getComponent(
                                          "05", GUIUtils.INPUT, 1));
                          final double exponent = GUIUtils
                                  .getDoubleValue(componentsWithActions.getComponent(
                                          "06", GUIUtils.INPUT, 1));

                          //get output raster name
                          final String outRasterName = GUIUtils
                                  .getStringValue(componentsWithActions.getComponent(
                                          "00", GUIUtils.OUTPUT, 1));

                          //checks
                          GUIUtils.checkStringValue(demRasterSelected, DEM_LABEL);
                          GUIUtils.checkFileValue(outRasterName,
                                  GUIUtils.getOutputRasterString());
                          //convert string values in correct objects
                          //get input dem                    
                          final DoubleBasicGrid demGrid = RasterUtils
                                  .getDoubleBasicGrid((CustomComboBox.RasterComboBox) componentsWithActions
                                          .getComponent("00", GUIUtils.INPUT, 1));

                          //get Parameters
                          final RoutingTimeParameters routingTimeParameters = new RoutingTimeParameters(
                                  HydroUtils.calculateSpeed(slopeVelValue),
                                  HydroUtils.calculateSpeed(channelVelValue),
                                  new Area(minThresholdValue, Length.LengthUnit.km),
                                  new Area(maxThresholdValue, Length.LengthUnit.km),
                                  exponent);

                          LineString[] bluelines = null;
                          if (bluelinesLayer != null) {
                              bluelines = GeometryUtils
                                      .getLineStringsFromFeatures(bluelinesLayer
                                              .getFeatureCollectionWrapper());
                          }

                          //Routing time
                          final RoutingTimeCalculator routingTimeCalculator = new RoutingTimeCalculator();
                          final DoubleBasicGrid routingTimeGrid = routingTimeCalculator
                                  .calcD8(demGrid, bluelines, 100d,
                                          routingTimeParameters);

                          //Create the output file and display on OJ 
                          //Save grid as tiff
                          RasterUtils.saveOutputRasterAsTiff(routingTimeGrid,
                                  new File(outRasterName));
                          //Display raster on OJ from file                
                          RasterUtils.displayRasterFileOnOJ(context
                                  .getWorkbenchContext(), new File(outRasterName),
                                  null);

                          JOptionPane.showMessageDialog(
                                  super.getInitialDialog(),
                                  PluginUtils.getResources().getString(
                                          "SetWorkspacePlugin.Done.message"),
                                  PluginUtils.plugInName,
                                  JOptionPane.INFORMATION_MESSAGE);

                      } catch (final WarningException ex) {
                          JOptionPane.showMessageDialog(super.getInitialDialog(),
                                  ex.getMessage(), PluginUtils.plugInName,
                                  JOptionPane.WARNING_MESSAGE);*/
                } catch (final Exception ex) {
                    ErrorDialog.show(super.getInitialDialog(),
                            PluginUtils.plugInName, ex.toString(),
                            StringUtil.stackTrace(ex));
                }
            };

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
                "RoutingTimePlugIn.RoutingTime.label");
    }

    private MainPanel mainPanel;
    private final PlugInContext context;
    private final String DEM_LABEL = PluginUtils.getResources().getString(
            "KlemGUI.InputFilledDem.label");
    //private final String UPSLOPE_LABEL = PluginUtils.getResources().getString("KlemGUI.InputUpslope.label");
    private final String SLOPE_VELOCITY_LABEL = PluginUtils.getResources()
            .getString("RoutingTimePlugIn.SlopeVelociti.label");
    private final String CHANNEL_VELOCITY_LABEL = PluginUtils.getResources()
            .getString("RoutingTimePlugIn.ChannelVelocity.label");
    private final String MIN_THRESHOLD_LABEL = PluginUtils.getResources()
            .getString("RoutingTimePlugIn.MinThreshold.label");
    private final String MAX_THRESHOLD_LABEL = PluginUtils.getResources()
            .getString("RoutingTimePlugIn.MaxThreshold.label");
    private final String EXPONENT = PluginUtils.getResources().getString(
            "RoutingTimePlugIn.Exponent.label");
    private final String M_S = "[m/s]";
    private final String KMQ = "[km2]";
    private final String ROUTINGTIME_LABEL = PluginUtils.getResources()
            .getString("RoutingTimePlugIn.RoutingTime.label");

    private final LayerablesList layerablesList;

}
