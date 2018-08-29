package com.geomaticaeambiente.klemgui.ui;

import javax.swing.tree.DefaultMutableTreeNode;

import com.geomaticaeambiente.klemgui.plugin.hydrology.CurveNumberPlugIn;
import com.geomaticaeambiente.klemgui.plugin.hydrology.DemFillerPlugIn;
import com.geomaticaeambiente.klemgui.plugin.hydrology.FlowDirectionPlugIn;
import com.geomaticaeambiente.klemgui.plugin.hydrology.HydroDistPlugIn;
import com.geomaticaeambiente.klemgui.plugin.hydrology.HydrologyGroupsPlugin;
import com.geomaticaeambiente.klemgui.plugin.hydrology.HyetographPlugIn;
import com.geomaticaeambiente.klemgui.plugin.hydrology.RoutingTimePlugIn;
import com.geomaticaeambiente.klemgui.plugin.hydrology.SlopeAspectHillshadePlugIn;
import com.geomaticaeambiente.klemgui.plugin.hydrology.UpslopeAreaPlugIn;
import com.geomaticaeambiente.klemgui.plugin.hydrology.WatershedPlugIn;
import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.HydrographGeomorphologicalPlugin;
import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.HydrographHortonTriangularPlugin;
import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.HydrographNashPlugin;
import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.HydrographSCSPlugin;
import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.HydrographTriangularPlugin;
import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem.HydrographKlemPlugin;
import com.geomaticaeambiente.klemgui.plugin.rastertools.AggregateRastersPlugin;
import com.geomaticaeambiente.klemgui.plugin.rastertools.CutRasterPlugin;
import com.geomaticaeambiente.klemgui.plugin.rastertools.PolygonsVectorizerPlugIn;
import com.geomaticaeambiente.klemgui.plugin.rastertools.RasterCombPlugIn;
import com.geomaticaeambiente.klemgui.plugin.rastertools.RasterConversionsPlugIn;
import com.geomaticaeambiente.klemgui.plugin.rastertools.RasterHistogramPlugIn;
import com.geomaticaeambiente.klemgui.plugin.rastertools.RasterizeVectorLayerPlugIn;
import com.geomaticaeambiente.klemgui.plugin.rastertools.ReclassRasterPlugin;
import com.geomaticaeambiente.klemgui.plugin.setting.SetWorkspacePlugin;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 * Class for creating the tree structure that will contain the plugins of the
 * extension. The method getTreeNodes() creates the elements for the tree
 * structure. For each leaf node the parameter for DefaultMutableTreeNode is a
 * plugin of the extension. The class also contains a number of String static
 * variables which are used by the plugins in getString() method. This string
 * will be displayed on leaf node name.
 * 
 * @author Geomatica
 */
public class PersonalTreeNode {

    // only for text without OJ
    public PersonalTreeNode() {

    }

    public PersonalTreeNode(PlugInContext context, InitialDialog initialDialog,
            LayerablesList rasterLayersList) {
        this.context = context;
        this.initialDialog = initialDialog;
        layerablesList = rasterLayersList;
    }

    /**
     * Method for creating the the elements for the tree structure. The
     * DefaultMutableTreeNode object of a leaf node contains an extension's
     * plugin as object.
     * 
     * @return The tree node structure.
     */
    public DefaultMutableTreeNode getTreeNodes() {

        final DefaultMutableTreeNode treeNode1 = new DefaultMutableTreeNode(
                PluginUtils.getResources().getString("KlemGUI.TreeName.label"));
        DefaultMutableTreeNode treeNode2 = new DefaultMutableTreeNode(
                PluginUtils.getResources().getString(
                        "KlemGUI.TreeSettingTools.label"));
        DefaultMutableTreeNode treeNode3 = new DefaultMutableTreeNode(
                new SetWorkspacePlugin(context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        // Deactivated menu for optionPlugIn
        // treeNode3 = new DefaultMutableTreeNode(new OptionPlugIn(context,
        // initialDialog, layerablesList));
        // treeNode2.add(treeNode3);
        treeNode1.add(treeNode2);

        treeNode2 = new DefaultMutableTreeNode(PluginUtils.getResources()
                .getString("KlemGUI.TreeRasterTools.label"));

        treeNode3 = new DefaultMutableTreeNode(new RasterHistogramPlugIn(
                context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new DefaultMutableTreeNode(new CutRasterPlugin(context,
                initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new DefaultMutableTreeNode(new AggregateRastersPlugin(
                context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new DefaultMutableTreeNode(new ReclassRasterPlugin(context,
                initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new DefaultMutableTreeNode(new RasterizeVectorLayerPlugIn(
                context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new DefaultMutableTreeNode(new RasterCombPlugIn(context,
                initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new DefaultMutableTreeNode(new RasterConversionsPlugIn(
                context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new DefaultMutableTreeNode(new PolygonsVectorizerPlugIn(
                context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);

        treeNode1.add(treeNode2);

        treeNode2 = new DefaultMutableTreeNode(PluginUtils.getResources()
                .getString("KlemGUI.HydrologyTools.label"));
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new DemFillerPlugIn(context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new SlopeAspectHillshadePlugIn(context, initialDialog,
                        layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new FlowDirectionPlugIn(context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new UpslopeAreaPlugIn(context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new HydroDistPlugIn(context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new WatershedPlugIn(context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new RoutingTimePlugIn(context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new HydrologyGroupsPlugin(context, initialDialog,
                        layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new CurveNumberPlugIn(context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new HyetographPlugIn(context, initialDialog, layerablesList));
        treeNode2.add(treeNode3);
        treeNode1.add(treeNode2);

        final DefaultMutableTreeNode treeNode4 = new javax.swing.tree.DefaultMutableTreeNode(
                PluginUtils.getResources().getString(
                        "KlemGUI.TreeHydrographName.label"));
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new HydrographSCSPlugin(context, initialDialog, layerablesList));
        treeNode4.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new HydrographTriangularPlugin(context, initialDialog,
                        layerablesList));
        treeNode4.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new HydrographHortonTriangularPlugin(context, initialDialog,
                        layerablesList));
        treeNode4.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new HydrographNashPlugin(context, initialDialog, layerablesList));
        treeNode4.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new HydrographGeomorphologicalPlugin(context, initialDialog,
                        layerablesList));
        treeNode4.add(treeNode3);
        treeNode3 = new javax.swing.tree.DefaultMutableTreeNode(
                new HydrographKlemPlugin(context, initialDialog, layerablesList));
        treeNode4.add(treeNode3);
        treeNode2.add(treeNode4);

        return treeNode1;
    }

    public static final String SCS_HYDROGRAPH = java.util.ResourceBundle
            .getBundle("com/geomaticaeambiente/klemgui/resources/Bundle")
            .getString("HyetographPlugIn.Scs");
    public static final String KINEMATIC_HYDROGRAPH = java.util.ResourceBundle
            .getBundle("com/geomaticaeambiente/klemgui/resources/Bundle")
            .getString("HyetographPlugIn.KinematicLocalExcessModel");

    private PlugInContext context;
    private InitialDialog initialDialog;
    private LayerablesList layerablesList;
}
