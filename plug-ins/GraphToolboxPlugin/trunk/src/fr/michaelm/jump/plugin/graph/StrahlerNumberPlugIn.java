package fr.michaelm.jump.plugin.graph;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jump.feature.*;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.util.CollectionUtil;
import com.vividsolutions.jump.workbench.model.Category;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.model.UndoableCommand;
import com.vividsolutions.jump.workbench.plugin.*;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.HTMLFrame;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;
import com.vividsolutions.jump.workbench.ui.renderer.style.*;
import com.vividsolutions.jump.workbench.ui.style.StylePanel;
import de.latlon.deejump.plugin.style.DeeChangeStylesPlugIn;
import fr.michaelm.jump.feature.jgrapht.FeatureAsEdge;
import fr.michaelm.jump.feature.jgrapht.GraphFactory;
import fr.michaelm.jump.feature.jgrapht.INode;
import org.jgrapht.DirectedGraph;
import org.jgrapht.Graph;
import org.jgrapht.Graphs;
import org.jgrapht.UndirectedGraph;
import org.jgrapht.alg.ConnectivityInspector;
import org.jgrapht.event.EdgeTraversalEvent;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DirectedWeightedMultigraph;
import org.jgrapht.graph.DirectedWeightedPseudograph;
import org.jgrapht.graph.WeightedPseudograph;
import org.jgrapht.traverse.BreadthFirstIterator;

import javax.swing.*;
import java.awt.*;
import java.util.*;

/**
 * Compute <a href="http://en.wikipedia.org/wiki/Strahler_number">Strahler Numbers</a>
 * on a graph edges.
 */
public class StrahlerNumberPlugIn extends ThreadedBasePlugIn {

    private static String LAYER;

    private static String GRAPH;
    private static String STRAHLER_NUMBERS;
    private static String GRAPH_COMPUTATION;

    private static final String STREAM_ORDER    = "StreamOrder";
    //private static final String STRAHLER_NUMBER = "StrahlerNb";

    Layer layer;

    public String getName() {return "Graph nodes PlugIn";}

    public void initialize(final PlugInContext context) throws Exception {

        GRAPH                   = I18NPlug.getI18N("Graph");
        GRAPH_COMPUTATION       = I18NPlug.getI18N("Graph-computation");
        STRAHLER_NUMBERS        = I18NPlug.getI18N("StrahlerNumberPlugIn.strahler-numbers");

        context.getFeatureInstaller().addMainMenuItem(
                this, new String[]{MenuNames.PLUGINS, GRAPH},
                STRAHLER_NUMBERS + "...",
                false, null, new MultiEnableCheck()
                        .add(context.getCheckFactory().createTaskWindowMustBeActiveCheck())
                        .add(context.getCheckFactory().createAtLeastNLayersMustExistCheck(1)));
    }

    @Override
    public boolean execute(PlugInContext context) throws Exception {
        final MultiInputDialog dialog = new MultiInputDialog(
                context.getWorkbenchFrame(), STRAHLER_NUMBERS, true);
        dialog.setSideBarImage(new ImageIcon(this.getClass().getResource("StrahlerNumber.png")));
        dialog.setSideBarDescription(I18NPlug.getI18N("StrahlerNumberPlugIn.description"));
        dialog.addLayerComboBox(LAYER, context.getCandidateLayer(0), null, context.getLayerManager());

        GUIUtil.centreOnWindow(dialog);
        dialog.setPreferredSize(new Dimension(400,480));
        dialog.setVisible(true);
        if (dialog.wasOKPressed()) {
            layer = dialog.getLayer(LAYER);
            return true;
        }
        else return false;
    }

    public void run(TaskMonitor monitor, PlugInContext context) throws Exception {
        monitor.allowCancellationRequests();
        monitor.report(GRAPH_COMPUTATION + "...");

        FeatureCollection sourceFC = layer.getFeatureCollectionWrapper();

        // Creates the schema for the output dataset (nodes)
        final FeatureSchema newSchema = (FeatureSchema)sourceFC.getFeatureSchema().clone();
        newSchema.addAttribute(STREAM_ORDER, AttributeType.INTEGER);
        //newSchema.addAttribute(STRAHLER_NUMBER, AttributeType.INTEGER);
        FeatureCollection resultFC = new FeatureDataset(newSchema);
        for (Object o : sourceFC.getFeatures()) {
            Feature f = (Feature)o;
            Feature bf = new BasicFeature(newSchema);
            for (int i = 0 ; i < f.getSchema().getAttributeCount() ; i++) {
                bf.setAttribute(i, f.getAttribute(i));
            }
            bf.setGeometry((Geometry)f.getGeometry().clone());
            resultFC.add(bf);
        }
        DirectedWeightedPseudograph<INode,FeatureAsEdge> graph = (DirectedWeightedPseudograph)GraphFactory
                .createDirectedPseudograph(resultFC.getFeatures(), false);
        //HTMLFrame htmlFrame = context.getOutputFrame();
        //htmlFrame.createNewDocument();
        Map<Integer,Set<Integer>> ancestorMap = new HashMap<Integer,Set<Integer>>();
        for (FeatureAsEdge arc : graph.edgeSet()) {
            if (arc.getAttribute(STREAM_ORDER) != null) continue;
            computeStreamOrder(graph, arc, ancestorMap);
        }

        context.getLayerManager().addLayer(StandardCategoryNames.RESULT, layer.getName()+"-strahler",resultFC);
        Layer resultLayer = context.getLayerManager().getLayer(layer.getName() + "-strahler");
        // Styling
        layer.setVisible(false);
        resultLayer.getBasicStyle().setEnabled(false);
        resultLayer.addStyle(getColorThemingStyle());
    }

    private void computeStreamOrder(DirectedWeightedPseudograph<INode,FeatureAsEdge> graph,
                                    FeatureAsEdge arc, Map<Integer,Set<Integer>> ancestorMap) {
        Set<Integer> ancestors = ancestorMap.get(arc.getID());
        if (ancestors == null) {
            ancestors = new HashSet<Integer>();
            ancestorMap.put(arc.getID(), ancestors);
        }
        int maxOrder = 0;
        int occ = 0;
        for (FeatureAsEdge upstream : graph.incomingEdgesOf(graph.getEdgeSource(arc))) {
            Object att = upstream.getAttribute(STREAM_ORDER);
            if (att == null) return;
            int upstreamOrder = (Integer)att;
            if (upstreamOrder > maxOrder) {
                maxOrder = upstreamOrder;
                occ = 1;
                ancestors.addAll(ancestorMap.get(upstream.getID()));
                ancestors.add(upstream.getID());
            }
            else if (upstreamOrder == maxOrder) {
                int card = ancestors.size();
                Set<Integer> newSet = ancestorMap.get(upstream.getID());
                ancestors.addAll(newSet);
                if (ancestors.size() == card + newSet.size()) {
                    occ++;
                }
            }
            else;
        }
        if (maxOrder == 0) arc.setAttribute(STREAM_ORDER, 1);
        else {
            arc.setAttribute(STREAM_ORDER, occ>1?maxOrder+1:maxOrder);
        }
        Set<FeatureAsEdge> downStreams = graph.outgoingEdgesOf(graph.getEdgeTarget(arc));

        for (FeatureAsEdge downStream : downStreams) {
            // In case of anastomosis, compute the downstream edge only once
            if (downStream.getAttribute(STREAM_ORDER) == null) {
                computeStreamOrder(graph, downStream, ancestorMap);
            }
        }
    }

    ColorThemingStyle getColorThemingStyle() {
        BasicStyle dbs = new BasicStyle();   dbs.setLineColor(new Color(255,0,0));      dbs.setLineWidth(2);
        BasicStyle bs1  = new BasicStyle();  bs1.setLineColor(new Color(120,240,255));  bs1.setLineWidth(1);
        BasicStyle bs2  = new BasicStyle();  bs2.setLineColor(new Color( 90,180,255));  bs2.setLineWidth(1);
        BasicStyle bs3  = new BasicStyle();  bs3.setLineColor(new Color( 60,120,255));  bs3.setLineWidth(2);
        BasicStyle bs4  = new BasicStyle();  bs4.setLineColor(new Color( 30, 60,255));  bs4.setLineWidth(3);
        BasicStyle bs5  = new BasicStyle();  bs5.setLineColor(new Color(  0,  0,255));  bs5.setLineWidth(4);
        BasicStyle bs6  = new BasicStyle();  bs6.setLineColor(new Color( 60,  0,255));  bs6.setLineWidth(5);
        BasicStyle bs7  = new BasicStyle();  bs7.setLineColor(new Color( 90,  0,255));  bs7.setLineWidth(7);
        BasicStyle bs8  = new BasicStyle();  bs8.setLineColor(new Color(120,  0,255));  bs8.setLineWidth(9);
        BasicStyle bs9  = new BasicStyle();  bs9.setLineColor(new Color(150,  0,255));  bs9.setLineWidth(11);
        BasicStyle bs10 = new BasicStyle(); bs10.setLineColor(new Color(180,  0,255)); bs10.setLineWidth(13);
        BasicStyle bs11 = new BasicStyle(); bs11.setLineColor(new Color(210,  0,255)); bs11.setLineWidth(15);
        BasicStyle bs12 = new BasicStyle(); bs12.setLineColor(new Color(240,  0,255)); bs12.setLineWidth(18);
        ColorThemingStyle cts =  new ColorThemingStyle(STREAM_ORDER,
                CollectionUtil.createMap(new Object[]{
                    1, bs1,
                    2, bs2,
                    3, bs3,
                    4, bs4,
                    5, bs5,
                    6, bs6,
                    7, bs7,
                    8, bs8,
                    9, bs9,
                    10, bs10,
                    11, bs11,
                    12, bs12}), dbs);
        cts.setEnabled(true);
        return cts;
    }

}
