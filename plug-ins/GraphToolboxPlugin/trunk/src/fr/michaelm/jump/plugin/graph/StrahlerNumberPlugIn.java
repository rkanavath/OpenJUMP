package fr.michaelm.jump.plugin.graph;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jump.feature.*;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.model.Category;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.ThreadedBasePlugIn;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.HTMLFrame;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;
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

    private static final String STRAHLER_NUMBER   = "StrahlerNb";

    Layer layer;
    boolean orientedGraph;

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
        final JComboBox jcb_layer = dialog.addLayerComboBox(
                LAYER, context.getCandidateLayer(0), null, context.getLayerManager());

        //final JCheckBox jcb_oriented_graph = dialog.addCheckBox(ORIENTED_GRAPH, false, ORIENTED_GRAPH_TOOLTIP);
        //final JCheckBox jcb_use_attribute  = dialog.addCheckBox(USE_ATTRIBUTE, false, USE_ATTRIBUTE_TOOLTIP);

        GUIUtil.centreOnWindow(dialog);
        dialog.setPreferredSize(new Dimension(400,480));
        dialog.setVisible(true);
        if (dialog.wasOKPressed()) {
            layer = dialog.getLayer(LAYER);
            //orientedGraph = dialog.getBoolean(ORIENTED_GRAPH);
            return true;
        }
        else return false;
    }

    public void run(TaskMonitor monitor, PlugInContext context) throws Exception {
        monitor.allowCancellationRequests();
        monitor.report(GRAPH_COMPUTATION + "...");

        FeatureCollection sourceFC = layer.getFeatureCollectionWrapper();

        // Creates the schema for the output dataset (nodes)
        FeatureSchema newSchema = (FeatureSchema)sourceFC.getFeatureSchema().clone();
        newSchema.addAttribute(STRAHLER_NUMBER, AttributeType.INTEGER);
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
        DirectedWeightedMultigraph<INode,FeatureAsEdge> graph = (DirectedWeightedMultigraph)GraphFactory
                .createDirectedGraph(resultFC.getFeatures(), false);
        HTMLFrame htmlFrame = context.getOutputFrame();
        htmlFrame.createNewDocument();
        Map<Integer,Set<Integer>> ancestorMap = new HashMap<Integer,Set<Integer>>();
        for (FeatureAsEdge arc : graph.edgeSet()) {
            if (arc.getAttribute(STRAHLER_NUMBER) != null) continue;
            computeStreamOrder(graph, arc, ancestorMap);
        }
        context.getLayerManager().addLayer(StandardCategoryNames.RESULT, layer.getName()+"-strahler",resultFC);
    }

    private void computeStreamOrder(DirectedWeightedMultigraph<INode,FeatureAsEdge> graph,
                                    FeatureAsEdge arc, Map<Integer,Set<Integer>> ancestorMap) {
        Set<Integer> ancestors = ancestorMap.get(arc.getID());
        if (ancestors == null) {
            ancestors = new HashSet<Integer>();
            ancestorMap.put(arc.getID(), ancestors);
        }
        int maxOrder = 0;
        int occ = 0;
        for (FeatureAsEdge upstream : graph.incomingEdgesOf(graph.getEdgeSource(arc))) {
            Object att = upstream.getAttribute(STRAHLER_NUMBER);
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
        if (maxOrder == 0) arc.setAttribute(STRAHLER_NUMBER, 1);
        else {
            arc.setAttribute(STRAHLER_NUMBER, occ>1?maxOrder+1:maxOrder);
        }
        Set<FeatureAsEdge> downStreams = graph.outgoingEdgesOf(graph.getEdgeTarget(arc));
        // Before downstream recursion, mark stream origins in case of anastomosis
        //for (FeatureAsEdge downStream : downStreams) {
        //    if (downStreams.size() > 1 || arc.getAttribute(STREAM_ORIGIN) != null) {
        //        Object previousOrigin = arc.getAttribute(STREAM_ORIGIN);
        //        downStream.setAttribute(STREAM_ORIGIN, previousOrigin==null?arc.getID() : previousOrigin);
        //    }
        //}
        for (FeatureAsEdge downStream : downStreams) {
            // In case of anastomosis, compute the downstream edge only once
            if (downStream.getAttribute(STRAHLER_NUMBER) == null) {
                computeStreamOrder(graph, downStream, ancestorMap);
            }
        }
    }

}
