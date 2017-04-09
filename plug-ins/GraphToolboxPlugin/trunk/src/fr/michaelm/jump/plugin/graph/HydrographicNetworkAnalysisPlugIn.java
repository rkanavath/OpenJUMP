package fr.michaelm.jump.plugin.graph;

import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jump.feature.*;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.ThreadedBasePlugIn;
import com.vividsolutions.jump.workbench.ui.*;
import com.vividsolutions.jump.workbench.ui.renderer.style.ArrowLineStringSegmentStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.RingVertexStyle;
import fr.michaelm.jump.feature.jgrapht.FeatureAsEdge;
import fr.michaelm.jump.feature.jgrapht.GraphFactory;
import fr.michaelm.jump.feature.jgrapht.INode;
import org.jgrapht.DirectedGraph;
import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jgrapht.alg.DijkstraShortestPath;
import org.jgrapht.alg.cycle.HawickJamesSimpleCycles;
import org.jgrapht.graph.DirectedSubgraph;
import org.jgrapht.graph.EdgeReversedGraph;
import org.jgrapht.traverse.BreadthFirstIterator;
import org.openjump.core.ui.style.decoration.ArrowLineStringMiddlepointStyle;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.Point2D;
import java.util.*;
import java.util.List;

/**
 * Created by UMichael on 06/04/2017.
 */
public class HydrographicNetworkAnalysisPlugIn extends ThreadedBasePlugIn {

    private static String LAYER;
    private static String GRAPH;
    private static String HYDROGRAPHIC_NETWORK_ANALYSIS;
    private static String GRAPH_COMPUTATION;

    private static String DETECT;
    private static String REPAIR;

    private static String FIND_CYCLES;
    private static String FIND_CYCLES_TT;
    private static String FIND_SOURCES;
    private static String FIND_SOURCES_TT;
    private static String FIND_SINKS;
    private static String FIND_SINKS_TT;
    private static String USE_Z;
    private static String USE_Z_TT;
    private static String TOL_Z;
    private static String TOL_Z_TT;

    private static String SOURCE;
    private static String SINK;
    private static String CYCLE;
    private static String UPWARD_EDGE;
    private static String Z_ANOMALY;
    private static String CYCLE_ANOMALY;
    private static String NODE_ANOMALY;

    private Layer layer;
    private boolean detect      = true;
    private boolean repair      = false;
    private boolean findCycles  = true;
    private boolean findSources = true; // we call source a node with indegree = 0 / outdegree > 1
    private boolean findSinks   = true; // we call well a node with outdegree = 0 / indegree > 1
    private boolean useZ        = true; // use z to find inverted edges
    private double tolZ         = 0;

    //public String getName() {return "Hydrographic network anomaly detection";}

    public void initialize(final PlugInContext context) throws Exception {

        LAYER                         = I18NPlug.getI18N("Layer");
        GRAPH                         = I18NPlug.getI18N("Graph");
        HYDROGRAPHIC_NETWORK_ANALYSIS = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn");
        GRAPH_COMPUTATION             = I18NPlug.getI18N("Graph-computation");

        DETECT           = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Detect");
        REPAIR           = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Repair");

        FIND_CYCLES      = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Cycles");
        FIND_CYCLES_TT   = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Cycles-tooltip");
        FIND_SOURCES     = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Sources-with-several-outcoming-edges");
        FIND_SOURCES_TT  = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Sources-with-several-outcoming-edges");
        FIND_SINKS       = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Sinks-with-several-incoming-edges");
        FIND_SINKS_TT    = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Sinks-with-several-incoming-edges");
        USE_Z            = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Use-z");
        USE_Z_TT         = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Use-z-to-find-inverted-edges");
        TOL_Z            = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Tolerance-for-Z");
        TOL_Z_TT         = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Altitude-differences-less-than-the-tolerance-are-ignored");

        SOURCE           = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Source");
        SINK             = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Sink");
        CYCLE            = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Cycle");
        UPWARD_EDGE      = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Upward-edge");
        Z_ANOMALY        = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.z-anomaly");
        CYCLE_ANOMALY    = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.cycle-anomaly");
        NODE_ANOMALY     = I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.node-anomaly");

        context.getFeatureInstaller().addMainMenuPlugin(
                this, new String[]{MenuNames.PLUGINS, GRAPH},
                HYDROGRAPHIC_NETWORK_ANALYSIS + "...",
                false, null, new MultiEnableCheck()
                        .add(context.getCheckFactory().createWindowWithAssociatedTaskFrameMustBeActiveCheck())
                        .add(context.getCheckFactory().createAtLeastNLayersMustExistCheck(1)));
    }

    public boolean execute(PlugInContext context) {

        final MultiInputDialog dialog = new MultiInputDialog(
                context.getWorkbenchFrame(), HYDROGRAPHIC_NETWORK_ANALYSIS, true);
        dialog.setSideBarDescription(I18NPlug.getI18N("HydrographicNetworkAnalysisPlugIn.Description"));

        final JComboBox jcb_layer = dialog.addLayerComboBox(
                LAYER, context.getCandidateLayer(0), null, context.getLayerManager());

        final JRadioButton jrb_detect = dialog.addRadioButton(DETECT, "action", detect, DETECT);
        final JRadioButton jrb_repair = dialog.addRadioButton(REPAIR, "action", repair, REPAIR);

        final JCheckBox jcb_cycles = dialog.addCheckBox(FIND_CYCLES, findCycles, FIND_CYCLES_TT);
        final JCheckBox jcb_source = dialog.addCheckBox(FIND_SOURCES, findSources, FIND_SOURCES_TT);
        final JCheckBox jcb_well   = dialog.addCheckBox(FIND_SINKS, findSinks, FIND_SINKS_TT);
        final JCheckBox jcb_use_z  = dialog.addCheckBox(USE_Z, useZ, USE_Z_TT);
        final JTextField jtf_tol_z = dialog.addDoubleField(TOL_Z, tolZ, 12, TOL_Z_TT);


        GUIUtil.centreOnWindow(dialog);
        dialog.setVisible(true);
        if (dialog.wasOKPressed()) {
            layer = dialog.getLayer(LAYER);
            detect      = dialog.getBoolean(DETECT);
            repair      = dialog.getBoolean(REPAIR);
            findCycles  = dialog.getBoolean(FIND_CYCLES);
            findSources = dialog.getBoolean(FIND_SOURCES);
            findSinks   = dialog.getBoolean(FIND_SINKS);
            useZ        = dialog.getBoolean(USE_Z);
            tolZ        = dialog.getDouble(TOL_Z);
            return true;
        }
        else return false;

    }

    public void run(TaskMonitor monitor, PlugInContext context) throws Exception {
        monitor.allowCancellationRequests();
        monitor.report(GRAPH_COMPUTATION + "...");
        FeatureCollection fc = layer.getFeatureCollectionWrapper();
        if (detect && useZ) {
            Layer lyr = context.getLayerManager().addLayer("Result", layer.getName() + "-" + Z_ANOMALY, getInversedEdges(fc));
            setInversionStyle(lyr);
        }

        DirectedGraph<INode,FeatureAsEdge> graph =
                (DirectedGraph<INode,FeatureAsEdge>) GraphFactory.createDirectedPseudograph(fc.getFeatures(), false);
        if (detect && findCycles) {
            Layer lyr = context.getLayerManager().addLayer("Result", layer.getName() + "-" + CYCLE_ANOMALY, getCycles(graph));
            setCycleStyle(lyr);
        }
        if (detect && (findSources || findSinks)) {
            Layer lyr = context.getLayerManager().addLayer("Result", layer.getName() + "-" + NODE_ANOMALY, getSourcesAndWells(graph));
            lyr.getBasicStyle().setFillColor(Color.RED);
            lyr.addStyle(new MyRingVertexStyle());
            lyr.getStyle(MyRingVertexStyle.class).setEnabled(true);
        }
        Set<Integer> set = new HashSet<Integer>();
        if (repair && findCycles) {
            repairCycles(graph, set);
        }
        if (repair && findSources) {
            repairSources(graph, set);
        }
        if (repair && findSinks) {
            repairSinks(graph, set);
        }
        if (repair) {
            EditTransaction transaction = new EditTransaction(new LinkedHashSet<Feature>(),
                    "HydrographicNetworkAnalysis", layer, true, true, context.getLayerViewPanel().getContext());
            for (Feature feature : fc.getFeatures()) {
                if (set.contains(feature.getID())) {
                    transaction.modifyFeatureGeometry(feature, feature.getGeometry().reverse());
                }
            }
            transaction.commit();
        }
    }

    private FeatureCollection getInversedEdges(FeatureCollection fc) {

        FeatureSchema anomalySchema = getAnomalySchema();
        FeatureCollection dataset = new FeatureDataset(anomalySchema);

        for (Feature feature : fc.getFeatures()) {
            Geometry geometry = feature.getGeometry();
            if (geometry.isEmpty()) continue;
            for (int i = 0 ; i < geometry.getNumGeometries() ; i++) {
                Geometry g = geometry.getGeometryN(i);
                if (g instanceof LineString) {
                    double z0 = ((LineString)g).getStartPoint().getCoordinate().z;
                    double z1 = ((LineString)g).getEndPoint().getCoordinate().z;
                    if (Double.isNaN(z0) || Double.isNaN(z1)) continue;
                    if (z0-z1 < -tolZ) {
                        Feature anomaly = new BasicFeature(anomalySchema);
                        anomaly.setGeometry(g);
                        anomaly.setAttribute("type", UPWARD_EDGE);
                        anomaly.setAttribute("comment", "[" + z0 + ";" + z1 + "]");
                        dataset.add(anomaly);
                    }
                }
            }
        }
        return dataset;
    }

    private FeatureCollection getCycles(DirectedGraph<INode,FeatureAsEdge> graph) {
        FeatureSchema anomalySchema = getAnomalySchema();
        FeatureCollection dataset = new FeatureDataset(anomalySchema);

        List<List<INode>> cycles = new HawickJamesSimpleCycles<INode,FeatureAsEdge>(graph).findSimpleCycles();
        for (List<INode> cycle : cycles) {
            Set<FeatureAsEdge> edgeSet = new DirectedSubgraph<INode,FeatureAsEdge>(
                    graph, new HashSet<INode>(cycle), null).edgeSet();
            for (FeatureAsEdge edge : edgeSet) {
                Feature feature = new BasicFeature(anomalySchema);
                feature.setGeometry(edge.getGeometry());
                feature.setAttribute("type", CYCLE);
                dataset.add(feature);
            }
        }
        return dataset;
    }

    private FeatureCollection getSourcesAndWells(DirectedGraph<INode,FeatureAsEdge> graph) {
        FeatureSchema anomalySchema = getAnomalySchema();
        FeatureCollection dataset = new FeatureDataset(anomalySchema);

        for (INode node : graph.vertexSet()) {
            if (findSources && isSource(graph, node)) {
                Feature feature = new BasicFeature(anomalySchema);
                feature.setGeometry(node.getGeometry());
                feature.setAttribute("type", SOURCE);
                dataset.add(feature);
            } else if (findSinks && isWell(graph, node)) {
                Feature feature = new BasicFeature(anomalySchema);
                feature.setGeometry(node.getGeometry());
                feature.setAttribute("type", SINK);
                dataset.add(feature);
            }
        }
        return dataset;
    }

    boolean isSource(DirectedGraph<INode,FeatureAsEdge> graph, INode node) {
        return graph.inDegreeOf(node) == 0 && graph.outDegreeOf(node) > 1;
    }

    boolean isWell(DirectedGraph<INode,FeatureAsEdge> graph, INode node) {
        return graph.outDegreeOf(node) == 0 && graph.inDegreeOf(node) > 1;
    }


    private List<Integer> repairCycles(DirectedGraph<INode,FeatureAsEdge> graph, Set<Integer> set) {
        List<List<INode>> cycles = new HawickJamesSimpleCycles<INode,FeatureAsEdge>(graph).findSimpleCycles();
        for (List<INode> cycle : cycles) {
            Set<FeatureAsEdge> edgeSet = new DirectedSubgraph<INode,FeatureAsEdge>(
                    graph, new HashSet<INode>(cycle), null).edgeSet();
            double max = 0;
            FeatureAsEdge edgeToReverse = null;
            for (FeatureAsEdge edge : edgeSet) {
                double score = evaluateEdgeInversion(graph, edge);
                if (score > max) {
                    max = score;
                    edgeToReverse = edge;
                }
            }
            if (edgeToReverse != null) {
                reverseEdge(graph, edgeToReverse);
                set.add(edgeToReverse.getFeature().getID());
            }
        }
        return new ArrayList<Integer>();
    }

    private void repairSources(DirectedGraph<INode,FeatureAsEdge> graph, Set<Integer> set) {
        for (INode node : graph.vertexSet()) {
            if (findSources && isSource(graph, node)) {
                BreadthFirstIterator<INode,FeatureAsEdge> it = new BreadthFirstIterator(graph, node);
                INode stopNode = null;
                while (it.hasNext()) {
                    INode n = it.next();
                    // Reverse edges until the first node that has has at least one other incoming edge
                    // or a node that has no more outgoind edge (a sink)
                    if ((graph.incomingEdgesOf(n).size()>1 || graph.outgoingEdgesOf(n).size()==0)
                            // but only if we don't want to use Z or if Z are NaN
                            && (!useZ ||
                                Double.isNaN(node.getGeometry().getCoordinate().z) ||
                                Double.isNaN(n.getCoordinate().z) ||
                                // or if Z of the final node is higher than the the z of initial node
                                // so that reversing edges makes them go down
                                n.getCoordinate().z > (node.getGeometry().getCoordinate().z-tolZ))) {
                        stopNode = n;
                        break;
                    }
                }
                if (stopNode != null) {
                    List<FeatureAsEdge> edges = DijkstraShortestPath.findPathBetween(graph, node, stopNode);
                    for (FeatureAsEdge edge : edges) {
                        reverseEdge(graph, edge);
                        set.add(edge.getFeature().getID());
                    }
                }
            }
        }
        //return new ArrayList<Integer>();
    }

    private void repairSinks(DirectedGraph<INode,FeatureAsEdge> graph, Set<Integer> set) {
        graph = new EdgeReversedGraph(graph);
        for (INode node : graph.vertexSet()) {
            if (findSources && isSource(graph, node)) {
                BreadthFirstIterator<INode,FeatureAsEdge> it = new BreadthFirstIterator(graph, node);
                INode stopNode = null;
                while (it.hasNext()) {
                    INode n = it.next();
                    // Reverse edges until the first node that has has at least one other incoming edge
                    // or a node that has no more outgoind edge (a sink)
                    if ((graph.incomingEdgesOf(n).size()>1 || graph.outgoingEdgesOf(n).size()==0)
                            // but only if we don't want to use Z or if Z are NaN
                            && (!useZ ||
                            Double.isNaN(node.getGeometry().getCoordinate().z) ||
                            Double.isNaN(n.getCoordinate().z) ||
                            // or if Z of the final node is higher than the the z of initial node
                            // so that reversing edges makes them go down
                            n.getCoordinate().z < (node.getGeometry().getCoordinate().z-tolZ))) {
                        stopNode = n;
                        break;
                    }
                }
                if (stopNode != null) {
                    List<FeatureAsEdge> edges = DijkstraShortestPath.findPathBetween(graph, node, stopNode);
                    for (FeatureAsEdge edge : edges) {
                        reverseEdge(graph, edge);
                        set.add(edge.getFeature().getID());
                    }
                }
            }
        }
        //return new ArrayList<Integer>();
    }

    private void reverseEdge(Graph<INode,FeatureAsEdge> graph, FeatureAsEdge edge) {
        INode start = graph.getEdgeSource(edge);
        INode end   = graph.getEdgeTarget(edge);
        //edge.getFeature().setGeometry(edge.getGeometry().reverse());
        graph.removeEdge(edge);
        graph.addEdge(end, start, edge);
    }

    // Returns a score determining if the inversion of this edge may improve the graph
    // If the inversion of the edge creates sources or wells, return 0
    // else if, return 1, except if useZ is on
    // If useZ is on, returns 1 if z orientation is improved and 0.25 if it is degraded
    // return 0.5 if no useful z information is available
    private double evaluateEdgeInversion(DirectedGraph<INode,FeatureAsEdge> graph, FeatureAsEdge edge) {
        INode start = graph.getEdgeSource(edge);
        INode end   = graph.getEdgeTarget(edge);
        graph.removeEdge(edge);
        graph.addEdge(end, start, edge);
        double result;
        if (isSource(graph, start) || isWell(graph, start)) result = 0;
        else if (isSource(graph, end) || isWell(graph, end)) result = 0;
        else if (useZ) {
            double z0 = ((LineString)edge.getGeometry()).getStartPoint().getCoordinate().z;
            double z1 = ((LineString)edge.getGeometry()).getEndPoint().getCoordinate().z;
            if (!Double.isNaN(z0) && !Double.isNaN(z1)) {
                if (z0 - z1 < -tolZ) {
                    result = 1;
                } else {
                    result = 0.25;
                }
            } else {
                result = 0.5;
            }
        } else {
            result = 1;
        }
        graph.removeEdge(edge);
        graph.addEdge(start, end, edge);
        return result;
    }

    private FeatureSchema getAnomalySchema() {
        FeatureSchema schema = new FeatureSchema();
        schema.addAttribute("geometry", AttributeType.GEOMETRY);
        schema.addAttribute("type", AttributeType.STRING);
        schema.addAttribute("comment", AttributeType.STRING);
        return schema;
    }

    public void setInversionStyle(Layer layer) {
        BasicStyle style = layer.getBasicStyle();
        style.setLineColor(Color.ORANGE);
        style.setLineWidth(3);
        style.setAlpha(200);
        style.setFillColor(Color.LIGHT_GRAY);
        layer.addStyle(new ArrowLineStringSegmentStyle.Solid());
    }

    public void setCycleStyle(Layer layer) {
        BasicStyle style = layer.getBasicStyle();
        style.setLineColor(Color.RED);
        style.setLineWidth(3);
        style.setAlpha(200);
        style.setFillColor(Color.LIGHT_GRAY);
    }

    private static class MyRingVertexStyle extends RingVertexStyle {

        MyRingVertexStyle() {super();}

        public int getSize() {return 25;}

        public void paint(Feature f, Graphics2D g, Viewport viewport) throws Exception {
            if (f.getGeometry() instanceof com.vividsolutions.jts.geom.Point) {
                Coordinate coord = f.getGeometry().getCoordinate();
                paint(g, viewport.toViewPoint(new Point2D.Double(coord.x, coord.y)));
            }
        }

        protected void render(java.awt.Graphics2D g) {
            g.setStroke(new java.awt.BasicStroke(2.5f));
            g.setColor(Color.RED);
            g.draw(shape);
        }
    }
}
