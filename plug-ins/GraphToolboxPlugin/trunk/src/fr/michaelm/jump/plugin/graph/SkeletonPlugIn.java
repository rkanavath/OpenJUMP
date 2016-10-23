package fr.michaelm.jump.plugin.graph;

import com.vividsolutions.jts.algorithm.MCPointInRing;
import com.vividsolutions.jts.densify.Densifier;
import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.geom.util.LineStringExtracter;
import com.vividsolutions.jts.operation.linemerge.LineMerger;
import com.vividsolutions.jts.simplify.TopologyPreservingSimplifier;
import com.vividsolutions.jts.triangulate.VoronoiDiagramBuilder;
import com.vividsolutions.jump.feature.*;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.LayerManager;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;
import com.vividsolutions.jump.workbench.ui.plugin.FeatureInstaller;
import fr.michaelm.jump.feature.jgrapht.GraphFactory;
import org.jgrapht.WeightedGraph;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import org.jgrapht.UndirectedGraph;
import fr.michaelm.jump.feature.jgrapht.FeatureAsEdge;
import fr.michaelm.jump.feature.jgrapht.INode;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.*;

/**
 * PlugIn to skeletonize polygons
 * See https://github.com/orbisgis/h2gis/wiki/3.1-Extract-central-skeleton
 */
public class SkeletonPlugIn extends AbstractThreadedUiPlugIn {

    private static String GRAPH                 = I18NPlug.getI18N("Graph");
    private static String CENTRAL_SKELETON      = I18NPlug.getI18N("SkeletonPlugIn");
    private static String SKELETONIZE           = I18NPlug.getI18N("SkeletonPlugIn.skeletonize");
    private static String SOURCE_LAYER          = I18NPlug.getI18N("SkeletonPlugIn.source-layer");
    private static String AUTO_PARAMETERS       = I18NPlug.getI18N("SkeletonPlugIn.auto-parameters");
    private static String AUTO_PARAMETERS_TT    = I18NPlug.getI18N("SkeletonPlugIn.auto-parameters-tooltip");
    private static String MIN_WIDTH             = I18NPlug.getI18N("SkeletonPlugIn.min-width");
    private static String MIN_WIDTH_TT          = I18NPlug.getI18N("SkeletonPlugIn.min-width-tooltip");
    private static String ITERATIONS            = I18NPlug.getI18N("SkeletonPlugIn.iterations");
    private static String ITERATIONS_TT         = I18NPlug.getI18N("SkeletonPlugIn.iterations-tooltip");
    private static String MIN_FORK_LENGTH       = I18NPlug.getI18N("SkeletonPlugIn.min-fork-length");
    private static String MIN_FORK_LENGTH_TT    = I18NPlug.getI18N("SkeletonPlugIn.min-fork-length-tooltip");
    private static String BEAUTIFY_ENDS         = I18NPlug.getI18N("SkeletonPlugIn.beautify-ends");
    private static String BEAUTIFY_ENDS_TT      = I18NPlug.getI18N("SkeletonPlugIn.beautify-ends-tooltip");
    private static String DISPLAY_VORONOI_EDGES = I18NPlug.getI18N("SkeletonPlugIn.display-voronoi-edges");
    private static String DESCRIPTION           = I18NPlug.getI18N("SkeletonPlugIn.description");

    private String layerName;
    private boolean autoParameters = true;
    private double minWidth = 1.0;
    private int iterations = 10;
    private double minForkLength = 10.0;
    private boolean snapEnds = true;
    private boolean displayVoronoiEdges = false;

    public void initialize(PlugInContext context) throws Exception {

        workbenchContext = context.getWorkbenchContext();
        FeatureInstaller featureInstaller = new FeatureInstaller(workbenchContext);
        featureInstaller.addMainMenuPlugin(
                this,
                new String[] {MenuNames.PLUGINS, GRAPH},
                CENTRAL_SKELETON + "...",
                false,			//checkbox
                new ImageIcon(this.getClass().getResource("Skeleton.png")),
                createEnableCheck(context.getWorkbenchContext()));
    }

    private static MultiEnableCheck createEnableCheck(WorkbenchContext workbenchContext) {
        EnableCheckFactory checkFactory = new EnableCheckFactory(workbenchContext);
        return new MultiEnableCheck().add(checkFactory.createAtLeastNLayersMustExistCheck(1));
    }


    public boolean execute(PlugInContext context) throws Exception{
        this.reportNothingToUndoYet(context);

        MultiInputDialog dialog = new MultiInputDialog(
                context.getWorkbenchFrame(), getName(), true);
        setDialogValues(dialog, context);
        GUIUtil.centreOnWindow(dialog);
        dialog.setVisible(true);
        if (! dialog.wasOKPressed()) { return false; }
        getDialogValues(dialog);
        return true;
    }

    private void setDialogValues(MultiInputDialog dialog, PlugInContext context) {
        dialog.setSideBarDescription(DESCRIPTION);
        layerName = context.getCandidateLayer(0).getName();
        dialog.addLayerComboBox(SOURCE_LAYER, context.getLayerManager().getLayer(layerName),
                null, context.getLayerManager());
        final JCheckBox autoJcb = dialog.addCheckBox(AUTO_PARAMETERS, autoParameters,AUTO_PARAMETERS_TT);
        final JTextField minWidthTF = dialog.addDoubleField(MIN_WIDTH, minWidth, 12, MIN_WIDTH_TT);
        final JTextField minForkLengthTF = dialog.addDoubleField(MIN_FORK_LENGTH, minForkLength, 12, MIN_FORK_LENGTH_TT);
        final JTextField iterationsTF = dialog.addIntegerField(ITERATIONS, iterations, 12, ITERATIONS_TT);
        dialog.addCheckBox(BEAUTIFY_ENDS, snapEnds, BEAUTIFY_ENDS_TT);
        dialog.addCheckBox(DISPLAY_VORONOI_EDGES, displayVoronoiEdges);

        minWidthTF.setEnabled(!autoJcb.isSelected());
        minForkLengthTF.setEnabled(!autoJcb.isSelected());
        autoJcb.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                minWidthTF.setEnabled(!autoJcb.isSelected());
                minForkLengthTF.setEnabled(!autoJcb.isSelected());
            }
        });
    }

    private void getDialogValues(MultiInputDialog dialog) {
        layerName = dialog.getLayer(SOURCE_LAYER).getName();
        autoParameters = dialog.getBoolean(AUTO_PARAMETERS);
        minWidth = dialog.getDouble(MIN_WIDTH);
        iterations = dialog.getInteger(ITERATIONS);
        minForkLength = dialog.getDouble(MIN_FORK_LENGTH);
        snapEnds = dialog.getBoolean(BEAUTIFY_ENDS);
        displayVoronoiEdges = dialog.getBoolean(DISPLAY_VORONOI_EDGES);
    }

    public void run(TaskMonitor monitor, PlugInContext context) throws Exception {
        monitor.report(SKELETONIZE);
        LayerManager layerManager = context.getLayerManager();
        FeatureCollection inputFC = layerManager.getLayer(layerName).getFeatureCollectionWrapper();
        FeatureCollection outputFC = new FeatureDataset(inputFC.getFeatureSchema());
        int count = 0;
        // edges = list to collect original edges from the voronoi diagram
        List<Geometry> edges = new ArrayList<Geometry>();
        for (Feature feature : inputFC.getFeatures()) {
            monitor.report(count++, inputFC.size(), "Feature");
            if (feature.getGeometry().getDimension() == 2) {
                Geometry geom = feature.getGeometry();
                if (!geom.isValid()) geom = geom.buffer(0);
                for (int i = 0 ; i < geom.getNumGeometries() ; i++) {
                    Feature newFeature = feature.clone(false, false);
                    Geometry g = geom.getGeometryN(i);
                    computeAutoParams(g);
                    newFeature.setGeometry(skeletonize(g, edges));
                    outputFC.add(newFeature);
                }
            } else {
                Feature newFeature = feature.clone(false, false);
                newFeature.setGeometry(feature.getGeometry());
                outputFC.add(newFeature);
            }
        }
        layerManager.addLayer(StandardCategoryNames.RESULT, layerName + " - skeletonized", outputFC);
        if (edges.size() > 0) {
            layerManager.addLayer(StandardCategoryNames.RESULT, layerName + " - voronoi-edges",
                    FeatureDatasetFactory.createFromGeometry(edges));
        }
    }

    // Get minWidth and meanWidth from the geometry
    private void computeAutoParams(Geometry geometry) throws Exception {
        if (autoParameters) {
            double meanWidth = getMeanWidth(geometry);
            if (meanWidth==0) meanWidth = geometry.getLength()/geometry.getNumPoints()/5;
            minForkLength = 2*meanWidth;
            // Iterative function to find the minimal width of the polygon
            // A buffer < -minWidth/2 will split the polygon into two polygons
            // (except in polygons with holes where)
            int numGeometries = geometry.getNumGeometries();
            double buffParam = meanWidth/2;
            Geometry buffer = geometry.buffer(-buffParam);
            while (buffer.getNumGeometries() > numGeometries) {
                buffParam = buffParam/2;
                buffer = geometry.buffer(-buffParam);
                if (buffParam < meanWidth/128) {
                    System.out.println("Problem with geometry " + geometry);
                    break;
                }
            }
            minWidth = 2*buffParam;
        }
    }

    // Computes the mean width of a polygon
    private Double getMeanWidth(Geometry g) {
        if (g.getDimension() == 2) {
            double length = g.getLength();
            double area = g.getArea();
            double val = (( length * length ) / 4.0 )-( 4.0 * area );
            if (val >= 0.0) {
                //calcul normal sur surface allongée
                return ((length / 2.0) - Math.sqrt(val)) / 2.0;
            } else {
                //diamètre du disque de même surface, sur une surface ramassée
                return 2.0 * Math.sqrt(area / Math.PI);
            }
        } else return 0d;
    }

    // Simplification/densification of input geometry
    private Geometry preprocess(Geometry geometry) throws Exception {
        geometry = TopologyPreservingSimplifier.simplify(geometry, minWidth/5);
        if (geometry.isEmpty() || Double.isNaN(minWidth)) {
            return geometry;
        } else {
            geometry = Densifier.densify(geometry, minWidth/2);
            return geometry;
        }
    }

    // Build a voronoi diagram based on geometry vertices
    // (geometry must have been densified before)
    private Geometry getVoronoiDiagram(Geometry geometry) throws Exception {
        if (geometry.isEmpty()) return geometry;
        VoronoiDiagramBuilder voronoiBuilder = new VoronoiDiagramBuilder();
        voronoiBuilder.setSites(geometry.getFactory().createMultiPoint(geometry.getCoordinates()));
        voronoiBuilder.setTolerance(0);
        Envelope env = geometry.getEnvelopeInternal();
        env.expandBy(env.getWidth()/4, env.getHeight()/4);
        voronoiBuilder.setClipEnvelope(env);
        try {
            return voronoiBuilder.getDiagram(geometry.getFactory());
        } catch(Exception e) {
            e.printStackTrace();
            Geometry newGeometry = preprocess(geometry.buffer(minWidth/10));
            if (newGeometry.equals(geometry)) throw new Exception("Cannot process " + geometry);
            return getVoronoiDiagram(newGeometry);
        }
    }

    // Get single line segments from the voronoi diagram
    private void getEdges(Geometry geometry, Set<LineString> edges) {
        for (int i = 0 ; i < geometry.getNumGeometries() ; i++) {
            Geometry g = geometry.getGeometryN(i);
            if (g instanceof GeometryCollection) {
                getEdges(g, edges);
            }
            else if (g.getDimension() == 0) {
                continue;
            }
            else if (g.getDimension() == 1) {
                addSegments((LineString)g, edges);
            }
            else {
                Polygon p = (Polygon)g;
                addSegments(p.getExteriorRing(), edges);
                for (int j = 0 ; j < p.getNumInteriorRing() ; j++) {
                    addSegments(p.getInteriorRingN(j), edges);
                }
            }
        }
    }

    // Add every single segment of a linestring to set
    private void addSegments(LineString lineString, Set<LineString> set) {
        Coordinate[] cc = lineString.getCoordinates();
        for (int i = 1 ; i < cc.length ; i++) {
            LineString s = lineString.getFactory().createLineString(new Coordinate[]{cc[i-1], cc[i]});
            s = (LineString)s.clone();
            s.normalize();
            set.add(s);
        }
    }

    // Keep only segments entirely located inside the geometry
    // This method is faster than a simple geometry.contains test.
    // We consider that edges issued from the voronoi diagram are simple line segments
    // which are entirely included in the geometry if both ends are included in the
    // polygon (which is only true if the geometry has been appropiately densified).
    // From this consideration, we can replace contains by pointInRing tests
    // On complex polygons (2800 pts) filter is by far the longest operation of
    // skeletisation and this method save up to 75% time.
    private Set<LineString> filter(Geometry geometry, Set<LineString> list) {
        Set<LineString> result = new HashSet<LineString>();
        if (geometry instanceof Polygon) {
            Polygon poly = (Polygon)geometry;
            for (LineString line : list) {
                Coordinate start = line.getCoordinateN(0);
                Coordinate end = line.getCoordinateN(line.getNumPoints()-1);
                MCPointInRing mcpir = new MCPointInRing((LinearRing)poly.getExteriorRing());
                if (mcpir.isInside(start) && mcpir.isInside(end)) {
                    boolean inside = true;
                    for (int i = 0 ; i < poly.getNumInteriorRing() ; i++) {
                        mcpir = new MCPointInRing((LinearRing)poly.getInteriorRingN(i));
                        if (mcpir.isInside(start) || mcpir.isInside(end)) {
                            inside = false;
                            break;
                        }
                    }
                    if (inside) result.add(line);
                }
            }
            return result;
        } else throw new IllegalArgumentException(geometry.getGeometryType() + " argument is not authorized");
    }


    private Geometry skeletonize(Geometry geometry, List<Geometry> list) throws Exception {

        // 1 - Build voronoi diagram and extract the edges
        long t0 = System.currentTimeMillis();
        Set<LineString> edges = new HashSet<LineString>();
        getEdges(getVoronoiDiagram(preprocess(geometry)), edges);
        //System.out.println("voronoi : " + (System.currentTimeMillis()-t0)/1000);

        // 2 - Filter voronoi edges strictly included in the geometry
        edges = filter(geometry, edges);
        //System.out.println("filter : " + (System.currentTimeMillis()-t0)/1000);

        // 3 - Merge filtered edges
        LineMerger merger = new LineMerger();
        merger.add(edges);
        edges = new HashSet(merger.getMergedLineStrings());
        //System.out.println("merge : " + (System.currentTimeMillis()-t0)/1000);

        if (displayVoronoiEdges) list.addAll(edges);

        // 4 - Compute the graph and iterate to eliminate edges
        UndirectedGraph<INode,FeatureAsEdge> graph =
                (UndirectedGraph<INode,FeatureAsEdge>)getGraph(edges);
        //System.out.println("build graph : " + (System.currentTimeMillis()-t0)/1000);

        // 5 - Simplify the graph iteratively
        for (int i = 0 ; i < iterations ; i++) {
            graph = simplify(graph, geometry.getBoundary(), i+1);
        }

        // 6 - Simplify the graph even more using forkingFactor
        // build a new array to avoid ConcurrentModificationException
        List<FeatureAsEdge> finalEdges = new ArrayList<FeatureAsEdge>(graph.edgeSet());
        for (FeatureAsEdge f : finalEdges) {
            int[] dd = getEdgeDegree(graph, f);
            if ((dd[0]==1 || dd[1]==1)) {
                if (f.getGeometry().getLength() < minForkLength) {
                    if (dd[0] == 1) graph.removeVertex(graph.getEdgeSource(f));
                    if (dd[1] == 1) graph.removeVertex(graph.getEdgeTarget(f));
                    graph.removeEdge(f);
                } else if (snapEnds) {
                    if (dd[0] == 1 && dd[1] == 1) {
                        if (f.getGeometry().getLength() > minWidth*6) {
                            CoordinateList cl = new CoordinateList(f.getGeometry().getCoordinates());
                            if (cl.getCoordinate(cl.size() - 2).distance(cl.getCoordinate(cl.size() - 1)) < minWidth * 2) {
                                cl.remove(cl.getCoordinate(cl.size() - 1));
                            }
                            if (cl.getCoordinate(0).distance(cl.getCoordinate(1)) < minWidth * 2) {
                                cl.remove(cl.getCoordinate(0));
                            }
                            if (cl.size() < f.getGeometry().getNumPoints()) {
                                f.setGeometry(f.getGeometry().getFactory().createLineString(cl.toCoordinateArray()));
                            }
                        }
                    } else if (dd[0] == 1 && f.getGeometry().getNumPoints() > 2) {
                        CoordinateList cl = new CoordinateList(f.getGeometry().getCoordinates());
                        if (cl.getCoordinate(0).distance(cl.getCoordinate(1)) < minWidth*2) {
                            cl.remove(cl.getCoordinate(0));
                            f.setGeometry(f.getGeometry().getFactory().createLineString(cl.toCoordinateArray()));
                        }
                    } else if (dd[1] == 1 && f.getGeometry().getNumPoints() > 2) {
                        CoordinateList cl = new CoordinateList(f.getGeometry().getCoordinates());
                        if (cl.getCoordinate(cl.size()-2).distance(cl.getCoordinate(cl.size()-1)) < minWidth*2) {
                            cl.remove(cl.getCoordinate(cl.size()-1));
                            f.setGeometry(f.getGeometry().getFactory().createLineString(cl.toCoordinateArray()));
                        }
                    }
                }
            }
        }
        //System.out.println(String.format("simplify graph : %d", (System.currentTimeMillis() - t0) / 1000));

        // 7 - Finalise (connect graph ens with geometry boundary)

        // 8 - Retourner une geometrie
        Geometry geom = graph2geometry(graph);
        return TopologyPreservingSimplifier.simplify(geom, minWidth/5);
    }

    private Geometry graph2geometry(UndirectedGraph<INode,FeatureAsEdge> graph) {
        Collection<Geometry> lines = new ArrayList<Geometry>();
        for (FeatureAsEdge f : graph.edgeSet()) {
            lines.add(f.getGeometry());
        }
        LineMerger merger = new LineMerger();
        merger.add(lines);
        lines = merger.getMergedLineStrings();
        GeometryFactory gf = lines.size()>0 ?
                lines.iterator().next().getFactory() : new GeometryFactory();
        return gf.buildGeometry(lines);
    }

    private int[] getEdgeDegree(UndirectedGraph<INode,FeatureAsEdge> graph, FeatureAsEdge edge) {
        INode source = graph.getEdgeSource(edge);
        INode target = graph.getEdgeTarget(edge);
        return new int[]{graph.degreeOf(source), graph.degreeOf(target)};
    }

    private WeightedGraph<INode,FeatureAsEdge> getGraph(Collection<LineString> edges) {
        // Build the graph (with jgrapht library)
        FeatureSchema schema = new FeatureSchema();
        schema.addAttribute("geometry", AttributeType.GEOMETRY);
        schema.addAttribute("keepit", AttributeType.BOOLEAN);
        List<Feature> features = new ArrayList<Feature>();
        for (LineString line : edges) {
            Feature feature = new BasicFeature(schema);
            feature.setGeometry(line);
            features.add(feature);
        }
        return GraphFactory.createGraph(features, false);
    }

    // Simplify the edges graph.
    // Remove edges having a node of degree 1 :
    // - if the distance from edge to the geometry boundary is greater than the
    //   tolerance (tolerance is relaxed after each iteration)
    // - if it touches another longer edge of degree 1 also close to the geometry
    // This method is used by skeletonize method in an iterative way.
    private UndirectedGraph<INode,FeatureAsEdge> simplify(
            UndirectedGraph<INode,FeatureAsEdge> graph,
            Geometry boundary,
            int iteration) {
        if (graph.edgeSet().size()==1) return graph;
        // Mark all edges having a node of degree 1 close to polygon boundary
        for (FeatureAsEdge edge : graph.edgeSet()) {
            INode ini = graph.getEdgeSource(edge);
            INode fin = graph.getEdgeTarget(edge);
            double degreeIni = graph.degreeOf(ini);
            double degreeFin = graph.degreeOf(fin);
            if (degreeIni == 1 && boundary.distance(ini.getGeometry()) < (Math.min(minWidth*iteration, minForkLength))) {
                edge.setAttribute("keepit", true);
            } else if (degreeFin == 1 && boundary.distance(fin.getGeometry()) < (Math.min(minWidth*iteration, minForkLength))) {
                edge.setAttribute("keepit", true);
            } else if (degreeIni > 1 && degreeFin > 1) {
                edge.setAttribute("keepit", true);
            } else {
                edge.setAttribute("keepit", false);
            }
        }
        // Mark edges of degree 1 touching another longer edge of degree 1
        for (FeatureAsEdge edge : graph.edgeSet()) {
            if ((Boolean)edge.getAttribute("keepit")) {
                INode ini = graph.getEdgeSource(edge);
                INode fin = graph.getEdgeTarget(edge);
                if (graph.degreeOf(ini) > 1 && graph.degreeOf(fin) == 1) {
                    for (FeatureAsEdge e : graph.edgesOf(ini)) {
                        if (e != edge && (Boolean)e.getAttribute("keepit")) {
                            if (graph.degreeOf(graph.getEdgeSource(e))>1 &&
                                    graph.degreeOf(graph.getEdgeTarget(e))>1) continue;
                            if (edge.getGeometry().getLength() < e.getGeometry().getLength() &&
                                    edge.getGeometry().getLength() < minForkLength) {
                                edge.setAttribute("keepit", false);
                                continue;
                            }
                        }
                    }
                }
                else if (graph.degreeOf(fin) > 1 && graph.degreeOf(ini) == 1) {
                    for (FeatureAsEdge e : graph.edgesOf(fin)) {
                        if (e != edge && (Boolean)e.getAttribute("keepit")) {
                            if (graph.degreeOf(graph.getEdgeSource(e))>1 &&
                                    graph.degreeOf(graph.getEdgeTarget(e))>1) continue;
                            if (edge.getGeometry().getLength() < e.getGeometry().getLength() &&
                                    edge.getGeometry().getLength() < minForkLength) {
                                edge.setAttribute("keepit", false);
                                continue;
                            }
                        }
                    }
                }
            }
        }
        // Supprimer les arcs et refusionner le résultat
        //Set<INode> nodesToRemove = new HashSet<INode>();
        Set<FeatureAsEdge> featuresToremove = new HashSet<FeatureAsEdge>();
        for (FeatureAsEdge edge : graph.edgeSet()) {
            if (!(Boolean)edge.getAttribute("keepit") &&
                    featuresToremove.size() < graph.edgeSet().size()-1) {
                featuresToremove.add(edge);
            }
        }
        graph.removeAllEdges(featuresToremove);
        //graph.removeAllVertices(nodesToRemove);

        // merge lines after simplification
        Geometry geom = graph2geometry(graph);
        List edges = LineStringExtracter.getLines(geom);
        LineMerger merger = new LineMerger();
        merger.add(edges);
        edges = (List)merger.getMergedLineStrings();
        graph = (UndirectedGraph<INode,FeatureAsEdge>)getGraph(edges);
        return graph;
    }

    /*
    private int minDegree(UndirectedGraph<INode,FeatureAsEdge> graph, FeatureAsEdge edge) {
        return Math.min(
                graph.degreeOf(graph.getEdgeSource(edge)),
                graph.degreeOf(graph.getEdgeTarget(edge)));
    }

    private int minDegree(UndirectedGraph<INode,FeatureAsEdge> graph, Set<FeatureAsEdge> edgeSet) {
        if (edgeSet.isEmpty()) return 0;
        int min = Integer.MAX_VALUE;
        for (FeatureAsEdge edge : edgeSet) min = Math.min(min, minDegree(graph, edge));
        return min;
    }

    private Set<FeatureAsEdge> connectedEdges(UndirectedGraph<INode,FeatureAsEdge> graph, FeatureAsEdge edge) {
        if (minDegree(graph,edge)==1) {
            Set<FeatureAsEdge> set = new HashSet<FeatureAsEdge>();
            set.addAll(graph.edgesOf(graph.getEdgeSource(edge)));
            set.addAll(graph.edgesOf(graph.getEdgeTarget(edge)));
            set.remove(edge);
            return set;
        } else {throw new IllegalArgumentException("The method is only defined for edges of degree 1");}
    }
    */


}
