/*
 * (C) 2011 Micha&euml;l Michaud
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * 
 * For more information, contact:
 *
 * michael.michaud@free.fr
 *
 */

package fr.michaelm.jump.plugin.graph;

import com.vividsolutions.jts.algorithm.ConvexHull;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jts.operation.polygonize.Polygonizer;
import com.vividsolutions.jts.operation.union.UnaryUnionOp;
import com.vividsolutions.jump.feature.*;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.*;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;
import fr.michaelm.jump.feature.jgrapht.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import org.jgrapht.*;
import org.jgrapht.graph.*;

/**
 * Find cycles in a network graph.
 * @author Micha&euml;l Michaud
 * @version 0.1.2 (2011-07-16)
 */
//version 0.1.2 (2011-07-16) typo and comments
//version 0.1.1 (2010-04-22) first svn version
//version 0.1 (2008-02-02)
public class CycleFinderPlugIn extends ThreadedBasePlugIn {

    Layer layer;

    boolean dim3;
    double min_features = 1, max_features = 12;
    double max_length = Double.MAX_VALUE;

    String attribute;
    boolean use_attribute = false, ignore_empty = true;
    boolean all_homogeneous_cycles = true, isolated_cycles, pendant_cycles, junction_cycles, fork_cycles;
    boolean all_heterogeneous_cycles, aab_abb_cycles_only;

    GeometryFactory gf = new GeometryFactory();

    public String getName() {return "Cycle Finder PlugIn";}

    public void initialize(final PlugInContext context) throws Exception {
        
        final String GRAPH                            = I18NPlug.getI18N("Graph");
        final String CYCLE_FINDING                    = I18NPlug.getI18N("CycleFinderPlugIn.cycle-finding");
        
        context.getFeatureInstaller().addMainMenuItem(
          this, new String[]{MenuNames.PLUGINS, GRAPH}, CYCLE_FINDING + "...",
          false, null, new MultiEnableCheck()
          .add(context.getCheckFactory().createTaskWindowMustBeActiveCheck())
          .add(context.getCheckFactory().createAtLeastNLayersMustExistCheck(1)));
    }
    
    public boolean execute(PlugInContext context) {
        
        final String CYCLE_FINDING                    = I18NPlug.getI18N("CycleFinderPlugIn.cycle-finding");
        final String LAYER                            = I18NPlug.getI18N("Layer");
        final String USE_ATTRIBUTE                    = I18NPlug.getI18N("CycleFinderPlugIn.use-attribute");
        final String USE_ATTRIBUTE_TOOLTIP            = I18NPlug.getI18N("CycleFinderPlugIn.use-attribute-tooltip");
        final String ATTRIBUTE                        = I18NPlug.getI18N("Attribute");
        final String IGNORE_EMPTY                     = I18NPlug.getI18N("ignore-empty");
        final String IGNORE_EMPTY_TOOLTIP             = I18NPlug.getI18N("ignore-empty-tooltip");
        final String DIM3                             = I18NPlug.getI18N("dim3");
        final String DIM3_TOOLTIP                     = I18NPlug.getI18N("dim3-tooltip");
        
        final String MIN_FEATURES                     = I18NPlug.getI18N("CycleFinderPlugIn.min-features");
        final String MIN_FEATURES_TOOLTIP             = I18NPlug.getI18N("CycleFinderPlugIn.min-features-tooltip");
        final String MAX_FEATURES                     = I18NPlug.getI18N("CycleFinderPlugIn.max-features");
        final String MAX_FEATURES_TOOLTIP             = I18NPlug.getI18N("CycleFinderPlugIn.max-features-tooltip");
        final String MAX_LENGTH                       = I18NPlug.getI18N("CycleFinderPlugIn.max-length");
        final String MAX_LENGTH_TOOLTIP               = I18NPlug.getI18N("CycleFinderPlugIn.max-length-tooltip");
        
        final String ALL_HOMOGENEOUS_CYCLES           = I18NPlug.getI18N("CycleFinderPlugIn.all-homogeneous-cycles");
        final String ALL_HOMOGENEOUS_CYCLES_TOOLTIP   = I18NPlug.getI18N("CycleFinderPlugIn.all-homogeneous-cycles-tooltip");
        final String ISOLATED_CYCLES                  = I18NPlug.getI18N("CycleFinderPlugIn.isolated-cycles");
        final String ISOLATED_CYCLES_TOOLTIP          = I18NPlug.getI18N("CycleFinderPlugIn.isolated-cycles-tooltip");
        final String PENDANT_CYCLES                   = I18NPlug.getI18N("CycleFinderPlugIn.pendant-cycles");
        final String PENDANT_CYCLES_TOOLTIP           = I18NPlug.getI18N("CycleFinderPlugIn.pendant-cycles-tooltip");
        final String JUNCTION_CYCLES                  = I18NPlug.getI18N("CycleFinderPlugIn.junction-cycles");
        final String JUNCTION_CYCLES_TOOLTIP          = I18NPlug.getI18N("CycleFinderPlugIn.junction-cycles-tooltip");
        final String FORK_CYCLES                      = I18NPlug.getI18N("CycleFinderPlugIn.fork-cycles");
        final String FORK_CYCLES_TOOLTIP              = I18NPlug.getI18N("CycleFinderPlugIn.fork-cycles-tooltip");
        
        final String ALL_HETEROGENEOUS_CYCLES         = I18NPlug.getI18N("CycleFinderPlugIn.all-heterogeneous-cycles");
        final String ALL_HETEROGENEOUS_CYCLES_TOOLTIP = I18NPlug.getI18N("CycleFinderPlugIn.all-heterogeneous-cycles-tooltip");
        final String AAB_ABB_CYCLES_ONLY              = I18NPlug.getI18N("CycleFinderPlugIn.aab-abb-cycles-only");
        final String AAB_ABB_CYCLES_ONLY_TOOLTIP      = I18NPlug.getI18N("CycleFinderPlugIn.aab-abb-cycles-only-tooltip");
        
        final MultiInputDialog dialog = new MultiInputDialog(
        context.getWorkbenchFrame(), CYCLE_FINDING, true);
        
        final JComboBox jcb_layer = dialog.addLayerComboBox(
            LAYER, context.getCandidateLayer(0), null, context.getLayerManager());

        final JCheckBox jcb_3d = dialog.addCheckBox(DIM3, false, DIM3_TOOLTIP);

        dialog.addSeparator();

        dialog.addIntegerField(MIN_FEATURES, 1, 6, MIN_FEATURES_TOOLTIP);
        dialog.addIntegerField(MAX_FEATURES, 10, 6, MAX_FEATURES_TOOLTIP);
        dialog.addDoubleField(MAX_LENGTH, 500, 6, MAX_LENGTH_TOOLTIP);

        dialog.addSeparator();
        
        final JCheckBox jcb_all_homogeneous_cycles = dialog.addCheckBox(ALL_HOMOGENEOUS_CYCLES, all_homogeneous_cycles, ALL_HOMOGENEOUS_CYCLES_TOOLTIP);
        final JCheckBox jcb_isolated_cycles = dialog.addCheckBox(ISOLATED_CYCLES, isolated_cycles, ISOLATED_CYCLES_TOOLTIP);
        jcb_isolated_cycles.setEnabled(!all_homogeneous_cycles);
        final JCheckBox jcb_pendant_cycles = dialog.addCheckBox(PENDANT_CYCLES, pendant_cycles, PENDANT_CYCLES_TOOLTIP);
        jcb_pendant_cycles.setEnabled(!all_homogeneous_cycles);
        final JCheckBox jcb_junction_cycles = dialog.addCheckBox(JUNCTION_CYCLES, junction_cycles, JUNCTION_CYCLES_TOOLTIP);
        jcb_junction_cycles.setEnabled(!all_homogeneous_cycles);
        final JCheckBox jcb_fork_cycles = dialog.addCheckBox(FORK_CYCLES, fork_cycles, FORK_CYCLES_TOOLTIP);
        jcb_fork_cycles.setEnabled(!all_homogeneous_cycles);
        
        dialog.addSeparator();
        
        final JCheckBox jcb_use_attribute = dialog.addCheckBox(USE_ATTRIBUTE, use_attribute, USE_ATTRIBUTE_TOOLTIP);
        List list = getFieldsFromLayerWithoutGeometry(context.getCandidateLayer(0));
        Object val = list.size()>0?list.iterator().next():null;
        final JComboBox jcb_attribute = dialog.addComboBox(ATTRIBUTE, val, list, USE_ATTRIBUTE_TOOLTIP);
        jcb_attribute.setEnabled(use_attribute);
        final JCheckBox jcb_ignore_empty = dialog.addCheckBox(IGNORE_EMPTY, ignore_empty, IGNORE_EMPTY_TOOLTIP);
        jcb_ignore_empty.setEnabled(use_attribute);
        
        final JCheckBox jcb_all_heterogeneous_cycles = dialog.addCheckBox(ALL_HETEROGENEOUS_CYCLES, all_heterogeneous_cycles, ALL_HETEROGENEOUS_CYCLES_TOOLTIP);
        jcb_all_heterogeneous_cycles.setEnabled(use_attribute);
        final JCheckBox jcb_aab_abb_cycles_only = dialog.addCheckBox(AAB_ABB_CYCLES_ONLY, aab_abb_cycles_only, AAB_ABB_CYCLES_ONLY_TOOLTIP);
        jcb_aab_abb_cycles_only.setEnabled(use_attribute && !all_heterogeneous_cycles);

        jcb_layer.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometry(dialog.getLayer(LAYER));
                if (list.size() == 0) {
                    jcb_attribute.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_use_attribute.setEnabled(false);
                    jcb_attribute.setEnabled(false);
                    jcb_ignore_empty.setEnabled(false);
                    jcb_all_heterogeneous_cycles.setEnabled(false);
                    jcb_aab_abb_cycles_only.setEnabled(false);
                    attribute = null;
                }
                else {
                    jcb_attribute.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
                }
            }
        });
        
        jcb_use_attribute.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                use_attribute = dialog.getBoolean(USE_ATTRIBUTE);
                jcb_attribute.setEnabled(use_attribute);
                jcb_ignore_empty.setEnabled(use_attribute);
                jcb_all_heterogeneous_cycles.setEnabled(use_attribute);
                jcb_aab_abb_cycles_only.setEnabled(use_attribute);
            }
        });

        jcb_all_homogeneous_cycles.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                all_homogeneous_cycles = dialog.getBoolean(ALL_HOMOGENEOUS_CYCLES);
                jcb_isolated_cycles.setEnabled(!all_homogeneous_cycles);
                jcb_pendant_cycles.setEnabled(!all_homogeneous_cycles);
                jcb_junction_cycles.setEnabled(!all_homogeneous_cycles);
                jcb_fork_cycles.setEnabled(!all_homogeneous_cycles);
            }
        });
        
        jcb_all_heterogeneous_cycles.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                all_heterogeneous_cycles = dialog.getBoolean(ALL_HETEROGENEOUS_CYCLES);
                jcb_aab_abb_cycles_only.setEnabled(!all_heterogeneous_cycles);
            }
        });
        
        GUIUtil.centreOnWindow(dialog);
        dialog.setVisible(true);
        if (dialog.wasOKPressed()) {
            layer = dialog.getLayer(LAYER);
            use_attribute = dialog.getBoolean(USE_ATTRIBUTE);
            if (use_attribute) attribute = dialog.getText(ATTRIBUTE);
            else attribute = null;
            ignore_empty = dialog.getBoolean(IGNORE_EMPTY);
            dim3    = dialog.getBoolean(DIM3);
            min_features = dialog.getInteger(MIN_FEATURES);
            max_features = dialog.getInteger(MAX_FEATURES);
            max_length = dialog.getDouble(MAX_LENGTH);
            all_homogeneous_cycles = dialog.getBoolean(ALL_HOMOGENEOUS_CYCLES);
            isolated_cycles = dialog.getBoolean(ISOLATED_CYCLES);
            pendant_cycles = dialog.getBoolean(PENDANT_CYCLES);
            junction_cycles = dialog.getBoolean(JUNCTION_CYCLES);
            fork_cycles = dialog.getBoolean(FORK_CYCLES);
            all_heterogeneous_cycles = dialog.getBoolean(ALL_HETEROGENEOUS_CYCLES);
            aab_abb_cycles_only = dialog.getBoolean(AAB_ABB_CYCLES_ONLY);
            return true;
        }
        else return false;
        
    }
    
    public void run(TaskMonitor monitor, PlugInContext context) throws Exception {
        
        String INDEXATION_OF         = I18NPlug.getI18N("CycleFinderPlugIn.indexation-of");
        String POLYGONIZATION_OF     = I18NPlug.getI18N("CycleFinderPlugIn.polygonization-of");
        String ANALYSIS_OF           = I18NPlug.getI18N("CycleFinderPlugIn.analysis-of");
        String PROCESSED_CYCLES      = I18NPlug.getI18N("CycleFinderPlugIn.processed-cycles");
        String NB_OF_EDGES           = I18NPlug.getI18N("CycleFinderPlugIn.number-of-edges");
        String LENGTH                = I18NPlug.getI18N("CycleFinderPlugIn.length");
        String AREA                  = I18NPlug.getI18N("CycleFinderPlugIn.area");
        String CONVEXITY             = I18NPlug.getI18N("CycleFinderPlugIn.convexity");
        String CONVEX                = I18NPlug.getI18N("CycleFinderPlugIn.convex");
        String CONCAVE               = I18NPlug.getI18N("CycleFinderPlugIn.concave");
        String CIRCULARITY           = I18NPlug.getI18N("CycleFinderPlugIn.circularity");
        String CYCLE_HOMOGENEITY     = I18NPlug.getI18N("CycleFinderPlugIn.cycle-homogeneity");
        String HOMOGENEOUS           = I18NPlug.getI18N("CycleFinderPlugIn.homogeneous");
        String ISOLATED              = I18NPlug.getI18N("CycleFinderPlugIn.isolated");
        String PENDANT               = I18NPlug.getI18N("CycleFinderPlugIn.pendant");
        String JUNCTION              = I18NPlug.getI18N("CycleFinderPlugIn.junction");
        String FORK                  = I18NPlug.getI18N("CycleFinderPlugIn.fork");
        String HETEROGENEOUS         = I18NPlug.getI18N("CycleFinderPlugIn.heterogeneous");
        String AABABB                = I18NPlug.getI18N("CycleFinderPlugIn.aababb");
        String COMMENT               = I18NPlug.getI18N("CycleFinderPlugIn.comment");
        
        String NO_CYCLE_FOUND        = I18NPlug.getI18N("CycleFinderPlugIn.no-cycle-found");
        String HOMOGENEOUS_CYCLES    = I18NPlug.getI18N("CycleFinderPlugIn.homogeneous-cycles");
        String HETEROGENEOUS_CYCLES  = I18NPlug.getI18N("CycleFinderPlugIn.heterogeneous-cycles");
        
        monitor.allowCancellationRequests();
        monitor.report(INDEXATION_OF + layer.getName() + "...");
        
        FeatureSchema schema = new FeatureSchema();
        schema.addAttribute("GEOMETRY", AttributeType.GEOMETRY);
        schema.addAttribute(NB_OF_EDGES, AttributeType.INTEGER);
        schema.addAttribute(LENGTH, AttributeType.DOUBLE);
        schema.addAttribute(AREA, AttributeType.DOUBLE);
        schema.addAttribute(CONVEXITY, AttributeType.STRING);
        schema.addAttribute(CIRCULARITY, AttributeType.DOUBLE);
        schema.addAttribute(CYCLE_HOMOGENEITY, AttributeType.STRING);
        schema.addAttribute(COMMENT, AttributeType.STRING);
        FeatureCollection homogeneous_cycles_FC = new FeatureDataset(schema);
        FeatureCollection heterogeneous_cycles_FC = new FeatureDataset(schema);

        // Filter and index input layer features
        // Do not eliminate long features too early,
        // because they can be used as incident edges
        FeatureCollection filteredFC = layer.getFeatureCollectionWrapper();
        if (use_attribute && ignore_empty) {
            filteredFC = new FeatureDataset(filteredFC.getFeatureSchema());
            for (Object f : layer.getFeatureCollectionWrapper().getFeatures()) {
                if (((Feature)f).getAttribute(attribute)!=null &&
                    ((Feature)f).getAttribute(attribute).toString().trim().length()>0) {
                    filteredFC.add(((Feature)f));
                }
            }
        }
        IndexedFeatureCollection ifc =
            new IndexedFeatureCollection(filteredFC, new STRtree());

        // Eliminate features with length < max before polygonization
        // WARNING : a long feature can cut a short cycle into 2 long cycles
        // ==> eliminating long features can produce small non-simple cycles
        Collection<Geometry> geoms = new ArrayList<Geometry>();
        Collection<Geometry> lines = new ArrayList<Geometry>();
        for (Object f : filteredFC.getFeatures()) {
            Geometry geom = ((Feature)f).getGeometry();
            if (geom.getLength()<=max_length) {
                if (geom.getDimension() == 1) lines.add(geom);
                else geoms.add(geom);
            }
        }
        // [2013-01-15] union cleans overlapping lines, which is necessary
        // to perform polygonization the right way
        geoms.add(UnaryUnionOp.union(lines));

        monitor.report(POLYGONIZATION_OF + layer.getName() + "...");
        // Polygonisation + selection of polygons with length < threshold
        Polygonizer polygonizer = new Polygonizer();
        polygonizer.add(geoms);
        Collection pols = polygonizer.getPolygons();
        geoms.clear();
        for (Object p : pols) {
            if (((Geometry)p).getLength() <= max_length) geoms.add((Geometry)p);
        }

        monitor.report(ANALYSIS_OF + layer.getName() + "...");
        int count = 0;
        // Loop over polygons representing cycles
        for (Geometry g : geoms) {
            // Select edges of the cycle and edges incident to the cycle
            List list = ifc.query(g.getEnvelopeInternal());
            for (int i = list.size()-1 ; i>=0 ; i--) {
                Geometry gfeature = ((Feature)list.get(i)).getGeometry();
                if (g.disjoint(gfeature)) list.remove(i);
            }

            // Creates the graph with features intersecting the polygon
            UndirectedGraph<INode,FeatureAsEdge> graph =
                (UndirectedGraph<INode,FeatureAsEdge>)GraphFactory.createGraph(list, dim3);
            // Graph node set and graph edges set
            Set<INode> nodeSet = graph.vertexSet();
            Set<FeatureAsEdge> edgeSet = graph.edgeSet();

            // Subgraph containing only the cycle
            Set<INode> cycleNodeSet = new HashSet();
            for (INode n : nodeSet) {
                if (n.getGeometry().intersects(g)) cycleNodeSet.add(n);
            }
            Subgraph<INode, FeatureAsEdge, Graph<INode, FeatureAsEdge>> cycle =
                new Subgraph(graph, cycleNodeSet);
            Set<FeatureAsEdge> cycleEdgeSet = cycle.edgeSet();

            // Eliminate too long ot too short cycles
            if (cycleEdgeSet.size() < min_features || cycleEdgeSet.size() > max_features) continue;

            String shape = g.equals(new ConvexHull(g).getConvexHull())?CONVEX:CONCAVE;
            double area = g.getArea();
            double perimeter = g.getLength();
            // Circularity (100 = circular / 0 = linear)
            double circularity = Math.floor(100.0*area*4.0*Math.PI/perimeter/perimeter);
            // Build one feature for each cycle under/over size thresholds
            Feature newf = new BasicFeature(schema);
            newf.setGeometry(g);
            newf.setAttribute(NB_OF_EDGES, cycleEdgeSet.size());
            newf.setAttribute(CONVEXITY, shape);
            newf.setAttribute(CIRCULARITY, circularity);
            newf.setAttribute(LENGTH, perimeter);
            newf.setAttribute(AREA, area);
            
            // feature attributes found in cycle
            Set attributeSet = new HashSet();
            String NOATT = "DO_NOT_USE_ATTRIBUTE";
            for (FeatureAsEdge e : cycleEdgeSet) {
                if (use_attribute) attributeSet.add(e.getAttribute(attribute));
                else attributeSet.add(NOATT);
            }
            
            // Case 0 : no attribute defined for cycle homogeneity
            if (!use_attribute) {
                newf.setAttribute(CYCLE_HOMOGENEITY, HOMOGENEOUS);
                int incident_edges = edgeSet.size() - cycleEdgeSet.size();
                if (incident_edges == 0 && (all_homogeneous_cycles || isolated_cycles)) {
                    newf.setAttribute(CYCLE_HOMOGENEITY, HOMOGENEOUS);
                    newf.setAttribute(COMMENT, ISOLATED);
                    homogeneous_cycles_FC.add(newf);
                }
                else if (incident_edges == 1 && (all_homogeneous_cycles || pendant_cycles)) {
                    newf.setAttribute(CYCLE_HOMOGENEITY, HOMOGENEOUS);
                    newf.setAttribute(COMMENT, PENDANT);
                    homogeneous_cycles_FC.add(newf);
                }
                else if (incident_edges == 2 && (all_homogeneous_cycles || junction_cycles)) {
                    newf.setAttribute(CYCLE_HOMOGENEITY, HOMOGENEOUS);
                    newf.setAttribute(COMMENT, JUNCTION);
                    homogeneous_cycles_FC.add(newf);
                }
                else if (incident_edges > 2 && (all_homogeneous_cycles || fork_cycles)) {
                    newf.setAttribute(CYCLE_HOMOGENEITY, HOMOGENEOUS);
                    newf.setAttribute(COMMENT, FORK);
                    homogeneous_cycles_FC.add(newf);
                }
            }
            
            // Case 1 : only one attribute value or !use_attribute
            else if (attributeSet.size() == 1) {
                newf.setAttribute(CYCLE_HOMOGENEITY, HOMOGENEOUS);
                Object atth = attributeSet.iterator().next();
                int count_homogeneous_edges = 0;
                for (FeatureAsEdge e : edgeSet) {
                    Object atte = use_attribute?e.getAttribute(attribute):NOATT;
                    if (atte.equals(atth)) count_homogeneous_edges++;
                }
                int incident_edges = count_homogeneous_edges - cycleEdgeSet.size();
                if (incident_edges == 0 && (all_homogeneous_cycles || isolated_cycles)) {
                    newf.setAttribute(CYCLE_HOMOGENEITY, HOMOGENEOUS);
                    newf.setAttribute(COMMENT, ISOLATED);
                    homogeneous_cycles_FC.add(newf);
                }
                else if (incident_edges == 1 && (all_homogeneous_cycles || pendant_cycles)) {
                    newf.setAttribute(CYCLE_HOMOGENEITY, HOMOGENEOUS);
                    newf.setAttribute(COMMENT, PENDANT);
                    homogeneous_cycles_FC.add(newf);
                }
                else if (incident_edges == 2 && (all_homogeneous_cycles || junction_cycles)) {
                    newf.setAttribute(CYCLE_HOMOGENEITY, HOMOGENEOUS);
                    newf.setAttribute(COMMENT, JUNCTION);
                    homogeneous_cycles_FC.add(newf);
                }
                else if (incident_edges > 2 && (all_homogeneous_cycles || fork_cycles)) {
                    newf.setAttribute(CYCLE_HOMOGENEITY, HOMOGENEOUS);
                    newf.setAttribute(COMMENT, FORK);
                    homogeneous_cycles_FC.add(newf);
                }
                
            }
            
            // Case 2 : use_attribute and several attribute values
            else if (attributeSet.size() > 1 && (all_heterogeneous_cycles || aab_abb_cycles_only)) {
                newf.setAttribute(CYCLE_HOMOGENEITY, HETEROGENEOUS);
                //Object atth = attributeSet.iterator().next();
                INode[] nodeArray = cycleNodeSet.toArray(new INode[0]);
                if (all_heterogeneous_cycles) heterogeneous_cycles_FC.add(newf);
                else if(aab_abb_cycles_only) {
                    for (int i = 0 ; i < nodeArray.length ; i++) {
                        //Map<Object,Integer> edgeMapI = new HashMap<Object,Integer>();
                        if (graph.edgesOf(nodeArray[i]).size() != 3) continue;
                        Map<Object,Integer> edgeMapI = incidentEdgeValues(nodeArray[i], graph, attribute);
                        if (edgeMapI.size() != 2) continue;
                        for (int j = 0 ; j < nodeArray.length ; j++) {
                            if (i==j || graph.edgesOf(nodeArray[j]).size() != 3) continue;
                            Map<Object,Integer> edgeMapJ = incidentEdgeValues(nodeArray[j], graph, attribute);
                            if (edgeMapI.keySet().equals(edgeMapJ.keySet()) && !edgeMapI.equals(edgeMapJ)) {
                                newf.setAttribute(COMMENT, AABABB);
                                heterogeneous_cycles_FC.add(newf);
                            }
                        }
                    }
                }
                else;
            }
            count++;
            monitor.report(count, geoms.size(), PROCESSED_CYCLES);
        }
        context.getLayerManager().addCategory(StandardCategoryNames.RESULT);
        if (homogeneous_cycles_FC.size()>0) {
            context.addLayer(StandardCategoryNames.RESULT, layer.getName()+"-Cycles", homogeneous_cycles_FC);
        }
        if (use_attribute && heterogeneous_cycles_FC.size()>0) {
            context.addLayer(StandardCategoryNames.RESULT,
                layer.getName()+"-CyclesHeterogènes", heterogeneous_cycles_FC);
        }
        if (homogeneous_cycles_FC.size() == 0 && heterogeneous_cycles_FC.size() == 0) {
            context.getWorkbenchFrame().warnUser(NO_CYCLE_FOUND);
        }
    }
    
    private List getFieldsFromLayerWithoutGeometry(Layer l) {
        List fields = new ArrayList();
        FeatureSchema schema = l.getFeatureCollectionWrapper().getFeatureSchema();
        for (int i = 0 ; i < schema.getAttributeCount() ; i++) {
            if (schema.getAttributeType(i) != AttributeType.GEOMETRY) {
                fields.add(schema.getAttributeName(i));  
           }
        }
        return fields;
    }
    
    // Returns a map counting distinct attribute values of incident edges
    private Map<Object,Integer> incidentEdgeValues(INode node,
                UndirectedGraph<INode,FeatureAsEdge> graph, String attribute) {
        Map<Object,Integer> edgeMap = new HashMap<Object,Integer>();
        Set<FeatureAsEdge> edges = graph.edgesOf(node);
        for (FeatureAsEdge edge : edges) {
            Object val = edge.getAttribute(attribute);
            if (edgeMap.containsKey(val)) {
                edgeMap.put(val, edgeMap.get(val)+1);
            }
            else edgeMap.put(val, new Integer(1));
        }
        return edgeMap;
    }

}
