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

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import org.jgrapht.*;
import org.jgrapht.alg.*;
import org.jgrapht.graph.*;

/**
 * Find main components of each graph or subgraph (number of connected
 * subgraphs, length, number of order 1 nodes...).
 * @author Micha&euml;l Michaud
 * @version 0.1.2 (2011-07-16)
 */
//version 0.1.2 (2011-07-16) typos and comments
//version 0.1.1 (2010-04-22) first svn version
//version 0.1 (2010-04-22)
public class GraphComponentsPlugIn extends ThreadedBasePlugIn {
    
    String GRAPH;
    String CONNECTED_COMPONENTS;
    String LAYER;
    String USE_ATTRIBUTE;
    String USE_ATTRIBUTE_TOOLTIP;
    String ATTRIBUTE;
    String IGNORE_EMPTY;
    String IGNORE_EMPTY_TOOLTIP;
    String DIM3;
    String DIM3_TOOLTIP;
    String FUZZY;
    String FUZZY_TOOLTIP;
    String RETURN_GRAPHS_AS;
    String RETURN_GRAPHS_AS_TOOLTIP;
    String POINT, MULTILINESTRING, SIMPLIFIED_MULTILINESTRING;
    String GRAPH_ANALYSIS;
    String CONNECTED_SUBGRAPH;
    String CONNECTED_SUBGRAPHS;
    String FEATURES;
    String PENDANT_VERTICES;
    String LENGTH;
    String PROCESSED_GRAPHS;
    String GRAPHS;
    String SUBGRAPHS;
    String NO_GRAPH;
    
    Layer layer;
    String attribute;
    AttributeType attType;
    boolean use_attribute = false;
    boolean ignore_empty = true;
    boolean dim3 = false;
    double fuzzy = 0.0;
    String return_graphs_as = "Point";
    
    GeometryFactory gf = new GeometryFactory();
    
    public String getName() {return "Graph components PlugIn";}

    public void initialize(final PlugInContext context) throws Exception {
        
        GRAPH                      = I18NPlug.getI18N("Graph");
        CONNECTED_COMPONENTS       = I18NPlug.getI18N("GraphComponentsPlugIn.connected-components");
        LAYER                      = I18NPlug.getI18N("Layer");
        USE_ATTRIBUTE              = I18NPlug.getI18N("use-attribute");
        USE_ATTRIBUTE_TOOLTIP      = I18NPlug.getI18N("use-attribute-tooltip");
        ATTRIBUTE                  = I18NPlug.getI18N("Attribute");
        IGNORE_EMPTY               = I18NPlug.getI18N("ignore-empty");
        IGNORE_EMPTY_TOOLTIP       = I18NPlug.getI18N("ignore-empty-tooltip");
        DIM3                       = I18NPlug.getI18N("dim3");
        DIM3_TOOLTIP               = I18NPlug.getI18N("dim3-tooltip");
        RETURN_GRAPHS_AS           = I18NPlug.getI18N("GraphComponentsPlugIn.return-graphs-as");
        RETURN_GRAPHS_AS_TOOLTIP   = I18NPlug.getI18N("GraphComponentsPlugIn.return-graphs-as-tooltip");
        POINT                      = I18NPlug.getI18N("GraphComponentsPlugIn.point");
        MULTILINESTRING            = I18NPlug.getI18N("GraphComponentsPlugIn.multilinestring");
        SIMPLIFIED_MULTILINESTRING = I18NPlug.getI18N("GraphComponentsPlugIn.simplified-multilinestring");
        GRAPH_ANALYSIS             = I18NPlug.getI18N("GraphComponentsPlugIn.graph-analysis");
        CONNECTED_SUBGRAPH         = I18NPlug.getI18N("GraphComponentsPlugIn.connected-subgraph");
        CONNECTED_SUBGRAPHS        = I18NPlug.getI18N("GraphComponentsPlugIn.connected-subgraphs");
        FEATURES                   = I18NPlug.getI18N("Features");
        PENDANT_VERTICES           = I18NPlug.getI18N("GraphComponentsPlugIn.pendant-vertices");
        LENGTH                     = I18NPlug.getI18N("GraphComponentsPlugIn.longueur");
        PROCESSED_GRAPHS           = I18NPlug.getI18N("GraphComponentsPlugIn.processed-graphs");
        GRAPHS                     = I18NPlug.getI18N("GraphComponentsPlugIn.graphs");
        SUBGRAPHS                  = I18NPlug.getI18N("GraphComponentsPlugIn.subgraphs");
        NO_GRAPH                   = I18NPlug.getI18N("GraphComponentsPlugIn.no-graph");
        
        context.getFeatureInstaller().addMainMenuItem(
          this, new String[]{MenuNames.PLUGINS, GRAPH}, CONNECTED_COMPONENTS + "...",
          false, null, new MultiEnableCheck()
          .add(context.getCheckFactory().createTaskWindowMustBeActiveCheck())
          .add(context.getCheckFactory().createAtLeastNLayersMustExistCheck(1)));
    }
    
    public boolean execute(PlugInContext context) {
        
        final MultiInputDialog dialog = new MultiInputDialog(
        context.getWorkbenchFrame(), GRAPH_ANALYSIS, true);
        
        final JComboBox jcb_layer = dialog.addLayerComboBox(
            LAYER, context.getCandidateLayer(0), null, context.getLayerManager());
        
        final JCheckBox jcb_use_attribute = dialog.addCheckBox(USE_ATTRIBUTE, use_attribute, USE_ATTRIBUTE_TOOLTIP);
        
        List list = getFieldsFromLayerWithoutGeometry(context.getCandidateLayer(0));
        Object val = list.size()>0?list.iterator().next():null;
        final JComboBox jcb_attribute = dialog.addComboBox(ATTRIBUTE, val, list, USE_ATTRIBUTE_TOOLTIP);
        jcb_attribute.setEnabled(false);
        
        final JCheckBox jcb_ignore_empty = dialog.addCheckBox(IGNORE_EMPTY, ignore_empty, IGNORE_EMPTY_TOOLTIP);
        jcb_ignore_empty.setEnabled(false);
        
        dialog.addSeparator();
        
        final JCheckBox jcb_3d = dialog.addCheckBox(DIM3, dim3, DIM3_TOOLTIP);
        jcb_3d.setEnabled(true);
        
        final JComboBox jcb_return_graph_as = dialog.addComboBox(
            RETURN_GRAPHS_AS,
            return_graphs_as,
            Arrays.asList(POINT, MULTILINESTRING, SIMPLIFIED_MULTILINESTRING), RETURN_GRAPHS_AS_TOOLTIP);
        
        dialog.addSeparator();
        
        jcb_layer.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometry(dialog.getLayer(LAYER));
                if (list.size() == 0) {
                    jcb_attribute.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_use_attribute.setEnabled(false);
                    jcb_attribute.setEnabled(false);
                    jcb_ignore_empty.setEnabled(false);
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
            }
        });
        
        GUIUtil.centreOnWindow(dialog);
        dialog.setVisible(true);
        if (dialog.wasOKPressed()) {
            layer = dialog.getLayer(LAYER);
            use_attribute = dialog.getBoolean(USE_ATTRIBUTE);
            attribute = dialog.getText(ATTRIBUTE);
            if (use_attribute) attType = layer.getFeatureCollectionWrapper()
                                              .getFeatureSchema()
                                              .getAttributeType(attribute);
            else attType = AttributeType.STRING;
            ignore_empty = dialog.getBoolean(IGNORE_EMPTY);
            dim3    = dialog.getBoolean(DIM3);
            return_graphs_as = dialog.getText(RETURN_GRAPHS_AS);
            return true;
        }
        else return false;
        
    }
    
    public void run(TaskMonitor monitor, PlugInContext context) throws Exception {
        monitor.allowCancellationRequests();
        monitor.report(GRAPH_ANALYSIS + "...");
        
        FeatureSchema schema_graphs = new FeatureSchema();
        schema_graphs.addAttribute("GEOMETRY", AttributeType.GEOMETRY);
        if (use_attribute) schema_graphs.addAttribute(attribute, attType);
        schema_graphs.addAttribute(CONNECTED_SUBGRAPHS, AttributeType.INTEGER);
        schema_graphs.addAttribute(FEATURES, AttributeType.INTEGER);
        schema_graphs.addAttribute(PENDANT_VERTICES, AttributeType.INTEGER);
        schema_graphs.addAttribute(LENGTH, AttributeType.DOUBLE);
        FeatureCollection graphsFC = new FeatureDataset(schema_graphs);
        
        FeatureSchema schema_subgraphs = new FeatureSchema();
        schema_subgraphs.addAttribute("GEOMETRY", AttributeType.GEOMETRY);
        if (use_attribute) schema_subgraphs.addAttribute(attribute, attType);
        schema_subgraphs.addAttribute(CONNECTED_SUBGRAPH, AttributeType.STRING);
        schema_subgraphs.addAttribute(FEATURES, AttributeType.INTEGER);
        schema_subgraphs.addAttribute(PENDANT_VERTICES, AttributeType.INTEGER);
        schema_subgraphs.addAttribute(LENGTH, AttributeType.DOUBLE);
        FeatureCollection subgraphsFC = new FeatureDataset(schema_subgraphs);
        
        // Order features by attribute value in a map
        Map<Object,List<Feature>> map = new HashMap<Object,List<Feature>>();
        Object key = "NO_ATTRIBUTE_USED";
        for (Iterator i = layer.getFeatureCollectionWrapper().iterator() ; i.hasNext() ; ) {
            Feature f = (Feature)i.next();
            if (use_attribute) key = f.getAttribute(attribute);
            if (use_attribute && ignore_empty &&
                (key == null || key.toString().trim().length() == 0)) {continue;}
            else if (!map.containsKey(key)) {map.put(key, new ArrayList());}
            else {}
            map.get(key).add(f);
        }
        
        int count = 1;
        // Loop on all the graphs to analyse
        for (Iterator i = map.keySet().iterator() ; i.hasNext() ; ) {
            key = i.next();
            // Creates a undirected graph from the feature list
            UndirectedGraph<INode,FeatureAsEdge> graph =
                (UndirectedGraph<INode,FeatureAsEdge>)GraphFactory.createGraph(map.get(key), dim3);
            List list = GraphUtil.createConnectedNodeSets(map.get(key), dim3);
            //if (except_connected && list.size()==1) continue;
            
            // List of connected components
            double graph_length = 0.0;
            int connected_component_number = list.size();
            int total_feature_number = 0;
            int graph_node1_number = 0;
            List<Geometry> graph_geometries = new ArrayList<Geometry>();
            
            for (int j = 0 ; j < list.size() ; j++) {
                Subgraph<INode,FeatureAsEdge,UndirectedGraph<INode,FeatureAsEdge>> sg
                    = new Subgraph<INode,FeatureAsEdge,UndirectedGraph<INode,FeatureAsEdge>>(graph, (Set)list.get(j));
                Set<FeatureAsEdge> edges = sg.edgeSet();
                
                double subgraph_length = 0.0;
                int feature_number = edges.size();
                int subgraph_node1_number = countOrder1Nodes(graph, (Set)list.get(j));
                List<Geometry> subgraph_geometries = new ArrayList<Geometry>();
                for (Iterator<FeatureAsEdge> it = edges.iterator() ; it.hasNext() ; ) {
                    Geometry g = it.next().getGeometry();
                    double length = g.getLength();
                    subgraph_length += length;
                    if (return_graphs_as.equals(POINT)) {
                        subgraph_geometries.add(g);
                    }
                    else if (return_graphs_as.equals(MULTILINESTRING)) {
                        subgraph_geometries.add(g);
                    }
                    else if (return_graphs_as.equals(SIMPLIFIED_MULTILINESTRING)) {
                        subgraph_geometries.add(gf.createLineString(new Coordinate[]{
                                g.getCoordinates()[0],
                                g.getCoordinates()[g.getCoordinates().length-1]}));
                    }
                }
                graph_length += subgraph_length;
                graph_geometries.addAll(subgraph_geometries);
                total_feature_number += feature_number;
                
                Feature newf = new BasicFeature(schema_subgraphs);
                if (return_graphs_as.equals(POINT)) {
                    newf.setGeometry(gf.buildGeometry(subgraph_geometries).getInteriorPoint());
                }
                else {
                    newf.setGeometry(gf.buildGeometry(subgraph_geometries));
                }
                if (use_attribute) newf.setAttribute(attribute, key);
                newf.setAttribute(CONNECTED_SUBGRAPH, ""+(j+1)+"/"+list.size());
                newf.setAttribute(FEATURES, edges.size());
                newf.setAttribute(PENDANT_VERTICES, countOrder1Nodes(graph, (Set)list.get(j)));
                newf.setAttribute(LENGTH, subgraph_length);
                subgraphsFC.add(newf);
            }
            
            Feature newf = new BasicFeature(schema_graphs);
            if (return_graphs_as.equals(POINT)) {
                newf.setGeometry(gf.buildGeometry(graph_geometries).getInteriorPoint());
            }
            else {
                newf.setGeometry(gf.buildGeometry(graph_geometries));
            }
            if (use_attribute) newf.setAttribute(attribute, key);
            newf.setAttribute(CONNECTED_SUBGRAPHS, list.size());
            newf.setAttribute(FEATURES, total_feature_number);
            newf.setAttribute(PENDANT_VERTICES, countOrder1Nodes(graph, graph.vertexSet()));
            newf.setAttribute(LENGTH, graph_length);
            graphsFC.add(newf);
            
            count++;
            monitor.report(count, map.size(), PROCESSED_GRAPHS);
        }
        
        context.getLayerManager().addCategory(StandardCategoryNames.RESULT);
        if (graphsFC.size()>0) {
            context.addLayer(StandardCategoryNames.RESULT, layer.getName()+"-"+GRAPHS, graphsFC);
            context.addLayer(StandardCategoryNames.RESULT, layer.getName()+"-"+SUBGRAPHS, subgraphsFC);
        }
        else {
            context.getWorkbenchFrame().warnUser(NO_GRAPH);
        }
    }
    
    private int countOrder1Nodes(UndirectedGraph graph, Set nodes) {
        int nb = 0;
        for (Iterator it = nodes.iterator() ; it.hasNext() ; ) {
            INode node = (INode)it.next();
            if (graph.degreeOf(node) == 1) nb++;
        }
        return nb;
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

}
