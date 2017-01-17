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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.DefaultComboBoxModel;

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
import org.jgrapht.DirectedGraph;

/**
 * Creates a graph from a linear layer with JGraphT and returns degree 1 nodes
 * (network dead-end), degree 2 nodes, degree 3+ nodes (intersection) or all
 * the nodes with their degree as attribute.
 * @author Micha&euml;l Michaud
 * @version 0.4.0 (2017-01-17)
 */
//version 0.4.0 (2017-01-17) directedGraph support
//version 0.1.2 (2011-07-16) typos and comments
//version 0.1.1 (2010-04-22) first svn version
//version 0.1 (2010-04-22)
public class GraphNodesPlugIn extends ThreadedBasePlugIn {

    private static String LAYER;

    private static String GRAPH;
    private static String NODES;
    private static String DEGREE;
    private static String IN_DEGREE;
    private static String OUT_DEGREE;
    private static String GRAPH_NODES;
    private static String GRAPH_COMPUTATION;
    
    private static String USE_ATTRIBUTE;
    private static String USE_ATTRIBUTE_TOOLTIP;
    
    private static String ATTRIBUTE;
    
    private static String IGNORE_EMPTY;
    private static String IGNORE_EMPTY_TOOLTIP;
    
    private static String DIM3;
    private static String DIM3_TOOLTIP;

    private static String DEGREE0;
    private static String DEGREE0_TOOLTIP;
    private static String DEGREE1;
    private static String DEGREE1_TOOLTIP;
    private static String DEGREE2;
    private static String DEGREE2_TOOLTIP;
    private static String DEGREE3P;
    private static String DEGREE3P_TOOLTIP;
    private static String DIRECTED_AND;
    private static String IN_DEGREE0;
    private static String OUT_DEGREE0;

    private static String NO_NODE_FOUND;
    
    Layer layer;
    String attribute;
    boolean use_attribute;
    boolean ignore_empty;
    boolean dim3;
    boolean indegree0  = false;
    boolean outdegree0 = false;
    boolean degree0    = false;
    boolean degree1    = true;
    boolean degree2    = false;
    boolean degree3p   = false;

    public String getName() {return "Graph nodes PlugIn";}
    
    public void initialize(final PlugInContext context) throws Exception {

        LAYER                 = I18NPlug.getI18N("Layer");
        GRAPH                 = I18NPlug.getI18N("Graph");
        NODES                 = I18NPlug.getI18N("Nodes");
        DEGREE                = I18NPlug.getI18N("Degree");
        IN_DEGREE             = I18NPlug.getI18N("InDegree");
        OUT_DEGREE            = I18NPlug.getI18N("OutDegree");
        GRAPH_COMPUTATION     = I18NPlug.getI18N("Graph-computation");
        GRAPH_NODES           = I18NPlug.getI18N("GraphNodesPlugIn.graph-nodes");
        USE_ATTRIBUTE         = I18NPlug.getI18N("use-attribute");
        USE_ATTRIBUTE_TOOLTIP = I18NPlug.getI18N("use-attribute-tooltip");
        ATTRIBUTE             = I18NPlug.getI18N("Attribute");
        IGNORE_EMPTY          = I18NPlug.getI18N("ignore-empty");
        IGNORE_EMPTY_TOOLTIP  = I18NPlug.getI18N("ignore-empty-tooltip");
        DIM3                  = I18NPlug.getI18N("dim3");
        DIM3_TOOLTIP          = I18NPlug.getI18N("dim3-tooltip");
        DEGREE0               = I18NPlug.getI18N("GraphNodesPlugIn.degree0");
        DEGREE0_TOOLTIP       = I18NPlug.getI18N("GraphNodesPlugIn.degree0-tooltip");
        DEGREE1               = I18NPlug.getI18N("GraphNodesPlugIn.degree1");
        DEGREE1_TOOLTIP       = I18NPlug.getI18N("GraphNodesPlugIn.degree1-tooltip");
        DEGREE2               = I18NPlug.getI18N("GraphNodesPlugIn.degree2");
        DEGREE2_TOOLTIP       = I18NPlug.getI18N("GraphNodesPlugIn.degree2-tooltip");
        DEGREE3P              = I18NPlug.getI18N("GraphNodesPlugIn.degree3p");
        DEGREE3P_TOOLTIP      = I18NPlug.getI18N("GraphNodesPlugIn.degree3p-tooltip");
        DIRECTED_AND          = I18NPlug.getI18N("GraphNodesPlugIn.directed-and");
        IN_DEGREE0            = I18NPlug.getI18N("GraphNodesPlugIn.in-degree0");
        OUT_DEGREE0           = I18NPlug.getI18N("GraphNodesPlugIn.out-degree0");
        NO_NODE_FOUND         = I18NPlug.getI18N("GraphNodesPlugIn.no-node-found");
        
        context.getFeatureInstaller().addMainMenuPlugin(
          this, new String[]{MenuNames.PLUGINS, GRAPH},
          GRAPH_NODES + "...",
          false, null, new MultiEnableCheck()
          .add(context.getCheckFactory().createTaskWindowMustBeActiveCheck())
          .add(context.getCheckFactory().createAtLeastNLayersMustExistCheck(1)));
    }
    
    public boolean execute(PlugInContext context) {
        
        final MultiInputDialog dialog = new MultiInputDialog(
        context.getWorkbenchFrame(), GRAPH_NODES, true);
        
        final JComboBox jcb_layer = dialog.addLayerComboBox(
            LAYER, context.getCandidateLayer(0), null, context.getLayerManager());
        
        final JCheckBox jcb_use_attribute = dialog.addCheckBox(USE_ATTRIBUTE, false, USE_ATTRIBUTE_TOOLTIP);
        
        List list = getFieldsFromLayerWithoutGeometry(context.getCandidateLayer(0));
        Object val = list.size()>0?list.iterator().next():null;
        final JComboBox jcb_attribute = dialog.addComboBox(ATTRIBUTE, val, list, USE_ATTRIBUTE_TOOLTIP);
        jcb_attribute.setEnabled(false);
        
        final JCheckBox jcb_ignore_empty = dialog.addCheckBox(IGNORE_EMPTY, false, IGNORE_EMPTY_TOOLTIP);
        jcb_ignore_empty.setEnabled(false);
        
        dialog.addSeparator();

        dialog.addCheckBox(DIM3, dim3, DIM3_TOOLTIP);

        dialog.addSeparator();

        dialog.addCheckBox(DEGREE0,      degree0,    DEGREE0_TOOLTIP);
        dialog.addCheckBox(DEGREE1,      degree1,    DEGREE1_TOOLTIP);
        dialog.addCheckBox(DEGREE2,      degree2,    DEGREE2_TOOLTIP);
        dialog.addCheckBox(DEGREE3P,     degree3p,   DEGREE3P_TOOLTIP);

        dialog.addSubTitle(DIRECTED_AND);

        dialog.addCheckBox(IN_DEGREE0,   indegree0,  IN_DEGREE0);
        dialog.addCheckBox(OUT_DEGREE0,  outdegree0, OUT_DEGREE0);
        
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
            ignore_empty = dialog.getBoolean(IGNORE_EMPTY);
            dim3       = dialog.getBoolean(DIM3);
            indegree0  = dialog.getBoolean(IN_DEGREE0);
            outdegree0 = dialog.getBoolean(OUT_DEGREE0);
            degree0    = dialog.getBoolean(DEGREE0);
            degree1    = dialog.getBoolean(DEGREE1);
            degree2    = dialog.getBoolean(DEGREE2);
            degree3p   = dialog.getBoolean(DEGREE3P);
            return true;
        }
        else return false;
        
    }
    
    public void run(TaskMonitor monitor, PlugInContext context) throws Exception {
        monitor.allowCancellationRequests();
        monitor.report(GRAPH_COMPUTATION + "...");
        
        FeatureCollection fc = layer.getFeatureCollectionWrapper();
        
        // Creates the schema for the output dataset (nodes)
        FeatureSchema schemaNodes = new FeatureSchema();
        schemaNodes.addAttribute("GEOMETRY", AttributeType.GEOMETRY);
        if (use_attribute) {
            schemaNodes.addAttribute(attribute, fc.getFeatureSchema().getAttributeType(attribute));
        }
        schemaNodes.addAttribute(IN_DEGREE, AttributeType.INTEGER);
        schemaNodes.addAttribute(OUT_DEGREE, AttributeType.INTEGER);
        schemaNodes.addAttribute(DEGREE, AttributeType.INTEGER);

        FeatureCollection resultNodes = new FeatureDataset(schemaNodes);        
        
        // Order features by attribute value in a map
        Map<Object,List<Feature>> map = new HashMap<Object,List<Feature>>();
        Object key = "NO_ATTRIBUTE_USED";
        for (Iterator i = fc.iterator() ; i.hasNext() ; ) {
            Feature f = (Feature)i.next();
            if (use_attribute) key = f.getAttribute(attribute);
            if (use_attribute && ignore_empty &&
                (key == null || key.toString().trim().length() == 0)) {continue;}
            else if (!map.containsKey(key)) {map.put(key, new ArrayList<Feature>());}
            else {}
            map.get(key).add(f);
        }
        
        //int count = 1;
        for (Object k : map.keySet()) {
            monitor.report(GRAPH_COMPUTATION + " (" + k + ")");
            DirectedGraph graph = (DirectedGraph)GraphFactory.createDirectedPseudograph(map.get(k), dim3);

            for (Iterator<INode> it = graph.vertexSet().iterator() ; it.hasNext() ; ) {
                INode node = it.next();
                int indegree =  graph.inDegreeOf(node);
                int outdegree = graph.outDegreeOf(node);
                int degree = indegree + outdegree;
                if (degree0 && degree == 0 ||
                        degree1 && degree == 1 ||
                        degree2 && degree == 2 ||
                        degree3p && degree > 2) {
                    if (indegree0 && indegree != 0) continue;
                    if (outdegree0 && outdegree != 0) continue;
                    Feature bf = new BasicFeature(schemaNodes);
                    bf.setGeometry(node.getGeometry());
                    if (use_attribute) bf.setAttribute(attribute, k);
                    bf.setAttribute(IN_DEGREE, indegree);
                    bf.setAttribute(OUT_DEGREE, outdegree);
                    bf.setAttribute(DEGREE, degree);
                    resultNodes.add(bf);

                }
            }
        }
        context.getLayerManager().addCategory(StandardCategoryNames.RESULT);
        if (resultNodes.size()>0) {
            context.addLayer(StandardCategoryNames.RESULT, layer.getName()+"-" + NODES, resultNodes);
        } else {
            context.getWorkbenchFrame().warnUser(NO_NODE_FOUND);
        }
    }
    
    private List<String> getFieldsFromLayerWithoutGeometry(Layer l) {
        List<String> fields = new ArrayList<String>();
        FeatureSchema schema = l.getFeatureCollectionWrapper().getFeatureSchema();
        for (int i = 0 ; i < schema.getAttributeCount() ; i++) {
        	if (schema.getAttributeType(i) != AttributeType.GEOMETRY) {
        	    fields.add(schema.getAttributeName(i));  
            }
        }
        return fields;
    }

}
