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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.DefaultComboBoxModel;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.index.strtree.STRtree;

import com.vividsolutions.jump.feature.*;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.*;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;

import fr.michaelm.jump.feature.jgrapht.*;
import org.jgrapht.UndirectedGraph;

/**
 * Creates a graph from a linear layer with JGraphT and returns degree 1 nodes
 * (network dead-end), degree 2 nodes, degree 3+ nodes (intersection) or all
 * the nodes with their degree as attribute.
 * @author Micha&euml;l Michaud
 * @version 0.1 (2010-04-22)
 */
//version 0.1.2 (2011-07-16) typos and comments
//version 0.1.1 (2010-04-22) first svn version
//version 0.1 (2010-04-22)
public class GraphNodesPlugIn extends ThreadedBasePlugIn {
    
    private static String LAYER;
    
    private static String GRAPH;
    private static String NODES;
    private static String DEGREE;
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
    
    Layer layer;
    String attribute;
    boolean use_attribute;
    boolean ignore_empty;
    boolean dim3;
    boolean degree0, degree1, degree2, degree3p;
    
    GeometryFactory gf = new GeometryFactory();
    
    public String getName() {return "Graph nodes PlugIn";}
    
    public void initialize(final PlugInContext context) throws Exception {
        
        GRAPH                 = I18NPlug.getI18N("Graph");
        NODES                 = I18NPlug.getI18N("Nodes");
        DEGREE                = I18NPlug.getI18N("Degree");
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
        
        context.getFeatureInstaller().addMainMenuItem(
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
        dialog.addCheckBox(DIM3, true, DIM3_TOOLTIP);
        dialog.addSeparator();
        
        dialog.addCheckBox(DEGREE0,  false, DEGREE0_TOOLTIP);
        dialog.addCheckBox(DEGREE1,  true,  DEGREE1_TOOLTIP);
        dialog.addCheckBox(DEGREE2,  false, DEGREE2_TOOLTIP);
        dialog.addCheckBox(DEGREE3P, false, DEGREE3P_TOOLTIP);
        
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
            dim3    = dialog.getBoolean(DIM3);
            degree0  = dialog.getBoolean(DEGREE0);
            degree1  = dialog.getBoolean(DEGREE1);
            degree2  = dialog.getBoolean(DEGREE2);
            degree3p = dialog.getBoolean(DEGREE3P);
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
        schemaNodes.addAttribute(DEGREE, AttributeType.INTEGER);
        FeatureCollection resultNodes = new FeatureDataset(schemaNodes);        
        
        // Order features by attribute value in a map
        Map<Object,List> map = new HashMap<Object,List>();
        Object key = "NO_ATTRIBUTE_USED";
        for (Iterator i = fc.iterator() ; i.hasNext() ; ) {
            Feature f = (Feature)i.next();
            if (use_attribute) key = f.getAttribute(attribute);
            if (use_attribute && ignore_empty &&
                (key == null || key.toString().trim().length() == 0)) {continue;}
            else if (!map.containsKey(key)) {map.put(key, new ArrayList());}
            else {}
            map.get(key).add(f);
        }
        
        //int count = 1;
        for (Iterator i = map.keySet().iterator() ; i.hasNext() ; ) {
            key = i.next();
            monitor.report(GRAPH_COMPUTATION + " (" + key + ")");
            UndirectedGraph<INode,FeatureAsEdge> graph =
                (UndirectedGraph<INode,FeatureAsEdge>)GraphFactory.createGraph(map.get(key), dim3);
            //int nbdegree1 = 0;
            for (Iterator<INode> it = graph.vertexSet().iterator() ; it.hasNext() ; ) {
                INode node = it.next();
                int degree = graph.degreeOf(node);
                if (degree == 0 && !degree0) continue;
                if (degree == 1) {
                    //nbdegree1++;
                    if (!degree1) continue;
                }
                if (degree == 2 && !degree2) continue;
                if (degree >  2 && !degree3p) continue;
                Feature bf = new BasicFeature(schemaNodes);
                bf.setGeometry(node.getGeometry());
                if (use_attribute) bf.setAttribute(attribute, key);
                bf.setAttribute(DEGREE, degree);
                resultNodes.add(bf);
            }
        }
        context.getLayerManager().addCategory(StandardCategoryNames.RESULT);
        if (resultNodes.size()>0) {
            context.addLayer(StandardCategoryNames.RESULT, layer.getName()+"-" + NODES, resultNodes);
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

}
