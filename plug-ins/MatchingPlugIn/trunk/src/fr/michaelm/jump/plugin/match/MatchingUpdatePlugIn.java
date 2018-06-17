/*
 * (C) 2017 Michaël Michaud
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
 * m.michael.michaud@orange.fr
 */

package fr.michaelm.jump.plugin.match;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.EnableCheck;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.ThreadedBasePlugIn;
import com.vividsolutions.jump.workbench.ui.AttributeTypeFilter;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.MultiTabInputDialog;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.Viewport;
import com.vividsolutions.jump.workbench.ui.renderer.style.RingVertexStyle;

import fr.michaelm.jump.plugin.match.matcher.*;
import org.openjump.core.ui.plugin.tools.aggregate.Aggregator;
import org.openjump.core.ui.plugin.tools.aggregate.Aggregators;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.geom.Point2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JOptionPane;

/**
 * PlugIn to find features from a layer matching features of another layer. 
 * @author Michaël Michaud
 */
public class MatchingUpdatePlugIn extends ThreadedBasePlugIn {
    
    private final String MATCHING                     = I18NPlug.getI18N("Matching");
    private final String MATCHING_OPTIONS             = I18NPlug.getI18N("Matching-options");
    
    private final String MATCHING_UPDATE              = I18NPlug.getI18N("Matching-update");

    // Source layer
    private final String SOURCE_LAYER                 = I18NPlug.getI18N("Source-layer");
    private final String SOURCE_LAYER_TOOLTIP         = I18NPlug.getI18N("Source-layer-tooltip");
    private final String SINGLE_SOURCE                = I18NPlug.getI18N("Single-source");
    private final String SINGLE_SOURCE_TOOLTIP        = I18NPlug.getI18N("Single-source-tooltip");
    
    // Target layer
    private final String TARGET_LAYER                 = I18NPlug.getI18N("Target-layer");
    private final String TARGET_LAYER_TOOLTIP         = I18NPlug.getI18N("Target-layer-tooltip");
    private final String SINGLE_TARGET                = I18NPlug.getI18N("Single-target");
    private final String SINGLE_TARGET_TOOLTIP        = I18NPlug.getI18N("Single-target-tooltip");
    
    // Link layer
    private final String LINK_LAYER                   = I18NPlug.getI18N("Link-layer");
    private final String LINK_LAYER_TOOLTIP           = I18NPlug.getI18N("Link-layer-tooltip");
    
    
    // Output options
    private final String OUTPUT_OPTIONS               = I18NPlug.getI18N("Output-options");
    private final String COPY_MATCHING_FEATURES       = I18NPlug.getI18N("Copy-matching-features");
    private final String COPY_NOT_MATCHING_FEATURES   = I18NPlug.getI18N("Copy-not-matching-features");

    // Attribute transfer / aggregation
    private final String TRANSFER_OPTIONS               = I18NPlug.getI18N("Transfer-options");
    private final String TRANSFER_TO_REFERENCE_LAYER    = I18NPlug.getI18N("Transfer-to-reference-layer");
    private final String TRANSFER_BEST_MATCH_ONLY       = I18NPlug.getI18N("Transfer-best-match-only");
    
    private final String STRING_AGGREGATION             = I18NPlug.getI18N("String-aggregation");
    private final String INTEGER_AGGREGATION            = I18NPlug.getI18N("Integer-aggregation");
    private final String DOUBLE_AGGREGATION             = I18NPlug.getI18N("Double-aggregation");
    private final String DATE_AGGREGATION               = I18NPlug.getI18N("Date-aggregation");

    // Processing and Error messages
    private final String SEARCHING_MATCHES              = I18NPlug.getI18N("Searching-matches");
    private final String MISSING_INPUT_LAYER            = I18NPlug.getI18N("Missing-input-layer");
    private final String WARNING                        = I18NPlug.getI18N("Warning");
    private final String INVALID_LINK_LAYER             = I18NPlug.getI18N("Invalid-link-layer");
    
    
    // Parameters : source layer and cardinality
    private String source_layer_name;
    private boolean single_source = false;
    // Parameters : target layer and cardinality
    private String target_layer_name;
    private boolean single_target = false;
    // Parameters : link layer
    private String link_layer_name;

    // Parameters : output options
    private boolean copy_matching_features = true;
    private boolean copy_not_matching_features;
    //private boolean transfer_option;

    // Parameters : transfer and aggregation
    private boolean transfer = true;
    private boolean transfer_best_match_only = false;
    //private boolean ignore_null = true;
    private Aggregator string_aggregator   =
            Aggregators.getAggregator(new Aggregators.ConcatenateUnique(true).getName());
    private Aggregator integer_aggregator =
            Aggregators.getAggregator(new Aggregators.IntSum().getName());
    private Aggregator double_aggregator =
            Aggregators.getAggregator(new Aggregators.DoubleMean(false).getName());
    private Aggregator date_aggregator =
            Aggregators.getAggregator(new Aggregators.DateMean(true).getName());

    
    public MatchingUpdatePlugIn() {
    }
    
    public String getName() {
        return MATCHING_UPDATE;
    }

    public void initialize(PlugInContext context) throws Exception {
        
        context.getFeatureInstaller().addMainMenuPlugin(
          this, new String[]{MenuNames.PLUGINS, MATCHING},
          MATCHING_UPDATE + "...",
          false, null, getEnableCheck(context));
    }
    
    public void setSourceLayerName(String sourceLayerName) {
        this.source_layer_name = sourceLayerName;
    }
    
    public void setTargetLayerName(String targetLayerName) {
        this.target_layer_name = targetLayerName;
    }
    
    public void setLinkLayerName(String linkLayerName) {
        this.link_layer_name = linkLayerName;
    }

    /**
     * Execute method initialize the plugin interface and get all the
     * parameters from the user.
     */
    public boolean execute(PlugInContext context) throws Exception {
        
        ////////////////////////////////////////////////////////////////////////
        // UI : CREATE MULTITAB INPUT DIALOG
        ////////////////////////////////////////////////////////////////////////
                
        final MultiTabInputDialog dialog = new MultiTabInputDialog(
            context.getWorkbenchFrame(), MATCHING_OPTIONS, MATCHING_OPTIONS, true);
        initDialog(dialog, context);

        GUIUtil.centreOnWindow(dialog);
        dialog.setVisible(true);
        
        if (dialog.wasOKPressed()) {

            // Get source layer parameters
            Layer source_layer = dialog.getLayer(SOURCE_LAYER);
            source_layer_name  = source_layer.getName();
            single_source      = dialog.getBoolean(SINGLE_SOURCE);

            // Get target layer parameters
            Layer target_layer = dialog.getLayer(TARGET_LAYER);
            target_layer_name  = target_layer.getName();
            single_target      = dialog.getBoolean(SINGLE_TARGET);
            
            Layer link_layer = dialog.getLayer(LINK_LAYER);
            link_layer_name  = link_layer.getName();
            
            // Get output options
            copy_matching_features       = dialog.getBoolean(COPY_MATCHING_FEATURES);
            copy_not_matching_features   = dialog.getBoolean(COPY_NOT_MATCHING_FEATURES);
            
            // get transfer options
            transfer                 = dialog.getBoolean(TRANSFER_TO_REFERENCE_LAYER);
            transfer_best_match_only = dialog.getBoolean(TRANSFER_BEST_MATCH_ONLY);
            string_aggregator        = (Aggregator)dialog.getComboBox(STRING_AGGREGATION).getSelectedItem();
            integer_aggregator       = (Aggregator)dialog.getComboBox(INTEGER_AGGREGATION).getSelectedItem();
            double_aggregator        = (Aggregator)dialog.getComboBox(DOUBLE_AGGREGATION).getSelectedItem();
            date_aggregator          = (Aggregator)dialog.getComboBox(DATE_AGGREGATION).getSelectedItem();
            return true;
        }
        else return false;
        
    }
    
    private void initDialog(final MultiTabInputDialog dialog, final PlugInContext context) {
        
        ////////////////////////////////////////////////////////////////////////
        // UI : INITIALIZE LAYERS FROM LAST ONES OR FROM CONTEXT
        ////////////////////////////////////////////////////////////////////////
        
        Layer source_layer;
        Layer target_layer;
        Layer link_layer;
        source_layer = context.getLayerManager().getLayer(source_layer_name);
        if (source_layer == null) source_layer = context.getCandidateLayer(0);
        
        target_layer = context.getLayerManager().getLayer(target_layer_name);
        int layerNumber = context.getLayerManager().getLayers().size();
        if (target_layer == null) target_layer = context.getCandidateLayer(layerNumber>1?1:0);

        link_layer = context.getLayerManager().getLayer(link_layer_name);
        if (link_layer == null) link_layer = context.getCandidateLayer(layerNumber>2?2:0);
        
        ////////////////////////////////////////////////////////////////////////
        // UI : CHOOSE SOURCE LAYER AND SOURCE CARDINALITY
        ////////////////////////////////////////////////////////////////////////

        dialog.addLabel("<html><b>"+MATCHING_OPTIONS+"</b></html>");
        
        final JComboBox jcb_layer = dialog.addLayerComboBox(SOURCE_LAYER, 
            source_layer, SOURCE_LAYER_TOOLTIP, context.getLayerManager());
        jcb_layer.setPreferredSize(new Dimension(220,20));
        jcb_layer.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {updateDialog(dialog);}
        });
        final JCheckBox singleSourceFeatureCheckBox = dialog.addCheckBox(
            SINGLE_SOURCE, single_source, SINGLE_SOURCE_TOOLTIP);

        
        ////////////////////////////////////////////////////////////////////////
        // UI : CHOOSE TARGET LAYER AND SOURCE CARDINALITY
        ////////////////////////////////////////////////////////////////////////
        
        final JComboBox jcb_layer_tgt = dialog.addLayerComboBox(TARGET_LAYER, 
            target_layer, TARGET_LAYER_TOOLTIP, context.getLayerManager());
        jcb_layer_tgt.setPreferredSize(new Dimension(220,20));
        jcb_layer_tgt.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {updateDialog(dialog);}
        });
        final JCheckBox singleTargetFeatureCheckBox = dialog.addCheckBox(
            SINGLE_TARGET, single_target, SINGLE_TARGET_TOOLTIP);
        
        ////////////////////////////////////////////////////////////////////////
        // UI : CHOOSE LINK LAYER
        ////////////////////////////////////////////////////////////////////////
        
        final JComboBox jcb_layer_link = dialog.addLayerComboBox(LINK_LAYER, 
            link_layer, LINK_LAYER_TOOLTIP, context.getLayerManager());
        jcb_layer_link.setPreferredSize(new Dimension(220,20));
        jcb_layer_link.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {updateDialog(dialog);}
        });

        ////////////////////////////////////////////////////////////////////////
        // UI : CHOOSE OUTPUT OPTIONS
        ////////////////////////////////////////////////////////////////////////
        dialog.addSeparator();
        dialog.addLabel("<html><b>"+OUTPUT_OPTIONS+"</b></html>");

        final JCheckBox jcb_new_layer_match = dialog.addCheckBox(COPY_MATCHING_FEATURES, copy_matching_features, null);
        final JCheckBox jcb_new_layer_diff  = dialog.addCheckBox(COPY_NOT_MATCHING_FEATURES, copy_not_matching_features, null);

        ////////////////////////////////////////////////////////////////////////
        // UI : TRANSFER ATTRIBUTE / AGGREGATION
        ////////////////////////////////////////////////////////////////////////
        dialog.addSeparator();
        dialog.addPane(TRANSFER_OPTIONS);
        dialog.addSubTitle(TRANSFER_OPTIONS);
        
        final JCheckBox jcb_transfer = dialog.addCheckBox(TRANSFER_TO_REFERENCE_LAYER, transfer, null);
        jcb_transfer.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {updateDialog(dialog);}
        });
        final JCheckBox jcb_transfer_best_match_only = dialog.addCheckBox(TRANSFER_BEST_MATCH_ONLY, transfer_best_match_only, null);
        jcb_transfer_best_match_only.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {updateDialog(dialog);}
        });

        final JComboBox jcb_string_aggregator = dialog.addComboBox(
                STRING_AGGREGATION, string_aggregator,
                Aggregators.getAggregators(AttributeType.STRING).values(), null
        );
        final JComboBox jcb_integer_aggregator = dialog.addComboBox(
                INTEGER_AGGREGATION, integer_aggregator,
                Aggregators.getAggregators(AttributeType.INTEGER).values(), null
        );
        final JComboBox jcb_double_aggregator = dialog.addComboBox(
                DOUBLE_AGGREGATION, double_aggregator,
                Aggregators.getAggregators(AttributeType.DOUBLE).values(), null
        );
        final JComboBox jcb_date_aggregator = dialog.addComboBox(
                DATE_AGGREGATION, double_aggregator,
                Aggregators.getAggregators(AttributeType.DATE).values(), null
        );
        
        updateDialog(dialog);
    }
    
    
    // update dialog is called by several component listeners to update the 
    // dialog before the final validation
    // 2012-06-29 : component values are stored in local variables as this is 
    // not necessary to change the plugin parameters until final validation
    private void updateDialog(MultiTabInputDialog dialog) {
        
        // Updates related to a layer change
        Layer srcLayer      = dialog.getLayer(SOURCE_LAYER);
        Layer tgtLayer      = dialog.getLayer(TARGET_LAYER);
        Layer lnkLayer      = dialog.getLayer(LINK_LAYER);
        
        boolean srcLayer_has_attributes = 
            AttributeTypeFilter.STRING_FILTER.filter(srcLayer.getFeatureCollectionWrapper().getFeatureSchema()).size() > 0;
        boolean tgtLayer_has_attributes = 
            AttributeTypeFilter.STRING_FILTER.filter(tgtLayer.getFeatureCollectionWrapper().getFeatureSchema()).size() > 0;
        FeatureSchema linkSchema = lnkLayer.getFeatureCollectionWrapper().getFeatureSchema();
        if (!linkSchema.hasAttribute("SOURCE") || 
            !linkSchema.hasAttribute("TARGET") || 
            !linkSchema.hasAttribute("SCORE")) {
            JOptionPane.showMessageDialog(dialog, INVALID_LINK_LAYER, WARNING, JOptionPane.ERROR_MESSAGE);
        }
        
        // Updates related to attribute transfer
        dialog.setTabEnabled(TRANSFER_OPTIONS, srcLayer_has_attributes);
        boolean _transfer = dialog.getBoolean(TRANSFER_TO_REFERENCE_LAYER);
        dialog.setFieldEnabled(TRANSFER_BEST_MATCH_ONLY, _transfer);
        boolean _transfer_best_match_only = dialog.getBoolean(TRANSFER_BEST_MATCH_ONLY);
        dialog.setFieldEnabled(STRING_AGGREGATION, _transfer && !_transfer_best_match_only);
        dialog.setFieldEnabled(INTEGER_AGGREGATION, _transfer && !_transfer_best_match_only);
        dialog.setFieldEnabled(DOUBLE_AGGREGATION, _transfer && !_transfer_best_match_only);
        dialog.setFieldEnabled(DATE_AGGREGATION, _transfer && !_transfer_best_match_only);
        
    }
    
    /**
     * Run executes the main process, looping through matching layer, and
     * looking for candidates in the reference layer.
     */
    public void run(TaskMonitor monitor, PlugInContext context) throws Exception {
        Layer source_layer = context.getLayerManager().getLayer(source_layer_name);
        Layer target_layer = context.getLayerManager().getLayer(target_layer_name);
        if (source_layer == null || target_layer ==  null) {
            context.getWorkbenchFrame().warnUser(MISSING_INPUT_LAYER);
            return;
        }
        Layer link_layer = context.getLayerManager().getLayer(link_layer_name);
        FeatureCollection source_fc = source_layer.getFeatureCollectionWrapper();
        FeatureCollection target_fc = target_layer.getFeatureCollectionWrapper();

        monitor.allowCancellationRequests();
        monitor.report(SEARCHING_MATCHES);
        
        // Replace matching algorithm by link layer reading
        FeatureCollectionMatcher matcher = new FeatureCollectionMatcher(
                source_fc.getFeatures(),
                target_fc.getFeatures(),
                new GeometryMatcher(){
                    public double match(Geometry source, Geometry target, Object context) throws Exception {
                        return 0.0;
                    }
                },
                null, 
                monitor);
        MatchMap matchMap = matcher.getMatchMap();
        Map<Integer,Feature> srcMap = getFeatureMap(source_layer);
        Map<Integer,Feature> tgtMap = getFeatureMap(target_layer);
        for (Object o : link_layer.getFeatureCollectionWrapper().getFeatures()) {
            Feature f = (Feature)o;
            matchMap.add(new Match(
                srcMap.get(f.getAttribute("SOURCE")),
                tgtMap.get(f.getAttribute("TARGET")), 
                f.getDouble(3)));
        }
        Collection<Feature> features = matcher.matchAll(single_source, single_target);
        
        
        if (matcher.interrupted) return;
        if (copy_matching_features) {
            Layer lyr = createLayer(
                features, context,
                source_layer.getName() + "-" + I18NPlug.getI18N("matched"));
            if (lyr != null) setMatchingStyle(lyr);
        }
        if (copy_not_matching_features) {
            Layer lyr = createLayer(
                inverse(source_layer.getFeatureCollectionWrapper(), features), 
                context, 
                source_layer.getName() + "-" + I18NPlug.getI18N("un-matched"));
            if (lyr != null) setNotMatchingStyle(lyr);
        }

        if (transfer) {
            FeatureSchema target_schema = target_fc.getFeatureSchema();
            FeatureSchema new_schema = target_schema.clone();
            if (!new_schema.hasAttribute("X_COUNT")) {
                new_schema.addAttribute("X_COUNT", AttributeType.INTEGER);
            }
            if (!new_schema.hasAttribute("X_MAX_SCORE")) {
                new_schema.addAttribute("X_MAX_SCORE", AttributeType.DOUBLE);
            }
            if (!new_schema.hasAttribute("X_MIN_DISTANCE")) {
                new_schema.addAttribute("X_MIN_DISTANCE", AttributeType.DOUBLE);
            }
            FeatureSchema source_schema = source_fc.getFeatureSchema();
            for (int i = 0 ; i < source_schema.getAttributeCount() ; i++) {
                if (source_schema.getAttributeType(i) != AttributeType.GEOMETRY &&
                    source_schema.getAttributeType(i) != AttributeType.OBJECT) {
                    new_schema.addAttribute(
                        "X_" + source_schema.getAttributeName(i),
                        source_schema.getAttributeType(i));
                }
            }
            FeatureCollection new_dataset = new FeatureDataset(new_schema);
            //MatchMap matchMap = matcher.getMatchMap();
            // If user wants to transfer attributes from the best match only
            // and MatchMap has not yet been filtered by single_source option
            if (transfer_best_match_only && !single_source) {
                matchMap = matchMap.filter(true, false);
            }
            for (Object o : target_fc.getFeatures()) {
                Feature f = (Feature)o;
                Feature bf = new BasicFeature(new_schema);
                Object[] attributes = new Object[new_schema.getAttributeCount()];
                System.arraycopy(f.getAttributes(), 0, attributes, 0, target_schema.getAttributeCount());
                bf.setAttributes(attributes);
                List<Feature> matches = matchMap.getMatchedFeaturesFromTarget(f);
                bf.setAttribute("X_COUNT", matches.size());
                if (matches.isEmpty()) bf.setAttribute("X_MAX_SCORE", 0.0);
                else bf.setAttribute("X_MAX_SCORE", matchMap.getMatchesForTargetFeature(f).iterator().next().getScore());
                double minDistance = Double.NaN;
                for (Match match : matchMap.getMatchesForTargetFeature(f)) {
                    minDistance = Double.isNaN(minDistance) ? 
                        f.getGeometry().distance(match.getSource().getGeometry()) : 
                        Math.min(minDistance, f.getGeometry().distance(match.getSource().getGeometry()));
                }
                if (!Double.isNaN(minDistance)) bf.setAttribute("X_MIN_DISTANCE", minDistance);
                for (int i = 0 ; i < source_schema.getAttributeCount() ; i++) {
                    String name = source_schema.getAttributeName(i);
                    AttributeType type = source_schema.getAttributeType(i);
                    if (type == AttributeType.GEOMETRY) continue;
                    else if (type == AttributeType.OBJECT) continue;
                    else if (type == AttributeType.STRING) {
                        string_aggregator.reset();
                        for (Feature mf : matches) string_aggregator.addValue(mf.getAttribute(i));
                        bf.setAttribute("X_" + name, string_aggregator.getResult());
                    }
                    else if (type == AttributeType.INTEGER) {
                        integer_aggregator.reset();
                        for (Feature mf : matches) integer_aggregator.addValue(mf.getAttribute(i));
                        bf.setAttribute("X_" + name, integer_aggregator.getResult());
                    }
                    else if (type == AttributeType.DOUBLE) {
                        double_aggregator.reset();
                        for (Feature mf : matches) double_aggregator.addValue(mf.getAttribute(i));
                        bf.setAttribute("X_" + name, double_aggregator.getResult());
                    }
                    else if (type == AttributeType.DATE) {
                        date_aggregator.reset();
                        for (Feature mf : matches) date_aggregator.addValue(mf.getAttribute(i));
                        bf.setAttribute("X_" + name, date_aggregator.getResult());
                    }
                }
                new_dataset.add(bf);
            }
            createLayer(new_dataset.getFeatures(), context, target_layer.getName());
        }
    }
    
    private Layer createLayer(Collection<Feature> features, PlugInContext context, String name) {
        if (features.size()>0) {
            FeatureSchema schema = (features.iterator().next()).getSchema();
            FeatureCollection fc = new FeatureDataset(schema);
            fc.addAll(features);
            return context.getLayerManager()
                          .addLayer(StandardCategoryNames.RESULT, name, fc);
        }
        return null;
    }
    
    private Collection<Feature> inverse(FeatureCollection fc, 
                                        Collection<Feature> features) {
        Map<Integer,Feature> map = new HashMap<>();
        for (Feature f : features) map.put(f.getID(), f);
        List<Feature> inverse = new ArrayList<>();
        for (Object o : fc.getFeatures()) {
            if (!map.containsKey(((Feature)o).getID())) inverse.add((Feature)o);    
        }
        return inverse;
    }
    
    private void setMatchingStyle(Layer layer) {
        BasicStyle style = layer.getBasicStyle();
        style.setLineColor(Color.GREEN);
        style.setLineWidth(5);
        style.setAlpha(200);
        style.setFillColor(Color.LIGHT_GRAY);
    }
    
    private void setNotMatchingStyle(Layer layer) {
        BasicStyle style = layer.getBasicStyle();
        style.setLineColor(Color.ORANGE);
        style.setLineWidth(5);
        style.setAlpha(200);
        style.setFillColor(Color.LIGHT_GRAY);
    }
    
    public void setLinkStyle(Layer layer) {
        BasicStyle style = layer.getBasicStyle();
        style.setLineColor(Color.RED);
        style.setLineWidth(2);
        style.setAlpha(255);
        style.setRenderingFill(false);
        layer.addStyle(new MyRingVertexStyle());
        layer.getStyle(MyRingVertexStyle.class).setEnabled(true);
    }
    
    private static class MyRingVertexStyle extends RingVertexStyle {
    
        MyRingVertexStyle() {super();}
    
        public int getSize() {return 25;}
        
        public void paint(Feature f, Graphics2D g, Viewport viewport) throws Exception {
            if (f.getGeometry() instanceof Point) {
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
    
    private Map<Integer,Feature> getFeatureMap(Layer layer) {
        Map<Integer,Feature> map = new HashMap<>();
        for (Object o : layer.getFeatureCollectionWrapper().getFeatures()) {
            Feature f = (Feature)o;
            map.put(f.getID(), f);
        }
        return map;
    }


    private EnableCheck getEnableCheck(final PlugInContext context) {
        return new MultiEnableCheck()
          .add(context.getCheckFactory().createTaskWindowMustBeActiveCheck())
          .add(context.getCheckFactory().createExactlyNLayersMustBeSelectedCheck(1))
          .add(context.getCheckFactory().createAtLeastNLayersMustExistCheck(3))
          .add(context.getCheckFactory().createSelectedLayersMustBeEditableCheck())
          .add(new EnableCheck(){
              @SuppressWarnings("deprecation")
              public String check(JComponent component) {
                  Layer lyr = context.getWorkbenchContext().getLayerableNamePanel().getSelectedLayers()[0];
                  FeatureSchema schema = lyr.getFeatureCollectionWrapper().getFeatureSchema();
                  return schema.hasAttribute("SOURCE") &&
                         schema.hasAttribute("TARGET") &&
                         schema.hasAttribute("SCORE") ? 
                      null : I18NPlug.getI18N("Invalid-link-layer");
              }
          });
    }

}
