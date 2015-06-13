package org.openjump.ext.setattributes;

import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.workbench.model.FeatureEventType;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.UndoableCommand;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.SelectionManager;
import org.apache.log4j.Logger;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Defines what happens when the user click on a SetAttributes button
 */
public class SetAttributesButtonActionListener implements ActionListener {

    final Logger LOG = Logger.getLogger(SetAttributesToolbox.class);
    final I18N I18N_ = I18N.getInstance("set_attributes");

    final PlugInContext pluginContext;
    final SetOfAttributes setOfAttributes;
    final boolean unselect;

    SetAttributesButtonActionListener(final PlugInContext pluginContext,
                                     final SetOfAttributes setOfAttributes,
                                     final boolean unselect) {
        this.pluginContext = pluginContext;
        this.setOfAttributes = setOfAttributes;
        this.unselect = unselect;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        SelectionManager selectionManager = pluginContext.getLayerViewPanel().getSelectionManager();
        pluginContext.getLayerManager().getUndoableEditReceiver().startReceiving();
        try {
            int editableLayers = 0;
            int editableFeatures = 0;
            Pattern layerPattern = null;
            // If layer is not null
            if (setOfAttributes.getLayer() != null) {
                try {
                    // create a pattern to read this specific layer name
                    layerPattern = Pattern.compile(Pattern.quote(setOfAttributes.getLayer()));
                    // then try to interpret * as glob
                    if (setOfAttributes.getLayer().contains("*")) {
                        layerPattern = Pattern.compile(setOfAttributes.getLayer().replaceAll("\\*", ".*"));
                    }
                    // and finally, try to read layer name as a regex if it starts and ends with a /
                    if (setOfAttributes.getLayer().startsWith("/") && setOfAttributes.getLayer().endsWith("/")) {
                        layerPattern = Pattern.compile(setOfAttributes.getLayer().substring(1, setOfAttributes.getLayer().length() - 1));
                    }
                } catch (PatternSyntaxException pse) {
                    pse.printStackTrace();
                }
            }
            final Map<Layer, Map<Feature, Feature>> mapSource = new HashMap<Layer, Map<Feature, Feature>>();
            final Map<Layer, Map<Feature, Feature>> mapTarget = new HashMap<Layer, Map<Feature, Feature>>();
            for (Layer lyr : selectionManager.getLayersWithSelectedItems()) {
                if (!lyr.isEditable()) continue;
                if (layerPattern != null && !layerPattern.matcher(lyr.getName()).matches()) continue;
                editableLayers++;
                Map<Feature, Feature> srcLayerMap = new HashMap<Feature, Feature>();
                Map<Feature, Feature> tgtLayerMap = new HashMap<Feature, Feature>();
                Collection<Feature> features = selectionManager.getFeaturesWithSelectedItems(lyr);
                editableFeatures += features.size();
                for (Feature feature : features) {
                    srcLayerMap.put(feature, feature.clone(false));
                }
                tgtLayerMap.putAll(setOfAttributes.setAttributes(selectionManager.getFeaturesWithSelectedItems(lyr), lyr.getName()));
                mapSource.put(lyr, srcLayerMap);
                mapTarget.put(lyr, tgtLayerMap);
            }
            if (unselect) {
                for (Layer lyr : selectionManager.getLayersWithSelectedItems()) {
                    selectionManager.unselectItems(lyr);
                }
            }
            if (editableLayers == 0 && setOfAttributes.getLayer() == null) {
                pluginContext.getWorkbenchFrame().warnUser(I18N_.getText("set_attributes",
                        "SetAttributesPlugIn.no-feature-found"));
            } else if (editableLayers == 0) {
                pluginContext.getWorkbenchFrame().warnUser(I18N_.getMessage("set_attributes",
                        "SetAttributesPlugIn.no-feature-found-in-layer",
                        new Object[]{setOfAttributes.getLayer()}));
            } else if (editableFeatures == 0 && setOfAttributes.getLayer() == null) {
                pluginContext.getWorkbenchFrame().warnUser(I18N_.getText("set_attributes",
                        "SetAttributesPlugIn.no-feature-found"));
            } else if (editableFeatures == 0) {
                pluginContext.getWorkbenchFrame().warnUser(I18N_.getMessage("set_attributes",
                        "SetAttributesPlugIn.no-feature-found-in-layer",
                        new Object[]{setOfAttributes.getLayer()}));
            } else {
                UndoableCommand command =
                        new UndoableCommand(I18N.get(SetAttributesPlugIn.class.getName())) {
                            public void execute() {
                                for (Layer lyr : mapTarget.keySet()) {
                                    Map<Feature, Feature> map = mapTarget.get(lyr);
                                    for (Feature feature : map.keySet()) {
                                        Feature newFeature = map.get(feature);
                                        FeatureSchema schema = feature.getSchema();
                                        for (SetAttribute setAtt : setOfAttributes.attributes) {
                                            String name = setAtt.getName();
                                            if (schema.hasAttribute(name)) {
                                                feature.setAttribute(name, newFeature.getAttribute(name));
                                            }
                                        }
                                    }
                                    //pluginContext.getLayerManager().fireFeaturesAttChanged(map.keySet(), FeatureEventType.ATTRIBUTES_MODIFIED, lyr, map.keySet());
                                    pluginContext.getLayerManager().fireFeaturesChanged(map.keySet(), FeatureEventType.ATTRIBUTES_MODIFIED, lyr);
                                }
                                pluginContext.getLayerViewPanel().repaint();
                            }

                            public void unexecute() {
                                for (Layer lyr : mapSource.keySet()) {
                                    Map<Feature, Feature> map = mapSource.get(lyr);
                                    for (Feature feature : map.keySet()) {
                                        Feature newFeature = map.get(feature);
                                        FeatureSchema schema = feature.getSchema();
                                        for (SetAttribute setAtt : setOfAttributes.attributes) {
                                            String name = setAtt.getName();
                                            if (schema.hasAttribute(name)) {
                                                feature.setAttribute(name, newFeature.getAttribute(name));
                                            }
                                        }
                                        //pluginContext.getLayerManager().fireFeaturesAttChanged(map.keySet(), FeatureEventType.ATTRIBUTES_MODIFIED, lyr, map.keySet());
                                        pluginContext.getLayerManager().fireFeaturesChanged(map.keySet(), FeatureEventType.ATTRIBUTES_MODIFIED, lyr);
                                    }
                                }
                                pluginContext.getLayerViewPanel().repaint();
                            }
                        };

                command.execute();
                pluginContext.getLayerManager().getUndoableEditReceiver().receive(command.toUndoableEdit());
            }
        } catch (Exception exc) {
            LOG.warn(null, exc);
            pluginContext.getWorkbenchFrame().warnUser(exc.getMessage());
        } finally {
            pluginContext.getLayerManager().getUndoableEditReceiver().stopReceiving();
        }
    }
}
