package org.openjump.ext.setattributes;

import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.FeatureEventType;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.UndoableCommand;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.SelectionManager;
import org.apache.log4j.Logger;

import javax.swing.*;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.*;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * A Toolbox containing buttons to set attributes to specific values
 */
@XmlRootElement
public class SetAttributesToolbox {

    final Logger LOG = Logger.getLogger(SetAttributesToolbox.class);

    I18N I18N_ = I18N.getInstance("set_attributes");

    @XmlAttribute (required=false)
    String title = "";

    @XmlAttribute (required=false)
    Integer maxCol = 6;

    @XmlAttribute
    Integer iconWidth = 32;

    @XmlAttribute
    Integer iconHeight = 32;

    @XmlElement (name="button")
    List<SetOfAttributes> buttons;

    public SetAttributesToolbox() {}

    public JDialog createDialog(WorkbenchContext context, File dir) {
        JDialog dialog = new JDialog(context.getWorkbench().getFrame(), title, false);
        dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
        dialog.getContentPane().setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.insets = new Insets(1,1,1,1);
        final PlugInContext pluginContext = context.createPlugInContext();
        for (final SetOfAttributes setOfAttributes : buttons) {
            ImageIcon icon = new ImageIcon(dir.getPath()+"/"+setOfAttributes.icon);
            final JButton button = icon.getImageLoadStatus() == MediaTracker.COMPLETE ?
                    new JButton(icon) :
                    new JButton(setOfAttributes.text);
            button.setMargin(new Insets(0,0,0,0));
            button.setPreferredSize(new Dimension(iconWidth,iconHeight));
            button.setToolTipText(setOfAttributes.getTooltip());
            button.addActionListener(new ActionListener() {
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
                                    layerPattern = Pattern.compile(setOfAttributes.getLayer().substring(1,setOfAttributes.getLayer().length()-1));
                                }
                            } catch(PatternSyntaxException pse) {pse.printStackTrace();}
                        }
                        final Map<Layer,Map<Feature,Feature>> mapSource = new HashMap<Layer,Map<Feature,Feature>>();
                        final Map<Layer,Map<Feature,Feature>> mapTarget = new HashMap<Layer,Map<Feature,Feature>>();
                        for (Layer lyr : selectionManager.getLayersWithSelectedItems()) {
                            if (!lyr.isEditable()) continue;
                            if (layerPattern != null && !layerPattern.matcher(lyr.getName()).matches()) continue;
                            editableLayers++;
                            Map<Feature,Feature> srcLayerMap = new HashMap<Feature,Feature>();
                            Map<Feature,Feature> tgtLayerMap = new HashMap<Feature,Feature>();
                            Collection<Feature> features = selectionManager.getFeaturesWithSelectedItems(lyr);
                            editableFeatures += features.size();
                            for (Feature feature : features) {
                                srcLayerMap.put(feature, feature.clone(false));
                            }
                            tgtLayerMap.putAll(setOfAttributes.setAttributes(selectionManager.getFeaturesWithSelectedItems(lyr)));
                            mapSource.put(lyr,srcLayerMap);
                            mapTarget.put(lyr,tgtLayerMap);
                        }
                        if (editableLayers == 0 || editableFeatures == 0) {
                            pluginContext.getWorkbenchFrame().warnUser(I18N_.getText("set_attributes","SetAttributesPlugIn.no-feature-found"));
                        } else {
                            UndoableCommand command =
                                    new UndoableCommand(I18N.get(SetAttributesPlugIn.class.getName())) {
                                        public void execute() {
                                            for (Layer lyr : mapTarget.keySet()) {
                                                Map<Feature,Feature> map = mapTarget.get(lyr);
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
                                                pluginContext.getLayerManager().fireFeaturesChanged(map.keySet(),FeatureEventType.ATTRIBUTES_MODIFIED, lyr);
                                            }
                                            pluginContext.getLayerViewPanel().repaint();
                                        }

                                        public void unexecute() {
                                            for (Layer lyr : mapSource.keySet()) {
                                                Map<Feature,Feature> map = mapSource.get(lyr);
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
                                                    pluginContext.getLayerManager().fireFeaturesChanged(map.keySet(),FeatureEventType.ATTRIBUTES_MODIFIED, lyr);
                                                }
                                            }
                                            pluginContext.getLayerViewPanel().repaint();
                                        }
                                    };

                            command.execute();
                            pluginContext.getLayerManager().getUndoableEditReceiver().receive(command.toUndoableEdit());
                        }
                    } catch(Exception exc) {
                        LOG.warn(null, exc);
                        pluginContext.getWorkbenchFrame().warnUser(exc.getMessage());
                    } finally {
                        pluginContext.getLayerManager().getUndoableEditReceiver().stopReceiving();
                    }
                }
            });
            dialog.getContentPane().add(button, constraints);
            constraints.gridx += 1;
            if (constraints.gridx >= maxCol) {
                constraints.gridx = 0;
                constraints.gridy += 1;
            }
        }
        return dialog;
    }

    public String getTitle() {
        return title;
    }

    public void addSetOfAttributes(SetOfAttributes set) {
        if (buttons == null) buttons = new ArrayList<SetOfAttributes>();
        buttons.add(set);
    }

    public List<SetOfAttributes> getButtons() {
        return buttons;
    }

}
