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
import java.awt.event.*;
import java.io.File;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
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

    @XmlAttribute (required=false)
    boolean unselect;

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
            final JButton button;
            if (icon.getImageLoadStatus() == MediaTracker.COMPLETE) {
                button = new JButton(icon);
            } else {
                button = new JButton(setOfAttributes.text);
                if (setOfAttributes.getBackgroundColor()!=null) {
                    button.setBackground(setOfAttributes.getBackgroundColor());
                    //button.setContentAreaFilled(false);
                    button.setOpaque(true);
                }
            }
            button.setMargin(new Insets(0, 0, 0, 0));
            button.setPreferredSize(new Dimension(iconWidth, iconHeight));
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
            });

            button.addMouseListener(new MouseAdapter() {

                final int defaultDismissTimeout = ToolTipManager.sharedInstance().getDismissDelay();

                @Override
                public void mousePressed(MouseEvent e) {
                    int defaultDismissTimeout = ToolTipManager.sharedInstance().getDismissDelay();

                    if (SwingUtilities.isRightMouseButton(e)) {
                        ToolTipManager.sharedInstance().setDismissDelay(10000);
                        button.setToolTipText(getHtmlToolTip(setOfAttributes));
                        MouseEvent phantom = new MouseEvent(
                                button,
                                MouseEvent.MOUSE_PRESSED,
                                System.currentTimeMillis(),
                                0,
                                0,
                                0,
                                0,
                                false);
                        ToolTipManager.sharedInstance().mouseMoved(phantom);
                    }
                }

                @Override
                public void mouseReleased(MouseEvent e) {
                    if (SwingUtilities.isRightMouseButton(e)) {
                        button.setToolTipText(setOfAttributes.getTooltip());
                        ToolTipManager.sharedInstance().setDismissDelay(defaultDismissTimeout);
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

    public String getHtmlToolTip(SetOfAttributes button) {
        StringBuilder text = new StringBuilder("<html>");
        text.append("<b>");
        text.append(I18N_.getText("set_attributes", "SetAttributesPlugIn.layer"));
        text.append("</b> : ");
        text.append(forHTML(button.getLayer()));

        text.append("<br><b>");
        text.append(I18N_.getText("set_attributes", "SetAttributesPlugIn.transaction-mode"));
        text.append("</b> : ");
        text.append(button.isAtomic() ?
                I18N_.getText("set_attributes", "SetAttributesPlugIn.transaction-mode.atomic") :
                I18N_.getText("set_attributes", "SetAttributesPlugIn.transaction-mode.normal"));

        if (button.getDimension() > -1) {
            text.append("<br><b>");
            text.append(I18N_.getText("set_attributes", "SetAttributesPlugIn.accepted-geometry-dimension"));
            text.append("</b> : ");
            text.append(button.getDimension());
        }

        text.append("<br><b>");
        text.append(I18N_.getText("set_attributes", "SetAttributesPlugIn.attributes-changed"));
        text.append("</b> :");
        text.append("<ul>");
        for (SetAttribute att : button.getAttributes()) {
            text.append("<li><b>").append(att.getName()).append("</b> : ").append(att.getValue());
            if (att.getPrerequisite() != null && att.getPrerequisite().length() > 0) {
                text.append("<i>[");
                text.append(I18N_.getText("set_attributes", "SetAttributesPlugIn.attributes-changed.if"));
                text.append(" ");
                text.append(forHTML(att.getPrerequisite()));
                text.append("]</i>");
            }
            text.append("</li>");
        }
        text.append("</ul></html>");
        return text.toString();
    }

    /**
     Escape characters for text appearing in HTML markup.

     <P>This method exists as a defence against Cross Site Scripting (XSS) hacks.
     The idea is to neutralize control characters commonly used by scripts, such that
     they will not be executed by the browser. This is done by replacing the control
     characters with their escaped equivalents.

     <P>The following characters are replaced with corresponding
     HTML character entities :
     <table border='1' cellpadding='3' cellspacing='0'>
     <tr><th> Character </th><th>Replacement</th></tr>
     <tr><td> < </td><td> &lt; </td></tr>
     <tr><td> > </td><td> &gt; </td></tr>
     <tr><td> & </td><td> &amp; </td></tr>
     <tr><td> " </td><td> &quot;</td></tr>
     <tr><td> \t </td><td> &#009;</td></tr>
     <tr><td> ! </td><td> &#033;</td></tr>
     <tr><td> # </td><td> &#035;</td></tr>
     <tr><td> $ </td><td> &#036;</td></tr>
     <tr><td> % </td><td> &#037;</td></tr>
     <tr><td> ' </td><td> &#039;</td></tr>
     <tr><td> ( </td><td> &#040;</td></tr>
     <tr><td> ) </td><td> &#041;</td></tr>
     <tr><td> * </td><td> &#042;</td></tr>
     <tr><td> + </td><td> &#043; </td></tr>
     <tr><td> , </td><td> &#044; </td></tr>
     <tr><td> - </td><td> &#045; </td></tr>
     <tr><td> . </td><td> &#046; </td></tr>
     <tr><td> / </td><td> &#047; </td></tr>
     <tr><td> : </td><td> &#058;</td></tr>
     <tr><td> ; </td><td> &#059;</td></tr>
     <tr><td> = </td><td> &#061;</td></tr>
     <tr><td> ? </td><td> &#063;</td></tr>
     <tr><td> @ </td><td> &#064;</td></tr>
     <tr><td> [ </td><td> &#091;</td></tr>
     <tr><td> \ </td><td> &#092;</td></tr>
     <tr><td> ] </td><td> &#093;</td></tr>
     <tr><td> ^ </td><td> &#094;</td></tr>
     <tr><td> _ </td><td> &#095;</td></tr>
     <tr><td> ` </td><td> &#096;</td></tr>
     <tr><td> { </td><td> &#123;</td></tr>
     <tr><td> | </td><td> &#124;</td></tr>
     <tr><td> } </td><td> &#125;</td></tr>
     <tr><td> ~ </td><td> &#126;</td></tr>
     </table>
     */
    public static String forHTML(String aText){
        final StringBuilder result = new StringBuilder();
        final StringCharacterIterator iterator = new StringCharacterIterator(aText);
        char character =  iterator.current();
        while (character != CharacterIterator.DONE ){
            if (character == '<') {
                result.append("&lt;");
            }
            else if (character == '>') {
                result.append("&gt;");
            }
            else if (character == '&') {
                result.append("&amp;");
            }
            else if (character == '\"') {
                result.append("&quot;");
            }
            else if (character == '\t') {
                addCharEntity(9, result);
            }
            else if (character == '!') {
                addCharEntity(33, result);
            }
            else if (character == '#') {
                addCharEntity(35, result);
            }
            else if (character == '$') {
                addCharEntity(36, result);
            }
            else if (character == '%') {
                addCharEntity(37, result);
            }
            else if (character == '\'') {
                addCharEntity(39, result);
            }
            else if (character == '(') {
                addCharEntity(40, result);
            }
            else if (character == ')') {
                addCharEntity(41, result);
            }
            else if (character == '*') {
                addCharEntity(42, result);
            }
            else if (character == '+') {
                addCharEntity(43, result);
            }
            else if (character == ',') {
                addCharEntity(44, result);
            }
            else if (character == '-') {
                addCharEntity(45, result);
            }
            else if (character == '.') {
                addCharEntity(46, result);
            }
            else if (character == '/') {
                addCharEntity(47, result);
            }
            else if (character == ':') {
                addCharEntity(58, result);
            }
            else if (character == ';') {
                addCharEntity(59, result);
            }
            else if (character == '=') {
                addCharEntity(61, result);
            }
            else if (character == '?') {
                addCharEntity(63, result);
            }
            else if (character == '@') {
                addCharEntity(64, result);
            }
            else if (character == '[') {
                addCharEntity(91, result);
            }
            else if (character == '\\') {
                addCharEntity(92, result);
            }
            else if (character == ']') {
                addCharEntity(93, result);
            }
            else if (character == '^') {
                addCharEntity(94, result);
            }
            else if (character == '_') {
                addCharEntity(95, result);
            }
            else if (character == '`') {
                addCharEntity(96, result);
            }
            else if (character == '{') {
                addCharEntity(123, result);
            }
            else if (character == '|') {
                addCharEntity(124, result);
            }
            else if (character == '}') {
                addCharEntity(125, result);
            }
            else if (character == '~') {
                addCharEntity(126, result);
            }
            else {
                //the char is not a special one
                //add it to the result as is
                result.append(character);
            }
            character = iterator.next();
        }
        return result.toString();
    }

    private static void addCharEntity(Integer aIdx, StringBuilder aBuilder){
        String padding = "";
        if( aIdx <= 9 ){
            padding = "00";
        }
        else if( aIdx <= 99 ){
            padding = "0";
        }
        else {
            //no prefix
        }
        String number = padding + aIdx.toString();
        aBuilder.append("&#" + number + ";");
    }
}
