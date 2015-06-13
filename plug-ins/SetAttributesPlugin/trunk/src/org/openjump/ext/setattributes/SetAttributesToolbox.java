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

    final I18N I18N_ = I18N.getInstance("set_attributes");

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
            if (setOfAttributes.getAttributes() == null || setOfAttributes.getAttributes().size()==0) {
                button.setEnabled(false);
            }
            button.setMargin(new Insets(0, 0, 0, 0));
            button.setPreferredSize(new Dimension(iconWidth, iconHeight));
            button.setToolTipText(setOfAttributes.getTooltip());

            button.addActionListener(new SetAttributesButtonActionListener(pluginContext, setOfAttributes, unselect));

            button.addMouseListener(new SetAttributesButtonMouseListener(setOfAttributes, button));
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
