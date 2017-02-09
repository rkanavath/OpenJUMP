/*
 * Library offering read and write capabilities for dsv formats
 * Copyright (C) 2017 Michaël MICHAUD
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

package fr.michaelm.jump.drivers.csv;

import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

/**
 * Class creating a chooser from a FeatureSchema to select attributes to be 
 * exported to the CSVFile.
 * @author Micha&euml;l MICHAUD
 * @version 0.6 (2012-03-25)
 * @since 0.6
 */
public class AttributeChooser {

    private final FeatureSchema schema;
    
    public AttributeChooser(FeatureSchema schema) {
        this.schema = schema;
    }
    
    /**
     * Open a MultiInputDialog and returns the list of attribute names selected
     * by the user.
     */
    public List<String> getSelectedAttributes() {
        final MultiInputDialog dialog = 
            new MultiInputDialog(null, I18NPlug.getI18N("drivers.csv.select-attributes"), true);
        dialog.setCancelVisible(false);
        // Workaround to install a JScrollPane in the MultiInputDialog
        JPanel panel = new JPanel(new GridBagLayout());
        JScrollPane scroll = new JScrollPane(panel);
        scroll.setPreferredSize(new Dimension(240, 320));
        dialog.getCurrentPanel().setLayout(new BorderLayout());
        dialog.getCurrentPanel().add(scroll);
        dialog.setCurrentPanel(panel);
        
        JCheckBox[] jcbs = new JCheckBox[schema.getAttributeCount()];
        dialog.addButton(I18NPlug.getI18N("drivers.csv.invert-selection")).addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent e) {
                dialog.getCheckBox("X").setSelected(!dialog.getBoolean("X"));
                dialog.getCheckBox("Y").setSelected(!dialog.getBoolean("Y"));
                dialog.getCheckBox("Z").setSelected(!dialog.getBoolean("Z"));
                for (int i = 0 ; i < schema.getAttributeCount() ; i++) {
                    dialog.getCheckBox(schema.getAttributeName(i))
                        .setSelected(!dialog.getBoolean(schema.getAttributeName(i)));
                }
            }
        });
        dialog.addSeparator();
        dialog.addCheckBox("X", true);
        dialog.addCheckBox("Y", true);
        dialog.addCheckBox("Z", false);
        for (int i = 0 ; i < schema.getAttributeCount() ; i++) {
            if (schema.getAttributeType(i) == AttributeType.GEOMETRY) {
                jcbs[i] = dialog.addCheckBox(schema.getAttributeName(i), false);
            }
            else {
                jcbs[i] = dialog.addCheckBox(schema.getAttributeName(i), true);
            }
        }
        com.vividsolutions.jump.workbench.ui.GUIUtil.centreOnScreen(dialog);
        dialog.setVisible(true);
        if (dialog.wasOKPressed()) {
            List<String> attributes = new ArrayList<String>();
            if (dialog.getBoolean("X")) {attributes.add("X");}
            if (dialog.getBoolean("Y")) {attributes.add("Y");}
            if (dialog.getBoolean("Z")) {attributes.add("Z");}
            for (int i = 0 ; i < schema.getAttributeCount() ; i++) {
                if (dialog.getBoolean(schema.getAttributeName(i))) {
                    attributes.add(schema.getAttributeName(i));
                }
            }
            return attributes;
        }
        else return null;
    }

}
