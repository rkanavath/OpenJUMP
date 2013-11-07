/*
 * Library offering read and write capabilities for dsv formats
 * Copyright (C) 2012 Micha�l MICHAUD
 * michael.michaud@free.fr
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

import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;

import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Map;
import java.util.HashMap;

import com.vividsolutions.jump.io.datasource.DataSourceQuery;
import com.vividsolutions.jump.workbench.datasource.SaveFileDataSourceQueryChooser;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;

import static fr.michaelm.jump.drivers.csv.FieldSeparator.*;

/**
 * SaveFileDataSourceQueryChooser for text formats
 * @author Micha&euml;l Michaud
 * @version 0.6 (2012-03-25)
 */

public class SaveCSVFileDataSourceQueryChooser extends SaveFileDataSourceQueryChooser {

    public SaveCSVFileDataSourceQueryChooser(Class clazz,
                                              String description,
                                              String[] extensions,
                                              WorkbenchContext workbenchContext) {
        super(clazz, description, extensions, workbenchContext);
    }
    

    private JComboBox fieldSeparator = new JComboBox(
        new Object[]{TABULATION,COMMA,SEMI_COLUMN,PIPE,WHITESPACE});

    private JCheckBox dataTypes = new JCheckBox("", false);
    
    private JCheckBox selectAttributes = new JCheckBox("", false);
    
    protected Component getSouthComponent1() {
            JPanel south = new JPanel();
            south.setLayout(new java.awt.GridLayout(2,2));
            
            JPanel fieldSeparatorJP = new JPanel();
            fieldSeparatorJP.add(new JLabel(I18NPlug.getI18N("drivers.csv.field-separator")));
            fieldSeparatorJP.add(fieldSeparator);
            south.add(fieldSeparatorJP);
            
            JPanel exportAttributeTypeJP = new JPanel(new FlowLayout(FlowLayout.RIGHT));
            exportAttributeTypeJP.add(new JLabel(I18NPlug.getI18N("drivers.csv.data-type-line")));
            exportAttributeTypeJP.add(dataTypes);
            exportAttributeTypeJP.setAlignmentY(1f);
            south.add(exportAttributeTypeJP);
            
            south.add(new JPanel()); // empty component
            
            JPanel selectAttributesJP = new JPanel(new FlowLayout(FlowLayout.RIGHT));
            selectAttributesJP.add(new JLabel(I18NPlug.getI18N("drivers.csv.select-attributes")));
            selectAttributesJP.add(selectAttributes);
            selectAttributesJP.setAlignmentY(1f);
            south.add(selectAttributesJP);

            south.setBorder(new TitledBorder(
                new LineBorder(Color.BLACK, 2),
                I18NPlug.getI18N("drivers.csv.options")));
            return south;
    }


    protected Map toProperties(File file) {
        HashMap properties = new HashMap(super.toProperties(file));
        properties.put(CSVDataSource.SAVED_AS, true);
        properties.put(CSVDataSource.CHARSET, Charset.defaultCharset().name());
        properties.put(CSVDataSource.FIELD_SEPARATOR, fieldSeparator.getSelectedItem());
        properties.put(CSVDataSource.DATA_TYPE_LINE, dataTypes.isSelected());
        properties.put(CSVDataSource.ATTRIBUTE_SELECTION, selectAttributes.isSelected());
        return properties;
    }

}

