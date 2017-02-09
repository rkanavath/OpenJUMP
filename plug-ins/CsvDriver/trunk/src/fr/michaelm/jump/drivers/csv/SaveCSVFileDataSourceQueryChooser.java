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

import javax.swing.*;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;

import java.awt.*;
import java.io.File;
import java.nio.charset.Charset;
import java.util.Map;
import java.util.HashMap;

import com.vividsolutions.jump.workbench.datasource.SaveFileDataSourceQueryChooser;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import org.openjump.core.ui.swing.factory.field.FieldComponentFactoryRegistry;

import static fr.michaelm.jump.drivers.csv.FieldSeparator.*;

/**
 * SaveFileDataSourceQueryChooser for text formats
 * @author Micha&euml;l Michaud
 * @version 1.0 (2014-06-16)
 */

public class SaveCSVFileDataSourceQueryChooser extends SaveFileDataSourceQueryChooser {

    WorkbenchContext context;

    public SaveCSVFileDataSourceQueryChooser(Class clazz,
                                              String description,
                                              String[] extensions,
                                              WorkbenchContext workbenchContext) {
        super(clazz, description, extensions, workbenchContext);
        this.context = workbenchContext;
    }
    

    private final JComboBox fieldSeparator = new JComboBox(
        new Object[]{TABULATION,COMMA,SEMI_COLUMN,PIPE,WHITESPACE});

    private final JComboBox<Charset> charsetChooser =
        new JComboBox<>(CSVDriverConfiguration.createCommonCharsetArray());

    private final JCheckBox dataTypes = new JCheckBox("", false);
    
    private final JCheckBox selectAttributes = new JCheckBox("", false);
    
    protected Component getSouthComponent1() {
        JPanel south = new JPanel();
        south.setLayout(new java.awt.GridBagLayout());
        GridBagConstraints c = new GridBagConstraints();
        c.insets = new Insets(5,5,10,5);

        c.gridx = 0; c.gridy = 0;
        c.anchor = GridBagConstraints.LINE_END;
        south.add(new JLabel(I18NPlug.getI18N("drivers.csv.field-separator")), c);
        c.gridx = 1; c.gridy = 0;
        c.anchor = GridBagConstraints.LINE_START;
        south.add(fieldSeparator, c);

        c.gridx = 0; c.gridy = 1;
        c.anchor = GridBagConstraints.LINE_END;
        south.add(new JLabel(I18NPlug.getI18N("drivers.csv.encoding")), c);
        c.gridx = 1; c.gridy = 1;
        c.anchor = GridBagConstraints.LINE_START;
        south.add(charsetChooser, c);

        c.gridx = 3; c.gridy = 0;
        c.anchor = GridBagConstraints.LINE_END;
        south.add(new JLabel(I18NPlug.getI18N("drivers.csv.data-type-line")), c);
        c.gridx = 4; c.gridy = 0;
        c.anchor = GridBagConstraints.LINE_START;
        south.add(dataTypes, c);

        c.gridx = 3; c.gridy = 1;
        c.anchor = GridBagConstraints.LINE_END;
        south.add(new JLabel(I18NPlug.getI18N("drivers.csv.select-attributes")), c);
        c.gridx = 4; c.gridy = 1;
        c.anchor = GridBagConstraints.LINE_START;
        south.add(selectAttributes, c);

        south.setBorder(new TitledBorder(new LineBorder(Color.BLACK, 2), I18NPlug.getI18N("drivers.csv.options")));
        return south;
    }


    protected Map toProperties(File file) {
        HashMap properties = new HashMap(super.toProperties(file));
        properties.put(CSVDataSource.SAVED_AS, true);
        properties.put(CSVDataSource.CHARSET, charsetChooser.getSelectedItem());
        properties.put(CSVDataSource.FIELD_SEPARATOR, fieldSeparator.getSelectedItem());
        properties.put(CSVDataSource.DATA_TYPE_LINE, dataTypes.isSelected());
        properties.put(CSVDataSource.ATTRIBUTE_SELECTION, selectAttributes.isSelected());
        return properties;
    }

}

