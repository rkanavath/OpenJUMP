/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * Copyright (C) 2006 Cadplan
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
 */
package com.cadplan.jump;

import com.cadplan.designer.GridBagDesigner;

import javax.swing.*;

import java.awt.*;

/**
 * User: geoff
 * Date: 10/01/2007
 * Time: 09:43:30
 * Copyright 2005 Geoffrey G Roy.
 */
public class FurnitureTitlePanel extends JPanel
{
    FurnitureTitle title;
    JTextField textField, layerField;
    JComboBox fontNameCombo, fontSizeCombo, fontStyleCombo;
    JCheckBox showCB;
    JLabel textLabel, fontLabel, sizeLabel, styleLabel, layerLabel;
    ColorButton colorButton;
    String [] styles;
    String [] sizes = {"6","7","8","9","10","12","14","16","18","20","24","28","32","36","48","64","72","84","96","108","120","132","144","156"};
    I18NPlug iPlug;

    public FurnitureTitlePanel(FurnitureTitle title, I18NPlug iPlug)
    {
        this.title = title;
        this.iPlug = iPlug;
        styles = new String [] {iPlug.get("JumpPrinter.Furniture.Title.Plain"),
                        iPlug.get("JumpPrinter.Furniture.Title.PlainItalic"),
                        iPlug.get("JumpPrinter.Furniture.Title.Bold"),
                        iPlug.get("JumpPrinter.Furniture.Title.BoldItalic")};
        init();
    }

    public void init()
    {
       GridBagDesigner gb = new GridBagDesigner(this);

       showCB = new JCheckBox(iPlug.get("JumpPrinter.Furniture.Show"));
       gb.setPosition(0,0);
       gb.setInsets(10,10,0,0);
       gb.setAnchor(GridBagConstraints.WEST);
       gb.addComponent(showCB);
       
        textLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Title.Title"));
        gb.setPosition(1,0);
        gb.setInsets(10,10,0,0);
        gb.addComponent(textLabel);

        textField = new JTextField(30);
        gb.setPosition(2,0);
        gb.setInsets(10,5,0,0);
        gb.setSpan(3,1);
        gb.setFill(GridBagConstraints.HORIZONTAL);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(textField);

        fontLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Title.Font"));
        gb.setPosition(1,1);
        gb.setInsets(10,10,0,0);
        gb.addComponent(fontLabel);

        
        fontNameCombo = new JComboBox(GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames());
        gb.setPosition(2,1);
        gb.setInsets(10,5,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.setSpan(1,1);
        gb.addComponent(fontNameCombo);

        //sizeLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Title.Size"));
        //gb.setPosition(0,3);
       // gb.setInsets(10,10,0,0);
        //gb.addComponent(sizeLabel);

        fontSizeCombo = new JComboBox(sizes);
        gb.setPosition(3,1);
        gb.setInsets(10,0,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(fontSizeCombo);
        
        
        //styleLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Title.Style"));
        //gb.setPosition(0,4);
        //gb.setInsets(10,10,10,0);
        //gb.addComponent(styleLabel);

        fontStyleCombo = new JComboBox(styles);
        gb.setPosition(4,1);
        gb.setInsets(10,0,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(fontStyleCombo);

        colorButton = new ColorButton(title);
        gb.setPosition(5,1);
        gb.setInsets(10,10,0,10);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(colorButton);

        layerLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Layer"));
        gb.setPosition(1,2);
        gb.setInsets(10,10,0,0);
        gb.addComponent(layerLabel);
        
        layerField = new JTextField(5);
        gb.setPosition(2,2);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(layerField);


        setFont();

    }

    private void setFont()
    {
        textField.setText(title.text);
        fontNameCombo.setSelectedItem(title.font.getName());
        fontSizeCombo.setSelectedItem(String.valueOf(title.font.getSize()));
        fontStyleCombo.setSelectedItem(styleString(title.font.getStyle()));
        showCB.setSelected(title.show);
        layerField.setText(String.valueOf(title.layerNumber));
    }

    public FurnitureTitle getTitle()
    {
        Font font= new Font((String) fontNameCombo.getSelectedItem(), styleNumber((String)fontStyleCombo.getSelectedItem()),
                Integer.parseInt((String) fontSizeCombo.getSelectedItem()));
        title.font = font;
        title.show = showCB.isSelected();
        title.text = textField.getText();
        try
        {
             title.layerNumber = Integer.parseInt(layerField.getText());
        }
        catch(NumberFormatException ex)
        {
        	JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Message2")+": "+layerField.getText(),
 	    			iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);
        }
        return title;
    }



    private String styleString(int style)
    {
        String s;
        if(style == Font.PLAIN) s = iPlug.get("JumpPrinter.Furniture.Title.Plain");
        else if(style == Font.BOLD) s = iPlug.get("JumpPrinter.Furniture.Title.Bold");
        else if(style == (Font.PLAIN + Font.ITALIC)) s = iPlug.get("JumpPrinter.Furniture.Title.PlainItalic");
        else s = iPlug.get("JumpPrinter.Furniture.Title.BoldItalic");
        return s;
    }

    private int styleNumber(String style)
    {
        int n;
        if(style.equals(iPlug.get("JumpPrinter.Furniture.Title.Plain"))) n = Font.PLAIN;
        else if(style.equals(iPlug.get("JumpPrinter.Furniture.Title.Bold"))) n = Font.BOLD;
        else if(style.equals(iPlug.get("JumpPrinter.Furniture.Title.PlainItalic"))) n = (Font.PLAIN + Font.ITALIC);
        else n = (Font.BOLD + Font.ITALIC);

        return n;
    }

}
