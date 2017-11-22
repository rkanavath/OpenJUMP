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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemListener;
import java.awt.event.ItemEvent;

/**
 * User: geoff
 * Date: 11/01/2007
 * Time: 08:50:07
 * Copyright 2005 Geoffrey G Roy.
 */
public class FurnitureNorthPanel extends JPanel implements ItemListener, ActionListener
{
    FurnitureNorth north;
    JCheckBox showCB;
    NorthPanel imagePanel;
    JRadioButton style1RB, style2RB, style3RB, style4RB, style5RB, style6RB;
    ButtonGroup buttonGroup;
    Button colorButton;
    JLabel rotationLabel, sizeLabel, layerLabel;
    JTextField rotationField, sizeField, layerField;
    I18NPlug iPlug;

    public FurnitureNorthPanel(FurnitureNorth north, I18NPlug iPlug)
    {
        this.north = north;
        this.iPlug = iPlug;
        init();
        setNorth();
    }



    public void init()
    {
        GridBagDesigner gb = new GridBagDesigner(this);

        showCB = new JCheckBox(iPlug.get("JumpPrinter.Furniture.Show"));
        gb.setPosition(0,0);
        gb.setInsets(12,10,10,10);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(showCB);

        rotationLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.North.Rotation"));
        gb.setPosition(1,0);
        gb.setInsets(10,0,0,10);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(rotationLabel);

        rotationField = new JTextField(5);
        gb.setPosition(2,0);
        gb.setInsets(10,0,0,10);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(rotationField);

        colorButton = new Button("    ");
        gb.setPosition(3,0);
        gb.setInsets(10,0,0,10);
        //gb.setAnchor(GridBagConstraints.WEST);
        //gb.setWeight(1.0,0.0);
        gb.addComponent(colorButton);
        colorButton.addActionListener(this);

        sizeLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Note.Size"));
        gb.setPosition(4,0);
        gb.setInsets(10,0,0,5);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(sizeLabel);

        sizeField = new JTextField(3);
        gb.setPosition(5,0);
        gb.setInsets(10,0,0,10);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(sizeField);

        layerLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Layer"));
        gb.setPosition(6,0);
        gb.setInsets(10,0,0,5);
        gb.setAnchor(GridBagConstraints.EAST);
        
        gb.addComponent(layerLabel);

        layerField = new JTextField(5);
        gb.setPosition(7,0);
        gb.setInsets(10,0,0,10);
        gb.setWeight(1.0,0.0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(layerField);
        

        style1RB = new JRadioButton(iPlug.get("JumpPrinter.Furniture.North.Style1"));
        gb.setPosition(0,1);
        gb.setInsets(10,10,10,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(style1RB);

        style2RB = new JRadioButton(iPlug.get("JumpPrinter.Furniture.North.Style2"));
        gb.setPosition(1,1);
        gb.setInsets(10,10,10,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(style2RB);

        style3RB = new JRadioButton(iPlug.get("JumpPrinter.Furniture.North.Style3"));
        gb.setPosition(2,1);
        gb.setInsets(10,10,10,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.setWeight(0.0,0.0);
        gb.addComponent(style3RB);

        style4RB = new JRadioButton(iPlug.get("JumpPrinter.Furniture.North.Style4"));
        gb.setPosition(3,1);
        gb.setInsets(10,10,10,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.setWeight(0.0,0.0);
        gb.addComponent(style4RB);
        
        style5RB = new JRadioButton(iPlug.get("JumpPrinter.Furniture.North.Style5"));
        gb.setPosition(4,1);
        gb.setInsets(10,10,10,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.setWeight(0.0,0.0);
        gb.addComponent(style5RB);
        
        style6RB = new JRadioButton(iPlug.get("JumpPrinter.Furniture.North.Style6"));
        gb.setPosition(5,1);
        gb.setInsets(10,10,10,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.setWeight(1.0,0.0);
        gb.addComponent(style6RB);

        buttonGroup = new ButtonGroup();
        buttonGroup.add(style1RB);
        buttonGroup.add(style2RB);
        buttonGroup.add(style3RB);
        buttonGroup.add(style4RB);
        buttonGroup.add(style5RB);
        buttonGroup.add(style6RB);



        imagePanel = new NorthPanel(north);
        imagePanel.setPreferredSize(new Dimension(300,80));
        gb.setPosition(0,2);
        gb.setFill(GridBagConstraints.BOTH);
        gb.setInsets(0,0,10,0);
        gb.setSpan(8,1);
        gb.setWeight(1.0,1.0);
        gb.addComponent(imagePanel);

        
        
        setNorth();

    }
    private void setNorth()
    {
         showCB.setSelected(north.show);
         rotationField.setText(String.valueOf(north.rotation));
         sizeField.setText(String.valueOf(north.sizeFactor));
         layerField.setText(String.valueOf(north.layerNumber));
         colorButton.setBackground(north.color);
         switch (north.type)
         {
             case 0:
             case 1: style1RB.setSelected(true); break;
             case 2: style2RB.setSelected(true); break;
             case 3: style3RB.setSelected(true); break;
             case 4: style4RB.setSelected(true); break;
             case 5: style5RB.setSelected(true); break;
             case 6: style6RB.setSelected(true); break;
         }
    }

    public FurnitureNorth getNorth()
    {
        north.show = showCB.isSelected();
        double rot = north.rotation;
        double size = north.sizeFactor;
        //north.layerNumber = Integer.parseInt(layerField.getText());
        try
		{
           north.layerNumber = Integer.parseInt(layerField.getText());
		}
		catch(NumberFormatException ex)
		{
	       	JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Message2")+": "+layerField.getText(),
 	    			iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);

		}
        
        try
        {
            rot = Double.parseDouble(rotationField.getText());
            north.rotation = rot;
        }
        catch (NumberFormatException ex)
        {
            JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.North.Message1"),iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);

        }
        try
        {
            size = Double.parseDouble(sizeField.getText());
            if(size > 0.1 && size <= 10.0)
            {
                  north.sizeFactor = size;
            }
            else
            {
                JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Message1"),iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);
                
            }
        }
        catch (NumberFormatException ex)
        {
            JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Message1"),iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);
            
        }

        if(style1RB.isSelected()) north.type = 1;
        else if(style2RB.isSelected()) north.type = 2;
        else if(style3RB.isSelected()) north.type = 3;
        else if(style4RB.isSelected()) north.type = 4;
        else if(style5RB.isSelected()) north.type = 5;
        else if(style6RB.isSelected()) north.type = 6;
        return north;
    }

    public void actionPerformed(ActionEvent ev)
    {
    	if(ev.getSource() == colorButton)
    	{
    		Color newColor = JColorChooser.showDialog(this,"Select color", colorButton.getBackground());
            if(newColor != null)
            {              
                colorButton.setBackground(newColor);
                north.color = newColor;
                imagePanel.repaint();
                
            }
    	}
    }
   public void itemStateChanged(ItemEvent ev)
   {

   }
    
}
