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
import java.text.DecimalFormat;

/**
 * User: geoff
 * Date: 10/01/2007
 * Time: 16:21:14
 * Copyright 2005 Geoffrey G Roy.
 */
public class FurnitureScalePanel extends JPanel implements ItemListener, ActionListener
{

    FurnitureScale scaleItem;
    JLabel scaleLabel, rangeLabel, intervalLabel, sizeLabel, numberLabel, fontLabel, unitsLabel, layerLabel;;
    JTextField rangeField, intervalField, sizeField, numberScaleField, unitsField, layerField;
    JCheckBox autoCB, showCB, showRatioCB, showUnitCB;;
    JComboBox fontNameCombo, fontSizeCombo, fontStyleCombo;
    ColorButton colorButton;
    Button color1Button, color2Button;
    I18NPlug iPlug;
    String [] styles;
    String [] sizes = {"6","7","8","9","10","12","14","16","18","20","24","28","32","36","48","64","72","84","96"};

    public FurnitureScalePanel(FurnitureScale scaleItem, I18NPlug iPlug)
    {
        this.scaleItem = scaleItem;
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
        //gb.setWeight(1.0,0.0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(showCB);

        scaleLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Scale.Scale")+formatScale(scaleItem.scale));
        gb.setPosition(1,0);
        gb.setInsets(10,0,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        //gb.setWeight(1.0,0.0);
        gb.addComponent(scaleLabel);
        
        fontLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Title.Font"));
        gb.setPosition(0,2);
        gb.setInsets(10,10,0,10);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(fontLabel);

        
        fontNameCombo = new JComboBox(GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames());
        gb.setPosition(1,2);
        gb.setInsets(10,0,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.setSpan(3,1);
        gb.setFill(GridBagConstraints.HORIZONTAL);
        gb.addComponent(fontNameCombo);

        //sizeLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Title.Size"));
        //gb.setPosition(0,3);
       // gb.setInsets(10,10,0,0);
        //gb.addComponent(sizeLabel);

        fontSizeCombo = new JComboBox(sizes);
        gb.setPosition(4,2);
        gb.setInsets(10,0,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.setFill(GridBagConstraints.HORIZONTAL);
        gb.addComponent(fontSizeCombo);
        
        
        //styleLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Title.Style"));
        //gb.setPosition(0,4);
        //gb.setInsets(10,10,10,0);
        //gb.addComponent(styleLabel);

        fontStyleCombo = new JComboBox(styles);
        gb.setPosition(5,2);
        gb.setInsets(10,0,0,0);
        gb.setSpan(2,1);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(fontStyleCombo);
        
        colorButton = new ColorButton(scaleItem);
        gb.setPosition(7,2);
        gb.setInsets(10,0,0,10);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(colorButton);

        
        
        

        

       

        rangeLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Scale.Range"));
        gb.setPosition(0,1);
        gb.setInsets(10,10,0,10);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(rangeLabel);

        rangeField = new JTextField(30);
        gb.setPosition(1,1);
        gb.setInsets(10,0,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.setSpan(3,1);
        //gb.setWeight(1.0,0.0);
        gb.addComponent(rangeField);
        
        autoCB = new JCheckBox(iPlug.get("JumpPrinter.Furniture.Scale.AutoRange"));
        gb.setPosition(4,1);
        gb.setInsets(10,0,0,0);
        gb.setSpan(1,1);
        gb.setAnchor(GridBagConstraints.WEST);
//        gb.setWeight(1.0,0.0);
        gb.addComponent(autoCB);
        autoCB.addItemListener(this);
        
        color1Button = new Button("  ");
        gb.setPosition(5,1);
        gb.setInsets(10,0,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(color1Button);
        color1Button.addActionListener(this);
        color1Button.setBackground(scaleItem.color1);
        
        color2Button = new Button("  ");
        gb.setPosition(6,1);
        gb.setInsets(10,0,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(color2Button);
        color2Button.addActionListener(this);
        color2Button.setBackground(scaleItem.color2);

        //intervalLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Scale.Interval"));
        //gb.setPosition(0,3);
        //gb.setInsets(10,10,0,10);
        //gb.setAnchor(GridBagConstraints.WEST);
        //gb.addComponent(intervalLabel);

        //intervalField = new JTextField(10);
        //gb.setPosition(1,3);
        //gb.setInsets(10,0,0,0);
        //gb.setAnchor(GridBagConstraints.WEST);
        //gb.addComponent(intervalField);
        
        
        numberLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Scale.Number"));
        gb.setPosition(0,4);
        gb.setInsets(10,10,0,10);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(numberLabel);

        numberScaleField = new JTextField(5);
        gb.setPosition(1,4);
        gb.setInsets(10,0,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(numberScaleField);
        
        unitsLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Scale.Units"));
        gb.setPosition(2,4);
        gb.setInsets(10,10,0,10);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(unitsLabel);

        unitsField = new JTextField(10);
        gb.setPosition(3,4);
        gb.setInsets(10,0,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.setFill(GridBagConstraints.HORIZONTAL);
        gb.addComponent(unitsField);

        showRatioCB = new JCheckBox(iPlug.get("JumpPrinter.Furniture.Scale.ShowRatio"));
        gb.setPosition(4,4);
        gb.setInsets(10,0,0,0);
        gb.addComponent(showRatioCB);
        
        showUnitCB = new JCheckBox(iPlug.get("JumpPrinter.Furniture.Scale.ShowUnits"));
        gb.setPosition(5,4);
        gb.setInsets(10,0,0,0);
        gb.setSpan(2,1);
        gb.addComponent(showUnitCB);
        
        sizeLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Note.Size"));
        gb.setPosition(0,5);
        gb.setInsets(10,0,0,10);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(sizeLabel);

        sizeField = new JTextField(5);
        gb.setPosition(1,5);
        gb.setInsets(10,0,0,10);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(sizeField);
        
        layerLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Layer"));
        gb.setPosition(2,5);
        gb.setInsets(10,10,0,10);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(layerLabel);

        layerField = new JTextField(5);
        gb.setPosition(3,5);
        gb.setInsets(10,0,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(layerField);
        

        setScale();
        setFont();
    }

    private void setFont()
    {
        
        fontNameCombo.setSelectedItem(scaleItem.font.getName());
        fontSizeCombo.setSelectedItem(String.valueOf(scaleItem.font.getSize()));
        fontStyleCombo.setSelectedItem(styleString(scaleItem.font.getStyle()));
        
    }
     private String formatScale(double v)
    {
        DecimalFormat scaleFormat= null;
        if (v >= 10.0 || v == 0.0)  scaleFormat = new DecimalFormat("#,###");
        else if(v >= 1.0 && v < 10.0) scaleFormat = new DecimalFormat("##0.0");
        else scaleFormat = new DecimalFormat("0.0E0");
        return scaleFormat.format(v);
    }
    private void setScale()
    {
           autoCB.setSelected(scaleItem.autoScale);
           rangeField.setText(scaleItem.rangeSpec);
           sizeField.setText(String.valueOf(scaleItem.sizeFactor));
           //intervalField.setText(String.valueOf(scaleItem.interval));
           numberScaleField.setText(String.valueOf(scaleItem.numberScale));
           unitsField.setText(scaleItem.units);
           showCB.setSelected(scaleItem.show);
           layerField.setText(String.valueOf(scaleItem.layerNumber));
           if(autoCB.isSelected())
           {
               rangeField.setEnabled(false);
               //intervalField.setEnabled(false);

           }
           showRatioCB.setSelected(scaleItem.showRatio);
           showUnitCB.setSelected(scaleItem.showUnits);
    }

    public FurnitureScale getScale()
    {
           boolean OK = true;
           double range = 0.0;
           double interval = 0.0;
           int numberScale = 1;
           double size = 1.0;
           
           Font font= new Font((String) fontNameCombo.getSelectedItem(), styleNumber((String)fontStyleCombo.getSelectedItem()),
                   Integer.parseInt((String) fontSizeCombo.getSelectedItem()));
           scaleItem.font = font;
           
            try
            { 
            //   range = Double.parseDouble(rangeField.getText());
               //interval = Double.parseDouble(intervalField.getText());
               numberScale = Integer.parseInt(numberScaleField.getText());
            }
            catch(NumberFormatException ex)
            {
                 JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Scale.Message2"),
                         iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);
                 OK = false;
            }
            //scaleItem.layerNumber = Integer.parseInt(layerField.getText());
            try
     		{
                scaleItem.layerNumber = Integer.parseInt(layerField.getText());
     		}
     		catch(NumberFormatException ex)
     		{
     	       	JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Message2")+": "+layerField.getText(),
     	    			iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);

     		}
            //if(OK && range <= 0.0)
            //{
             //   JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Scale.Message2"),
            //            iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);
             //   OK = false;

            //}
            //if(OK && (interval <= 0.0 || interval > 0.5*range))
           // {
            //   JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Scale.Message3"),
            //            iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);
            //    OK = false;
//
            //}
            try
            {
               size = Double.parseDouble(sizeField.getText());
               if (size > 0.1 && size <= 10.0)
               {
                  scaleItem.sizeFactor = size;
               }
               else
               {
                   JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Message1"),
                           iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);
               }
            }
            catch(NumberFormatException ex)
            {
                 JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Message1"),
                         iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);
                 OK = false;
            }
            
            
            if(OK)
            {
                scaleItem.autoScale = autoCB.isSelected();
                scaleItem.show = showCB.isSelected();
                scaleItem.rangeSpec = rangeField.getText();
                if(scaleItem.rangeSpec == null || scaleItem.rangeSpec.equals("")) scaleItem.autoScale = true;
                
                //scaleItem.interval = interval;
                scaleItem.numberScale = numberScale;
                scaleItem.units = unitsField.getText();
                scaleItem.showRatio = showRatioCB.isSelected();
                scaleItem.showUnits = showUnitCB.isSelected();
                scaleItem.color1 = color1Button.getBackground();
                scaleItem.color2 = color2Button.getBackground();
            }
            
            if(!validScaleRange(scaleItem.rangeSpec) && !scaleItem.autoScale)
            {
            	
            	//JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Message3"),
                //        iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);
            	//OK = false;
            	scaleItem.validItem = false;
            	scaleItem.autoScale = true;
            }
            else
            {
            	scaleItem.validItem = true;
            }

           return scaleItem;
    }

    public void itemStateChanged(ItemEvent ev)
    {
        if(ev.getSource() == autoCB)
        {
             rangeField.setEnabled(!autoCB.isSelected());
             //intervalField.setEnabled(!autoCB.isSelected());
        }
    }
    
    public void actionPerformed(ActionEvent ev)
	{   
    	if(ev.getSource() == color1Button)
    	{
	    	Color newColor = JColorChooser.showDialog(this,"Select color", color1Button.getBackground());
	        if(newColor != null)
	        {              
	            color1Button.setBackground(newColor);
	        }
    	}
    	if(ev.getSource() == color2Button)
    	{
	        Color newColor = JColorChooser.showDialog(this,"Select color", color2Button.getBackground());
	        if(newColor != null)
	        {              
	            color2Button.setBackground(newColor);
	        }
    	}
    }
    
    private boolean validScaleRange(String ranges)
    {
    	boolean OK = false;
    	String pattern = "(-\\d+:\\d+,(\\d+,)\\d+(,\\d+)+)|(\\d+,)\\d+(,\\d+)+";
    	if(ranges.matches(pattern)) OK = true;
    	//System.out.println("pattern: "+pattern+"  ranges: "+ranges+"  OK: "+OK);
    	
    	return OK;
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
