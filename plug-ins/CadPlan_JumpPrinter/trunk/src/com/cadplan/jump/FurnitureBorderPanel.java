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
import javax.swing.border.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.util.Vector;

/**
 * User: geoff
 * Date: 11/01/2007
 * Time: 08:00:10
 * Copyright 2005 Geoffrey G Roy.
 */
public class FurnitureBorderPanel extends JPanel implements ActionListener, AdjustmentListener
{
    FurnitureBorder border;
    Vector<FurnitureBorder> borders;
    JPanel bordersPanel;
    JCheckBox showCB, fixedCB, fillCB;
    JCheckBox [] showBordersCB, fixedBordersCB, fillBordersCB;
    JComboBox thicknessCB;
    JComboBox [] thicknessBordersCB;
    ColorButton colorButton;
    ColorButton[] colorBordersButton;
    Button [] fillBordersButton;
    Button fillButton;
    JButton [] deleteButton;
    JButton addButton;
    JLabel thicknessLabel, showLabel, fixedLabel, shadeLabel;
    String [] thicknesses = {"0.5","1.0","1.5","2.0","2.5","3.0","4.0","6.0"};
    I18NPlug iPlug;
    GridBagDesigner gbp, gb;
    TitledBorder titledBorder = new TitledBorder(new BevelBorder(BevelBorder.RAISED), "Extra Borders");
    FurnitureDialog parent;
    JScrollPane scrollPane;
    JScrollBar vsb;
    JLabel layerLabel;
    JTextField layerField;
    JTextField [] layerFields;

    public FurnitureBorderPanel(FurnitureDialog parent, FurnitureBorder border, Vector<FurnitureBorder> borders, I18NPlug iPlug)
    {
        this.border = border;
        this.borders = borders;
        this.parent = parent;
        this.iPlug = iPlug;
        init();
        setBorder();
        
    }


    private void setBorder()
    {
         showCB.setSelected(border.show);
         fixedCB.setSelected(border.fixed);
         thicknessCB.setSelectedItem(String.valueOf(border.thickness));
         layerField.setText(String.valueOf(border.layerNumber));
         fillCB.setSelected(border.showFill);
         fillButton.setBackground(border.color1);
         
         for(int i=0; i < borders.size(); i++)
         {
        	 FurnitureBorder aborder = borders.elementAt(i);
        	 showBordersCB[i].setSelected(aborder.show);
        	 thicknessBordersCB[i].setSelectedItem(String.valueOf(aborder.thickness));
        	 //fixedBordersCB[i].setSelected(aborder.fixed);
        	 layerFields[i].setText(String.valueOf(aborder.layerNumber));
        	 fillBordersCB[i].setSelected(aborder.showFill);
        	 fillBordersButton[i].setBackground(aborder.color1);
        	 
        	 
        	 //System.out.println("Setting border:"+aborder.show+","+aborder.fixed+","+aborder.thickness);
         }

    }

    public FurnitureBorder getBorderItem()
    {
        border.show = showCB.isSelected();
        border.fixed = fixedCB.isSelected();
        border.thickness = Double.parseDouble((String)thicknessCB.getSelectedItem());
        border.showFill = fillCB.isSelected();
        border.color1= fillButton.getBackground();
        //border.layerNumber = Integer.parseInt(layerField.getText());
        try
 		{
            border.layerNumber = Integer.parseInt(layerField.getText());
 		}
 		catch(NumberFormatException ex)
 		{
 			JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Message2")+": "+layerField.getText(),
 	    			iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);
 		}
        return border;
    }
    public Vector<FurnitureBorder> getBorders()
    {
    	
    	for(int i=0; i < borders.size(); i++)
    	{
    		FurnitureBorder aborder = borders.elementAt(i);
    		aborder.show = showBordersCB[i].isSelected();
    		aborder.showFill = fillBordersCB[i].isSelected();
    		aborder.color1 = fillBordersButton[i].getBackground();
            //aborder.fixed = fixedBordersCB[i].isSelected();
            aborder.thickness = Double.parseDouble((String)thicknessBordersCB[i].getSelectedItem());
            //aborder.layerNumber = Integer.parseInt(layerFields[i].getText());
            try
     		{
                aborder.layerNumber = Integer.parseInt(layerFields[i].getText());
     		}
     		catch(NumberFormatException ex)
     		{
     	    	JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Message2")+" "+i+": "+layerFields[i].getText(),
     	    			iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);

     		}
            //System.out.println("Getting border:"+aborder.show+","+aborder.fixed+","+aborder.thickness);
    	}
    	return borders;
    }
    public void init()
    {
        gb = new GridBagDesigner(this);

        showLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Show"));
        gb.setPosition(0,0);
        gb.setInsets(10,20,0,0);
        //gb.setAnchor(GridBagConstraints.NORTHWEST);
        //gb.setWeight(0.0,0.0);
        gb.addComponent(showLabel);

        thicknessLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Border.LineWidth"));
        gb.setPosition(1,0);
        gb.setInsets(10,10,0,0);
        //gb.setAnchor(GridBagConstraints.NORTHEAST);
        //gb.setWeight(0.0,0.0);
        gb.addComponent(thicknessLabel);
        
        fixedLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Border.Fixed"));
        gb.setPosition(6,0);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.NORTHWEST);
        gb.setWeight(1.0,0.0);
        gb.addComponent(fixedLabel);
        
        layerLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Layer"));
        gb.setPosition(5,0);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.NORTHWEST);
        //gb.setWeight(1.0,0.0);
        gb.addComponent(layerLabel);
        
        shadeLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Note.Shade"));
        gb.setPosition(3,0);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.NORTHWEST);
        gb.setSpan(2,1);
        //gb.setWeight(1.0,0.0);
        gb.addComponent(shadeLabel);
        
        showCB = new JCheckBox("");
        gb.setPosition(0,1);
        gb.setInsets(10,25,0,0);
        //gb.setAnchor(GridBagConstraints.NORTHWEST);
        //gb.setWeight(0.0,0.0);
        gb.addComponent(showCB);

        thicknessCB = new JComboBox(thicknesses);
        gb.setPosition(1,1);
        gb.setInsets(10,0,0,0);
        //gb.setAnchor(GridBagConstraints.NORTHWEST);
        //gb.setWeight(0.0,0.0);
        gb.addComponent(thicknessCB);

        colorButton = new ColorButton(border);
        gb.setPosition(2,1);
        gb.setInsets(10,5,0,0);
        //gb.setAnchor(GridBagConstraints.NORTHWEST);
        //gb.setWeight(1.0,0.0);
        gb.addComponent(colorButton);
        
        fillCB = new JCheckBox("");
        gb.setPosition(3,1);
        gb.setInsets(10,10,0,0);
        //gb.setAnchor(GridBagConstraints.NORTHWEST);
        //gb.setWeight(0.0,0.0);
        gb.addComponent(fillCB);

        fillButton = new Button("    ");
        gb.setPosition(4,1);
        gb.setInsets(10,5,0,0);
        //gb.setAnchor(GridBagConstraints.NORTHWEST);
        //gb.setWeight(1.0,0.0);
        gb.addComponent(fillButton);
        fillButton.addActionListener(this);
        

       
        
        layerField = new JTextField(5);
        gb.setPosition(5,1);
        gb.setInsets(10,10,0,10);
        gb.setAnchor(GridBagConstraints.WEST);
        //gb.setWeight(1.0,0.0);
        gb.addComponent(layerField);
        
        fixedCB = new JCheckBox("");
        gb.setPosition(6,1);
        gb.setInsets(10,10,0,10);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.setWeight(1.0,0.0);
        gb.addComponent(fixedCB);
        

        bordersPanel = new JPanel();
        //bordersPanel.setBorder(titledBorder);
        //gbp = new GridBagDesigner(bordersPanel);
        setupBordersPanel();
        
        createScrollPane();
        
        
        
        addButton = new JButton(iPlug.get("JumpPrinter.Furniture.Add"));
        gb.setPosition(6,4);
        gb.setInsets(10,0,10,10);
        gb.setAnchor(GridBagConstraints.SOUTHEAST);
        gb.addComponent(addButton);
        addButton.addActionListener(this);
       
        
    }
    
    private void createScrollPane()
    {
    	scrollPane = new JScrollPane(bordersPanel); 
    	
    	
    	//scrollPane.add(bordersPanel);
        scrollPane.setPreferredSize(new Dimension(400,200));
        gb.setPosition(0,3);
        gb.setInsets(10,10,10,10);
        gb.setAnchor(GridBagConstraints.NORTHWEST);
        gb.setFill(GridBagConstraints.BOTH);
        gb.setWeight(1.0,1.0);
        gb.setSpan(7,1);
        gb.addComponent(scrollPane);
        
        vsb = scrollPane.getVerticalScrollBar();
        vsb.addAdjustmentListener(this);
    }
    
    private void setupBordersPanel()
    {
    	bordersPanel = new JPanel();
    	gbp = new GridBagDesigner(bordersPanel);
    	int n = borders.size();
        showBordersCB = new JCheckBox[n];
        fixedBordersCB = new JCheckBox[n];
        thicknessBordersCB = new JComboBox[n];
        colorBordersButton = new ColorButton[n];
        fillBordersButton = new Button[n];
        fillBordersCB = new JCheckBox[n];
        deleteButton = new JButton[n];
        layerFields = new JTextField[n];
        //System.out.println("Number of borders="+n);
        for(int i=0; i < borders.size(); i++)
        {
        	
        	showBordersCB[i] = new JCheckBox("");
        	gbp.setPosition(0,i);
        	gbp.setInsets(5,10,0,0);
        	gbp.setAnchor(GridBagConstraints.NORTHWEST);
        	gbp.addComponent(showBordersCB[i]);
        	
        	
        	thicknessBordersCB[i] = new JComboBox(thicknesses);
        	gbp.setPosition(1,i);
        	gbp.setInsets(10,10,0,0);
        	gbp.setAnchor(GridBagConstraints.NORTH);
        	gbp.addComponent(thicknessBordersCB[i]);
        	
        	
        	colorBordersButton[i] = new ColorButton(borders.elementAt(i));
        	gbp.setPosition(2,i);
        	gbp.setInsets(10,5,0,0);
        	gbp.setAnchor(GridBagConstraints.NORTH);
        	gbp.addComponent(colorBordersButton[i]);
        	
        	//fixedBordersCB[i] = new JCheckBox("");
        	//gbp.setPosition(3,i);
        	//gbp.setInsets(10,10,0,0);
        	//gbp.setAnchor(GridBagConstraints.NORTH);
        	//gbp.addComponent(fixedBordersCB[i]);
        	//fixedBordersCB[i].setEnabled(false);
        	
        	fillBordersCB[i] = new JCheckBox("");
        	gbp.setPosition(3,i);
        	gbp.setInsets(10,10,0,0);
        	gbp.setAnchor(GridBagConstraints.NORTH);
        	gbp.addComponent(fillBordersCB[i]);
        	
        	fillBordersButton[i] = new Button("    ");
        	gbp.setPosition(4,i);
        	gbp.setInsets(10,5,0,0);
        	gbp.setAnchor(GridBagConstraints.NORTH);
        	gbp.addComponent(fillBordersButton[i]);
        	fillBordersButton[i].addActionListener(this);
        	
        	layerFields[i] = new JTextField(5);
            gbp.setPosition(5,i);
            gbp.setInsets(10,10,0,10);
            gbp.setAnchor(GridBagConstraints.WEST);
            //gb.setWeight(1.0,0.0);
            gbp.addComponent(layerFields[i]);
        	
        	deleteButton[i] = new JButton(iPlug.get("JumpPrinter.Furniture.Delete"));
        	gbp.setPosition(6,i);
        	gbp.setInsets(10,10,0,10);
        	gbp.setAnchor(GridBagConstraints.NORTHWEST);
        	gbp.setWeight(1.0,0.0);
        	gbp.addComponent(deleteButton[i]);
        	deleteButton[i].addActionListener(this);
        	
        }
        //gb.setPosition(0,3);
        //gb.setInsets(10,10,10,10);
        //gb.setAnchor(GridBagConstraints.NORTHWEST);
        //gb.setFill(GridBagConstraints.BOTH);
        //gb.setWeight(1.0,1.0);
        //gb.setSpan(4,1);
        //gb.addComponent(bordersPanel);
    }
    
    public void actionPerformed(ActionEvent ev)
    {
    	
    	if(ev.getSource() == addButton)
    	{
    		
    		borders = getBorders();
    		FurnitureBorder border = new FurnitureBorder(1.0, true, true);
    		border.setBorder(0, 200, 100, 50, true);
    		borders.addElement(border);
    		
    		remove(scrollPane);
            setupBordersPanel();
            createScrollPane();
            parent.pack();
            
            setBorder();
            scrollPane.repaint();
            
            
    		
    	}
    	
    	if(ev.getSource() == fillButton)
    	{
    		Color newColor = JColorChooser.showDialog(this,"Select color", fillButton.getBackground());
	        if(newColor != null)
	        {              
	        	fillButton.setBackground(newColor);
	        }
        	return;
    	}
    	
    	for(int i=0; i < borders.size(); i++)
    	{
    		if(ev.getSource() == deleteButton[i])
    		{
    			//System.out.println("Removing border: "+i);
    			borders.removeElementAt(i);
    			//borders = getBorders();
    			remove(scrollPane);
    			bordersPanel = new JPanel();
        		//bordersPanel.setBorder(titledBorder);
        		gbp = new GridBagDesigner(bordersPanel);
                setupBordersPanel();
                createScrollPane();
                //scrollPane.add(bordersPanel);
                parent.pack();
                
                setBorder();
    			return;
    		}
    		
    		if(ev.getSource() == fillBordersButton[i])
    		{    			
    		    
    	    	Color newColor = JColorChooser.showDialog(this,"Select color", fillBordersButton[i].getBackground());
    	        if(newColor != null)
    	        {              
    	        	fillBordersButton[i].setBackground(newColor);
    	        }
	        	return;
    		}
    	}
    }
    public  void adjustmentValueChanged(AdjustmentEvent ev)
    {
    	
    	if(ev.getSource() == vsb)
    	{
    		
    		scrollPane.repaint();
    		parent.repaint();
    	}
    }
}
