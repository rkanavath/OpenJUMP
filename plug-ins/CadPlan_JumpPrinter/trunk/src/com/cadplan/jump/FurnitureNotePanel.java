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
import java.awt.event.*;
import java.util.Vector;

/**
 * User: geoff
 * Date: 12/01/2007
 * Time: 13:22:15
 * Copyright 2005 Geoffrey G Roy.
 */
public class FurnitureNotePanel extends JPanel implements ActionListener
{
        Vector<FurnitureNote> notes;
        FurnitureNote note;
        TextArea textField;
        JComboBox fontNameCombo, fontSizeCombo, fontStyleCombo, justifyCombo;
        JCheckBox showCB, bcolorCB, borderCB, fixedWidthCB;
        JTextField widthField, layerField;
        JLabel textLabel, fontLabel, sizeLabel, styleLabel, justifyLabel, layerLabel;
        ColorButton colorButton;
        ColorButtonB bcolorButton;
        Button color1Button;
        JButton backB, deleteB, nextB, addB;
        I18NPlug iPlug;
        String [] styles;
        String [] sizes;
        String [] justifies;
        int noteNumber = 0;
        GridBagDesigner gb;
        boolean fixedWidth = true;
        
        public FurnitureNotePanel(Vector<FurnitureNote> notes, I18NPlug iPlug)
        {
            this.notes = notes;
            this.iPlug = iPlug;
            styles = new String[] {iPlug.get("JumpPrinter.Furniture.Note.Plain"),
                            iPlug.get("JumpPrinter.Furniture.Note.PlainItalic"),
                            iPlug.get("JumpPrinter.Furniture.Note.Bold"),
                            iPlug.get("JumpPrinter.Furniture.Note.BoldItalic")};
             sizes = new String[] {"6","7","8","9","10","12","14","16","18","20","24","28","32","36","48","64","72","84","96"};
              justifies = new String[] {iPlug.get("JumpPrinter.Furniture.Note.Left"),
                              iPlug.get("JumpPrinter.Furniture.Note.Centre"),
                              iPlug.get("JumpPrinter.Furniture.Note.Right"),
                              iPlug.get("JumpPrinter.Furniture.Note.Full")};

            init();
        }

        public void init()
        {
        	note = notes.elementAt(noteNumber);
            gb = new GridBagDesigner(this);

            textLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Note.Note"));
            gb.setPosition(0,1);
            gb.setInsets(10,10,0,0);
            gb.addComponent(textLabel);

            if(fixedWidth)textField = new TextArea("",4,30,TextArea.SCROLLBARS_VERTICAL_ONLY);
            else textField = new TextArea(4,30);
            gb.setPosition(1,1);
            gb.setInsets(10,0,0,10);
            gb.setSpan(7,1);
            gb.setWeight(1.0,1.0);
            gb.setFill(GridBagConstraints.BOTH);
            gb.setAnchor(GridBagConstraints.WEST);
            gb.addComponent(textField);
            

            fontLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Note.Font"));
            gb.setPosition(0,2);
            gb.setInsets(10,10,0,0);
            gb.addComponent(fontLabel);


            fontNameCombo = new JComboBox(GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames());
            gb.setPosition(1,2);
            gb.setInsets(10,0,0,0);
            gb.setSpan(2,1);
            gb.setAnchor(GridBagConstraints.WEST);
            gb.addComponent(fontNameCombo);
            
            //sizeLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Note.Size"));
            //gb.setPosition(0,3);
            //gb.setInsets(10,10,0,0);
            //gb.addComponent(sizeLabel);

            fontSizeCombo = new JComboBox(sizes);
            gb.setPosition(3,2);
            gb.setInsets(10,0,0,0);
            gb.setAnchor(GridBagConstraints.WEST);
            gb.setFill(GridBagConstraints.HORIZONTAL);
            gb.addComponent(fontSizeCombo);
            
            //styleLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Note.Style"));
           // gb.setPosition(0,4);
            //gb.setInsets(10,10,10,0);
           // gb.addComponent(styleLabel);

            fontStyleCombo = new JComboBox(styles);
            gb.setPosition(4,2);
            gb.setInsets(10,0,0,0);
            gb.setAnchor(GridBagConstraints.WEST);
            gb.setSpan(1,1);
            gb.addComponent(fontStyleCombo);
            
            //justifyLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Note.Justify"));
            //gb.setPosition(2,3);
            //gb.setInsets(10,10,0,0);
            //gb.addComponent(justifyLabel);

            justifyCombo = new JComboBox(justifies);
            gb.setPosition(5,2);
            gb.setInsets(10,0,0,0);
            gb.setAnchor(GridBagConstraints.WEST);
            gb.addComponent(justifyCombo);

            colorButton = new ColorButton(note);
            gb.setPosition(6,2);
            gb.setInsets(10,10,0,10);
            gb.setAnchor(GridBagConstraints.WEST);
            gb.addComponent(colorButton);
            
            layerLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Layer"));
            gb.setPosition(0,3);
            gb.setInsets(10,10,0,0);
            gb.addComponent(layerLabel);
            
            layerField = new JTextField(5);
            gb.setPosition(1,3);
            gb.setInsets(10,0,0,0);
            gb.addComponent(layerField);
            
            
            
            bcolorCB = new JCheckBox(iPlug.get("JumpPrinter.Furniture.Note.Shade"));
            gb.setPosition(3,3);
            gb.setInsets(10,0,0,5);
            gb.addComponent(bcolorCB);
            
            
            
            bcolorButton = new ColorButtonB(note);
            gb.setPosition(4,3);
            gb.setInsets(10,10,0,10);
            gb.setAnchor(GridBagConstraints.WEST);
            gb.addComponent(bcolorButton);

            borderCB = new JCheckBox(iPlug.get("JumpPrinter.Furniture.Note.Border"));
            gb.setPosition(5,3);
            gb.setInsets(10,0,0,0);
            gb.addComponent(borderCB);
            
            color1Button = new Button("      ");
            gb.setPosition(6,3);
            gb.setInsets(10,10,0,10);
            gb.setAnchor(GridBagConstraints.WEST);
            gb.addComponent(color1Button);
            color1Button.addActionListener(this);
           

            showCB = new JCheckBox(iPlug.get("JumpPrinter.Furniture.Show"));
            gb.setPosition(0,0);
            gb.setInsets(10,0,0,10);
            gb.setAnchor(GridBagConstraints.EAST);
            gb.addComponent(showCB);
            
            //fixedWidthCB = new JCheckBox("Fixed Width");
            //gb.setPosition(2,0);
            //gb.setInsets(10,0,0,10);
            //gb.setAnchor(GridBagConstraints.EAST);
            //gb.addComponent(fixedWidthCB);
            //fixedWidthCB.addActionListener(this);
            JLabel widthLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Note.Width"));
            gb.setPosition(1,0);
            gb.setInsets(10,0,0,0);
            //gb.setAnchor(GridBagConstraints.EAST);
            gb.addComponent(widthLabel);
            
            widthField = new JTextField(5);
            
            gb.setPosition(2,0);
            gb.setInsets(10,0,0,10);
            gb.setAnchor(GridBagConstraints.WEST);
            gb.addComponent(widthField);

            backB = new JButton("<<");
            gb.setPosition(3,5);
            gb.setInsets(10,0,10,0);
            gb.setAnchor(GridBagConstraints.EAST);
            gb.addComponent(backB);
            backB.addActionListener(this);
            backB.setEnabled(false);
            
            deleteB = new JButton(iPlug.get("JumpPrinter.Furniture.Note.Delete"));
            gb.setPosition(4,5);
            gb.setInsets(10,5,10,0);
            gb.addComponent(deleteB);
            deleteB.addActionListener(this);
            deleteB.setEnabled(false);
            
            nextB = new JButton(">>");
            gb.setPosition(5,5);
            gb.setInsets(10,5,10,0);
            gb.addComponent(nextB);
            nextB.addActionListener(this);
            nextB.setEnabled(false);
            
            addB = new JButton(iPlug.get("JumpPrinter.Furniture.Note.Add"));
            gb.setPosition(6,5);
            gb.setInsets(10,5,10,10);
            gb.setAnchor(GridBagConstraints.WEST);
            gb.addComponent(addB);
            addB.addActionListener(this);
            addB.setEnabled(false);
            
            setDetails();
            updateButtons();

        }

        public void actionPerformed(ActionEvent ev)
        {
        	if(ev.getSource() == backB)
        	{
        		//System.out.println("Back");
        		notes.set(noteNumber, getNote()); // save current note first
        		noteNumber--;
        		note = notes.elementAt(noteNumber);
        		setDetails();
        	}
        	if(ev.getSource() == deleteB)
        	{
        		//System.out.println("Delete");
        		int result = JOptionPane.showConfirmDialog(this, iPlug.get("JumpPrinter.Furniture.Note.Message1") );
        		if(result == JOptionPane.YES_OPTION)
        		{
        			notes.remove(noteNumber);
        			noteNumber--;
        			setDetails();
        		}
        	}
        	if(ev.getSource() == nextB)
        	{
        		//System.out.println("Next");
        		notes.set(noteNumber, getNote()); // save current note first
        		noteNumber++;
        		note = notes.elementAt(noteNumber);
        		setDetails();
        	}
        	if(ev.getSource() == addB)
        	{
        		notes.set(noteNumber, getNote()); // save current note first
        		//System.out.println("Add");
        		note = new FurnitureNote("",new Font("SansSerif",Font.PLAIN,12),0,0,new Rectangle(0,150,50,20),false);
        		notes.addElement(note);
        		noteNumber = notes.size()-1;
        		setDetails();
        	}
        	
        	if(ev.getSource() == color1Button)
        	{
        		Color newColor = JColorChooser.showDialog(this,"Select color", color1Button.getBackground());
                if(newColor != null)
                {              
                    color1Button.setBackground(newColor);
                }
        	}
        	/*
        	if(ev.getSource() == fixedWidthCB)
        	{
        		fixedWidth = fixedWidthCB.isSelected();
        		String text = textField.getText();
        		int rows = textField.getRows();
        		int cols = textField.getColumns();
        		remove(textField);
        		if(fixedWidth)textField = new TextArea("",rows,cols,TextArea.SCROLLBARS_VERTICAL_ONLY);
                else textField = new TextArea("",rows,cols);
                gb.setPosition(1,1);
                gb.setInsets(10,0,0,10);
                gb.setSpan(6,1);
                gb.setWeight(1.0,1.0);
                gb.setFill(GridBagConstraints.BOTH);
                gb.setAnchor(GridBagConstraints.WEST);
                gb.addComponent(textField);
                //this.repaint(0);
                textField.setText(text);
                textField.repaint();
                //System.out.println("Fixed width: "+ fixedWidth+": "+text);
        	}
        	*/
        	updateButtons();
        }
        
        private void updateButtons()
        {
        	int numNotes = notes.size();
        	backB.setEnabled(false);
        	deleteB.setEnabled(false);
        	nextB.setEnabled(false);
        	addB.setEnabled(true);
        	if(noteNumber > 0 && numNotes > 1)
        	{
        		backB.setEnabled(true);
        		deleteB.setEnabled(true);
        	}
        	if(noteNumber < numNotes -1)
        	{
        		nextB.setEnabled(true);
        	}
        	
        }
        private void setDetails()
        {
        	note = notes.elementAt(noteNumber);
            textField.setText(note.text);
            fontNameCombo.setSelectedItem(note.font.getName());
            fontSizeCombo.setSelectedItem(String.valueOf(note.font.getSize()));
            fontStyleCombo.setSelectedItem(styleString(note.font.getStyle()));
            showCB.setSelected(note.show);
            borderCB.setSelected(note.border);
            justifyCombo.setSelectedIndex(note.justify);
            bcolorCB.setSelected(note.setBackColor);
            colorButton.setItem(note);
            bcolorButton.setItem(note);
            color1Button.setBackground(note.color1);
            
                 widthField.setText(String.valueOf(note.width));
                 //fixedWidthCB.setSelected(note.width > 0);
            
            layerField.setText(String.valueOf(note.layerNumber));
        }

        public FurnitureNote getNote()
        {
            Font font= new Font((String) fontNameCombo.getSelectedItem(), styleNumber((String)fontStyleCombo.getSelectedItem()),
                    Integer.parseInt((String) fontSizeCombo.getSelectedItem()));
            note.font = font;
            note.show = showCB.isSelected();
            note.border = borderCB.isSelected();
            note.text = textField.getText();
            note.justify = justifyCombo.getSelectedIndex();
            note.setBackColor = bcolorCB.isSelected();
            note.color1 = color1Button.getBackground();
            //note.layerNumber = Integer.parseInt(layerField.getText());
            try
    		{
               note.layerNumber = Integer.parseInt(layerField.getText());
    		}
    		catch(NumberFormatException ex)
    		{
    	       	JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Message2")+": "+layerField.getText(),
     	    			iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);

    		}
            
            
            try
            {
                note.width= Integer.parseInt(widthField.getText());
            }
            catch (NumberFormatException ex)
            {
            	note.width = 0;
            }
            
            if(note.justify == 3 && note.width <= 0) note.justify = 0;
            justifyCombo.setSelectedIndex(note.justify);
            
            notes.set(noteNumber,note);
            
            return note;
        }

        public Vector<FurnitureNote> getNotes()
        {
        	return notes;
        }


        private String styleString(int style)
        {
            String s;
            if(style == Font.PLAIN) s = iPlug.get("JumpPrinter.Furniture.Note.Plain");
            else if(style == Font.BOLD) s = iPlug.get("JumpPrinter.Furniture.Note.Bold");
            else if(style == (Font.PLAIN + Font.ITALIC)) s = iPlug.get("JumpPrinter.Furniture.Note.PlainItalic");
            else s = iPlug.get("JumpPrinter.Furniture.Note.BoldItalic");
            return s;
        }

        private int styleNumber(String style)
        {
            int n;
            if(style.equals(iPlug.get("JumpPrinter.Furniture.Note.Plain"))) n = Font.PLAIN;
            else if(style.equals(iPlug.get("JumpPrinter.Furniture.Note.Bold"))) n = Font.BOLD;
            else if(style.equals(iPlug.get("JumpPrinter.Furniture.Note.PlainItalic"))) n = (Font.PLAIN + Font.ITALIC);
            else n = (Font.BOLD + Font.ITALIC);

            return n;
        }

}
