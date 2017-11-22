package com.cadplan.jump;

import com.cadplan.designer.GridBagDesigner;

import javax.swing.*;

import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * User: geoff
 * Date: 12/02/2007
 * Time: 14:59:12
 * Copyright 2005 Geoffrey G Roy.
 */
public class LayerLegendPanel extends JPanel implements ActionListener
{
       LayerLegend legend;
       JLabel titleLabel, fontLabel, sizeLabel, styleLabel, layerLabel;
       JCheckBox showCB, borderCB, showTitleCB;
       JTextField titleField, layerField;
       JComboBox fontNameCombo, fontSizeCombo, fontStyleCombo;
       
       JScrollPane scrollPane;
       JPanel listPanel;
       JCheckBox [] chooseCB;
       JButton checkAllButton, clearAllButton;
       Button titleColorButton, textColorButton, borderColorButton;
       ColorButton colorButton;
       JComboBox sizeCombo;
       JRadioButton vertRB, horRB;
       ButtonGroup bgroup;
       int numItems;
       I18NPlug iPlug;
       String [] styles;
       String [] sizeItems = {"6","7","8","9","10","12","14","16","18","20","24","28","32","36","48","64","72","84","96"};

       public LayerLegendPanel(LayerLegend legend, I18NPlug iPlug)
       {
           this.legend = legend;
           this.iPlug = iPlug;
           numItems = legend.legendItems.size();
           styles = new String [] {iPlug.get("JumpPrinter.Furniture.Title.Plain"),
                   iPlug.get("JumpPrinter.Furniture.Title.PlainItalic"),
                   iPlug.get("JumpPrinter.Furniture.Title.Bold"),
                   iPlug.get("JumpPrinter.Furniture.Title.BoldItalic")};
           init();
           setFont();
       }

       public void init()
       {
          GridBagDesigner gb = new GridBagDesigner(this);
/*
           showCB = new JCheckBox(iPlug.get("JumpPrinter.Furniture.Show"));
           gb.setPosition(0,0);
           gb.setAnchor(GridBagConstraints.WEST);
           gb.setInsets(10,5,0,0);
           gb.addComponent(showCB);
           showCB.setSelected(legend.show);


           checkAllButton = new JButton(iPlug.get("JumpPrinter.Furniture.Legend.CheckAll"));
           gb.setPosition(1,0);
           gb.setInsets(10,5,0,0);
           gb.setSpan(2,1);
           gb.addComponent(checkAllButton);
           checkAllButton.addActionListener(this);

           clearAllButton = new JButton(iPlug.get("JumpPrinter.Furniture.Legend.ClearAll"));
           gb.setPosition(3,0);
           gb.setInsets(10,5,0,0);
           gb.setWeight(0.0,0.0);
           gb.setAnchor(GridBagConstraints.WEST);
           gb.addComponent(clearAllButton);
           clearAllButton.addActionListener(this);

           colorButton = new ColorButton(legend);
           gb.setPosition(4,0);
           gb.setInsets(10,10,0,0);
           gb.setAnchor(GridBagConstraints.WEST);
           gb.setWeight(1.0,0.0);
           gb.addComponent(colorButton);

           borderCB = new JCheckBox(iPlug.get("JumpPrinter.Furniture.Legend.Border"));
           gb.setPosition(0,1);
           gb.setInsets(10,5,0,0);
           //gb.setWeight(1.0,0.0);
           gb.setAnchor(GridBagConstraints.WEST);
           gb.addComponent(borderCB);
           borderCB.setSelected(legend.border);

           JLabel sizeLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Note.Size"));
           gb.setPosition(1,1);
           gb.setAnchor(GridBagConstraints.EAST);
           gb.setInsets(10,5,0,0);
           gb.addComponent(sizeLabel);

           sizeCombo = new JComboBox(sizeItems);
           gb.setPosition(2,1);
           gb.setInsets(10,0,0,0);
           gb.setAnchor(GridBagConstraints.WEST);
           gb.addComponent(sizeCombo);
           sizeCombo.setSelectedItem(String.valueOf(legend.size));
*/
          showCB = new JCheckBox(iPlug.get("JumpPrinter.Furniture.Show"));
          gb.setPosition(0,0);
          gb.setAnchor(GridBagConstraints.WEST);
          gb.setInsets(10,5,0,0);
          gb.addComponent(showCB);
          showCB.setSelected(legend.show);

          titleLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Title.Title"));
          gb.setPosition(1,0);
          gb.setInsets(10,5,0,0);
          gb.addComponent(titleLabel);
          
          
          titleField = new JTextField(20);
          gb.setPosition(2,0);
          gb.setInsets(10,5,0,0);
          gb.setSpan(4,1);
          gb.setFill(GridBagConstraints.HORIZONTAL);
          gb.addComponent(titleField);
          titleField.setText(legend.legendName);
          
          showTitleCB = new JCheckBox(iPlug.get("JumpPrinter.Furniture.Title.Title"));
          gb.setPosition(6,0);
          gb.setInsets(10,5,0,0);
          gb.addComponent(showTitleCB);
          showTitleCB.setSelected(legend.showTitle);
          
          titleColorButton = new Button("  ");
          titleColorButton.setBackground(legend.legendTitleColor);
          gb.setPosition(7,0);
          gb.setInsets(10,0,0,10);
          gb.setAnchor(GridBagConstraints.WEST);
          gb.addComponent(titleColorButton);
          titleColorButton.addActionListener(this);
          
          fontLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Title.Font"));
          gb.setPosition(0,1);
          gb.setInsets(5,10,0,0);
          gb.addComponent(fontLabel);

          
          fontNameCombo = new JComboBox(GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames());
          gb.setPosition(1,1);
          gb.setInsets(5,0,0,0);
          gb.setAnchor(GridBagConstraints.WEST);
          gb.setSpan(3,1);
          gb.addComponent(fontNameCombo);

          

          fontSizeCombo = new JComboBox(sizeItems);
          gb.setPosition(4,1);
          gb.setInsets(5,0,0,0);
          gb.setAnchor(GridBagConstraints.WEST);
          gb.setFill(GridBagConstraints.HORIZONTAL);
          gb.addComponent(fontSizeCombo);

          

          fontStyleCombo = new JComboBox(styles);
          gb.setPosition(5,1);
          gb.setInsets(5,0,0,0);
          gb.setAnchor(GridBagConstraints.WEST);
          gb.addComponent(fontStyleCombo);
          
          textColorButton = new Button("  ");
          textColorButton.setBackground(legend.legendTextColor);
          gb.setPosition(7,1);
          gb.setInsets(5,0,0,10);
          gb.setAnchor(GridBagConstraints.WEST);
          gb.setWeight(1.0,0.0);
          gb.addComponent(textColorButton);
          textColorButton.addActionListener(this);
          
          borderCB = new JCheckBox(iPlug.get("JumpPrinter.Furniture.Legend.Border"));
          gb.setPosition(6,2);
          gb.setInsets(5,0,0,0);
          //gb.setWeight(1.0,0.0);
          gb.setAnchor(GridBagConstraints.WEST);
          gb.addComponent(borderCB);
          borderCB.setSelected(legend.border);
          
          borderColorButton = new Button("  ");
          borderColorButton.setBackground(legend.legendBorderColor);
          gb.setPosition(7,2);
          gb.setInsets(5,0,0,10);
          gb.setAnchor(GridBagConstraints.WEST);
          gb.addComponent(borderColorButton);
          borderColorButton.addActionListener(this);
          
          checkAllButton = new JButton(iPlug.get("JumpPrinter.Furniture.Legend.CheckAll"));
          gb.setPosition(0,2);
          gb.setInsets(10,0,0,0);
          gb.addComponent(checkAllButton);
          checkAllButton.addActionListener(this);

          clearAllButton = new JButton(iPlug.get("JumpPrinter.Furniture.Legend.ClearAll"));
          gb.setPosition(1,2);
          gb.setInsets(10,0,0,0);
          gb.setWeight(0.0,0.0);
          gb.setAnchor(GridBagConstraints.WEST);
          gb.addComponent(clearAllButton);
          clearAllButton.addActionListener(this);     
           
          layerLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Layer"));
          gb.setPosition(2,2);
          gb.setInsets(10,10,10,0);
          gb.addComponent(layerLabel);
          
          
          layerField = new JTextField(5);
          gb.setPosition(3,2);
          gb.setInsets(10,5,10,0);
          gb.addComponent(layerField);
          layerField.setText(String.valueOf(legend.layerNumber));
           
           vertRB = new JRadioButton(iPlug.get("JumpPrinter.Furniture.Vertical"));
           gb.setPosition(4,2);
           gb.setInsets(0,0,0,0);
           gb.setAnchor(GridBagConstraints.WEST);
           gb.addComponent(vertRB);
           
           horRB = new JRadioButton(iPlug.get("JumpPrinter.Furniture.Horizontal"));
           gb.setPosition(5,2);
           gb.setInsets(0,0,0,10);
           gb.setAnchor(GridBagConstraints.WEST);
           gb.addComponent(horRB);
           
           bgroup = new ButtonGroup();
           bgroup.add(vertRB);
           bgroup.add(horRB);
           
           if(legend.horizontal)horRB.setSelected(true);
           else vertRB.setSelected(true);

           chooseCB = new JCheckBox[numItems];

           listPanel = new JPanel();
           GridBagDesigner gbl = new GridBagDesigner(listPanel);
           for (int i=0; i < numItems; i++)
           {
               gbl.setPosition(0,i);
               gbl.setWeight(1.0,0.0);
               gbl.setAnchor(GridBagConstraints.WEST);
               chooseCB[i] = new JCheckBox(legend.legendItems.elementAt(i).name);
               chooseCB[i].setSelected(legend.legendItems.elementAt(i).include);
               gbl.addComponent(chooseCB[i]);
           }
           scrollPane = new JScrollPane(listPanel);
           scrollPane.setPreferredSize(new Dimension(300,150));
           gb.setPosition(0,3);
           gb.setFill(GridBagConstraints.BOTH);
           gb.setWeight(1.0,1.0);
           gb.setSpan(8,1);
           gbl.setAnchor(GridBagConstraints.WEST);
           gb.addComponent(scrollPane);


           
           
          
          
       }
       private void setFont()
       {
           
           fontNameCombo.setSelectedItem(legend.legendFont.getName());
           fontSizeCombo.setSelectedItem(String.valueOf(legend.legendFont.getSize()));
           fontStyleCombo.setSelectedItem(styleString(legend.legendFont.getStyle()));
           
       }


       public LayerLegend getLegend()
       {
           legend.show = showCB.isSelected();
           legend.border = borderCB.isSelected();
           legend.horizontal = horRB.isSelected();
           
           legend.showTitle = showTitleCB.isSelected();
           legend.border = borderCB.isSelected();
           legend.legendName = titleField.getText();
           legend.legendTitleColor = titleColorButton.getBackground();
           legend.legendTextColor = textColorButton.getBackground();
           legend.legendBorderColor = borderColorButton.getBackground();          
           legend.legendFont = new Font((String) fontNameCombo.getSelectedItem(), styleNumber((String)fontStyleCombo.getSelectedItem()),
                   Integer.parseInt((String) fontSizeCombo.getSelectedItem()));
           
           //legend.layerNumber = Integer.parseInt(layerField.getText());
           try
    		{
               legend.layerNumber = Integer.parseInt(layerField.getText());
    		}
    		catch(NumberFormatException ex)
    		{
    			JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Message2")+": "+layerField.getText(),
     	    			iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);
    		}
           for (int i=0; i < numItems; i++)
           {
               LegendElement item = legend.legendItems.elementAt(i);
               item.include = chooseCB[i].isSelected();
           }
           //legend.size = Integer.parseInt((String)sizeCombo.getSelectedItem());
           return legend;
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


  public void actionPerformed(ActionEvent ev)
  {
        if(ev.getSource() == checkAllButton)
        {
             for(int i=0; i < numItems; i++)
             {
                 chooseCB[i].setSelected(true);
             }
        }

        if(ev.getSource() == clearAllButton)
        {
            for(int i=0; i < numItems; i++)
             {
                 chooseCB[i].setSelected(false);
             }
        }
        if(ev.getSource() == titleColorButton)
        {
        	Color newColor = JColorChooser.showDialog(this,"Select color", titleColorButton.getBackground());
            if(newColor != null)
            {              
                titleColorButton.setBackground(newColor);
            }
        }
        if(ev.getSource() == textColorButton)
        {
        	Color newColor = JColorChooser.showDialog(this,"Select color", textColorButton.getBackground());
            if(newColor != null)
            {              
                textColorButton.setBackground(newColor);
            }
        }
        if(ev.getSource() == borderColorButton)
        {
        	Color newColor = JColorChooser.showDialog(this,"Select color", borderColorButton.getBackground());
            if(newColor != null)
            {              
                borderColorButton.setBackground(newColor);
            }
        }
  }


}
