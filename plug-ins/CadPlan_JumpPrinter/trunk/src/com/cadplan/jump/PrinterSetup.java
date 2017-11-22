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
import com.vividsolutions.jump.util.Blackboard;
import com.vividsolutions.jump.util.java2xml.Java2XML;
import com.vividsolutions.jump.workbench.ui.WorkbenchFrame;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import javax.swing.*;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.ItemListener;
import java.awt.event.ItemEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.print.PageFormat;
import java.awt.print.Paper;
import java.awt.print.PrinterJob;
import java.awt.*;
import java.awt.geom.Point2D;
import java.util.StringTokenizer;
import java.util.Vector;
import java.text.DecimalFormat;
import java.io.File;

/**
 * User: geoff
 * Date: 23/12/2006
 * Time: 07:44:45
 * Copyright 2005 Geoffrey G Roy.
 */
public class PrinterSetup extends JDialog implements ActionListener, ItemListener, WindowListener
{

    private JLabel scaleLabel, printLabel;
    private JTextField scaleField;
    static public JCheckBox autoCB, qualityCB, singlePageCB;
    private JButton setupButton, cancelButton, printButton, aboutButton, furnitureButton, loadButton, saveButton,
          zoomInButton,zoomOutButton,zoom100Button, helpButton, saveImageButton;
    
    private JComboBox printSizeCombo, printQualityCombo, configCombo;
    public boolean cancelled = false;
    private PageFormat pageFormat = null;
    private PrinterJob printerJob = PrinterJob.getPrinterJob();
    double imageableWidth = 462.0;   // guess initally for A4
    double imageableHeight = 692.0;
    double scale  = 1000.0;
    boolean autoScale = true;
    boolean qualityOption = true;
    int scaleLength;
    int scaleInterval;
    PrinterPreview preview;
    Blackboard blackboard;
    PlugInContext context;
    String [] paperSizes = {"A4","A4R","A3","A3R"};
    I18NPlug iPlug;
    //String [] qualityItems = {"OJ Quality Renderer", "OJ Accurate Renderer", "External Renderer","SkyJump Renderer"};
    String [] qualityItems = {"ISA Renderer", "Core Renderer", "External Renderer"};
    Point2D.Double pageOffset;
    FurnitureTitle title = new FurnitureTitle("",new Font("SansSerif",Font.PLAIN,24),new Rectangle(0,0,50,20),false);
    FurnitureScale scaleItem = new FurnitureScale(scale,100.0,10.0, new Rectangle(0,50,150,25),false);
    FurnitureBorder border = new FurnitureBorder(1.0,false,false);
    FurnitureNorth north = new FurnitureNorth(0,new Rectangle(0,100,50,50),false);
    Vector<FurnitureBorder> borders = new Vector<FurnitureBorder>();
    FurnitureNote note = new FurnitureNote("",new Font("SansSerif",Font.PLAIN,12),0,0, new Rectangle(0,150,50,20),false);
    Vector<FurnitureNote> notes = new Vector<FurnitureNote>();
    Vector<FurnitureImage> imageItems = new Vector<FurnitureImage>();
    
    FurnitureLegend legend = null;
    LayerLegend layerLegend = null;
    String configFileName = null;
    int printMode = 0;
    
    public boolean saveAsImage = false;
    private Vector<String> configNames;
    private int selectedConfigIndex = 0;
    private boolean printSinglePage = false;


   public PrinterSetup(PlugInContext context,PrinterPreview preview, Blackboard blackboard, I18NPlug iPlug)
   {
       super(new JFrame(),iPlug.get("JumpPrinter.Setup"), true);
       this.context = context;
       this.preview = preview;
       this.blackboard = blackboard;
       this.iPlug = iPlug;
       scaleItem.setIPlug(this.iPlug);
       notes.addElement(note);
       restoreBlackboard();


       // set up user interface
       
        
       
       
       GridBagDesigner gb = new GridBagDesigner(this);


       aboutButton = new JButton(iPlug.get("JumpPrinter.Setup.About"));
       aboutButton.addActionListener(this);
       gb.setPosition(0,0);
       gb.setInsets(0,5,0,0);
       gb.setFill(GridBagConstraints.HORIZONTAL);
       gb.addComponent(aboutButton);

       autoCB = new JCheckBox(iPlug.get("JumpPrinter.Setup.FitToPage") );
       gb.setPosition(1,0);
       gb.setInsets(0,10,0,0);
       gb.addComponent(autoCB);
       autoCB.setSelected(autoScale);

//       qualityCB = new JCheckBox(iPlug.get("JumpPrinter.Setup.Quality"));
//       gb.setPosition(2,0);
//       gb.setInsets(0,0,0,0);
//       gb.addComponent(qualityCB);
//       qualityCB.setSelected(qualityOption);

       printQualityCombo = new JComboBox(qualityItems);
       gb.setPosition(2,0);
       gb.setInsets(0,0,0,5);
       gb.addComponent(printQualityCombo);
       if(printMode >= qualityItems.length) printMode = 0;
       printQualityCombo.setSelectedIndex(printMode);



       scaleLabel = new JLabel(iPlug.get("JumpPrinter.Setup.Scale"));
       gb.setPosition(3,0);
       gb.setInsets(0,0,0,0);
       gb.setAnchor(GridBagConstraints.EAST);
       gb.addComponent(scaleLabel);

       scaleField = new JTextField(10);
       scaleField.addActionListener(this);
       scaleField.setText(formatScale(scale));
       scaleField.setMinimumSize(new Dimension(70,20));
       gb.setPosition(4,0);
       gb.setInsets(0,0,0,0);
       gb.addComponent(scaleField);

       JPanel zoomPanel = new JPanel();
       GridBagDesigner gbz = new GridBagDesigner(zoomPanel);

       zoomInButton = new JButton("+");
       zoomInButton.setMargin(new Insets(0,5,0,5));
       zoomInButton.setFont(new Font("SansSerif",Font.BOLD,14));
       gbz.setPosition(0,0);
       gbz.addComponent(zoomInButton);
       zoomInButton.addActionListener(this);


       zoom100Button = new JButton("O");
       zoom100Button.setMargin(new Insets(0,5,0,5));
       zoom100Button.setFont(new Font("SansSerif",Font.BOLD,14));
       gbz.setPosition(1,0);
       gbz.addComponent(zoom100Button);
       zoom100Button.addActionListener(this);


       zoomOutButton = new JButton("-");
       zoomOutButton.setMargin(new Insets(0,6,0,6));
       zoomOutButton.setFont(new Font("SansSerif",Font.BOLD,14));
       gbz.setPosition(2,0);
       gbz.addComponent(zoomOutButton);
       zoomOutButton.addActionListener(this);



       gb.setPosition(5,0);
       gb.setInsets(0,5,0,0);
       gb.setAnchor(GridBagConstraints.WEST);
       gb.addComponent(zoomPanel);

       furnitureButton = new JButton(iPlug.get("JumpPrinter.Setup.Furniture"));
       furnitureButton.addActionListener(this);
       gb.setPosition(6,0);
       gb.setInsets(0,5,0,0);
       //gb.setFill(GridBagConstraints.HORIZONTAL);
       gb.addComponent(furnitureButton);

       helpButton = new JButton(iPlug.get("JumpPrinter.Setup.Help"));
       helpButton.addActionListener(this);
       gb.setPosition(7,0);
       gb.setInsets(0,5,0,10);
       gb.setAnchor(GridBagConstraints.EAST);
       gb.addComponent(helpButton);

//       printLabel = new JLabel("Print Size");
//       gb.setPosition(5,0);
//       gb.setInsets(0,10,0,0);
//       gb.setAnchor(GridBagConstraints.EAST);
//       gb.addComponent(printLabel);
//
//       printSizeCombo = new JComboBox(paperSizes);
//       gb.setPosition(6,0);
//       gb.setInsets(3,10,10,0);
//       gb.addComponent(printSizeCombo);



       gb.setPosition(0,1);
       gb.setSpan(8,1);
       gb.setFill(GridBagConstraints.BOTH);
       gb.setWeight(1.0,1.0);
       gb.addComponent(preview);

       JPanel bottomPanel = new JPanel();
       GridBagDesigner gbb = new GridBagDesigner(bottomPanel);

       cancelButton = new JButton(iPlug.get("JumpPrinter.Setup.Cancel"));
       cancelButton.addActionListener(this);
       gbb.setPosition(0,0);
       gbb.setInsets(0,5,0,0);
       //gb.setFill(GridBagConstraints.HORIZONTAL);
       gbb.addComponent(cancelButton);

       loadButton = new JButton(iPlug.get("JumpPrinter.Setup.LoadCfg"));
       loadButton.addActionListener(this);
       gbb.setPosition(1,0);
       gbb.setInsets(0,10,0,0);
       //gb.setFill(GridBagConstraints.HORIZONTAL);
       gbb.addComponent(loadButton);
       configNames = (Vector<String>) blackboard.get("ConfigFiles");       
       if(configNames.size() < 1) configNames.add("Default");   // fix of vector is empty at this point
       int item = (Integer) blackboard.get("ConfigItem",0);
       if(item < 0 ) item = 0;
       if(item >= configNames.size()) item = 0;    // may occur if a new project has been loaded
       configCombo = new JComboBox(configNames);
       //configCombo.setPreferredSize(new Dimension(150,25));
       configCombo.setEditable(true);
       gbb.setPosition(2,0);
       gb.setInsets(0,0,0,0);
       gb.setWeight(1.0, 0.0);
       gbb.addComponent(configCombo);
       configCombo.setSelectedIndex(item);
       configCombo.addActionListener(this);

       saveButton = new JButton(iPlug.get("JumpPrinter.Setup.SaveCfg"));
       saveButton.addActionListener(this);
       gbb.setPosition(3,0);
       gbb.setInsets(0,0,0,0);
       gbb.setSpan(1,2);
       gbb.setFill(GridBagConstraints.HORIZONTAL);
       gbb.addComponent(saveButton);

       setupButton = new JButton(iPlug.get("JumpPrinter.Setup.PageSetup"));
       setupButton.addActionListener(this);
       gbb.setPosition(5,0);
       gbb.setInsets(0,10,0,0);
       gbb.setAnchor(GridBagConstraints.WEST);
       gbb.setFill(GridBagConstraints.HORIZONTAL);
       gbb.addComponent(setupButton);

       printButton = new JButton(iPlug.get("JumpPrinter.Setup.Print"));
       printButton.addActionListener(this);
       gbb.setPosition(6,0);
       gbb.setInsets(0,10,0,0);
       gbb.setAnchor(GridBagConstraints.WEST);
       gbb.setFill(GridBagConstraints.HORIZONTAL);
       gbb.addComponent(printButton);
       
       
       singlePageCB = new JCheckBox(iPlug.get("JumpPrinter.Setup.SinglePage"));
       gbb.setPosition(7,0);
       gbb.setInsets(0,0,0,0);
       gbb.addComponent(singlePageCB);
       singlePageCB.setSelected(printSinglePage);
       

      saveImageButton = new JButton(iPlug.get("JumpPrinter.Setup.SaveImage"));
      saveImageButton.addActionListener(this);
      gbb.setPosition(8,0);
      gbb.setInsets(0,10,0,10);
      gbb.setAnchor(GridBagConstraints.WEST);
      gbb.setFill(GridBagConstraints.HORIZONTAL);
      gbb.addComponent(saveImageButton);



       gb.setPosition(0,2);
       gb.setInsets(0,0,0,0);
       gb.setSpan(8,1);
       gb.setAnchor(GridBagConstraints.WEST);
       gb.setWeight(1.0,0.0);
       gb.addComponent(bottomPanel);

       legend.updateLegend(context,iPlug);
       layerLegend.updateLegend(context,iPlug);
       updateDrawing();

       autoCB.addItemListener(this);
       singlePageCB.addItemListener(this);
       //qualityCB.addItemListener(this);
       printQualityCombo.addItemListener(this);
//       drawBorderCB.addItemListener(this);
//       drawScaleCB.addItemListener(this);
//       printSizeCombo.addItemListener(this);

       //preview.setPreferredSize(new Dimension(800,600));
       pack();
       //setResizable(false);
       addWindowListener(this);
       setVisible(true);

   }

    /**
     * gets the set scale
     * @return  the scale
     */
    public double getScale()
    {
        return scale;
    }

    /**
     * gets the postScale setting
     * @return
     */
    public boolean getQualityOption()
    {
        return qualityOption;
    }
    public int getPrintMode()
    {
        return printMode;
    }
    
    public boolean getPrintSinglePage()
    {
    	return printSinglePage;
    }

    /**
     * updates drawing
     */
    public void updateDrawing()
    {
        //border.location = new Rectangle(0,0,preview.getPrintSize().width, preview.getPrintSize().height);
        preview.setBorder(border);
        preview.setBorders(borders);
        preview.setTitle(title);
        preview.setScaleItem(scaleItem);
        preview.setNorth(north);
        preview.setNote(note);
        preview.setNotes(notes);
        preview.setLegend(legend);
        preview.setLayerLegend(layerLegend);
        preview.setImages(imageItems);
        preview.setPrintSinglePage(printSinglePage);
       // JOptionPane.showMessageDialog(this,"update: "+imageableWidth+","+imageableHeight+" scale="+scale);

        if(autoCB.isSelected())
        {
            scale = preview.getAutoScale(imageableWidth, imageableHeight);
            scaleItem.scale = scale;
            scaleField.setText(formatScale(scale));
            preview.setOffsets(new Point2D.Double(0.0,0.0));
            border.setBorder(0,0,preview.getPrintSize().width, preview.getPrintSize().height, false);
           // border.location = new Rectangle(0,0,preview.getPrintSize().width, preview.getPrintSize().height);

        }
        preview.setPaper(imageableWidth, imageableHeight, scale);
        if(border.fixed)  border.setBorder(border.location.x, border.location.y,preview.getPrintSize().width, preview.getPrintSize().height, false);
        else  border.setBorder(0, 0,preview.getPrintSize().width, preview.getPrintSize().height, false);
        preview.setBorder(border);
        preview.repaint();
        
    }


    /**
     * formats scale display
     * @param v
     * @return formatted scale
     */
   private String formatScale(double v)
    {
        DecimalFormat scaleFormat= null;
        if (v >= 10.0 || v == 0.0)  scaleFormat = new DecimalFormat("####");
        else if(v >= 1.0 && v < 10.0) scaleFormat = new DecimalFormat("##0.0");
        else scaleFormat = new DecimalFormat("0.0E0");
        return scaleFormat.format(v);
    }

    /**
     * gets PageFormat
     * @return   PageFormat
     */
    public PageFormat getPageFormat()
    {
        return pageFormat;
    }

    /**
     * validate value in scale field
     * @return
     */
    private boolean validScaleValue()
    {
        double tempScale;
        try
        {
            String scaleValue = scaleField.getText();
            scaleValue = scaleValue.replaceAll(",","");
            scaleValue = scaleValue.replaceAll(" ","");
            tempScale = Double.parseDouble(scaleValue.replaceAll(",",""));
        }
        catch(NumberFormatException ex)
        {
            JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Setup.Message2")+ "["+scaleField.getText()+"]",iPlug.get("JumpPrinter.Error"),
                    JOptionPane.ERROR_MESSAGE);
            return false;
        }
        if(tempScale <= 0.0)
        {
            JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Setup.Message1"),iPlug.get("JumpPrinter.Error"),
                    JOptionPane.ERROR_MESSAGE);
            scaleField.setText(formatScale(scale));
            return false;

        }
        scale = tempScale;
        scaleItem.scale = scale;
        return true;
    }

    /**
     * updates the blackboard ffrom current settings
     */
    private void updateBlackboard()
    {
         blackboard.put("PageFormat",pageFormat);
         blackboard.put("Scale", scale);
         blackboard.put("Quality", qualityOption);
         blackboard.put("AutoScale", autoScale);
         blackboard.put("PageOffset", preview.getOffsets());
         blackboard.put("Title", title);
         blackboard.put("ScaleItem", scaleItem);
         blackboard.put("Border", border);
         blackboard.put("Borders", borders);
         blackboard.put("North", north);
         blackboard.put("Note", note);
         blackboard.put("Notes", notes);
         blackboard.put("Legend", legend);
         blackboard.put("LayerLegend", layerLegend);
         blackboard.put("Images", imageItems);
         blackboard.put("PrintMode", printMode);
         blackboard.put("SinglePage", singlePageCB.isSelected());
    }

    /**
     * restores curent settings from blackboard
     */
    private void restoreBlackboard()
    {
        try
       {
           Object object = null;
           object = blackboard.get("PageFormat",null);
           pageFormat = (PageFormat) object;
           imageableWidth = pageFormat.getImageableWidth();
           imageableHeight = pageFormat.getImageableHeight();
       }
       catch (Exception ex)
       {
       }

       try
       {
           scale = blackboard.getDouble("Scale");
           scaleField.setText(formatScale(scale));          
           autoCB.setSelected(false);
       }
       catch (Exception ex)
       {
       }
        try
       {
           Object object = null;
           object = blackboard.get("AutoScale",null);
           autoScale = (Boolean) object;
           autoCB.setSelected(autoScale);
       }
       catch (Exception ex)
       {
       }
       try
       {
           Object object = null;
           object = blackboard.get("Quality",null);
           qualityOption = (Boolean) object;
           qualityCB.setSelected(qualityOption);
       }
       catch (Exception ex)
       {
       }
       try
       {
           Object object = null;
           object = blackboard.get("SinglePage",null);
           printSinglePage = (Boolean) object;
           //System.out.println("Singlepage: "+ printSinglePage);
           singlePageCB.setSelected(printSinglePage);
       }
       catch (Exception ex)
       {
       }
        try
       {
           printMode = blackboard.getInt("PrintMode");
           if(printMode >= qualityItems.length) printMode = 0;
           printQualityCombo.setSelectedIndex(printMode);
       }
       catch (Exception ex)
       {
       }

       try
       {
           Object object = null;
           object = blackboard.get("PageOffset",null);
           pageOffset = (Point2D.Double) object;
           preview.setOffsets(pageOffset);
       }
       catch (Exception ex)
       {
       }

       try
       {
           Object object = null;
           object = blackboard.get("Title",null);
           if(object != null) title = (FurnitureTitle)  object;
       }
       catch (Exception ex)
       {
       }
       try
       {
           Object object = null;
           object = blackboard.get("ScaleItem",null);
           if(object != null) scaleItem = (FurnitureScale)  object;
           scaleField.setText(formatScale(scaleItem.scale));
           autoCB.setSelected(false);
       }
       catch (Exception ex)
       {
       }
       try
       {
           Object object = null;
           object = blackboard.get("Border",null);
           if(object != null) border = (FurnitureBorder)  object;
       }
       catch (Exception ex)
       {
       }
       try
       {
           Object object = null;
           object = blackboard.get("Borders",null);
           if(object != null) borders = (Vector<FurnitureBorder>)  object;
       }
       catch (Exception ex)
       {
       }
       try
       {
           Object object = null;
           object = blackboard.get("North",null);
           if(object != null) north = (FurnitureNorth)  object;
       }
       catch (Exception ex)
       {
       }
       try
       {
           Object object = null;
           object = blackboard.get("Note",null);
           if(object != null) note = (FurnitureNote)  object;
       }
       catch (Exception ex)
       {
       }
       try
       {
           Object object = null;
           object = blackboard.get("Notes",null);
           if(object != null) notes = (Vector<FurnitureNote>)  object;
       }
       catch (Exception ex)
       {
       }
       try
       {
           Object object = null;
           object = blackboard.get("ConfigFilePath",null);
           if(object != null) configFileName = (String) object;
       }
       catch (Exception ex)
       {
       }
        try
       {
           Object object = null;
           object = blackboard.get("Legend",null);
           if(object != null) legend = (FurnitureLegend)  object;
           else
           {
               legend = new FurnitureLegend(context,new Rectangle(0,200,50,100));
           }
       }
       catch (Exception ex)
       {
           
       }

        try
       {
           Object object = null;
           object = blackboard.get("LayerLegend",null);
           if(object != null) layerLegend = (LayerLegend)  object;
           else
           {
               layerLegend = new LayerLegend(context,new Rectangle(0,300,50,100));
           }
       }
       catch (Exception ex)
       {

       }
       try
       {
           Object object = null;
           object = blackboard.get("Images",null);
           if(object != null) imageItems = (Vector<FurnitureImage>)  object;
       }
       catch (Exception ex)
       {
       }
    }
    /**
     * handles ActionEvents
     * @param ev
     */
    
    public void actionPerformed(ActionEvent ev)
    {
         if(ev.getSource() == setupButton)
         {
            if(pageFormat == null)
            {
               pageFormat = printerJob.defaultPage(); 
            }

             try
             {
            	 Paper paper = pageFormat.getPaper();
            	 //System.out.println("Paper: "+paper.getWidth()+","+paper.getHeight());
                  pageFormat = printerJob.pageDialog(pageFormat);
             }
             catch (Exception ex)
             {
                 System.out.println("SETUP ERROR 1: "+ex);
                 pageFormat = printerJob.defaultPage();
                 try
                 {
                     pageFormat = printerJob.pageDialog(pageFormat);
                 }
                 catch (Exception ex2)
                 {
                     System.out.println("SETUP ERROR 2: "+ex2);                     
                     ex2.printStackTrace();
                     JOptionPane.showMessageDialog(this,"ERROR: in setting page format.\n"+ex);
                 }
             }

            imageableWidth = pageFormat.getImageableWidth();
            imageableHeight = pageFormat.getImageableHeight();
            updateDrawing();
         }
        if(ev.getSource() == cancelButton)
         {
             cancelled = true;
             updateBlackboard();
             dispose();
         }
        if(ev.getSource() == printButton)
         {
             cancelled = false;
             updateBlackboard();
             dispose();
         }
          if(ev.getSource() == saveImageButton)
         {
        	  //System.out.println("Saving image");
        	 
        	  
             cancelled = false;
             updateBlackboard();
             saveAsImage = true;
             dispose();
         }
          

        if(ev.getSource() == scaleField)
        {
            if(!validScaleValue()) return;
            autoCB.setSelected(false);
            updateDrawing();
            //repaint();
        }
        if(ev.getSource() == aboutButton)
        {
            JOptionPane.showMessageDialog(this,"Jump Printer Plugin: vers:"+blackboard.get("Version","xxx")+
            "\n© 2007-11 Cadplan\n"+
            "http://www.cadplan.com.au",iPlug.get("JumpPrinter.Setup.About")+"...",JOptionPane.INFORMATION_MESSAGE);
        }

        if(ev.getSource() == helpButton)
        {
            HelpDialog help = new HelpDialog(this, iPlug);
            //OnLineHelp help = new OnLineHelp(this,"en");
        }

        if(ev.getSource() == furnitureButton)
        {
            legend.updateLegend(context,iPlug);
            layerLegend.updateLegend(context, iPlug);
            FurnitureDialog fd = new FurnitureDialog(this, title, scaleItem, border, borders,  north, notes, legend, layerLegend, imageItems, iPlug);
        }

        if(ev.getSource() == saveButton)
        {
            updateBlackboard();
            String cfileName = configFileName+"_"+configCombo.getSelectedItem()+".xml";
            if(configCombo.getSelectedItem().equals("Default")) cfileName = configFileName+".xml";
            XMLconverter converter = new XMLconverter(cfileName, iPlug);
            converter.save(blackboard);

            //preview.context.getWorkbenchFrame().setStatusMessage(iPlug.get("JumpPrinter.Setup.Message4")+configFileName+"_"+configCombo.getSelectedItem());
            preview.context.getWorkbenchFrame().setStatusMessage(iPlug.get("JumpPrinter.Setup.Message4")+" "+configCombo.getSelectedItem());

        }

        if(ev.getSource() == loadButton)
        {
            updateBlackboard();
            String cfileName = configFileName+"_"+configCombo.getSelectedItem()+".xml";
            if(configCombo.getSelectedItem().equals("Default")) cfileName = configFileName+".xml";

            //System.out.println("creating converter: iPlug:"+iPlug.pluginName);
            XMLconverter converter = new XMLconverter(cfileName, iPlug);
            if(converter.parse(blackboard))
            {
                restoreBlackboard();
                updateDrawing();
                preview.context.getWorkbenchFrame().setStatusMessage(iPlug.get("JumpPrinter.Setup.Message3")+" "+configCombo.getSelectedItem());

            }
            else
            {
                JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Setup.Message5"), iPlug.get("JumpPrinter.Warning"),
                        JOptionPane.WARNING_MESSAGE);
            }
        }

        if(ev.getSource() == zoomInButton)
        {
            preview.globalScale = preview.globalScale*1.2;
            preview.xpan = (int)(preview.xpan*1.2);
            preview.ypan = (int)(preview.ypan*1.2);
            updateDrawing();

        }
        if(ev.getSource() == zoom100Button)
        {
              preview.globalScale = 1.0;
              preview.xpan = 0;
              preview.ypan = 0;
              updateDrawing();
        }
        if(ev.getSource() == zoomOutButton)
        {
             preview.globalScale = preview.globalScale/1.2;
             preview.xpan = (int)(preview.xpan/1.2);
             preview.ypan = (int)(preview.ypan/1.2);
             updateDrawing();

        }
        if(ev.getSource() == configCombo)
        {
            //System.out.println("Action event: config combo");
            int selectedIndex = configCombo.getSelectedIndex();
            String item = (String) configCombo.getSelectedItem(); //configNames.elementAt(selectedConfigIndex);
            //System.out.println("prev index="+selectedConfigIndex+"    item="+item + "  sel:"+selectedIndex);
            if(item == null || item.length() == 0 && selectedConfigIndex > 0)   // delete item
            {
                //System.out.println("Index: "+selectedConfigIndex);
               // configCombo.removeActionListener(this);

               //System.out.println("Removing: "+selectedConfigIndex);
               int response = JOptionPane.showConfirmDialog(this,iPlug.get("JumpPrinter.Setup.Message9")+ " "+
                  configNames.elementAt(selectedConfigIndex), iPlug.get("JumpPrinter.Warning"), JOptionPane.YES_NO_OPTION);
                if( response == JOptionPane.YES_OPTION)
                {
                   File cfile= new File(configFileName+"_"+configNames.elementAt(selectedConfigIndex)+".xml");
                   boolean OK = cfile.delete();
                   //System.out.println("File: "+cfile.getAbsolutePath()+" deleted:"+OK);
    //               configCombo.removeItemAt(selectedConfigIndex);
                   configNames.remove(selectedConfigIndex);
                   selectedConfigIndex = 0;
                   configCombo.setSelectedIndex(0);

                }
                else
                {
                    configCombo.setSelectedIndex(selectedConfigIndex);
                }
                //configCombo.addActionListener(this);
            }
            else if(item.length() > 0 && !configNames.contains(item))    // add item
            {
                //configNames.addElement(item);
                item = item.replaceAll(" ","_");
                configCombo.addItem(item);
                configCombo.setSelectedItem(item);
                //System.out.println("Index: "+selectedConfigIndex);

                //System.out.println("Added: "+ item);
            }
            else         // display item
            {
                 selectedConfigIndex = configCombo.getSelectedIndex();
                //System.out.println("Selected index: "+selectedConfigIndex);
            }
//            System.out.println("\nConfig Names:");
//            for(int i=0; i < configNames.size(); i++)
//            {
//                System.out.println(configNames.elementAt(i));
//            }

            blackboard.put("ConfigFiles",configNames);
            blackboard.put("ConfigItem",configCombo.getSelectedIndex());
        }
    }

    /**
     * handles ItemStateChnages events
     * @param ev
     */
    public void itemStateChanged(ItemEvent ev)
    {
       if(ev.getSource() == autoCB)
       {
           if(!validScaleValue()) return; 
           autoScale = autoCB.isSelected();
           
           updateDrawing();
       }
       if(ev.getSource() == singlePageCB)
       {
    	   printSinglePage = singlePageCB.isSelected();
    	   updateDrawing();
       }
        if(ev.getSource() == qualityCB)
       {
           preview.setBorder(border);
           qualityOption = qualityCB.isSelected();
           if(!qualityOption)
           {
               JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Setup.Message7"),iPlug.get("JumpPrinter.Warning"), JOptionPane.WARNING_MESSAGE);
//               JOptionPane.showMessageDialog(this,"Choosing the \'Accurate\' option may not correctly scale raster\n"+
//               "image layers.  Use the \'Quality\' option for raster layers.","Warning...", JOptionPane.WARNING_MESSAGE);
           }
       }

       if(ev.getSource() == printQualityCombo)
       {
           preview.setBorder(border);
           int previousMode = printMode;
           printMode = printQualityCombo.getSelectedIndex();
//           if(printMode == 1 && previousMode != printMode)
//           {
//               JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Setup.Message7"),iPlug.get("JumpPrinter.Warning"), JOptionPane.WARNING_MESSAGE);
//           }
//           if(printMode == 2 && previousMode != printMode)
//           {
//               JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Setup.Message8"),iPlug.get("JumpPrinter.Warning"), JOptionPane.WARNING_MESSAGE);
//           }
       }
//         if(ev.getSource() == drawScaleCB)
//       {
//
//           preview.setDrawScale(drawScaleCB.isSelected());
//           updateDrawing();
//       }
    }

	@Override
	public void windowActivated(WindowEvent e)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowClosed(WindowEvent e)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowClosing(WindowEvent e)
	{
		// TODO Auto-generated method stub
		cancelled = true;
        updateBlackboard();
        dispose();
		
	}

	@Override
	public void windowDeactivated(WindowEvent e)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowDeiconified(WindowEvent e)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowIconified(WindowEvent e)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowOpened(WindowEvent e)
	{
		// TODO Auto-generated method stub
		
	}

}
