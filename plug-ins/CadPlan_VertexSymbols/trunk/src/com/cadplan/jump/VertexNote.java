package com.cadplan.jump;

import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.LayerEventType;
import com.vividsolutions.jump.workbench.ui.renderer.style.VertexStyle;
import com.vividsolutions.jump.workbench.ui.SchemaPanel;
import com.vividsolutions.jump.feature.*;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;
import com.cadplan.designer.GridBagDesigner;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import javax.swing.event.ChangeEvent;
import java.util.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * User: geoff
 * Date: 1/08/2007
 * Time: 08:29:28
 * Copyright 2005 Geoffrey G Roy.
 */
public class VertexNote extends JDialog implements ActionListener, ChangeListener
{
    boolean debug = false;
    PlugInContext context;
    I18NPlug iPlug;
    JCheckBox showLabelCB;
    TextArea textArea;
    JButton cancelButton, clearButton, acceptButton, resetButton;
    Layer[] layers;
    String textValue;
    boolean showLabel;
    VertexStyle vertexStyle;
    Feature selectedFeature;
    int textAttributeIndex;
    FeatureDataset dataset;
    boolean allowEdit = true;

    JTabbedPane tabbedPane;
    VectorPanel vectorPanel;
    ImagePanel imagePanel;
    WKTPanel wktPanel;
    TextLabelPanel labelPanel;
    ButtonGroup group, rotateGroup;
    JRadioButton absValueRB, byAttributeRB;
    JComboBox attributeCB;
    JScrollPane scrollPane, scrollPane2;
    String symbolName = "";
    int symbolType;
    int symbolNumber;

    public VertexNote(PlugInContext context, I18NPlug iPlug)
    {
        super(new JFrame(),iPlug.get("VertexNote.Dialog.Editor"), true);
        this.context = context;
        this.iPlug = iPlug;

        layers = context.getSelectedLayers();

        if(layers.length > 1)
        {
            return;
        }
        if(layers == null || layers.length == 0)
        {
            JOptionPane.showMessageDialog(null,iPlug.get("VertexNote.Dialog.Message2"),
                        "Warning...",JOptionPane.WARNING_MESSAGE);
            return;
        }
        boolean isEditable  = layers[0].isEditable();
        if(!isEditable)
        {
             JOptionPane.showMessageDialog(null,iPlug.get("VertexNote.Dialog.Message3"),
                        "Warning...",JOptionPane.WARNING_MESSAGE);
            return;
        }
        vertexStyle = layers[0].getVertexStyle();
        VertexParams.selectedLayer = layers[0];
        try
        {
            String textAttributeName = null;
            try
            {
                 textAttributeName = ((ExternalSymbolsType) vertexStyle).getTextAttributeName();
            }
            catch (ClassCastException ex2)
            {
                JOptionPane.showMessageDialog(null,iPlug.get("VertexNote.Dialog.Message4"),
                        "Warning...", JOptionPane.WARNING_MESSAGE);
//                System.out.println("Class cast Excpetion:");
//                ex2.printStackTrace();
                return;
            }

            selectedFeature = getSelectedPoint();
            if(selectedFeature == null)
            {
                 JOptionPane.showMessageDialog(null,iPlug.get("VertexNote.Dialog.Message1"),
                        "Warning...",JOptionPane.WARNING_MESSAGE);
                return;

            }
            if(debug) System.out.println("Feature="+selectedFeature);

            //FeatureSchema featureSchema = layers[0].getFeatureCollectionWrapper().getFeatureSchema();
            FeatureSchema featureSchema = selectedFeature.getSchema();
            if(debug) System.out.println("Initial feature size: "+featureSchema.getAttributeCount());

            try
            {
                int i = featureSchema.getAttributeIndex("ShowLabel");
                if(debug) System.out.println("ShowLabel found at i="+i);
            }
            catch (Exception ex)
            {
                addAttribute(layers[0],"ShowLabel",AttributeType.INTEGER);
//                JOptionPane.showMessageDialog(null,"You must first create an attribute named \"ShowLabel\" of type \"Integer\"",
//                        "Warning...",JOptionPane.WARNING_MESSAGE);
                //return;
//                featureSchema.addAttribute("ShowLabel", AttributeType.INTEGER);
//                selectedFeature.setSchema(featureSchema);
//                System.out.println("Attribute ShowLabel added, number="+featureSchema.getAttributeCount()+","+selectedFeature.getAttributes().length);
            }

            try
            {
                int i = featureSchema.getAttributeIndex("SymbolName");
                if(debug) System.out.println("SymbolName found at i="+i);
            }
            catch (Exception ex)
            {
                addAttribute(layers[0],"SymbolName",AttributeType.STRING);
            }


            if(textAttributeName.equals("$FID")) textAttributeIndex = -1;
            else if(textAttributeName.equals("$POINT")) textAttributeIndex = -2;
            else
            {
                try
                {
                   textAttributeIndex = featureSchema.getAttributeIndex(textAttributeName);
                }
                catch(IllegalArgumentException ex)
                {
                    textAttributeIndex = -1;
                }
            }


            if(textAttributeIndex < 0)
            {
                // fail
//                JOptionPane.showMessageDialog(null,"Cannot edit $FID or $POINT attributes","Warning...", JOptionPane.WARNING_MESSAGE);
//                return;
                allowEdit = false;
            }
//            else if(featureSchema.getAttributeType(textAttributeName) != AttributeType.STRING)
//            {
//                //fail
////                JOptionPane.showMessageDialog(null,"Can only edit STRING attributes","Warning...", JOptionPane.WARNING_MESSAGE);
////                return;
//                allowEdit = false;
//            }
            ((ExternalSymbolsType)vertexStyle).setTextAttributeValue(selectedFeature);
            
            textValue = ((ExternalSymbolsType) vertexStyle).getTextAttributeValue();
            showLabel = ((ExternalSymbolsType) vertexStyle).getShowNote();
            if(vertexStyle instanceof ExternalSymbolsImplType)
            {
                 symbolName = ((ExternalSymbolsImplType) vertexStyle).getActualSymbolName();
                 symbolType = ((ExternalSymbolsImplType) vertexStyle).getSymbolType();
                 symbolNumber = ((ExternalSymbolsImplType) vertexStyle).getSymbolNumber();
                if(debug) System.out.println("Getting current symbol: name="+symbolName+"  number="+symbolNumber+"  type="+symbolType);

            }

            init();
        }
        catch (Exception ex)
        {
            System.out.println("ERROR: "+ex);
            ex.printStackTrace();
        }
    }
    //==========================================================================
    // These two methods (modified)  have been taken from ViewSchemaPlugin to modify an
    // existing schema
    //========================================================================
    private void addAttribute(final Layer layer, String newAttName, AttributeType newAttType)
    {


        FeatureSchema newSchema = new FeatureSchema();
        FeatureSchema oldSchema = layers[0].getFeatureCollectionWrapper().getFeatureSchema();
        int numAtt = oldSchema.getAttributeCount();
        for (int i=0; i < numAtt; i++)
        {
            String attName = oldSchema.getAttributeName(i);
            AttributeType attType = oldSchema.getAttributeType(i);
            newSchema.addAttribute(attName, attType);
        }
        newSchema.addAttribute(newAttName,newAttType);


        java.util.List originalFeatures = layer.getFeatureCollectionWrapper().getFeatures();
        ArrayList tempFeatures = new ArrayList();

        //Two-phase commit. Phase 1: check that no conversion errors occur. [Jon Aquino]
        for (Iterator i = layer.getFeatureCollectionWrapper().iterator();i.hasNext();)
        {
            Feature feature = (Feature) i.next();
            tempFeatures.add(convert(feature, newSchema));
        }

        //Phase 2: commit. [Jon Aquino]
        for (int i = 0; i < originalFeatures.size(); i++)
        {
            Feature originalFeature = (Feature) originalFeatures.get(i);
            Feature tempFeature = (Feature) tempFeatures.get(i);

            //Modify existing features rather than creating new features, because
            //there may be references to the existing features (e.g. Attribute Viewers).
            //[Jon Aquino]
            originalFeature.setSchema(tempFeature.getSchema());
            originalFeature.setAttributes(tempFeature.getAttributes());
        }

        //Non-undoable. [Jon Aquino]
        layer.getLayerManager().getUndoableEditReceiver().getUndoManager()
             .discardAllEdits();
        layer.setFeatureCollection(new FeatureDataset(originalFeatures,
                newSchema));
        layer.fireLayerChanged(LayerEventType.METADATA_CHANGED);

    }
    private void delAttribute(final Layer layer, String delAttName)
    {


        FeatureSchema newSchema = new FeatureSchema();
        FeatureSchema oldSchema = layers[0].getFeatureCollectionWrapper().getFeatureSchema();
        int numAtt = oldSchema.getAttributeCount();
        int delItem = -1;
        for (int i=0; i < numAtt; i++)
        {
            String attName = oldSchema.getAttributeName(i);
            if(!attName.equals(delAttName))
            {
           	    AttributeType attType = oldSchema.getAttributeType(i);
                newSchema.addAttribute(attName, attType);
            }
            else
            {
            	delItem = i;
            }
        }
        


        java.util.List originalFeatures = layer.getFeatureCollectionWrapper().getFeatures();
        ArrayList tempFeatures = new ArrayList();

        //Two-phase commit. Phase 1: check that no conversion errors occur. [Jon Aquino]
        for (Iterator i = layer.getFeatureCollectionWrapper().iterator();i.hasNext();)
        {
            Feature feature = (Feature) i.next();
            tempFeatures.add(convertDel(feature, delItem, newSchema));
        }

        //Phase 2: commit. [Jon Aquino]
        for (int i = 0; i < originalFeatures.size(); i++)
        {
            Feature originalFeature = (Feature) originalFeatures.get(i);
            Feature tempFeature = (Feature) tempFeatures.get(i);

            //Modify existing features rather than creating new features, because
            //there may be references to the existing features (e.g. Attribute Viewers).
            //[Jon Aquino]
            originalFeature.setSchema(tempFeature.getSchema());
            originalFeature.setAttributes(tempFeature.getAttributes());
        }

        //Non-undoable. [Jon Aquino]
        layer.getLayerManager().getUndoableEditReceiver().getUndoManager()
             .discardAllEdits();
        layer.setFeatureCollection(new FeatureDataset(originalFeatures,
                newSchema));
        layer.fireLayerChanged(LayerEventType.METADATA_CHANGED);

    }

     private Feature convert(Feature oldFeature, FeatureSchema newSchema)
     {
        Feature newFeature = new BasicFeature(newSchema);
        Object [] oldAttributes= oldFeature.getAttributes();
        for (int i = 0; i < oldAttributes.length; i++)
        {
            newFeature.setAttribute(i, oldAttributes[i]);
        }
        newFeature.setAttribute(oldAttributes.length,0);


        return newFeature;
    }
     
     private Feature convertDel(Feature oldFeature, int delItem, FeatureSchema newSchema)
     {
        Feature newFeature = new BasicFeature(newSchema);
        Object [] oldAttributes= oldFeature.getAttributes();
        int j = 0;
        for (int i = 0; i < oldAttributes.length; i++)
        {
        	if(i != delItem)
        	{
                newFeature.setAttribute(j, oldAttributes[i]);
                j++;
        	}
        }
        


        return newFeature;
    }
    //========================================================================================
    public void init()
    {
        JPopupMenu.setDefaultLightWeightPopupEnabled(false);
        ToolTipManager ttm = ToolTipManager.sharedInstance();
        ttm.setLightWeightPopupEnabled(false);

        GridBagDesigner gb = new GridBagDesigner(this);

        showLabelCB = new JCheckBox(iPlug.get("VertexNote.Dialog.ShowLabel"));
        gb.setPosition(0,0);
        gb.setInsets(10,10,0,0);
        //gb.setSpan(3,1);
        gb.addComponent(showLabelCB);
        showLabelCB.setSelected(showLabel);


        textArea = new TextArea("",5,50, TextArea.SCROLLBARS_VERTICAL_ONLY);

        gb.setPosition(1,0);
        gb.setInsets(10,0,0,10);
        //gb.setSpan(3,1);
        gb.setFill(GridBagConstraints.BOTH);
        gb.setWeight(1.0,1.0);
        gb.addComponent(textArea);
        textArea.setText(textValue);
        textArea.setEditable(allowEdit);

        JLabel vertexLabel = new JLabel(iPlug.get("VertexNote.Dialog.SelectSymbol"));
        gb.setPosition(0,1);
        gb.setInsets(10,10,10,0);
        gb.setAnchor(GridBagConstraints.NORTH);
        gb.addComponent(vertexLabel);




        group = new ButtonGroup();

        tabbedPane = new JTabbedPane(JTabbedPane.TOP);
        //tabbedPane.setPreferredSize(new Dimension(400,300));
        tabbedPane.addChangeListener(this);

        vectorPanel = new VectorPanel(group, iPlug);
        vectorPanel.setBackground(Color.WHITE);
        tabbedPane.addTab(iPlug.get("VertexSymbols.Dialog.Vector"), vectorPanel);

        imagePanel = new ImagePanel(group);
        imagePanel.setBackground(Color.WHITE);
        scrollPane = new JScrollPane(imagePanel);
        scrollPane.setPreferredSize(new Dimension(400,300));
        tabbedPane.addTab(iPlug.get("VertexSymbols.Dialog.Image"), scrollPane);

        wktPanel = new WKTPanel(group);
        wktPanel.setBackground(Color.WHITE);
        scrollPane2 = new JScrollPane(wktPanel);
        scrollPane2.setPreferredSize(new Dimension(400,300));
        tabbedPane.addTab(iPlug.get("VertexSymbols.Dialog.WKTshapes"), scrollPane2);

//        labelPanel = new TextLabelPanel(group, iPlug);
//       // labelPanel.setBackground(Color.WHITE);
//
//        tabbedPane.addTab("Labels", labelPanel);


        gb.setPosition(1,1);
        //gb.setSpan(4,1);
        //gb.setWeight(1.0,1.0);
        gb.setFill(GridBagConstraints.BOTH);
        gb.setInsets(10,0,10,10);
        gb.addComponent(tabbedPane);





        JPanel bottomPanel = new JPanel();
        GridBagDesigner gbb = new GridBagDesigner(bottomPanel);
        cancelButton = new JButton(iPlug.get("VertexSymbols.Dialog.Cancel"));
        gbb.setPosition(0,0);
        gbb.setInsets(10,10,10,0);
        gbb.addComponent(cancelButton);
        cancelButton.addActionListener(this);

        clearButton = new JButton(iPlug.get("VertexSymbols.Dialog.Clear"));
        gbb.setPosition(1,0);
        gbb.setInsets(10,10,10,0);
        gbb.addComponent(clearButton);
        clearButton.addActionListener(this);
        
        resetButton = new JButton(iPlug.get("VertexSymbols.Dialog.Reset"));
        gbb.setPosition(2,0);
        gbb.setInsets(10,10,10,10);
        gbb.addComponent(resetButton);
        resetButton.addActionListener(this);

        acceptButton = new JButton(iPlug.get("VertexSymbols.Dialog.Accept"));
        gbb.setPosition(3,0);
        gbb.setInsets(10,10,10,10);
        gbb.setAnchor(GridBagConstraints.EAST);
        gbb.addComponent(acceptButton);
        acceptButton.addActionListener(this);

        gb.setPosition(0,2);
        gb.setSpan(2,1);
        gb.addComponent(bottomPanel);

        setValues();

        pack();
        setVisible(true);

    }

    private void setValues()
    {
        setCurrentSymbolName();

    }
    private void getValues()
    {
         symbolName = getSymbolName();
         symbolType = getSymbolType();
    }

    public void setCurrentSymbolName()
    {
        if(debug) System.out.println("Setting current symbol: name="+symbolName+"  number="+symbolNumber+"  type="+symbolType);
        int n = vectorPanel.symbolPanel.getTypeIndex(symbolNumber, symbolType);
        //System.out.println("Setting Values: n="+n+"  symbolName="+VertexParams.symbolName+"  type="+VertexParams.symbolType+
         //          " number="+VertexParams.symbolNumber);
        //System.out.println("selectedImage="+VertexParams.selectedImage+"  selectedWKT="+VertexParams.selectedWKT);
        if(n >= 0)
        {
            vectorPanel.symbolPanel.vertexRB[n].setSelected(true);
            return;
        }
        else if(symbolType == ExternalSymbolsRenderer.IMAGES)
        {
            n = vectorPanel.symbolPanel.getImageIndex(symbolName);
            if(debug) System.out.println("Image update: "+symbolName+"  n="+n);
            if( n >= 0)
            {
                imagePanel.imageRB[n].setSelected(true);
                return;
            }
        }
        else if(symbolType == ExternalSymbolsRenderer.WKTS)
        {
            n = vectorPanel.symbolPanel.getWKTIndex(symbolName);
            if(n >= 0)
            {
                  wktPanel.imageRB[n].setSelected(true);
            }
        }
    }
    public String getSymbolName()
       {
            for (int i=0; i < vectorPanel.symbolPanel.vertexRB.length; i++)
           {
             if(vectorPanel.symbolPanel.vertexRB[i].isSelected())
             {
                 if (i < 7) return "@poly"+String.valueOf(vectorPanel.symbolPanel.sides[i]);
                 else if(i < 14) return "@star"+String.valueOf(vectorPanel.symbolPanel.sides[i]);
                 else return "@any"+String.valueOf(vectorPanel.symbolPanel.sides[i]);
             }
           }
           for(int i=0; i < imagePanel.imageRB.length; i++)
           {
               if(imagePanel.imageRB[i].isSelected())
               {
                   return VertexParams.imageNames[i];
               }
           }
           for(int i=0; i < wktPanel.imageRB.length; i++)
           {
               if(wktPanel.imageRB[i].isSelected())
               {
                   return VertexParams.wktNames[i];
               }
           }
           return null;
       }

       public int getSymbolType()
       {
           for (int i=0; i < vectorPanel.symbolPanel.vertexRB.length; i++)
           {
             if(vectorPanel.symbolPanel.vertexRB[i].isSelected())
             {
                 if (i < 7) return ExternalSymbolsRenderer.POLYS;
                 else if(i < 14) return ExternalSymbolsRenderer.STARS;
                 else return ExternalSymbolsRenderer.ANYS;
             }
           }
           for(int i=0; i < imagePanel.imageRB.length; i++)
           {
               if(imagePanel.imageRB[i].isSelected())
               {
                   return ExternalSymbolsRenderer.IMAGES;
               }
           }
           for(int i=0; i < wktPanel.imageRB.length; i++)
           {
               if(wktPanel.imageRB[i].isSelected())
               {
                   return ExternalSymbolsRenderer.WKTS;
               }
           }
           return -1;
       }

    private Feature getSelectedPoint()
    {
        Feature feature = null;
        Collection selectedItems = context.getLayerViewPanel().getSelectionManager().getFeaturesWithSelectedItems();
        if(debug) System.out.println("Number of selected items: "+selectedItems.size());
        if (selectedItems.size() != 1) return null;
        Iterator i = selectedItems.iterator();
        while (i.hasNext())
        {
            feature = (Feature) i.next();
            Geometry geometry = feature.getGeometry();
            if(debug) System.out.println("Geometry: "+geometry.toString());
            return feature;
//            if(geometry instanceof Point)
//            {
//                return feature;
//            }
        }
        return null;
    }

    public void stateChanged(ChangeEvent ev)
    {

    }
    public void actionPerformed(ActionEvent ev)
    {
         if(ev.getSource() == cancelButton)
         {
             dispose();
         }

         if(ev.getSource() == acceptButton)
         {
             int show = 0;
             getValues();
             if(showLabelCB.isSelected()) show = 1;
             FeatureSchema featureSchema = layers[0].getFeatureCollectionWrapper().getFeatureSchema();
             if(debug) System.out.println("Updated schema: num="+featureSchema.getAttributeCount());
             int numAtt = selectedFeature.getAttributes().length;
             if(debug) System.out.println("Num feature att: "+numAtt);
             try
             {
                  selectedFeature.setAttribute("ShowLabel", show);
                  selectedFeature.setAttribute("SymbolName", symbolName);
             }
             catch (IllegalArgumentException ex)
             {
                  JOptionPane.showMessageDialog(null,iPlug.get("VertexNote.Dialog.Message3"),
                          "Warning...", JOptionPane.WARNING_MESSAGE);
             }

             if(textAttributeIndex >= 0)  selectedFeature.setAttribute(textAttributeIndex,textArea.getText());
             dispose();
         }
         if(ev.getSource() == clearButton)
         {
             textArea.setText("");
         }
         
         if(ev.getSource() == resetButton)
         {
        	 //System.out.println("Resetting vertices for layer");
        	 int response = JOptionPane.showConfirmDialog(this,iPlug.get("VertexSymbols.Dialog.Warning5"),
                             "Warning...", JOptionPane.OK_CANCEL_OPTION);
        	 if(response == JOptionPane.CANCEL_OPTION) return;
        	 
        	 FeatureSchema featureSchema = selectedFeature.getSchema();
        	 try
             {
                 int i = featureSchema.getAttributeIndex("ShowLabel");
                 //System.out.println("ShowLabel found at i="+i);
                 delAttribute(layers[0],"ShowLabel");
             }
             catch (Exception ex)
             {
                 
             }

             try
             {
                 int i = featureSchema.getAttributeIndex("SymbolName");
                 //System.out.println("SymbolName found at i="+i);
                 delAttribute(layers[0],"SymbolName");
             }
             catch (Exception ex)
             {
                 
             }

             dispose();
             
             
         }
    }
}
