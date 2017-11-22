package com.cadplan.jump;

import com.cadplan.designer.GridBagDesigner;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.feature.AttributeType;

import javax.swing.*;
import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: geoff
 * Date: 14/07/2007
 * Time: 13:19:44
 * To change this template use File | Settings | File Templates.
 */
public class TextLabelPanel extends JPanel
{
     private ButtonGroup group;
     private JCheckBox enabledCB, pointsCB, linesCB, polysCB, fillCB;
     private JLabel attributeLabel, fontLabel, backgroundLabel, foregroundLabel, scopeLabel, borderLabel,
             positionLabel;
     public JComboBox attributeCombo;
     private JComboBox fontNameCombo, fontSizeCombo, fontStyleCombo, positionCombo, offsetCombo,
              fontJustificationCombo, borderStyleCombo;
    private JTextField offsetValueField;
    private VertexColorButton fgColorButton, bgColorButton;
    I18NPlug iPlug;
    String [] fontSizes;
    String [] fontStyles;
    String [] fontJust;
    String [] positions;
    String [] offsets;
    String [] borders;


    public TextLabelPanel(ButtonGroup group, I18NPlug iPlug)
    {
        this.group = group;
        this.iPlug = iPlug;
        setNames();
        init();
    }

    public void setNames()
    {
        fontSizes = new String [] {"7","8","9","10","12","14", "16","18", "20", "24", "32", "48", "72"};
        fontStyles = new String [] {iPlug.get("VertexSymbols.Dialog.Plain"),
                                iPlug.get("VertexSymbols.Dialog.Bold"),
                                iPlug.get("VertexSymbols.Dialog.PlainItalic"),
                                iPlug.get("VertexSymbols.Dialog.BoldItalic")};
        fontJust = new String [] {iPlug.get("VertexSymbols.Dialog.Left"),
                              iPlug.get("VertexSymbols.Dialog.Centre"),
                              iPlug.get("VertexSymbols.Dialog.Right")};
        positions = new String [] {iPlug.get("VertexSymbols.Dialog.Centre"),
                               iPlug.get("VertexSymbols.Dialog.North"),
                               iPlug.get("VertexSymbols.Dialog.NorthEast"),
                               iPlug.get("VertexSymbols.Dialog.East"),
                               iPlug.get("VertexSymbols.Dialog.SouthEast"),
                               iPlug.get("VertexSymbols.Dialog.South"),
                               iPlug.get("VertexSymbols.Dialog.SouthWest"),
                               iPlug.get("VertexSymbols.Dialog.West"),
                               iPlug.get("VertexSymbols.Dialog.NorthWest")};
        offsets = new String [] {iPlug.get("VertexSymbols.Dialog.None"),
                             iPlug.get("VertexSymbols.Dialog.Auto"),
                             iPlug.get("VertexSymbols.Dialog.Value")};
        borders = new String [] {iPlug.get("VertexSymbols.Dialog.None"),
                             iPlug.get("VertexSymbols.Dialog.Boxed"),
                             iPlug.get("VertexSymbols.Dialog.Callout")};
    }
    
    public void init()
    {
       GridBagDesigner gb = new GridBagDesigner(this);

        enabledCB = new JCheckBox(iPlug.get("VertexSymbols.Dialog.Enabled"));
        gb.setPosition(0,0);
        gb.setInsets(10,10,0,0);
        gb.addComponent(enabledCB);

        attributeLabel = new JLabel(iPlug.get("VertexSymbols.Dialog.Attribute"));
        gb.setPosition(1,0);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(attributeLabel);

        attributeCombo = new JComboBox();
        gb.setPosition(2,0);
        gb.setInsets(10,10,0,10);
        gb.setSpan(2,1);
        gb.setFill(GridBagConstraints.HORIZONTAL);
        gb.addComponent(attributeCombo);

        fontLabel = new JLabel(iPlug.get("VertexSymbols.Dialog.Font"));
        gb.setPosition(0,1);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(fontLabel);

        fontNameCombo = new JComboBox(GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames());
        gb.setPosition(1,1);
        gb.setInsets(10,10,0,10);
        gb.setSpan(2,1);
        gb.addComponent(fontNameCombo);

        fontSizeCombo = new JComboBox(fontSizes);
        gb.setPosition(3,1);
        gb.setInsets(10,10,0,10);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(fontSizeCombo);

        fontStyleCombo = new JComboBox(fontStyles);
        gb.setPosition(1,2);
        gb.setInsets(10,10,0,10);
        gb.addComponent(fontStyleCombo);

        fontJustificationCombo = new JComboBox(fontJust);
        gb.setPosition(2,2);
        gb.setInsets(10,10,0,10);
        gb.addComponent(fontJustificationCombo);

        positionLabel = new JLabel(iPlug.get("VertexSymbols.Dialog.Position"));
        gb.setPosition(0,3);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(positionLabel);

        positionCombo = new JComboBox(positions);
        gb.setPosition(1,3);
        gb.setInsets(10,10,0,0);
        gb.addComponent(positionCombo);

        offsetCombo = new JComboBox(offsets);
        gb.setPosition(2,3);
        gb.setInsets(10,10,0,0);
        gb.addComponent(offsetCombo);

        offsetValueField = new JTextField(5);
        gb.setPosition(3,3);
        gb.setInsets(10,10,0,10);
        gb.addComponent(offsetValueField);

        backgroundLabel = new JLabel(iPlug.get("VertexSymbols.Dialog.BackColor"));
        gb.setPosition(0,4);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(backgroundLabel);

        bgColorButton = new VertexColorButton(Color.WHITE);
        gb.setPosition(1,4);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(bgColorButton);

        foregroundLabel = new JLabel(iPlug.get("VertexSymbols.Dialog.ForeColor"));
        gb.setPosition(2,4);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(foregroundLabel);

        fgColorButton = new VertexColorButton(Color.BLACK);
        gb.setPosition(3,4);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(fgColorButton);

        borderLabel = new JLabel(iPlug.get("VertexSymbols.Dialog.Border"));
        gb.setPosition(0,5);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(borderLabel);

        borderStyleCombo = new JComboBox(borders);
        gb.setPosition(1,5);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(borderStyleCombo);

        fillCB = new JCheckBox(iPlug.get("VertexSymbols.Dialog.FillBackground"));
        gb.setPosition(2,5);
        gb.setInsets(10,10,0,0);
        gb.addComponent(fillCB);

        scopeLabel = new JLabel(iPlug.get("VertexSymbols.Dialog.Scope"));
        gb.setPosition(0,6);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(scopeLabel);

        pointsCB = new JCheckBox(iPlug.get("VertexSymbols.Dialog.Points"));
        gb.setPosition(1,6);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(pointsCB);

        linesCB = new JCheckBox(iPlug.get("VertexSymbols.Dialog.Lines"));
        gb.setPosition(2,6);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(linesCB);

        polysCB = new JCheckBox(iPlug.get("VertexSymbols.Dialog.Polygons"));
        gb.setPosition(3,6);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.WEST);
        gb.addComponent(polysCB);

        setValues();
    }

    private void setValues()
    {
        //System.out.println("singlelayer:"+VertexParams.singleLayer+"  selectedLayer="+VertexParams.selectedLayer);
        if(VertexParams.singleLayer && VertexParams.selectedLayer != null)
        {
            attributeCombo.removeAllItems();
            attributeCombo.addItem("$FID");
            attributeCombo.addItem("$POINT");
            FeatureSchema featureSchema = VertexParams.selectedLayer.getFeatureCollectionWrapper().getFeatureSchema();
            for (int i=0; i < featureSchema.getAttributeCount(); i++)
            {
                AttributeType type = featureSchema.getAttributeType(i);
                if( type == AttributeType.DOUBLE || type == AttributeType.INTEGER ||
                        type == AttributeType.STRING || type == AttributeType.DATE || type == AttributeType.GEOMETRY)
                {
                    String name = featureSchema.getAttributeName(i);
                    attributeCombo.addItem(name);

                }
            }
            if(VertexParams.attName != null && !VertexParams.attTextName.equals(""))
                              attributeCombo.setSelectedItem(VertexParams.attTextName);
        }

        enabledCB.setSelected(VertexParams.textEnabled);
        fontNameCombo.setSelectedItem(VertexParams.textFontName);
        fontSizeCombo.setSelectedItem(String.valueOf(VertexParams.textFontSize));
        fontStyleCombo.setSelectedItem(VertexParams.textStyle);
        fontJustificationCombo.setSelectedIndex(VertexParams.textJustification);
        positionCombo.setSelectedIndex(VertexParams.textPosition);
        offsetCombo.setSelectedIndex(VertexParams.textOffset);
        offsetValueField.setText(String.valueOf(VertexParams.textOffsetValue));
        bgColorButton.setBackground(VertexParams.textBackgroundColor);
        fgColorButton.setBackground(VertexParams.textForegroundColor);
        borderStyleCombo.setSelectedIndex(VertexParams.textBorder);
        fillCB.setSelected(VertexParams.textFill);

        pointsCB.setSelected((VertexParams.textScope & 1) > 0);
        linesCB.setSelected((VertexParams.textScope & 2) > 0);
        polysCB.setSelected((VertexParams.textScope & 4) > 0);


    }

    public boolean getValues()
    {
        VertexParams.attTextName = (String) attributeCombo.getSelectedItem();
        VertexParams.textEnabled = enabledCB.isSelected();
        VertexParams.textFontName = (String) fontNameCombo.getSelectedItem();
        VertexParams.textFontSize = Integer.parseInt((String) fontSizeCombo.getSelectedItem());
        VertexParams.textStyle = getFontStyle((String)fontStyleCombo.getSelectedItem());
        VertexParams.textJustification =  fontJustificationCombo.getSelectedIndex();
        VertexParams.textPosition = positionCombo.getSelectedIndex();
        VertexParams.textOffset = offsetCombo.getSelectedIndex();
        VertexParams.textOffsetValue =Integer.parseInt(offsetValueField.getText());
        VertexParams.textBackgroundColor = bgColorButton.getBackground();
        VertexParams.textForegroundColor = fgColorButton.getBackground();
        VertexParams.textBorder =  borderStyleCombo.getSelectedIndex();
        VertexParams.textFill = fillCB.isSelected();

        int scope = 0;
        if(pointsCB.isSelected()) scope = scope + 1;
        if(linesCB.isSelected()) scope = scope + 2;
        if(polysCB.isSelected()) scope = scope + 4;
        VertexParams.textScope = scope;
        return true;

    }

    private int getFontStyle(String style)
    {
        if(style.equals(iPlug.get("VertexSymbols.Dialog.Plain"))) return Font.PLAIN;
        if(style.equals(iPlug.get("VertexSymbols.Dialog.Bold"))) return Font.BOLD;
        if(style.equals(iPlug.get("VertexSymbols.Dialog.PlainItalic"))) return Font.PLAIN+Font.ITALIC;
        if(style.equals(iPlug.get("VertexSymbols.Dialog.BoldItalic"))) return Font.BOLD+Font.ITALIC;
        return Font.PLAIN;
    }

}
