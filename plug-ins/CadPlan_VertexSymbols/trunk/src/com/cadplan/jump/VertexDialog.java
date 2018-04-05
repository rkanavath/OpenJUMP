package com.cadplan.jump;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.ToolTipManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.cadplan.designer.GridBagDesigner;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.workbench.ui.OKCancelApplyPanel;

/**
 * User: geoff Date: 16/06/2007 Time: 13:24:24 Copyright 2005 Geoffrey G Roy.
 */
public class VertexDialog extends JDialog implements ActionListener,
        ItemListener, ChangeListener {
    JLabel sizeLabel, orienLabel, baseScaleLabel;
    public JTextField sizeField, orienField, baseScaleField;
    public JCheckBox showLineCB, showFillCB, dottedCB, sizeByScaleCB;
    JButton cancelButton, aboutButton, acceptButton;
    JTabbedPane tabbedPane;
    VectorPanel vectorPanel;
    ImagePanel imagePanel;
    WKTPanel wktPanel;
    TextLabelPanel labelPanel;
    ButtonGroup group, rotateGroup;
    JRadioButton absValueRB, byAttributeRB;
    JComboBox attributeCB;
    JScrollPane scrollPane, scrollPane2;
    I18NPlug iPlug;
    public boolean cancelled = false;
    OKCancelApplyPanel okPanel = new OKCancelApplyPanel();

    public VertexDialog(I18NPlug iPlug) {
        super(new JFrame(), iPlug.get("VertexSymbols.Dialog"), true);
        this.iPlug = iPlug;
        // [Giuseppe Aruta 2018-04-05] workaround to close in a proper way the
        // dialog
        // <Bug: when clicking on "x" button the style was applied anyhow>
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent windowEvent) {
                cancelled = true;
                dispose();
            }
        });
        setIconImage(getIcon().getImage());
        init();
    }

    public ImageIcon getIcon() {
        return new ImageIcon(getClass().getResource("vsicon.gif"));
    }

    public void init() {
        JPopupMenu.setDefaultLightWeightPopupEnabled(false);
        final ToolTipManager ttm = ToolTipManager.sharedInstance();
        ttm.setLightWeightPopupEnabled(false);

        final GridBagDesigner gb = new GridBagDesigner(this);
        group = new ButtonGroup();

        tabbedPane = new JTabbedPane(JTabbedPane.TOP);
        // tabbedPane.setPreferredSize(new Dimension(400,300));
        tabbedPane.addChangeListener(this);

        vectorPanel = new VectorPanel(group, iPlug);
        vectorPanel.setBackground(Color.WHITE);
        tabbedPane
                .addTab(iPlug.get("VertexSymbols.Dialog.Vector"), vectorPanel);

        imagePanel = new ImagePanel(group);
        imagePanel.setBackground(Color.WHITE);
        scrollPane = new JScrollPane(imagePanel);
        scrollPane.setPreferredSize(new Dimension(400, 300));
        tabbedPane.addTab(iPlug.get("VertexSymbols.Dialog.Image"), scrollPane);

        wktPanel = new WKTPanel(group);
        wktPanel.setBackground(Color.WHITE);
        scrollPane2 = new JScrollPane(wktPanel);
        scrollPane2.setPreferredSize(new Dimension(400, 300));
        tabbedPane.addTab(iPlug.get("VertexSymbols.Dialog.WKTshapes"),
                scrollPane2);

        labelPanel = new TextLabelPanel(group, iPlug);
        // labelPanel.setBackground(Color.WHITE);

        tabbedPane.addTab(iPlug.get("VertexSymbols.Dialog.Labels"), labelPanel);

        gb.setPosition(0, 0);
        gb.setSpan(4, 1);
        gb.setWeight(1.0, 1.0);
        gb.setFill(GridBagConstraints.BOTH);
        gb.addComponent(tabbedPane);

        final JPanel optionPanel = new JPanel();
        final GridBagDesigner gbb = new GridBagDesigner(optionPanel);

        sizeLabel = new JLabel(iPlug.get("VertexSymbols.Dialog.Size"));
        gbb.setPosition(0, 0);
        gbb.setInsets(0, 10, 5, 5);
        gbb.setAnchor(GridBagConstraints.EAST);
        gbb.addComponent(sizeLabel);

        sizeField = new JTextField(8);
        gbb.setPosition(1, 0);
        gbb.setInsets(0, 10, 5, 5);
        gbb.addComponent(sizeField);

        showLineCB = new JCheckBox(iPlug.get("VertexSymbols.Dialog.ShowLine"));
        gbb.setPosition(2, 0);
        gbb.setInsets(0, 10, 5, 5);
        gbb.setAnchor(GridBagConstraints.WEST);
        gbb.addComponent(showLineCB);

        showFillCB = new JCheckBox(iPlug.get("VertexSymbols.Dialog.ShowFill"));
        gbb.setPosition(3, 0);
        gbb.setInsets(0, 10, 5, 5);
        gbb.setAnchor(GridBagConstraints.WEST);
        gbb.addComponent(showFillCB);

        orienLabel = new JLabel(iPlug.get("VertexSymbols.Dialog.Orientation"));
        gbb.setPosition(0, 1);
        gbb.setInsets(0, 10, 5, 5);
        gbb.setAnchor(GridBagConstraints.EAST);
        gbb.addComponent(orienLabel);

        absValueRB = new JRadioButton(iPlug.get("VertexSymbols.Dialog.ByValue"));
        gbb.setPosition(1, 1);
        gbb.setInsets(0, 5, 5, 0);
        gbb.addComponent(absValueRB);

        orienField = new JTextField(8);
        gbb.setPosition(2, 1);
        gbb.setInsets(0, 10, 5, 5);
        gbb.addComponent(orienField);

        dottedCB = new JCheckBox(iPlug.get("VertexSymbols.Dialog.Dotted"));
        gbb.setPosition(4, 0);
        gbb.setInsets(0, 10, 5, 5);
        gbb.addComponent(dottedCB);

        byAttributeRB = new JRadioButton(
                iPlug.get("VertexSymbols.Dialog.ByAttribute"));
        gbb.setPosition(3, 1);
        gbb.setInsets(0, 5, 5, 5);
        gbb.addComponent(byAttributeRB);

        rotateGroup = new ButtonGroup();
        rotateGroup.add(absValueRB);
        rotateGroup.add(byAttributeRB);
        absValueRB.setSelected(true);

        attributeCB = new JComboBox();
        gbb.setPosition(4, 1);
        gbb.setInsets(0, 5, 5, 5);
        gbb.addComponent(attributeCB);

        baseScaleLabel = new JLabel(iPlug.get("VertexSymbols.Dialog.BaseScale"));
        gbb.setPosition(0, 2);
        gbb.setInsets(5, 10, 0, 0);
        gbb.addComponent(baseScaleLabel);

        baseScaleField = new JTextField(8);
        gbb.setPosition(1, 2);
        gbb.setInsets(5, 10, 0, 0);
        gbb.addComponent(baseScaleField);
        baseScaleField.setText(String.valueOf(VertexParams.baseScale));

        sizeByScaleCB = new JCheckBox(
                iPlug.get("VertexSymbols.Dialog.SizeByScale"));
        gbb.setPosition(2, 2);
        gbb.setInsets(5, 10, 0, 0);
        gbb.addComponent(sizeByScaleCB);
        sizeByScaleCB.setSelected(VertexParams.sizeByScale);

        aboutButton = new JButton(iPlug.get("VertexSymbols.Dialog.About"));
        gbb.setPosition(4, 2);
        gbb.setInsets(5, 10, 0, 0);
        gbb.addComponent(aboutButton);
        aboutButton.addActionListener(this);

        gb.setPosition(0, 1);
        gb.setFill(GridBagConstraints.HORIZONTAL);
        gb.setInsets(10, 0, 0, 0);
        gb.setSpan(4, 1);
        gb.addComponent(optionPanel);

        gb.setPosition(0, 3);
        gb.setSpan(4, 1);
        gb.setWeight(1.0, 1.0);
        gb.setFill(GridBagConstraints.BOTH);
        gb.addComponent(okPanel);

        okPanel.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (okPanel.wasOKPressed()) {

                    final boolean OK = getValues();
                    if (!OK) {
                        return;
                    }
                    dispose();

                    return;

                } else {
                    cancelled = true;
                    dispose();
                }
            }
        });

        // cancelButton = new JButton(iPlug.get("VertexSymbols.Dialog.Cancel"));
        // gb.setPosition(0, 3);
        // gb.setInsets(10, 10, 10, 10);
        // gb.addComponent(cancelButton);
        // cancelButton.addActionListener(this);

        // aboutButton = new JButton(iPlug.get("VertexSymbols.Dialog.About"));
        // gb.setPosition(1, 3);
        // gb.setInsets(10, 10, 10, 10);
        // gb.addComponent(aboutButton);
        // aboutButton.addActionListener(this);

        // acceptButton = new JButton(iPlug.get("VertexSymbols.Dialog.Accept"));
        // gb.setPosition(3, 3);
        // gb.setAnchor(GridBagConstraints.EAST);
        // gb.setInsets(10, 10, 10, 10);
        // gb.addComponent(acceptButton);
        // acceptButton.addActionListener(this);

        setValues();
        pack();
        setLocation(100, 100);
        setVisible(true);

    }

    public void setValues() {
        sizeField.setText(String.valueOf(VertexParams.size));
        orienField.setText(String.valueOf(VertexParams.orientation));
        showLineCB.setSelected(VertexParams.showLine);
        showFillCB.setSelected(VertexParams.showFill);
        dottedCB.setSelected(VertexParams.dotted);
        sizeByScaleCB.setSelected(VertexParams.sizeByScale);

        // int n = vectorPanel.symbolPanel.getIndex(VertexParams.sides,
        // VertexParams.type);
        // if( n >= 0 && VertexParams.selectedImage < 0)
        // {
        // vectorPanel.symbolPanel.vertexRB[n].setSelected(true);
        // }
        // else if(VertexParams.selectedImage >= 0)
        // {
        // imagePanel.imageRB[VertexParams.selectedImage].setSelected(true);
        // }
        // else if(VertexParams.selectedWKT >= 0)
        // {
        // wktPanel.imageRB[VertexParams.selectedWKT].setSelected(true);
        // }

        final int n = vectorPanel.symbolPanel.getTypeIndex(
                VertexParams.symbolNumber, VertexParams.symbolType);
        // System.out.println("Setting Values: n="+n+"  symbolName="+VertexParams.symbolName+"  type="+VertexParams.symbolType+
        // " number="+VertexParams.symbolNumber);
        // System.out.println("selectedImage="+VertexParams.selectedImage+"  selectedWKT="+VertexParams.selectedWKT);
        if (n >= 0) {
            vectorPanel.symbolPanel.vertexRB[n].setSelected(true);
        } else if (VertexParams.selectedImage >= 0) {
            imagePanel.imageRB[VertexParams.selectedImage].setSelected(true);
        } else if (VertexParams.selectedWKT >= 0) {
            wktPanel.imageRB[VertexParams.selectedWKT].setSelected(true);
        }

        // absValueRB.setEnabled(VertexParams.singleLayer);
        byAttributeRB.setEnabled(VertexParams.singleLayer);
        boolean enableAttributeCB = false;
        if (VertexParams.singleLayer && VertexParams.selectedLayer != null) {
            attributeCB.removeAllItems();

            final FeatureSchema featureSchema = VertexParams.selectedLayer
                    .getFeatureCollectionWrapper().getFeatureSchema();
            for (int i = 0; i < featureSchema.getAttributeCount(); i++) {
                final AttributeType type = featureSchema.getAttributeType(i);
                if (type == AttributeType.DOUBLE
                        || type == AttributeType.INTEGER) {
                    final String name = featureSchema.getAttributeName(i);
                    attributeCB.addItem(name);
                    enableAttributeCB = true;
                }
            }
            if (VertexParams.attName != null
                    && !VertexParams.attName.equals("")) {
                attributeCB.setSelectedItem(VertexParams.attName);
            }
        }
        if (VertexParams.byValue) {
            absValueRB.setSelected(true);
        } else {
            byAttributeRB.setSelected(true);
        }
        if (!VertexParams.singleLayer) {
            absValueRB.setSelected(true);
        }
        byAttributeRB.setEnabled(enableAttributeCB);
        attributeCB.setEnabled(enableAttributeCB);
        if (!VertexParams.singleLayer || VertexParams.selectedLayer == null) {
            tabbedPane.remove(labelPanel);
        }
    }

    public boolean getValues() {
        try {
            VertexParams.size = Integer.parseInt(sizeField.getText());
        } catch (final NumberFormatException ex) {
            JOptionPane.showMessageDialog(this,
                    iPlug.get("VertexSymbols.Dialog.Warning1"), "Warning...",
                    JOptionPane.WARNING_MESSAGE);
            return false;
        }
        try {
            VertexParams.orientation = Double.parseDouble(orienField.getText());
        } catch (final NumberFormatException ex) {
            JOptionPane.showMessageDialog(this,
                    iPlug.get("VertexSymbols.Dialog.Warning2"), "Warning...",
                    JOptionPane.WARNING_MESSAGE);
            return false;
        }
        try {
            VertexParams.baseScale = Double.parseDouble(baseScaleField
                    .getText());
        } catch (final NumberFormatException ex) {
            JOptionPane.showMessageDialog(this,
                    iPlug.get("VertexSymbols.Dialog.Warning3"), "Warning...",
                    JOptionPane.WARNING_MESSAGE);
            return false;
        }

        VertexParams.showLine = showLineCB.isSelected();
        VertexParams.showFill = showFillCB.isSelected();
        VertexParams.dotted = dottedCB.isSelected();
        VertexParams.sizeByScale = sizeByScaleCB.isSelected();
        VertexParams.type = getVertexType();
        VertexParams.sides = getSides(VertexParams.type);
        // System.out.println("getSides="+VertexParams.sides); //***
        VertexParams.selectedImage = imagePanel.getSelectedImage();
        VertexParams.selectedWKT = wktPanel.getSelectedImage();
        // System.out.println("***SelectedImage="+VertexParams.selectedImage+"  selectedWKT="+VertexParams.selectedWKT);
        VertexParams.attName = (String) attributeCB.getSelectedItem();
        if (VertexParams.attName == null) {
            VertexParams.attName = "";
        }
        VertexParams.byValue = absValueRB.isSelected();
        VertexParams.symbolName = getSymbolName();
        VertexParams.symbolType = getSymbolType();
        // System.out.println("getSymbolName="+VertexParams.symbolName+"  Type="+VertexParams.symbolType);
        // //***
        if (VertexParams.symbolName == null) {
            JOptionPane.showMessageDialog(this,
                    iPlug.get("VertexSymbols.Dialog.Warning4"), "Warning...",
                    JOptionPane.WARNING_MESSAGE);
            return false;
        }
        final Boolean OK = labelPanel.getValues();
        return true && OK;
    }

    public int getSides(int type) {
        for (int i = 0; i < vectorPanel.symbolPanel.vertexRB.length; i++) {
            if (i < 7 && type == VertexParams.POLYGON
                    && vectorPanel.symbolPanel.vertexRB[i].isSelected()) {
                return vectorPanel.symbolPanel.sides[i];
            }
            if (i >= 7 && type == VertexParams.STAR
                    && vectorPanel.symbolPanel.vertexRB[i].isSelected()) {
                return vectorPanel.symbolPanel.sides[i];
            }
            if (i >= 14 && type == VertexParams.ANYSHAPE
                    && vectorPanel.symbolPanel.vertexRB[i].isSelected()) {
                return vectorPanel.symbolPanel.sides[i];
            }
        }

        return vectorPanel.symbolPanel.sides[2];
    }

    public int getVertexType() {
        // for (int i=0; i < vectorPanel.symbolPanel.vertexRB.length; i++)
        // {
        // if(vectorPanel.symbolPanel.vertexRB[i].isSelected())
        // {
        // if (i < 7) return VertexParams.POLYGON;
        // else if(i < 14) return VertexParams.STAR;
        // else return VertexParams.ANYSHAPE;
        // }
        // }
        // for(int i=0; i < imagePanel.imageRB.length; i++)
        // {
        // if(imagePanel.imageRB[i].isSelected())
        // {
        // return VertexParams.IMAGE;
        // }
        // }
        // for(int i=0; i < wktPanel.imageRB.length; i++)
        // {
        // if(wktPanel.imageRB[i].isSelected())
        // {
        // return VertexParams.WKT;
        // }
        // }
        // return VertexParams.IMAGE;
        return VertexParams.EXTERNAL;
    }

    public String getSymbolName() {
        for (int i = 0; i < vectorPanel.symbolPanel.vertexRB.length; i++) {
            if (vectorPanel.symbolPanel.vertexRB[i].isSelected()) {
                if (i < 7) {
                    return "@poly"
                            + String.valueOf(vectorPanel.symbolPanel.sides[i]);
                } else if (i < 14) {
                    return "@star"
                            + String.valueOf(vectorPanel.symbolPanel.sides[i]);
                } else {
                    return "@any"
                            + String.valueOf(vectorPanel.symbolPanel.sides[i]);
                }
            }
        }
        for (int i = 0; i < imagePanel.imageRB.length; i++) {
            if (imagePanel.imageRB[i].isSelected()) {
                return VertexParams.imageNames[i];
            }
        }
        for (int i = 0; i < wktPanel.imageRB.length; i++) {
            if (wktPanel.imageRB[i].isSelected()) {
                return VertexParams.wktNames[i];
            }
        }
        return null;
    }

    public int getSymbolType() {
        for (int i = 0; i < vectorPanel.symbolPanel.vertexRB.length; i++) {
            if (vectorPanel.symbolPanel.vertexRB[i].isSelected()) {
                if (i < 7) {
                    return ExternalSymbolsRenderer.POLYS;
                } else if (i < 14) {
                    return ExternalSymbolsRenderer.STARS;
                } else {
                    return ExternalSymbolsRenderer.ANYS;
                }
            }
        }
        for (final JRadioButton element : imagePanel.imageRB) {
            if (element.isSelected()) {
                return ExternalSymbolsRenderer.IMAGES;
            }
        }
        for (final JRadioButton element : wktPanel.imageRB) {
            if (element.isSelected()) {
                return ExternalSymbolsRenderer.WKTS;
            }
        }
        return -1;
    }

    @Override
    public void stateChanged(ChangeEvent ev) {

    }

    @Override
    public void itemStateChanged(ItemEvent ev) {

    }

    @Override
    public void actionPerformed(ActionEvent ev) {
        if (ev.getSource() == cancelButton) {
            cancelled = true;
            dispose();
        }

        if (ev.getSource() == aboutButton) {
            JOptionPane.showMessageDialog(this,
                    "Vertex Symbols plugin for OpenJUMP\n" + "Vers: "
                            + VertexParams.version + "\n"
                            + "(c) 2007-12, Geoffrey G. Roy\n"
                            + "Cadplan: http://www.cadplan.com.au", "About...",
                    JOptionPane.INFORMATION_MESSAGE);
        }

        if (ev.getSource() == acceptButton) {
            final boolean OK = getValues();
            if (!OK) {
                return;
            }
            dispose();
        }

    }
}
