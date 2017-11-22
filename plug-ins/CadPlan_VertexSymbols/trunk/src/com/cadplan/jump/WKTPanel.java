package com.cadplan.jump;

import com.cadplan.designer.GridBagDesigner;

import javax.swing.*;
import java.awt.*;

/**
 * User: geoff
 * Date: 8/07/2007
 * Time: 10:41:53
 * Copyright 2005 Geoffrey G Roy.
 */
public class WKTPanel extends JPanel
{
    ButtonGroup group;
    JRadioButton [] imageRB;

    int numImages;
    public WKTPanel(ButtonGroup group)
    {
        this.group = group;
        init();
    }

    public void init()
    {
        if(VertexParams.wktShapes == null) return;

        numImages = VertexParams.wktShapes.length;
        imageRB = new JRadioButton[numImages];

        GridBagDesigner gb = new GridBagDesigner(this);
        int row, col;

        for (int i=0; i < numImages; i++)
        {
            WKTVertexPanel wktVertexPanel = new WKTVertexPanel( VertexParams.wktNames[i]);
            wktVertexPanel.setBackground(Color.WHITE);
            double scale = ((WKTVertexStyle)wktVertexPanel.symbol).getScale();
            if(VertexParams.wktShapes[i] != null)
            {
               wktVertexPanel.setPreferredSize(new Dimension((int)((VertexParams.wktShapes[i].extent)/scale)+4,(int)(VertexParams.wktShapes[i].extent/scale)+4));
            }
            else
            {
               wktVertexPanel.setPreferredSize(new Dimension(40,20));
            }
            row = i % 7;
            col = (i/7)*2;

            gb.setPosition(col,row);
            gb.setInsets(5,10,5,0);

            gb.addComponent(wktVertexPanel);

            String name  = VertexParams.wktNames[i];
            int k = name.lastIndexOf(".");
            if(k > 0) name = name.substring(0,k);

            imageRB[i] = new JRadioButton(name);
            imageRB[i].setBackground(Color.WHITE);
            group.add(imageRB[i]);
            gb.setPosition(col+1,row);
            gb.setInsets(5,0,5,10);
            gb.setAnchor(GridBagConstraints.WEST);
            gb.setWeight(1.0,1.0);
            gb.setFill(GridBagConstraints.BOTH);
            gb.addComponent(imageRB[i]);

        }
    }

    public int getSelectedImage()
    {
        for (int i=0; i < numImages; i++)
        {
            if(imageRB[i].isSelected()) return i;
        }
        return -1;
    }

}
