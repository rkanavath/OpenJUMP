package com.cadplan.jump;

import com.cadplan.designer.GridBagDesigner;

import javax.swing.*;
import java.awt.*;

/**
 * User: geoff
 * Date: 17/06/2007
 * Time: 09:05:21
 * Copyright 2005 Geoffrey G Roy.
 */
public class ImagePanel extends JPanel
{
    ButtonGroup group;
    JRadioButton [] imageRB;

    int numImages;
    public ImagePanel(ButtonGroup group)
    {
        this.group = group;
        init();
    }

    public void init()
    {
        if(VertexParams.images == null) return;
        
        numImages = VertexParams.images.length;
        imageRB = new JRadioButton[numImages];

        GridBagDesigner gb = new GridBagDesigner(this);
        int row, col;

        for (int i=0; i < numImages; i++)
        {
            ImageVertexPanel imageVertexPanel = new ImageVertexPanel( VertexParams.imageNames[i]);
            imageVertexPanel.setBackground(Color.WHITE);
            double scale = ((ImageVertexStyle)imageVertexPanel.symbol).getScale();
            if(VertexParams.images[i] != null)
            {
               imageVertexPanel.setPreferredSize(new Dimension((int)(VertexParams.images[i].getWidth(null)/scale)+4,(int)(VertexParams.images[i].getHeight(null)/scale)+4));
            }
            else
            {
               imageVertexPanel.setPreferredSize(new Dimension(40,20)); 
            }
            row = i % 7;
            col = (i/7)*2;

            gb.setPosition(col,row);
            gb.setInsets(5,10,5,0);

            gb.addComponent(imageVertexPanel);

            String name  = VertexParams.imageNames[i];
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
