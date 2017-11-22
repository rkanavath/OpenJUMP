package com.cadplan.jump;

import com.vividsolutions.jump.workbench.ui.renderer.style.VertexStyle;

import javax.swing.*;
import java.awt.*;

/**
 * User: geoff
 * Date: 17/06/2007
 * Time: 10:53:09
 * Copyright 2005 Geoffrey G Roy.
 */
public class ImageVertexPanel extends JPanel
{
    VertexStyle symbol;

    public ImageVertexPanel(String imageName)
    {
        symbol = new ImageVertexStyle();
        ((ImageVertexStyle)symbol).setName(imageName);
        double scale = ((ImageVertexStyle)symbol).getScale();
        int size = ((ImageVertexStyle)symbol).image.getWidth(null);
        ((ImageVertexStyle)symbol).setSize((int)((double)size/scale));
    }

    public void paint(Graphics g)
    {
        ((ImageVertexStyle)symbol).render((Graphics2D) g);
    }
}
