package com.cadplan.jump;

import com.cadplan.jump.VertexParams;
import com.cadplan.jump.WKTVertexStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.VertexStyle;

import javax.swing.*;

import java.awt.*;

/**
 * User: geoff
 * Date: 8/07/2007
 * Time: 10:49:25
 * Copyright 2005 Geoffrey G Roy.
 */
public class WKTVertexPanel extends JPanel
{
    VertexStyle symbol;

    public WKTVertexPanel(String imageName)
    {
        symbol = new WKTVertexStyle();
        ((WKTVertexStyle)symbol).setName(imageName);
        double scale = ((WKTVertexStyle)symbol).getScale();
        int size = ((WKTVertexStyle)symbol).wktShape.extent;
        ((WKTVertexStyle)symbol).setSize((int)((double)size/scale));
        ((WKTVertexStyle) symbol).setColors(VertexParams.selectedLayer
                .getBasicStyle().getLineColor(), VertexParams.selectedLayer
                .getBasicStyle().getFillColor());
    }

    public void paint(Graphics g)
    {
        ((WKTVertexStyle)symbol).render((Graphics2D) g);
    }
}
