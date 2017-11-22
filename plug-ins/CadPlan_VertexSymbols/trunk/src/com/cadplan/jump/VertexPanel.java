package com.cadplan.jump;

import com.vividsolutions.jump.workbench.ui.renderer.style.VertexStyle;

import javax.swing.*;
import java.awt.*;

/**
 * User: geoff
 * Date: 16/06/2007
 * Time: 14:01:11
 * Copyright 2005 Geoffrey G Roy.
 */
public class VertexPanel  extends JPanel
{
    private int sides;
    private int type;
    VertexStyle symbol;

    public VertexPanel(int sides, int type)
    {
        this.sides = sides;
        this.type = type;
        setPreferredSize(new Dimension(30,30));
        init();
    }

    public void init()
    {
         if(type == VertexParams.POLYGON)
         {
             symbol = new PolygonVertexStyle();
             ((PolygonVertexStyle)symbol).setNumSides(sides);
         }
         else if(type == VertexParams.STAR)
         {
             symbol = new StarVertexStyle();
             ((StarVertexStyle)symbol).setNumSides(sides);
         }
        else
         {
              symbol = new AnyShapeVertexStyle();
             ((AnyShapeVertexStyle)symbol).setType(sides);
         }

         symbol.setSize(20);
    }

    public void paint(Graphics g)
    {
        if(type == VertexParams.POLYGON)
        {
             ((PolygonVertexStyle)symbol).render((Graphics2D) g);
        }
        else if(type == VertexParams.STAR)
        {
            ((StarVertexStyle)symbol).render((Graphics2D) g);
        }
        else
        {
            ((AnyShapeVertexStyle)symbol).render((Graphics2D) g);
        }
    }
}
