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

import javax.swing.*;
import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;

/**
 * User: geoff
 * Date: 10/01/2007
 * Time: 09:09:32
 * Copyright 2005 Geoffrey G Roy.
 */
public class FurnitureTitle extends Furniture
{
    public String text;
    public Font font;

    

    public FurnitureTitle(String text, Font font,Rectangle location, boolean show)
    {
        this.text = text;
        this.font = font;
        this.location = location;
        this.show = show;
        layerNumber = 20;

    }

    public void setFont(Font font)
    {
       this.font = font;
    }

    public void setColor(Color color)
    {
        this.color = color;
    }

    public void setLocation(Rectangle location)
    {
        this.location = location;
    }

    public void setShow(boolean show)
    {
        this.show = show;
    }

   

    public void paint(Graphics g, double scale)
    {
        Graphics2D g2 = (Graphics2D) g;
        int x = location.x;
        int y = location.y;
        g.setColor(color);
        if(scale > 0.0)
        {
            g2.scale(scale, scale);
        }
        g2.setFont(font);
        //g2.drawString(text,x,y+location.height);
        if(text == null || text.length() == 0) return;
        FontRenderContext frc = g2.getFontRenderContext();
        TextLayout layout = new TextLayout(text, font, frc);
        Rectangle2D bounds = layout.getBounds();
        location.width = (int) bounds.getWidth();
        location.height = (int) bounds.getHeight();

        g.setFont(font);
        //layout.draw(g2, (float)location.x, (float)location.y + (float) location.height);
        g.drawString(text,location.x, location.y+location.height);


        if(scale > 0.0)
        {
            g2.setColor(boundsColor);
            g2.setStroke(boundsStroke);
            g2.drawRect(location.x, location.y, location.width,  location.height);
            g2.setStroke(normalStroke);
            g2.scale(1.0/scale, 1.0/scale);
        }


    }
}
