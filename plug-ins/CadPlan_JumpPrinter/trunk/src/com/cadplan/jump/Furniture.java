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

/**
 * User: geoff
 * Date: 10/01/2007
 * Time: 09:06:44
 * Copyright 2005 Geoffrey G Roy.
 */
public class Furniture extends JComponent
{
    protected Rectangle location = new Rectangle(0,0,0,0);
    protected boolean show = false;
    protected Color color = Color.BLACK;
    protected Color bcolor = null;
    protected boolean setBackColor = false;
    public static Stroke boundsStroke = new BasicStroke(0.5f,BasicStroke.CAP_SQUARE,BasicStroke.JOIN_BEVEL,10.0f,new float[]{1.0f, 3.0f},0.0f);
    public static Stroke normalStroke = new BasicStroke();
    public static Color boundsColor = Color.BLUE;
    protected int layerNumber = 0;
    protected boolean validItem = true;
    

    public boolean inside(int x, int y, double scale)
    {
        boolean inside = false;
        if(show)
        {
            inside = true;
            if(x < location.x*scale) inside = false;
            if(x > (location.x+location.width)*scale) inside = false;
            if(y < location.y*scale) inside = false;
            if(y > (location.y+location.height)*scale) inside = false;
            //JOptionPane.showMessageDialog(null,"x="+x+" y="+y+"  scale="+scale+"  location:"+location+"  inside="+inside);
        }
        return inside;
    }
    
}
