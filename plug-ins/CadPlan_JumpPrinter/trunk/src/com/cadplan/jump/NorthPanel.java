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
 * Date: 11/01/2007
 * Time: 14:56:15
 * Copyright 2005 Geoffrey G Roy.
 */
public class NorthPanel extends JPanel
{
    FurnitureNorth north;
    public NorthPanel(FurnitureNorth north)
    {
        this.north = north;
    }

    public void paint(Graphics g)
    {
        north.sf = 1.0;
        int xsize = getSize().width;
        int ysize = getSize().height;
        g.setColor(Color.WHITE);
        g.fillRect(0,0,xsize,ysize);
        
        north.drawNorth(g,10,30,1);
        north.drawNorth(g,90,30,2);
        north.drawNorth(g,160,30,3);
        north.drawNorth(g,200,30,4);
        north.drawNorth(g,290,30,5);
        north.drawNorth(g,360,30,6);
    }
}
