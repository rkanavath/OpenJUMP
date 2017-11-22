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

/**
 * User: geoff
 * Date: 28/04/2007
 * Time: 11:31:54
 * Copyright 2007 Geoffrey G Roy.
 */
public class ChartValues
{
    public int ID;
    public String name;
    public double x;
    public double y;
    public double [] values;
    public String [] names;
    public String [] types;

    public ChartValues(int ID, String name, double x, double y, double [] values, String [] names, String [] types)
    {
        this.ID = ID;
        this.name = name;
        this.x = x;
        this.y = y;
        this.values = values;
        this.names = names;
        this.types = types;
    }

    public String toString()
    {
        String s = "Item:"+ID+ ":"+name+" ["+x+","+y+"]";
        for (int i=0; i < values.length; i++)
        {
            s = s + "["+types[i]+":"+names[i]+"] "+ values[i];
            if(i < values.length-1) s = s+",";
        }
        return s;
    }
}
