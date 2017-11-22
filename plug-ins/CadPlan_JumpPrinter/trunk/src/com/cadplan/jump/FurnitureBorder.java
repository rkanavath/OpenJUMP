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

import java.awt.*;

/**
 * User: geoff
 * Date: 11/01/2007
 * Time: 07:50:37
 * Copyright 2005 Geoffrey G Roy.
 */
public class FurnitureBorder extends Furniture
{
    double  thickness = 1.0;
    boolean fixed = false;
    Color color1 = Color.white;
    boolean showFill = false;
    int dragSize = 10;

    public FurnitureBorder(double thickness, boolean fixed, boolean show )
    {
        this.show = show;
        this.thickness = thickness;
        this.fixed = fixed;
        layerNumber = 10;
    }

    /**
     * test for a point indide the drag square (bottom-rigth)
     * @param x
     * @param y
     * @param scale
     * @return
     */
    public boolean insideBR(int x, int y, double scale, double globalScale)
    {
        boolean inside = false;
        if(show)
        {
            inside = true;
            if(x < (location.x+location.width)*scale-dragSize/globalScale) inside = false;
            if(x > (location.x+location.width)*scale) inside = false;
            if(y < (location.y+location.height)*scale-dragSize/globalScale) inside = false;
            if(y > (location.y+location.height)*scale) inside = false;
            //JOptionPane.showMessageDialog(null,"x="+x+" y="+y+"  scale="+scale+"  location:"+location+"  inside="+inside);
        }
        return inside;
    }
    /**
     * test for a point indide the drag square (top-left)
     * @param x
     * @param y
     * @param scale
     * @return
     */
    public boolean insideTL(int x, int y, double scale, double globalScale)
    {
        boolean inside = false;
        if(show)
        {
            inside = true;
            if(x < (location.x)*scale) inside = false;
            if(x > (location.x)*scale+dragSize/globalScale) inside = false;
            if(y < (location.y)*scale) inside = false;
            if(y > (location.y)*scale+dragSize/globalScale) inside = false;
            //JOptionPane.showMessageDialog(null,"x="+x+" y="+y+"  scale="+scale+"  location:"+location+"  inside="+inside);
        }
        return inside;
    }
    /**
     * sets the border dimensions, depends of if border is fixed and whether a forced change is required.
     * @param w
     * @param h
     * @param force
     */
    public void setBorder(int x, int y, int w, int h, boolean force)
    {
        if( x <= 0 ) x = 1;
        if( y <= 0 ) y = 1;
          if(!fixed && !force || force && fixed)
          {
              location.x = x;
              location.y = y;
              location.width = w;
              location.height = h;

          }
    }

    public void paint(Graphics g, double scale, double globalScale)
    {
        Graphics2D g2 = (Graphics2D) g;
        
        
        g.setColor(color);
        Stroke stroke = new BasicStroke((float)thickness);
        double scalet = 1.0;
        if(scale > 0.0)
        {
            g2.scale(scale,scale);
            scalet = scale;
        }
        g2.setStroke(stroke);        
        int x = location.x + (int) Math.floor((thickness/2.0/scalet));
        int y = location.y + (int) Math.floor((thickness/2.0/scalet));
        int w = location.width - (int) (thickness/scalet);
        int h = location.height -(int) (thickness/scalet);

        if( w <= 0) g.drawLine(x,y,x,y+h);
        else if(h <= 0) 
        {
        	g.drawLine(x,y,x+w,y);
        }
        else 
        {
        	if(showFill)
        	{
        		g.setColor(color1);
        		g.fillRect(x,y,w,h);
        	}
        	g.setColor(color);
        	g.drawRect(x,y,w,h);
        }
        g2.setStroke(new BasicStroke());

        if(fixed && scale > 0.0)
        {
            g.setColor(Color.BLUE);
            int dx = (int) (dragSize/(scale*globalScale));
            g.drawRect(location.x+location.width-dx,location.y+location.height-dx, dx,dx);
            g.drawRect(location.x,location.y, dx,dx);
        }
        if(scale > 0.0)
        {
            g2.scale(1.0/scale, 1.0/scale);
        }




    }
   
}
