
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
 */package com.cadplan.jump;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.AffineTransform;

/**
 * User: geoff
 * Date: 11/01/2007
 * Time: 08:46:37
 * Copyright 2005 Geoffrey G Roy.
 */
public class FurnitureNorth extends Furniture
{
    int type = 0;
    double rotation = 0.0;
    BasicStroke stroke = new BasicStroke(1.0f,BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND);
    double sizeFactor = 1.0;
    public double sf = sizeFactor;

    public FurnitureNorth(int type, Rectangle location, boolean show)
    {
        this.type = type;
        this.show = show;
        this.location = location;
        layerNumber = 50;
    }

    public void drawNorth(Graphics g, int x, int y, int type)
    {

         Graphics2D g2 = (Graphics2D) g;
         g2.setStroke(stroke);
         if(type <= 1)
        {

            int dia = (int) (50*sf);
            g.setColor(Color.BLACK);
            g.drawOval(x, y,dia, dia);
            g.setColor(color);
            Polygon poly = new Polygon();
            poly.addPoint(x+dia/2,y);
            poly.addPoint(x+dia-2, y+dia/2+(int) (7*sf));
            poly.addPoint(x+2,y+dia/2+(int)(7*sf));
            g.fillPolygon(poly);
            g.setFont(new Font("SansSerif",Font.PLAIN,(int)(12*sf)));
            g.drawString("North",x+(int)(10*sf), y+dia/2+(int)(17*sf));
            location.width = dia;
            location.height = dia;


        }

        if(type == 2)
        {
            int lx = x;
            int ly = y;

             int h = (int)(90*sf);
             int t = (int) (36*sf);
             int w = (int) (54*sf);
             int s = (int) (5*sf);
             Polygon p0, p1, p2, p3, p4, p5, p6, p7;

//             g.setColor(Color.BLACK);
//             Font font = new Font("Serif",Font.BOLD, 48);
//             g.setFont(font);
//             g.drawString("N", lx+w/2-20, ly+t+20);
            
             g.setColor(color);      // top right face
             p0 = new Polygon();
             p0.addPoint(lx+w/2, ly);
             p0.addPoint(lx+w/2+s, ly + t-s);
             p0.addPoint(lx+w/2,ly+t);
             g.drawPolygon(p0);

            p1 = new Polygon();
            p1.addPoint(lx+w,ly+t);
            p1.addPoint(lx+w/2,ly+t);
            p1.addPoint(lx+w/2+s,ly+t-s);
            g.fillPolygon(p1);
            g.drawPolygon(p1);
            
            p2 = new Polygon();
            p2.addPoint(lx+w,ly+t);
            p2.addPoint(lx+w/2+s,ly+t+s);
            p2.addPoint(lx+w/2, ly+t);
            g.drawPolygon(p2);

            p3 = new Polygon();
            p3.addPoint(lx+w/2,ly+t);
            p3.addPoint(lx+w/2+s,ly+t+s);
            p3.addPoint(lx+w/2, ly+h);
            g.fillPolygon(p3);
            g.drawPolygon(p3);

            p4 = new Polygon();
            p4.addPoint(lx+w/2,ly+t);
            p4.addPoint(lx+w/2,ly+h);
            p4.addPoint(lx+w/2-s, ly+t);
            g.drawPolygon(p4);

            p5 = new Polygon();
            p5.addPoint(lx+w/2,ly+t);
            p5.addPoint(lx+w/2-s,ly+t+s);
            p5.addPoint(lx, ly+t);
            g.fillPolygon(p5);
            g.drawPolygon(p5);

            p6 = new Polygon();
            p6.addPoint(lx+w/2,ly+t);
            p6.addPoint(lx,ly+t);
            p6.addPoint(lx+w/2-5,ly+t-s);
            g.drawPolygon(p6);

            p7 = new Polygon();
            p7.addPoint(lx+w/2, ly+t);
            p7.addPoint(lx+w/2-s, ly+t-s);
            p7.addPoint(lx+w/2, ly);
            g.fillPolygon(p7);
            g.drawPolygon(p7);

            double wt = h*Math.sin(Math.toRadians(rotation)) + w*Math.sin(Math.toRadians(90.0-rotation));
            double ht = h*Math.cos(Math.toRadians(rotation)) + w*Math.cos(Math.toRadians(90.0-rotation));

            location.width = w;
            location.height = h;


        }

        if(type == 3)
        {
            int h = (int) (80*sf);
            int w = (int) (40*sf);
            int t = (int) (30*sf);
            int hh = (int) (20*sf);
            int hw = (int) (16*sf);

            g.setColor(color);

            g.drawLine(x+w/2,y,x+w/2,y+h);
            g.drawLine(x,y+t,x+w,y+t);
             Polygon p = new Polygon();
            p.addPoint(x+w/2,y);
            p.addPoint(x+w/2+hw/2,y+hh);
            p.addPoint(x+w/2-hw/2,y+hh);
            g.fillPolygon(p);

            g.setFont(new Font("Serif",Font.BOLD, (int) (20*sf)));
            g.setColor(Color.BLACK);
            g.drawString("N",x+w/2-(int)(7*sf),y+t+(int) (20*sf));

            location.width = w;
            location.height = h;
        }

        if(type == 4)
        {
            int s1 = (int) (48*sf);
            int s11 = (int) (8*sf);
            int s2 = (int) (28*sf);
            int s22 = (int) (14*sf);
            Polygon p = new Polygon();

            Color darkColor = color;
            int red = color.getRed()+ 3*(255-color.getRed())/4;
            int green = color.getGreen() + 3*(255-color.getGreen())/4;
            int blue = color.getBlue() + 3*(255-color.getBlue())/4;
            Color lightColor = new Color(red, green, blue);

            g.setColor(lightColor);
            p = new Polygon();
            p.addPoint(x+s1-s2,y+s1-s2);
            p.addPoint(x+s1,y+s1-s22);
            p.addPoint(x+s1,y+s1);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(darkColor);
            p = new Polygon();
            p.addPoint(x+s1,y+s1-s22);
            p.addPoint(x+s1+s2,y+s1-s2);
            p.addPoint(x+s1,y+s1);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(lightColor);
            p = new Polygon();
            p.addPoint(x+s1+s2,y+s1-s2);
            p.addPoint(x+s1+s22,y+s1);
            p.addPoint(x+s1,y+s1);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(darkColor);
            p = new Polygon();
            p.addPoint(x+s1+s22,y+s1);
            p.addPoint(x+s1+s2,y+s1+s2);
            p.addPoint(x+s1,y+s1);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(lightColor);
            p = new Polygon();
            p.addPoint(x+s1+s2,y+s1+s2);
            p.addPoint(x+s1,y+s1+s22);
            p.addPoint(x+s1,y+s1);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(darkColor);
            p = new Polygon();
            p.addPoint(x+s1,y+s1);
            p.addPoint(x+s1,y+s1+s22);
            p.addPoint(x+s1-s2,y+s1+s2);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(lightColor);
            p = new Polygon();
            p.addPoint(x+s1,y+s1);
            p.addPoint(x+s1-s2,y+s1+s2);
            p.addPoint(x+s1-s22,y+s1);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(darkColor);
            p = new Polygon();
            p.addPoint(x+s1,y+s1);
            p.addPoint(x+s1-s22,y+s1);
            p.addPoint(x+s1-s2,y+s1-s2);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(lightColor);
            p = new Polygon();
            p.addPoint(x+s1,y);
            p.addPoint(x+s1+s11,y+s1-s11);
            p.addPoint(x+s1,y+s1);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(darkColor);
            p = new Polygon();
            p.addPoint(x+s1+s1,y+s1);
            p.addPoint(x+s1,y+s1);
            p.addPoint(x+s1+s11,y+s1-s11);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(lightColor);
            p = new Polygon();
            p.addPoint(x+s1+s1,y+s1);
            p.addPoint(x+s1+s11,y+s1+s11);
            p.addPoint(x+s1,y+s1);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(darkColor);
            p = new Polygon();
            p.addPoint(x+s1,y+s1+s1);
            p.addPoint(x+s1,y+s1);
            p.addPoint(x+s1+s11,y+s1+s11);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(lightColor);
            p = new Polygon();
            p.addPoint(x+s1,y+s1+s1);
            p.addPoint(x+s1-s11,y+s1+s11);
            p.addPoint(x+s1,y+s1);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(darkColor);
            p = new Polygon();
            p.addPoint(x,y+s1);
            p.addPoint(x+s1,y+s1);
            p.addPoint(x+s1-s11,y+s1+s11);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(lightColor);
            p = new Polygon();
            p.addPoint(x,y+s1);
            p.addPoint(x+s1-s11,y+s1-s11);
            p.addPoint(x+s1,y+s1);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(darkColor);
            p = new Polygon();
            p.addPoint(x+s1,y);
            p.addPoint(x+s1,y+s1);
            p.addPoint(x+s1-s11,y+s1-s11);
            g.fillPolygon(p);
            g.setColor(Color.BLACK);
            g.drawPolygon(p);

            g.setColor(Color.WHITE);
            int d = (int) (16*sf);
            g.fillOval(x+s1-d/2,y+s1-s2-d/2,d,d);
            g.setColor(Color.BLACK);
            g.drawOval(x+s1-d/2,y+s1-s2-d/2,d,d);
            Font font = new Font("SansSerif", Font.BOLD, (int) (12*sf));
            g.setFont(font);
            g.drawString("N",x+s1-(int)(4*sf), y+s1-s2+ (int) (5*sf));

            location.width = 2*s1;
            location.height = 2*s1;

        }
        if(type == 5)
        {
            
            g.setColor(color);
            Polygon poly = new Polygon();
            poly.addPoint(x+(int)(30*sf),y);
            poly.addPoint(x+(int)(60*sf), y+(int) (40*sf));
            poly.addPoint(x+(int)(40*sf),y+(int) (40*sf));
            poly.addPoint(x+(int)(40*sf),y+(int) (80*sf));
            poly.addPoint(x+(int)(20*sf),y+(int) (80*sf));
            poly.addPoint(x+(int)(20*sf),y+(int) (40*sf));
            poly.addPoint(x,y+(int) (40*sf));
            poly.addPoint(x+(int)(30*sf),y);
            g.setColor(color);
            g.fillPolygon(poly);
            g.setColor(Color.BLACK);
            g.drawPolygon(poly);
            
            g.setFont(new Font("SansSerif",Font.BOLD,(int)(18*sf)));
            if(color.equals(Color.BLACK)) g.setColor(Color.WHITE);
            //System.out.println("color="+color+"  current="+g.getColor());
            Polygon poly2 = new Polygon();
            poly2.addPoint(x+(int)Math.round(23*sf),y+(int)Math.round(40*sf));
            poly2.addPoint(x+(int)Math.round(27*sf),y+(int)Math.round(40*sf));
            poly2.addPoint(x+(int)Math.round(33*sf),y+(int)Math.round(52*sf));
            poly2.addPoint(x+(int)Math.round(33*sf),y+(int)Math.round(40*sf));
            poly2.addPoint(x+(int)Math.round(37*sf),y+(int)Math.round(40*sf));
            poly2.addPoint(x+(int)Math.round(37*sf),y+(int)Math.round(60*sf));
            poly2.addPoint(x+(int)Math.round(33*sf),y+(int)Math.round(60*sf));
            poly2.addPoint(x+(int)Math.round(27*sf),y+(int)Math.round(48*sf));
            poly2.addPoint(x+(int)Math.round(27*sf),y+(int)Math.round(60*sf));
            poly2.addPoint(x+(int)Math.round(23*sf),y+(int)Math.round(60*sf));
            poly2.addPoint(x+(int)Math.round(23*sf),y+(int)Math.round(40*sf));
            
            g.fillPolygon(poly2);
            //g.drawString("N",x+(int)(20*sf+3*sf), y+(int)(40*sf)+15);
            location.width = (int)(60*sf);
            location.height = (int)(80*sf);


        }
        if(type == 6)
        {



            Polygon poly = new Polygon();
            poly.addPoint(x+(int)(30*sf),y);
            poly.addPoint(x+(int)(60*sf), y+(int) (30*sf));
            poly.addPoint(x+(int)(60*sf), y+(int) (42*sf));
            poly.addPoint(x+(int)(34*sf), y+(int) (16*sf));
            poly.addPoint(x+(int)(34*sf), y+(int) (70*sf));
            poly.addPoint(x+(int)(26*sf), y+(int) (70*sf));
            poly.addPoint(x+(int)(26*sf), y+(int) (16*sf));
            poly.addPoint(x, y+(int) (42*sf));
            poly.addPoint(x, y+(int) (30*sf));
            poly.addPoint(x+(int)(30*sf), y);
            
            g.setColor(color);
            g.fillPolygon(poly);
            g.setColor(Color.BLACK);
            g.drawPolygon(poly);;
            
            location.width = (int)(60*sf);
            location.height = (int)(70*sf);


        }


        g2.setStroke(new BasicStroke());

    }

    public void paint(Graphics g, double scale)
    {
        Graphics2D g2 = (Graphics2D) g;
        AffineTransform currentTransform = g2.getTransform();
        int xo = (int) (location.x+location.width/2);
        int yo = (int) (location.y +location.height/2);
        if(scale > 0.0)     // for screen display
        {
            g2.scale(scale,scale);
            sf = sizeFactor;
        }
        else if (scale < 0.0)  // for printer
        {
            sf = sizeFactor;
        }
        else sf = 1.0;
        
        g2.rotate(Math.toRadians(rotation), xo, yo);
        
         drawNorth(g,(int) (location.x), (int)(location.y), type);

        if(scale > 0.0)
        {
            g.setColor(boundsColor);
            g2.setStroke(boundsStroke);
//            g.drawOval(location.x, location.y, (int) (location.height/scale),(int) (location.height/scale) );
            g.drawRect(location.x, location.y, location.width, location.height);
            g2.setStroke(normalStroke);
            //g2.scale(1.0/scale, 1.0/scale);
        }
        g2.setTransform(currentTransform);
//        location.x = (int) (location.x/sizeFactor);
//        location.y = (int)(location.y/sizeFactor);
//        location.width = (int) (location.width*sizeFactor);
//        location.height = (int) (location.height*sizeFactor);
    }

}
