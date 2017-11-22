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
import java.awt.geom.Rectangle2D;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;
import java.awt.font.LineBreakMeasurer;
import java.awt.font.TextAttribute;
import java.text.AttributedString;
import java.util.StringTokenizer;

/**
 * User: geoff
 * Date: 12/01/2007
 * Time: 13:20:24
 * Copyright 2005 Geoffrey G Roy.
 */
public class FurnitureNote extends Furniture
{
     public String text;
     public Font font;
     public int justify;
     public boolean border = false;
     public int width = 0;
     public Color color1 = Color.BLACK;  //border color


    public FurnitureNote(String text, Font font, int justify, int width, Rectangle location, boolean show)
    {
        this.text = text;
        this.font = font;
        this.location = location;
        this.show = show;
        this.justify = justify;
        this.width = width;
        layerNumber = 30;
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

    public void setBorder(boolean border)
    {
    	this.border = border;
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

        FontRenderContext frc = g2.getFontRenderContext();
        TextLayout layout = new TextLayout(text, font, frc);
        int lineSpace = (int) (layout.getAscent()+layout.getDescent());
        Rectangle2D bounds = layout.getBounds();
        location.width = (int) bounds.getWidth();
        location.height = (int) bounds.getHeight();

       
       
        
        g.setFont(font);
        //layout.draw(g2, (float)location.x, (float)location.y + (float) location.height);
        StringTokenizer st = new StringTokenizer(text,"\n");

        int maxWidth = 0;
        int numLines = 0;
        y = y + lineSpace;
        
        //find size parameters
        while (st.hasMoreTokens())
        {
            String line = st.nextToken();
            layout = new TextLayout(line, font, frc);
            bounds = layout.getBounds();
            //g.drawString(line,x, y);
            //y = y + lineSpace;
            if(bounds.getWidth() > maxWidth) maxWidth = (int) bounds.getWidth();
            if(justify == 3) maxWidth = width;
            //numLines++;
            
            
            //String line = st.nextToken();
            //layout = new TextLayout(line, font, frc);
            if(justify < 3)
            {
	            bounds = layout.getBounds();
	            int xp = x;
	            if(justify == 1) xp = x + (int)((maxWidth - bounds.getWidth())/2.0);
	            if(justify == 2) xp = x + (int)((maxWidth - bounds.getWidth()));
	
	            //g.drawString(line,xp,y);
	            //y = y + lineSpace;
	            numLines++;
            }
            else
            {
            	AttributedString attrLine = new AttributedString(line);
            	attrLine.addAttribute(TextAttribute.FONT, font, 0, line.length());
            	
            	LineBreakMeasurer measurer =  new LineBreakMeasurer(attrLine.getIterator(), frc);
            	float fwidth = (float) width;
            	maxWidth = width;
            	//numLines--;
            	while(measurer.getPosition() < line.length())
            	{
            		TextLayout layout2= measurer.nextLayout(fwidth);
            		if(measurer.getPosition() < line.length())
            		{
            			TextLayout layout3 = layout2.getJustifiedLayout(fwidth);
            			//layout3.draw(g2, x, y);
            		}
            		else
            		{
            			//layout2.draw(g2, x, y);
            		}
            		numLines++;
            		
            		//y = y + lineSpace;
            	}
            }
            
                
            
            
        }
        //paint background if required
        int dWidth = maxWidth;
        if(width > 0) dWidth = width;
        if(setBackColor & bcolor != null)
        {
        	g.setColor(bcolor);
        	g.fillRect(location.x-5,location.y,dWidth+10, numLines*lineSpace +lineSpace/2);
        }
        if(border)
        {
        	g.setColor(color1);
        	g.drawRect(location.x-5,location.y,dWidth+10, numLines*lineSpace +lineSpace/2);
        }
        
        //draw text on top of background
        g.setColor(color);
        st = new StringTokenizer(text,"\n");
        
	        while (st.hasMoreTokens())
	        {
	            String line = st.nextToken();
	            layout = new TextLayout(line, font, frc);
	            if(justify < 3 || width <= 0)
	            {
		            bounds = layout.getBounds();
		            int xp = x;
		            if(justify == 1) xp = x + (int)((dWidth - bounds.getWidth())/2.0);
		            if(justify == 2) xp = x + (int)((dWidth - bounds.getWidth()));
		
		            g.drawString(line,xp,y);
		            y = y + lineSpace;
	            }
	            
	            else
	            {
	            	AttributedString attrLine = new AttributedString(line);
	            	attrLine.addAttribute(TextAttribute.FONT, font, 0, line.length());
	            	
	            	LineBreakMeasurer measurer =  new LineBreakMeasurer(attrLine.getIterator(), frc);
	            	float fwidth = (float) width;
	            	maxWidth = width;
	            	//numLines--;
	            	while(measurer.getPosition() < line.length())
	            	{
	            		TextLayout layout2= measurer.nextLayout(fwidth);
	            		if(measurer.getPosition() < line.length())
	            		{
	            			TextLayout layout3 = layout2.getJustifiedLayout(fwidth);
	            			layout3.draw(g2, x, y);
	            		}
	            		else
	            		{
	            			layout2.draw(g2, x, y);
	            		}
	            		//numLines++;
	            		
	            		y = y + lineSpace;
	            	}
	            }
	            
	        }
        
        //System.out.println("text: width="+width+"\n"+text+"<");
        if(scale > 0.0)
        {
            g2.setColor(boundsColor);
            g2.setStroke(boundsStroke);
            g2.drawRect(location.x, location.y, maxWidth,  numLines*lineSpace);
            g2.setStroke(normalStroke);
            location.width = maxWidth;
            location.height = numLines*lineSpace;
            
            
            g2.scale(1.0/scale, 1.0/scale);
        }


    }
}
