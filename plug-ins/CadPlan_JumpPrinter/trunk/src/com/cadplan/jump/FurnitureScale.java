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
import java.text.DecimalFormat;
import java.util.StringTokenizer;

import javax.swing.JOptionPane;

/**
 * User: geoff
 * Date: 10/01/2007
 * Time: 15:42:09
 * Copyright 2005 Geoffrey G Roy.
 */
public class FurnitureScale extends Furniture
{
    boolean debug = false;
    double range;
    double interval;
    double scale;
    boolean autoScale = true;
    double px = 72.0/25.4;
    double lendiv = 100.0;
    int numdiv = 10;
    double rangediv = 1.0;
    double sizeFactor = 1.0;
    I18NPlug iPlug;
    int numberScale = 1;
    String rangeSpec = "";
    String units = "";
    Color color1 = Color.BLACK;
    Color color2 = Color.WHITE;
    Font font = new Font("SansSerif",Font.PLAIN, 12);
    boolean showRatio = true;
    boolean showUnits = true;
    String [] scaleValues;
    double [] scaleIntervals;
    boolean scaleLeader = false;
    int leaderInterval;
    
    
    public FurnitureScale(double scale, double range, double interval, Rectangle location, boolean show)
    {
        this.scale = scale;
        this.range = range;
        this.interval = interval;
        this.location = location;
        this.show = show;
        color = Color.BLACK;
        numberScale = 1;
        layerNumber = 40;
        

        
    }
    
    public void setIPlug(I18NPlug iPlug)
    {
    	this.iPlug = iPlug;
    	//System.out.println("Setting iPlug: "+iPlug.get("JumpPrinter.Furniture.Scale.Scale"));
    }

    /**
     * recomputes size of scaleItem
     * @param displayScale
     */
    public void resizeScale(double displayScale)
    {
        int [] baseDiv = {10,10,8,12,8,10,12,7,8,9};
               double lenp = 150;

               int h = 5;
                int x = location.x;
                int y = location.y;

                if(!autoScale)
                {
                    //numdiv = (int) Math.round(range/interval);
                    //lendiv = (range/(double) numdiv)*1000.0*px/scale;
                    //rangediv = range/(double) numdiv;
                    decodeRange();
                    location.width = (int) scaleIntervals[scaleIntervals.length-1];
                }
                else
                {
                    double len = lenp*scale/(1000.0*px);
                    double exp = Math.floor(Math.log10(len));
                    double base = Math.round(len / Math.pow(10.0,exp));
                    double range = base * Math.pow(10.0,exp);
                    //int numdiv = 10;
                    int nbase = (int) base;
                    if(nbase > 9) nbase = 9;
                    numdiv = baseDiv[nbase];

            //        if(debug) sb.append("Drawing scale:  scale="+scale+"  px="+px+"\n");
             //       if(debug) sb.append("        lenp="+lenp+" len="+len+" exp="+exp+" base="+base+" range="+range+"\n");
                     lendiv = (range/(double) numdiv)*1000.0*px/scale;
             //       if(debug) sb.append("  lendiv="+lendiv+" numdiv="+numdiv+"\n");

                    while (lendiv < 20 && (numdiv % 2) == 0)
                    {
                        lendiv = 2.0*lendiv;
                        numdiv = numdiv/2;
                    }

                     rangediv = range/(double) numdiv;
                     location.width = (int)Math.round((lendiv*numdiv));
                }
                
                //System.out.println("Scale: lendiv="+lendiv+" numdiv="+numdiv+"  width="+location.width);
                location.height = 25;

    }
    
    private void decodeRange()
    {
    	String ranges = rangeSpec;
    	StringTokenizer st = new StringTokenizer(ranges,",");
    	int n = st.countTokens();
    	leaderInterval =5;
    	scaleValues = new String[n];
    	scaleIntervals = new double[n];
    	int i=0;
    	while(st.hasMoreTokens())
    	{
    		scaleValues[i] = st.nextToken();
    		i++;
    	}
    	scaleLeader = false;
    	double leaderValue = 0.0;
    	
    	try
		{
	    	for(int j=0; j < n; j++)
	    	{
    		
	    		double val = 0.0; 
	    		if( j == 0 && scaleValues[j].startsWith("-"))
	    		{
	    			StringTokenizer st2 = new StringTokenizer(scaleValues[j],":");
	    			scaleValues[j] = st2.nextToken();
	    			if(st2.hasMoreTokens()) leaderInterval = Integer.parseInt(st2.nextToken());
	    			else leaderInterval = 5;
	    			val = Double.parseDouble(scaleValues[j]);
	    			scaleLeader = true;
	    			leaderValue = -val*1000.0*px/scale;
	    			scaleIntervals[j] = 0.0;
	    			scaleValues[j] = scaleValues[j].substring(1);
	    		} 
	    		if(!scaleLeader || j > 0)
	    		{
	    			val = Double.parseDouble(scaleValues[j]);
		    		if(scaleLeader)
		    		{
		    			scaleIntervals[j] = leaderValue + val*1000.0*px/scale;
		    		}
		    		else
		    		{
		    			scaleIntervals[j] = val*1000.0*px/scale;
		    		}
	    		}
    		
    	
	    	}
		}
		catch(NumberFormatException ex)
		{
            JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Scale.Message3"),
                    iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);

		}
    	//System.out.println("scaleLeader:"+scaleLeader+"  value="+leaderInterval);
    	//for(int k=0; k < scaleValues.length; k++) System.out.print(scaleValues[k]+",");
    	//System.out.println();
    	//for(int k=0; k < scaleIntervals.length; k++) System.out.print(scaleIntervals[k]+",");
    	//System.out.println();
    }
   /**
     * draws the scale symbol
     * @param g the graphics context
     * @param displayScale the drawing scale on the preview screen
     */
    private void drawScale(Graphics g, double displayScale)
    {
        boolean debug = false;
        double sf = sizeFactor;
        Graphics2D g2 = (Graphics2D) g;
//       int [] baseDiv = {10,10,8,12,8,10,12,7,8,9};
//       double lenp = 150;
//       double lendiv = 100.0;
//       int numdiv = 10;
 //      double rangediv = 1.0;
        if(!validItem) return;
        
       int h =(int) (5*sf);
       int lastWidth = 0;
       g2.setStroke(normalStroke);
       resizeScale(displayScale);
       //g.setFont(new Font("SansSerif",Font.PLAIN,(int) (7*sf)));
       g.setFont(font);
       FontMetrics fm = g.getFontMetrics(font);
       int fh = fm.getHeight();
        int x = location.x;
        int y = location.y;
        location.height=h+2*fh+5;
//
//        if(!autoScale)
//        {
//            numdiv = (int) Math.round(range/interval);
//            lendiv = (range/(double) numdiv)*1000.0*px/scale;
//            rangediv = range/(double) numdiv;
//        }
//        else
//        {
//            double len = lenp*scale/(1000.0*px);
//            double exp = Math.floor(Math.log10(len));
//            double base = Math.round(len / Math.pow(10.0,exp));
//            double range = base * Math.pow(10.0,exp);
//            //int numdiv = 10;
//            int nbase = (int) base;
//            if(nbase > 9) nbase = 9;
//            numdiv = baseDiv[nbase];
//
//    //        if(debug) sb.append("Drawing scale:  scale="+scale+"  px="+px+"\n");
//     //       if(debug) sb.append("        lenp="+lenp+" len="+len+" exp="+exp+" base="+base+" range="+range+"\n");
//             lendiv = (range/(double) numdiv)*1000.0*px/scale;
//     //       if(debug) sb.append("  lendiv="+lendiv+" numdiv="+numdiv+"\n");
//
//            while (lendiv < 20 && (numdiv % 2) == 0)
//            {
//                lendiv = 2.0*lendiv;
//                numdiv = numdiv/2;
//            }
//
//             rangediv = range/(double) numdiv;
//        }
        if(displayScale > 0.0)
        {
             ((Graphics2D)g).scale(displayScale, displayScale);
        }
//        location.width = (int)Math.round((lendiv*numdiv));
//        System.out.println("Scale: lendiv="+lendiv+" numdiv="+numdiv+"  width="+location.width);
//        location.height = 25;


        g.setColor(color1);
        if(debug) System.out.println("Scale color: "+color1+"  numdiv:"+numdiv+" x="+x+"  y="+y);
        if(autoScale)
        {
	        for (int i=0; i < numdiv; i=i+2)
	        {
	            int xd = x + (int)Math.round((i*lendiv));
	            int xd2 = x + (int)Math.round(((i+1)*lendiv)) -xd; //(int)Math.round(lendiv);
	            g.fillRect(xd,y,xd2,h);
	        }
	        g.setColor(color2);
	        for (int i=1; i < numdiv; i=i+2)
	        {
	            int xd = x + (int)Math.round((i*lendiv));
	            int xd2 = x + (int)Math.round(((i+1)*lendiv)) -xd; //(int)Math.round(lendiv);
	            g.fillRect(xd,y,xd2,h);
	        }
	        g.setColor(color1);
	        g.drawLine(x,y,x+(int)Math.round((lendiv*numdiv)), y);
	        g.drawLine(x,y+h,x+ (int)Math.round( (lendiv*numdiv)), y+h);
	        g.setColor(color);
	        for (int i=0; i <= numdiv; i++)
	        {
	           int xd = x + (int)Math.round((i*lendiv));
	           g.setColor(color1);
	           g.drawLine(xd,y, xd,y+h+5);
	           double value = i*rangediv / (double)numberScale;
	           g.setColor(color);
	          
	           g.drawString(formatScale(value),xd,y+h+fh); //(int)(10*sf));
	           lastWidth = fm.stringWidth(formatScale(value));
	        }
        }
        else  // manual scale
        {
        	int oddStart = 0;
        	for(int i=0; i < scaleValues.length-1; i++)
        	{
        		int xd = x + (int) Math.round(scaleIntervals[0]);
        		int xd2 = 0;
        		if(scaleLeader && i == 0)
        		{
        			int nint = leaderInterval;
        			oddStart = (nint+1) % 2;
        			double interval = scaleIntervals[1]/(double)nint;
        			//if(interval < 4.0) 
        			//{
        			//	nint = 5;
        			//	interval = scaleIntervals[1]/nint;
        			//	oddStart = 0;
        			//}
        			//System.out.println("intervals="+interval);
        			for (int j=0; j < nint; j++)
        			{
        				int xdt = x + (int)Math.round((scaleIntervals[i] + (double) j*interval));
    		            int xd2t = (int) Math.round(interval); 
    		            //System.out.println("j="+j+"  xdt="+xdt+"  xd2t="+xd2t+"  y="+y+"  h="+h);
    		            if(j % 2 == 0) g.setColor(color1);
    		            else g.setColor(color2);
    		            g.fillRect(xdt,y,xd2t,h);
        			}
        		}
        		else
        		{
	        		xd = x + (int)Math.round((scaleIntervals[i]));
		            xd2 = x + (int)Math.round((scaleIntervals[i+1])) -xd; //(int)Math.round(lendiv);
		            
		            if(i % 2 == oddStart) g.setColor(color1);
		            else g.setColor(color2);
		            g.fillRect(xd,y,xd2,h);
	      		}
        		g.setColor(color1);
		        g.drawLine(xd,y, xd,y+h+5);
		        String values = scaleValues[i];
		        g.setColor(color);
		        double val = 0.0;
		        try
		        {
		        	 val = Double.parseDouble(values)/numberScale;
		        	
		        }
		        catch(NumberFormatException ex)
		        {
		        	//JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Scale.Message3"),
	                //        iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);
		        }
		       
		        g.drawString(formatScale(val),xd,y+h+fh); //(int)(10*sf));
		        //System.out.println("Text at:"+xd);
	            
        	}
        	g.setColor(color1);
        	int xd = (int) Math.round(scaleIntervals[scaleIntervals.length-1]);
        	g.drawRect(x, y, xd, h);
        	g.drawLine(x+xd,y, x+xd,y+h+5); //(int)(2*sf));
	        String values = scaleValues[scaleValues.length-1];
	        g.setColor(color);
	        double val = Double.parseDouble(values)/numberScale;
	        g.drawString(formatScale(val),x+xd,y+h+fh); //(int)(10*sf));
	        lastWidth = fm.stringWidth(formatScale(val));
	        //System.out.println("Text final at:"+(x+xd));
            
        }
        g.setColor(color);
        String scaleLabel;
        try
        {
            scaleLabel = iPlug.get("JumpPrinter.Furniture.Scale.Scale");
            //System.out.println("Scale label:"+scaleLabel);
        }
        catch(NullPointerException ex)
        {
        	//System.out.println("iPlug not found");
        	ex.printStackTrace();
        	scaleLabel = "Scale 1:";
        			
        }
        String text = "";
        if(showRatio) text = text + scaleLabel+String.valueOf(formatScale(scale))+"  ";
        //if(showUnits) text = text  + units;	
        	
        g.drawString(text,x,y+(int)(2*fh+5*sf)); //(int)(25*sf));
        
        int xu = x+location.width+lastWidth+5;
        if(showUnits) g.drawString(units,xu,y+fh+h);
        

        if(displayScale > 0.0)
        {
            g.setColor(boundsColor);

            g2.setStroke(boundsStroke);
            g.drawRect(x,y,location.width, location.height);
            g2.setStroke(normalStroke);
            ((Graphics2D)g).scale(1.0/displayScale, 1.0/displayScale);
        }
    }

    /**
     * format the labels on the scale symbol
     * @param v the label value
     * @return the formatted string
     */
    private String formatScale(double v)
    {
        DecimalFormat scaleFormat= null;
        if (v >= 10.0 || v == 0.0)  scaleFormat = new DecimalFormat("#,###");
        else if(v >= 1.0 && v < 10.0) scaleFormat = new DecimalFormat("##0.0");
        else if(v > 0.1 && v < 1.0) scaleFormat = new DecimalFormat("#0.0");
        else scaleFormat = new DecimalFormat("0.0E0");
        return scaleFormat.format(v);
    }
    public void paint(Graphics g, double scale)
    {
        drawScale(g,  scale);
    }
}
