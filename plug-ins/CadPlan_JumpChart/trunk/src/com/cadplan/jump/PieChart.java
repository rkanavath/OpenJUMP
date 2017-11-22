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

import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jump.feature.*;

import java.util.Vector;

/**
 * User: geoff
 * Date: 28/04/2007
 * Time: 13:20:18
 * Copyright 2007 Geoffrey G Roy.
 */
public class PieChart
{
    boolean debug = false;
    ChartAttribute [] attributes;
    ChartValues [] values;
    double screenScale;
    double startAngle;
    double maxValue;
    double maxSize;
    double xoffs, yoffs;
    double originX, originY;

    public PieChart(ChartAttribute [] attributes, ChartValues [] values, double screenScale, double startAngle)
    {
        this.attributes = attributes;
        this.values = values;
        this.screenScale = screenScale;
        this.startAngle = startAngle;
        xoffs = ChartParams.originX*screenScale;
        yoffs = ChartParams.originY*screenScale;
    }

    public void addFeatures(FeatureCollection dataset)
    {
         double sumValues, totalValues;
         double maxRad;
         int [] sizeOrderIndex = new int[values.length];

         if(ChartParams.autoScale)
             {
                 maxRad = 20*screenScale;
             }
             else
             {
                 maxRad = ChartParams.scaleValue*screenScale;
             }

         totalValues = 0.0;
         double [] itemSum = new double[values.length];
         double itemMax = Double.MIN_VALUE;
         for (int i=0; i < values.length; i++)
         {
        	// System.out.println("&&&& Preparing data for: "+values[i].name);
             double [] data = values[i].values;
             itemSum[i] = 0.0;
             for (int j=0; j < data.length; j++)
             {
                 totalValues = totalValues + data[j];
                 itemSum[i] = itemSum[i] + data[j];

             }
             if(itemSum[i] > itemMax) itemMax = itemSum[i];
         }
         
         // create sizeOrderIndex
         double [] tempValues = new double [values.length];
         for(int i=0; i < values.length;i++) tempValues[i] = itemSum[i];
         for(int i=0; i < values.length;i++) sizeOrderIndex[i] = i;
         for(int i=0; i < tempValues.length-1;i++)
         {
        	 int n = 0;
        	 int nMax = i;
        	 double sizeMax = tempValues[i];
        	 boolean swap = false;
        	 for (int k=i+1; k < tempValues.length; k++)
        	 {
        		 if(tempValues[k] > sizeMax)
        		 {
        			 sizeMax = tempValues[k];
        			 nMax = k;
        			 swap = true;
        			 
        		 }
        	 }
        	 if(swap)
        	 {
        		 double temp = tempValues[i];
        		 tempValues[i] = tempValues[nMax];
        		 tempValues[nMax] = temp;
        		 
        		 int tempIndex = sizeOrderIndex[i];
        		 sizeOrderIndex[i] = sizeOrderIndex[nMax];
        		 sizeOrderIndex[nMax] = tempIndex;       		 
        	 }
        	 //System.out.println("**** i="+i);
        	 //for(int k =0; k < tempValues.length; k++)
        	 //{
        	//	 System.out.println("    k="+k+"  order="+  sizeOrderIndex[k] + " >> " + tempValues[k]);
        	 //}
        	 
        	 
        	 
         }
         
         //System.out.println("Size order");
         //for(int i=0; i < values.length; i++) System.out.println("i="+i+" k="+sizeOrderIndex[i]+" >> "+tempValues[i]+"   itemsum[i]="+itemSum[i]);

         maxValue = itemMax;
         ChartParams.maxValue = maxValue;
         //System.out.println("maxValue="+maxValue+"  maxRad="+maxRad);
         maxSize = maxRad;
         for (int k=0; k < values.length; k++)
         {
        	 int i = sizeOrderIndex[k];
             sumValues = 0.0;
             double [] data = values[i].values;
             String nameValue = values[i].name;
             double x = values[i].x;
             double y = values[i].y;
             double rad = 50;
             double angle = startAngle; //0.0;

             for (int j=0; j < data.length; j++)
             {
                 sumValues = sumValues + data[j];
             }

             if(debug) System.out.println("totalvalues="+totalValues+  "  sumValues="+sumValues);
             if(ChartParams.uniformSize)
             {
                 rad = maxRad;
             }
             else
             {
            	 if(ChartParams.linearScale) 
            	 {
                     rad = maxRad*(itemSum[i])/(itemMax);
            	 }
            	 else
            	 {
            		 rad = maxRad*Math.sqrt((itemSum[i])/(itemMax));
            	 }
             }
             if(Double.isInfinite(rad)) rad = 30.0;
             //System.out.println("***Rad="+rad);
             for (int j=0; j < data.length; j++)
             {
                 double prop = data[j]/sumValues;    // fraction of circle for item
                 if(Double.isInfinite(prop)) prop = 1.0;
                 if(Double.isNaN(prop))  prop = 1.0;
                 String name = attributes[j].name;
                 Feature feature = buildFeature(j, x, y, rad, prop, angle, data[j], name);
                 //System.out.println("\nFeature:"+feature.toString());
                 dataset.add(feature);
                 if(!ChartParams.clockwise) angle = angle + 2.0*Math.PI*prop;
                 else angle = angle - 2.0*Math.PI*prop;
             }
             
             // -------------------------------------------
             //  build underlabel feature if required
             //--------------------------------------------
             //String nameValue = "Missing";
             if(ChartParams.showUnderLabels) 
             {
            	
            	// System.out.println("Adding feature value namevalue="+nameValue);
            	 Feature feature = buildLabelFeature(x, y-rad, nameValue);
            	 //System.out.println("Feature:"+feature.toString());
            	 dataset.add(feature);
             }
            
         }
         if(ChartParams.localScale && !ChartParams.uniformSize)
         {
 	        for(int i=0; i < values.length; i++)  
 	        {
 	        	double [] data = values[i].values;
 	            
 	            double x = values[i].x;
 	            double y = values[i].y;
 	            originX = x + xoffs;
 	            originY = y + yoffs;
 	           
 			        double interval = ChartParams.scaleInterval(maxValue);
 			        double scale = maxSize/maxValue;
 			        if(ChartParams.localScale) createLocalScale(dataset,originX, originY, interval, scale, itemSum[i]);
 	            
 	        }
         }
    }

    public double getMaxValue()
    {
        return maxValue;
    }
    public double getMaxSize()
    {
        return maxSize;
    }
    
    private Feature buildLabelFeature(double x, double y, String name)
    {
    	double xpos, ypos;
    	double fontHeight = 10.0 * screenScale;
    	//System.out.println("fontHeight = " + fontHeight);
    	xpos = x;
    	ypos = y - fontHeight;
    	
    	FeatureSchema featureSchema = new FeatureSchema();

        featureSchema.addAttribute("Geometry",AttributeType.GEOMETRY);
        featureSchema.addAttribute("Index", AttributeType.INTEGER);
        featureSchema.addAttribute("dValue", AttributeType.DOUBLE);
        featureSchema.addAttribute("iValue", AttributeType.INTEGER);
        featureSchema.addAttribute(ChartParams.underLabelName, AttributeType.STRING);
        
        //System.out.println("Label Feature at:"+x+","+y+"  name="+name+ " for attribute: " + ChartParams.underLabelName);
        Feature feature = new BasicFeature(featureSchema);
       
        	               
        Geometry geometry = new GeometryFactory().createPoint(new Coordinate(xpos,ypos));
        feature.setGeometry(geometry);
        feature.setAttribute("iValue",-1);
        feature.setAttribute("dValue",-1.0);
        feature.setAttribute(ChartParams.underLabelName,name);
        
        
        
        return feature;
    }
    
    
    private Feature buildFeature(int index, double x, double y, double rad, double prop, double angle, double value, String name)
    {
        FeatureSchema featureSchema = new FeatureSchema();

        featureSchema.addAttribute("Geometry",AttributeType.GEOMETRY);
        featureSchema.addAttribute("Index", AttributeType.INTEGER);
        featureSchema.addAttribute("dValue", AttributeType.DOUBLE);
        featureSchema.addAttribute("iValue", AttributeType.INTEGER);
        featureSchema.addAttribute("Name", AttributeType.STRING);
        
        Vector<Coordinate> points = new Vector<Coordinate>(10,2);

        if(debug) System.out.println("Feature at:"+x+","+y+" rad="+rad+" prop="+prop+"  angle="+angle+"  value="+value);
        //System.out.println("prop="+prop);
        Coordinate p = new Coordinate(x+xoffs,y+yoffs);
        if(prop < 1.0) points.addElement(p);
        double sweepAngle = prop*(2.0*Math.PI);
        if(ChartParams.clockwise) sweepAngle = -sweepAngle;
        double stepAngle = 2.0*Math.PI/64.0;
        if(ChartParams.clockwise) stepAngle = -stepAngle;
        if(debug) System.out.println("sweep="+sweepAngle+"  step="+stepAngle);
        double startAngle = angle;
        double xp = x + xoffs + rad*Math.cos(startAngle);
        double yp = y + yoffs + rad*Math.sin(startAngle);
        p = new Coordinate(xp,yp);
        points.addElement(p);
        if(!ChartParams.clockwise)
        {
            while (startAngle + stepAngle < angle+sweepAngle)
            {
                  startAngle = startAngle + stepAngle;
                  xp = x + xoffs + rad*Math.cos(startAngle);
                  yp = y + yoffs + rad*Math.sin(startAngle);
                  p = new Coordinate(xp,yp);
                  points.addElement(p);
            }
        }
        else  // clockwise
        {
            while (startAngle + stepAngle > angle+sweepAngle)
            {
                  startAngle = startAngle + stepAngle;
                  xp = x + xoffs + rad*Math.cos(startAngle);
                  yp = y + yoffs + rad*Math.sin(startAngle);
                  p = new Coordinate(xp,yp);
                  points.addElement(p);
            }
        }
        startAngle = angle + sweepAngle;
        xp = x + xoffs + rad*Math.cos(startAngle);
        yp = y + yoffs + rad*Math.sin(startAngle);
        p = new Coordinate(xp,yp);
        points.addElement(p);


        xp = x + xoffs;
        yp = y + yoffs;
        p = new Coordinate(xp,yp);
        if(prop < 1.0) points.addElement(p);
        else points.addElement(points.elementAt(0));

        Coordinate [] pointArray = new Coordinate[points.size()];
        for (int i=0; i < points.size(); i++)
        {
            pointArray[i] = points.elementAt(i);
            if(debug) System.out.println("Point "+i+":"+pointArray[i]);
        }

        LinearRing lr = new GeometryFactory().createLinearRing(pointArray);
        Geometry geometry = new GeometryFactory().createPolygon(lr, null);

        Feature feature = new BasicFeature(featureSchema);

        if(debug) System.out.println("Geometry: "+geometry);
        feature.setGeometry(geometry);
        feature.setAttribute("iValue", (int)value);
        feature.setAttribute("dValue", value);
        feature.setAttribute("Index", index);
        if(ChartParams.showLabels) feature.setAttribute("Name",name);
        else feature.setAttribute("Name","");
        return feature;
    }
    private void createLocalScale(FeatureCollection dataset, double x, double y, double interval, double scale, double maxValue)
    {
    	//boolean debug = true;
    	FeatureSchema featureSchema = new FeatureSchema();
        featureSchema.addAttribute("Geometry", AttributeType.GEOMETRY);
        featureSchema.addAttribute("dValue", AttributeType.DOUBLE);
        featureSchema.addAttribute("iValue", AttributeType.INTEGER);
        featureSchema.addAttribute("Index", AttributeType.INTEGER);
        featureSchema.addAttribute("Name", AttributeType.STRING);

        double width = 3*screenScale;
        double yp = y+ 5*interval*scale;
        
            //Coordinate [] points = new Coordinate[2];
            //points[0] = new Coordinate(x,y);
            //points[1] = new Coordinate(x,y+ 5*interval*scale);
           
            //Geometry geometry = new GeometryFactory().createLineString(points);
            
            //if(debug) System.out.println("Line Geometry: "+geometry);
            //Feature feature = new BasicFeature(featureSchema);

            
            //feature.setGeometry(geometry);
            //feature.setAttribute("iValue",-1);
            //feature.setAttribute("dValue",-1.0);
            //feature.setAttribute("Name","");
            //dataset.add(feature);
            double yMax = interval*scale*5;
            double yAct;
            for(int i=1; i <=5 ; i++)
            {
            	yAct = interval*scale*i;
            	if(!ChartParams.linearScale)
            	{
            		yAct = yMax*Math.sqrt(interval*scale*i/yMax);
            	}
	            Coordinate [] linePoints = new Coordinate[2];
	            linePoints[0] = new Coordinate(x,y+yAct);
	            linePoints[1] = new Coordinate(x-width, y+yAct);
	
	            Geometry linegeometry = new GeometryFactory().createLineString(linePoints);
	            
	            Feature linefeature = new BasicFeature(featureSchema);
	
	            if(debug) System.out.println("i="+i+" Bar Geometry: "+linegeometry);
	            linefeature.setGeometry(linegeometry);
	            linefeature.setAttribute("dValue",-1.0);
	            linefeature.setAttribute("iValue",-1);
	            linefeature.setAttribute("Index",-1);
	            
	            if(interval*i < maxValue  && i < 5)
	            {
	            	linefeature.setAttribute("Name","");
	            	dataset.add(linefeature);
	            }
	            else
	            {
	            	linefeature.setAttribute("Name",NumberFormatter.format(interval*i));
	            	dataset.add(linefeature);
	            	
	            	Coordinate [] points = new Coordinate[2];
	                points[0] = new Coordinate(x,y);
	                points[1] = new Coordinate(x,y+ yAct);	               
	                Geometry geometry = new GeometryFactory().createLineString(points);
	                if(debug) System.out.println("Line Geometry: "+geometry);
	                Feature feature = new BasicFeature(featureSchema);	                
	                feature.setGeometry(geometry);
	                feature.setAttribute("iValue",-1);
	                feature.setAttribute("dValue",-1.0);
	                feature.setAttribute("Name","");
	                dataset.add(feature);
	            	break;
	            }
	            
	            //if(i < 5) linefeature.setAttribute("Name","");
	            //else linefeature.setAttribute("Name",NumberFormatter.format(interval*i));
	            //dataset.add(linefeature);
	
	            
            }
        
        
        
    }
    
}
