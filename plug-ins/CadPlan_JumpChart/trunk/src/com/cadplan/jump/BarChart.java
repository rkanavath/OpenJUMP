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

import com.vividsolutions.jump.feature.*;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Geometry;

import java.util.Vector;

/**
 * User: geoff
 * Date: 28/04/2007
 * Time: 16:50:31
 * Copyright 2007 Geoffrey G Roy.
 */
public class BarChart
{
    boolean debug = false;
    ChartAttribute [] attributes;
    ChartValues [] values;
    double screenScale;
    double maxValue;
    double maxSize;
    double xoffs, yoffs;
    double originX, originY;

    public BarChart(ChartAttribute [] attributes, ChartValues [] values, double screenScale)
    {
        this.attributes = attributes;
        this.values = values;
        this.screenScale = screenScale;
        xoffs = ChartParams.originX*screenScale;
        yoffs = ChartParams.originY*screenScale;
    }

    public double getMaxValue()
    {
        return maxValue;
    }
    public double getMaxSize()
    {
        return maxSize;
    }
    public void addFeatures(FeatureCollection dataset)
    {

         double barHeight;
         double barWidth = 5*screenScale;
         double height = 60;
         double dataMax = Double.MIN_VALUE;
         double x, y;
         double barHeightRatio;
         double maxHeight;
         double dx = 0.0;

         double [] itemMax = new double[values.length];

         for (int i=0; i < values.length; i++)
         {

             double [] data = values[i].values;
             itemMax[i] = Double.MIN_VALUE;

             for (int j=0; j < data.length; j++)
             {
                 if(data[j] > dataMax) dataMax = data[j];
                 if(data[j] > itemMax[i]) itemMax[i] = data[j];
             }
         }
        maxValue = dataMax;
       

        for(int i=0; i < values.length; i++)
        {
             double [] data = values[i].values;
             String [] ctypes = values[i].types;
             String nameValue = values[i].name;
             x = values[i].x;
             y = values[i].y;
             originY = y;
             
             if(ChartParams.autoScale)
             {
                 maxHeight = height*screenScale;
                 barWidth = 7*screenScale;
             }
             else
             {
                 maxHeight = ChartParams.scaleValue*screenScale;
                 barWidth = ChartParams.barWidth*screenScale;
             }
             maxSize = maxHeight;
             if(ChartParams.uniformSize)
             {
                 if(itemMax[i] != 0.0) barHeightRatio = maxHeight/itemMax[i];
                 else barHeightRatio = 1.0;
             }
             else
             {
                 if(dataMax != 0.0) barHeightRatio = maxHeight/dataMax;
                 else barHeightRatio = 1.0;
             }
            if(Double.isInfinite(barHeightRatio))  barHeightRatio = 1.0;
            //System.out.println("barHeightRatio="+barHeightRatio);
             dx = x - data.length*barWidth/2;
             for (int j=0; j < data.length; j++)
             {
            	 String name = attributes[j].name;
                 Feature feature = buildFeature(j, dx, y, barWidth, data[j]*barHeightRatio, data[j], name, ctypes[j]);
                 dataset.add(feature);
                 dx = dx + barWidth;
             }
             
             // -------------------------------------------
             //  build underlabel feature if required
             //--------------------------------------------
             //String nameValue = "Missing";
             if(ChartParams.showUnderLabels) 
             {
            	
            	 //System.out.println("Adding feature value namevalue="+nameValue);
            	 Feature feature = buildLabelFeature(x, y, nameValue);
            	 dataset.add(feature);
             }
             
        }
        
       
        
        
        if(ChartParams.localScale && !ChartParams.uniformSize)
        {
	        for(int i=0; i < values.length; i++)  
	        {
	        	double [] data = values[i].values;
	            
	            x = values[i].x;
	            y = values[i].y;
	            originX = x + data.length*barWidth/2 + barWidth/2 + xoffs;
	            originY = y + yoffs;
	           
			        double interval = ChartParams.scaleInterval(maxValue);
			        double scale = maxSize/maxValue;
			        if(ChartParams.localScale) createLocalScale(dataset,originX, originY, interval, scale, itemMax[i]);
	            
	        }
        }
        
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
    

    private Feature buildFeature(int index, double x, double y,  double width, double height, double value, String name, String ctype)
    {
        FeatureSchema featureSchema = new FeatureSchema();

        featureSchema.addAttribute("Geometry", AttributeType.GEOMETRY);
        featureSchema.addAttribute("iValue", AttributeType.INTEGER);
        featureSchema.addAttribute("dValue", AttributeType.DOUBLE);
        featureSchema.addAttribute("Index", AttributeType.INTEGER);
        featureSchema.addAttribute("Name", AttributeType.STRING);
        

        if(debug) System.out.println("Feature at:"+x+","+y+"  value="+value);
        Coordinate p = new Coordinate(x+xoffs,y+yoffs);
        Coordinate [] pointArray = new Coordinate[5];
        pointArray[0] = new Coordinate(x+xoffs,y+yoffs);
        pointArray[1]= new Coordinate(x+xoffs+width,y+yoffs);
        pointArray[2] = new Coordinate(x+xoffs+width,y+yoffs+height);
        pointArray[3] = new Coordinate(x+xoffs,y+yoffs+height);
        pointArray[4] = new Coordinate(x+xoffs,y+yoffs);
        for (int i=0; i < pointArray.length; i++)
        {
            if(debug) System.out.println("Point "+i+":"+pointArray[i]);
        }

        LinearRing lr = new GeometryFactory().createLinearRing(pointArray);
        Geometry geometry = new GeometryFactory().createPolygon(lr, null);

        Feature feature = new BasicFeature(featureSchema);

        if(debug) System.out.println("Geometry: "+geometry);
        feature.setGeometry(geometry);
        
        feature.setAttribute("iValue", (int) value);
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
            for(int i=1; i <=5 ; i++)
            {
	            Coordinate [] linePoints = new Coordinate[2];
	            linePoints[0] = new Coordinate(x,y+interval*scale*i);
	            linePoints[1] = new Coordinate(x-width, y+interval*scale*i);
	
	            Geometry linegeometry = new GeometryFactory().createLineString(linePoints);
	            
	            Feature linefeature = new BasicFeature(featureSchema);
	
	            if(debug) System.out.println("i="+i+" Bar Geometry: "+linegeometry);
	            linefeature.setGeometry(linegeometry);
	            linefeature.setAttribute("dValue",-1.0);
	            linefeature.setAttribute("iValue",-1);
	            linefeature.setAttribute("Index",-1);
	            
	            if(interval*i < maxValue && i < 5)
	            {
	            	linefeature.setAttribute("Name","");
	            	dataset.add(linefeature);
	            }
	            else
	            {
	            	linefeature.setAttribute("Name",formatValue(interval,i));
	            	dataset.add(linefeature);
	            	
	            	Coordinate [] points = new Coordinate[2];
	                points[0] = new Coordinate(x,y);
	                points[1] = new Coordinate(x,y+ i*interval*scale);	               
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
	            //else linefeature.setAttribute("Name",formatValue(interval,i));
	            //dataset.add(linefeature);
	
	            
            }
        
        
        
    }
    
    private String formatValue(double interval, int step)
    {
    	return NumberFormatter.format(interval*step);
    	//if(interval < 1.0) return String.valueOf(interval*step);
    	//return String.valueOf((int)(interval*step));
    }
   
}
