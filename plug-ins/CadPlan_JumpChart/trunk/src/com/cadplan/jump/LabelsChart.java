/* The Unified Mapping Platform (JUMP) is an extensible, interactive GUI
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
public class LabelsChart
{
    boolean debug = false;
    ChartAttribute [] attributes;
    ChartValues [] values;
    double screenScale;
    double maxValue;
    double maxSize;
    double xoffs, yoffs;
    double originX, originY;

    public LabelsChart(ChartAttribute [] attributes, ChartValues [] values, double screenScale)
    {
        this.attributes = attributes;
        this.values = values;
        this.screenScale = screenScale;
        xoffs = ChartParams.originX*screenScale;
        yoffs = ChartParams.originY*screenScale;
        
        if(debug)
        {
	        for (int i=0; i < attributes.length; i++)
	        {
	        	System.out.println("Attribute "+i+":"+attributes[i].name);
	        }
	        for (int i=0; i < values.length; i++)
	        {
	        	System.out.println("Value "+i+":" +values[i].toString());
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
    public void addFeatures(FeatureCollection dataset)
    {

         
         double x, y;
         String name;
        
         double dx = 0.0;

         double [] itemMax = new double[values.length];

        
       

        for(int i=0; i < values.length; i++)
        {
             String [] data = values[i].names;             
             x = values[i].x;
             y = values[i].y;
             originY = y;
             name = values[i].name;
            
             Feature feature = buildFeature(i, name, attributes, x, y,  data);
             dataset.add(feature);           
        }
               
    }
   

    private Feature buildFeature(int index, String name, ChartAttribute [] attributes, double x, double y,  String [] data)
    {
    	boolean debug = false;
        FeatureSchema featureSchema = new FeatureSchema();

        //featureSchema.addAttribute("$FID", AttributeType.INTEGER);
        featureSchema.addAttribute("Geometry", AttributeType.GEOMETRY);
        featureSchema.addAttribute("Name", AttributeType.STRING);
        for (int i=0; i < attributes.length; i++)
        {
        	featureSchema.addAttribute(attributes[i].toString(), AttributeType.STRING);
        }
        
        

        if(debug) System.out.println("Feature at:"+x+","+y+"  data="+data);
        Coordinate p = new Coordinate(x+xoffs,y+yoffs);
        Coordinate [] pointArray = new Coordinate[1];     
        pointArray[0] = new Coordinate(x+xoffs,y+yoffs);
        Geometry pointgeometry = new GeometryFactory().createMultiPoint(pointArray);

       

        Feature feature = new BasicFeature(featureSchema);

        if(debug) System.out.println("Geometry: "+pointgeometry);
        feature.setGeometry(pointgeometry);
        //feature.setAttribute("$FID", index);
        feature.setAttribute("Name",name);
        for(int i=0; i < data.length; i++)
        {
        	if(debug) System.out.println("data "+i+":"+data[i]);
        }
        for(int i=0; i < attributes.length; i++)
        {
        	
        	feature.setAttribute(attributes[i].toString(), data[i]);
        }
  
        return feature;
    }

    
   
}