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

/**
 * User: geoff
 * Date: 29/04/2007
 * Time: 13:18:57
 * Copyright 2007 Geoffrey G Roy.
 */
public class ColumnChart
{
    boolean debug = false;
       ChartAttribute [] attributes;
       ChartValues [] values;
       double screenScale;
       double maxValue;
       double maxSize;
       double xoffs, yoffs;
       double originX, originY;

       public ColumnChart(ChartAttribute [] attributes, ChartValues [] values, double screenScale)
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

            double barHeight = 60;
            double barWidth = 5*screenScale;
            double maxHeight = 60;
            double dataMax = Double.MIN_VALUE;
            double x, y;

            double [] itemMax = new double[values.length];
            double [] sumItem = new double[values.length];
            double sumMax = Double.MIN_VALUE;
            for (int i=0; i < values.length; i++)
            {

                double [] data = values[i].values;
                itemMax[i] = Double.MIN_VALUE;
                sumItem[i] = 0.0;
                for (int j=0; j < data.length; j++)
                {
                    if(data[j] > dataMax) dataMax = data[j];
                    if(data[j] > itemMax[i]) itemMax[i] = data[j];
                    sumItem[i] = sumItem[i] + data[j];
                }
                if(sumItem[i] > sumMax) sumMax = sumItem[i];
            }
           double barHeightRatio = maxHeight/dataMax;


           for(int i=0; i < values.length; i++)
           {
                double [] data = values[i].values;
                String nameValue = values[i].name;
                x = values[i].x;
                y = values[i].y;
                double dy = y;
                

                if(ChartParams.autoScale)
                {
                    maxHeight = barHeight*screenScale;
                    barWidth = 10*screenScale;
                }
                else
                {
                    maxHeight = ChartParams.scaleValue*screenScale;
                    barWidth = ChartParams.barWidth*screenScale;
                }
                double dx = x - barWidth/2;
                if(ChartParams.uniformSize)
                {
                    barHeightRatio = 1.0*maxHeight;
                }
                else
                {
                    barHeightRatio = maxHeight*sumItem[i]/sumMax;
                }
               maxValue = sumMax;
               maxSize =  maxHeight;
                if(Double.isInfinite(barHeightRatio)) barHeightRatio = 1.0;
                for (int j=0; j < data.length; j++)
                {
                    double height = data[j]*barHeightRatio/sumItem[i];
                    String name = attributes[j].name;
                    if(Double.isInfinite(height)) height = 30.0;
                    if(Double.isNaN(height)) height = 30.0;
                    //System.out.println("i="+i+"  j="+j+" height="+height+"  barHeightratio="+barHeightRatio);
                    Feature feature = buildFeature(j, dx, dy, barWidth, height, data[j], name);
                    dataset.add(feature);
                    dy = dy + height;
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
   	            originX = x + 1.0 * barWidth + xoffs;
   	            originY = y + yoffs;
   	           
   			        double interval = ChartParams.scaleInterval(maxValue);
   			        double scale = maxSize/maxValue;
   			        if(ChartParams.localScale) createLocalScale(dataset,originX, originY, interval, scale, sumItem[i]);
   	            
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
       

       private Feature buildFeature(int index, double x, double y,  double width, double height, double value, String name)
       {
           FeatureSchema featureSchema = new FeatureSchema();

           featureSchema.addAttribute("Geometry", AttributeType.GEOMETRY);
           featureSchema.addAttribute("dValue", AttributeType.DOUBLE);
           featureSchema.addAttribute("iValue", AttributeType.INTEGER);
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
           feature.setAttribute("dValue", value);
           feature.setAttribute("iValue", (int) value);
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
           double barWidth = ChartParams.barWidth*screenScale;
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
   	            linePoints[1] = new Coordinate(x-barWidth/2, y+interval*scale*i);
   	
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
	            	linefeature.setAttribute("Name",NumberFormatter.format(interval*i));
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
   	            //else linefeature.setAttribute("Name",String.valueOf(interval*i));
   	            //dataset.add(linefeature);
   	
   	            
               }
           
           
           
       }
       
}
