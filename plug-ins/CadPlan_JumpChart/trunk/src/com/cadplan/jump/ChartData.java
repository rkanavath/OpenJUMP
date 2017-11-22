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

import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

import javax.swing.*;
import java.util.Vector;
import java.util.List;
import java.util.Iterator;
import java.text.*;

/**
 * User: geoff
 * Date: 28/04/2007
 * Time: 10:27:06
 * Copyright 2007 Geoffrey G Roy.
 */
public class ChartData
{
    boolean debug = false;
    Layer selectedLayer;
    FeatureSchema featureSchema;
    Vector<ChartAttribute> attributes;
    ChartValues [] chartData;
    
    
    Vector<String> possibleNames = new Vector<String>();
    I18NPlug iPlug;
    String chosenName = "";
    DecimalFormat iformat = new DecimalFormat("#####");


    public ChartData(PlugInContext context, I18NPlug iPlug)
    {
        this.iPlug = iPlug;
        ChartDialog chartDialog;
        selectedLayer = context.getSelectedLayer(0);
        if(selectedLayer != null)
        {
            featureSchema = selectedLayer.getFeatureCollectionWrapper().getFeatureSchema();
            
            
            
            do
            { 
                //System.out.println("Preparing data - ChartType = "+ ChartParams.chartType);
                attributes = new Vector<ChartAttribute>(10,2);
 	            for (int i=0; i < featureSchema.getAttributeCount(); i++)
	            {
	                AttributeType type = featureSchema.getAttributeType(i);
	                if( type == AttributeType.DOUBLE || type == AttributeType.INTEGER  || // type == AttributeType.STRING ||
	                		((ChartParams.chartType == ChartParams.LABELS) && type == AttributeType.STRING) )
	                {
	                    String name = featureSchema.getAttributeName(i);
	
	                    ChartAttribute att = new ChartAttribute(i, name);
	                    attributes.addElement(att);
	                    //System.out.println("Attribute added: "+name);
	                }
	            }

            
                 chartDialog = new ChartDialog(selectedLayer.getName(),attributes, iPlug);
                 //System.out.println("Reload = "+ ChartParams.reloadDialog);
                 if(ChartParams.reloadDialog) chartDialog = null;
                 
            } while (ChartParams.reloadDialog);
            
            if(!ChartParams.cancelled)
            {
                attributes = chartDialog.getSelections();
                if(debug) System.out.println("Number of selected att: "+attributes.size());

             // if attribute "Name" is not present choose required name label.
                if(ChartParams.chartType == ChartParams.LABELS || ChartParams.showUnderLabels)
                {
	                boolean nameFound = false;
	                for (int i=0; i < featureSchema.getAttributeCount(); i++)
	                {
	                    AttributeType type = featureSchema.getAttributeType(i);
	                    if( type == AttributeType.STRING  )
	                    {
	                        String name = featureSchema.getAttributeName(i);
	                        possibleNames.addElement(name);
	                        //System.out.println("Possible Name added: "+name);
	                        if (name.equals("Name")) nameFound = true;
	                    }
	                }
	                String [] pNames = new String[possibleNames.size()];
	                
	                for (int i=0; i < possibleNames.size(); i++)
	                {
	                	pNames[i] = possibleNames.elementAt(i);
	                }
	                
	                if(!nameFound && pNames.length > 0)
	                {
	                	String result = (String) JOptionPane.showInputDialog(null,iPlug.get("JumpChart.message5"),iPlug.get("JumpChart.message6"),
	                			             JOptionPane.QUESTION_MESSAGE, null, pNames, pNames[0]);
	                	
	                	if(result != null) chosenName = result;
	                	else chosenName = null;
	                	
	                }
	                else
	                {
	                	chosenName = "Missing";
	                }
                }
                
                List featureList = selectedLayer.getFeatureCollectionWrapper().getFeatures();
                
                
                int numFeatures = featureList.size();
                chartData = new ChartValues[numFeatures];
                
                Iterator j = featureList.iterator();
                int count = 0;
                while (j.hasNext())
                {
                    Feature feature = (Feature) j.next();
                    int ID = feature.getID();
                    String name;
                    String underName = "Missing";
                    String ctype = "INT";
                    
                    try
                    { 
                    	name= (String) feature.getAttribute("Name");
                    	
                    } catch (Exception ex)
                    {
                    	if(ChartParams.chartType == ChartParams.LABELS && chosenName != null )name = (String) feature.getAttribute(chosenName);
                    	else name = "";
                    	
                    	if(ChartParams.showUnderLabels && chosenName != null) name = (String) feature.getAttribute(chosenName);
                    	//System.out.println("ERROR: Attribute Name not found - inserted: "+name);
                    }
                    
                    
                   
                    
                    Geometry geometry = feature.getGeometry();
                    Point centroid = geometry.getInteriorPoint();
                    double [] values = new double[attributes.size()];
                    String [] nameValues = new String[attributes.size()];
                    String [] chartTypes = new String[numFeatures];
                    for (int k=0; k < attributes.size(); k++)
                    {
                        int index = attributes.elementAt(k).index;
                        double value = -1.0;
                        String vtype = "UND";
                        String nameValue = String.valueOf(value);
                        try
                        {
                            value = feature.getDouble(index);
                            nameValue = String.valueOf(value);
                            //System.out.println("Decoding Double: "+value+ " to "+nameValue);
                            vtype = "DBL";
                        }
                        catch (ClassCastException ex)
                        {
                        	try
                        	{
                                value = (double) feature.getInteger(index);
                                nameValue = String.valueOf(iformat.format(value));
                                vtype = "INT";
                                
                                //System.out.println("Decoding Integer: "+value+ " to "+nameValue);
                        	}
                        	catch (ClassCastException ex2)
                        	{
                        		value = -1.0;
                        		nameValue = (String) feature.getString(index);
                        		vtype = "STR";
                        		//System.out.println("Decoding String: "+value+ " to "+nameValue);
                        	}
                        	
                        }
                        catch (Exception ex)
                        {
                        	
                            value = -1.0;
                            nameValue = String.valueOf(value);
                            vtype = "UND";
                            //System.out.println("Fail: decode as string: "+nameValue+"  value:"+value);
                        }
                        values[k] = value;
                        nameValues[k] = nameValue;
                        chartTypes[k] = vtype;
                        
                    }
                    
                    
                    ChartValues chartValues = new ChartValues(ID, name, centroid.getX(),centroid.getY(),values, nameValues, chartTypes);
                    chartData[count] = chartValues;
                     //System.out.println("Values: "+chartData[count].toString());
                    count++;
                }
            }

            
          
        }
        else
        {
            ChartParams.cancelled = true;
            JOptionPane.showMessageDialog(null, iPlug.get("JumpChart.message1"),"Warning...",JOptionPane.WARNING_MESSAGE);
        }
    }

    public ChartValues [] getValues()
    {
        return chartData;
    }

    public ChartAttribute[] getAttributes()
    {
        ChartAttribute [] attributeData = new ChartAttribute[attributes.size()];
        for (int i=0; i < attributes.size(); i++)
        {
            attributeData[i] = attributes.elementAt(i);
        }
        return attributeData;
    }
}
