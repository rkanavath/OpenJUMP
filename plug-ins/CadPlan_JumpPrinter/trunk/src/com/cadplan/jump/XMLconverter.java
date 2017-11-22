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

import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.AttributesImpl;
import org.xml.sax.*;

import java.util.Date;
import java.util.Vector;
import java.util.StringTokenizer;
import java.util.Properties;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.text.DateFormat;
import java.awt.*;
import java.awt.geom.Point2D;
import java.awt.print.PageFormat;
import java.awt.print.PrinterJob;
import java.awt.print.Paper;


import javax.xml.parsers.SAXParserFactory;
import javax.xml.parsers.SAXParser;
import javax.swing.*;
import javax.print.attribute.PrintRequestAttribute;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.Size2DSyntax;
import javax.print.attribute.standard.MediaPrintableArea;
import javax.print.attribute.standard.OrientationRequested;
import javax.print.attribute.standard.MediaSizeName;
import javax.print.attribute.standard.MediaSize;
import javax.print.PrintServiceLookup;
import javax.print.PrintService;
import javax.print.DocFlavor;

import com.vividsolutions.jump.util.Blackboard;
import java.io.*;

/**
 * User: geoff
 * Date: 11/01/2007
 * Time: 16:08:19
 * Copyright 2005 Geoffrey G Roy.
 */

    public class XMLconverter extends DefaultHandler {

        private boolean debug = false;
        private String filename;
        private String dir;
        private boolean validDoc = false;
        private StringBuffer accumulator = new StringBuffer();
        private AttributesImpl attributes;
        private Blackboard blackboard;
        private static final boolean vers16 = false;
        private int numNotes = 0;
        private Vector<FurnitureNote> notes = new Vector<FurnitureNote>();
        private Vector<FurnitureBorder> borders = new Vector<FurnitureBorder>();
        private Vector<FurnitureImage> imageItems =new Vector<FurnitureImage>();
        private I18NPlug iPlug;



        public XMLconverter(String filename, I18NPlug iPlug)
        {

            this.filename = filename;
            this.iPlug = iPlug;

        }

        public XMLconverter(String dir, String fname, I18NPlug iPlug)
        {
           this.filename = dir+ File.separator+fname;
           this.dir = dir;
           this.iPlug = iPlug;
            //System.out.println("XML Dir:"+dir+"  File:"+fname);
        }

    //-------------------------------------
    //   save file
    //-------------------------------------
        public boolean save(Blackboard blackboard)
        {
            try
            {
                 //FileWriter out = new FileWriter(filename);
            	OutputStreamWriter out = new OutputStreamWriter(new FileOutputStream(filename),"Unicode");

                //out.write("<?xml version = '1.0'  encoding = \'"+"ISO-8859-1"+"\'  standalone = 'yes' ?>\n");
                out.write("<?xml version = '1.0'  standalone = 'yes' ?>\n");
                DateFormat dateformat = DateFormat.getDateTimeInstance(DateFormat.LONG,DateFormat.LONG);
                Date date = new Date();
                String dateString = dateformat.format(date);
                out.write("<!-- "+"OpenJumpPrinter Configuration"+" created on "+dateString+" -->\n");
                out.write("<printer>\n");


              try
              {
                  Object object = null;
                  object = blackboard.get("PageFormat",null);
                  PageFormat pageFormat = (PageFormat) object;
                  if(pageFormat != null)
                  {
                      double imageableWidth = pageFormat.getImageableWidth();
                      double imageableHeight = pageFormat.getImageableHeight();
                      double imageableX = pageFormat.getImageableX();
                      double imageableY = pageFormat.getImageableY();
                      int orientation = pageFormat.getOrientation();
                      double width = pageFormat.getWidth();
                      double height = pageFormat.getHeight();
                      out.write("<pageformat width=\""+width+"\" height=\""+height+"\" orientation=\""+orientation +
                            "\" imageableWidth=\""+imageableWidth+"\" imageableHeight=\""+imageableHeight+
                              "\" imageableX=\""+imageableX+"\" imageableY=\""+imageableY+"\" />\n");

                  }
              }
              catch (Exception ex)
              {
                  out.write("ERROR:" + ex);
              }



               try
               {
                   double scale = blackboard.getDouble("Scale");
                   out.write("<drawingscale value=\""+scale+"\" />\n");
               }
               catch (Exception ex)
               {
               }


                try
               {
                   Object object = null;
                   object = blackboard.get("AutoScale",null);
                   boolean autoScale = (Boolean) object;
                   out.write("<autoscale value=\"" + autoScale+"\" />\n");

               }
               catch (Exception ex)
               {
               }

               try
               {
                   Object object = null;
                   object = blackboard.get("Quality",null);
                   boolean qualityOption = (Boolean) object;
                   out.write("<quality value=\"" + qualityOption+"\" />\n");

               }
               catch (Exception ex)
               {
               }
               try
               {
                   int printMode = blackboard.getInt("PrintMode");

                   out.write("<printmode value=\"" + printMode+"\" />\n");

               }
               catch (Exception ex)
               {
               }


               try
               {
                   Object object = null;
                   object = blackboard.get("PageOffset",null);
                   Point2D.Double pageOffset = (Point2D.Double) object;
                   out.write("<offset point=\""+pageOffset.x+","+pageOffset.y+"\" />\n");


               }
               catch (Exception ex)
               {
               }

               try
               {
                   Object object = null;
                   object = blackboard.get("Title",null);
                   FurnitureTitle title = null;
                   if(object != null)
                   {
                       title = (FurnitureTitle)  object;
                        out.write("<title text=\""+clean(title.text)+ "\" show=\""+title.show+
                           "\" location=\""+title.location.x+","+title.location.y+","+title.location.width+","+title.location.height+"\" "+
                           " font=\""+title.font.getName()+","+title.font.getStyle()+","+title.font.getSize()+"\" "+
                           " layer=\""+title.layerNumber+"\" "+
                           " color=\""+encodeColor(title.color)+"\" />\n");
                   }
                }
               catch (Exception ex)
               {
               }
               try
               {
                   Object object = null;
                   object = blackboard.get("ScaleItem",null);
                   FurnitureScale scaleItem;
                   if(object != null)
                   {
                       scaleItem = (FurnitureScale)  object;
                       out.write("<scale location=\""+scaleItem.location.x+","+scaleItem.location.y+","+scaleItem.location.width+","+scaleItem.location.height+
                               "\" color=\""+encodeColor(scaleItem.color)+
                       "\" show=\""+scaleItem.show+"\" range=\""+scaleItem.range+"\" interval=\""+scaleItem.interval+ "\" size=\""+scaleItem.sizeFactor +
                           "\" scale=\""+scaleItem.scale+"\" autoscale=\""+scaleItem.autoScale+
                           "\" numberscale=\""+ scaleItem.numberScale+"\" units=\"" + scaleItem.units+
                           "\" showratio=\""+scaleItem.showRatio + "\" showunit=\""+scaleItem.showUnits+"\" rangespec=\""+scaleItem.rangeSpec +
                           "\" color1=\"" + encodeColor(scaleItem.color1)+
                           "\" color2=\"" + encodeColor(scaleItem.color2)+
                           "\" font=\"" + scaleItem.font.getName()+","+scaleItem.font.getStyle()+","+scaleItem.font.getSize()+
                           "\" layer=\""+scaleItem.layerNumber+
                       
                       "\"/>\n");
                   }
               }
               catch (Exception ex)
               {
               }
               try
               {
                   Object object = null;
                   object = blackboard.get("Border",null);
                   FurnitureBorder border;
                   if(object != null)
                   {
                       border = (FurnitureBorder)  object;
                       out.write("<border location=\""+border.location.x+","+border.location.y+","+border.location.width+","+border.location.height+
                       "\" color=\""+encodeColor(border.color)+
                       "\" thickness=\""+border.thickness+"\" fixed=\""+border.fixed+
                       "\" showfill=\""+border.showFill+"\" color1=\""+encodeColor(border.color1)+
                       "\" layer=\""+border.layerNumber+
                       "\" show=\""+border.show+"\" />\n");
                   }
               }
               catch (Exception ex)
               {
               }
               try
               {
                   Object object = null;
                   object = blackboard.get("Borders",null);
                   Vector<FurnitureBorder> borders;
                   if(object != null)
                   {   
                	   borders = (Vector<FurnitureBorder>)  object;
                	   for(int i=0; i < borders.size(); i++)
                	   {
	                       FurnitureBorder border = borders.elementAt(i);
	                       out.write("<subborder location=\""+border.location.x+","+border.location.y+","+border.location.width+","+border.location.height+
	                       "\" color=\""+encodeColor(border.color)+
	                       "\" thickness=\""+border.thickness+"\" fixed=\""+border.fixed+
	                       "\" showfill=\""+border.showFill+"\" color1=\""+encodeColor(border.color1)+
	                       "\" layer=\""+border.layerNumber+
	                       "\" show=\""+border.show+"\" />\n");
                	   }
                   }
               }
               catch (Exception ex)
               {
               }
               try
               {
                   Object object = null;
                   object = blackboard.get("North",null);
                   FurnitureNorth north;
                   if(object != null)
                   {
                       north = (FurnitureNorth)  object;
                       out.write("<north location=\""+north.location.x+","+north.location.y+","+north.location.width+","+north.location.height+
                       "\" color=\""+encodeColor(north.color)+
                       "\" type=\""+north.type+
                       "\" rotation=\""+north.rotation+ "\" size=\""+north.sizeFactor +
                       "\" layer=\""+north.layerNumber+
                       "\" show=\""+north.show+"\" />\n");

                   }
               }
               catch (Exception ex)
               {
               }
               try
               {
            	   Object object = null;
            	   object = blackboard.get("Notes", null);
            	   if(object != null)
            	   {
            		   Vector<FurnitureNote> notes = (Vector<FurnitureNote>) object;
		               for(int i = 0; i < notes.size(); i++)
		               {
		            	   FurnitureNote note = notes.elementAt(i);
			               
			               out.write("<note text=\""+clean(note.text)+ "\" show=\""+note.show+"\" justify=\""+note.justify+
			                           "\" location=\""+note.location.x+","+note.location.y+","+note.location.width+","+note.location.height+"\" "+
			                           " font=\""+note.font.getName()+","+note.font.getStyle()+","+note.font.getSize()+"\" "+
			                           " color=\""+encodeColor(note.color)+"\" "+
			                           " color1=\""+encodeColor(note.color1)+"\" "+
			                           " layer=\""+note.layerNumber+
			                            "\" ");
			               if(note.bcolor != null) out.write(" bcolor=\""+encodeColor(note.bcolor)+"\" ");
			               out.write(" border=\""+note.border+"\" "); 
			               out.write(" width=\""+note.width+"\" ");
			               out.write(" />\n");
			                   
		               }
            	   }
               }
               catch (Exception ex)
               {
            	   
               }
                try
               {
                   Object object = null;
                   object = blackboard.get("Legend",null);
                   FurnitureLegend legend = null;
                   if(object != null)
                   {
                       legend = (FurnitureLegend)  object;
                        out.write("<legend show =\""+ legend.show+ "\" location=\""+legend.location.x+","+legend.location.y+","+
                                  legend.width+","+legend.height+"\" border=\""+legend.border+ "\" size=\""+legend.size+
                                  "\" color=\""+encodeColor(legend.color)+
                                  "\" name=\"" + legend.legendName +"\"  showname=\""+legend.showTitle+
                                  "\" titlecolor=\""+encodeColor(legend.legendTitleColor)+
                                  "\" textcolor=\""+encodeColor(legend.legendTextColor)+
                                  "\" bordercolor=\""+encodeColor(legend.legendBorderColor)+
                                  "\" textfont=\""+legend.legendFont.getName()+","+legend.legendFont.getStyle()+","+legend.legendFont.getSize()+
                                  "\" layer=\""+legend.layerNumber+
                                "\" include=\"");
                        for (int i=0; i < legend.legendItems.size(); i++)
                        {
                            LegendElement item = legend.legendItems.elementAt(i);
                            if(item.include) out.write(clean(item.name));
                            if(i < legend.legendItems.size()-1) out.write(",");
                        }

                        out.write("\" />\n");
                   }
                }
               catch (Exception ex)
               {
               }
                try
               {
                   Object object = null;
                   object = blackboard.get("LayerLegend",null);
                   LayerLegend legend = null;
                   if(object != null)
                   {
                       legend = (LayerLegend)  object;
                        out.write("<layerlegend show =\""+ legend.show+ "\" location=\""+legend.location.x+","+legend.location.y+","+
                                  legend.width+","+legend.height+"\" border=\""+legend.border+  "\" size=\""+legend.size+
                                  "\" color=\""+encodeColor(legend.color)+
                                
                                  "\" name=\"" + legend.legendName +"\"  showname=\""+legend.showTitle+
                                  "\" titlecolor=\""+encodeColor(legend.legendTitleColor)+
                                  "\" textcolor=\""+encodeColor(legend.legendTextColor)+
                                  "\" bordercolor=\""+encodeColor(legend.legendBorderColor)+
                                  "\" textfont=\""+legend.legendFont.getName()+","+legend.legendFont.getStyle()+","+legend.legendFont.getSize()+
                                  "\" layer=\""+legend.layerNumber+                   
                        
                        
                                   "\" include=\"");
                        for (int i=0; i < legend.legendItems.size(); i++)
                        {
                            LegendElement item = legend.legendItems.elementAt(i);
                            if(item.include) out.write(clean(item.name));
                            if(i < legend.legendItems.size()-1) out.write(",");
                        }

                        out.write("\" horizontal=\""+legend.horizontal+"\" />\n");
                   }
                }
               catch (Exception ex)
               {
               }
               
               try
               {
                   Object object = null;
                   object = blackboard.get("Images",null);
                   Vector<FurnitureImage> imageItems;
                   if(object != null)
                   {   
                	   imageItems = (Vector<FurnitureImage>)  object;
                	   for(int i=0; i < imageItems.size(); i++)
                	   {
	                       FurnitureImage imageItem = imageItems.elementAt(i);
	                       out.write("<image location=\""+imageItem.location.x+","+imageItem.location.y+","+imageItem.location.width+","+imageItem.location.height+
	                       "\" filename=\""+imageItem.fileName+
	                       "\" ratio=\"" +imageItem.ratioLocked+
	                       "\" show=\""+imageItem.show+"\" />\n");
                	   }
                   }
               }
               catch (Exception ex)
               {
               }

                out.write("</printer>\n");

                out.close();
                return true;

            }
            catch (IOException ex)
            {
                System.out.println("ERROR writing XML file: "+ex.toString());
                return false;
            }
        }

    //-----------------------------------------
    // import file
    //-----------------------------------------
        public boolean parse(Blackboard blackboard)
        {
            this.blackboard = blackboard;
            SAXParserFactory spf = SAXParserFactory.newInstance();
//		spf.setValidating(validation);

            XMLReader xmlReader = null;
            try
            {
                // Create a JAXP SAXParser
                SAXParser saxParser = spf.newSAXParser();

                // Get the encapsulated SAX XMLReader
                xmlReader = saxParser.getXMLReader();
            }
            catch (Exception ex) {
                System.err.println(ex);
                return false;
            }

            // Set the ContentHandler of the XMLReader
            xmlReader.setContentHandler(this);

            // Set an ErrorHandler before parsing
            xmlReader.setErrorHandler(new MyErrorHandler(System.err));

            try
            {
                // Tell the XMLReader to parse the XML document
                xmlReader.parse(convertToFileURL(filename));
                return true;

            }
            catch (SAXException se)
            {
                System.err.println(se.getMessage());
                return false;
            }
            catch (IOException ioe)
            {
                System.err.println(ioe);
                return false;
            }

        }

//========================================================================
// startDocument - Parser calls this once at the beginning of a document
//========================================================================
        public void startDocument() throws SAXException
        {
            validDoc = false;
        }

        public boolean isValid()
        {
            return validDoc;
        }
//======================================================
// chartacters - accumulate between tags
//======================================================
        public void characters(char [] buffer, int start, int length)
        {
            accumulator.append(buffer, start,length);
        }

//====================================================================
// startElement - Parser calls this for each element in a document
//====================================================================
        public void startElement(String namespaceURI, String localName,
                                 String qName, Attributes atts)
        throws SAXException
        {
            accumulator.setLength(0);
            String token;

            if(qName.equals("printer"))
            {
               // blackboard = new Blackboard();
            }

            if(qName.equals("pageformat"))
            {
                attributes = new AttributesImpl(atts);
                double imageableX = Double.parseDouble(attributes.getValue("imageableX"));
                double imageableY = Double.parseDouble(attributes.getValue("imageableY"));
                double imageableWidth = Double.parseDouble(attributes.getValue("imageableWidth"));
                double imageableHeight = Double.parseDouble(attributes.getValue("imageableHeight"));
                double width = Double.parseDouble(attributes.getValue("width"));
                double height = Double.parseDouble(attributes.getValue("height"));
                int orientation = Integer.parseInt(attributes.getValue("orientation"));
                
                //System.out.println("paper width="+width+" height="+height+" orientation="+orientation);
                //System.out.println("iX="+imageableX+" iY="+imageableY+" iW="+imageableWidth+" iH="+imageableHeight+"\n-----");
                PageFormat pageFormat = createPageFormat(orientation,imageableX, imageableY, imageableWidth,
                          imageableHeight, width, height);


                blackboard.put("PageFormat", pageFormat);
              // JOptionPane.showMessageDialog(null,"PageFormat1: "+width+","+height+","+imageableWidth+","+imageableHeight+","+orientation+","+imageableX+","+imageableY);

            }

            if(qName.equals("drawingscale"))
            {
                attributes = new AttributesImpl(atts);
                double drawingScale = Double.parseDouble(attributes.getValue("value"));
                blackboard.put("Scale",drawingScale);
            }

             if(qName.equals("autoscale"))
            {
                attributes = new AttributesImpl(atts);
                boolean autoScale = Boolean.parseBoolean(attributes.getValue("value"));
                blackboard.put("AutoScale",autoScale);
            }
             if(qName.equals("quality"))
            {
                attributes = new AttributesImpl(atts);
                boolean qualityOption = Boolean.parseBoolean(attributes.getValue("value"));
                blackboard.put("Quality",qualityOption);
            }
             if(qName.equals("printmode"))
            {
                attributes = new AttributesImpl(atts);
                int printMode = Integer.parseInt(attributes.getValue("value"));
                blackboard.put("PrintMode",printMode);
                //System.out.println("PrintMode="+printMode);
            }

             if(qName.equals("offset"))
            {
                attributes = new AttributesImpl(atts);
                String points = attributes.getValue("point");
                StringTokenizer st = new StringTokenizer(points,",");
                double x = Double.parseDouble(st.nextToken());
                double y = Double.parseDouble(st.nextToken());
                Point2D.Double point = new Point2D.Double(x,y);
                blackboard.put("PageOffset",point);
            }
             if(qName.equals("title"))
            {
               attributes = new AttributesImpl(atts);
               String text = attributes.getValue("text");
               boolean show = Boolean.parseBoolean(attributes.getValue("show"));
               Rectangle location = decodeLocation(attributes.getValue("location"));
               Color color = decodeColor(attributes.getValue("color"));
               Font font = decodeFont(attributes.getValue("font"));
               int layer = 0;
               if(attributes.getValue("layer") != null) layer = Integer.parseInt(attributes.getValue("layer"));
               FurnitureTitle title = new FurnitureTitle(text,font,location,show);
               title.color = color;
               title.layerNumber = layer;
               blackboard.put("Title", title);
            }
             if(qName.equals("scale"))
            {
                attributes = new AttributesImpl(atts);
                boolean show = Boolean.parseBoolean(attributes.getValue("show"));
                Rectangle location = decodeLocation(attributes.getValue("location"));
                Color color = decodeColor(attributes.getValue("color"));
                double range = Double.parseDouble(attributes.getValue("range"));
                double interval = Double.parseDouble(attributes.getValue("interval"));
                double scale = Double.parseDouble(attributes.getValue("scale"));
                int layer = 0;
                if(attributes.getValue("layer") != null) layer = Integer.parseInt(attributes.getValue("layer"));
                int numberScale = 1;
                try
                {
                	numberScale = Integer.parseInt(attributes.getValue("numberscale"));
                }
                catch(Exception ex)
                {
                	
                }
                boolean autoScale = Boolean.parseBoolean(attributes.getValue("autoscale"));
                boolean showRatio = Boolean.parseBoolean(attributes.getValue("showratio"));
                boolean showUnit = Boolean.parseBoolean(attributes.getValue("showunit"));
                String rangeSpec = attributes.getValue("rangespec");
                String units = attributes.getValue("units");
                String fonts = attributes.getValue("font");
                Font font = decodeFont(fonts);
                
                Color color1 = decodeColor(attributes.getValue("color1"));
                	
                Color color2 = decodeColor(attributes.getValue("color2"));
                
                FurnitureScale scaleItem = new FurnitureScale(scale, range, interval, location, show);
                scaleItem.autoScale = autoScale;
                scaleItem.color = color;
                String sizes = attributes.getValue("size");
                double size = 1.0;
                if(sizes != null) size = Double.parseDouble(sizes);
                scaleItem.sizeFactor = size;
                scaleItem.numberScale = numberScale;
                scaleItem.showRatio = showRatio;
                scaleItem.showUnits = showUnit;
                scaleItem.rangeSpec = rangeSpec;
                scaleItem.color = color;
                scaleItem.color1 = color1;
                scaleItem.color2 = color2;
                scaleItem.font = font;
                scaleItem.units = units;
                scaleItem.layerNumber = layer;
                
                scaleItem.setIPlug(iPlug);
                blackboard.put("ScaleItem", scaleItem);
            }
             if(qName.equals("border"))
            {
                attributes = new AttributesImpl(atts);
                boolean show = Boolean.parseBoolean(attributes.getValue("show"));
                boolean showFill = Boolean.parseBoolean(attributes.getValue("showfill"));
                Rectangle location = decodeLocation(attributes.getValue("location"));
                int layer = 0;
                if(attributes.getValue("layer") != null) layer = Integer.parseInt(attributes.getValue("layer"));
                boolean fixed = false;
                try
                {
                    fixed = Boolean.parseBoolean(attributes.getValue("fixed"));
                }
                catch (Exception ex) {}
                double thickness = 1.0;
                try
                {
                        thickness = Double.parseDouble(attributes.getValue("thickness"));
                }
                catch(Exception ex)
                {

                }
                Color color = decodeColor(attributes.getValue("color"));
                Color color1 = decodeColor(attributes.getValue("color1"));
                FurnitureBorder border = new FurnitureBorder(thickness,fixed,show);
                border.color = color;
                border.color1 = color1;
                border.location = location;
                border.layerNumber = layer;
                border.showFill = showFill;
                blackboard.put("Border", border);
            }
             if(qName.equals("subborder"))
             {
                 attributes = new AttributesImpl(atts);
                 boolean show = Boolean.parseBoolean(attributes.getValue("show"));
                 boolean showFill = Boolean.parseBoolean(attributes.getValue("showfill"));
                 Rectangle location = decodeLocation(attributes.getValue("location"));
                 boolean fixed = false;
                 int layer = 0;
                 if(attributes.getValue("layer") != null) layer = Integer.parseInt(attributes.getValue("layer"));
                 try
                 {
                     fixed = Boolean.parseBoolean(attributes.getValue("fixed"));
                 }
                 catch (Exception ex) {}
                 double thickness = 1.0;
                 try
                 {
                         thickness = Double.parseDouble(attributes.getValue("thickness"));
                 }
                 catch(Exception ex)
                 {

                 }
                 Color color = decodeColor(attributes.getValue("color"));
                 Color color1 = decodeColor(attributes.getValue("color1"));
                 FurnitureBorder border = new FurnitureBorder(thickness,fixed,show);
                 border.color = color;
                 border.color1 = color1;
                 border.location = location;
                 border.layerNumber = layer;
                 border.showFill = showFill;
                 borders.addElement(border);
                
             }
             if(qName.equals("north"))
            {
                attributes = new AttributesImpl(atts);
                boolean show = Boolean.parseBoolean(attributes.getValue("show"));
                int layer = 0;
                if(attributes.getValue("layer") != null) layer = Integer.parseInt(attributes.getValue("layer"));
                double rotation = 0.0;
                try
                {
                    rotation = Double.parseDouble(attributes.getValue("rotation"));
                }
                catch (Exception ex)
                {

                }
                double size = 1.0;
                try
                {
                    size = Double.parseDouble(attributes.getValue("size"));
                }
                catch (Exception ex)
                {

                }
                Rectangle location = decodeLocation(attributes.getValue("location"));
                Color color = decodeColor(attributes.getValue("color"));
                int type = Integer.parseInt(attributes.getValue("type"));
                FurnitureNorth north = new FurnitureNorth(type, location, show);
                north.color = color;
                north.rotation = rotation;
                north.sizeFactor = size;
                north.layerNumber = layer;
                blackboard.put("North", north);
            }
             if(qName.equals("note"))
            {
               attributes = new AttributesImpl(atts);
               String text = declean(attributes.getValue("text"));
               boolean show = Boolean.parseBoolean(attributes.getValue("show"));
               boolean border = Boolean.parseBoolean(attributes.getValue("border"));
               Rectangle location = decodeLocation(attributes.getValue("location"));
               Color color = decodeColor(attributes.getValue("color"));
               Color color1 = decodeColor(attributes.getValue("color1"));
               int layer = 0;
               if(attributes.getValue("layer") != null) layer = Integer.parseInt(attributes.getValue("layer"));
               Color bcolor = null;
               boolean setBackColor = false;
               if(attributes.getValue("bcolor") != null) 
               {
            	   bcolor = decodeColor(attributes.getValue("bcolor"));
            	   setBackColor = true;
            	}
               Font font = decodeFont(attributes.getValue("font"));
               int justify = Integer.parseInt(attributes.getValue("justify"));
               int width=0;
               String swidth = attributes.getValue("width");
               if(swidth != null) width = Integer.parseInt(swidth);
               FurnitureNote note = new FurnitureNote(text,font,justify,width,location,show);
               
               note.color = color;
               note.setBackColor = setBackColor;
               note.bcolor = bcolor;
               note.border = border;
               note.color1 = color1;
               note.layerNumber = layer;
               notes.addElement(note); // accumulate notes
               blackboard.put("Note", note);  // add single (last note here only)
               //System.out.println("Note added: "+note.text);
               
            }
             if(qName.equals("legend"))
            {
               attributes = new AttributesImpl(atts);
               boolean show = Boolean.parseBoolean(attributes.getValue("show"));
               Rectangle location = decodeLocation(attributes.getValue("location"));
               boolean border = Boolean.parseBoolean(attributes.getValue("border"));
               String name = attributes.getValue("name");
               boolean showTitle = Boolean.parseBoolean(attributes.getValue("showname"));
               Color titleColor = decodeColor(attributes.getValue("titlecolor"));
               Color textColor = decodeColor(attributes.getValue("textcolor"));
               Color borderColor = decodeColor(attributes.getValue("bordercolor"));
               Font textFont = decodeFont(attributes.getValue("textfont"));
               int layer = 0;
               if(attributes.getValue("layer") != null) layer = Integer.parseInt(attributes.getValue("layer"));
               String fsize = attributes.getValue("size");
               int size = 8; // default
               if(fsize != null) size = Integer.parseInt(fsize);
               String includeNames = attributes.getValue("include");
               Color color = decodeColor(attributes.getValue("color"));
               StringTokenizer st = new StringTokenizer(includeNames,",");
                int numTokens = st.countTokens();
               String [] names = new String[st.countTokens()];
               for (int i=0; i < numTokens; i++)
               {
                   names[i] = st.nextToken();
               }

               try
               {
                   Object object = null;
                   object = blackboard.get("Legend",null);
                   FurnitureLegend legend = null;
                   if(object != null)
                   {
                       legend = (FurnitureLegend)  object;
                       legend.show = show;
                       legend.location = location;
                       legend.border = border;
                       legend.color = color;
                       legend.size = size;
                       legend.legendName = name;
                       legend.showTitle = showTitle;
                       legend.legendTitleColor = titleColor;
                       legend.legendTextColor = textColor;
                       legend.legendBorderColor = borderColor;
                       legend.legendFont = textFont;
                       legend.layerNumber = layer;
                       for (int i=0; i < legend.legendItems.size(); i++)
                       {
                           LegendElement item = legend.legendItems.elementAt(i);
                           item.include = false;
                           for (int j=0; j < names.length; j++)
                           {
                               if(names[j].equals(item.name)) item.include = true;
                           }
                       }
                   }
                   blackboard.put("Legend", legend);
                }
               catch (Exception ex)
               {
               }

            }
             if(qName.equals("layerlegend"))
            {
               attributes = new AttributesImpl(atts);
               boolean show = Boolean.parseBoolean(attributes.getValue("show"));
               Rectangle location = decodeLocation(attributes.getValue("location"));
               boolean border = Boolean.parseBoolean(attributes.getValue("border"));
               
               String name = attributes.getValue("name");
               boolean showTitle = Boolean.parseBoolean(attributes.getValue("showname"));
               Color titleColor = decodeColor(attributes.getValue("titlecolor"));
               Color textColor = decodeColor(attributes.getValue("textcolor"));
               Color borderColor = decodeColor(attributes.getValue("bordercolor"));
               Font textFont = decodeFont(attributes.getValue("textfont"));
               int layer = 0;
               if(attributes.getValue("layer") != null) layer = Integer.parseInt(attributes.getValue("layer"));
               //System.out.println("textFont="+textFont);
               boolean horizontal;
               try
               {
            	    horizontal = Boolean.parseBoolean(attributes.getValue("horizontal"));
               }
               catch (Exception ex)
               {
            	   horizontal = true;
               }
               String includeNames = attributes.getValue("include");
               String fsize = attributes.getValue("size");
               int size = 8; // default
               if(fsize != null) size = Integer.parseInt(fsize);
               Color color = decodeColor(attributes.getValue("color"));
               StringTokenizer st = new StringTokenizer(includeNames,",");
                int numTokens = st.countTokens();
               String [] names = new String[st.countTokens()];
               for (int i=0; i < numTokens; i++)
               {
                   names[i] = st.nextToken();
               }

               try
               {
                   Object object = null;
                   object = blackboard.get("LayerLegend",null);
                   LayerLegend legend = null;
                   if(object != null)
                   {
                       legend = (LayerLegend)  object;
                       legend.show = show;
                       legend.location = location;
                       legend.border = border;
                       legend.color = color;
                       legend.size = size;
                       legend.horizontal = horizontal;
                       
                       legend.legendName = name;
                       legend.showTitle = showTitle;
                       legend.legendTitleColor = titleColor;
                       legend.legendTextColor = textColor;
                       legend.legendBorderColor = borderColor;
                       legend.legendFont = textFont;
                       legend.layerNumber = layer;
                       
                       for (int i=0; i < legend.legendItems.size(); i++)
                       {
                           LegendElement item = legend.legendItems.elementAt(i);
                           item.include = false;
                           for (int j=0; j < names.length; j++)
                           {
                               if(names[j].equals(item.name)) item.include = true;
                           }
                       }
                   }
                   blackboard.put("LayerLegend", legend);
                }
               catch (Exception ex)
               {
               }

            }
             if(qName.equals("image"))
             {
                 attributes = new AttributesImpl(atts);
                 boolean show = Boolean.parseBoolean(attributes.getValue("show"));
                 Rectangle location = decodeLocation(attributes.getValue("location"));
                 String fileName = attributes.getValue("filename");
                 boolean locked = Boolean.parseBoolean(attributes.getValue("ratio"));
                 int layer = 0;
                 if(attributes.getValue("layer") != null) layer = Integer.parseInt(attributes.getValue("layer"));
                 
                 
                 
                 FurnitureImage imageItem = new FurnitureImage(fileName,show);
                 ImageLoader loader = new ImageLoader();
                 imageItem.image = loader.loadImage(fileName);
                 imageItem.location = location;
                 imageItem.layerNumber = layer;
                 imageItem.ratioLocked = locked;
                 imageItems.addElement(imageItem);
                
             }
        }

 //--------------------------------------------------------
 // some utilities methods
 //--------------------------------------------------------
 private Rectangle decodeLocation(String locs)
 {
      StringTokenizer st = new StringTokenizer(locs,",");
      int x = Integer.parseInt(st.nextToken());
      int y = Integer.parseInt(st.nextToken());
      int width = Integer.parseInt(st.nextToken());
      int height = Integer.parseInt(st.nextToken());
      Rectangle rect = new Rectangle(x,y,width, height);
      return rect;
 }

 private Color decodeColor(String cols)
 {
      if(cols == null) return Color.BLACK;
      StringTokenizer st = new StringTokenizer(cols,",");
      int r = Integer.parseInt(st.nextToken());
      int g = Integer.parseInt(st.nextToken());
      int b = Integer.parseInt(st.nextToken());
      int alpha = 255;
      if(st.hasMoreTokens()) alpha = Integer.parseInt(st.nextToken());
      Color color = new Color(r,g,b, alpha);
      return color;
 }
 
 private String encodeColor(Color color)
 {
	 int red = color.getRed();
	 int green = color.getGreen();
	 int blue = color.getBlue();
	 int alpha = color.getAlpha();
	 String cols = String.valueOf(red)+","+String.valueOf(green)+","+String.valueOf(blue)+","+String.valueOf(alpha);
	 return cols;
 }

 private Font decodeFont(String fonts)
 {
	  if(fonts == null) return new Font("SansSerif",Font.PLAIN,12);
      StringTokenizer st = new StringTokenizer(fonts,",");
      String name = st.nextToken();
      int style = Integer.parseInt(st.nextToken());
      int size = Integer.parseInt(st.nextToken());
      Font font = new Font(name, style, size);
      return font;
 }

 private double convertMM(double v)
 {
    // return (int)(1000.0*v/(72.0/25.4));
    // return (int)Math.round(v/(72.0/25.4));
     return v/(72.0/25.4);
     
 }

private PageFormat createPageFormat(int orientation, double imageableX, double imageableY,
                                        double imageableWidth, double imageableHeight, double width, double height)
{

//    This is the obvious approach but appears to have problems
        PageFormat pageFormat = new PageFormat();
        Paper paper = new Paper();
        pageFormat.setOrientation(orientation);
        if(orientation == PageFormat.PORTRAIT) // Portrait
        {
        	paper.setSize(width, height);
            paper.setImageableArea(imageableX, imageableY, imageableWidth, imageableHeight);
            pageFormat.setPaper(paper);
        }
        else                // Landscape & Reverse Landscape
        {
        	paper.setSize(height, width);
            //paper.setImageableArea(imageableY, imageableX, imageableHeight, imageableWidth);
        	paper.setImageableArea(imageableY,  width-imageableWidth-imageableX, imageableHeight, imageableWidth);
            pageFormat.setPaper(paper);
        }
        
        
        float pw = (float)(convertMM(width)/1000.0); ///1000.0);
        float ph = (float) (convertMM(height)/1000.0);  ///1000.0);
        int pu = Size2DSyntax.MM;
        //System.out.println("pw="+pw+" ph="+ph+" pu="+pu);
        //PrintRequestAttribute paperAtt = MediaSize.findMedia(pw,ph, pu);
        //System.out.println("paperAtt:"+paperAtt);
        
        //System.out.println("PageFormat: imageableX="+pageFormat.getImageableX()+" imageableY="+pageFormat.getImageableY()+"  width="+width+" height="+height+"  orientation= "+orientation);

       return pageFormat;


//  This approach works most of the time, but in some instances fails to restore the imageableX and imagfeableY parameters
//  correctly in some cases
	
/*	
       int iXmm = convertMM(imageableX);
       int iYmm = convertMM(imageableY);
       int iWmm = convertMM(imageableWidth);
       int iHmm = convertMM(imageableHeight);
       float widthmm = (float)convertMM(width);
       float heightmm = (float)convertMM(height);

       MediaPrintableArea pa = new MediaPrintableArea(iXmm, iYmm, iWmm, iHmm, MediaPrintableArea.MM);
       PrintRequestAttribute areaAtt = pa;

        PrintRequestAttribute orienAtt = OrientationRequested.PORTRAIT;
        if(orientation == 0 ) orienAtt = OrientationRequested.LANDSCAPE;
       // PrintRequestAttribute paperAtt = MediaSize.findMedia(convertMM(width)/1000,convertMM(height)/1000, Size2DSyntax.MM);
        PrintRequestAttribute paperAtt = MediaSize.findMedia(widthmm, heightmm, Size2DSyntax.MM);

        PrintRequestAttributeSet aset = new HashPrintRequestAttributeSet();
        aset.add(orienAtt);
        aset.add(paperAtt);
        aset.add(areaAtt);
        PrinterJob pj = PrinterJob.getPrinterJob();

        Properties props = System.getProperties();
        String javaVersion = (String) props.get("java.version");
        //System.out.println("Java version:"+javaVersion);
        PageFormat pageFormat;
        if(javaVersion.compareTo("1.6") < 0)
        {
              pageFormat = pj.pageDialog(aset);
        }
         else
        {
         //**************************  swap for java 1.5, 1.6
               pageFormat = pj.getPageFormat(aset);  // to avoid pf dialog - does not yet work
               //pageFormat = pj.pageDialog(aset);
        }
         if(pageFormat == null)
         {
            pageFormat =  pj.pageDialog(pj.defaultPage());  // use default if cancelled
         }
//        System.out.println("conversions: "+iXmm+","+iYmm+","+iWmm+","+iHmm+" MM="+MediaPrintableArea.MM+";"+Size2DSyntax.MM);
//        System.out.println("PageFormat: imageableX="+pageFormat.getImageableX()+" imageableY="+pageFormat.getImageableY());
//        System.out.println("PageFormat: X="+pa.getX(Size2DSyntax.MM)+" Y="+pa.getY(Size2DSyntax.MM));
//        System.out.println("Media: "+paperAtt.toString()+"  pa: "+pa.toString());

        
        return pageFormat;
 */

    }
//================================================================
// endElement
//================================================================
        public void endElement(String namespaceURI, String localName,
                               String qName)
        {
             String text = accumulator.toString();




            if(qName.equals("printer"))
            {

            }
       }


//=======================================================================
// endDocument - Parser calls this once after parsing a document
//=======================================================================
        public void endDocument() throws SAXException
        {
        	blackboard.put("Notes", notes);// put all notes onto blackboard
        	blackboard.put("Borders",borders);
        	blackboard.put("Images",imageItems);
        	//System.out.println("All notes added");
            validDoc = true;

 //         if(debug) displayModel(model,0);
        }

        /**
         * Convert from a filename to a file URL.
         */
        private static String convertToFileURL(String filename)
        {
            if(filename.startsWith("http")) return(filename);

            // On JDK 1.2 and later, simplify this to:
            // "path = file.toURL().toString()".
            String path = new File(filename).getAbsolutePath();
            if (File.separatorChar != '/') {
                path = path.replace(File.separatorChar, '/');
            }
            if (!path.startsWith("/")) {
                path = "/" + path;
            }
            return "file:" + path;
        }


        // Error handler to report errors and warnings
//    private static class MyErrorHandler implements ErrorHandler
        private  class MyErrorHandler implements ErrorHandler {
            /** Error handler output goes here */
            private PrintStream out;

            MyErrorHandler(PrintStream out)
            {
                this.out = out;
            }

            /**
             * Returns a string describing parse exception details
             */
            private String getParseExceptionInfo(SAXParseException spe)
            {
                String systemId = spe.getSystemId();
                if (systemId == null) {
                    systemId = "null";
                }
                String info = "URI=" + systemId +
                    " Line=" + spe.getLineNumber() +
                    ": " + spe.getMessage();
                return info;
            }

            // The following methods are standard SAX ErrorHandler methods.
            // See SAX documentation for more info.

            public void warning(SAXParseException spe) throws SAXException
            {
                out.println("Warning: " + getParseExceptionInfo(spe));
//			parent.writemessageln("\nWarning: " + getParseExceptionInfo(spe));
            }

            public void error(SAXParseException spe) throws SAXException
            {
                String message = "Error: " + getParseExceptionInfo(spe);
//            parent.writemessageln("\nError: " + getParseExceptionInfo(spe));
                throw new SAXException(message);
            }

            public void fatalError(SAXParseException spe) throws SAXException
            {
                String message = "Warning: " + getParseExceptionInfo(spe);
//            parent.writemessageln("\nWarning: " + getParseExceptionInfo(spe));
                throw new SAXException(message);
            }
        }

    private String clean(String s)
    {
        if(s == null || s.trim().equals("")) return "null";
        StringBuffer os = new StringBuffer(s);
        StringBuffer ns = new StringBuffer();
        StringBuffer temp = new StringBuffer();
        for (int i=0;i<os.length() ;i++ )
        {
            char c = os.charAt(i);
            temp.append(String.valueOf((int)c)+",");
            switch (c)
            {
                case '<': ns.append("&lt;");
                    break;
                case '>': ns.append("&gt;");
                    break;
                case '&': ns.append("&amp;");
                    break;
                case '\"': ns.append("&quot;");
                    break;
                case '\'': ns.append("&apos;");
                    break;
                case '\n': ns.append("|");
                    break;
                case '\r': break;
                default:   ns.append(c);
                    break;
            }
        }
        return(ns.toString());

    }
    private String declean(String s)
    {
        if(s == null || s.trim().equals("")) return "null";
        StringBuffer os = new StringBuffer(s);
        StringBuffer ns = new StringBuffer();
        for (int i=0; i<os.length() ;i++ )
        {
            char c = os.charAt(i);
            switch (c)
            {
                case '|': ns.append("\n");
                    break;
                default:   ns.append(c);
                    break;
            }
        }
        return(ns.toString());


    }



}

