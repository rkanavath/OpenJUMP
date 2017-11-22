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
import com.vividsolutions.jump.workbench.ui.renderer.style.FillPatternFactory;
import com.vividsolutions.jump.workbench.ui.renderer.style.WKTFillPattern;
import com.vividsolutions.jump.workbench.ui.renderer.style.ImageFillPattern;
import com.vividsolutions.jump.io.ParseException;
import com.vividsolutions.jts.io.WKTReader;
import com.vividsolutions.jts.geom.Geometry;
import com.cadplan.fileio.TextFile;

import javax.swing.*;
import java.util.Collection;
import java.util.ArrayList;
import java.util.Properties;
import java.util.StringTokenizer;
import java.io.File;
import java.io.FilenameFilter;
import java.awt.*;
import java.net.URL;
import java.net.MalformedURLException;
import org.apache.batik.transcoder.*;

/**
 * User: geoff
 * Date: 4/07/2007
 * Time: 09:41:24
 * Copyright 2005 Geoffrey G Roy.
 */
public class LoadFillPatterns extends Component implements FilenameFilter
{
    boolean debug = false;
    PlugInContext context;
    MediaTracker tracker;
    String wd;
    String [] patternNames = null;
    Collection customFillPatterns;

    public LoadFillPatterns(PlugInContext context)
    {
        this.context = context;

        File pluginDir = context.getWorkbenchContext().getWorkbench().getPlugInManager().getPlugInDirectory();

        tracker = new MediaTracker(this);
		Properties prop = System.getProperties();
	    wd = pluginDir.getAbsolutePath(); //.getProperty("user.dir");

        customFillPatterns = (Collection) context.getWorkbenchContext().getWorkbench().getBlackboard().get(FillPatternFactory.CUSTOM_FILL_PATTERNS_KEY,new ArrayList());

        loadNames();

    }

   

     public boolean accept(File dir, String name)
    {
        boolean match = false;
        if(name.toLowerCase().endsWith(".gif") ||
           name.toLowerCase().endsWith(".jpeg") ||
           name.toLowerCase().endsWith(".jpg") ||
           name.toLowerCase().endsWith(".png") ||
           name.toLowerCase().endsWith(".svg") ||
           name.toLowerCase().endsWith(".wkt")) match = true;
        return match;
    }

     public void loadNames()
    {
        File file= new File(wd+File.separator+"FillPatterns");
        if(!file.exists()) file.mkdirs();
        if(debug) System.out.println("Location: "+file);
        patternNames = file.list(this);
        if(patternNames == null || patternNames.length == 0) return;
        FillPatternParams.images =  new Image[patternNames.length];
        FillPatternParams.imageNames = patternNames;
        for(int i=0; i < patternNames.length; i++)
        {
            if(debug) System.out.println("Loading pattern: "+patternNames[i]);
            if(patternNames[i].toLowerCase().endsWith(".wkt"))
            {
                TextFile tfile = new TextFile(wd+File.separator+"FillPatterns",patternNames[i]);
                tfile.openRead();
                String text = tfile.readAll();
                tfile.close();
                StringTokenizer st = new StringTokenizer(text,":");
                int width = 1, extent = 10;
                String wkt = "";
                try
                {
                     width = Integer.parseInt(st.nextToken());
                     extent = Integer.parseInt(st.nextToken());
                     wkt = st.nextToken();

                    WKTReader testReader = new WKTReader();
                    try
                    {
                        Geometry geometry = testReader.read(wkt);
                        customFillPatterns.add(new WKTFillPattern(width,extent,wkt));
                    }
                    catch (com.vividsolutions.jts.io.ParseException ex)
                    {
                        JOptionPane.showMessageDialog(null,"Error parsing WKT in file: "+patternNames[i]+"\n"+wkt,
                                "Error...",JOptionPane.ERROR_MESSAGE);
                        System.out.println("Error parsing WKT file: "+patternNames[i]+"\n"+wkt);
                    }
                }
                catch (Exception ex)
                {
                   JOptionPane.showMessageDialog(null,"Error parsing WKT file: "+patternNames[i]+"\n"+text,
                            "Error...",JOptionPane.ERROR_MESSAGE);
                }
                if(debug) System.out.println("WKT:"+text);

            }
            else if(patternNames[i].toLowerCase().endsWith(".svg"))
            {
                String name = patternNames[i];
                Image image = null;
                URL url = null;
                try
                {
                    url = new URL( "file:///"+wd+File.separator+"FillPatterns"+File.separator+patternNames[i]);
                }
                catch(MalformedURLException ex)
                {
                    JOptionPane.showMessageDialog(null,"Error: "+ex,"Error...", JOptionPane.ERROR_MESSAGE);
                }
                if(debug) System.out.println("Loading SVG image: "+name);
                
                SVGRasterizer r = new SVGRasterizer(url);
                int size = 32;
                int k = name.lastIndexOf("_x");
                if(k > 0)
                {
                   int j = name.lastIndexOf(".");
                   String ss = name.substring(k+2,j);
                   j = ss.indexOf("x");
                   if(j > 0)
                   {
                       ss = ss.substring(j+1,ss.length());
                       try
                       {
                           size = Integer.parseInt(ss);
                       }
                       catch(NumberFormatException ex)
                       {
                           size = 32;
                       }
                   }
                    else
                   {
                       ss = ss.substring(j+1,ss.length());
                       try
                       {
                           size = Integer.parseInt(ss);
                       }
                       catch(NumberFormatException ex)
                       {
                           size = 32;
                       }
                   }
                }
                if(debug) System.out.println("SVG Image:"+name+"   size="+size);
                r.setImageWidth(size);
                r.setImageHeight(size);
                //r.setBackgroundColor(java.awt.Color.white);
                try
                {
                     image = r.createBufferedImage();
                }
                catch(TranscoderException ex)
                {
                    if(debug) System.out.println("ERROR:"+ex);
                }
                try
                {
                    tracker.addImage(image, 1);
                    tracker.waitForID(1);
                }
                catch (InterruptedException e)
                {
                }
                if(debug) System.out.println("Image size: "+image.getWidth(this)+", "+image.getHeight(this));
                FillPatternParams.images[i] = image;
                customFillPatterns.add(new MyImageFillPattern(name));
            }
            else
            {
                Image image = loadImage(wd+File.separator+"FillPatterns"+File.separator+patternNames[i]);
                FillPatternParams.images[i] = image;
                customFillPatterns.add(new MyImageFillPattern(patternNames[i]));
            }
        }
    }

    

    public Image loadImage(String name)
    {
        URL url= null;
        Image image = null;
        try
        {
            url = new URL( "file:///"+name );
        }
        catch(MalformedURLException ex)
        {
            JOptionPane.showMessageDialog(null,"Error: "+ex,"Error...", JOptionPane.ERROR_MESSAGE);
        }


        image = Toolkit.getDefaultToolkit().getImage(url);
        try
        {
            tracker.addImage(image, 1);
            tracker.waitForID(1);
        }
        catch (InterruptedException e)
        {
        }

        if(debug) System.out.println("Image size: "+image.getWidth(this)+", "+image.getHeight(this));
        if(image.getWidth(this) < 0) image = null;

//       Icon icon = new ImageIcon(image);
        return(image);

    }
}
