package com.cadplan.jump;

import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.renderer.style.WKTFillPattern;
import com.vividsolutions.jts.io.WKTReader;
import com.vividsolutions.jts.geom.Geometry;
import com.cadplan.fileio.TextFile;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.Properties;
import java.util.StringTokenizer;
import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.net.URL;
import java.net.MalformedURLException;
import org.apache.batik.transcoder.TranscoderException;


/**
 * User: geoff
 * Date: 17/06/2007
 * Time: 09:39:05
 * Copyright 2005 Geoffrey G Roy.
 */
public class LoadImages extends Component implements FilenameFilter, Runnable
{
    boolean debug = false;
    String [] imageNames = null;
    Image[]  images = null;
    String [] wktNames = null;
    WKTshape [] wktShapes = null;
    int numImages = 0;
    int numWKT = 0;
    MediaTracker tracker;
    String wd;
    Thread thread;
    boolean stopped = false;
    PlugInContext context;
    int numberSymbols = 0;

    public LoadImages(PlugInContext context)
    {
        this.context = context;
        start();
    }
    
    private void loadImagesData()
    {
          File pluginDir = context.getWorkbenchContext().getWorkbench().getPlugInManager().getPlugInDirectory();
        
        tracker = new MediaTracker(this);
		Properties prop = System.getProperties();
	    wd = pluginDir.getAbsolutePath(); //.getProperty("user.dir");
        loadNames();
        VertexParams.imageNames = imageNames;
        VertexParams.images = images;
        VertexParams.wktNames = wktNames;
        VertexParams.wktShapes = wktShapes;
    }

    public boolean accept(File dir, String name)
    {
        boolean match = false;
        if(name.toLowerCase().endsWith(".gif") ||
           name.toLowerCase().endsWith(".jpeg") ||
           name.toLowerCase().endsWith(".jpg") ||
           name.toLowerCase().endsWith(".png") ||
           name.toLowerCase().endsWith(".wkt") ||
           name.toLowerCase().endsWith(".svg")) match = true;
        return match;
    }

    private void loadNames()
    {
        File file= new File(wd+File.separator+"VertexImages");
        if(!file.exists()) file.mkdirs();
        if(debug) System.out.println("Location: "+file);
        String [] fileNames = file.list(this);
        if(fileNames == null || fileNames.length == 0) return;
        numberSymbols = fileNames.length;
        for(int i=0; i < fileNames.length;i++)
        {
            if(fileNames[i].toLowerCase().endsWith(".wkt"))
            {
                numWKT++;
            }
            else
            {
                numImages++;
            }
        }
        images = new Image[numImages];
        imageNames = new String[numImages];
        wktShapes = new WKTshape[numWKT];
        wktNames = new String[numWKT];
        int j = 0;
        for(int i=0; i < fileNames.length; i++)
        {
            if(!fileNames[i].toLowerCase().endsWith(".wkt"))
            {
                if(debug) System.out.println("Loading image: "+fileNames[i]);
                imageNames[j] = fileNames[i];
                images[j] = loadImage(wd+File.separator+"VertexImages"+File.separator+fileNames[i]);
                j++;
            }
        }
        j=0;
        for(int i=0; i < fileNames.length; i++)
        {
            if(fileNames[i].toLowerCase().endsWith(".wkt"))
            {
                if(debug) System.out.println("Loading image: "+fileNames[i]);
                wktNames[j] = fileNames[i];
                wktShapes[j] = loadWKT(wd+File.separator+"VertexImages"+File.separator+fileNames[i]);
                j++;
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

        if(name.toLowerCase().endsWith(".svg"))
        {
            if(debug) System.out.println("Loading SVG image: "+name);
            SVGRasterizer r = new SVGRasterizer(url);
            int size = 64;
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
                       size = 64;
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
        }

        else
        {
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
        }
//       Icon icon = new ImageIcon(image);
	    return(image);
    }

    private WKTshape loadWKT(String name)
    {
        Geometry geometry = null;
        int k = name.lastIndexOf(File.separator);
        String dirName = name.substring(0,k);
        String fileName = name.substring(k+1,name.length());
        if(debug) System.out.println("dir:"+dirName+"  file:"+fileName);
        TextFile tfile = new TextFile(dirName,fileName);
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
                 geometry = testReader.read(wkt);
            }
            catch (com.vividsolutions.jts.io.ParseException ex)
            {
                JOptionPane.showMessageDialog(null,"Error parsing WKT in file: "+name+"\n"+wkt,
                        "Error...",JOptionPane.ERROR_MESSAGE);
                System.out.println("Error parsing WKT file: "+name+"\n"+wkt);
            }

        }
        catch (Exception ex)
        {
           JOptionPane.showMessageDialog(null,"Error parsing WKT file: "+name+"\n"+text,
                    "Error...",JOptionPane.ERROR_MESSAGE);
        }
        if(debug) System.out.println("WKT:"+text);

        return new WKTshape(width, extent, wkt);
    }

	
	public void start()
	{
		thread = new Thread(this);
		thread.start();
	}
	public void stop()
	{
		stopped = true;
	}
	public void run() 
	{
		/*try
		{
			Thread.sleep(20000);
		}
		catch (InterruptedException ex)
		{
			
		}*/
		long start = System.currentTimeMillis();
		loadImagesData();
		double timeMillis = (System.currentTimeMillis()-start)/1000.0;
		//JOptionPane.showMessageDialog(null,"Vertex Data all loaded ["+numberSymbols+" symbols: "+timeMillis+"msec]",
		//		"Loading Vertex Data...", JOptionPane.INFORMATION_MESSAGE);
		
	}
    
}
