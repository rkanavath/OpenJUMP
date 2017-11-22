package com.cadplan.jump;

import java.awt.Component;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.JOptionPane;

import org.apache.batik.transcoder.TranscoderException;

public class ImageLoader extends Component
{	
	boolean debug = false;
	public ImageLoader()
	{
		
	}
	public Image loadImage(String fileName)
	{
		URL url= null;
		Image image = null;
		MediaTracker tracker = new MediaTracker(this);
		
		try
	    {
	        url = new URL( "file:///"+fileName );
	    }
	    catch(MalformedURLException ex)
	    {
	        JOptionPane.showMessageDialog(null,"Error: "+ex,"Error...", JOptionPane.ERROR_MESSAGE);
	    }
	    
		if(fileName.toLowerCase().endsWith(".svg"))
        {
            if(debug) System.out.println("Loading SVG image: "+fileName);
            SVGRasterizer r = new SVGRasterizer(url);
            int size = 256;
            int k = fileName.lastIndexOf("x");
            if(k > 0)
            {
               int j = fileName.lastIndexOf(".");
               String ss = fileName.substring(k+1,j);
                          
               try
               {
                   size = Integer.parseInt(ss);
               }
               catch(NumberFormatException ex)
               {
                   size = 256;
               }
               
            }
            if(debug) System.out.println("SVG Image:"+fileName+"   size="+size);
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
		
		    //System.out.println("Image size: "+image.getWidth(this)+", "+image.getHeight(this));
		    if(image.getWidth(this) < 0) 
		    {
		    	JOptionPane.showMessageDialog(null,"Image File not found:"+fileName,"Error...", JOptionPane.ERROR_MESSAGE);
		    	image = null;
		    }
		}
	    return image;
	}
}