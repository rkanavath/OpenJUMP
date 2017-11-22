package com.cadplan.fileio;

//==================================================================
//
// IconLoader
//
//==================================================================
//
// This module is a part of the P-Coder package
// ©2002, Geoffrey G. Roy, School of Engineering, Murdoch University
//==================================================================


import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.net.*;
import java.util.*;
import java.io.*;
import java.util.jar.*;
import java.lang.*;
import javax.swing.*;




public class IconLoader extends Component
{
     boolean debug = false;
     Image image = null;
	 Properties prop = null;
	 MediaTracker tracker;
	 URL url, jarurl;
     String appName;
	 String wd = "";


    public IconLoader(String wd, String appName)
    {
        this.wd = wd;
        this.appName = appName;
    }
    
     public IconLoader(String appName)
 	{

		prop = System.getProperties();
	    wd = prop.getProperty("user.dir");
        this.appName = appName;
    }

    public void setWD(String dirName)
    {
        wd = dirName;
    }
	
    public Image loadImage(String name)
	{
        tracker = new MediaTracker(this);
        Image image = load(name);
		return(image);
		
	}

   
    public Image load(String name)
	{


		if(debug) System.out.println("User WD = "+ wd);
		String filename;
		filename = "jar:file:"+wd+"/"+appName+".jar!/"+"Resources/"+name;
        if(debug) System.out.println("Loading from jar: "+filename);
		try
		{
		  jarurl = new URL(filename);
	//	  System.out.println("URL= "+jarurl);
		  JarURLConnection jarConnection = (JarURLConnection) jarurl.openConnection();
		  Manifest manifest = jarConnection.getManifest();	
	//	  System.out.println("Loading splash from JAR");		
		  
		}
		catch (MalformedURLException e)
		{
			if(debug) System.out.println("Jar file not found at "+wd);
		}
		catch (IOException e)
		{ 


			if(debug) System.out.println("Error opening Jar file at: "+wd);
			if(debug) System.out.println("Jar file not found "+e);
            if(debug) System.out.println("Error opening Jar file from URL, trying local file: "+e);
            try
			{
					String fname = "";
					fname = "file:///"+wd+"/Classes/Resources/"+name;
					if(debug) System.out.println("Loading from: "+fname);
					jarurl = new URL(fname );			
				    if(debug) System.out.println("Loading icon from GIF");		
			}
			catch (MalformedURLException e1)
			{
				System.out.println("ERROR - "+e1);
		    }

		}
		if(debug) System.out.println("URL= "+jarurl);
        image = Toolkit.getDefaultToolkit().getImage(jarurl);
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
