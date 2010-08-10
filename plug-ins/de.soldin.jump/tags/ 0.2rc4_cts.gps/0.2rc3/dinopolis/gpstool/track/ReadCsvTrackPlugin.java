/***********************************************************************
 * @(#)$RCSfile: ReadCsvTrackPlugin.java,v $   $Revision: 1.1 $$Date: 2006/11/06 20:17:52 $
 *
 * Copyright (c) 2002 IICM, Graz University of Technology
 * Inffeldgasse 16c, A-8010 Graz, Austria.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License (LGPL)
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public 
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 
 * 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 ***********************************************************************/


package org.dinopolis.gpstool.track;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.NoSuchElementException;

import org.dinopolis.gpstool.GpsylonKeyConstants;
import org.dinopolis.gpstool.plugin.PluginSupport;
import org.dinopolis.gpstool.plugin.ReadTrackPlugin;
import org.dinopolis.util.Debug;
import org.dinopolis.util.io.Tokenizer;

//----------------------------------------------------------------------
/**
 * This plugin reads track data from a stream (ususally from a file)
 * and provides one or more tracks (lists of {@link Trackpoint}
 * objects). This plugin reads a csv file. It is a quick and dirty plugin.
 * The csv file holds lines in format
 * latitude,longitude,altitude,unix time
 *
 * @author Christof Dallermassl
 * @version $Revision: 1.1 $
 */

public class ReadCsvTrackPlugin implements ReadTrackPlugin, GpsylonKeyConstants
{

  //Resources resources_;
  
  public ReadCsvTrackPlugin()
  {
  }


//----------------------------------------------------------------------
/**
 * Initialize the plugin and pass a PluginSupport that provides
 * objects, the plugin may use.
 *
 * @param support the PluginSupport object
 */
  public void initializePlugin(PluginSupport support)
  {
    //resources_ = support.getResources();
  }
  
//----------------------------------------------------------------------
/**
 * The application calls this method to indicate that the plugin is
 * activated and will be used from now on. The Plugin should
 * initialize any needed resources (files, etc.) in this method.
 *
 * @throws Exception if an error occurs. If this method throws an
 * exception, the plugin will not be used by the application.
 */

  public void startPlugin()
    throws Exception
  {
  }

//----------------------------------------------------------------------
/**
 * The application calls this method to indicate that the plugin is
 * deactivated and will not be used any more. The Plugin should
 * release all resources (close files, etc.) in this method.
 *
 * @throws Exception if an error occurs.
 */

  public void stopPlugin()
    throws Exception
  {
  }

//----------------------------------------------------------------------
/**
 * Returns a short description of the track data that may be used e.g. in
 * a file chooser. If possible, the description should be localized.
 *
 * @return The short description of the content.
 */

  public String getContentDescription()
  {
    return "CSV Files";
  }
  
//----------------------------------------------------------------------
/**
 * Returns possible file extensions the content. This information
 * may be used in a file chooser as a filter (e.g. ["jpg","jpeg"]).
 *
 * @return The file extensions to use for this kind of data.
 */

  public String[] getContentFileExtensions()
  {
    return new String[] {"csv"};
  }
  

//----------------------------------------------------------------------
/**
 * Parse the given input stream and return tracks. If no tracks could
 * be read, an empty array (length of 0) is returned (not null!).
 *
 * @param in the inputstream to read the data from.
 * @return an array of {@link Track} objects.
 * @throws IOException if an error occurs during reading.  */
  public Track[] getTracks(InputStream in)
    throws IOException
  {
    int linenumber = 0;
    try
    {
      BufferedReader track_in = new BufferedReader(new InputStreamReader(in));
      String line;
      MessageFormat line_format;
      Track track = new TrackImpl();
      if(Debug.DEBUG)
        Debug.println("read_track","loading GPSMap track");

      Tokenizer tokenizer = new Tokenizer(in);
      List tokens;
      String token;
      float latitude = 0.0f;
      float longitude = 0.0f;
      float altitude = 0.0f;
      Date date = null;
      while(tokenizer.hasNextLine())
      {
        linenumber = tokenizer.getLineNumber();
        tokens = tokenizer.nextLine();
        if(tokens.size() > 0) 
        {
          token = (String) tokens.get(0);
          if(!token.startsWith("#"))
          {
             latitude = Float.parseFloat((String) tokens.get(0));
             longitude = Float.parseFloat((String) tokens.get(1));
             altitude = Float.parseFloat((String) tokens.get(2));
             date = new Date(Long.parseLong((String)tokens.get(3)));
             Trackpoint point = new TrackpointImpl();
             point.setDate(date);
             point.setLongitude(longitude);
             point.setLatitude(latitude);
             point.setAltitude(altitude);
             track.addWaypoint(point);
          }
        }
      }
      if(Debug.DEBUG)
        Debug.println("read_track_plugin","finished loading GPSMap track");

      track_in.close();
      
      Date first_date = ((Trackpoint)track.getWaypoint(0)).getDate();
      if(first_date == null)
        first_date = new Date(); // use now!
      SimpleDateFormat track_date_format = new SimpleDateFormat("yyyyMMdd-HH:mm:ss");
      track.setIdentification(track_date_format.format(first_date));
      track.setComment("track imported from csv");
      System.out.println("read track:"+track);
      return(new Track[] {track});
    }
    catch(ClassCastException cce)
    {
      System.err.println("ERROR: Error in line "+linenumber+": "+cce.getMessage());
      cce.printStackTrace();
    }
    catch(NumberFormatException nfe)
    {
      System.out.println("ERROR: ParseError in line "+linenumber);
      nfe.printStackTrace();
    }
    catch(NoSuchElementException nsee)
    {
      System.out.println("ERROR: Invalid track line format in line "+linenumber);
      nsee.printStackTrace();
    }
    return(new Track[0]); // in case of error, return empty array
  }


//----------------------------------------------------------------------
/**
 * Returns the unique id of the plugin. The id is used to identify
 * the plugin and to distinguish it from other plugins.
 *
 * @return The id of the plugin.
 */

  public String getPluginIdentifier()
  {
    return("ReadTrackGPSMap");
  }

//----------------------------------------------------------------------
/**
 * Returns the version of the plugin. The version may be used to
 * choose between different version of the same plugin. 
 *
 * @return The version of the plugin.
 */

  public float getPluginVersion()
  {
    return(1.0f);
  }

//----------------------------------------------------------------------
/**
 * Returns the name of the Plugin. The name should be a human
 * readable and understandable name like "Save Image as JPEG". It is
 * prefereable but not necessary that the name is localized. 
 *
 * @return The name of the plugin.
 */

  public String getPluginName()
  {
    return("Read Track Data from GPSMap");
  }

//----------------------------------------------------------------------
/**
 * Returns a description of the Plugin. The description should be
 * human readable and understandable like "This plugin saves the
 * content of the main window as an image in jpeg format". It is
 * prefereable but not necessary that the description is localized. 
 *
 * @return The description of the plugin.
 */

  public String getPluginDescription()
  {
    return("This plugin reads track data that was saved by GPSMap.");
  }


}
