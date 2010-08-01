/***********************************************************************
 * @(#)$RCSfile: SerialPortList.java,v $   $Revision: 1.3 $ $Date: 2006/01/23 16:20:56 $
 *
 * Copyright (c) 2001 IICM, Graz University of Technology
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


package org.dinopolis.gpstool.gpsinput;


import java.net.URISyntaxException;
import java.net.URL;
import java.util.*;
import gnu.io.*;

public class SerialPortList2 
{
  public static void main (String[] args) throws URISyntaxException 
  {
	setLibraryPath();
	//System.out.println(System.getProperties());
    Enumeration portList = CommPortIdentifier.getPortIdentifiers();
    CommPortIdentifier portId;

    while (portList.hasMoreElements()) {
      portId = (CommPortIdentifier) portList.nextElement();
      if (portId.getPortType() == CommPortIdentifier.PORT_SERIAL) 
      {
        System.out.println(portId.getName());
      } // end of if ()
    }
  } // end of main ()
  
  private static void setLibraryPath() throws URISyntaxException {
	  String os = System.getProperty("os.name");
	  String arch = System.getProperty("os.arch");
	  
	  URL whereami = SerialPortList2.class.getProtectionDomain().getCodeSource().getLocation();
	  
	  System.out.println(os);
	  System.out.println(arch);
	  System.out.println(whereami.toString());
	  
	  String libpath = System.getProperty("java.library.path");
	  String sep = System.getProperty("file.separator");
	  
	  gnu.io.Loader.libraryPath = whereami.getPath().substring(1) + "../rxtx/win32/";
	  gnu.io.Loader.libraryPath = "Y:/projekte/gps/proj_gpsylon_src-0.5.3/lib/native/Windows XP/x86";
	  //System.loadLibrary( "rxtxSerial2" );
	  //System.load(System.mapLibraryName(whereami.getPath().substring(1) + "../rxtx/win32/rxtxSerial"));
	  //System.load("Y://projekte//gps//proj_gpsylon_src-0.5.3//rxtx//win32//rxtxSerial.dll");
  }
  
}

