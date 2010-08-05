/***********************************************************************
 * @(#)$RCSfile: DebugMessageDIFFMILLISECONDS.java,v $   $Revision: 1.1.1.1 $ $Date: 2003/01/10 15:33:37 $
 *
 * Copyright (c) 2000 IICM, Graz University of Technology
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


package org.dinopolis.util.debug;

//______________________________________________________________________
//______________________________________________________________________
/**
 * This class is chosen by the DebugMessageFactory depending on the
 * token used in the message format.
 *
 * This implementation returns the time difference since the last
 * debug message was printed in milliseconds.
 *     
 */
public class DebugMessageDIFFMILLISECONDS extends DebugMessageFormatObject
{

//______________________________________________________________________
/**
 * This implementation returns the time difference since the last
 * debug message was printed in milliseconds.
 *
 * @param level the debug level for the given debug message.
 * @param debug_message the debug message to be printed.
 * @param debug_instance the debug object this message string belongs to. 
 * It can be used in the <code>getMessage</code>-method to retrieve
 * additional information about what should be returned exactly.
 */
  public String getEvaluatedKeyword(String level,
                                    String debug_message,
                                    Debug debug_instance)
  {
    return(String.valueOf(System.currentTimeMillis() 
                          - debug_instance.getLastDebugMessageTime()));
  }
}



