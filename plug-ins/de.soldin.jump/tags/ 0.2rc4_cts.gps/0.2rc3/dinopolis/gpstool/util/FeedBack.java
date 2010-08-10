/***********************************************************************
 * @(#)$RCSfile: FeedBack.java,v $   $Revision: 1.1.1.1 $$Date: 2003/01/10 15:33:36 $
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


package org.dinopolis.gpstool.util;

//----------------------------------------------------------------------
/**
 * @author Christof Dallermassl
 * @version $Revision: 1.1.1.1 $
 */

public interface FeedBack 
{
//----------------------------------------------------------------------
/**
 * Sets the message object as a feedback information.
 *
 * @param message the message.
 */
  public void feedBack(Object message);
}


