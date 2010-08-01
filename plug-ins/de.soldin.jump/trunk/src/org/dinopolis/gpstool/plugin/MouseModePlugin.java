/***********************************************************************
 * @(#)$RCSfile: MouseModePlugin.java,v $   $Revision: 1.5 $$Date: 2003/02/18 08:10:11 $
 *
 * Copyright (c) 2003 IICM, Graz University of Technology
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


package org.dinopolis.gpstool.plugin;

import org.dinopolis.gpstool.gui.MouseMode;

import com.bbn.openmap.Layer;

//----------------------------------------------------------------------
/**
 * This interface is used for all plugins that want to provide any
 * mouse handling for the application. If the mouse mode wants to draw
 * anything on the map, it may return a layer in the {@link #getLayer()}
 * method.
 * <p>
 * The plugins are informed about start and stop, so they may
 * initialize and free their resources on startup and on closing of
 * the application.
 *
 * @author Christof Dallermassl
 * @version $Revision: 1.5 $
 */

public interface MouseModePlugin extends Plugin, MouseMode
{

//----------------------------------------------------------------------
/**
 * If the mouse mode plugin wants to draw anything on the map it may
 * return a layer here or <code>null</code> if not. If this method is
 * called more than once, the plugin should return always the same
 * layer!
 *
 * @return the layer the plugin wants to paint into.
 * @see com.bbn.openmap.Layer
 */

  public Layer getLayer();

 
}


