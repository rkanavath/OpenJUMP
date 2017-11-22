/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI 
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * JUMP is Copyright (C) 2003 Vivid Solutions
 *
 * This program implements extensions to JUMP and is
 * Copyright (C) 2004 Integrated Systems Analysts, Inc.
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
 * For more information, contact:
 *
 * Integrated Systems Analysts, Inc.
 * 630C Anchors St., Suite 101
 * Fort Walton Beach, Florida
 * USA
 *
 * (850)862-7321
 * www.ashs.isa.com
 */

package com.ashs.jump.plugin;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.Collection;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.util.Assert;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.LayerManager;
import com.vividsolutions.jump.workbench.ui.renderer.RenderingManager;
import com.vividsolutions.jump.workbench.ui.renderer.ThreadQueue;
import com.vividsolutions.jump.workbench.ui.LayerViewPanel;
import com.vividsolutions.jump.workbench.ui.LayerViewPanelContext;

/**
 * Renders layers as an Image, which can then be saved to a file or printed.
 */
public class LayerPrinter2
{
    private boolean debug = false;
    /**
	 * @param layers earlier layers will be rendered above later layers
	 */
	public BufferedImage print(Collection layers, Envelope envelope, int extentInPixels) throws Exception
    {
	    Assert.isTrue(!layers.isEmpty());
	
	    final Throwable[] throwable = new Throwable[] { null };
	    LayerViewPanel panel = 
	    	new LayerViewPanel(((Layer) layers.iterator().next()).getLayerManager(),
	            new LayerViewPanelContext()
                {
	                public void setStatusMessage(String message) {
	                }
	
	                public void warnUser(String warning) {
	                }
	
	                public void handleThrowable(Throwable t) {
	                    throwable[0] = t;
	                }
	            });
	    int extentInPixelsX;
	    int extentInPixelsY;
	    double width = envelope.getWidth();
	    double height = envelope.getHeight();
	    
	    if (width > height)
	    {
	    	extentInPixelsX = extentInPixels;
	    	extentInPixelsY = (int)Math.round(height / width * extentInPixels);
	    }
	    else
	    {
	    	extentInPixelsY = extentInPixels;
	    	extentInPixelsX = (int)Math.round(width / height * extentInPixels);
	    }

//        extentInPixelsX = 2*extentInPixelsX;
//        extentInPixelsY = 2*extentInPixelsY;
        if(debug) System.out.println("LayerPrinter2: extents: "+extentInPixelsX+","+extentInPixelsY);
        panel.setSize(extentInPixelsX, extentInPixelsY);
	    panel.getViewport().zoom(envelope);
	    
	    BufferedImage bufferedImage = new BufferedImage(panel.getWidth(),
	            panel.getHeight(), BufferedImage.TYPE_INT_ARGB);
	    Graphics2D graphics = bufferedImage.createGraphics();
		graphics.setRenderingHint(                            // LDB Added
				RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);


        paintBackground(graphics, extentInPixels);
	
	    RenderingManager renderingManager = panel.getRenderingManager();
		renderingManager.renderAll();
		ThreadQueue runningThreads = renderingManager.getDefaultRendererThreadQueue();
		while (runningThreads.getRunningThreads()>0)
			Thread.sleep(200);
		renderingManager.copyTo(graphics);
		
	    if (throwable[0] != null) {
	        throw throwable[0] instanceof Exception ? (Exception) throwable[0]
	                                                : new Exception(throwable[0].getMessage());
	    }
	    panel.dispose();
	    return bufferedImage;
	}

    private void paintBackground(Graphics2D graphics, int extent) {
        graphics.setColor(Color.white);
        graphics.fillRect(0, 0, extent, extent);
    }

}
