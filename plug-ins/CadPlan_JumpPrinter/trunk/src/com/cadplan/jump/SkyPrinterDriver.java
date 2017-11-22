package com.cadplan.jump;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.Rectangle2D;
import java.awt.print.PageFormat;
import java.awt.print.Paper;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import javax.print.attribute.Attribute;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.print.attribute.standard.MediaPrintableArea;
import javax.print.attribute.standard.MediaSize;
import javax.print.attribute.standard.MediaSizeName;
import javax.print.attribute.standard.OrientationRequested;
import javax.swing.*;

import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.LayerViewPanel;
import com.vividsolutions.jump.workbench.ui.renderer.LayerRenderer;
import com.vividsolutions.jump.workbench.ui.renderer.Renderer;
import com.vividsolutions.jump.workbench.ui.renderer.RenderingManager;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.ColorThemingStyle;

/**
 * This class performs basic vector or raster printing of the LayerViewPanel
 * set in the constructor.
 * Set the printLayerables collection to invoke the custom render() method or
 * override it with your own.
 * The static optimizeForVectors() method is provided for your convenience.
 * @author Larry Becker
 */
public class SkyPrinterDriver
{
   private boolean debug = false;
	private int printCallCount = 0;
	private LayerViewPanel panel = null;
//	final Throwable[] throwable = new Throwable[] { null };
	private boolean printBorder = false;
	private PlugInContext context = null;
	private double  resolutionFactor = 1;
    private ArrayList printLayerables = null;  //use RenderingManager when null
	private boolean singlePageMode = true;
	
	public ArrayList getPrintLayerables()
    {
		return printLayerables;
	}
	public void setPrintLayerables(ArrayList printLayerables)
    {
		this.printLayerables = printLayerables;
	}
	public boolean getSinglePageMode()
    {
		return singlePageMode;
	}
	public void setSinglePageMode(boolean singlePageMode)
    {
		this.singlePageMode = singlePageMode;
	}
	public boolean getPrintBorder()
    {
		return printBorder;
	}
	public void setPrintBorder(boolean printBorder)
    {
		this.printBorder = printBorder;
	}
	public double getResolutionFactor()
    {
		return resolutionFactor;
	}
	public void setResolutionFactor(double resolutionFactor)
    {
		this.resolutionFactor = resolutionFactor;
	}



    public SkyPrinterDriver(PlugInContext context, LayerViewPanel panel)
    {
		this.context = context;
		this.panel = panel;
	}
	
	/**
	 * An alternate method of rendering that tries to avoid anything that would
	 * cause PeekGraphics to set the raster mode of printing.  It is your
	 * repsonsibility to turn off transparency before calling.
	 * @param g
	 * @param layersReversed - pass the list of layers to render in reverse order
	 * @throws Exception
	 */
	private void render(Graphics2D g, Collection layersReversed)
			 throws Exception
    {
        if(debug) System.out.println("Render Graphics: "+g.toString()+"::"+g.getClipBounds());
        //Graphics2D graphics = (Graphics2D) g;
        for (Iterator i = layersReversed.iterator(); i.hasNext();)
        {
			Layerable layerable = (Layerable) i.next();
			if (!layerable.isVisible()) continue;
			if (layerable instanceof Layer)
            {
				// need to avoid using image buffers of any kind
				Layer layer = (Layer) layerable;
                if(debug) System.out.println("Painting layer: "+layer.getName()+"  panel:"+panel.getName()+" size:"+panel.getSize());
                LayerRenderer renderer = new LayerRenderer(layer, panel);
				renderer.createRunnable(); //don't run the runnable
               
                renderer.getSimpleFeatureCollectionRenderer().copyTo(g);
                while(renderer.getSimpleFeatureCollectionRenderer().isRendering())
                {
                   if(debug) System.out.println("Waiting for renderer");
                   try
                   {
                       Thread.sleep(100);
                   }
                   catch(InterruptedException ex)
                   {

                   }
                }

                
                renderer.clearImageCache();  //free memory
			}
//			else
//            {	// a WMSLayer or something like that	       //************
//				Renderer renderer = panel.getRenderingManager().createRenderer(layerable);
//				Runnable runnable = renderer.createRunnable();
//				if (runnable == null) continue;  //skip inactive layers
//				runnable.run();
//				renderer.copyTo(g);
//				renderer.clearImageCache();  //free memory
//			}
		}
		return;
	}


	public void paintLayers(Graphics g, PageFormat pf) //, PageFormat pf, int page)
			throws PrinterException
    {
        if(debug) System.out.println("printLayers Graphics: "+g.toString()+"::"+g.getClipBounds());
        Graphics2D graphics = (Graphics2D) g;
//		if (singlePageMode && (page > 0))
//        { /* only one page, and 'page' is zero-based */
//			return NO_SUCH_PAGE;
//		}
		//Optimize the graphics context for printing
		//Graphics2D graphics = (Graphics2D) g;
		//these hints will probably have little effect, but they don't hurt
		graphics.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS,
				RenderingHints.VALUE_FRACTIONALMETRICS_ON);
		graphics.setRenderingHint(RenderingHints.KEY_RENDERING,
				RenderingHints.VALUE_RENDER_QUALITY);
		graphics.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL,
				RenderingHints.VALUE_STROKE_NORMALIZE);
		graphics.setRenderingHint( RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);
//		if (printBorder){
//			Rectangle2D pageBorder = new Rectangle2D.Double(
//				pf.getImageableX(), pf.getImageableY(),
//				pf.getImageableWidth(), pf.getImageableHeight());
//
//			graphics.setPaint(Color.black);
//			graphics.draw(pageBorder);
//		}
		//this can take a while.  Let the user know something is happening
		printCallCount++;
        
        Rectangle r = graphics.getClipBounds();
        if(debug) System.out.println("Initial Clip bounds: "+r);
        if (r != null)
        {
			int correctedPanelHeight = Math.round(panel.getHeight() / (float) resolutionFactor);
			if (r.height > correctedPanelHeight)
            {
				graphics.setClip(r.x,r.y,r.width, correctedPanelHeight);
			}
             if(debug) System.out.println("Adjusted Clip bounds: "+graphics.getClipBounds());
            int yBottom = r.y+r.height;
            if(debug) System.out.println("yBottom="+yBottom);
            context.getWorkbenchFrame().setStatusMessage(
					"Printing (pass) "
					+ Integer.toString(printCallCount)
					+ " Y = " + Integer.toString(yBottom));
		}
		/*
	     * User (0,0) is typically outside the imageable area, so we must
		 * translate by the X and Y values in the PageFormat to avoid clipping
		 */
		 //graphics.translate(pf.getImageableX(), pf.getImageableY());
        if(debug) System.out.println("pf imageable X="+pf.getImageableX()+" Y+"+ pf.getImageableY());
        /*
	     * Multiplying the size of the imageable area and then drawing
		 * with the inverse scale factor increases the apparent 
		 * resolution while maintaining the scale of everything
		 */			
    	//if (resolutionFactor != 1d) graphics.scale(1d/resolutionFactor,1d/resolutionFactor);
        if(debug) System.out.println("imageableX="+pf.getImageableX()+"  imageableY="+pf.getImageableY()+"  resolutionFactor="+resolutionFactor+
        "  scale="+(1d/resolutionFactor));
        try
        {
			if (printLayerables != null)
            {
				render(graphics, printLayerables);  //use specialized renderer
			}
            else
            {  //try the default method of rendering
//				final boolean [] locked = { true };
//				panel.getRenderingManager().setRenderingMode(new Runnable() {
//					public void run() {
//						synchronized (locked) {
//							locked[0] = false;
//							locked.notify();
//						}
//					}
//				},RenderingManager.SINGLE_THREAD_QUEUE);
				//panel.getRenderingManager().setRenderingMode(         //*******
				//		RenderingManager.EXECUTE_ON_EVENT_THREAD);    //*******
				panel.getRenderingManager().renderAll();
				panel.getRenderingManager().copyTo(graphics);
//				while (locked[0]) locked.wait();
				//panel.getRenderingManager().setRenderingMode(     //********
				//		RenderingManager.INTERACTIVE);		    //********
			}
		}
        catch (Exception e)
        {
            e.printStackTrace();
            String message = (e.getMessage()==null) ? e.toString(): e.getMessage();
    		System.err.println(message); 
    		throw new PrinterException(message);
    	}
		/* tell the caller that this page is part of the printed document */
		//return PAGE_EXISTS;
	}

	   /**
	 * Modifies the styles of the passed layerable ArrayList to optimize them
	 * for vector printing
	 * @param printLayerables ArrayList
	 * @param removeThemeFills - remove fills from color themed layers
	 * @param removeBasicFill - remove fills from basic styles
	 * @return old Layer Style collections to restore later
	 */
   public static ArrayList optimizeForVectors(ArrayList printLayerables,
			boolean removeThemeFills, boolean removeBasicFill,
			boolean changeLineWidth, float lineWidth, boolean removeTransparency)
   {
    	if (!(removeThemeFills || removeBasicFill 
    			|| changeLineWidth || removeTransparency))
    		return null;
		ArrayList oldStyleList = new ArrayList(printLayerables.size());
		for (Iterator i = printLayerables.iterator(); i.hasNext();) {
			Object layerable = i.next();
			if (layerable instanceof Layer) {
				Layer layer = (Layer) layerable;
				final Collection oldStyles = layer.cloneStyles(); // copy
				oldStyleList.add(oldStyles); // save to restore later
				final Collection currentStyles = layer.getStyles();
				for (Iterator j = currentStyles.iterator(); j.hasNext();) {
					Object style = j.next();
					if (style instanceof BasicStyle) {
						BasicStyle basicStyle = (BasicStyle) style;
						if (removeTransparency)
							basicStyle.setAlpha(255); // 255 is opaque
						if (removeBasicFill)
							basicStyle.setRenderingFill(false);
						if (changeLineWidth)
							//basicStyle.setFractionalLineWidth(lineWidth);    //********
                            basicStyle.setLineWidth((int)lineWidth);
                    } else if (style instanceof ColorThemingStyle) {
						ColorThemingStyle themedStyle = (ColorThemingStyle) style;
						if (removeTransparency)
							themedStyle.setAlpha(255); // 255 is opaque
						Map attributeValueToBasicStyleMap = themedStyle
								.getAttributeValueToBasicStyleMap();
						for (Iterator k = attributeValueToBasicStyleMap
								.keySet().iterator(); k.hasNext();) {
							Object attribute = (Object) k.next();
							BasicStyle basicStyle = ((BasicStyle) 
									attributeValueToBasicStyleMap.get(attribute));
							if (removeThemeFills)
								basicStyle.setRenderingFill(false);
							if (changeLineWidth)
								//basicStyle.setFractionalLineWidth(lineWidth);   //*******
                                basicStyle.setLineWidth((int)lineWidth);
                        }
					}
				}
			}
		}
		return oldStyleList;
	}


	/**
	 * Disables double buffering on passed component by 
	 * disabling it in the current RepaintManager
	 * @param c Component to use
	 */
	public static void disableDoubleBuffering(Component c)
    {
		RepaintManager currentManager = RepaintManager.currentManager(c);
		currentManager.setDoubleBufferingEnabled(false);
	}

	/**
	 * Reenables double buffering on passed component
	 * using the current RepaintManager
	 * @param c Component to use
	 */
	public static void enableDoubleBuffering(Component c)
    {
		RepaintManager currentManager = RepaintManager.currentManager(c);
		currentManager.setDoubleBufferingEnabled(true);
	}

}
