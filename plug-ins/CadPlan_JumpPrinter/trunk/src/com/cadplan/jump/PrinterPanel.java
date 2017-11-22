
/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * Copyright (C) 2006 Cadplan
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
 */


package com.cadplan.jump;

import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.renderer.RenderingManager;
import com.vividsolutions.jump.workbench.ui.renderer.LayerRenderer;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.Viewport;
import com.vividsolutions.jump.workbench.ui.LayerPrinter;
import com.vividsolutions.jump.workbench.ui.LayerViewPanel;
import com.vividsolutions.jump.workbench.ui.LayerViewPanelContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.model.LayerManager;
import com.vividsolutions.jump.workbench.model.Category;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.ashs.jump.plugin.LayerPrinter2;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.Pageable;
import java.awt.geom.Point2D;
import java.text.DecimalFormat;
import java.util.*;

/**
 * User: geoff
 * Date: 22/12/2006
 * Time: 12:20:53
 * Copyright 2005 Geoffrey G Roy.
 */
public class PrinterPanel extends JPanel implements Printable, Pageable, Comparator
{
    boolean debug = false;

    double scale;
    int xsize = 1000;
    int ysize = 800;
    int xsizeFinal;
    int ysizeFinal;
    public StringBuffer sb = new StringBuffer();
    double drawingScale = 1.0;
    double drawingScaleFactor = 1.0;
    double pixelUnit = 72.0/25.4;  // pixel/mm on paper
    PlugInContext context;
    RenderingManager renderingManager;
    Rectangle visRect;
    boolean drawBorder= false;
    boolean drawScale = true;
    PageFormat pageFormat;
    int numPagesX;
    int numPagesY;
    int numPages;
    double originX;
    double originY;
    boolean resize = false;  // set to true to resize the JPanel, false forces scaling in paint()
    int xoff;
    int yoff;
    FurnitureTitle title;
    FurnitureScale scaleItem;
    FurnitureBorder border;
    Vector<FurnitureBorder> borders;
    FurnitureNorth north;
    FurnitureNote note;
    Vector<FurnitureNote> notes;
    FurnitureLegend legend;
    LayerLegend layerLegend;
    Vector<FurnitureImage> imageItems;
    boolean qualityOption = false;
    Envelope bounds;
    int printingMode = 2; // 0 - uses copyTo(), 1- uses LayerPrinter2, 2 - usesMapImagePrinter
    boolean printSinglePage;
    Vector<Furniture> itemsForPrinting;

    public PrinterPanel( PlugInContext context,  int xsize, int ysize, PageFormat pageFormat, double scale)
    {
        this.context = context;
        this.xsize = xsize;
        this.ysize = ysize;        
        this.xsizeFinal = xsize;
        this.ysizeFinal = ysize;
        this.pageFormat = pageFormat;
        this.scale = scale;

        Viewport viewPort = context.getLayerViewPanel().getViewport();
        Envelope envelope = viewPort.getEnvelopeInModelCoordinates();

        Rectangle rect = context.getLayerViewPanel().getBounds();
        if(debug) System.out.println("Drawing size: "+xsize+","+ysize+"\n");
        if(debug) System.out.println("Old bounds:"+context.getLayerViewPanel().getBounds().toString()+"\n");

        if( resize)
        {
            context.getLayerViewPanel().setBounds(rect.x,rect.y,xsize,ysize); // try to set bounds - fails
        }
            if(debug) System.out.println("New bounds:"+context.getLayerViewPanel().getBounds().toString()+"\n");

        try
        {
            viewPort.zoom(envelope);
        }
        catch(Exception ex)
        {

        }
        visRect = context.getLayerViewPanel().getVisibleRect();
        bounds = context.getLayerViewPanel().getViewport().getEnvelopeInModelCoordinates();

        Point2D.Double origin = (Point2D.Double) viewPort.getOriginInModelCoordinates();

        renderingManager = context.getLayerViewPanel().getRenderingManager();

        setPreferredSize(new Dimension(xsize, ysize));

        setBackground(Color.WHITE);
        if(debug) System.out.println("*****  Origin="+origin.toString()+"\n");

        drawingScale = (double)(xsizeFinal-xoff)/(double)visRect.width; // ratio of required size to actual size - temp fix

        if(debug) System.out.println("***** Draw scale= "+this.scale+"  ratio="+drawingScale+" xsize="+xsize+"  visRect.width="+visRect.width+"\n");

    }

    /**
     * sets the draw border option
     * @param border
     */
    public void setBorder(FurnitureBorder border)
    {
        this.border = border;
    }
    public void setBorders(Vector<FurnitureBorder> borders)
    {
        this.borders = borders;
    }

    /**
     * sets the draw scale option
     * @param drawScale
     */
    public void setDrawScale(boolean drawScale)
    {
        this.drawScale = drawScale;
    }
    
    public void setDrawingScaleFactor(double factor)
    {
    	drawingScaleFactor = factor;
    	drawingScale = drawingScale * factor;
    }

    /**
     * sets the post scale option (quality)
     * @param qualityOption
     */
    public void setQualityOption(boolean qualityOption)
    {
         this.qualityOption = qualityOption;
    }
    public void setPrintMode(int printingMode)
    {
        this.printingMode = printingMode;
    }
    public void setPrintSinglePage(boolean printSinglePage)
    {
    	this.printSinglePage = printSinglePage;
    }
    /**
     * sets the title item
     * @param title
     */
    public void setTitle(FurnitureTitle title)
    {
        this.title = title;
    }

    /**
     * sets the scale item
     * @param scaleItem
     */
    public void setScaleItem(FurnitureScale scaleItem)
    {
        this.scaleItem = scaleItem;
    }

    /**
     * sets the north item
     * @param north
     */
    public void setNorth(FurnitureNorth north)
    {
        this.north = north;
    }

    /**
     * sets the note item
     * @param note
     */
    public void setNote(FurnitureNote note)
    {
        this.note = note;
    }
    
    public void setNotes(Vector<FurnitureNote> notes)
    {
    	this.notes = notes;
    }

    public void setLegend(FurnitureLegend legend)
    {
        this.legend = legend;
    }

    public void setLayerLegend(LayerLegend legend)
    {
        this.layerLegend = legend;
    }
    
    public void setImages(Vector<FurnitureImage> imageItems)
    {
    	this.imageItems = imageItems;
    }
    /**
     * paints the image
     * @param gp
     */
    public void paint(Graphics gp)
    {
        //boolean debug = true;
        int xb = (int) ((double)bounds.getWidth()*1000.0*pixelUnit/scale); //xsize;
        int yb = (int) ((double)bounds.getHeight()*1000.0*pixelUnit/scale); //ysize;
        int xoffs = xoff;
        int yoffs = yoff;
        LayerViewPanel panel = null;
        if(debug)System.out.println("Drawing 1: xb="+xb+" yb="+yb+" xoffs="+xoffs+" yoffs="+yoffs);

        
        Graphics2D g = (Graphics2D) gp;
        g.setColor(Color.WHITE);
        g.fillRect(0,0,xb, yb);
        if(debug) System.out.println("Drawing 2: xb="+xb+" yb="+yb+" xoffs="+xoff+" yoffs="+yoff);
        if(debug) System.out.println("printing mode="+printingMode);
    //=========================================================================
    boolean test = true;
    //==========================================================================

//    if(printingMode == 3)
//    {
//        if(debug) System.out.println("Sky Printing mode");
//        try
//        {
//              //SkyPrint skyPrint = new SkyPrint();
//              //skyPrint.execute(context);
//             final Throwable[] throwable = new Throwable[] { null };
//             Envelope windowEnvelope = context.getLayerViewPanel().getViewport().getEnvelopeInModelCoordinates();
//             //LayerViewPanel panel = createLayerPanel(context.getLayerManager(), throwable);
//
//             //panel.getViewport().zoom(windowEnvelope);
//              panel = context.getLayerViewPanel();
//            //panel.setSize((int)(visRect.width*drawingScale),(int)(visRect.height*drawingScale));
//            if(debug) System.out.println("Creating panel name:"+panel.getName()+"  size:"+panel.getSize());
//              SkyPrinterDriver.disableDoubleBuffering(panel);
//
//		     Geometry fence = context.getLayerViewPanel().getFence();
//
//	         ArrayList printLayerables = new ArrayList(context.getLayerManager().getLayerables(Layerable.class));  //includes Layers and WMSLayers
//		     Collections.reverse(printLayerables);  //print bottom to top
//
//            //Create and set up a custom PrinterDriver for use by PrinterJob
//            SkyPrinterDriver printerDriver = new SkyPrinterDriver(context, panel);
//            //printerDriver.setPrintBorder(true);
//            double dscale = 1000.0*pixelUnit/scale;
//            if(debug) System.out.println("dscale="+dscale);
//            printerDriver.setResolutionFactor(1.0/dscale);
//
//            //optimize layer styles for print type
//		   ArrayList oldStyleList = printerDriver.optimizeForVectors(printLayerables, false, false, true,1.0f,false);
//	//				removeThemeFills, removeBasicFills,
//	//				changeLineWidth, lineWidth, removeTransparency);
//            printerDriver.setPrintLayerables(printLayerables);
//            g.translate(xoff,yoff);
//            g.scale(drawingScale,drawingScale);
//            printerDriver.paintLayers(gp, pageFormat);
//            g.scale(1.0/drawingScale, 1.0/drawingScale);
//            g.translate(-xoff,-yoff);
//
//
//            if (oldStyleList != null)  // restore the original styles
//            {
//                boolean wasFiringEvents = panel.getLayerManager().isFiringEvents();
//                panel.getLayerManager().setFiringEvents(false);
//                int j = 0;
//                for (Iterator i = printLayerables.iterator(); i.hasNext();)
//                {
//                    Object layerable = i.next();
//                    if (layerable instanceof Layer)
//                    {
//                        Layer layer = (Layer) layerable;
//                        layer.setStyles( (Collection) oldStyleList.get(j++));
//                    }
//			}
//			panel.getLayerManager().setFiringEvents(wasFiringEvents);
//		}
//        }
//        catch (Exception ex)
//        {
//            System.out.println("ERROR in SkyPrint: "+ex );
//            ex.printStackTrace();
//        }
//    }
//
//    else

        //------------------------------------------------------------
        //  External printing renderer
        //------------------------------------------------------------

        //System.out.println("\nPrinting Mode ="+printingMode);
        //System.out.println("Trans begin:"+g.getTransform().getTranslateX()+","+g.getTransform().getTranslateX()+" scale:"+g.getTransform().getScaleX());
    if(printingMode == 2) // External option
    {
       double dscale = 1000.0*pixelUnit/scale;
       g.setClip(new Rectangle(xoff,yoff,(int)(visRect.width*drawingScale),(int)(visRect.height*drawingScale)));
       MapImagePrinter mapPrinter = new MapImagePrinter(context, xb, yb, dscale);
       mapPrinter.createImage(g, xoff, yoff);
       int xp = xoff;// - (int) Math.round(bounds.getMinX()/dscale);
       int yp = yoff;// + (int) Math.round(bounds.getMinY()/dscale);
        //System.out.println("bounds: "+bounds.getMinX()+","+bounds.getMinY()+","+bounds.getMaxX()+","+bounds.getMaxY()+"  dscale="+dscale);
        //System.out.println("Painting image at: "+xp+","+yp+"  xoff="+xoff+"  yoff="+yoff);
       //g.drawImage(bimage, xp, yp, null);
        //if(g.getClip() != null) System.out.println("****** graphic bounds after createImage: "+gp.getClip().toString());
        g.setClip(null);
     }
    //------------------------------------------------------------
    // Std core option using existing LayerViewPanel - with scaling
    //------------------------------------------------------------
    else if(printingMode == 1)  // quality option
     {
           if(debug) System.out.println("Quality mode:");
           java.util.List layerCollection = context.getLayerViewPanel().getLayerManager().getVisibleLayers(true);
            g.translate(xoff,yoff);

            //g.setClip(new Rectangle(0,0,(int)(visRect.width*drawingScale),(int)(visRect.height*drawingScale)));
            g.scale(drawingScale, drawingScale);

           try
           {
               final Throwable[] throwable = new Throwable[] { null };
               LayerViewPanel mpanel = context.getLayerViewPanel();
               renderingManager.copyTo(g);
           }
           catch (IllegalArgumentException ex)
           {
               System.out.println("Printing ERROR: "+ex);
           }
           catch (UnsupportedOperationException ex)
           {
               System.out.println("Printing ERROR: "+ex);
           }
           catch( Exception ex)
           {
               System.out.println("Printing ERROR: "+ex);
           }
            boolean drawing = true;
            while(drawing)
            {
                try
                {
                    Thread.sleep(200);
                    drawing = false;

                    int nt = renderingManager.getDefaultRendererThreadQueue().getRunningThreads();
                    if(nt > 0)
                    {
                        drawing = true;
                    }
                    if(debug) System.out.println("Waiting...drawing:"+drawing+"  nt="+nt);
                }
                catch (InterruptedException ex)
                {

                }
            }

            g.setStroke(new BasicStroke());
            g.scale(1.0/drawingScale, 1.0/drawingScale);
            g.setClip(null);
            g.translate(-xoff,-yoff);

        }
    //------------------------------------------------------------------------------
    //  ISA Improved core renderer using a new LayerViewPanel - scaled to suit printer
    //------------------------------------------------------------------------------
     else if(printingMode == 0)  // new isa quality option
     {
           if(debug) System.out.println("ISA Quality mode:");


            g.translate(xoff,yoff);

            //g.setClip(new Rectangle(0,0,(int)(visRect.width*drawingScale),(int)(visRect.height*drawingScale)));

           try
           {
               final Throwable[] throwable = new Throwable[] { null };
               Envelope windowEnvelope = context.getLayerViewPanel().getViewport().getEnvelopeInModelCoordinates();
               panel = createLayerPanel(context.getLayerManager(), throwable);
            //System.out.println("windowEnvelope:"+windowEnvelope);
            //System.out.println("Visible rect width="+visRect.width+"  height:"+visRect.height+"  drawingScale:"+drawingScale);

             //LayerViewPanel panel = context.getLayerViewPanel();
               panel.setSize((int)(visRect.width*drawingScale),(int)(visRect.height*drawingScale));
            //panel.setSize((int)(visRect.width),(int)(visRect.height));
               panel.getViewport().zoom(windowEnvelope);

               java.util.List layerCollection = context.getLayerViewPanel().getLayerManager().getLayers();
               java.util.List allCategories = context.getLayerViewPanel().getLayerManager().getCategories();
               if(debug) System.out.println("Number of Categories to print: "+allCategories.size());

               for (int j = allCategories.size()-1; j >= 0; j--)
               {
                   Category cat = (Category) allCategories.get(j); //j.next();
                   if(debug) System.out.println("Category "+j+":"+cat.getName());
                   java.util.List allLayerables =cat.getLayerables();
                   if(debug) System.out.println("Number of layers:"+allLayerables.size());

                   for(int i=allLayerables.size()-1; i >= 0; i--)
                   {

                       Layerable layerable = (Layerable)allLayerables.get(i); // i.next();
                       if(layerable.isVisible())
                       {
                           //System.out.println("Layer "+i+":"+layerable.getName());
                           if(layerable instanceof Layer)      // for std layers
                           {
                               if(debug) System.out.println("Rendering Normal layer: "+layerable.getName());
                               Layer layer = (Layer) layerable;
                               LayerRenderer renderer = new LayerRenderer(layer, panel);
                               BasicStyle style = layer.getBasicStyle();
                               int alpha = style.getAlpha();
                               //style.setAlpha(alpha);
                               if(debug) System.out.println("   alpha="+alpha);
                               Runnable runnable = renderer.createRunnable();
                               //if(runnable == null) continue;
                               
                               renderer.getSimpleFeatureCollectionRenderer().copyTo(g);
                               //renderer.copyTo(g);


                                boolean drawing = true;
                                while(drawing)  // wait until rendering is compplete
                                {

                                    try
                                    {
                                        Thread.sleep(200);
                                        drawing = false;
                                        int nt = panel.getRenderingManager().getDefaultRendererThreadQueue().getRunningThreads();

                                        if(nt > 0)
                                        {
                                            drawing = true;
                                        }
                                        if(debug) System.out.println("Waiting...drawing:"+drawing+"  nt="+nt);
                                    }
                                    catch (InterruptedException ex)
                                    {

                                    }
                                }
                                renderer.clearImageCache();
                           }
                           else    // for all others (eg images)
                           {
                               boolean scaleGraphics = true;
                               if(layerable instanceof com.vividsolutions.jump.workbench.model.WMSLayer) scaleGraphics = false;
                               //com.vividsolutions.jump.workbench.ui.renderer.
                               if(debug) System.out.println("Rendering Image layer: "+layerable.getName()+"  type:"+layerable.getClass().toString());

                               MyRenderer myRenderer = new MyRenderer(panel);
                               com.vividsolutions.jump.workbench.ui.renderer.Renderer renderer = myRenderer.createMyRenderer(layerable);
                               //com.vividsolutions.jump.workbench.ui.renderer.Renderer renderer = panel.getRenderingManager().getRenderer(layerable);
                               Runnable runnable = renderer.createRunnable();
                               if(runnable == null) continue;
                               runnable.run();

                               if(scaleGraphics) g.scale(drawingScale, drawingScale);
                               renderer.copyTo(g);
                               if(scaleGraphics) g.scale(1.0/drawingScale, 1.0/drawingScale);




                                boolean drawing = true;
                                while(drawing)  // wait until rendering is compplete
                                {

                                    try
                                    {
                                        Thread.sleep(200);
                                        drawing = false;
                                        int nt = myRenderer.getDefaultRendererThreadQueue().getRunningThreads();
                                        //int nt = panel.getRenderingManager().getDefaultRendererThreadQueue().getRunningThreads();

                                        if(nt > 0)
                                        {
                                            drawing = true;
                                        }
                                        if(debug) System.out.println("Waiting...drawing:"+drawing+"  nt="+nt);
                                    }
                                    catch (InterruptedException ex)
                                    {

                                    }
                                }
                                renderer.clearImageCache();
                           }


                        }
                   }
               }
           }
           catch (IllegalArgumentException ex)
           {
               System.out.println("Printing ERROR: "+ex);
           }
           catch (UnsupportedOperationException ex)
           {
               System.out.println("Printing ERROR: "+ex);
           }
           catch( Exception ex)
           {
               System.out.println("Printing ERROR: "+ex);
           }


            g.setStroke(new BasicStroke());
            //g.scale(1.0/drawingScale, 1.0/drawingScale);
            g.setClip(null);
            g.translate(-xoff,-yoff);
            panel.dispose();
            panel = null;

        }
//        else    // accuracy option mode = 1
//        {
//            Viewport viewPort = context.getLayerViewPanel().getViewport();
//            Envelope bounds = viewPort.getEnvelopeInModelCoordinates();
//            visRect = context.getLayerViewPanel().getVisibleRect();
////            double printRatio = (double)(xb-xoffs)/(double) visRect.width;
//            int scaleRatio = 1;
//
////            LayerPrinter printer = new LayerPrinter();
////            Envelope env = context.getLayerViewPanel().getViewport().getEnvelopeInModelCoordinates();
////           try
////           {
////             Image image  = printer.print(context.getLayerManager().getLayers(), env, xb );
////             g.drawImage(image,xoff,yoff,null);
////           }
////           catch (Exception ex)
////           {
////              JOptionPane.showMessageDialog(null,"PRINTING ERROR: "+ ex,"Error...",JOptionPane.ERROR_MESSAGE);
////           }
//
//
//            BufferedImage image = null;
//            java.util.List layerCollection = context.getLayerViewPanel().getLayerManager().getVisibleLayers(true);
//            LayerPrinter2 layerPrinter= new LayerPrinter2();
//            try
//            {
//                image = layerPrinter.print(layerCollection,bounds,xb);
//            }
//            catch(Exception ex)
//            {
//                 JOptionPane.showMessageDialog(null,"ERROR: "+ ex,"Error...",JOptionPane.ERROR_MESSAGE);
//            }
//            g.drawImage(image,xoff,yoff,null);
//        }

        //if(gp.getClip() != null) System.out.println("****** graphic bounds: "+gp.getClip().toString());
        g.setClip(null);

        //System.out.println("Trans end  :"+g.getTransform().getTranslateX()+","+g.getTransform().getTranslateX()+" scale:"+g.getTransform().getScaleX());


        itemsForPrinting = new Vector<Furniture>();
        if(border.show) itemsForPrinting.addElement(border);
        for(int i=0; i < borders.size(); i++) if(borders.elementAt(i).show) itemsForPrinting.addElement(borders.elementAt(i));
        if(title.show) itemsForPrinting.addElement(title);
        if(scaleItem.show)itemsForPrinting.addElement(scaleItem);
        if(north.show)itemsForPrinting.addElement(north);
        for(int i=0; i < notes.size(); i++) if(notes.elementAt(i).show) itemsForPrinting.addElement(notes.elementAt(i));
        if(legend.show)itemsForPrinting.addElement(legend);
        if(layerLegend.show)itemsForPrinting.addElement(layerLegend);
        for(int i=0; i < imageItems.size(); i++) if(imageItems.elementAt(i).show) itemsForPrinting.addElement(imageItems.elementAt(i));
        //System.out.println("Number of printable items: "+ itemsForPrinting.size());
        Collections.sort(itemsForPrinting, this);
        for(int i=0; i < itemsForPrinting.size(); i++)
        {
        	Furniture item = itemsForPrinting.elementAt(i);
        	//System.out.println("Painting: "+item.layerNumber+":"+item.toString());
        	if(item instanceof FurnitureBorder) ((FurnitureBorder)item).paint(gp,-1.0, 1.0);	
        	else if(item instanceof FurnitureTitle) ((FurnitureTitle)item).paint(gp,-1.0);
        	else if(item instanceof FurnitureScale) ((FurnitureScale)item).paint(gp,-1.0);
        	else if(item instanceof FurnitureNorth) ((FurnitureNorth)item).paint(gp,-1.0);
        	else if(item instanceof FurnitureNote) ((FurnitureNote)item).paint(gp,-1.0);
        	else if(item instanceof FurnitureLegend) ((FurnitureLegend)item).paint(gp,-1.0, 1.0, printingMode);
        	else if(item instanceof LayerLegend) ((LayerLegend)item).paint(gp,-1.0, 1.0, printingMode);
        	else if(item instanceof FurnitureImage) ((FurnitureImage)item).paint(gp,-1.0, 1.0);
        	
        }
/*
        if(border.show)
        {
            //draw border
             if(debug) System.out.println("printing border at "+border.location+"  clip: "+gp.getClip());
            border.paint(gp,-1.0, 1.0);
        }
        for(int i=0; i < borders.size(); i++)
        {
        	FurnitureBorder aborder = borders.elementAt(i);
        	if(aborder.show) aborder.paint(gp, -1.0, 1.0);
        	
        }


        if(title.show)
        {
            if(debug) System.out.println("printing title at "+title.location);

             title.paint(gp,-1.0);
        }
        if(scaleItem.show)
        {
            if(debug) System.out.println("printing scale at "+scaleItem.location);
            scaleItem.paint(gp,-1.0);
        }

        if(north.show)
        {
            if(debug) System.out.println("printing north at "+north.location);
            north.paint(gp,-1.0);
        }
        //if(note.show)
        //{
        //    note.paint(gp,-1.0);
        //}
        for(int i=0; i < notes.size(); i++)
        {
        	FurnitureNote note = notes.elementAt(i);
        	if(note.show) note.paint(gp, -1.0);
        }
         if(legend.show)
        {
            legend.paint(gp,-1.0, 1.0, printingMode);
        }
        if(layerLegend.show)
        {
            layerLegend.paint(gp,-1.0, 1.0, printingMode);

        }
        */
    }
    
    public int compare(Object item1, Object item2)
    {
    	if(((Furniture)item1).layerNumber < ((Furniture)item2).layerNumber)return -1;
    	else if(((Furniture)item1).layerNumber > ((Furniture)item2).layerNumber) return 1;
    	else return 0;
    }

    private LayerViewPanel createLayerPanel(LayerManager layerManager, final Throwable[] throwable)
    {
        LayerViewPanel layerViewPanel = new LayerViewPanel(layerManager,
                new LayerViewPanelContext()
                {
                    public void setStatusMessage(String message)
                    {

                    }
                    public void warnUser(String warninmg)
                    {

                    }
                    public void handleThrowable(Throwable t)
                    {
                        throwable[0] = t;
                    }
                });
        
        return layerViewPanel;
    }
    /**
     * gets number of pages to print
     * @return  number of pages
     */
     public int getNumberOfPages()
    {
         numPagesX = (int) ( (xsize + Math.round(pageFormat.getImageableWidth())  - 1)/pageFormat.getImageableWidth());
         numPagesY = (int) ( (ysize + Math.round(pageFormat.getImageableHeight()) - 1)/pageFormat.getImageableHeight());
         numPages = numPagesX * numPagesY;
        //System.out.println("Number of pages to print: "+numPages);
         return numPages;
    }


    /**
     * gets the current PageFormat
     * @param pageIndex
     * @return  the PageFormat
     */
    public PageFormat getPageFormat(int pageIndex)
    {
         return pageFormat;
    }

    /**
     * sets the PageFormat
     * @param pageFormat
     */
    public void setPageFormat(PageFormat pageFormat)
    {
         this.pageFormat = pageFormat;
    }

    public void setOffsets(Point2D.Double point)
    {
        //boolean debug = true;
         xoff = (int) (point.x*1000.0*pixelUnit/scale);
         yoff = (int) (point.y*1000.0*pixelUnit/scale);

         if(!resize)
         {
            //** fix until panel can be re-sized
//            xoff = (int)(xoff/drawingScale);
//            yoff = (int)(yoff/drawingScale);
//            xsize = xoff + visRect.width;
//            ysize = yoff + visRect.height;
         }
        drawingScale = (double)bounds.getWidth()*1000.0*pixelUnit/scale/( (double)visRect.width); // ratio of required size to actual size - temp fix
 
       if(debug) System.out.println("Offsets: "+point+"  drawingScale="+drawingScale+" xoff="+xoff+"  yoff=" + yoff+" xise="+xsize+" ysize="+ysize+"  xsizeFinal="+xsizeFinal+"\n");
    }

    /**
     * gets the Printable Object
     * @param pageIndex
     * @return   the Printable object
     * @throws IndexOutOfBoundsException
     */
    public Printable getPrintable(int pageIndex) throws IndexOutOfBoundsException
    {
        if(pageIndex >= numPages)
        {
            throw new IndexOutOfBoundsException();
        }
        originX = (pageIndex % numPagesX) * pageFormat.getImageableWidth();
        originY = (pageIndex / numPagesX) * pageFormat.getImageableHeight();
        if(debug) System.out.println("getPrintable: page: "+pageIndex+" oX="+originX+" oY="+originY+"\n");
        return this;
    }

    /**
     * prints the graphic
     * @param g
     * @param pageFormat
     * @param pageIndex
     * @return PAGE_EXISTS
     */
    public int print(Graphics g, PageFormat pageFormat, int pageIndex)
    {
        Graphics2D g2 = (Graphics2D) g;
        g2.translate(pageFormat.getImageableX(), pageFormat.getImageableY());
        g2.translate(-originX, -originY);
        paint(g2);
        //System.out.println("Printing page: "+pageIndex);
        if(printSinglePage && pageIndex  > 0) return NO_SUCH_PAGE;
        return PAGE_EXISTS;
    }

	

}
class MyRenderer extends RenderingManager
   {
        public MyRenderer(LayerViewPanel panel)
       {
           super(panel);
       }

       public com.vividsolutions.jump.workbench.ui.renderer.Renderer createMyRenderer(Layerable layerable)
       {
           return createRenderer(layerable);
       }
   }