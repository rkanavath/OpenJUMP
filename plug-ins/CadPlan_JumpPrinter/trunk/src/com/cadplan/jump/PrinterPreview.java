
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
import com.vividsolutions.jump.workbench.ui.Viewport;
import com.vividsolutions.jts.geom.Envelope;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import java.io.File;
import java.text.DecimalFormat;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;

/**
 * User: geoff
 * Date: 22/12/2006
 * Time: 12:20:53
 * Copyright 2005 Geoffrey G Roy.
 */
public class PrinterPreview extends JPanel implements MouseListener, MouseMotionListener, Comparator
{
    boolean debug = false;

    double scale;
    double viewScale;
    int xsize = 1000;
    int ysize = 800;
    int xoff = 0;
    int yoff = 0;
    double xmin = Double.MAX_VALUE, xmax = Double.MIN_VALUE, ymin = Double.MAX_VALUE, ymax = Double.MIN_VALUE;
    public StringBuffer sb = new StringBuffer();
    double paperWidth = 200.0;
    double paperHeight = 287.0;
    double paperWidthMeasure;     // size of paper in world units
    double paperHeightMeasure;
    double drawingScale = 1.0;
    double drawingWidth;         // in world units
    double drawingHeight;
    double pixelUnit = 72.0/25.4;  // pixel/mm on paper
    
    java.util.List layers;
    PlugInContext context;
    RenderingManager renderingManager;
    Rectangle visRect;

    boolean drawScale = true;
    Rectangle mapRect;
    int xlast = -1;
    int ylast = -1;
    boolean initDrag = true;
    int xd;
    int yd;
    boolean dragging = false;
    boolean draggingTitle = false;
    boolean draggingScale = false;
    boolean draggingNorth = false;
    boolean draggingNote = false;
    boolean draggingBorder = false;
    boolean draggingBorderBR = false;
    boolean draggingBorderTL = false;
    boolean draggingImageBR = false;
    boolean draggingImageTL = false;
    boolean draggingLegend = false;
    boolean draggingLayerLegend = false;
    boolean draggingImage = false;
    boolean panning = false;
    int borderSpace = 10;
    PrinterSetup printerSetup;
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
    double furnitureScale;
    double globalScale = 1.0;
    int xpan = 0;
    int ypan = 0;
    boolean paperTooSmall = false;
    int selectedNote = 0;
    int selectedBorder = 0;
    int selectedImage = 0;
    boolean printSinglePage = false;
    Vector<Furniture> itemsForPrinting;
    



    public PrinterPreview( PlugInContext context)
    {
        this.context = context;
        renderingManager = context.getLayerViewPanel().getRenderingManager();
        try
        {
            Viewport viewPort = context.getLayerViewPanel().getViewport();
            Envelope bounds = viewPort.getEnvelopeInModelCoordinates();
            if(debug) sb.append("Envelope: "+bounds.toString()+"\n");
            visRect = context.getLayerViewPanel().getVisibleRect();
            if(debug) sb.append("Viewport: "+viewPort.toString()+"  visRect: "+visRect.toString()+"\n");
            Point2D.Double origin = (Point2D.Double) viewPort.getOriginInModelCoordinates();
            if(debug) sb.append("origin:"+origin.x+","+origin.y+"\n");

            xmin = bounds.getMinX();
            ymin = bounds.getMinY();
            xmax = bounds.getMaxX();
            ymax = bounds.getMaxY();

            this.xsize = visRect.width;    // make the preview panel the same size as the LayerViewPanel
            this.ysize = visRect.height;
        }
         catch(Exception ex)
        {
        }

        setPreferredSize(new Dimension(this.xsize, this.ysize));
        mapRect = new Rectangle(xoff, yoff ,visRect.width,visRect.height);
        if(debug) sb.append("mapRect:"+mapRect.toString()+"\n");
        setBackground(Color.WHITE);
        viewScale = 1.0/context.getLayerViewPanel().getViewport().getScale();
        Point2D origin = context.getLayerViewPanel().getViewport().getOriginInModelCoordinates();
        if(debug) sb.append("***** View scale= "+viewScale+" origin="+origin.toString()+"\n");
        scale = viewScale;
        setPaper(450,690.0,1000.0);  // inital guess only

        addMouseListener(this);
        addMouseMotionListener(this);

       
        
        repaint(100);
        //renderingManager.setPaintingEnabled(false);

         File projectFile = context.getTask().getProjectFile();
         if(debug) sb.append("ProjectFile: "+projectFile.getParent()+"\n");
    }

    public void setPrintSinglePage(boolean printSinglePage)
    {
    	this.printSinglePage = printSinglePage;
    }
    /**
     * Computes the scale to fit drawing on one page of paper
     * @param width
     * @param height
     * @return the scale
     */
     public double getAutoScale(double width, double height)
    {
//        Dimension size = getPrintSize();
//        double scalex = size.width*drawingScale/(width);
//        double scaley = size.height*drawingScale/(height);
        double x = ((xmax-xmin)*1000.0*pixelUnit);
        double y =  ((ymax-ymin)*1000.0*pixelUnit);
        double wx = width;
        double wy = height;
        //System.out.println("Autoscale  pre: wx="+wx+"  wy="+wy+ " viewScale="+viewScale);
        if(border != null && border.show && !border.fixed)
        {
            wx = wx - borderSpace;
            wy = wy - borderSpace;
        }
       //System.out.println("Autoscale post: wx="+wx+"  wy="+wy);

        double scalex = x/wx;
        double scaley = y/wy;
        return Math.max(scalex, scaley);
    }
     
     public boolean getPrintSinglePage()
     {
    	 return printSinglePage;
     }

    public Dimension getImageSize()
    {
        int width = (int) (paperWidth*pixelUnit);
        int height = (int) (paperHeight*pixelUnit);
        return new Dimension(width, height);
    }
    /**
     * gets the size of the printer panel to draw map at the required scale on paper
     * in world coordinates
     * @return the panel size
     */
    public Dimension getPrintSize()
    {
        boolean debug = false;
//        int width  = (int) (((double)xoff*viewScale + xmax-xmin)*1000.0*pixelUnit/drawingScale);
//        int height = (int) (((double)yoff*viewScale + ymax-ymin)*1000.0*pixelUnit/drawingScale);

        int width  = (int) (((double)xoff*viewScale + xmax-xmin)*1000.0*pixelUnit/drawingScale);
        int height = (int) (((double)yoff*viewScale + ymax-ymin)*1000.0*pixelUnit/drawingScale);
        if(debug) System.out.println("============================== Print Size\nxoff="+xoff+"  yoff="+yoff);
        if(debug) System.out.println("image: width="+width+"  height="+height);


        if(title != null && title.show)
        {
            if(title.location.x+title.location.width > width) width = title.location.x+title.location.width;
            if(title.location.y+title.location.height > height) height = title.location.y+title.location.height;
        }
        if(north != null && north.show)
        {
            if(north.location.x+north.location.width > width) width = north.location.x+north.location.width;
            if(north.location.y+north.location.height > height) height = north.location.y+north.location.height;
        }
        if(note != null && note.show)
        {
            if(note.location.x+note.location.width > width) width = note.location.x+note.location.width;
            if(note.location.y+note.location.height > height) height = note.location.y+note.location.height;
        }
        if(notes != null) for(int i=0; i < notes.size(); i++)
        {
        	FurnitureNote anote = notes.elementAt(i);
        	if(anote.location.x+anote.location.width > width) width = anote.location.x+anote.location.width;
            if(anote.location.y+anote.location.height > height) height = anote.location.y+anote.location.height;
        }
        
        if(borders != null) for(int i=0; i < borders.size(); i++)
        {
        	FurnitureBorder aborder = borders.elementAt(i);
        	if(aborder.location.x+aborder.location.width > width) width = aborder.location.x+aborder.location.width;
            if(aborder.location.y+aborder.location.height > height) height = aborder.location.y+aborder.location.height;
        }
        
        if(scaleItem != null && scaleItem.show)
        {
            if(scaleItem != null && scaleItem.show) scaleItem.resizeScale(furnitureScale);
            if(scaleItem.location.x+scaleItem.location.width > width) width = scaleItem.location.x+scaleItem.location.width;
            if(scaleItem.location.y+scaleItem.location.height > height) height = scaleItem.location.y+scaleItem.location.height;
        }
        if(legend != null && legend.show)
        {

            if(legend.location.x+legend.location.width > width) width = legend.location.x+legend.location.width;
            if(legend.location.y+legend.location.height > height) height = legend.location.y+legend.location.height;
        }
        if(layerLegend != null && layerLegend.show)
        {

            if(layerLegend.location.x+layerLegend.location.width > width) width = layerLegend.location.x+layerLegend.location.width;
            if(layerLegend.location.y+layerLegend.location.height > height) height = layerLegend.location.y+layerLegend.location.height;
        }
        if(imageItems != null) for(int i=0; i < imageItems.size(); i++)
        {
        	FurnitureImage imageItem = imageItems.elementAt(i);
        	if(imageItem.location.x+imageItem.location.width > width) width = imageItem.location.x+imageItem.location.width;
            if(imageItem.location.y+imageItem.location.height > height) height = imageItem.location.y+imageItem.location.height;
        }

        width = width +borderSpace;  // add a little space on bottom/right edges
        height = height+borderSpace;
        border.setBorder(0,0,width, height, false);

        if(border != null && border.show)
        {
            
            if(border.location.x+border.location.width > width) width = border.location.x+border.location.width;
            if(border.location.y+border.location.height > height) height = border.location.y+border.location.height;
        }


        if(debug) System.out.println("Panel width="+getWidth()+"  height="+getHeight()+" viewScale="+viewScale);
        if(debug) System.out.println("Printing size: width="+width+"  height="+height+ " scale="+drawingScale);
        //System.out.println("Border size:"+border.location);
        return new Dimension((int)Math.round(width),(int)Math.round(height));
    }


    /**
     * sets the paper size properties
     * @param width
     * @param height
     * @param scale
     */
    public void setPaper(double width, double height, double scale)
    {
    	 //boolean debug = true;
         paperWidth = width/pixelUnit;
         paperHeight = height/pixelUnit;
         drawingScale = scale;
         paperWidthMeasure = paperWidth * scale / 1000.0;
         paperHeightMeasure = paperHeight * scale / 1000.0;
         
         //System.out.println("width="+width+"  height="+height+"  scale="+scale+","+this.scale);
         
         if(debug)
         {
             sb.append("\nSetPaper: paperWidth="+paperWidth+" paperHeight="+paperHeight+" drawingScale="+
             drawingScale+" paperWidthMeasure="+paperWidthMeasure+" paperHeightMeasure="+paperHeightMeasure+
                 " pixelUnit="+pixelUnit+" scale="+this.scale+"\n");
         }
         if(paperWidthMeasure/this.scale < 5 || paperHeightMeasure/this.scale < 5)
         {
             paperTooSmall = true;
            String message = "Page format has not been set correctly, or scale is too small to show pages:\n"+
            "        scale="+scale+" width="+paperWidthMeasure+" height="+paperHeightMeasure;
            //System.out.println("message: "+message);
            if(debug) sb.append("\nmessage: "+message);
            //JOptionPane.showMessageDialog(this,message ,"Warning...",JOptionPane.WARNING_MESSAGE);

         }
         else paperTooSmall = false;
    }

    /**
     * gets the north furniture
     * @return
     */
    public FurnitureNorth getNorth()
    {
        return north;
    }

    public void setNorth(FurnitureNorth north)
    {
        this.north = north;
    }
    /**
     * gets the FurnitureTitle
     * @return
     */
    public FurnitureTitle getTitle()
    {
        return title;
    }

    /**
     * gets the scale furniture item
     * @return
     */
    public FurnitureScale getScaleItem()
    {
        return scaleItem;
    }
    /**
     * gets the draw border option
     */
    public FurnitureBorder getBorderItem()
    {
        return border;
    }
    /**
     * sets the draw border option
     * @param border
     */
    public void setBorder(FurnitureBorder border)
    {
        this.border = border;
        this.border.setBorder(border.location.x, border.location.y,getPrintSize().width, getPrintSize().height, false);

    }
    
    public Vector<FurnitureBorder> getBorders()
    {
    	return borders;
    }
    public void setBorders(Vector<FurnitureBorder> borders)
    {
    	this.borders = borders;
    	
    }

    /**
     * sets the title
     * @param title
     */
    public void setTitle(FurnitureTitle title)
    {
        this.title = title;
    }

    /**
     * sets the scale furniture item
     * @param scaleItem
     */
    public void setScaleItem(FurnitureScale scaleItem)
    {
        this.scaleItem = scaleItem;
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

    /**
     * gets the note item
     * @return
     */
    public FurnitureNote getNote()
    {
        return note;
    }
    public Vector<FurnitureNote> getNotes()
    {
    	return notes;
    }

    public void setLegend(FurnitureLegend legend)
    {
        this.legend = legend;
    }

    public void setLayerLegend(LayerLegend layerLegend)
    {
        this.layerLegend = layerLegend;
    }

    public LayerLegend getLayerLegend()
    {
        return layerLegend;
    }
    public FurnitureLegend getLegend()
    {
        return legend;
    }
    public void setImages(Vector<FurnitureImage> images)
    {
    	this.imageItems = images;
    }
    public Vector<FurnitureImage> getImages()
    {
    	return imageItems;
    }
    /**
     * gets the draw scale option
     */
    public boolean getDrawScale()
    {
        return drawScale;
    }
    /**
     * sets the draw scale option
     * @param drawScale
     */
    public void setDrawScale(boolean drawScale)
    {
        this.drawScale = drawScale;
    }

    /**
     * gets the map offsets on the paper
     * @return a Point
     */
    public Point2D.Double getOffsets()
    {
         return new Point2D.Double(((double)xoff*viewScale),((double)yoff*viewScale));
    }

    /**
     * sets the offsets for the mao
     * @param pageOffset
     */
    public void setOffsets(Point2D.Double pageOffset)
    {
        xoff = (int) (pageOffset.x/viewScale);
        yoff = (int) (pageOffset.y/viewScale);
        mapRect = new Rectangle(xoff, yoff, visRect.width, visRect.height);
    }

    /**
     * paints the panel with the map and paper page boundaries
     * @param gp
     */
    public void paint(Graphics gp)
    {
        Graphics2D g = (Graphics2D) gp;
        g.setColor(Color.WHITE);
        if(printSinglePage)g.setColor(Color.LIGHT_GRAY);
        g.fillRect(0,0,xsize,ysize);
        g.setColor(Color.WHITE);
        xsize = getSize().width;
        ysize = getSize().height;
         
        getPrintSize();
        
        //System.out.println("Print single page: "+printSinglePage);
        g.translate(xpan,ypan);
        g.scale(globalScale, globalScale);
        
        if(!printSinglePage)
        {
	        g.fillRect(0,0,xsize,ysize);
	        g.setClip(0,0,xsize, ysize);
        }
        else
        {
        	g.fillRect(0,0,(int) (paperWidthMeasure/scale),(int) (paperHeightMeasure/scale));
        	g.setClip(0,0,(int) (paperWidthMeasure/scale), (int) (paperHeightMeasure/scale));
        }
        //System.out.println("Bounds: x="+xsize+" y="+ysize+"  paper x="+(int) (paperWidthMeasure/scale)+" y="+(int) (paperHeightMeasure/scale));

        
        g.translate(xoff,yoff);
        int dw = (int)(visRect.width);
        int dh = (int)(visRect.height);
        //System.out.println("Clipping: dw="+dw+"  dh="+dh+"  xpan="+xpan+"  ypan="+ypan+"  globalScale="+globalScale);
        if(dh > (getHeight()-ypan)/globalScale -yoff) dh = (int)((getHeight()-ypan)/globalScale)-yoff;
        //if(dw > getWidth()) dw = getWidth();
        //System.out.println("Modified: height="+getHeight()+ "  dw="+dw+"  dh="+dh);
        g.setClip(new Rectangle(0, 0 ,dw, dh));

        renderingManager.copyTo(g);
        renderingManager.renderAll();
        g.setStroke(new BasicStroke());
        g.setClip(null);
        g.translate(-xoff,-yoff);


        g.setClip(0,0,(int)((getWidth()-xpan)/globalScale), (int)((getHeight()-ypan)/globalScale));
        // draw map boundary
        g.setColor(Furniture.boundsColor);
        g.setStroke(Furniture.boundsStroke);
        gp.drawRect(mapRect.x,  mapRect.y, mapRect.width, mapRect.height);
        g.setStroke(Furniture.normalStroke);
        furnitureScale =  (1.0/viewScale)/(pixelUnit*1000.0/drawingScale);

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
        	if(item instanceof FurnitureBorder) 
        	{
        		if(border.fixed) border.setBorder(item.location.x, item.location.y,getPrintSize().width, getPrintSize().height, false);
        		((FurnitureBorder)item).paint(gp,furnitureScale, globalScale);	
        		//System.out.println("Painting border: "+((FurnitureBorder)item).location.x+","+((FurnitureBorder)item).location.y);
        	}
        	else if(item instanceof FurnitureTitle) ((FurnitureTitle)item).paint(gp,furnitureScale);
        	else if(item instanceof FurnitureScale) ((FurnitureScale)item).paint(gp,furnitureScale);
        	else if(item instanceof FurnitureNorth) ((FurnitureNorth)item).paint(gp,furnitureScale);
        	else if(item instanceof FurnitureNote) ((FurnitureNote)item).paint(gp,furnitureScale);
        	else if(item instanceof FurnitureLegend) ((FurnitureLegend)item).paint(gp, furnitureScale, furnitureScale, -1);
        	else if(item instanceof LayerLegend) ((LayerLegend)item).paint(gp, furnitureScale, furnitureScale, -1);
        	else if(item instanceof FurnitureImage) ((FurnitureImage)item).paint(gp, furnitureScale, globalScale);
        	
        	
        }


        // draw the scale
        if(drawScale)
        {
            gp.setColor(Color.BLACK);
            gp.setFont(new Font("SansSerif",Font.PLAIN,9));
            //gp.drawString("Scale 1:"+String.valueOf(formatScale(drawingScale)),x,y);
        }
/*
        // draw border
        if(border.show)
        {
            if(debug) System.out.println("Border - paint: "+border.location);
            border.setBorder(border.location.x, border.location.y,getPrintSize().width, getPrintSize().height, false);
            //border.location = new Rectangle(0,0,getPrintSize().width, getPrintSize().height);
            border.paint(gp,furnitureScale, globalScale);
        }
        if(borders != null) for(int i=0; i < borders.size(); i++)
        {
        	FurnitureBorder aborder = borders.elementAt(i);
        	//System.out.println("Painting border: "+i+","+aborder.thickness+","+aborder.color+","+aborder.show);
        	if(aborder.show) aborder.paint(gp, furnitureScale, globalScale);
        }


        if(debug) sb.append("furnitureScale ="+furnitureScale+"  drawingScale="+drawingScale+"  viewScale="+viewScale+"\n");
        if(title.show)
        {
            if(debug) sb.append("drawing title\n");
            title.paint(gp,furnitureScale);
        }
        if(scaleItem.show)
        {
            if(debug) sb.append("drawing scale\n");
            scaleItem.paint(gp, furnitureScale);
        }
        if(north.show)
        {
            if(debug) sb.append("drawing north\n");
            north.paint(gp, furnitureScale);
        }

        //if(note.show)
        //{
        //    note.paint(gp, furnitureScale);
        //}
        for(int i=0; i < notes.size(); i++)
        {
        	FurnitureNote note = notes.elementAt(i);
        	if(note.show) note.paint(gp, furnitureScale);
        }

        if(legend != null && legend.show)
        {
            legend.paint(gp, furnitureScale, furnitureScale, -1);
        }

         if(layerLegend != null && layerLegend.show)
        {
            layerLegend.paint(gp, furnitureScale, furnitureScale, -1);
        }
*/
        // draw paper boundaries
        int x = 0;
        int y = 0;
        gp.setColor(Color.RED);

//        if(paperWidthMeasure/scale < 5 || paperHeightMeasure/scale < 5)
//        {
//            String message = "Page format has not been set correctly, or scale is too small to show pages:\n"+
//            "        scale="+scale+" width="+paperWidthMeasure+" height="+paperHeightMeasure;
//            System.out.println("message: "+message);
//            //JOptionPane.showMessageDialog(this,message ,"Warning...",JOptionPane.WARNING_MESSAGE);
//        }
//        else
        if(!paperTooSmall)
        {
            while (x < xsize/globalScale - xpan)
            {
               gp.drawLine(x,0,x,(int)((ysize-ypan)/globalScale));
               x = x + (int) (paperWidthMeasure/scale);
            }
            while (y < ysize/globalScale - ypan)
            {
               gp.drawLine(0,y,(int)((xsize-xpan)/globalScale),y);
               y = y + (int) (paperHeightMeasure/scale);
            }
        }
        g.scale(1.0/globalScale,1.0/globalScale);
        g.translate(-xpan,-ypan);

    }
    
    public int compare(Object item1, Object item2)
    {
    	if(((Furniture)item1).layerNumber < ((Furniture)item2).layerNumber)return -1;
    	else if(((Furniture)item1).layerNumber > ((Furniture)item2).layerNumber) return 1;
    	else return 0;
    }
    /**
     * draw the pan symbol
     * @param g
     * @param x
     * @param y
     * @param w
     * @param h
     * @param scale
     */
    private void drawPan(Graphics g, int x, int y, int w, int h, double scale)
    {
        int x0,x1,y0,y1;
        int off = 20;
        x0 = (int) Math.round(x-w/2) + off;
        x1 = (int) Math.round(x+w/2) - off;
        y0 = (int) Math.round(y-h/2) + off;
        y1 = (int) Math.round(y+h/2) - off;
        int d = (int) (40/scale);

        g.drawRect(x0, y0, w-2*off, h-2*off);
        g.drawLine(x0, y0, x1, y1);
        g.drawLine(x0, y1, x1, y0);
        g.drawOval(x-d/2, y-d/2, d, d);
    }
    /**
     * formats the scale value
     * @param v the scale value
     * @return formatted scale
     */
    private String formatScale(double v)
    {
        DecimalFormat scaleFormat= null;
        if (v >= 10.0 || v == 0.0)  scaleFormat = new DecimalFormat("#,###");
        else if(v >= 1.0 && v < 10.0) scaleFormat = new DecimalFormat("##0.0");
        else scaleFormat = new DecimalFormat("0.0E0");
        return scaleFormat.format(v);
    }

    /**
     * handle mouse events
     * @param ev
     */
    public void mouseClicked(MouseEvent ev) {}
    public void mouseDown(MouseEvent ev)
    {

    }
    public void mouseReleased(MouseEvent ev)
    {
        int xp = xlast - xd;
        int yp = ylast - yd;
        if(xp < 0) xp = 0;
        if(yp < 0) yp = 0;

        if(panning)
        {
            panning = false;
            if(!initDrag && (Math.abs(xlast-xd) > 10 || Math.abs(ylast-yd) > 10))
            {
                xpan = xpan + (int) ((xlast-xd)*globalScale);
                ypan = ypan + (int) ((ylast-yd)*globalScale);
            }
            //System.out.println("xd="+xd+"  yd="+yd+" xlast="+xlast+" ylast="+ylast+" xpan="+xpan+"  ypan="+ypan);
            xlast = -1;
            ylast = -1;
            initDrag = true;
            repaint();
        }
        if(draggingTitle && xlast >= 0 && ylast >= 0)
        {
            draggingTitle = false;
            title.location = new Rectangle((int) ((xp)/furnitureScale), (int)((yp)/furnitureScale), title.location.width, title.location.height);
            xlast = -1;
            ylast = -1;
            initDrag = true;
            repaint();
//            PrinterSetup.autoCB.setSelected(false);
        }
         if(draggingScale && xlast >= 0 && ylast >= 0)
        {
            draggingScale = false;
            scaleItem.location = new Rectangle((int) ((xp)/furnitureScale), (int)((yp)/furnitureScale),
                    scaleItem.location.width, scaleItem.location.height);
            xlast = -1;
            ylast = -1;
            initDrag = true;
            repaint();
//            PrinterSetup.autoCB.setSelected(false);
        }
         if(draggingNorth && xlast >= 0 && ylast >= 0)
        {
            draggingNorth = false;
            north.location = new Rectangle((int) ((xp)/furnitureScale), (int)((yp)/furnitureScale),
                    north.location.width, north.location.height);
            xlast = -1;
            ylast = -1;
            initDrag = true;
            repaint();
//            PrinterSetup.autoCB.setSelected(false);
        }
         if(draggingNote && xlast >= 0 && ylast >= 0)
        {
            draggingNote = false;
            note.location = new Rectangle((int) ((xp)/furnitureScale), (int)((yp)/furnitureScale),
                    note.location.width, note.location.height);
            xlast = -1;
            ylast = -1;
            initDrag = true;
            notes.set(selectedNote, note);
            repaint();
            //sb.append("note painted:"+note.location+"\n");

//            PrinterSetup.autoCB.setSelected(false);
        }
         
         if(draggingBorder && xlast >= 0 && ylast >= 0)
         {
        	 draggingBorder = false;
        	 FurnitureBorder aborder = border;
         	 if(selectedBorder >= 0) aborder = borders.elementAt(selectedBorder);
             aborder.location = new Rectangle((int) ((xp)/furnitureScale), (int)((yp)/furnitureScale),
                     aborder.location.width, aborder.location.height);
             xlast = -1;
             ylast = -1;
             initDrag = true;
             aborder.setBorder(aborder.location.x, aborder.location.y,aborder.location.width, aborder.location.height,true);
             //System.out.println("Setting border "+selectedBorder);
             repaint();
         }

         if(draggingBorderBR && xlast >= 0 && ylast >= 0)
        {
        	FurnitureBorder aborder = border;
        	if(selectedBorder >= 0) aborder = borders.elementAt(selectedBorder);
            draggingBorderBR = false;
            int x = aborder.location.x;
            int y = aborder.location.y;
            int width = (int)Math.round(xlast/furnitureScale - aborder.location.x);
            int height =  (int)Math.round(ylast/furnitureScale - aborder.location.y);
            
            //System.out.println("BR: width="+width+" height="+height+"  xlast="+xlast+"  ylast="+ylast+" xd="+xd+" yd="+yd);
            
            if (width < 0) width = 0;
            if (height < 0) height = 0;
            aborder.setBorder(x,y,width,height, true);
//            border.location.x, border.location.y,
//                         (int)(xlast/furnitureScale - border.location.x),
//                         (int)(ylast/furnitureScale - border.location.y), true);
            //border.location = new Rectangle(0,0,(int)(xlast/furnitureScale), (int)(ylast/furnitureScale));
            xlast = -1;
            ylast = -1;
            initDrag = true;
            repaint();
            //sb.append("note painted:"+note.location+"\n");

//            PrinterSetup.autoCB.setSelected(false);
        }

         if(draggingBorderTL && xlast >= 0 && ylast >= 0)
        {
        	FurnitureBorder aborder = border;
         	if(selectedBorder >= 0) aborder = borders.elementAt(selectedBorder);
            draggingBorderTL = false;
            int x = (int)Math.round(xlast/furnitureScale);
            int y = (int)Math.round(ylast/furnitureScale);
            int width = (int)Math.round(aborder.location.x+aborder.location.width-xlast/furnitureScale);
            int height = (int) Math.round(aborder.location.y+aborder.location.height-ylast/furnitureScale);
            if (width < 0)
            {
                x = aborder.location.x + aborder.location.width;
                width = 0;
            }
            if (height < 0)
            {
                y = aborder.location.y+aborder.location.height;
                height = 0;
            }
            aborder.setBorder(x,y,width,height, true);

            

            //border.location = new Rectangle(0,0,(int)(xlast/furnitureScale), (int)(ylast/furnitureScale));
            xlast = -1;
            ylast = -1;
            initDrag = true;
            repaint();
            //sb.append("note painted:"+note.location+"\n");

//            PrinterSetup.autoCB.setSelected(false);
        }
         if(draggingLegend && xlast >= 0 && ylast >= 0)
        {
            draggingLegend = false;
            legend.location = new Rectangle((int) ((xp)/furnitureScale), (int)((yp)/furnitureScale), legend.location.width, legend.location.height);
            xlast = -1;
            ylast = -1;
            initDrag = true;
            repaint();
//            PrinterSetup.autoCB.setSelected(false);
        }

        if(draggingLayerLegend && xlast >= 0 && ylast >= 0)
        {
            draggingLayerLegend = false;
            layerLegend.location = new Rectangle((int) ((xp)/furnitureScale), (int)((yp)/furnitureScale), layerLegend.location.width, layerLegend.location.height);
            xlast = -1;
            ylast = -1;
            initDrag = true;
            repaint();
//            PrinterSetup.autoCB.setSelected(false);
        }
        
        if(draggingImage && xlast >= 0 && ylast >= 0)
        {
        	draggingImage = false;
        	FurnitureImage imageItem = imageItems.elementAt(selectedImage);
        	imageItem.location = new Rectangle((int) ((xp)/furnitureScale), (int)((yp)/furnitureScale), imageItem.location.width, imageItem.location.height);
            xlast = -1;
            ylast = -1;
            initDrag = true;
            repaint();
        }
        if(draggingImageBR && xlast >= 0 && ylast >= 0)
        {
        	FurnitureImage imageItem = null;
        	if(selectedImage >= 0) imageItem = imageItems.elementAt(selectedImage);
            draggingImageBR = false;
            double ratio = (double)imageItem.location.width/(double)imageItem.location.height;
            int x = imageItem.location.x;
            int y = imageItem.location.y;
            int width = (int)(xlast/furnitureScale - imageItem.location.x);
            int height =  (int)(ylast/furnitureScale - imageItem.location.y);
            if(imageItem.ratioLocked) height = (int) (width/ratio);
            if (width < 0) width = 0;
            if (height < 0) height = 0;
            imageItem.setImage(x,y,width,height);
//            border.location.x, border.location.y,
//                         (int)(xlast/furnitureScale - border.location.x),
//                         (int)(ylast/furnitureScale - border.location.y), true);
            //border.location = new Rectangle(0,0,(int)(xlast/furnitureScale), (int)(ylast/furnitureScale));
            xlast = -1;
            ylast = -1;
            initDrag = true;
            repaint();
            //sb.append("note painted:"+note.location+"\n");

//            PrinterSetup.autoCB.setSelected(false);
        }

         if(draggingImageTL && xlast >= 0 && ylast >= 0)
        {
        	FurnitureImage imageItem = null;
         	if(selectedImage >= 0) imageItem = imageItems.elementAt(selectedImage);
            draggingImageTL = false;
            double ratio = (double)imageItem.location.width/(double)imageItem.location.height;
            int x = (int)(xlast/furnitureScale);
            int y = (int)(ylast/furnitureScale);
            int width = (int)(imageItem.location.x+imageItem.location.width-xlast/furnitureScale);
            int height = (int) (imageItem.location.y+imageItem.location.height-ylast/furnitureScale);
            if(imageItem.ratioLocked) height = (int) (width/ratio);
            if (width < 0)
            {
                x = imageItem.location.x + imageItem.location.width;
                width = 0;
            }
            if (height < 0)
            {
                y = imageItem.location.y+imageItem.location.height;
                height = 0;
            }
            imageItem.setImage(x,y,width,height);

            

            //border.location = new Rectangle(0,0,(int)(xlast/furnitureScale), (int)(ylast/furnitureScale));
            xlast = -1;
            ylast = -1;
            initDrag = true;
            repaint();
            //sb.append("note painted:"+note.location+"\n");

//            PrinterSetup.autoCB.setSelected(false);
        }


        if(dragging && xlast >= 0 && ylast >= 0)
        {
            dragging = false;
            xoff = xlast - xd;
            yoff = ylast - yd;
            xlast = -1;
            ylast = -1;
            initDrag = true;
            mapRect = new Rectangle(xoff, yoff, mapRect.width, mapRect.height);
            border.setBorder(border.location.x, border.location.y, getPrintSize().width, getPrintSize().height, false);
           // border.location = new Rectangle(0,0,getPrintSize().width, getPrintSize().height);
            repaint();
            PrinterSetup.autoCB.setSelected(false);
        }
        border.setBorder(border.location.x, border.location.y,getPrintSize().width, getPrintSize().height, false);
        initDrag = true;
        repaint();
        setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }
    public void mousePressed(MouseEvent ev)
    {
        //boolean debug = true;
        boolean shift = false;
        if((ev.getModifiers() & Event.SHIFT_MASK) != 0) shift = true;
        int x = (int) ((ev.getX()-xpan)/globalScale);
        int y = (int) ((ev.getY()-ypan)/globalScale);
        if(shift)
        {
            setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            panning = true;
            xd = x;
            yd = y;
            return;
        }


        if(debug) sb.append(">>> Pressed at: "+x+","+y+"\n");
        dragging = false;
        draggingTitle = false;
        
        if(title.inside(x,y, furnitureScale))
        {
            setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
            xd = x - (int) (title.location.x*furnitureScale);
            yd = y - (int) (title.location.y*furnitureScale);
            draggingTitle = true;
            return;
        }

         if(scaleItem.inside(x,y, furnitureScale))
        {
            setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
            xd = x - (int) (scaleItem.location.x*furnitureScale);
            yd = y - (int) (scaleItem.location.y*furnitureScale);
            draggingScale = true;
            return;
        }
        if(north.inside(x,y, furnitureScale))
       {
           setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
           xd = x - (int) (north.location.x*furnitureScale);
           yd = y - (int) (north.location.y*furnitureScale);
           draggingNorth = true;
           return;
       }
        for(int i=0; i < notes.size(); i++)
        {
        	note = notes.elementAt(i);
	        if(note.inside(x,y, furnitureScale))
	       {
	           if(debug) sb.append(">>> Pressed at note: "+x+","+y+"\n");
	
	           setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
	           xd = x - (int) (note.location.x*furnitureScale);
	           yd = y - (int) (note.location.y*furnitureScale);
	           draggingNote = true;
	           selectedNote = i;
	           return;
	       }
        }
        
        

       if(border.insideBR(x,y, furnitureScale, globalScale) && border.fixed)
       {
           if(debug) sb.append(">>> Pressed at border: "+x+","+y+"\n");

           setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
           xd = x - (int) (border.location.x*furnitureScale);
           yd = y - (int) (border.location.y*furnitureScale);
           draggingBorderBR = true;
           selectedBorder = -1;
           return;
       }
       if(border.insideTL(x,y, furnitureScale, globalScale) && border.fixed)
       {
           if(debug) sb.append(">>> Pressed at border: "+x+","+y+"\n");

           setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
           xd = x - (int) (border.location.x*furnitureScale);
           yd = y - (int) (border.location.y*furnitureScale);
           draggingBorderTL = true;
           selectedBorder = -1;
           return;
       }
       if(border.inside(x,y, furnitureScale) && border.fixed)
       {
           if(debug) sb.append(">>> Pressed at border: "+x+","+y+"\n");

           setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
           xd = x - (int) (border.location.x*furnitureScale);
           yd = y - (int) (border.location.y*furnitureScale);
           draggingBorder = true;
           selectedBorder = -1;
           return;
       }
       for(int i=0; i < borders.size(); i++)
       {
    	   FurnitureBorder aborder = borders.elementAt(i);
    	   if(aborder.insideBR(x,y, furnitureScale, globalScale) && aborder.fixed)
           {
               if(debug) sb.append(">>> Pressed at border: "+x+","+y+"\n");

               setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
               xd = x - (int) (aborder.location.x*furnitureScale);
               yd = y - (int) (aborder.location.y*furnitureScale);
               draggingBorderBR = true;
               selectedBorder = i;
               return;
           }
           if(aborder.insideTL(x,y, furnitureScale, globalScale) && aborder.fixed)
           {
               if(debug) sb.append(">>> Pressed at border: "+x+","+y+"\n");

               setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
               xd = x - (int) (aborder.location.x*furnitureScale);
               yd = y - (int) (aborder.location.y*furnitureScale);
               draggingBorderTL = true;
               selectedBorder = i;
               return;
           }
           if(aborder.inside(x,y, furnitureScale) && aborder.fixed)
           {
               if(debug) sb.append(">>> Pressed at border: "+x+","+y+"\n");

               setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
               xd = x - (int) (aborder.location.x*furnitureScale);
               yd = y - (int) (aborder.location.y*furnitureScale);
               draggingBorder = true;
               selectedBorder = i;
               return;
           }
       }
       
       
       if(legend.inside(x,y, furnitureScale))
        {
            setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
            xd = x - (int) (legend.location.x*furnitureScale);
            yd = y - (int) (legend.location.y*furnitureScale);
            draggingLegend = true;
            return;
        }
        if(layerLegend.inside(x,y, furnitureScale))
        {
            setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
            xd = x - (int) (layerLegend.location.x*furnitureScale);
            yd = y - (int) (layerLegend.location.y*furnitureScale);
            draggingLayerLegend = true;
            return;
        }
        for(int i=0; i < imageItems.size(); i++)
        {
     	   FurnitureImage imageItem = imageItems.elementAt(i);
     	  
     	   if(imageItem.insideBR(x,y, furnitureScale, globalScale))
            {
                if(debug) sb.append(">>> Pressed at image: "+x+","+y+"\n");

                setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
                xd = x - (int) (imageItem.location.x*furnitureScale);
                yd = y - (int) (imageItem.location.y*furnitureScale);
                draggingImageBR = true;
                selectedImage = i;
                return;
            }
            if(imageItem.insideTL(x,y, furnitureScale, globalScale) )
            {
                if(debug) sb.append(">>> Pressed at image: "+x+","+y+"\n");

                setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
                xd = x - (int) (imageItem.location.x*furnitureScale);
                yd = y - (int) (imageItem.location.y*furnitureScale);
                draggingImageTL = true;
                selectedImage = i;
                return;
            }
            if(imageItem.inside(x,y, furnitureScale))
      	    {
      		  if(debug) sb.append(">>> Pressed at image: "+x+","+y+"\n");

               setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
               xd = x - (int) (imageItem.location.x*furnitureScale);
               yd = y - (int) (imageItem.location.y*furnitureScale);
               draggingImage = true;
               selectedImage = i;
               return;
      	    }
        }
        
        
        
        // only move maps if no furniture is selected.
        if(mapRect.contains(x,y))
        {
            setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
            xd = x - mapRect.x;
            yd = y - mapRect.y;
            dragging = true;


        }

    }

    public void mouseEntered(MouseEvent ev)
    {

    }
    public void mouseExited(MouseEvent ev)
    {
      
    }
    public void mouseMoved(MouseEvent ev) {}
    public void mouseDragged(MouseEvent ev)
    {
        Graphics g = getGraphics();
        Graphics2D g2 = (Graphics2D) g;
        g2.translate(xpan,ypan);
        g2.scale(globalScale, globalScale);


        int x = (int) ((ev.getX()-xpan)/globalScale);
        int y = (int) ((ev.getY()-ypan)/globalScale);

        if(panning)
        {
            g.setColor(Color.WHITE);
            g.setXORMode(Color.BLUE);
            int w = (int) (getWidth()/globalScale);
            int h = (int) (getHeight()/globalScale);
            //if(!initDrag) drawPan(g, xlast, ylast, w, h, globalScale);
            //drawPan(g,x,y,w,h, globalScale);
           if(title.show)
           {
            if(!initDrag) g.drawRect((int) (title.location.x*furnitureScale)+(xlast-xd),
                                     (int) (title.location.y*furnitureScale)+(ylast-yd),
                                     (int) (title.location.width*furnitureScale),
                                     (int) (title.location.height*furnitureScale));
            g.drawRect((int) (title.location.x*furnitureScale)+(x-xd),
                       (int) (title.location.y*furnitureScale)+(y-yd),
                       (int) (title.location.width*furnitureScale),
                       (int) (title.location.height*furnitureScale));
           }
           if(scaleItem.show)
           {
            if(!initDrag) g.drawRect((int) (scaleItem.location.x*furnitureScale)+(xlast-xd),
                                     (int) (scaleItem.location.y*furnitureScale)+(ylast-yd),
                                     (int) (scaleItem.location.width*furnitureScale),
                                     (int) (scaleItem.location.height*furnitureScale));
            g.drawRect((int) (scaleItem.location.x*furnitureScale)+(x-xd),
                       (int) (scaleItem.location.y*furnitureScale)+ (y-yd),
                       (int) (scaleItem.location.width*furnitureScale),
                       (int) (scaleItem.location.height*furnitureScale));
           }
           if(north.show)
           {
            if(!initDrag) g.drawRect((int) (north.location.x*furnitureScale)+(xlast-xd),
                                     (int) (north.location.y*furnitureScale)+(ylast-yd),
                                     (int) (north.location.width*furnitureScale),
                                     (int) (north.location.height*furnitureScale));
            g.drawRect((int) (north.location.x*furnitureScale)+(x-xd),
                       (int) (north.location.y*furnitureScale)+(y-yd),
                       (int) (north.location.width*furnitureScale),
                       (int) (north.location.height*furnitureScale));
           }
           if(note.show)
           {
            if(!initDrag) g.drawRect((int) (note.location.x*furnitureScale)+(xlast-xd),
                                     (int) (note.location.y*furnitureScale)+(ylast-yd),
                                     (int) (note.location.width*furnitureScale),
                                     (int) (note.location.height*furnitureScale));
            g.drawRect((int) (note.location.x*furnitureScale)+(x-xd),
                       (int) (note.location.y*furnitureScale)+(y-yd),
                       (int) (note.location.width*furnitureScale),
                       (int) (note.location.height*furnitureScale));
           }
           if(border.show)
           {
            if(!initDrag) g.drawRect((int) (border.location.x*furnitureScale)+(xlast-xd),
                                     (int)( border.location.y*furnitureScale)+(ylast-yd),
                                     (int) (border.location.width*furnitureScale),
                                     (int)( border.location.height*furnitureScale));
            g.drawRect((int) (border.location.x*furnitureScale)+(x-xd),
                             (int)( border.location.y*furnitureScale)+(y-yd),
                            (int) (border.location.width*furnitureScale),
                            (int)( border.location.height*furnitureScale));
           }
           if(legend.show)
           {
            if(!initDrag) g.drawRect((int) (legend.location.x*furnitureScale)+(xlast-xd),
                                     (int) (legend.location.y*furnitureScale)+(ylast-yd),
                                     (int) (legend.location.width*furnitureScale),
                                     (int) (legend.location.height*furnitureScale));
            g.drawRect((int) (legend.location.x*furnitureScale)+(x-xd),
                       (int) (legend.location.y*furnitureScale)+(y-yd),
                       (int) (legend.location.width*furnitureScale),
                       (int) (legend.location.height*furnitureScale));
           }
           if(layerLegend.show)
           {
            if(!initDrag) g.drawRect((int) (layerLegend.location.x*furnitureScale)+(xlast-xd),
                                     (int) (layerLegend.location.y*furnitureScale)+(ylast-yd),
                                     (int) (layerLegend.location.width*furnitureScale),
                                     (int) (layerLegend.location.height*furnitureScale));
            g.drawRect((int) (layerLegend.location.x*furnitureScale)+(x-xd),
                       (int) (layerLegend.location.y*furnitureScale)+(y-yd),
                       (int) (layerLegend.location.width*furnitureScale),
                       (int) (layerLegend.location.height*furnitureScale));
           }
           

            if(!initDrag) g.drawRect(xoff+xlast-xd, yoff+ylast-yd, mapRect.width, mapRect.height);
            g.drawRect(xoff+x-xd, yoff+y-yd, mapRect.width, mapRect.height);

            xlast = x;
            ylast = y;
            initDrag = false;
        }
        if(draggingTitle)
        {
            

            g.setColor(Color.WHITE);
            g.setXORMode(Color.BLUE);
            if(!initDrag) g.drawRect((xlast-xd),
                                     (ylast-yd),
                                     (int) (title.location.width*furnitureScale),
                                     (int) (title.location.height*furnitureScale));
            g.drawRect((x-xd),
                       (y-yd),
                       (int) (title.location.width*furnitureScale),
                       (int) (title.location.height*furnitureScale));
            xlast = x;
            ylast = y;
            initDrag = false;
        }

        if(draggingScale)
        {
//            int x = ev.getX();
//            int y = ev.getY();

            g.setColor(Color.WHITE);
            g.setXORMode(Color.BLUE);
            if(!initDrag) g.drawRect((xlast-xd),
                                     (ylast-yd),
                                     (int) (scaleItem.location.width*furnitureScale),
                                     (int) (scaleItem.location.height*furnitureScale));
            g.drawRect((x-xd),
                       (y-yd),
                       (int) (scaleItem.location.width*furnitureScale),
                       (int) (scaleItem.location.height*furnitureScale));
            xlast = x;
            ylast = y;
            initDrag = false;
        }

         if(draggingNorth)
        {
//            int x = ev.getX();
//            int y = ev.getY();

            g.setColor(Color.WHITE);
            g.setXORMode(Color.BLUE);
            if(!initDrag) g.drawRect((xlast-xd),
                                     (ylast-yd),
                                     (int) (north.location.width*furnitureScale),
                                     (int) (north.location.height*furnitureScale));
            g.drawRect((x-xd),
                       (y-yd),
                       (int) (north.location.width*furnitureScale),
                       (int) (north.location.height*furnitureScale));
            xlast = x;
            ylast = y;
            initDrag = false;
        }

         if(draggingNote)
        {
//            int x = ev.getX();
//            int y = ev.getY();

            g.setColor(Color.WHITE);
            g.setXORMode(Color.BLUE);
            if(!initDrag) g.drawRect((xlast-xd),
                                     (ylast-yd),
                                     (int) (note.location.width*furnitureScale),
                                     (int) (note.location.height*furnitureScale));
            g.drawRect((x-xd),
                       (y-yd),
                       (int) (note.location.width*furnitureScale),
                       (int) (note.location.height*furnitureScale));
            //sb.append("note location:"+note.location+"\n");
            xlast = x;
            ylast = y;
            initDrag = false;
        }
         
         if(draggingBorder)
         {
//           int x = ev.getX();
//           int y = ev.getY();
           Furniture aborder = border;
           if(selectedBorder >= 0) aborder = borders.elementAt(selectedBorder);
           g.setColor(Color.WHITE);
           g.setXORMode(Color.BLUE);
           if(!initDrag) g.drawRect((xlast-xd),
                                    (ylast-yd),
                                    (int) (aborder.location.width*furnitureScale),
                                    (int) (aborder.location.height*furnitureScale));
           g.drawRect((x-xd),
                      (y-yd),
                      (int) (aborder.location.width*furnitureScale),
                      (int) (aborder.location.height*furnitureScale));
           //sb.append("border location:"+border.location+"\n");
           xlast = x;
           ylast = y;
           initDrag = false;
         }
        	       	

         if(draggingBorderBR)
        {
//            int x = ev.getX();
//            int y = ev.getY();
        	FurnitureBorder aborder = border;
          	if(selectedBorder >= 0) aborder = borders.elementAt(selectedBorder);
            g.setColor(Color.WHITE);
            g.setXORMode(Color.BLUE);
            if(!initDrag) g.drawRect((int) (aborder.location.x*furnitureScale),
                                     (int)( aborder.location.y*furnitureScale),
                                     xlast - (int) (aborder.location.x*furnitureScale),
                                     ylast - (int)( aborder.location.y*furnitureScale));
           g.drawRect((int) (aborder.location.x*furnitureScale),
                             (int)( aborder.location.y*furnitureScale),
                             x - (int) (aborder.location.x*furnitureScale),
                             y - (int)( aborder.location.y*furnitureScale));
            //sb.append("note location:"+note.location+"\n");
            xlast = x;
            ylast = y;
            initDrag = false;
        }
         if(draggingBorderTL)
        {
//            int x = ev.getX();
//            int y = ev.getY();
        	FurnitureBorder aborder = border;
          	if(selectedBorder >= 0) aborder = borders.elementAt(selectedBorder);
            g.setColor(Color.WHITE);
            g.setXORMode(Color.BLUE);
            if(!initDrag) g.drawRect(xlast, ylast,
                                   (int)((aborder.location.x+aborder.location.width)*furnitureScale)-xlast,
                                   (int) ((aborder.location.y+aborder.location.height)*furnitureScale)-ylast);
            g.drawRect(x,y,
                    (int)((aborder.location.x+aborder.location.width)*furnitureScale)-x,
                    (int) ((aborder.location.y+aborder.location.height)*furnitureScale)-y);
            //sb.append("note location:"+note.location+"\n");
            xlast = x;
            ylast = y;
            initDrag = false;
        }
        if(draggingLegend)
        {


            g.setColor(Color.WHITE);
            g.setXORMode(Color.BLUE);
            if(!initDrag) g.drawRect((xlast-xd),
                                     (ylast-yd),
                                     (int) (legend.location.width*furnitureScale),
                                     (int) (legend.location.height*furnitureScale));
            g.drawRect((x-xd),
                       (y-yd),
                       (int) (legend.location.width*furnitureScale),
                       (int) (legend.location.height*furnitureScale));
            xlast = x;
            ylast = y;
            initDrag = false;
        }
        if(draggingLayerLegend)
        {


            g.setColor(Color.WHITE);
            g.setXORMode(Color.BLUE);
            if(!initDrag) g.drawRect((xlast-xd),
                                     (ylast-yd),
                                     (int) (layerLegend.location.width*furnitureScale),
                                     (int) (layerLegend.location.height*furnitureScale));
            g.drawRect((x-xd),
                       (y-yd),
                       (int) (layerLegend.location.width*furnitureScale),
                       (int) (layerLegend.location.height*furnitureScale));
            xlast = x;
            ylast = y;
            initDrag = false;
        }
        
        if(draggingImage)
        {
//            int x = ev.getX();
//            int y = ev.getY();

            g.setColor(Color.WHITE);
            g.setXORMode(Color.BLUE);
            FurnitureImage imageItem = imageItems.elementAt(selectedImage);
            if(!initDrag) g.drawRect((xlast-xd),
                                     (ylast-yd),
                                     (int) (imageItem.location.width*furnitureScale),
                                     (int) (imageItem.location.height*furnitureScale));
            g.drawRect((x-xd),
                       (y-yd),
                       (int) (imageItem.location.width*furnitureScale),
                       (int) (imageItem.location.height*furnitureScale));
            //sb.append("note location:"+note.location+"\n");
            xlast = x;
            ylast = y;
            initDrag = false;
        }
        if(draggingImageBR)
        {
//            int x = ev.getX();
//            int y = ev.getY();
        	FurnitureImage imageItem = null;
          	if(selectedImage >= 0) imageItem = imageItems.elementAt(selectedImage);
            g.setColor(Color.WHITE);
            g.setXORMode(Color.BLUE);
            if(!initDrag) g.drawRect((int) (imageItem.location.x*furnitureScale),
                                     (int)( imageItem.location.y*furnitureScale),
                                     xlast - (int) (imageItem.location.x*furnitureScale),
                                     ylast - (int)( imageItem.location.y*furnitureScale));
           g.drawRect((int) (imageItem.location.x*furnitureScale),
                             (int)( imageItem.location.y*furnitureScale),
                             x - (int) (imageItem.location.x*furnitureScale),
                             y - (int)( imageItem.location.y*furnitureScale));
            //sb.append("note location:"+note.location+"\n");
            xlast = x;
            ylast = y;
            initDrag = false;
        }
         if(draggingImageTL)
        {
//            int x = ev.getX();
//            int y = ev.getY();
        	FurnitureImage imageItem = null;
          	if(selectedImage >= 0) imageItem = imageItems.elementAt(selectedImage);
            g.setColor(Color.WHITE);
            g.setXORMode(Color.BLUE);
            if(!initDrag) g.drawRect(xlast, ylast,
                                   (int)((imageItem.location.x+imageItem.location.width)*furnitureScale)-xlast,
                                   (int) ((imageItem.location.y+imageItem.location.height)*furnitureScale)-ylast);
            g.drawRect(x,y,
                    (int)((imageItem.location.x+imageItem.location.width)*furnitureScale)-x,
                    (int) ((imageItem.location.y+imageItem.location.height)*furnitureScale)-y);
            //sb.append("note location:"+note.location+"\n");
            xlast = x;
            ylast = y;
            initDrag = false;
        }
        if(dragging)
        {
//            int x = ev.getX();
//            int y = ev.getY();

            g.setColor(Color.WHITE);
            g.setXORMode(Color.BLUE);

            if(!initDrag) g.drawRect(xlast-xd, ylast-yd, mapRect.width, mapRect.height);
            g.drawRect(x-xd, y-yd, mapRect.width, mapRect.height);

            xlast = x;
            ylast = y;
            initDrag = false;
        }



        g2.scale(1.0/globalScale, 1.0/globalScale);
        g2.translate(-xpan,-ypan);
        
    }

}
