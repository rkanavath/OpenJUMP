package com.cadplan.jump;

import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.VertexStyle;
import com.vividsolutions.jump.workbench.ui.Viewport;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.feature.Feature;

import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RectangularShape;
import java.awt.geom.Point2D;
import java.awt.geom.AffineTransform;

/**
 * User: geoff
 * Date: 8/07/2007
 * Time: 10:05:08
 * Copyright 2005 Geoffrey G Roy.
 */
public class WKTVertexStyle extends ExternalSymbolsType
{
    public WKTshape wktShape;
    private String name;
    private int width;
    private int height;
    private double scale = 1.0;
    private double orientation = 0.0;
    private String attributeName = "";
    private int attributeIndex = -1;
    private double attributeValue = 0.0;
    private boolean byValue = true;
    private boolean showLine = true;
    private boolean showFill = true;
    private Color lineColor = Color.BLACK;
    private Color fillColor = Color.WHITE;
    private boolean dotted;
    //private boolean sizeByScale = true;
    private double baseScale = 1.0;

    public WKTVertexStyle()
    {
       super(new Rectangle2D.Double());

    }

   // private void setSizeByScale(boolean sizeByScale)
   // {
   // 	this.sizeByScale = sizeByScale;
   // }
   // private boolean getSizeByScale()
   // {
    //	return sizeByScale;
   // }
    private void setBaseScale(double baseScale)
    {
    	this.baseScale = baseScale;
    }
    private double getbaseScale()
    {
    	return baseScale;
    }
    public void setShowFill(boolean showFill)
    {
        this.showFill = showFill;
    }
    public boolean getShowFill()
    {
        return showFill;
    }
     public void setShowLine(boolean showLine)
    {
        this.showLine = showLine;
    }
    public boolean getShowLine()
    {
        return showLine;
    }

    public void setColors(Color lineColor, Color fillColor)
    {
        this.lineColor = lineColor;
        this.fillColor = fillColor;
    }

    public void setSize(int width, int height)
    {
        this.size = Math.max(width,height);
        this.width = width;
        this.height = height;
       ((RectangularShape)shape).setFrame(0.0,0.0,width,height);
        //System.out.println("Setting size: "+this.size);
    }

    public void setScale(double scale)
    {
        this.scale = scale;
    }
    public double getScale()
    {
        return scale;
    }

    public int getSize()
    {
        return size;
    }
    public void setSize(int size)
    {
        this.size = size;
    }
    public void setOrientation(double angle)
    {
        orientation = angle;
    }
    public double getOrientation()
    {
        return orientation;
    }

    public void setDotted(boolean dotted)
    {
        this.dotted = dotted;
    }
    public boolean getDotted()
    {
        return dotted;
    }
     public void setByValue(boolean byValue)
    {
        this.byValue = byValue;
    }
    public boolean getByValue()
    {
        return byValue;
    }
    public void setAttributeName (String attName)
    {
        this.attributeName = attName;
    }
    public String getAttributeName()
    {
        return attributeName;
    }

    public void setName(String name)
    {
        this.name = name;
        wktShape = null;
        boolean found = false;
        for (int i=0; i < VertexParams.wktShapes.length; i++)
        {
            if(VertexParams.wktNames[i].equals(name))
            {
                found = true;
                wktShape = VertexParams.wktShapes[i];
                if(wktShape != null)
                {
                    //size = image.getWidth(null);
                    width = wktShape.extent;
                    height = wktShape.extent;
                    //size = wktShape.extent;
                    int k = name.lastIndexOf("_x");
                    int j = name.lastIndexOf(".");
                    scale = 1;
                    if(k > 0)
                    {
                        String scaleS = name.substring(k+2,j);
                        int n = scaleS.lastIndexOf("x");
                        if(n > 0) scaleS = scaleS.substring(0,n);
                        try
                        {
                            scale = Integer.parseInt(scaleS);
                        }
                        catch (NumberFormatException ex)
                        {
                            scale = 1;
                        }
                        //System.out.println("Name:"+name+"  image scale factor="+scale);
                    }
                }
                //System.out.println("name="+name+ "  Before scaling: size="+size+ "  scale="+scale);
                size = (int) (size/scale);
                //System.out.println("name="+name+ "  After scaling: size="+size+ "  scale="+scale);
            }

        }
        if(!found)    // missing image
        {
           //Icon icon = (Icon) UIManager.get("JOptionPane.questionIcon");
           wktShape = null;
        }

    }
    public String getName()
    {
        return name;
    }

     public void initialize(Layer layer)
    {
        BasicStyle style = layer.getBasicStyle();
        lineColor = GUIUtil.alphaColor(style.getLineColor(), style.getAlpha());
        fillColor = GUIUtil.alphaColor(style.getFillColor(), style.getAlpha());
        if(layer == null) return;
        try
        {
             FeatureSchema featureSchema = layer.getFeatureCollectionWrapper().getFeatureSchema();
             if(attributeName != null && !attributeName.equals("")) attributeIndex = featureSchema.getAttributeIndex(attributeName);
        }
        catch (Exception ex)
        {
            attributeIndex = -1;
        }
        initializeText(layer);

    }

    public void paint(Feature feature, Graphics2D g2, Viewport viewport)
    {
        this.viewport = viewport;        
        if(!byValue && attributeIndex >= 0)
        {
            try
            {
                attributeValue = feature.getDouble(attributeIndex);
            }
            catch(Exception ex)
            {
                try
                {
                      attributeValue = (double) feature.getInteger(attributeIndex);
                }
                catch (Exception ex2)
                {
                    attributeValue = 0.0;
                }
            }
        }
        setTextAttributeValue(feature);
        
        try
        {
            super.paint(feature, g2, viewport);
        }
        catch(Exception ex)
        {
            System.out.println("Painting ERROR:"+ex);
            ex.printStackTrace();
            
        }
    }

    public void paint(Feature feature, Graphics2D g, Point2D p)
    {
         if(!byValue && attributeIndex >= 0)
        {
            try
            {
                attributeValue = feature.getDouble(attributeIndex);
            }
            catch(Exception ex)
            {
                try
                {
                      attributeValue = (double) feature.getInteger(attributeIndex);
                }
                catch (Exception ex2)
                {
                    attributeValue = 0.0;
                }
            }
        }
        setTextAttributeValue(feature);
        paint(g,p);
    }

    public void paint(Graphics2D g, Point2D p)
    {
        setFrame(p);
        render(g);
    }

    private void setFrame(Point2D p)
    {
        ((RectangularShape)shape).setFrame(p.getX()-width/(2*scale), p.getY()-height/(2*scale), width/scale, height/scale);
        //System.out.println("shape:"+shape);

    }

     protected void render(Graphics2D g)
    {
        ExternalSymbolsRenderer r = new ExternalSymbolsRenderer();

        int x = (int) shape.getBounds2D().getX();
        int y  = (int) shape.getBounds2D().getY();
        //***XX
        int shapeSize = size;
        //if(sizeByScale) shapeSize = (int) (size*baseScale/VertexParams.actualScale);
        //System.out.println("WKTrenderer: size="+size+" sizeByScale="+sizeByScale+" shapeSize="+shapeSize+"  baseScale="+baseScale+"  actualScale="+VertexParams.actualScale);
        WKTshape wktShape = r.getWKTShape(ExternalSymbolsRenderer.WKTS, name, shapeSize, shape);
        
        //Image scaledImage = image.getScaledInstance(width/scale, height/scale,Image.SCALE_DEFAULT);
        //System.out.println("scaledSize:"+scaledImage.getWidth(null)+","+scaledImage.getHeight(null)+ "  scale="+scale);

        AffineTransform currentTransform = g.getTransform();
        double angle= orientation;
        if(!byValue) angle = attributeValue;
        double  x0 = x + r.width/r.scale/2.0;
        double  y0 = y + r.height/r.scale/2.0;
        //System.out.println("location of WKT:"+x0+","+y0+"  in rend:"+r.x0+","+r.y0);
        int sizex = size;
        int sizey = size*height/width;
        g.translate(r.x0, r.y0);
        g.rotate(Math.toRadians(angle), 0, 0);

        r.paint(g, ExternalSymbolsRenderer.WKTS, wktShape, r.size, showLine, lineColor, showFill, fillColor, dotted);
//        if(wktShape == null)
//        {
//            g.setColor(Color.BLACK);
//            g.drawString("No WKT",x,y+10);
//        }
//        else
//        {
//            wktShape.paint(g, size, showLine, lineColor, showFill, fillColor, dotted);
//        }
//        if(dotted)
//        {
//            g.fillRect(-1, -1, 2, 2);
//        }
        g.setTransform(currentTransform);
        
        drawTextLabel(g, (float)r.x0, (float)r.y0);

    }

}
