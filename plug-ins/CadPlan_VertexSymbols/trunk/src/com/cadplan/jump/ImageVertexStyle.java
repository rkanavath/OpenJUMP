package com.cadplan.jump;

import com.vividsolutions.jump.workbench.ui.renderer.style.VertexStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.Viewport;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.feature.Feature;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.RectangularShape;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;
import java.awt.geom.AffineTransform;

/**
 * User: geoff
 * Date: 17/06/2007
 * Time: 10:57:44
 * Copyright 2005 Geoffrey G Roy.
 */
public class ImageVertexStyle extends ExternalSymbolsType
{
    public Image image;
    private String name;
    private int width;
    private int height;
    private double scale = 1.0;
    private double orientation = 0.0;
    private String attributeName = "";
    private int attributeIndex = -1;
    private double attributeValue = 0.0;
    private boolean byValue = true;

    public ImageVertexStyle()
    {
       super(new Rectangle2D.Double());

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
        image = null;
        boolean found = false;
        for (int i=0; i < VertexParams.images.length; i++)
        {
            if(VertexParams.imageNames[i].equals(name))
            {
                found = true;
                image = VertexParams.images[i];
                if(image != null)
                {
                    //size = image.getWidth(null);
                    width = image.getWidth(null);
                    height = image.getHeight(null);
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
                //size = (int) (size/scale);
                //System.out.println("name="+name+ "  After scaling: size="+size);
            }

        }
        if(!found)    // missing image
        {
           //Icon icon = (Icon) UIManager.get("JOptionPane.questionIcon");
           image = null;
        }

    }
    public String getName()
    {
        return name;
    }

     public void initialize(Layer layer)
    {
        BasicStyle style = layer.getBasicStyle();
        //lineColor = GUIUtil.alphaColor(style.getLineColor(), style.getAlpha());
        //fillColor = GUIUtil.alphaColor(style.getFillColor(), style.getAlpha());
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
        Image image = r.getImage(ExternalSymbolsRenderer.IMAGES, name, size, shape);
        int x = (int) shape.getBounds2D().getX();
        int y  = (int) shape.getBounds2D().getY();
        //Image scaledImage = image.getScaledInstance(width/scale, height/scale,Image.SCALE_DEFAULT);
        //System.out.println("scaledSize:"+scaledImage.getWidth(null)+","+scaledImage.getHeight(null)+ "  scale="+scale);

        AffineTransform currentTransform = g.getTransform();
        double angle= orientation;
        if(!byValue) angle = attributeValue;
        r.x0 = (float) (x + r.width/r.scale/2.0);
        r.y0 = (float) (y + r.height/r.scale/2.0);
        int sizex = size;
        int sizey = size*height/width;
        g.rotate(Math.toRadians(angle), r.x0, r.y0);
        r.paint(g, ExternalSymbolsRenderer.IMAGES, image);
        //g.rotate(Math.toRadians(angle), x+size/2, y+size/2);

//        if(image == null)
//        {
//            g.setColor(Color.BLACK);
//            g.drawString("No Image",x,y+10);
//        }
//        else
//        {
//            //g.drawImage(image,x,y,(int)(width/scale), (int)(height/scale),null);
//            //g.drawImage(image,x,y,size, size,null);
//            g.drawImage(image,(int)(x0-sizex/2),(int) (y0-sizey/2), sizex, sizey, null);
//        }
        g.setTransform(currentTransform);
        drawTextLabel(g, (float)r.x0, (float)r.y0);

    }
}
