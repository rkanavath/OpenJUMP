package com.cadplan.jump;

import com.vividsolutions.jump.workbench.ui.renderer.style.VertexStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.Viewport;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.feature.Feature;

import java.awt.geom.*;
import java.awt.*;

/**
 * User: geoff
 * Date: 20/06/2007
 * Time: 08:32:25
 * Copyright 2005 Geoffrey G Roy.
 */
public class AnyShapeVertexStyle extends ExternalSymbolsType
{
    private int type = 0;
    //private int size = 10;
    private Double orientation = 0.0;
    private boolean showLine = true;
    private boolean showFill = true;
    private boolean dotted = false;
    private Color lineColor = Color.BLACK;
    private Color fillColor = Color.WHITE;
    private float x0 = 0.0f;
    private float y0 = 0.0f;
    private String attributeName = "";
    private int attributeIndex = -1;
    private double attributeValue = 0.0;
    private boolean byValue = true;
   public AnyShapeVertexStyle()
    {
       super(new Ellipse2D.Double());
       ((RectangularShape)shape).setFrame(0.0,0.0,getSize(),getSize());
    }

    public void setSize(int size)
    {
        this.size = size;
    }

    public int getSize()
    {
        return size;
    }

    public void setType(int n)
    {
        type = n;
    }
    public int getType()
    {
        return type;
    }

    public void setShowLine(boolean show)
    {
        this.showLine = show;
    }
    public boolean getShowLine()
    {
        return showLine;
    }

    public void setShowFill(boolean show)
    {
        this.showFill = show;
    }

    public boolean getShowFill()
    {
        return showFill;
    }

    public void setOrientation(double orientation)
    {
        this.orientation = orientation;
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
    public void initialize(Layer layer)
    {
        if(layer == null) return;
        BasicStyle style = layer.getBasicStyle();
        lineColor = GUIUtil.alphaColor(style.getLineColor(), style.getAlpha());
        fillColor = GUIUtil.alphaColor(style.getFillColor(), style.getAlpha());
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

    public void setColors(Color lineColor, Color fillColor)
    {
        this.lineColor = lineColor;
        this.fillColor = fillColor;
    }

    private GeneralPath buildShape()
    {
        GeneralPath path = new GeneralPath();
        float x, y;
        float s = (float) (((double) size) /2.0);
        float s2 = s;
        x0 = (float) shape.getBounds2D().getX() + (float) (s);
        y0 = (float) shape.getBounds2D().getY() + (float) (s);

        switch (type)
        {
            case 0:
                path.moveTo(x0-s,y0);
                path.lineTo(x0+s,y0);
                path.moveTo(x0,y0);
                path.lineTo(x0-s/2,y0-s/2);
                path.moveTo(x0,y0);
                path.lineTo(x0,y0-s);
                path.moveTo(x0,y0);
                path.lineTo(x0+s/2,y0-s/2);

                break;

            case 1:
                path.moveTo(x0-s,y0);
                path.lineTo(x0+s,y0);
                path.moveTo(x0+s/2,y0+s/2);
                path.lineTo(x0-s/2,y0-s/2);
                path.moveTo(x0,y0+s);
                path.lineTo(x0,y0-s);
                path.moveTo(x0-s/2,y0+s/2);
                path.lineTo(x0+s/2,y0-s/2);
                break;

            case 2:
                path.moveTo(x0-s,y0);
                path.lineTo(x0+s,y0);
                path.moveTo(x0,y0-s);
                path.lineTo(x0,y0+s);

                path.moveTo(x0+s/2,y0);
                for (int i=0; i <= 16; i++)
                {
                    double angle = (double)i*2.0*Math.PI/16.0;
                    path.lineTo((float)(x0+(s/2)*Math.cos(angle)), (float)(y0-(s/2)*Math.sin(angle)));
                }
                break;

            case 3:
                path.moveTo(x0-s,y0);
                path.lineTo(x0+s,y0);
                path.moveTo(x0,y0-s);
                path.lineTo(x0,y0+s);

                path.moveTo(x0-s/2,y0-s/2);
                path.lineTo(x0+s/2,y0-s/2);
                path.lineTo(x0+s/2,y0+s/2);
                path.lineTo(x0-s/2,y0+s/2);
                path.lineTo(x0-s/2,y0-s/2);

                break;

            case 4:
                path.moveTo(x0,y0);
                path.lineTo(x0,y0-s2/2);


                path.moveTo(x0+s2/4,y0-3*s2/4);
                for (int i=0; i <= 16; i++)
                {
                    double angle = (double)i*2.0*Math.PI/16.0;
                    path.lineTo((float)(x0+(s2/4)*Math.cos(angle)), (float)(y0-(3*s2/4)-(s2/4)*Math.sin(angle)));
                }
                break;

            case 5:
                path.moveTo(x0,y0);
                path.lineTo(x0,y0-s2/2);
                path.lineTo(x0,y0-s2);
                path.lineTo(x0+s2/2,y0-3*s2/4);
                path.lineTo(x0,y0-s2/2);
                break;

            case 6:
                path.moveTo(x0,y0);
                path.lineTo(x0,y0-s2);
                path.moveTo(x0-s2/2,y0);
                path.lineTo(x0,y0-s2/2);
                path.lineTo(x0+s2/2,y0);
                path.lineTo(x0-s2/2,y0);
                break;
        }
        return path;
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
    
    protected void render(Graphics2D g)
    {
        //GeneralPath path = buildShape();
        ExternalSymbolsRenderer r = new ExternalSymbolsRenderer();
        GeneralPath path = r.buildShape(ExternalSymbolsRenderer.ANYS, shape, byValue, orientation,size,type);
        float width = 1.0f;
       // if(size > 10.0) width = (float) (size/10.0);
        BasicStroke stroke  = new BasicStroke(width);
        g.setStroke(stroke);
        g.setColor(fillColor);

        AffineTransform currentTransform = g.getTransform();
        double angle = orientation;
        if(!byValue) angle = attributeValue;
        g.rotate(Math.toRadians(angle), r.x0, r.y0);

        r.paint(g,ExternalSymbolsRenderer.ANYS,path, showLine, lineColor, showFill, fillColor, dotted);
//        if(showFill) g.fill(path);
//        g.setColor(lineColor);
//        if(showLine) g.draw(path);
        g.setTransform(currentTransform);
//        if(dotted)
//        {
//            g.fillRect((int)(x0-1), (int)(y0-1), 2, 2);
//        }
        drawTextLabel(g, r.x0, r.y0);

    }


}
