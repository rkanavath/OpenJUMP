package com.cadplan.jump;

import com.vividsolutions.jump.workbench.ui.renderer.style.VertexStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.Viewport;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.Feature;

import java.awt.*;
import java.awt.geom.*;

/**
 * User: geoff
 * Date: 15/06/2007
 * Time: 16:34:46
 * Copyright 2005 Geoffrey G Roy.
 */
public class PolygonVertexStyle extends ExternalSymbolsType
{
    private BasicStroke stroke;
    private int numSides  = 4;
    private boolean showLine = true;
    private boolean showFill = true;
    private float width = 1.0f;
    private double orientation = 0.0;

    private Color lineColor = Color.BLACK;
    private Color fillColor = Color.WHITE;
    private boolean dotted;
    private float x0, y0;
    private String attributeName = "";
    private int attributeIndex = -1;
    private double attributeValue = 0.0;
    private boolean byValue = true;

    public PolygonVertexStyle()
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

    public void setNumSides(int n)
    {
        numSides = n;
    }
    public int getNumSides()
    {
        return numSides;
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
        double absAngle = orientation;
        if(!byValue) absAngle = 0.0;
        double s = ((double) size) /2.0;
        int n = numSides;
        double angle = Math.toRadians(absAngle)  - Math.PI/2.0 + Math.PI/((double)n);
        x0 = (float) shape.getBounds2D().getX() + (float) (s);
        y0 = (float) shape.getBounds2D().getY() + (float) (s);

        for (int i=0; i < n; i++)
        {
            x = (float) (s*Math.cos(angle));
            y = (float) (s*Math.sin(angle));
            if(i == 0) path.moveTo(x0+x,y0-y);
            else path.lineTo(x0+x,y0-y);
            angle = angle + 2.0*Math.PI/(double)n;
        }
        if(n == 2)
        {
            angle = Math.toRadians(absAngle) + Math.PI/((double)n);
            x = (float) (s*Math.cos(angle));
            y = (float) (s*Math.sin(angle));
            path.moveTo(x0+x,y0-y);
            angle = angle + Math.PI;
            x = (float) (s*Math.cos(angle));
            y = (float) (s*Math.sin(angle));
            path.lineTo(x0+x,y0-y);
        }
        else
        {
            angle = Math.toRadians(absAngle) - Math.PI/2.0 + Math.PI/((double)n);
            x = (float) (s*Math.cos(angle));
            y = (float) (s*Math.sin(angle));
            path.lineTo(x0+x,y0-y);
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
        //setSelectedSymbolName(feature);


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
        //setSelectedSymbolName(feature);

        //System.out.println("feature: "+feature.getID()+"  attIndex:"+attributeIndex+"  value="+attributeValue);
        paint(g,p);
    }
    protected void render(Graphics2D g)
    {
        ExternalSymbolsRenderer r = new ExternalSymbolsRenderer();
        GeneralPath path = null;


        path = r.buildShape(ExternalSymbolsRenderer.POLYS, shape, byValue, orientation, size, numSides);

        //GeneralPath path = buildShape();
        float width = 1.0f;
       // if(size > 10.0) width = (float) (size/10.0);
        stroke  = new BasicStroke(width);
        g.setStroke(stroke);
        g.setColor(fillColor);

        //System.out.println("In PolygonType: x0="+r.x0+"  y0="+r.y0);
        AffineTransform currentTransform = g.getTransform();
        double angle= 0.0;
        if(!byValue) angle = attributeValue;
        //System.out.println("Angle = "+angle);
        g.rotate(Math.toRadians(angle), r.x0, r.y0);


        

        if(showFill) g.fill(path);
        g.setColor(lineColor);
        if(showLine) g.draw(path);

        if(dotted)
        {
            g.fillRect((int)(x0-1), (int)(y0-1), 2, 2);
        }
        g.setTransform(currentTransform);

        //System.out.println("Polygon label at:"+r.x0+","+r.y0);
        drawTextLabel(g, r.x0, r.y0);

    }
}
