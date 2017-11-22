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
public class ExternalSymbolsImplType extends ExternalSymbolsType
{
    boolean debug = false;
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
    private String symbolName;
    private int symbolType;
    private int symbolNumber;
    private String defaultSymbolName;
    private int defaultSymbolType;
    private int defaultSymbolNumber;
    //private boolean sizeByScale = false;

    public ExternalSymbolsImplType()
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

    public String getSymbolName()
    {
        return defaultSymbolName;
    }

    public String getActualSymbolName()
    {
        return symbolName;
    }
    

    
    public void setSymbolName(String name)
    {
        boolean debug = false;
        if(debug) System.out.println("setSymbolName - initial call: "+name);
        symbolName = name;
        defaultSymbolName = name;
        if (name == null) return;
        setSelectedSymbolName(name);

        if(debug) System.out.println("setSymbolName: "+symbolName+"  type:"+symbolType+"  symbolNumber"+ symbolNumber+"  numSides="+numSides);
        VertexParams.symbolName = defaultSymbolName;
        VertexParams.symbolType = symbolType;
        VertexParams.symbolNumber = symbolNumber;
        
        defaultSymbolType = symbolType;
        defaultSymbolNumber = symbolNumber;

    }
     private int getNumber(String s)
    {
        int n = -1;
        try
        {
            n = Integer.parseInt(s);
        }
        catch (NumberFormatException ex)
        {
            n = -1;
        }
        return n;
    }

    public int getSymbolNumber()
    {
        return symbolNumber;
    }
    
    public void setSymbolType(int type)
    {
        symbolType = type;
    }
     public int getSymbolType()
    {
        return defaultSymbolType;
    }

    public void setSelectedSymbolName(String name)
    {
        int number = 0;
        int type;
        boolean debug = false;

        if(debug) System.out.println("Setting symbol Name for:"+name);
        if(name.startsWith("@poly"))
        {
             type = ExternalSymbolsRenderer.POLYS;
             number = getNumber(name.substring(5));
        }
        else if (name.startsWith("@star"))
        {
            type = ExternalSymbolsRenderer.STARS;
            number = getNumber(name.substring(5));

        }
        else if(name.startsWith("@any"))
        {
            type = ExternalSymbolsRenderer.ANYS;
            number = getNumber(name.substring(4));

        }
        else if(name.toLowerCase().endsWith(".wkt"))
        {
            type = ExternalSymbolsRenderer.WKTS;
            number = -1;

        }
        else
        {
            type = ExternalSymbolsRenderer.IMAGES;
            number = -1;

        }

        symbolName = name;
        symbolType = type;
        symbolNumber = number;
        if(debug) System.out.println("SymbolName = "+name+"  SymbolType = "+symbolType+"  symbolNumber="+symbolNumber);

    }
    public void setSelectedSymbolName(Feature feature)
    {
        String name = null;
        int number = 0;
        int type;
        boolean debug = false;

        if(debug) System.out.println("\nSetting symbolName for feature: "+feature.getID());
        try
        {
             name = (String) feature.getAttribute("SymbolName");
             if(debug) System.out.println("Feature:"+feature.getID()+"  SymbolName:<"+name+">");
             if(name == null || name.equals("") || name.equals("0"))
             {
                 name = null;
             }

        }
        catch(IllegalArgumentException ex3)
        {
            if(debug) System.out.println("SymbolName.... not found");
            if(debug) System.out.println("Exception   using default symbol :"+symbolName+"  number="+symbolNumber+"  type="+symbolType);
        }
        catch(ClassCastException ex4)
        {
            if(debug) System.out.println("ClassCast Exception   using default symbol :"+symbolName+"  number="+symbolNumber+"  type="+symbolType);
            name = null;
        }
       if(name != null) setSelectedSymbolName(name);
       else
       {
            symbolName = defaultSymbolName; //VertexParams.symbolName;
            symbolNumber = defaultSymbolNumber; //VertexParams.symbolNumber;
            symbolType = defaultSymbolType; //VertexParams.symbolType;
            if(debug) System.out.println("name==null   using default symbol :"+symbolName+"  number="+symbolNumber+"  type="+symbolType);
       }


    }
    public void initialize(Layer layer)
    {
        if(layer == null) return;
        BasicStyle style = layer.getBasicStyle();
        lineColor = GUIUtil.alphaColor(style.getLineColor(), style.getAlpha());
        fillColor = GUIUtil.alphaColor(style.getFillColor(), style.getAlpha());
        ExternalSymbolsImplType vertexStyle = (ExternalSymbolsImplType)layer.getVertexStyle();


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
        setSelectedSymbolName(feature);


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
        setSelectedSymbolName(feature);

        //System.out.println("feature: "+feature.getID()+"  attIndex:"+attributeIndex+"  value="+attributeValue);
        paint(g,p);
    }
    protected void render(Graphics2D g)
    {
        ExternalSymbolsRenderer r = new ExternalSymbolsRenderer();
        GeneralPath path = null;
        WKTshape wktShape = null;
        Image image = null;
        //***XX
        //System.out.println("Rendering type="+symbolType+"  fontSize="+VertexParams.textFontSize+":"+textFontSize);
        //System.out.println("Render: VertexParams.sizeByScale="+VertexParams.sizeByScale);
        Double viewScale = 1.0;
        try
        {
            viewScale = VertexParams.context.getLayerViewPanel().getViewport().getScale();
        }
        catch(Exception ex)
        {
        	System.out.println("Deleting attribute from schema");
        }
        
        //System.out.println("scale="+viewScale);
        int newSize = size;
        double scaleFactor = viewScale*VertexParams.baseScale;
        if(VertexParams.sizeByScale) newSize = (int) (size * scaleFactor);
        //System.out.println("*** setSize:"+VertexParams.size+"  scale="+viewScale+"  new size="+newSize+"  sizeByScale="+VertexParams.sizeByScale);
        if(symbolType == ExternalSymbolsRenderer.POLYS)
        {
            path = r.buildShape(ExternalSymbolsRenderer.POLYS, shape, byValue, orientation, newSize,
                               symbolNumber);
        }
        else if(symbolType == ExternalSymbolsRenderer.STARS)
        {
           path = r.buildShape(ExternalSymbolsRenderer.STARS, shape, byValue, orientation, newSize,
                               symbolNumber);
        }
        else if(symbolType == ExternalSymbolsRenderer.ANYS)
        {
           path = r.buildShape(ExternalSymbolsRenderer.ANYS, shape, byValue, orientation, newSize,
                               symbolNumber);
        }
        else if(symbolType == ExternalSymbolsRenderer.WKTS)
        {
            wktShape = r.getWKTShape(ExternalSymbolsRenderer.WKTS, symbolName, newSize, shape);
        }
        else if(symbolType == ExternalSymbolsRenderer.IMAGES)
        {
            image = r.getImage(ExternalSymbolsRenderer.IMAGES,symbolName,newSize, shape);
        }

        //GeneralPath path = buildShape();
        float width = 1.0f;
       // if(size > 10.0) width = (float) (size/10.0);
        stroke  = new BasicStroke(width);
        g.setStroke(stroke);
        g.setColor(fillColor);

        //System.out.println("\nRender Type:" + symbolType + ":: x0="+r.x0+"  y0="+r.y0);
        AffineTransform currentTransform = g.getTransform();
        //System.out.println("****Current  Transform: "+g.getTransform());
        double angle= orientation;
        if(!byValue) angle = attributeValue;
        //System.out.println("*****Angle = "+angle);
//        g.translate(r.x0, r.y0);
//        g.rotate(Math.toRadians(angle), 0, 0);
        
        

        if(symbolType == ExternalSymbolsRenderer.POLYS)
        {
            
            g.rotate(0.0, r.x0, r.y0);
            if(VertexParams.sizeByScale) g.translate((size-newSize)/2, (size-newSize)/2);
            if(!byValue) g.rotate(Math.toRadians(angle),r.x0, r.y0);
            //System.out.println("Drawing POLY: transform:"+g.getTransform());
            r.paint(g, ExternalSymbolsRenderer.POLYS, path, showLine, lineColor, showFill, fillColor, dotted);
            g.setTransform(currentTransform);
        }
        else if(symbolType == ExternalSymbolsRenderer.STARS)
        {
        	
            g.rotate(0.0, r.x0, r.y0);
            if(VertexParams.sizeByScale) g.translate((size-newSize)/2, (size-newSize)/2);
            if(!byValue) g.rotate(Math.toRadians(angle),r.x0, r.y0);
            r.paint(g, ExternalSymbolsRenderer.STARS, path, showLine, lineColor, showFill, fillColor, dotted);
            g.setTransform(currentTransform);

        }
        else if(symbolType == ExternalSymbolsRenderer.ANYS)
        {
            //g.rotate(0.0, r.x0, r.y0);
           
           if(VertexParams.sizeByScale) g.translate((size-newSize)/2, (size-newSize)/2);
           g.rotate(Math.toRadians(angle), r.x0, r.y0);
            r.paint(g, ExternalSymbolsRenderer.ANYS, path, showLine, lineColor, showFill, fillColor, dotted);
            g.setTransform(currentTransform);

        }
        else if(symbolType == ExternalSymbolsRenderer.WKTS)
        {
            g.translate(r.x0, r.y0);
            if(VertexParams.sizeByScale) g.translate((size-newSize)/2, (size-newSize)/2);
            g.rotate(Math.toRadians(angle), 0, 0);
            //System.out.println("Drawing WKTS: transform:"+g.getTransform());
            r.paint(g, ExternalSymbolsRenderer.WKTS, wktShape, r.size, showLine, lineColor, showFill, fillColor, dotted);
            g.setTransform(currentTransform);

        }
        else if(symbolType == ExternalSymbolsRenderer.IMAGES)
        {
        	if(VertexParams.sizeByScale) g.translate((size-newSize)/2, (size-newSize)/2);
            g.rotate(Math.toRadians(angle), r.x0, r.y0);
            
            r.paint(g, ExternalSymbolsRenderer.IMAGES, image);
            g.setTransform(currentTransform);

        }

//        if(showFill) g.fill(path);
//        g.setColor(lineColor);
//        if(showLine) g.draw(path);
//
//        if(dotted)
//        {
//            g.fillRect((int)(x0-1), (int)(y0-1), 2, 2);
//        }
        //System.out.println("****New Transform: "+g.getTransform());
        //System.out.println("Polygon label at:"+r.x0+","+r.y0);
        
        if(VertexParams.sizeByScale) g.translate((size-newSize)/2, (size-newSize)/2);
        //System.out.println("drawTextLabel call=<"+">"+  "fontsize="+textFontSize+":"+VertexParams.textFontSize);
        drawTextLabel(g, r.x0, r.y0);
        g.setTransform(currentTransform);
        //System.out.println("****Reset Transform: "+g.getTransform());

        

    }
}
