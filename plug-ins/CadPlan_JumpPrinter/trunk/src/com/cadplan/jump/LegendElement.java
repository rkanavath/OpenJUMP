package com.cadplan.jump;

import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.VertexStyle;

import java.awt.*;
import java.util.Collection;
import java.util.Set;

/**
 * User: geoff
 * Date: 12/02/2007
 * Time: 15:35:12
 * Copyright 2005 Geoffrey G Roy.
 */
public class LegendElement
{
    public boolean include = false;
    public String name;
    public Color lineColor;
    public Color fillColor;
    public Stroke lineStroke;
    public Paint fillPattern;
    public Collection themeStyles;
    public Collection keyValues;
    public VertexStyle vertexStyle;
    public boolean showFill = true;
    public boolean showLine = true;



    public LegendElement(boolean include, String name, Color lineColor, Stroke lineStroke,
                          Color fillColor, Paint fillPattern, Collection themeStyles, Collection keyValues, VertexStyle vertexStyle,
                          boolean showLine, boolean showFill)
    {
        this.include = include;
        this.name = name;
        this.lineColor = lineColor;
        this.lineStroke = lineStroke;
        this.fillColor = fillColor;
        this.fillPattern = fillPattern;
        this.themeStyles = themeStyles;
        this.keyValues = keyValues;
        this.vertexStyle = vertexStyle;
        this.showLine = showLine;
        this.showFill = showFill;
        
    }

    public String toString()
    {
        return name+"["+lineColor+lineStroke+":"+fillColor+":"+fillPattern+"]<"+include+">";
    }
}
