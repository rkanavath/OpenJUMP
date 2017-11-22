package com.cadplan.jump;

import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;

/**
 * User: geoff
 * Date: 2/08/2007
 * Time: 13:51:44
 * Copyright 2005 Geoffrey G Roy.
 */
public class HTMLTextSegment
{
    private boolean debug = false;
    private String line;
    private int size;
    private int style;
    private double height;
    private double width;
    private double lineSpace;
    private String fontName;
    private Font font;


    public HTMLTextSegment(Graphics2D g, String line, String fontName, int size, int style)
    {
        this.fontName = fontName;
        this.line = line;
        this.size = size;
        this.style = style;

        font = new Font(fontName,style,size);
        FontRenderContext frc = g.getFontRenderContext();
        TextLayout layout = new TextLayout(line, font, frc);
        lineSpace = (layout.getAscent()+layout.getDescent());
        Rectangle2D bounds = layout.getBounds();
        width = bounds.getWidth();
        height = bounds.getHeight();
    }

    public double getHeight()
    {
        return height;
    }
    public double getWidth()
    {
       return width;
    }
    public Font getFont()
    {
       return font;
    }

}
