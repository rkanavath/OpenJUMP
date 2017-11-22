package com.cadplan.jump;

import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.io.WKTReader;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.GeneralPath;
import java.awt.geom.Area;

/**
 * User: geoff
 * Date: 8/07/2007
 * Time: 10:08:28
 * Copyright 2005 Geoffrey G Roy.
 */
public class WKTshape
{
    public int lineWidth;
    public int extent;
    public String wktText;

    public WKTshape(int width, int extent, String text)
    {
        this.lineWidth = width;
        this.extent = extent;
        this.wktText = text;
    }

    public void paint(Graphics2D g, int size, boolean showLine, Color lineColor, boolean showFill, Color fillColor, boolean dotted)
    {
        Geometry geometry = null;
        double scale = (double) size / (double) extent;
        Stroke stroke  = new BasicStroke(lineWidth);
        g.setStroke(stroke);
        
        WKTReader testReader = new WKTReader();
        try
        {
             geometry = testReader.read(wktText);
        }
        catch (com.vividsolutions.jts.io.ParseException ex)
        {
           //any error shoule be detected on lading file
        }
        
        if(geometry instanceof LineString )
        {
            paintLineString(g, (LineString) geometry, size, showLine, lineColor, showFill, fillColor, dotted, scale);

        }
        else if(geometry instanceof MultiLineString)
        {
            int numLineString = ((GeometryCollection)geometry).getNumGeometries();
            for (int k=0; k < numLineString; k++)
            {
                LineString ls = (LineString) ((GeometryCollection)geometry).getGeometryN(k);
                paintLineString(g, ls, size, showLine, lineColor, showFill, fillColor, dotted, scale);

            }
        }
        else if (geometry instanceof Polygon )
        {
            paintPolygon(g, (Polygon) geometry, size, showLine, lineColor, showFill, fillColor, dotted, scale);
        }
        else if(geometry instanceof MultiPolygon)
        {
             int numPolygons = ((MultiPolygon) geometry).getNumGeometries();
             for (int k=0; k < numPolygons; k++)
             {
                 Polygon p = (Polygon) ((MultiPolygon) geometry).getGeometryN(k);
                 paintPolygon(g, p, size, showLine, lineColor, showFill, fillColor, dotted, scale);
             }
        }
        else if(geometry instanceof GeometryCollection)
        {
             int numGeometries = ((GeometryCollection)geometry).getNumGeometries();
             for (int i=0; i < numGeometries; i++)
             {
                 Geometry pg = ((GeometryCollection)geometry).getGeometryN(i);
                 if(pg instanceof LineString )
                {
                    paintLineString(g, (LineString) pg, size, showLine, lineColor, showFill, fillColor, dotted, scale);

                }
                else if(pg instanceof MultiLineString)
                 {
                    int numLineString = ((GeometryCollection)pg).getNumGeometries();
                    for (int k=0; k < numLineString; k++)
                    {
                        LineString ls = (LineString) ((GeometryCollection)pg).getGeometryN(k);
                        paintLineString(g, ls, size, showLine, lineColor, showFill, fillColor, dotted, scale);

                    }
                 }
                else if (pg instanceof Polygon )
                {
                    paintPolygon(g, (Polygon)pg, size, showLine, lineColor, showFill, fillColor, dotted, scale);
                }
                 else if(pg instanceof MultiPolygon)
                 {
                     int numPolygons = ((MultiPolygon) pg).getNumGeometries();
                     for (int k=0; k < numPolygons; k++)
                     {
                         Polygon p = (Polygon) ((MultiPolygon) pg).getGeometryN(k);
                         paintPolygon(g, p, size, showLine, lineColor, showFill, fillColor, dotted, scale);
                     }
                 }
             }
        }
    }

    private void paintLineString(Graphics2D g, LineString geometry, int size, boolean showLine, Color lineColor,
                                 boolean showFill, Color fillColor, boolean dotted, double scale)
    {
            //int numGeometries = ((GeometryCollection)geometry).getNumGeometries();
            GeneralPath path = new GeneralPath();
            float x, y;
            //for (int k=0; k < numGeometries; k++)
            //{
               //Geometry pgeometry = ((GeometryCollection)geometry).getGeometryN(k);
                Coordinate [] coords = geometry.getCoordinates();


                x = (float) (coords[0].x * scale);
                y = -(float) (coords[0].y * scale);
                path.moveTo(x,y);
                for (int i=1; i < coords.length; i++)
                {
                    x = (float) (coords[i].x * scale);
                    y = -(float) (coords[i].y * scale);
                    path.lineTo(x,y);
                }

            //}
            if(showLine)
            {
                g.setColor(lineColor);
                g.draw(path);
            }
    }

    private void paintPolygon(Graphics2D g, Polygon geometry, int size, boolean showLine, Color lineColor,
                              boolean showFill, Color fillColor, boolean dotted, double scale)
    {
            //int numGeometries = geometry.getNumGeometries();
            GeneralPath path;
            Area outer = null, hole = null;
            Area total = new Area();
            float x, y;
            //for(int k=0; k < numGeometries; k++)
            //{
                Polygon pgeometry = geometry; // ((GeometryCollection)geometry).getGeometryN(k);
                int numRings = ((Polygon)geometry).getNumInteriorRing();
                for(int j=0; j < numRings+1; j++)
                {
                    LineString lineString;
                    path = new GeneralPath();
                    if(j == 0) lineString= pgeometry.getExteriorRing();
                    else lineString = pgeometry.getInteriorRingN(j-1);
                    Coordinate [] coords = lineString.getCoordinates();

                    x = (float) (coords[0].x * scale);
                    y = -(float) (coords[0].y * scale);
                    path.moveTo(x,y);
                    for (int i=1; i < coords.length; i++)
                    {
                        x = (float) (coords[i].x * scale);
                        y = -(float) (coords[i].y * scale);
                        path.lineTo(x,y);
                    }
                    if(j == 0)
                    {
                        outer = new Area(path);
                    }
                    else
                    {
                        hole = new Area(path);
                        outer.subtract(hole);
                    }

                }
//                if(k == 0)
//                {
//                    total = new Area(outer);
//                }
//                else
//                {
//                    total.add(outer);
//                }
            //}
            if(showFill)
            {
                g.setColor(fillColor);
                g.fill(outer);
            }
            if(showLine)
            {
               g.setColor(lineColor);
               g.draw(outer);
            }
    }
}
