package com.cadplan.jump;

import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.ColorThemingStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.Style;
import com.vividsolutions.jump.workbench.ui.renderer.style.VertexStyle;

import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;
import java.awt.geom.GeneralPath;
import java.awt.image.BufferedImage;
import java.text.DecimalFormat;
import java.util.*;

/**
 * User: geoff
 * Date: 12/02/2007
 * Time: 14:47:51
 * Copyright 2005 Geoffrey G Roy.
 */
public class LayerLegend   extends Furniture
{
    private boolean debug = false;
    Vector<LegendElement> legendItems = null;
    boolean border = true;
    String legendName  = null;
    boolean showTitle= true;
    Font legendFont = new Font("SansSerif", Font.PLAIN, 12);
    Color legendTitleColor = Color.BLACK;
    Color legendTextColor = Color.BLACK;
    Color legendBorderColor = Color.BLACK;
    int yspace = 14;
    int width = 48;  // must be 4*height
    int height = 12;
    Color noColor = Color.WHITE;
    double fscale = 1.0;
    int size;
    I18NPlug iPlug;
    boolean horizontal = true;

    public LayerLegend(PlugInContext context, Rectangle location)
    {
        this.location = location;
        legendItems = new Vector<LegendElement>(10,5);
        updateLegend(context, null);
        layerNumber = 70;
    }


    public void updateLegend(PlugInContext context,I18NPlug iPlug)
    {
        Vector<LegendElement> tempItems = new Vector<LegendElement>(10,5);
        this.iPlug = iPlug;
        if(iPlug != null && legendName == null) legendName = iPlug.get("JumpPrinter.Furniture.LayerLegend");
        boolean showLine;
        boolean showFill;
        if(legendItems != null)
        {
            for (int i=0; i < legendItems.size(); i++)
            {
                tempItems.addElement(legendItems.elementAt(i));
            }
        }
        legendItems = new Vector<LegendElement>(10,5);
        java.util.List layerCollection = context.getLayerViewPanel().getLayerManager().getVisibleLayers(true);

        Iterator i = layerCollection.iterator();
        int count = 0;
        while (i.hasNext())
        {
            boolean theming = false; // we are only going to include layers that have a theming style
            Layer layer = (Layer) i.next();
            String name = layer.getName();
            String desc = layer.getDescription();
            if(debug) System.out.println("layer "+count+":"+name+","+desc);
            BasicStyle basicStyle = layer.getBasicStyle();
            VertexStyle vertexStyle = layer.getVertexStyle();
            Collection themeStyles = null;

             ColorThemingStyle themeStyle = (ColorThemingStyle) layer.getStyle(ColorThemingStyle.class);
             if(debug) System.out.println("       Theming enabled: "+ themeStyle.isEnabled());
             Collection keyValues = null;
             if(themeStyle.isEnabled())
             {
                  Map themeMap =   themeStyle.getAttributeValueToBasicStyleMap();
                  Map labelMap = themeStyle.getAttributeValueToLabelMap();
                  
                  
                  
                  themeStyles = themeMap.values();
                  Iterator it = themeStyles.iterator();
                 // while(it.hasNext())
                 // {
                //	  BasicStyle style = (BasicStyle) it.next();
                 //      System.out.println("Theme Map:"+style.getFillPattern());
                //  }
                  theming = true;
                  keyValues = labelMap.values();
                 if(debug) System.out.println("lableMap:"+labelMap.toString());
                 if(debug) System.out.println("mapKeys:"+keyValues.toString());

             }
            if(theming)
            {
                int alpha = basicStyle.getAlpha();
                Color lineColor = basicStyle.getLineColor();
                lineColor = new Color(lineColor.getRed(), lineColor.getGreen(), lineColor.getBlue(), alpha);
                showLine = true;
                if(!basicStyle.isRenderingLine())showLine = false;

                Color fillColor = basicStyle.getFillColor();
                fillColor = new Color(fillColor.getRed(), fillColor.getGreen(), fillColor.getBlue(), alpha);
                showFill = true;

                if(!basicStyle.isRenderingFill()) showFill = false;


                String linePattern = basicStyle.getLinePattern();
                Stroke lineStroke = basicStyle.getLineStroke();
                Paint fillPattern =basicStyle.getFillPattern();
                if(!basicStyle.isRenderingFillPattern())  fillPattern = null;

               // System.out.println("************ Theming i="+i.hashCode()+"\nLineColor: "+basicStyle.getLineColor()+"  FillColor:"+basicStyle.getFillColor()+"  Pattern:"+fillPattern);
                // create new legend item, include by default
                LegendElement legendElement = new LegendElement(true, name, lineColor, lineStroke, fillColor, fillPattern,
                        themeStyles, keyValues, vertexStyle, showLine, showFill);

                //set include on existing items
                for (int k=0; k < tempItems.size(); k++)
                {
                    LegendElement anitem = tempItems.elementAt(k);
                    if(anitem.name.equals(legendElement.name))
                    {
                        legendElement.include = anitem.include;
                    }
                }
                legendItems.addElement(legendElement);
                //System.out.println(count+": "+legendElement.toString());
                count ++;
            }
        }
    }



    public void paint(Graphics g, double scale, double globalScale, int printingMode)
    {
         //System.out.println("Painting layerLegend");
          Graphics2D g2 = (Graphics2D) g;
          Font bigFont = new Font(legendFont.getFontName(),Font.BOLD, legendFont.getSize());
          Font smallFont = legendFont;
          //System.out.println("legendFont:"+legendFont);

          FontMetrics fm = getFontMetrics(smallFont);
          FontMetrics fmt = getFontMetrics(bigFont);
          int fontHeight = fmt.getHeight()+2*fmt.getDescent();
          int patchx = 12;      // color patch size
          int patchy = 12;
          if(patchx < fontHeight/2)
          {
        	  patchx = fontHeight/2;
        	  patchy = patchx;
          }
          int patchspace = 15;
          if(patchspace < fontHeight) patchspace = fontHeight+2;
          int width = 100;
          int height = 100;
          int totalWidth = 0;
          int totalHeight = 0;
          int sWidth = 0;
          int sHeight = 0;
          int maxX = 0;
          int maxY = 0;
          int numIncluded = 0;
          double maxVertexSize = 0.0;
          int y=0;
          int maxlen = 2*width; //fmt.stringWidth("Legend");
          if(scale > 0.0)
          {
             g2.scale(scale, scale);
              fscale = scale;
          }
          float factor = 1.0f;
          if(printingMode == 2) factor = (float) (1.0/MapImagePrinter.drawFactor);
          if(printingMode != 2) factor = (float) fscale;   // to fix line properties!
          g.setColor(legendTitleColor);
          g.setFont(bigFont);
          String name = legendName; 
          if(name == null) name = iPlug.get("JumpPrinter.Furniture.LayerLegend");
          if(showTitle)
          {
             g.drawString(name,location.x+5, location.y+fontHeight/2+5);
          }
          int tw = fmt.stringWidth(name)+15; //-patchx;
          //System.out.println("name=<"+name+"> tw="+tw+ " patchx="+patchx);
          
          if(tw < 0) tw = 0;
          int xp = location.x;
          int yp;
          if(showTitle) yp = location.y + 3*fontHeight/2+10;
          else yp = location.y+ fontHeight/2+15;
          for (int i=0; i < legendItems.size(); i++)
          {
              LegendElement element = legendItems.elementAt(i);
              
              //System.out.println(i+": "+element.toString());
              
              if(i == 0) sWidth = tw;
              else sWidth = 0;
              if(!horizontal) sWidth = 0;
              sHeight = patchspace;
              if(horizontal) sHeight = 0;
              //System.out.println("sWidth="+sWidth);
              if(element.include)
              {
                   
                   numIncluded++;
                   g.setFont(smallFont);
                   g.setColor(legendTextColor);
                   int wt = fm.stringWidth(element.name)+5;
                   if(wt - patchx > sWidth) sWidth = wt - patchx;
                   if(sWidth < 0) sWidth = 0;
                   g.drawString(element.name,xp+5,yp-fontHeight/2);

                   Iterator it = element.themeStyles.iterator();
                   Iterator itm = element.keyValues.iterator();
                   int count = 0;
                   
                   while (it.hasNext())
                   {
                       BasicStyle style = (BasicStyle) it.next();
                       //keyMap = itm.next();
                       String keyName = itm.next().toString();
                       if(debug) System.out.println("keyName="+keyName);
                       Color color = style.getFillColor();
                       
                       Paint fillPattern = style.getFillPattern();
                       
                       //System.out.println("** Fill pattern:"+fillPattern);
                       
                       int alpha = style.getAlpha();
                       Color fillColor = new Color(color.getRed(), color.getGreen(), color.getBlue(), alpha);
                       g.setColor(fillColor);
                       
                       // remove for the moment as fillPattern seems to be wrong
                       //if(fillPattern != null) g2.setPaint(fillPattern);
                       
                       g.fillRect(xp+5, yp+patchspace*count, patchx, patchy);
                       
                       //System.out.println("Filling patch: color:"+color+"  alpha:"+alpha+"  fillColor:"+fillColor+" pattern:"+fillPattern);
                       g.setColor(Color.BLACK);
                       g.drawRect(xp+5, yp+patchspace*count, patchx, patchy);
                       g.setFont(smallFont);
                       int w = fm.stringWidth(keyName)+5;
                       if(w > sWidth) sWidth = w;
                       
                       //System.out.println(" update sWidth="+sWidth);
                       g.setColor(legendTextColor);
                       g.drawString(clean(keyName),xp+5+patchx+5,yp+patchspace*count+patchy); //  /2+5);
                       if(debug) System.out.println("    count="+count+" color:+"+fillColor);
                       count++;

                   }
                   if(count*patchspace > maxY) maxY = count*patchspace;
                   if(count*fontHeight > maxY) maxY = count*fontHeight;
                   sHeight = maxY; // + patchspace;




                   int len = fm.stringWidth(element.name);
                   if(len > maxlen) maxlen = len;
                   if(horizontal) xp = xp+5+patchx+5 + sWidth; //width;
                   //if(!horizontal) yp = yp + count*patchspace +2*fontHeight;
                   if(!horizontal) yp = yp + sHeight + fontHeight;
              }
              if(horizontal)
              {
            	  totalWidth = totalWidth + 5+patchx+5 + sWidth;
            	  if(totalHeight < sHeight) totalHeight = sHeight;
              }
              else // vertical
              {
            	  totalHeight = totalHeight + sHeight +20;
            	  if(totalWidth < 5+patchx+5 + sWidth) totalWidth = 5+patchx+5 + sWidth ;
            	  if(totalWidth < tw+10) totalWidth = tw+10;
              }
              
              
          }
          g2.setStroke(new BasicStroke());
          location.height = totalHeight + 2*fontHeight ; // +15 + patchspace; //maxY + 2*fontHeight+15 ;
          if(showTitle) location.height = totalHeight + 2*fontHeight + 10;
          location.width = totalWidth; //numIncluded*(width);
          g.setColor(legendBorderColor);
          if(border) g.drawRect(location.x, location.y, location.width, location.height);

          if(scale > 0.0)
          {
            g2.setColor(boundsColor);
            g2.setStroke(boundsStroke);
            g2.drawRect(location.x-2, location.y-2, location.width+4,  location.height+4);
            g2.setStroke(normalStroke);
            g2.scale(1.0/scale, 1.0/scale);
          }
    }

  private String clean(String value)
  {
	  String result;
	  
	  try
	  {
		  int intval = Integer.parseInt(value);
		  result = String.valueOf(intval);
	  }
	  catch (NumberFormatException ex)
	  {
		  try
		  {
			  double doubval = Double.parseDouble(value);
			  String fstring = "####.#";
			  if(doubval < 10) fstring = "###.##";
			  if(doubval < 1) fstring = "###.###";
			  if(doubval < 0.01) fstring = "#.####E##";
			  DecimalFormat df = new DecimalFormat(fstring);
			  result = df.format(doubval);
		  }
		  catch (Exception ex2)
		  {
			  result = value;
		  }
	  }	  
	  return result;
  }
}
