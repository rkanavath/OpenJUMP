package com.cadplan.jump;

import com.vividsolutions.jump.workbench.ui.renderer.style.VertexStyle;
import com.vividsolutions.jump.workbench.ui.Viewport;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollectionWrapper;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Envelope;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;
import java.util.Date;
import java.util.StringTokenizer;
import java.util.Vector;
import java.text.SimpleDateFormat;
import java.text.DecimalFormat;

/**
 * Created by IntelliJ IDEA.
 * User: geoff
 * Date: 14/07/2007
 * Time: 12:33:28
 * To change this template use File | Settings | File Templates.
 */
public class ExternalSymbolsType extends VertexStyle
{
    boolean debug = false;
    protected boolean textEnabled = false;
    protected String textAttributeName = null; // name of selected attribute
    protected String textAttributeValue = "";
    protected int textAttributeIndex = -1;
    protected String textFontName = "SansSerif";
    protected int textFontSize = 10;
    protected int textFontStyle = Font.PLAIN;
    protected int textFontJustification = 0;
    protected int textPosition = 0;
    protected int textOffset = 1;
    protected int textOffsetValue = 0;
    protected int textScope = 0;  // 0- points, 1 - lines, 2 - polygons
    private boolean drawIt = true;
    protected Color textBackgroundColor = Color.WHITE;
    protected Color textForegroundColor = Color.BLACK;
    protected boolean textFill = false;
    protected int textBorder = 0; // border style
    protected Viewport viewport;
    private DecimalFormat df;
    private int showNoteIndex = -1;
    private boolean showNote = true;
    private double alpha = 1.0;
    private Color transBackColor;
    private double drawFactor = 1.0;
    protected  boolean sizeByScale = false;


    public ExternalSymbolsType()
    {

    }
    public ExternalSymbolsType(Shape shape)
    {
        super(shape);
    }

    public void setSizeByScale(boolean sizeByScale)
    {
    	this.sizeByScale =  sizeByScale;
    	//System.out.println("Setting sizeByScale:"+sizeByScale);
    	VertexParams.sizeByScale = sizeByScale;
    }
    public boolean getSizeByScale()
    {
    	return sizeByScale;
    }  
    
    public void setTextEnabled(boolean enabled)
    {
        this.textEnabled = enabled;
        VertexParams.textEnabled = enabled;
    }
    public boolean getTextEnabled()
    {
        return textEnabled;
    }
    public void setTextAttributeName(String attribute)
       {
           this.textAttributeName = attribute;
           VertexParams.attTextName = attribute;
       }
       public String getTextAttributeName()
       {
           return textAttributeName;
       }

    public void setTextFontName(String fontName)
       {
           this.textFontName = fontName;
           VertexParams.textFontName = fontName;
       }
       public String getTextFontName()
       {
           return textFontName;
       }

    public void setTextFontSize(int fontSize)
       {
           this.textFontSize = fontSize;
           //System.out.println("Setting fontSize:"+fontSize);
           VertexParams.textFontSize = fontSize;
       }
       public int getTextFontSize()
       {
           return textFontSize;
       }
    public void setTextFontStyle(int fontStyle)
       {
           this.textFontStyle = fontStyle;
           VertexParams.textStyle = fontStyle;
       }
       public int getTextFontStyle()
       {
           return textFontStyle;
       }

    public void setTextFontJustification(int fontJustification)
       {
           this.textFontJustification = fontJustification;
           VertexParams.textJustification = fontJustification;
       }
       public int getTextFontJustification()
       {
           return textFontJustification;
       }

    public void setTextPosition(int position)
       {
           this.textPosition = position;
           VertexParams.textPosition = position;
       }
       public int getTextPosition()
       {
           return textPosition;
       }

    public void setTextOffset(int offset)
       {
           this.textOffset = offset;
           VertexParams.textOffset = offset;
       }
       public int  getTextOffset()
       {
           return textOffset;
       }
    public void setTextOffsetValue(int offset)
       {
           this.textOffsetValue = offset;
           VertexParams.textOffsetValue = offset;
       }
       public int getTextOffsetValue()
       {
           return textOffsetValue;
       }

    public void setTextScope(int scope)
       {
           this.textScope = scope;
           VertexParams.textScope = scope;
       }
       public int getTextScope()
       {
           return textScope;
       }

    public void setTextBackgroundColor(Color backgroundColor)
       {
           this.textBackgroundColor = backgroundColor;
           VertexParams.textBackgroundColor = backgroundColor;
       }
       public Color getTextBackgroundColor()
       {
           return textBackgroundColor;
       }

    public void setTextForegroundColor(Color foregroundColor)
       {
           this.textForegroundColor = foregroundColor;
           VertexParams.textForegroundColor = foregroundColor;
       }
       public Color getTextForegroundColor()
       {
           return textForegroundColor;
       }
    public void setTextFill(boolean fill)
       {
           this.textFill = fill;
           VertexParams.textFill = fill;
       }
       public boolean getTextFill()
       {
           return textFill;
       }

    public void setTextBorder(int border)
       {
           this.textBorder = border;
           VertexParams.textBorder = border;
       }
       public int getTextBorder()
       {
           return textBorder;
       }

    public void setDrawFactor(double drawFactor)
    {
        this.drawFactor = drawFactor;
    }

    public void initializeText(Layer layer)
    {
        FeatureCollectionWrapper fcw = layer.getFeatureCollectionWrapper();
        if(fcw == null) return;
        FeatureSchema featureSchema = fcw.getFeatureSchema();
         try
        {

            if(textAttributeName != null && !textAttributeName.equals(""))
                     textAttributeIndex = featureSchema.getAttributeIndex(textAttributeName);

        }
        catch (Exception ex)
        {
            if(textAttributeName.equals("$FID")) textAttributeIndex = -1;
            else if(textAttributeName.equals("$POINT")) textAttributeIndex = -2;
            else textAttributeIndex = -1;
        }
        try
        {
            showNoteIndex = featureSchema.getAttributeIndex("ShowLabel");
        }
        catch(Exception ex)
        {
            showNoteIndex = -1;
        }
        alpha = layer.getBasicStyle().getAlpha()/255.0;
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
    public boolean getShowNote()
    {
        return showNote;
    }

    public String getTextAttributeValue()
    {
        return textAttributeValue;
    }

    public void setTextAttributeValue(Feature feature)
    {
        if(textAttributeIndex == -1) textAttributeValue = String.valueOf(feature.getID());
        if(textAttributeIndex == -2) textAttributeValue = "$POINT";

        try
        {
              if(showNoteIndex >= 0) showNote = (feature.getInteger(showNoteIndex) == 1);
        }
        catch (Exception ex)
        {
            showNote = true;
        }

        if(textEnabled && textAttributeIndex >= 0)
        {
            Object obj = feature.getAttribute(textAttributeIndex);
            try
            {
                Date date = (Date) obj;
                SimpleDateFormat df = new SimpleDateFormat("yyyy/MM/dd'|'HH:mm:ss");
                textAttributeValue = df.format(date).trim();
                //System.out.println("Date formated: <"+textAttributeValue+">");
                if(textAttributeValue.endsWith("|00:00:00"))
                {
                    int k = textAttributeValue.indexOf("|");
                    textAttributeValue = textAttributeValue.substring(0,k).trim();
                }
                //System.out.println("date: "+textAttributeValue);
            }
            catch (Exception ex4)
            {
                try
                {
                    textAttributeValue = feature.getString(textAttributeIndex);
                }
                catch (Exception ex)
                {
                    try
                    {
                        textAttributeValue = String.valueOf(feature.getDouble(textAttributeIndex));
                    }
                    catch (Exception ex2)
                    {
                        try
                        {
                            textAttributeValue = String.valueOf(feature.getInteger(textAttributeIndex));
                        }
                        catch (Exception ex3)
                        {
                            textAttributeValue = "Error";
                        }
                    }
                }
            }

        }
        Geometry geometry = feature.getGeometry();
        if(geometry instanceof com.vividsolutions.jts.geom.Point || geometry instanceof com.vividsolutions.jts.geom.MultiPoint)
        {
            drawIt = (textScope & 1) > 0;
        }
        else if(geometry instanceof com.vividsolutions.jts.geom.LineString || geometry instanceof com.vividsolutions.jts.geom.MultiLineString)
        {
            drawIt = (textScope & 2) > 0;
        }
        else if(geometry instanceof com.vividsolutions.jts.geom.Polygon || geometry instanceof com.vividsolutions.jts.geom.MultiPolygon)
        {
            drawIt = (textScope & 4) > 0;
        }
    }

    public void drawTextLabel(Graphics2D g, float x0, float y0)
    {
        Point2D modelPoint = null;
        //System.out.println("drawPlain text 00 =<"+textAttributeValue+"> + shownote:"+showNote);
        //System.out.println("drawTextLabel text=<"+">"+  "fontsize="+textFontSize);
        //System.out.println("Point origin0: "+x0+","+y0);
        //if(!showNote) return; //**********************************************************
        if(textAttributeIndex == -2)   // $POINT
        {
           // double viewScale = 1.0/viewport.getScale();
            //Point2D origin = viewport.getOriginInModelCoordinates();
            //Envelope bounds = viewport.getEnvelopeInModelCoordinates();

            try
            {
                modelPoint = viewport.toModelPoint(new Point2D.Double(x0,y0));
            }
            catch (Exception ex)
            {
                modelPoint = new Point2D.Double(0.0,0.0);
            }
            textAttributeValue = "("+format(modelPoint.getX())+":"+format(modelPoint.getY())+")";
            //System.out.println("Point: "+x0+","+y0+ " :: "+modelPoint+" :: "+textAttributeValue);
        }
         //System.out.println("drawPlain text 01:"+"textEnabled:"+textEnabled+" drawIt:"+drawIt+" value=<"+textAttributeValue+">");
        if(textEnabled && drawIt && textAttributeValue.trim().length() > 0)
        {
            double offset = 0.0;
            if(textOffset == 1)   // auto
            {
                offset = size/2;
            }
            else if(textOffset == 2) // value
            {
                offset = textOffsetValue;
            }

            transBackColor = new Color(textBackgroundColor.getRed(),textBackgroundColor.getGreen(),textBackgroundColor.getBlue(),(int)(alpha*255));

            //System.out.println("Point origin: "+x0+","+y0);
            //System.out.println("drawPlain text 02 =<"+textAttributeValue+">");
            if(textAttributeValue.toUpperCase().startsWith("<HTML>"))
            {
                drawHTMLText(g,textAttributeValue, offset, x0, y0);
            }
            else drawPlainText(g, textAttributeValue, offset, x0, y0);
        }
    }

    private void drawPlainText(Graphics2D g, String text, double offset, float x0, float y0)
    {
    	    Double viewScale = 1.0;
    	    try
    	    {
                  viewScale = VertexParams.context.getLayerViewPanel().getViewport().getScale();
    	    }
    	    catch(Exception ex)
    	    {
    	    	System.out.println("Deleting attribute from schema");
    	    }
        
            //System.out.println("drawPlain text=<"+text+">"+  "fontsize="+textFontSize);
            int newSize = textFontSize;
            double scaleFactor = viewScale*VertexParams.baseScale;
            if(VertexParams.sizeByScale) 
            {
            	newSize = (int) (textFontSize * scaleFactor);
            	offset = offset * scaleFactor;
            	
            }
        
            Font font = new Font(textFontName, textFontStyle, newSize);
            //System.out.println("Drawing label: "+text+" size="+newSize+"  x0="+x0+" y0="+y0);
            FontRenderContext frc = g.getFontRenderContext();
            TextLayout layout = new TextLayout(textAttributeValue.trim(), font, frc);
            int lineSpace = (int) (layout.getAscent()+layout.getDescent());
            Rectangle2D bounds = layout.getBounds();
            double width = bounds.getWidth();
            double height = bounds.getHeight();

            StringTokenizer st = new StringTokenizer(textAttributeValue,"\\|");

            double textBlockWidth = 0;
            double textBlockHeight = 0;
            int numLines = 0;

            while (st.hasMoreTokens())
            {
                String line = st.nextToken().trim();
                layout = new TextLayout(line, font, frc);
                bounds = layout.getBounds();
                if(bounds.getHeight() > height) height = bounds.getHeight();
                if(bounds.getWidth() > textBlockWidth) textBlockWidth = bounds.getWidth();
                numLines++;
            }
            height = height+2;
            textBlockHeight = (height) * (double)numLines + 3*scaleFactor;
            textBlockWidth = textBlockWidth+10+5*scaleFactor;

            g.setColor(transBackColor);
            Stroke stroke = new BasicStroke(1.0f);
            g.setStroke(stroke);
            g.setFont(font);
            double xp = x0;
            double yp = y0;
            double xp0 = xp;
            double yp0 = yp;
            int lineNumber = 0;
            width = textBlockWidth;
            int hs = 6; // fiddle to gte text apprx cnentred
            //System.out.println("Callout0: x0="+x0+" y0="+y0+"  xp="+xp+" yp="+yp+" block width="+width+"  height="+textBlockHeight);
           xp = x0 ;
           yp = y0 - height;
           switch (textPosition)
           {
               case 0: xp = xp - width/2;
                       yp = yp - textBlockHeight/2 ;
                       break;
               case 1: xp = xp - width/2;
                       yp =yp - offset - textBlockHeight + hs;
                       break;
               case 2: xp = xp + offset;
                       yp = yp - offset - textBlockHeight + hs;
                       break;
               case 3: xp = xp + offset;
                       yp = yp  - textBlockHeight/2 + hs/2;
                       break;
               case 4: xp = xp + offset;
                       yp = yp +  offset;
                       break;
               case 5: xp = xp - width/2;
                       yp = yp + offset;
                       break;
               case 6: xp = xp - width - offset;
                       yp = yp +  offset;
                       break;
               case 7: xp = xp - width - offset;
                       yp = yp  - textBlockHeight/2 + hs/2;
                       break;
               case 8: xp = xp - width - offset;
                       yp = yp - offset - textBlockHeight + hs;
                       break;
           }
            yp = yp + height;
            g.setColor(textForegroundColor);
            //System.out.println("Callout: x0="+x0+" y0="+y0+"  xp="+xp+" yp="+yp);
            LabelCallout callout = new LabelCallout(g, textBorder,textPosition, (int)x0, (int) y0,
                    (int) xp, (int) yp , (int) textBlockWidth, (int) textBlockHeight,
                     textForegroundColor, transBackColor,textFill);

            //System.out.println("Number of text lines: "+numLines+" : "+textAttributeValue);
            st = new StringTokenizer(textAttributeValue,"\\|");
            while (st.hasMoreTokens())
            {
               
               yp = yp + height;
               double xt = xp+5;
               g.setColor(textForegroundColor);
               String line = st.nextToken().trim();
               layout = new TextLayout(line, font, frc);
               bounds = layout.getBounds();

               if(textFontJustification == 1)       // centre
               {
                   xt = xp + (int)((textBlockWidth - bounds.getWidth())/2.0);
               }
                else if(textFontJustification == 2) // right
               {
                   xt = xp + (int)((textBlockWidth - bounds.getWidth())) -5;
               }

                //System.out.println("draw text: "+line);
               g.drawString(line,(float)xt,(float)yp);
               lineNumber++;
            }


    }

    private void drawHTMLText(Graphics2D g, String text, double offset, float x0, float y0)
    {
         double xp = x0;
         double yp = y0;

         Double viewScale = VertexParams.context.getLayerViewPanel().getViewport().getScale();
         
         int newSize = textFontSize;
         double scaleFactor = viewScale*VertexParams.baseScale;
         if(VertexParams.sizeByScale) 
         {
         	newSize = (int) (textFontSize * scaleFactor);
         	offset = offset * scaleFactor;
         }
         
         
         String s1 = text.substring(0,6);
         String s2 = text.substring(6);
         String colorCode = "#FF0000";
         String newText = s1 + "<FONT SIZE=\""+"1"+"\" FACE=\""+textFontName+"\" COLOR=\""+colorCode+"\">" + s2;
         if(debug) System.out.println("text:"+newText);
         HTMLTextComponent textComponent = new HTMLTextComponent(text, offset, x0, y0, transBackColor,
                textForegroundColor, alpha, textBorder, textPosition, textFill, textFontName, newSize);
        int width = textComponent.imageWidth;
        int height = textComponent.imageHeight;
        switch (textPosition)
        {
            case 0: xp = x0 - width/2;
                    yp = y0 - height/2;
                    break;
            case 1: xp = x0 - width/2;
                    yp = y0 - offset - height;
                    break;
            case 2: xp = x0  + offset;
                    yp = y0 - offset - height;
                    break;
            case 3: xp = x0 + offset;
                    yp = y0 - height/2;
                    break;
            case 4: xp = x0 + offset;
                    yp = y0 + offset;
                    break;
            case 5: xp = x0 - width/2;
                    yp = y0 + offset;
                    break;
            case 6: xp = x0 - width - offset;
                    yp = y0 + offset;
                    break;
            case 7: xp = x0 - width - offset;
                    yp = y0 - height/2;
                    break;
            case 8: xp = x0 - width - offset;
                    yp = y0 - height - offset;
                    break;

        }

       
        textComponent.paint(g, (float)xp, (float)yp, drawFactor);
    }
    private String format(double v)
    {
        String pat = "#,##0.0000";
        if(viewport != null)
        {
           double xRange = viewport.getEnvelopeInModelCoordinates().getWidth();

           if(xRange >= 10.0) pat = "#,##0.000";
           if(xRange >= 100.0) pat = "#,##0.00";
           if(xRange >= 1000.0) pat = "#,##0.0";
        }
        else
        {
            pat = "#,##0.00";
        }
       df = new DecimalFormat(pat);
       return df.format(v);
    }

    public void setupTextParameters()
    {
        setTextEnabled(VertexParams.textEnabled);
        setTextAttributeName(VertexParams.attTextName);
        setTextFontName(VertexParams.textFontName);
        setTextFontSize(VertexParams.textFontSize);
        setTextFontStyle(VertexParams.textStyle);
        setTextFontJustification(VertexParams.textJustification);
        setTextPosition(VertexParams.textPosition);
        setTextOffset(VertexParams.textOffset);
        setTextOffsetValue(VertexParams.textOffsetValue);
        setTextBackgroundColor(VertexParams.textBackgroundColor);
        setTextForegroundColor(VertexParams.textForegroundColor);
        setTextBorder(VertexParams.textBorder);
        setTextFill(VertexParams.textFill);
        setTextScope(VertexParams.textScope);
        setSizeByScale(VertexParams.sizeByScale);

    }

    public void presetTextParameters()
    {
        VertexParams.textEnabled = getTextEnabled();
        VertexParams.attTextName = getTextAttributeName();
        VertexParams.textFontName = getTextFontName();
        VertexParams.textFontSize = getTextFontSize();
        VertexParams.textStyle = getTextFontStyle();
        VertexParams.textJustification = getTextFontJustification();
        VertexParams.textPosition = getTextPosition();
        VertexParams.textOffset = getTextOffset();
        VertexParams.textOffsetValue = getTextOffsetValue();
        VertexParams.textBackgroundColor = getTextBackgroundColor();
        VertexParams.textForegroundColor = getTextForegroundColor();
        VertexParams.textBorder = getTextBorder();
        VertexParams.textFill = getTextFill();
        VertexParams.textScope = getTextScope();
        VertexParams.sizeByScale = getSizeByScale();
    }

    public void copyText(ExternalSymbolsType from)
    {
        textEnabled = from.textEnabled;
        textAttributeName = from.textAttributeName;
        textAttributeValue = from.textAttributeValue;
        textAttributeIndex = from.textAttributeIndex;
        textFontName = from.textFontName;
        textFontSize = from.textFontSize;
        textFontStyle = from.textFontStyle;
        textFontJustification = from.textFontJustification;
        textPosition = from.textPosition;
        textOffset = from.textOffset;
        textOffsetValue = from.textOffsetValue;
        textBackgroundColor = from.textBackgroundColor;
        textForegroundColor = from.textForegroundColor;
        textBorder = from.textBorder;
        textFill = from.textFill;
        textScope = from.textScope;
        viewport = from.viewport;
        sizeByScale = from.sizeByScale;
    }
}

