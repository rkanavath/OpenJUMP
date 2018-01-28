package com.cadplan.jump;

import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.model.*;
import com.vividsolutions.jump.workbench.ui.renderer.style.*;
import com.vividsolutions.jump.workbench.ui.Viewport;
import com.vividsolutions.jump.workbench.JUMPWorkbench;
import com.vividsolutions.jump.workbench.imagery.ReferencedImage;
import com.vividsolutions.jump.feature.FeatureCollectionWrapper;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.geom.EnvelopeUtil;
import com.vividsolutions.jump.util.Range;
import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.geom.Point;

import javax.swing.*;
import java.awt.image.BufferedImage;
import java.awt.*;
import java.awt.Polygon;
import java.awt.geom.GeneralPath;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;
import java.util.*;
import java.util.List;
import java.text.DecimalFormat;
import java.net.URL;
import java.io.IOException;

import org.openjump.core.ui.style.decoration.ArrowLineStringMiddlepointStyle;

/**
 * User: geoff
 * Date: 9/03/2007
 * Time: 08:03:47
 * Copyright 2005 Geoffrey G Roy.
 */
public class MapImagePrinter extends Component
{
    boolean debug = false;
    PlugInContext context;
    int xsize, ysize;
    BufferedImage image = null;
    Color noColor = Color.WHITE;
    Envelope envelope;
    Geometry envelopeGeometry;
    Rectangle visRect;
    Graphics2D graphics;
    double scale;
    Viewport viewPort;
    int xoff;
    int yoff;
    boolean includeImages = true;
    static double drawFactor = 0.667;  // scale fonts for printed page
    int START = 0;      // decoration locations
    int END = 1;
    int SEGMENT = 2;
    int VERTEX = 3;
    int MIDDLE = 4;
    Font vertexFont = new Font("SansSerif",Font.PLAIN,7);
    Color vertexFontColor = Color.BLACK;
    //Point2D vertexPoint;
    //Point2D lastVertexPoint;
    DecimalFormat df;
    //String VERSION = JUMPWorkbench.VERSION_TEXT;
    Color lineColor;
    RendererData rd;
    MediaTracker tracker;

    public MapImagePrinter(PlugInContext context, int extentInPixelsX, int extentInPixelsY, double scale)
    {
        this.context = context;
        xsize = extentInPixelsX;
        this.scale = scale;


        viewPort = context.getLayerViewPanel().getViewport();
        envelope = viewPort.getEnvelopeInModelCoordinates();
        visRect = context.getLayerViewPanel().getVisibleRect();

        envelopeGeometry = EnvelopeUtil.toGeometry(envelope);


        ysize = extentInPixelsY;
        image = new BufferedImage(extentInPixelsX, extentInPixelsY, BufferedImage.TYPE_INT_ARGB);

        if(debug) System.out.println("xsize="+xsize+ "  ysize="+ysize+" scale="+scale);
        rd = new RendererData();
        tracker = new MediaTracker(this);

    }

    public void createImage(Graphics2D graphics, int xoff, int yoff)
    {
        //boolean debug = true;
        this.graphics = graphics;
        this.xoff = xoff;
        this.yoff = yoff;
        if(debug) System.out.println("xsize="+xsize+ "  ysize="+ysize+" scale="+scale);
        
        java.util.List allLayers = context.getLayerViewPanel().getLayerManager().getLayers();
        java.util.List allCategories = context.getLayerViewPanel().getLayerManager().getCategories();
        if(debug) System.out.println("Number of layers: "+allLayers.size()+"  categories:"+allCategories.size());
        ArrayList<Layerable> rasterLayers = new ArrayList<Layerable>();
        
        if(includeImages)
        {
            Iterator j = allCategories.iterator();
            while (j.hasNext())
            {
                Category cat = (Category) j.next();
                java.util.List allLayerables =cat.getLayerables();
                if(debug) System.out.println("Cataegory: "+ cat.getName()+ "  size="+allLayerables.size());

                Iterator ijj = allLayerables.iterator();

                while(ijj.hasNext()) // extract all visible raster layers
                {
                    Layerable layer = (Layerable) ijj.next();
                    try
                    {
                        Class dummy = Class.forName("org.openjump.core.rasterimage.RasterImageLayer");
                        if((AbstractLayerable) layer instanceof org.openjump.core.rasterimage.RasterImageLayer)
                        {
                            boolean isVisible = ((org.openjump.core.rasterimage.RasterImageLayer)layer).isVisible();
                            if(isVisible) rasterLayers.add(layer);
                            if(debug) System.out.println("Layer:"+layer.getName()+"        Layer is Raster***********  visible:"+isVisible);
                        }
                    }
                    catch(ClassNotFoundException ex)
                    {
                             // ignore if image is not Pirol type

                    }
                    if(!(layer instanceof Layer))
                    {
                          if(debug) System.out.println("External Rendering image layer: "+layer.getName()+"  type:"+layer.getClass().toString());
                          if(layer.isVisible()) rasterLayers.add(layer);
                    }
                }
            }
                for(int jj = rasterLayers.size()-1; jj >= 0; jj--) // scan in reverse order
                {
                    Layerable layer = rasterLayers.get(jj);
                    BufferedImage bimage = null;
                    double transparencyLevel = 0.0;
                    Envelope imageEnvelope = null;
                    int imageX = 0;
                    int imageY = 0;

                    if((layer instanceof WMSLayer))
                    {
                        try
                        {
                          Image image = ((com.vividsolutions.jump.workbench.model.WMSLayer)layer).createImage(context.getLayerViewPanel());
                           bimage = new BufferedImage(image.getWidth(null), image.getHeight(null),BufferedImage.TYPE_INT_ARGB);
                           Graphics ig = bimage.createGraphics();
                           ig.setColor(Color.WHITE);
                           ig.fillRect(0,0,image.getWidth(null), image.getHeight(null));
                           ig.drawImage(image,0,0,null);
                           imageEnvelope = envelope;
                           transparencyLevel = 1.0 - ((com.vividsolutions.jump.workbench.model.WMSLayer)layer).getAlpha()/255.0;
                           if(debug) System.out.println("transp = "+transparencyLevel);
                            //Renderer renderer = ((com.vividsolutions.jump.workbench.model.WMSLayer)layer).getRenderer();
                           //graphics.drawImage(image,0,0,null);
                        }
                        catch (IOException ex)
                        {

                        }
                    }
                    else
                    {
                         bimage = ((org.openjump.core.rasterimage.RasterImageLayer)layer).createImage(context.getLayerViewPanel());
                         imageEnvelope = ((org.openjump.core.rasterimage.RasterImageLayer)layer).getWholeImageEnvelope();
                         transparencyLevel = ((org.openjump.core.rasterimage.RasterImageLayer)layer).getTransparencyLevel();
                         imageX = ((org.openjump.core.rasterimage.RasterImageLayer)layer).getXOffset();
                         imageY = ((org.openjump.core.rasterimage.RasterImageLayer)layer).getYOffset();
                    }

                        if(debug) System.out.println("View envelope: "+envelope.getMinX()+","+envelope.getMinY()+":"+envelope.getWidth()+","+envelope.getHeight());
                        if(debug) System.out.println("VisRect: "+visRect.getMinX()+","+visRect.getMinY()+":"+visRect.getWidth()+","+visRect.getHeight());

                        if(debug) System.out.println("Image size: "+bimage.getWidth()+","+bimage.getHeight());
                        if(debug) System.out.println("Image envelope: "+imageEnvelope.getMinX()+","+imageEnvelope.getMinY()+","+imageEnvelope.getWidth()+","+imageEnvelope.getHeight());
                        if(debug) System.out.println("offsets: "+xoff+","+yoff);


                         int x = (int) (imageEnvelope.getMinX()*scale);
                         int y = (int) (imageEnvelope.getMinY()*scale);
                         int width = (int) (imageEnvelope.getWidth()*scale);
                         int height = (int) (imageEnvelope.getHeight()*scale);
                         if(debug) System.out.println("Scaled Image envelope: "+x+","+y+":"+width+","+height);

                         double ascale = (double) envelope.getWidth()/ (double)visRect.getWidth();
                         double imageOffX = (imageEnvelope.getMinX()-envelope.getMinX())/ ascale;
                         double imageOffY = (envelope.getHeight()-imageEnvelope.getHeight()-(imageEnvelope.getMinY()-envelope.getMinY()))/ascale;
                         if(debug) System.out.println("Image offsets: "+imageOffX+","+imageOffY);



                        int scaledWidth = (int) visRect.getWidth();
                        int scaledHeight = (int) visRect.getHeight();
                        if(debug) System.out.println("visRect: "+scaledWidth+","+scaledHeight);

                        double x0 = envelope.getMinX();
                        double y0 = envelope.getMaxY();

                        double w0 = envelope.getWidth();
                        double h0 = envelope.getHeight();
                        if(debug) System.out.println("View Envelope: "+x0+","+y0+":"+w0+","+h0);

                        double x1 = imageEnvelope.getMinX();
                        double y1 = imageEnvelope.getMaxY();

                        double w1 = imageEnvelope.getWidth();
                        double h1 = imageEnvelope.getHeight();
                        if(debug) System.out.println("Image Envelope: "+x1+","+y1+":"+w1+","+h1);
                        if(debug) System.out.println("ImageX="+imageX+"  imageY="+imageY);


                        int dx0 = pX(imageEnvelope.getMinX());
                        if(dx0 < pX(envelope.getMinX())) dx0 = pX(envelope.getMinX());
                        int dy0 = pY(imageEnvelope.getMaxY());
                        if(dy0 < pY(envelope.getMaxY())) dy0 = pY(envelope.getMaxY());
                        int dx1 = pX(imageEnvelope.getMaxX());
                        if(dx1 > pX(envelope.getMaxX())) dx1 = pX(envelope.getMaxX());
                        int dy1 = pY(imageEnvelope.getMinY());
                        if(dy1 > pY(envelope.getMinY())) dy1 = pY(envelope.getMinY());

                        int sx0 = 0;
                        int sy0 = 0;
                        int sx1 = bimage.getWidth();
                        int sy1 = bimage.getHeight();
                        if(debug) System.out.println("dest: "+dx0+","+dy0+":"+dx1+","+dy1+"  source: "+sx0+","+sy0+":"+sx1+","+sy1);

                        int currentColor;
                        int [] argb = new int[4];
                        int fullTransparentAlpha = 255;
                        if(debug) System.out.println("Transparency: "+transparencyLevel);
                        for (int w=0; w < bimage.getWidth(); w++)
                        {
                            for (int h=0; h < bimage.getHeight(); h++)
                            {
                                currentColor = bimage.getRGB(w,h);
                                Color cc = new Color(currentColor);

                                argb[0] = (int) (transparencyLevel*255.0);
                                argb[1] = cc.getRed();
                                argb[2] = cc.getGreen();
                                argb[3] = cc.getBlue();

                                Color newColor = new Color(cc.getRed(), cc.getGreen(), cc.getBlue(), (int) (transparencyLevel*255.0));
                                bimage.setRGB(w,h,newColor.getRGB());
                            }
                        }
                        graphics.drawImage(bimage,dx0,dy0,dx1,dy1,sx0,sy0,sx1,sy1,null);

                }
        }


        //debug = false;


        java.util.List layerCollection = context.getLayerViewPanel().getLayerManager().getVisibleLayers(true);
        
        
        int count = 0;
        Object [] layerArray = layerCollection.toArray();
        for (int i= layerArray.length-1; i >= 0; i--)
        {
            Layer layer = (Layer) layerArray[i]; //i.next();
            if(debug) System.out.println("Normal layer: name:"+layer.getName());
            rd.layer = layer;
            
                String name = clean(layer.getName());
                String desc = layer.getDescription();
                int vertexSize = layer.getVertexStyle().getSize();
                rd.vertexSize = vertexSize;
                rd.vertexStyle = layer.getVertexStyle();
                boolean showVertex = layer.getVertexStyle().isEnabled();
                rd.showVertex = showVertex;
                Color labelColor = layer.getLabelStyle().getColor();
                rd.labelColor = labelColor;
                Font labelFont = layer.getLabelStyle().getFont();
                rd.labelFont = labelFont;
                double labelSize = layer.getLabelStyle().getHeight()*drawFactor; // seems to be necessary to match screen sizes

                boolean isScaled = layer.getLabelStyle().isScaling();
                if(isScaled && labelSize > 8) labelSize =  8;
                rd.lableSize = labelSize;
                labelFont = new Font(labelFont.getName(),labelFont.getStyle(), (int) labelSize);
                rd.labelFont = labelFont;
                String labelAngle = layer.getLabelStyle().getAngleAttribute();

                String labelHeight = layer.getLabelStyle().getHeightAttribute();

                String labelAlignment = layer.getLabelStyle().getVerticalAlignment();
                rd.labelAlignment = labelAlignment;
                if(debug) System.out.println("Layer:"+name+ " LabelAngle="+labelAngle+"  Height="+labelHeight+"  alignment="+labelAlignment);
                String labelName = null;
                if(layer.getLabelStyle().isEnabled())
                {
                    labelName = layer.getLabelStyle().getAttribute();
                }
                rd.labelName = labelName;
                String labelValue;
                if(debug) System.out.println("\n========================================");
                if(debug) System.out.println("layer "+count+":"+name+","+desc);
                BasicStyle basicStyle = layer.getBasicStyle();
                Collection themeStyles = null;
                Map themeMap = null;
                Set mapKeys = null;

                 ColorThemingStyle themeStyle = (ColorThemingStyle) layer.getStyle(ColorThemingStyle.class);
                 if(debug) System.out.println("       Theming enabled: "+ themeStyle.isEnabled());
                 if(themeStyle.isEnabled())
                 {
                      themeMap =   themeStyle.getAttributeValueToBasicStyleMap();
                      if(debug) System.out.println("Theme Map:"+themeMap.toString());
                      themeStyles = themeMap.values();
//                      Iterator is = themeStyles.iterator();
//                     while(is.hasNext())
//                     {
//                         BasicStyle istyle = (BasicStyle) is.next();
//                         System.out.println("Style fill color:"+istyle.getFillColor());
//                     }

                      mapKeys = themeMap.keySet();
                      if(debug) System.out.println("mapKeys:"+mapKeys.toString());
                 }

                int alpha = basicStyle.getAlpha();
                lineColor = basicStyle.getLineColor();
                double lineWidth = basicStyle.getLineWidth();
                lineColor = new Color(lineColor.getRed(), lineColor.getGreen(), lineColor.getBlue(), alpha);
                rd.lineColor = lineColor;
                rd.baseLineColor = lineColor;
                if(!basicStyle.isRenderingLine())lineColor = noColor;

                Color fillColor = basicStyle.getFillColor();
                fillColor = new Color(fillColor.getRed(), fillColor.getGreen(), fillColor.getBlue(), alpha);
                if(!basicStyle.isRenderingFill()) fillColor = noColor;
                rd.fillColor = fillColor;
                rd.baseFillColor = fillColor;
                String linePattern = basicStyle.getLinePattern();
                Stroke lineStroke = basicStyle.getLineStroke();
                rd.lineWidth = ((BasicStroke)lineStroke).getLineWidth() * drawFactor;
                float [] dash = ((BasicStroke)lineStroke).getDashArray();
                try
                {
                    for (int id=0; id < dash.length; id++) dash[id] = dash[id]*(float)drawFactor;
                }
                catch (NullPointerException ex)
                {
                    
                }
                float dashPhase = ((BasicStroke)lineStroke).getDashPhase() * (float)drawFactor;
                float miterLimit = ((BasicStroke)lineStroke).getMiterLimit();
                int endCap = ((BasicStroke)lineStroke).getEndCap();
                int lineJoin = ((BasicStroke)lineStroke).getLineJoin();
                rd.lineStroke =  new BasicStroke((float)(rd.lineWidth),endCap, lineJoin,
                              miterLimit, dash, dashPhase);

                Paint fillPattern =basicStyle.getFillPattern();
                if(!basicStyle.isRenderingFillPattern())  fillPattern = null;
                rd.fillPattern = fillPattern;
                FeatureCollectionWrapper fcw = layer.getFeatureCollectionWrapper();
                java.util.List featureCollection = fcw.getFeatures();
                Iterator k = featureCollection.iterator();

                if(debug) System.out.println("\n**** Number of features in layer: "+featureCollection.size());
                int count2 = 0;
                while(k.hasNext())
                {
                    Feature feature = (Feature) k.next();
                    if(debug) System.out.println("***Feature ID: "+feature.getID()+"  type:"+feature.getClass().toString());

                    // handle images
                    boolean featureIsImage = false;
                    Image featureImage = null;
                    FeatureSchema featureSchema = fcw.getFeatureSchema();
                    try
                    {
                        int imageFormatIndex =  featureSchema.getAttributeIndex("IMAGEFORMAT");
                        int imageFactoryIndex =  featureSchema.getAttributeIndex("IMAGEFACT");
                        int imageFileIndex = featureSchema.getAttributeIndex("IMAGEFILE");
                        String imageFactoryName = (String) feature.getAttribute(imageFactoryIndex);
                        if(debug) System.out.println("***IMAGEFILE index = "+imageFileIndex);
                        String filePath = (String) feature.getAttribute(imageFileIndex);
//                        com.vividsolutions.jump.workbench.imagery.graphic.GraphicImageFactory imf = (com.vividsolutions.jump.workbench.imagery.graphic.GraphicImageFactory)feature.getAttribute(imageFactoryIndex);
//                        ReferencedImage rImage = imf.createImage(filePath);

                        if(debug) System.out.println("** File: "+ filePath);
                        //URL url = new URL((String) feature.getAttribute(imageFileIndex));

                        featureImage = Toolkit.getDefaultToolkit().getImage(filePath);
                        try
                        {
                            tracker.addImage(featureImage, 1);
                            tracker.waitForID(1);
                        }
                        catch (InterruptedException e)
                        {
                        }
                        if(debug) System.out.println("Image size:"+featureImage.getWidth(null)+","+featureImage.getHeight(null));
                        featureIsImage = true;
                    }
                    catch(Exception ex)
                    {

                    }



                    rd.feature = feature;
                    labelValue = null;
                    if(labelName != null)
                    {
                        try
                        {
                           labelValue = String.valueOf(feature.getAttribute(labelName));
                        }
                        catch(Exception ex)
                        {
                           labelValue = String.valueOf(feature.getID());
                        }
                    }
                    rd.labelValue = labelValue;
                    Geometry geometry = feature.getGeometry();
                    if(debug) System.out.println("Geometry: "+geometry.toString());

                    if(featureIsImage)
                    {
                        Coordinate[] coords  = geometry.getEnvelope().getCoordinates();
                        int x = pX(coords[0].x);
                        int y = pY(coords[2].y);
                        int w = pX(coords[2].x) - x;
                        int h = pY(coords[0].y) - y;
                        if(debug) System.out.println("Image x="+x+"  y="+y+"  w="+w+"  h="+h);
                        graphics.drawImage(featureImage, x, y, w, h, null);
                    }
                    else if(!geometry.isEmpty())
                    {
                        //boolean debug = true;
                        if(debug) System.out.println("feature "+count2+":"+geometry);
                        String type = geometry.getGeometryType();
                        if(debug) System.out.println("type="+type);
                        Coordinate[] coords = geometry.getCoordinates();
                        if(themeStyles != null)
                        {
                            if(debug) System.out.println("Using theme style");
                            String themeAttribute = themeStyle.getAttributeName();
                            Object themeAttributeValue = feature.getAttribute(themeAttribute);
                            if(debug) System.out.println("Attribute name:"+themeAttribute+"  value:"+themeAttributeValue);
                            rd.fillPattern = null; // muts be if themeing is on

                            Iterator ki = mapKeys.iterator();
                            int index = -1;  // the theme index
                            int kk = 0;
                            while(ki.hasNext())
                            {
                                String key = ki.next().toString();
                                //check for valid range
                                boolean isRange = true;
                                if(key.indexOf("-") < 0) isRange = false;
                                else
                                {
                                    StringTokenizer st = new StringTokenizer(key,"-"); // test if range is numeric
                                    try
                                    {
                                        Double v = Double.parseDouble(st.nextToken());
                                        isRange = true;
                                    }
                                    catch(NumberFormatException ex)
                                    {
                                        isRange = false;
                                    }

                                }
                                if(!isRange)
                                {

                                    String themeAttributeValueString= null;
                                    try
                                    {
                                        themeAttributeValueString = (String) themeAttributeValue;
                                    }
                                    catch (Exception ex)
                                    {
                                        try
                                        {
                                            themeAttributeValueString = String.valueOf((Integer) themeAttributeValue);
                                        }
                                        catch (Exception ex1)
                                        {
                                            themeAttributeValueString = String.valueOf((Double) themeAttributeValue);
                                        }
                                    }
                                    try
                                    {
                                         if(debug) System.out.println("key="+key);
                                         if(debug) System.out.println("attValue="+themeAttributeValueString);
                                         if(key.trim().equals(themeAttributeValueString.trim())) index = kk;
                                    }
                                    catch (Exception ex)
                                    {
                                        //ex.printStackTrace();
                                        //index = -1;
                                        //kk= -1;
                                    }

                                     if(debug) System.out.println("themeAttribute:<"+themeAttributeValue+">");
                                     if(debug) System.out.println("keyString: <"+key+"> index="+index+" kk="+kk);

                                }
                                else   // have a range
                                {
                                    StringTokenizer st = new StringTokenizer(key,"-");
                                    double v0 = Double.MIN_VALUE;
                                    double v1 = Double.MAX_VALUE;

                                    try
                                    {
                                        v0 = Double.parseDouble(st.nextToken());
                                        try
                                        {
                                            if(st.hasMoreTokens()) v1 = Double.parseDouble(st.nextToken());
                                        }
                                        catch(NumberFormatException ex2)
                                        {
                                            
                                        }
                                    }
                                    catch(Exception ex)  
                                    {
                                        System.out.println("ERROR - in extacting them style for: "+key);
                                    }
                                    double v;
                                    try
                                    {
                                           v = (Double) themeAttributeValue;
                                    }
                                    catch(Exception ex)
                                    {
                                        v = (double) ((Integer) themeAttributeValue);
                                    }
                                    if(v >= v0 && v < v1) index = kk;
                                    if(debug) System.out.println("key: "+key+" index="+index+" kk="+kk);
                                }
                                kk++;
                            }

                            Iterator si = themeStyles.iterator();
                            kk = 0;
                            boolean found = false;
                            while(si.hasNext() && !found)
                            {
                                BasicStyle style = (BasicStyle) si.next();
                                if(debug) System.out.println("style color: "+style.getFillColor());
                                if(kk == index)
                                {
                                    fillColor = style.getFillColor();
                                    alpha = style.getAlpha();
                                    fillColor = new Color(fillColor.getRed(), fillColor.getGreen(), fillColor.getBlue(), alpha);
                                    lineColor = new Color(fillColor.getRed()/2, fillColor.getGreen()/2, fillColor.getBlue()/2, alpha);
                                    rd.fillColor = fillColor;
                                    rd.lineColor = lineColor;
                                    if(debug)  System.out.println("Theme color: "+fillColor);
                                    found = true;
                                }
                                kk++;
                            }
                            if(!found)
                            {
                                rd.fillColor = themeStyle.getDefaultStyle().getFillColor();
                                rd.lineColor = themeStyle.getDefaultStyle().getLineColor();
                                if(debug) System.out.println("Themeing style not found for index:"+index);
                            }
                            else
                            {
                                    if(debug) System.out.println("found: index="+index+"  fillColor="+rd.fillColor);

                            }

                        }
                        List styleList = layer.getStyles();

                        if(debug) System.out.println("No of styles in layer: "+styleList.size());
                        rd.styleList = styleList;
                        //System.out.println("Feature type:"+type);

                        if(type.equals("MultiPoint") || type.equals("Point"))
                        {
                            try
                            {
                                Geometry inter = geometry.intersection(envelopeGeometry);
                                int numGeom = 1;
                                try
                                {
                                    numGeom = ((GeometryCollection) inter).getNumGeometries();
                                }
                                catch (Exception ex)
                                {

                                }
                                Geometry interp;
                                if(inter != null && numGeom > 0)
                                {
                                    for(int n=0; n < numGeom; n++)
                                    {
                                        try
                                        {
                                             interp = ((GeometryCollection) inter).getGeometryN(n);
                                        }
                                        catch(Exception ex)
                                        {
                                            interp = inter;
                                        }
                                         coords = interp.getCoordinates();
                                         rd.previousVertex = new Point2D.Double(coords[0].x, coords[0].y);
                                         rd.thisVertex = new Point2D.Double(coords[0].x, coords[0].y);
                                         drawPoint(graphics);
                                         if(labelValue != null)
                                         {
                                              double actualHeight = getAttribute(feature, labelHeight);
                                              rd.labelHeight = actualHeight;
                                              double actualAngle = getAttribute(feature, labelAngle);
                                              rd.labelAngle = actualAngle;
                                              rd.lineAngle = 0.0;
                                              rd.x = coords[0].x;
                                              rd.y = coords[0].y;
                                              drawLabel(graphics);
                                         }
                                    }
                                }
                            }
                            catch (Exception ex)
                            {
                                System.out.println("Point ERROR in JTS Geometry: "+ex);
                                ex.printStackTrace();
                            }
                        }
                        else if(type.equals("LineString") || type.equals("MultiLineString"))
                        {
                            try
                            {
                                Geometry inter = geometry.intersection(envelopeGeometry);
                                int numGeom = 1;
                                try
                                {
                                    numGeom = ((GeometryCollection) inter).getNumGeometries();
                                }
                                catch (Exception ex)
                                {

                                }
                                Geometry interp;
                                if(inter != null && numGeom > 0)
                                {
                                    for(int n=0; n < numGeom; n++)
                                    {
                                        try
                                        {
                                            interp = ((GeometryCollection) inter).getGeometryN(n);
                                        }
                                        catch(Exception ex)
                                        {
                                            interp = inter;
                                        }
                                        coords = interp.getCoordinates();
                                        Coordinate [] linec = interp.getCoordinates();
                                        rd.cline = linec;
                                        drawLine(graphics);
                                        if(linec.length > 0)
                                        {
                                            double longest = 0;
                                            double x0 = linec[0].x;
                                            double y0 = linec[0].y;
                                            double x1 = linec[1].x;
                                            double y1 = linec[1].y;
                                            for (int ic = 0; ic < linec.length-1; ic++) // find longest segment
                                            {
                                                 double len = Math.sqrt((linec[ic].x - linec[ic+1].x)*(linec[ic].x - linec[ic+1].x) +
                                                                        (linec[ic].y - linec[ic+1].y)*(linec[ic].y - linec[ic+1].y));
                                                 if(len > longest )
                                                 {
                                                     longest = len;
                                                     x0 = linec[ic].x;
                                                     y0 = linec[ic].y;
                                                     x1 = linec[ic+1].x;
                                                     y1 = linec[ic+1].y;
                                                 }
                                            }
                                            if(labelValue != null)
                                            {
                                                double angle = Math.atan2(y1-y0,x1-x0);
                                                double actualHeight = getAttribute(feature, labelHeight);
                                                double actualAngle = getAttribute(feature, labelAngle);
                                                rd.labelHeight = actualHeight;
                                                rd.labelAngle = actualAngle;
                                                Font newFont = null;
                                                if(actualHeight > 0.0)
                                                {
                                                    newFont = new Font(labelFont.getName(), labelFont.getStyle(), (int) (actualHeight*drawFactor));
                                                }
                                                else newFont = labelFont;
                                                FontMetrics fm = graphics.getFontMetrics(newFont);
                                                double strlen = (fm.stringWidth(labelValue.trim()))/scale;
                                                double seglen = Math.sqrt((x1-x0)*(x1-x0) + (y1-y0)*(y1-y0));
                                                double labstart = seglen/2-strlen/2;
                                                //System.out.println("strlen="+strlen+" seglen="+seglen+" labstart="+labstart+" angle="+angle);
                                                rd.x = x0 + labstart*Math.cos(angle);
                                                rd.y = y0 + labstart*Math.sin(angle);
                                                if(angle < -Math.PI/2.0 || angle > Math.PI/2.0)
                                                {
                                                    angle = angle + Math.PI;
                                                    rd.x = x1 + labstart*Math.cos(angle);
                                                    rd.y = y1 + labstart*Math.sin(angle);
                                                }
                                                rd.lineAngle = angle;

                                                lineWidth = ((BasicStroke)lineStroke).getLineWidth();
                                                //System.out.println("rd.x="+rd.x+"  rd.y="+rd.y+ "  name="+rd.labelValue +"("+x0+","+x1+")");
                                                drawLabel(graphics);
                                            }
                                        }
                                    }
                                }
                            }
                            catch (Exception ex)
                            {
                                System.out.println("Line ERROR in JTS Geometry: "+ex);
                                ex.printStackTrace();
                            }
                        }
                        else if(type.equals("Polygon") || type.equals("MultiPolygon"))
                        {
                            try
                            {
                                Geometry inter = geometry.intersection(envelopeGeometry);
                                int numGeom = 1;
                                try
                                {
                                    numGeom = ((GeometryCollection) inter).getNumGeometries();
                                }
                                catch(Exception ex)
                                {

                                }
                                Geometry interp;
                                if(inter != null && numGeom > 0)
                                {
                                    for(int n = 0; n < numGeom; n++)
                                    {
                                        try
                                        {
                                             interp = ((GeometryCollection) inter).getGeometryN(n);
                                        }
                                        catch(Exception ex)
                                        {
                                              interp = inter;
                                        }
                                        coords = interp.getCoordinates();
                                        rd.polygon = interp;
                                        drawPolygon(graphics);
                                        Point centroid = interp.getInteriorPoint(); //.getCentroid();
                                        if(centroid == null) centroid = interp.getCentroid();
                                        if(centroid != null)
                                        {
                                            double actualHeight = getAttribute(feature, labelHeight);
                                            double actualAngle = getAttribute(feature, labelAngle);
                                            rd.labelHeight = actualHeight;
                                            rd.labelAngle = actualAngle;
                                            Font newFont = null;
                                            if(actualHeight > 0.0)
                                            {
                                                newFont = new Font(labelFont.getName(), labelFont.getStyle(), (int) actualHeight);
                                            }
                                            else newFont = labelFont;
                                            FontMetrics fm = graphics.getFontMetrics(newFont);
                                            if(labelValue != null)
                                            {
                                                int slen = fm.stringWidth(labelValue);
                                                int sheight = (int) actualHeight;
                                                try
                                                {
                                                    Coordinate coord = new Coordinate(centroid.getX()-(slen/2)/scale,centroid.getY(),0.0);

                                                    rd.lineAngle = 0.0;
                                                    rd.labelAlignment = LabelStyle.ON_LINE;
                                                    rd.x = coord.x;
                                                    rd.y = coord.y;
                                                    drawLabel(graphics);
                                                }
                                                catch (Exception ex)
                                                {

                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            catch (Exception ex)
                            {
                                System.out.println("Polygon ERROR in JTS Geometry: ");
                                ex.printStackTrace();
                            }
                        }
                        count2++;
                    }
                    count++;
                }
        }
    }

//================================================================
// scale coords to pixels
//================================================================
   private int pX(double x)
   {
       int px = xoff + (int) Math.round((x - envelope.getMinX())*scale);
       return px;
   }

    private int pY(double y)
    {
       int py = yoff + ysize -(int) Math.round((y - envelope.getMinY())*scale);
        return py;
    }

   private void drawLabel(Graphics2D g)
   {
       //System.out.println("labelAngle="+labelAngle+"  height="+labelHeight);
       //System.out.println("angle="+angle+"  Font:"+labelFont);
       double labelAngle = Math.toRadians(rd.labelAngle);
       double actualAngle = rd.lineAngle + labelAngle;
       double actualHeight = rd.labelFont.getSize();
       if(rd.labelHeight > 0.0) actualHeight = rd.labelHeight*drawFactor;
       double xoff = 0.0, yoff = 0.0;
       double adjust = 0.0;
       if(rd.labelAlignment.equals(LabelStyle.ABOVE_LINE))
       {
           yoff = - adjust;
       }
       if(rd.labelAlignment.equals(LabelStyle.BELOW_LINE))
       {
           yoff = actualHeight - adjust;
       }
       if(rd.labelAlignment.equals(LabelStyle.ON_LINE))
       {
           yoff = actualHeight/2.0 - adjust;
       }
       //System.out.println("Label at:      x="+pX(rd.x)+"  y="+pY(rd.y)+ "  xoff="+xoff+"  yoff="+yoff);
       if(rd.labelName != null)
       {
          g.setColor(rd.labelColor);
          Font newFont = new Font(rd.labelFont.getName(), rd.labelFont.getStyle(), (int) actualHeight);
          //System.out.println("actualFont: "+newFont);
          g.setFont(newFont);
          // System.out.println("label font:"+labelFont);
          g.translate(pX(rd.x),pY(rd.y));
          g.rotate(-actualAngle);
          g.drawString(rd.labelValue, (int)xoff, (int)yoff);
          g.rotate(actualAngle);
          g.translate(-pX(rd.x),-pY(rd.y));
       }
   }
    
   private void drawPoint(Graphics2D g)
   {
       if(!rd.showVertex) return;
      int x = pX(rd.thisVertex.getX()) - rd.vertexSize/2;
      int y = pY(rd.thisVertex.getY()) - rd.vertexSize/2;
      if(debug) System.out.println("Drawing point: "+rd.previousVertex+ " at "+x+","+y);
      rd.lineWidth = ((BasicStroke)rd.lineStroke).getLineWidth();
      Stroke stroke = new BasicStroke((float)(rd.lineWidth/2.0),BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
      g.setStroke(stroke);

      g.setColor(rd.baseFillColor);
      boolean symbols = false;
      if(rd.vertexStyle.isEnabled())
      {
            try
            {
                Class dummy = Class.forName("com.cadplan.jump.VertexSymbols"); // test if VertexSymbols pluign is installed

                if(rd.vertexStyle instanceof ExternalSymbolsImplType)
                {
                    ExternalSymbolsImplType vertex = new ExternalSymbolsImplType();
                    vertex.setNumSides(((ExternalSymbolsImplType)rd.vertexStyle).getNumSides());
                    vertex.setOrientation(((ExternalSymbolsImplType)rd.vertexStyle).getOrientation());
                    vertex.setDotted(((ExternalSymbolsImplType)rd.vertexStyle).getDotted());
                    vertex.setShowLine(((ExternalSymbolsImplType)rd.vertexStyle).getShowLine());
                    vertex.setShowFill(((ExternalSymbolsImplType)rd.vertexStyle).getShowFill());
                    vertex.setSize((int)(((ExternalSymbolsImplType)rd.vertexStyle).getSize()*drawFactor));
                    vertex.setByValue(((ExternalSymbolsImplType)rd.vertexStyle).getByValue());
                    vertex.setAttributeName(((ExternalSymbolsImplType)rd.vertexStyle).getAttributeName());
                    vertex.setSymbolName(((ExternalSymbolsImplType)rd.vertexStyle).getSymbolName());
                    vertex.setSymbolType(((ExternalSymbolsImplType)rd.vertexStyle).getSymbolType());

                    vertex.initialize(rd.layer);
                    vertex.copyText((ExternalSymbolsType)rd.vertexStyle);
                    vertex.setTextFontSize((int)(vertex.getTextFontSize()*drawFactor));
                    vertex.setDrawFactor(drawFactor);
                    x = pX(rd.thisVertex.getX());
                    y = pY(rd.thisVertex.getY());
                    vertex.paint(rd.feature,g,new Point2D.Double(x,y));
                    symbols = true;
                }
                else if(rd.vertexStyle instanceof com.cadplan.jump.PolygonVertexStyle)
                {
                    PolygonVertexStyle vertex = new PolygonVertexStyle();
                    vertex.setNumSides(((PolygonVertexStyle)rd.vertexStyle).getNumSides());
                    vertex.setOrientation(((PolygonVertexStyle)rd.vertexStyle).getOrientation());
                    vertex.setDotted(((PolygonVertexStyle)rd.vertexStyle).getDotted());
                    vertex.setShowLine(((PolygonVertexStyle)rd.vertexStyle).getShowLine());
                    vertex.setShowFill(((PolygonVertexStyle)rd.vertexStyle).getShowFill());
                    vertex.setSize((int)(((PolygonVertexStyle)rd.vertexStyle).getSize()*drawFactor));
                    vertex.setByValue(((PolygonVertexStyle)rd.vertexStyle).getByValue());
                    vertex.setAttributeName(((PolygonVertexStyle)rd.vertexStyle).getAttributeName());
                    vertex.initialize(rd.layer);
                    vertex.copyText((ExternalSymbolsType)rd.vertexStyle);
                    vertex.setTextFontSize((int)(vertex.getTextFontSize()*drawFactor));
                    vertex.setDrawFactor(drawFactor);
                    x = pX(rd.thisVertex.getX());
                    y = pY(rd.thisVertex.getY());
                    vertex.paint(rd.feature,g,new Point2D.Double(x,y));
                    symbols = true;
                }
                else if(rd.vertexStyle instanceof com.cadplan.jump.StarVertexStyle)
                {
                    StarVertexStyle vertex = new StarVertexStyle();
                    vertex.setNumSides(((StarVertexStyle)rd.vertexStyle).getNumSides());
                    vertex.setOrientation(((StarVertexStyle)rd.vertexStyle).getOrientation());
                    vertex.setDotted(((StarVertexStyle)rd.vertexStyle).getDotted());
                    vertex.setShowLine(((StarVertexStyle)rd.vertexStyle).getShowLine());
                    vertex.setShowFill(((StarVertexStyle)rd.vertexStyle).getShowFill());
                    vertex.setSize((int)(((StarVertexStyle)rd.vertexStyle).getSize()*drawFactor));
                    vertex.setByValue(((StarVertexStyle)rd.vertexStyle).getByValue());
                    vertex.setAttributeName(((StarVertexStyle)rd.vertexStyle).getAttributeName());
                    vertex.initialize(rd.layer);
                    vertex.copyText((ExternalSymbolsType)rd.vertexStyle);
                    vertex.setTextFontSize((int)(vertex.getTextFontSize()*drawFactor));
                    vertex.setDrawFactor(drawFactor);

                    x = pX(rd.thisVertex.getX());
                    y = pY(rd.thisVertex.getY());
                    vertex.paint(rd.feature,g,new Point2D.Double(x,y));
                    symbols = true;
                }
                else if(rd.vertexStyle instanceof com.cadplan.jump.AnyShapeVertexStyle)
                {
                    AnyShapeVertexStyle vertex = new AnyShapeVertexStyle();
                    vertex.setType(((AnyShapeVertexStyle)rd.vertexStyle).getType());
                    vertex.setOrientation(((AnyShapeVertexStyle)rd.vertexStyle).getOrientation());
                    vertex.setDotted(((AnyShapeVertexStyle)rd.vertexStyle).getDotted());
                    vertex.setShowLine(((AnyShapeVertexStyle)rd.vertexStyle).getShowLine());
                    vertex.setShowFill(((AnyShapeVertexStyle)rd.vertexStyle).getShowFill());
                    vertex.setSize((int)(((AnyShapeVertexStyle)rd.vertexStyle).getSize()*drawFactor));
                    vertex.setByValue(((AnyShapeVertexStyle)rd.vertexStyle).getByValue());
                    vertex.setAttributeName(((AnyShapeVertexStyle)rd.vertexStyle).getAttributeName());
                    vertex.initialize(rd.layer);
                    vertex.copyText((ExternalSymbolsType)rd.vertexStyle);
                    vertex.setTextFontSize((int)(vertex.getTextFontSize()*drawFactor));
                    vertex.setDrawFactor(drawFactor);

                    x = pX(rd.thisVertex.getX());
                    y = pY(rd.thisVertex.getY());
                    vertex.paint(rd.feature,g,new Point2D.Double(x,y));
                    symbols = true;
                }
                else if(rd.vertexStyle instanceof com.cadplan.jump.ImageVertexStyle)
                {
                    ImageVertexStyle vertex = new ImageVertexStyle();
                    vertex.setName(((ImageVertexStyle)rd.vertexStyle).getName());
                    vertex.setOrientation(((ImageVertexStyle)rd.vertexStyle).getOrientation());
                    vertex.setScale((((ImageVertexStyle)rd.vertexStyle).getScale()/drawFactor));
                    vertex.setSize((int)(((ImageVertexStyle)rd.vertexStyle).getSize()*drawFactor));
                    vertex.setByValue(((ImageVertexStyle)rd.vertexStyle).getByValue());
                    vertex.setAttributeName(((ImageVertexStyle)rd.vertexStyle).getAttributeName());
                    vertex.initialize(rd.layer);
                    vertex.copyText((ExternalSymbolsType)rd.vertexStyle);
                    vertex.setTextFontSize((int)(vertex.getTextFontSize()*drawFactor));
                    vertex.setDrawFactor(drawFactor);

                    x = pX(rd.thisVertex.getX());
                    y = pY(rd.thisVertex.getY());
                    vertex.paint(rd.feature,g,new Point2D.Double(x,y));
                    symbols = true;
                    //System.out.println("byValue="+vertex.getByValue()+"  attname="+vertex.getAttributeName());
                }
                else if(rd.vertexStyle instanceof com.cadplan.jump.WKTVertexStyle)
                {
                    WKTVertexStyle vertex = new WKTVertexStyle();
                    vertex.setName(((WKTVertexStyle)rd.vertexStyle).getName());
                    vertex.setOrientation(((WKTVertexStyle)rd.vertexStyle).getOrientation());
                    vertex.setShowLine(((WKTVertexStyle)rd.vertexStyle).getShowLine());
                    vertex.setScale((((WKTVertexStyle)rd.vertexStyle).getScale()/drawFactor));
                    vertex.setSize((int)(((WKTVertexStyle)rd.vertexStyle).getSize()*drawFactor));
                    vertex.setByValue(((WKTVertexStyle)rd.vertexStyle).getByValue());
                    vertex.setAttributeName(((WKTVertexStyle)rd.vertexStyle).getAttributeName());
                    vertex.initialize(rd.layer);
                    vertex.copyText((ExternalSymbolsType)rd.vertexStyle);
                    vertex.setTextFontSize((int)(vertex.getTextFontSize()*drawFactor));
                    vertex.setDrawFactor(drawFactor);
                    
                    x = pX(rd.thisVertex.getX());
                    y = pY(rd.thisVertex.getY());
                    vertex.paint(rd.feature,g,new Point2D.Double(x,y));
                    symbols = true;
                    //System.out.println("byValue="+vertex.getByValue()+"  attname="+vertex.getAttributeName());
                }

            }
            catch(ClassNotFoundException ex)  // VertexSymbols plugin not installed
            {

            }
            catch (Exception ex)   // any other error
            {
                System.out.println("ERROR: showing vertex symbols\n"+ex);
                ex.printStackTrace();
            }
      }


      if(!symbols && rd.vertexStyle.isEnabled()) // paint default vertex shape
      {
          GeneralPath path = new GeneralPath();
          path.moveTo(x,y);
          path.lineTo(x+rd.vertexSize,y);
          path.lineTo(x+rd.vertexSize,y+rd.vertexSize);
          path.lineTo(x,y+rd.vertexSize);
          path.lineTo(x,y);
          g.fill(path);
          g.setColor(rd.baseLineColor);
          g.draw(path);
      }

   }

    private void drawLine(Graphics2D g)
    {
    	//boolean debug = true;
       float x0 = 0f, x1 = 0f;
       float y0 = 0f, y1 = 0f;
       float xp = 0f, yp = 0f;
       float xpp = 0f, ypp = 0f;
       double startAngle = 0.0, endAngle= 0.0, segmentAngle = 0.0;
       rd.lineWidth = ((BasicStroke)rd.lineStroke).getLineWidth();
       int index = 0;
       Coordinate [] coords = rd.cline;
       GeneralPath path = new GeneralPath();
       for (int i=0; i < coords.length; i++)
       {
           rd.index = index;
           g.setColor(rd.lineColor);
           g.setStroke(rd.lineStroke);
           x0 = (float) pX(coords[i].x);
           y0 = (float) pY(coords[i].y);
           rd.thisVertex = new Point2D.Double(coords[i].x, coords[i].y);
           if(rd.showVertex) drawPoint(g);
           if(i == 0)
           {
               path.moveTo(x0,y0);
               x1 = x0;
               y1 = y0;
               rd.px = pX(x0);
               rd.py = pY(y0);
               rd.firstVertex = new Point2D.Double(coords[i].x, coords[i].y);
               drawDecorations(g,  VERTEX);
           }
           else
           {
               path.lineTo(x0,y0);
               segmentAngle = Math.atan2((y0-yp),(x0-xp));
               rd.lineAngle = segmentAngle;
               rd.px = pX(x0);
               rd.py = pY(y0);
               drawDecorations(g, SEGMENT);
               drawDecorations(g, VERTEX);

           }
           if(i == 1)
           {
               startAngle = Math.atan2((y0-y1),(x0-x1));
               rd.startAngle = startAngle;
           }
           if( i == coords.length-1)
           {
               endAngle = Math.atan2((y0-yp),(x0-xp));
               rd.endAngle = endAngle;
               xpp = xp;
               ypp = yp;
               rd.lastVertex = new Point2D.Double(coords[i].x, coords[i].y);
           }
           xp = x0;
           yp = y0;

           if(debug) System.out.println("Drawing Line: "+coords+ " at "+x0+","+y0);
           index++;
 //          lastVertexPoint = (Point2D.Double)vertexPoint.clone();
           rd.previousVertex = new Point2D.Double(coords[i].x, coords[i].y);

       }
        rd.previousVertex = new Point2D.Double(coords[coords.length-2].x, coords[coords.length-2].y);
        g.setColor(rd.lineColor);
        g.setStroke(rd.lineStroke);
        g.draw(path);
        drawDecorations(g, START);
        drawDecorations(g,  END);
        drawDecorations(g,  MIDDLE);
    }

    private void drawPolygon(Graphics2D g)
    {
    	 if(debug) System.out.println("Drawing Polygon");
       float x0=0.0f, x=0.0f, xp = 0.0f;
       float y0=0.0f, y=0.0f, yp = 0.0f;
       double startAngle = 0.0, endAngle = 0.0, segmentAngle=0.0;
       rd.lineWidth = ((BasicStroke)rd.lineStroke).getLineWidth();
       int index = 0;
       Coordinate [] coords = ((com.vividsolutions.jts.geom.Polygon)rd.polygon).getExteriorRing().getCoordinates();
       int numberHoles = ((com.vividsolutions.jts.geom.Polygon)rd.polygon).getNumInteriorRing();
       GeneralPath path = new GeneralPath();
       float xs = 0.0f;
       float ys = 0.0f;
       for (int i=0; i < coords.length-1; i++)
       {
           g.setColor(this.lineColor);
           g.setStroke(rd.lineStroke);
           x = (float) pX(coords[i].x);
           y = (float) pY(coords[i].y);
           rd.thisVertex = new Point2D.Double(coords[i].x, coords[i].y);
           if(rd.showVertex) drawPoint(g);
           rd.index = index;
           if( i == 0)
           {
              path.moveTo(x,y);
              x0 = x;
              y0 = y;
              //if(coords.length > 500) System.out.println("Line start i="+i+" "+x+","+y);
               x0 = x;
               y0 = y;
               drawDecorations(g,  VERTEX);
               rd.firstVertex = new Point2D.Double(coords[i].x, coords[i].y);

           }
           else
           {
               path.lineTo(x,y);
               //if(coords.length > 500) System.out.println("Line i="+i+" "+x+","+y);
               segmentAngle = Math.atan2((y-yp),(x-xp));
               rd.lineAngle = segmentAngle;
               drawDecorations(g, SEGMENT);
               drawDecorations(g, VERTEX);

           }
       
           index++;
           if(i == 1)
           {
               startAngle = Math.atan2((y-y0),(x-x0));
               rd.startAngle = startAngle;
           }
           xp = x;
           yp = y;
           rd.previousVertex = new Point2D.Double(coords[i].x, coords[i].y);

       }
       //close polygon
       //if(x != x0 || y != y0)
       //{
    	//   path.lineTo(x0,y0);
    	//   System.out.println("Closing polygon: "+x0+","+y0);
       //}
       
       xp = pX(coords[coords.length-2].x);
       yp = pY(coords[coords.length-2].y);
       x = pX(coords[coords.length-1].x);
       y = pY(coords[coords.length-1].y);
       rd.previousVertex = new Point2D.Double(coords[coords.length-2].x,coords[coords.length-2].y);
       rd.thisVertex =new Point2D.Double(coords[coords.length-1].x,coords[coords.length-1].y);
       rd.lastVertex = new Point2D.Double(coords[coords.length-1].x,coords[coords.length-1].y);

       endAngle = Math.atan2((y-yp),(x-xp));
       rd.endAngle = endAngle;
       rd.lineAngle = endAngle;
       drawDecorations(g,  START);
       drawDecorations(g,  END);
       drawDecorations(g,  MIDDLE);
       drawDecorations(g,  SEGMENT);


       Area outer = new Area(path);
        //System.out.println("outer="+outer+"  numholes="+numberHoles);

       if(numberHoles > 0)
       {
           GeneralPath hpath = new GeneralPath();
           for (int n=0; n < numberHoles; n++)
           {
                index = 0;
                Coordinate [] hcoords = ((com.vividsolutions.jts.geom.Polygon)rd.polygon).getInteriorRingN(n).getCoordinates();
                for (int ip = 0; ip < hcoords.length; ip++)
                {
                    rd.index = index;
                    x = (float) pX(hcoords[ip].x);
                    y = (float) pY(hcoords[ip].y);
                    rd.thisVertex = new Point2D.Double(hcoords[ip].x, hcoords[ip].y);
                    if(rd.showVertex) drawPoint(g);

                   if( ip == 0)
                   {
                       hpath.moveTo(x,y);
                       x0 = x;
                       y0 = y;
                       rd.firstVertex = new Point2D.Double(hcoords[ip].x, hcoords[ip].y);
                       drawDecorations(g,  VERTEX);
                   }
                   else
                   {
                       hpath.lineTo(x,y);
                       segmentAngle = Math.atan2((y-yp),(x-xp));
                       rd.lineAngle = segmentAngle;
                       drawDecorations(g, SEGMENT);
                       if(ip < hcoords.length-1) drawDecorations(g, VERTEX);

                   }
                   index++;
                   if(ip == 1)
                   {
                       startAngle = Math.atan2((y-y0),(x-x0));
                       rd.startAngle = startAngle;
                   }
                    xp = x;
                    yp = y;
                    rd.previousVertex = new Point2D.Double(hcoords[ip].x, hcoords[ip].y);
                    rd.lastVertex =  new Point2D.Double(hcoords[ip].x, hcoords[ip].y);
                   //System.out.println("      hole: "+n+" : "+x+","+y);
                }
                xp = pX(hcoords[hcoords.length-2].x);
                yp = pY(hcoords[hcoords.length-2].y);
                x = pX(hcoords[hcoords.length-1].x);
                y = pY(hcoords[hcoords.length-1].y);
                rd.previousVertex = new Point2D.Double(hcoords[hcoords.length-2].x,hcoords[hcoords.length-2].y);
                rd.thisVertex = new Point2D.Double(hcoords[hcoords.length-1].x,hcoords[hcoords.length-1].y);
                endAngle = Math.atan2((y-yp),(x-xp));
                rd.endAngle = endAngle;
                rd.lineAngle = endAngle;
                rd.lastVertex = new Point2D.Double(hcoords[hcoords.length-1].x, hcoords[hcoords.length-1].y);

                drawDecorations(g, START);
                drawDecorations(g, END);
                drawDecorations(g, MIDDLE);

           }
           Area hole = new Area(hpath);
           outer.subtract(hole);
       }
       g.setColor(rd.fillColor);

       if(rd.fillPattern != null)
       {
           g.setPaint(rd.fillPattern);
       }
       
	       //System.out.println("Painting Layer:"+rd.layer.getName()+" Area:"+outer.toString()+"  coords size:"+coords.length);
	       //System.out.println("lineColor:"+this.lineColor+"  fillColor:"+rd.fillColor+"  pattern:"+rd.fillPattern  );
	       
       
       if(!rd.fillColor.equals(Color.WHITE)) g.fill(outer);
       g.setColor(this.lineColor);
       g.setStroke(rd.lineStroke);
       g.draw(outer);
    }


    private double getAttribute(Feature feature, String attribute)
    {
        double value = 0.0;
        if(!attribute.equals(""))
        {
            Object ob = feature.getAttribute(attribute);
            //System.out.println("ob="+ob);
            try
            {
                value = Double.parseDouble(String.valueOf(ob));
            }
            catch (Exception ex)
            {
                value = 0.0;
            }
        }
        //System.out.println("**** attribute: "+attribute+ "    value="+value);
        return value;
    }
    //============================  draw decorations

    private void drawDecorations(Graphics2D g, int position)
    {
        Iterator is = rd.styleList.iterator();
        while( is.hasNext())
        {
            Style style = (Style) is.next();
            if(position == START)
            {
                float x = (float)pX(rd.firstVertex.getX());
                float y = (float)pY(rd.firstVertex.getY());

                if(style instanceof CircleLineStringEndpointStyle.Start) drawCircle(g,x,y, rd.lineWidth);
                if(style instanceof ArrowLineStringEndpointStyle.FeathersStart ) drawFeathers(g,x,y,rd.startAngle, rd.lineWidth);
                if(style instanceof ArrowLineStringEndpointStyle.NarrowSolidStart) drawNarrowSolid(g,x,y,rd.startAngle, rd.lineWidth);
                if(style instanceof ArrowLineStringEndpointStyle.SolidStart) drawSolid(g,x,y,rd.startAngle, rd.lineWidth);
                if(style instanceof ArrowLineStringEndpointStyle.OpenStart) drawOpen(g,x,y,rd.startAngle, rd.lineWidth);
            }

            if(position == END)
            {
                float x = (float)pX(rd.lastVertex.getX());
                float y = (float)pY(rd.lastVertex.getY());

                if(style instanceof CircleLineStringEndpointStyle.End) drawCircle(g,x,y, rd.lineWidth);
                if(style instanceof ArrowLineStringEndpointStyle.FeathersEnd ) drawFeathers(g,x,y,rd.endAngle+Math.PI, rd.lineWidth);
                if(style instanceof ArrowLineStringEndpointStyle.NarrowSolidEnd) drawNarrowSolid(g,x,y,rd.endAngle+Math.PI, rd.lineWidth);
                if(style instanceof ArrowLineStringEndpointStyle.SolidEnd) drawSolid(g,x,y,rd.endAngle+Math.PI, rd.lineWidth);
                if(style instanceof ArrowLineStringEndpointStyle.OpenEnd) drawOpen(g,x,y,rd.endAngle+Math.PI, rd.lineWidth);
            }

            //if(VERSION.compareTo("1.1") >= 0)
            //{
                if(position == SEGMENT)
                {
                    double x0 = rd.previousVertex.getX();
                    double y0 = rd.previousVertex.getY();
                    double x1 = rd.thisVertex.getX();
                    double y1 = rd.thisVertex.getY();
                    float xs = (float)pX((x0+x1)/2.0);
                    float ys = (float)pY((y0+y1)/2.0);
                    if(style instanceof ArrowLineStringSegmentStyle.Open) drawOpen(g,xs,ys,rd.lineAngle+Math.PI, rd.lineWidth);
                    if(style instanceof ArrowLineStringSegmentStyle.Solid)drawSolid(g,xs,ys,rd.lineAngle+Math.PI, rd.lineWidth);
                    if(style instanceof ArrowLineStringSegmentStyle.NarrowSolid) drawNarrowSolid(g,xs,ys, rd.lineAngle+Math.PI, rd.lineWidth);
                    if(style instanceof MetricsLineStringSegmentStyle) drawMetrics(g, xs, ys, rd.lineAngle+Math.PI, rd.lineWidth);
                }
                if(position == VERTEX)
                {
                    float x1 = (float)pX(rd.thisVertex.getX());
                    float y1 = (float)pY(rd.thisVertex.getY());

                    if(style instanceof VertexIndexLineSegmentStyle.VertexIndex) drawIndex(g,x1,y1,0.0, rd.index, rd.lineWidth);
                    if(style instanceof VertexXYLineSegmentStyle.VertexXY) drawXY(g,x1,y1,0.0, rd.index, rd.lineWidth);

                }
                if(position == MIDDLE)
                {
                    double x0 = rd.previousVertex.getX();
                    double y0 = rd.previousVertex.getY();
                    double x1 = rd.thisVertex.getX();
                    double y1 = rd.thisVertex.getY();
                    float xs = (float)pX((x0+x1)/2.0);
                    float ys = (float)pY((y0+y1)/2.0);
                    if(style instanceof ArrowLineStringMiddlepointStyle.NarrowSolidMiddle) drawNarrowSolid(g, xs, ys, rd.lineAngle+Math.PI, rd.lineWidth);

                }
            //}

        }

    }
    private void drawIndex(Graphics2D g, float x, float y, double angle, int index, double lineWidth)
    {

        g.setFont(vertexFont);
        g.setColor(vertexFontColor);
        String note = String.valueOf(index);
        g.drawString(note,x,y);
    }

    private void drawXY(Graphics2D g, float x, float y, double angle, int index, double lineWidth)
    {
        g.setFont(vertexFont);
        g.setColor(vertexFontColor);
        String note = String.valueOf(format(rd.thisVertex.getX())) +","+ String.valueOf(format(rd.thisVertex.getY()));
        g.drawString(note,x,y);
    }

    private void drawMetrics(Graphics2D g, float x, float y, double angle, double lineWidth)
    {
        g.setFont(vertexFont);
        g.setColor(vertexFontColor);
        double x0 = rd.previousVertex.getX();
        double y0 = rd.previousVertex.getY();
        double x1 = rd.thisVertex.getX();
        double y1 = rd.thisVertex.getY();
        double len = Math.sqrt((x1-x0)*(x1-x0) + (y1-y0)*(y1-y0));
        double bearing = Math.atan2(y1-y0, x1-x0);

        String note = String.valueOf(format(len) +"/"+ String.valueOf(formatAngle(Math.toDegrees(bearing)))+"�");
        g.drawString(note,x,y);

    }

    private String format(double v)
    {
       double xRange = envelope.getWidth();
       String pat = "#,##0.0000";
       if(xRange >= 10.0) pat = "#,##0.000";
       if(xRange >= 100.0) pat = "#,##0.00";
       if(xRange >= 1000.0) pat = "#,##0.0";
       df = new DecimalFormat(pat);
       return df.format(v);
    }

    private String formatAngle(double v)
    {
       String pat = "#,##0.000";
       df = new DecimalFormat(pat);
       return df.format(v);
    }


    private void drawCircle(Graphics2D g, float x, float y, double lineWidth)
    {
        int dia = 8;
        if(lineWidth > 2) dia = (int)lineWidth+4;
        Stroke stroke = new BasicStroke((float)(lineWidth/2.0),BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
        g.setStroke(stroke);
        g.setColor(rd.lineColor);
        g.drawOval((int) (x-dia/2), (int)(y-dia/2), dia, dia);

    }

    private void drawFeathers(Graphics2D g, float x, float y, double angle, double lineWidth)
    {
        int d = 4;
        int len = 6;
        int s = len/2;
        if(lineWidth > 2)
        {
            d = (int) lineWidth+4;
            len = (int)lineWidth + 6;
            s = len/2;
        }
        Stroke stroke = new BasicStroke((float)(lineWidth/2.0),BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
        g.setStroke(stroke);
        g.setColor(rd.lineColor);
        GeneralPath path = new GeneralPath();
        path.moveTo(x, y-d);
        path.lineTo(x+len,y);
        path.lineTo(x,y+d);
        path.moveTo(x+s,y-d);
        path.lineTo(x+s+len,y);
        path.lineTo(x+s,y+d);

        g.rotate(angle,x,y);
        g.draw(path);
        g.rotate(-angle,x,y);
    }

    private void drawNarrowSolid(Graphics2D g, float x, float y, double angle, double lineWidth)
    {
        int d = 2;
        int len = 4*d;
        if(lineWidth > 2)
        {
           d = (int) (lineWidth/2) + 2;
           len = 4*d;
        }
        Stroke stroke = new BasicStroke((float)(lineWidth/2.0),BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
        g.setStroke(stroke);
        g.setColor(rd.lineColor);
        GeneralPath path = new GeneralPath();
        path.moveTo(x+len, y);
        path.lineTo(x+len, y-d);
        path.lineTo(x,y);
        path.lineTo(x+len,y+d);
        path.lineTo(x+len,y);

        g.rotate(angle,x,y);
        g.fill(path);
        g.draw(path);
        g.rotate(-angle,x,y);

    }
    private void drawSolid(Graphics2D g, float x, float y, double angle, double lineWidth)
    {
        int d = 4;
        int len = d*3/2;
        if(lineWidth > 2)
        {
           d = (int) (lineWidth/2) + 4;
           len = d*3/2;
        }
        Stroke stroke = new BasicStroke((float)(lineWidth/2.0),BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
        g.setStroke(stroke);
        g.setColor(rd.lineColor);
        GeneralPath path = new GeneralPath();
        path.moveTo(x+len, y);
        path.lineTo(x+len, y-d);
        path.lineTo(x,y);
        path.lineTo(x+len,y+d);
        path.lineTo(x+len,y);


        g.rotate(angle,x,y);
        g.fill(path);
        g.draw(path);
        g.rotate(-angle,x,y);

    }
    private void drawOpen(Graphics2D g, float x, float y, double angle, double lineWidth)
    {
        int d = 4;
        int len = d*3/2;
        if(lineWidth > 2)
        {
           d = (int) (lineWidth/2) + 4;
           len = d*3/2;
        }
        Stroke stroke = new BasicStroke((float)(lineWidth/2.0),BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
        g.setStroke(stroke);
        g.setColor(rd.lineColor);
        GeneralPath path = new GeneralPath();
        path.moveTo(x+len, y-d);
        path.lineTo(x, y);
        path.lineTo(x+len,y+d);

        g.rotate(angle,x,y);
        g.draw(path);

        g.rotate(-angle,x,y);

    }



   private String clean(String name)
   {
       String cleanName = name.replace(",","").replace("&","and");
       return cleanName;
   }
}
