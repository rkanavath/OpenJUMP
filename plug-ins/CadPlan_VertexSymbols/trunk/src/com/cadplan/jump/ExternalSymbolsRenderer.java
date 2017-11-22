package com.cadplan.jump;

import java.awt.geom.GeneralPath;
import java.awt.geom.RectangularShape;
import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: geoff
 * Date: 1/09/2007
 * Time: 13:39:48
 * To change this template use File | Settings | File Templates.
 */
public class ExternalSymbolsRenderer
{
    static final int POLYS = 0, STARS = 1, ANYS = 2, WKTS = 3, IMAGES = 4;
    

    private int type;
    public int width;
    public int height;
    public int size;
    public int scale = 1;
    public float x0;
    public float y0;


     public GeneralPath buildShape(int type, Shape shape, boolean byValue, double orientation, int bsize,
                                    int numSides)
    {
        GeneralPath path = new GeneralPath();
        float x, y;
        int n;
        double s, angle;
        size = bsize;
        //System.out.println("Building shape:\ntype="+type+" shape="+shape+" byValue="+byValue+" orientation="+orientation+" size="+size+"  numSides="+numSides);
        switch (type)
        {
            case POLYS:

                        double absAngle = orientation;
                        if(!byValue) absAngle = 0.0;
                         s = ((double) bsize) /2.0;
                         n = numSides;
                         angle = Math.toRadians(absAngle)  - Math.PI/2.0 + Math.PI/((double)n);
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
            case STARS:
                        float xm, ym;
                        double baseAngle = orientation;
                        if(!byValue) baseAngle = 0.0;

                         s = ((double) bsize) /2.0;
                         n = numSides;
                         angle = Math.toRadians(baseAngle)  - Math.PI/2.0 + Math.PI/((double)n);
                        double halfAngle =  Math.PI/((double)n);
                        x0 = (float) shape.getBounds2D().getX() + (float) (s);
                        y0 = (float) shape.getBounds2D().getY() + (float) (s);

                        for (int i=0; i < n; i++)
                        {
                            x = (float) (s*Math.cos(angle));
                            y = (float) (s*Math.sin(angle));
                            xm = (float) ((s/3.0)*Math.cos(angle-halfAngle));
                            ym = (float) ((s/3.0) * Math.sin(angle-halfAngle));
                            if(i == 0) path.moveTo(x0+x,y0-y);
                            else
                            {
                                path.lineTo(x0+xm, y0-ym);
                                path.lineTo(x0+x, y0-y);
                            }
                            angle = angle + 2.0*Math.PI/(double)n;
                        }
                        angle = Math.toRadians(baseAngle) - Math.PI/2.0 + Math.PI/((double)n);
                        xm = (float) ((s/3.0)*Math.cos(angle-halfAngle));
                        ym = (float) ((s/3.0) * Math.sin(angle-halfAngle));
                        x = (float) (s*Math.cos(angle));
                        y = (float) (s*Math.sin(angle));
                        path.lineTo(x0+xm, y0-ym);
                        path.lineTo(x0+x,y0-y);
                        return path;

            case ANYS:

                        float sf = (float) (((double) bsize) /2.0);
                        float s2 = sf;
                        x0 = (float) shape.getBounds2D().getX() + (float) (sf);
                        y0 = (float) shape.getBounds2D().getY() + (float) (sf);

                        int atype = numSides;
                        switch (atype)
                        {
                            case 0:
                                path.moveTo(x0-sf,y0);
                                path.lineTo(x0+sf,y0);
                                path.moveTo(x0,y0);
                                path.lineTo(x0-sf/2,y0-sf/2);
                                path.moveTo(x0,y0);
                                path.lineTo(x0,y0-sf);
                                path.moveTo(x0,y0);
                                path.lineTo(x0+sf/2,y0-sf/2);

                                break;

                            case 1:
                                path.moveTo(x0-sf,y0);
                                path.lineTo(x0+sf,y0);
                                path.moveTo(x0+sf/2,y0+sf/2);
                                path.lineTo(x0-sf/2,y0-sf/2);
                                path.moveTo(x0,y0+sf);
                                path.lineTo(x0,y0-sf);
                                path.moveTo(x0-sf/2,y0+sf/2);
                                path.lineTo(x0+sf/2,y0-sf/2);
                                break;

                            case 2:
                                path.moveTo(x0-sf,y0);
                                path.lineTo(x0+sf,y0);
                                path.moveTo(x0,y0-sf);
                                path.lineTo(x0,y0+sf);

                                path.moveTo(x0+sf/2,y0);
                                for (int i=0; i <= 16; i++)
                                {
                                     angle = (double)i*2.0*Math.PI/16.0;
                                    path.lineTo((float)(x0+(sf/2)*Math.cos(angle)), (float)(y0-(sf/2)*Math.sin(angle)));
                                }
                                break;

                            case 3:
                                path.moveTo(x0-sf,y0);
                                path.lineTo(x0+sf,y0);
                                path.moveTo(x0,y0-sf);
                                path.lineTo(x0,y0+sf);

                                path.moveTo(x0-sf/2,y0-sf/2);
                                path.lineTo(x0+sf/2,y0-sf/2);
                                path.lineTo(x0+sf/2,y0+sf/2);
                                path.lineTo(x0-sf/2,y0+sf/2);
                                path.lineTo(x0-sf/2,y0-sf/2);

                                break;

                            case 4:
                                path.moveTo(x0,y0);
                                path.lineTo(x0,y0-s2/2);


                                path.moveTo(x0+s2/4,y0-3*s2/4);
                                for (int i=0; i <= 16; i++)
                                {
                                     angle = (double)i*2.0*Math.PI/16.0;
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
            case WKTS:
                
        }
        return null;
    }

    public Image getImage(int type, String name, int bsize, Shape shape)
    {
        Image image = null;
        int x =0, y=0;
        if(type == IMAGES)
        {
           boolean found = false;
           size = bsize;
           x = (int) shape.getBounds2D().getX();
           y  = (int) shape.getBounds2D().getY();
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
                   //System.out.println("Image name="+name+ "  After scaling: size="+size+ " width="+width+" height="+height+" scale="+scale);
               }

           }
           if(!found)    // missing image
           {
              //Icon icon = (Icon) UIManager.get("JOptionPane.questionIcon");
              image = null;
           }

        }
        int newWidth;
        int newHeight;
        if(width > height)
        {
             newWidth = size/scale;
             if(width > 0) newHeight =size*height/width/scale;
             else newHeight = height;
        }
        else
        {
            newHeight = size/scale;
            if(height > 0) newWidth = size*width/height/scale;
            else newWidth = width;
        }
        //width = newWidth;
        //height = newHeight;
        x0 = (float) (x + size/2.0);
        y0 = (float) (y + size/2.0);
//        x0 = x;
//        y0 = y;

        return image;
    }
    public WKTshape getWKTShape(int type,String name, int bsize, Shape shape)
    {
       WKTshape wktShape = null;
       boolean found = false;
       size = bsize;
       scale = 1;
       int x,y;
       x = (int) shape.getBounds2D().getX();
       y = (int) shape.getBounds2D().getY();

        //System.out.println("WKT position: x="+x+"  y="+y);
       if(type == WKTS)
       {

           for (int i=0; i < VertexParams.wktShapes.length; i++)
           {
               if(VertexParams.wktNames[i].equals(name))
               {
                   found = true;
                   wktShape = VertexParams.wktShapes[i];
                   if(wktShape != null)
                   {
                       //size = image.getWidth(null);
                       width = wktShape.extent;
                       height = wktShape.extent;
                       //size = wktShape.extent;
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
                   //System.out.println("name="+name+ "  Before scaling: size="+size+ "  scale="+scale);
                   size = (int) (size/scale);
                   //System.out.println("name="+name+ "  After scaling: size="+size+ "  scale="+scale+" width="+width+" height="+height);
               }

           }
       }
       if(!found)    // missing WKT shape
       {
          //Icon icon = (Icon) UIManager.get("JOptionPane.questionIcon");
          wktShape = null;
       }
        int newWidth;
        int newHeight;
        if(width > height)
        {
             newWidth = size;
             newHeight =size*height/width;
        }
        else
        {
            newHeight = size;
            newWidth = size*width/height;
        }
        width = newWidth;
        height = newHeight;
        x0 = (float) (x + width/scale/2.0);
        y0 = (float) (y + height/scale/2.0);


        //System.out.println("WKT x0="+x0+" y0="+y0+"  width="+width+"  height="+height+"  size="+size+"  scale="+scale);

//        width = (int) (width/scale);
//        height = (int) (height/scale);
//        size = Math.max(width,height);
       ((RectangularShape)shape).setFrame(0.0,0.0,width,height);
       return wktShape;
    }

    public void paint(Graphics2D g, int type, WKTshape wktShape, int size, boolean showLine, Color lineColor,
                      boolean showFill, Color fillColor, boolean dotted)
    {
        if(type == WKTS)
        {
            //System.out.println("Painting WKT shape: size="+size+" x0="+x0+" y0="+y0);
            if(wktShape == null)
            {
                g.setColor(Color.BLACK);
                g.drawString("No WKT",0,0+10);
            }
            else
            {
                wktShape.paint(g, size, showLine, lineColor, showFill, fillColor, dotted);
            }
            if(dotted)
            {
            	g.setColor(Color.BLACK);
                g.fillRect(-1, -1, 2, 2);
            }
        }
    }

    public void paint(Graphics2D g, int type, Image image)
    {
       if(type == IMAGES)
       {
           if(image == null)
            {
                g.setColor(Color.BLACK);
                g.drawString("No Image",0,0+10);
            }
            else
            {
                 int sizex = size;
                 int sizey = size*height/width;
                //g.drawImage(image,x,y,(int)(width/scale), (int)(height/scale),null);
                //g.drawImage(image,x,y,size, size,null);
                g.drawImage(image,(int)(x0-sizex/2),(int) (y0-sizey/2), sizex, sizey, null);
            }
       }
    }
    public void paint(Graphics2D g, int type, GeneralPath path, boolean showLine, Color lineColor,
                      boolean showFill, Color fillColor, boolean dotted)
    {
        if(path == null) return;
         switch(type)
         {
             case POLYS:
             case STARS:
             case ANYS:
                        g.setColor(fillColor);
                        if(showFill) g.fill(path);
                        g.setColor(lineColor);
                        if(showLine) g.draw(path);

                        if(dotted)
                        {
                        	g.setColor(Color.BLACK);
                        	//System.out.println("ANYS dotted "+ path.getBounds2D().toString());
                        	Rectangle bounds = path.getBounds();
                        	g.fillRect(bounds.x+bounds.width/2-1,bounds.y+bounds.height/2-1,2,2);
                            //g.fillRect(-1, -1, 2, 2);
                        }
         }
    }
}
