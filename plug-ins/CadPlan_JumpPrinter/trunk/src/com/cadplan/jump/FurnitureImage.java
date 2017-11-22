package com.cadplan.jump;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Stroke;

public class FurnitureImage extends Furniture
{
	Image image;
    int dragSize = 10;
    String fileName;
    boolean ratioLocked = true;

    public FurnitureImage( String fileName, boolean show )
    {
        this.show = show;
        this.fileName = fileName;
        
        layerNumber = 0;
    }
   

    /**
     * test for a point inside the drag square (bottom-right)
     * @param x
     * @param y
     * @param scale
     * @return
     */
    public boolean insideBR(int x, int y, double scale, double globalScale)
    {
        boolean inside = false;
        if(show)
        {
            inside = true;
            if(x < (location.x+location.width)*scale-dragSize/globalScale) inside = false;
            if(x > (location.x+location.width)*scale) inside = false;
            if(y < (location.y+location.height)*scale-dragSize/globalScale) inside = false;
            if(y > (location.y+location.height)*scale) inside = false;
            //JOptionPane.showMessageDialog(null,"x="+x+" y="+y+"  scale="+scale+"  location:"+location+"  inside="+inside);
        }
        return inside;
    }
    /**
     * test for a point inside the drag square (top-left)
     * @param x
     * @param y
     * @param scale
     * @return
     */
    public boolean insideTL(int x, int y, double scale, double globalScale)
    {
        boolean inside = false;
        if(show)
        {
            inside = true;
            if(x < (location.x)*scale) inside = false;
            if(x > (location.x)*scale+dragSize/globalScale) inside = false;
            if(y < (location.y)*scale) inside = false;
            if(y > (location.y)*scale+dragSize/globalScale) inside = false;
            //JOptionPane.showMessageDialog(null,"x="+x+" y="+y+"  scale="+scale+"  location:"+location+"  inside="+inside);
        }
        return inside;
    }
    /**
     * sets the border dimensions, depends of if border is fixed and whether a forced change is required.
     * @param w
     * @param h
     * @param force
     */
    public void setImage(Image image, int x, int y, int w, int h)
    {
          this.image = image;
          location.x = x;
          location.y = y;
          location.width = w;
          location.height = h;

          
    }
    
    public void setImage( int x, int y, int w, int h)
    {
    	location.x = x;
        location.y = y;
        location.width = w;
        location.height = h;
    }

    public void paint(Graphics g, double scale, double globalScale)
    {
        Graphics2D g2 = (Graphics2D) g;
        
        
        g.setColor(color);
        

        if(scale > 0.0)
        {
            g2.scale(scale,scale);
        }
             
        int x = location.x ;
        int y = location.y ;
        int w = location.width ;
        int h = location.height;

       g.drawImage(image, x, y, w, h, null);
        

        if(scale > 0.0)
        {
            g.setColor(Color.BLUE);
            int dx = (int) (10.0/(scale*globalScale));
            g.drawRect(location.x+location.width-dx,location.y+location.height-dx, dx,dx);
            g.drawRect(location.x,location.y, dx,dx);
            g2.setColor(boundsColor);
            g2.setStroke(boundsStroke);
            g2.drawRect(location.x, location.y, location.width,  location.height);
            g2.setStroke(normalStroke);
        }
        if(scale > 0.0)
        {
            g2.scale(1.0/scale, 1.0/scale);
        }




    }
   
}
