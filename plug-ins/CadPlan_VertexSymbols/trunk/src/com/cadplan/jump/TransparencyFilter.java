package com.cadplan.jump;

import java.awt.image.RGBImageFilter;
import java.awt.*;

/**
 * User: geoff
 * Date: 3/08/2007
 * Time: 10:28:44
 * Copyright 2005 Geoffrey G Roy.
 */
public class TransparencyFilter extends RGBImageFilter
{
    int markerRGB;
    int base  = 0x00FFFFFF;
    int trans = 0x00000000;
    int limit = 150;
    int newrgb;

    public TransparencyFilter(Color color, double alpha)
    {
//        int red = color.getRed();
//        int green = color.getGreen();
//        int blue = color.getBlue();
//        if(red > limit && green > limit && blue > limit) color = Color.WHITE;

        markerRGB = color.getRGB() | 0xFF000000;
        trans = (int)(alpha*255) << 24;
    }
    public int filterRGB(int x, int y, int rgb)
    {
        Color newColor = null;
         if ( ( rgb | 0xFF000000 ) == markerRGB )
         {
              // Mark the alpha bits as zero - transparent
               //return 0x00FFFFFF & rgb;
             newrgb = (base | trans) & rgb;
             newColor = new Color(newrgb, true);
         }
         else
         {
              // nothing to do
              newrgb= rgb;
         }
         //if(x == 0 && y == 0) System.out.println("Pixel: 0,0: "+Integer.toHexString(newrgb)+"  alpha="+newColor.getAlpha());
         return newrgb;
    }
}
