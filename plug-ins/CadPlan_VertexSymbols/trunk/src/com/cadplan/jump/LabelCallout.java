package com.cadplan.jump;

import java.awt.*;
import java.awt.geom.GeneralPath;

/**
 * User: geoff
 * Date: 3/08/2007
 * Time: 15:47:00
 * Copyright 2005 Geoffrey G Roy.
 */
public class LabelCallout
{

    public LabelCallout(Graphics g,int border, int position,int x0, int y0, int x, int y, int w, int h,
                        Color foregroundColor, Color backgroundColor, boolean fillText)
    {
        GeneralPath path = new  GeneralPath();
        Graphics2D g2 = (Graphics2D) g;
        int s = 7;
        if(border == 1)
        {
            g.drawRect(x, y, w, h );
            path.moveTo(x,y);
            path.lineTo(x+w,y);
            path.lineTo(x+w,y+h);
            path.lineTo(x,y+h);
            path.lineTo(x,y);

        }
        else if (border == 2)
        {
            switch (position)
            {
                case 0:  g.drawRect(x, y, w, h );
                         break;
                case 1:
                case 2:
                case 8:
                         path.moveTo(x0,y0);
                         path.lineTo(x+w/2-s, y+h);
                         path.lineTo(x,y+h);
                         path.lineTo(x,y);
                         path.lineTo(x+w,y);
                         path.lineTo(x+w,y+h);
                         path.lineTo(x+w/2+s,y+h);
                         path.lineTo(x0,y0);
                    break;
                case 3:  path.moveTo(x0,y0);
                         path.lineTo(x, y+h/2-s);
                         path.lineTo(x,y);
                         path.lineTo(x+w,y);
                         path.lineTo(x+w,y+h);
                         path.lineTo(x,y+h);
                         path.lineTo(x,y+h/2+s);
                         path.lineTo(x0,y0);
                         break;
                case 4:
                case 5:
                case 6: path.moveTo(x0,y0);
                         path.lineTo(x+w/2+s, y);
                         path.lineTo(x+w,y);
                         path.lineTo(x+w,y+h);
                         path.lineTo(x,y+h);
                         path.lineTo(x,y);
                         path.lineTo(x+w/2-10,y);
                         path.lineTo(x0,y0);
                        break;
                case 7: path.moveTo(x0,y0);
                         path.lineTo(x+w, y+h/2+s);
                         path.lineTo(x+w,y+h);
                         path.lineTo(x,y+h);
                         path.lineTo(x,y);
                         path.lineTo(x+w,y);
                         path.lineTo(x+w,y+h/2-10);
                         path.lineTo(x0,y0);
                       break;
            }

        }
        if(fillText)
        {
            g2.setColor(backgroundColor);
            g2.fill(path);
        }
        if(border > 0)
        {
              g2.setColor(foregroundColor);
              g2.draw(path);
        }

    }
}
