/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * Copyright (C) 2006 Cadplan
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 */

package com.cadplan.jump;

import com.vividsolutions.jump.workbench.ui.renderer.style.BasicFillPattern;
import com.vividsolutions.jump.workbench.ui.renderer.style.ImageFillPattern;
import com.vividsolutions.jump.workbench.ui.images.IconLoader;
import com.vividsolutions.jump.util.Blackboard;
import com.vividsolutions.jump.util.CollectionUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;

/**
 * User: geoff
 * Date: 4/07/2007
 * Time: 10:53:12
 * Copyright 2005 Geoffrey G Roy.
 */
public class MyImageFillPattern extends BasicFillPattern //ImageFillPattern
{
    private boolean debug = false;
    private static final String FILENAME_KEY = "FILENAME";

    public MyImageFillPattern( String imageName)
    {

         super(new Blackboard().putAll(CollectionUtil.createMap(
                    new Object[]
                    {
                        BasicFillPattern.COLOR_KEY, Color.black, 
                        FILENAME_KEY, imageName
                    })));

        if(debug) System.out.println("MyImageFillPattern constructor 2:"); //   width="+this.image.getWidth(null));
    }
    /**
     * Parameterless constructor for Java2XML
     */
    public MyImageFillPattern()
    {
         if(debug) System.out.println("MyImageFillPattern constructor 0:");

    }

    public MyImageFillPattern clone()
    {
         if(debug) System.out.println("Cloning MyImageFillPattern");
        String imageName = (String) getProperties().get(FILENAME_KEY);
        MyImageFillPattern fillPattern = new MyImageFillPattern(imageName);
        fillPattern.setProperties((Blackboard)(getProperties().clone()));

        return fillPattern;
    }


    public boolean equals(Object obj)
    {
        //System.out.println("Equality test");
        if(obj == null ) return false;
        if(obj instanceof MyImageFillPattern)
        {
            String imageName = (String) getProperties().get(FILENAME_KEY);
            //if (((MyImageFillPattern)obj).imageName == null) return false;
            //if(((MyImageFillPattern)obj).imageName.equals(imageName))
            if(imageName.equals(((MyImageFillPattern)obj).getProperties().get(FILENAME_KEY)))
            {
                if(debug) System.out.println("Equality: true");
                return true;
            }

        }
       return false;
    }

    public BufferedImage createImage(Blackboard properties)
    {

        String imageName = (String) getProperties().get(FILENAME_KEY);
        Image image = null;
        for(int i=0; i < FillPatternParams.imageNames.length; i++)
        {
            if(FillPatternParams.imageNames[i].equals(imageName)) image = FillPatternParams.images[i];
        }
        BufferedImage bufferedImage = null;
        if(image == null)
        {
            System.out.println("createImage: NULL image");
            bufferedImage = new BufferedImage(100,  25, BufferedImage.TYPE_INT_ARGB);
            Graphics2D g = (Graphics2D) bufferedImage.getGraphics();
            g.setColor(Color.BLACK);
            g.drawString("No Pattern",0,20);
        }
        else
        {
            bufferedImage = new BufferedImage(image.getWidth(null),  image.getHeight(null), BufferedImage.TYPE_INT_ARGB);
            Graphics2D g = (Graphics2D) bufferedImage.getGraphics();
            g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
                    ((Color) getProperties().get(COLOR_KEY)).getAlpha() / 255f));
            g.drawImage(image, 0, 0, null);
        }

        return bufferedImage;
    }
}
