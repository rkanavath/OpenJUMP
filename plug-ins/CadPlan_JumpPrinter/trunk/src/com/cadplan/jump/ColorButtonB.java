package com.cadplan.jump;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.*;

/**
 * User: geoff
 * Date: 16/01/2007
 * Time: 14:22:56
 * Copyright 2005 Geoffrey G Roy.
 */
public class ColorButtonB extends Button implements ActionListener
{
    Color color;
    Furniture item;

    public ColorButtonB(Furniture item)
    {
        super("      ");
        this.item = item;
        color = item.bcolor;
        setBackground(color);
        addActionListener(this);
    }

    public Color getColor()
    {
        return color;
    }
    
    public void setItem(Furniture item)
    {
    	this.item = item;
        color = item.bcolor;
        setBackground(color);
    }
    public void actionPerformed(ActionEvent ev)
    {
         Color newColor = JColorChooser.showDialog(this,"Select color", color);
         if(newColor != null)
         {
             color = newColor;
             setBackground(color);
             item.bcolor = color;
         }
    }
}

