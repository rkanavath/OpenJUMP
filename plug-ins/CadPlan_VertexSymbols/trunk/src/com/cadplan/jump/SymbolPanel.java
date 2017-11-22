package com.cadplan.jump;

import com.cadplan.designer.GridBagDesigner;

import javax.swing.*;
import java.awt.*;

/**
 * User: geoff
 * Date: 16/06/2007
 * Time: 13:26:45
 * Copyright 2005 Geoffrey G Roy.
 */
public class SymbolPanel extends JPanel
{
    int numSymbols = 21;
    public JRadioButton [] vertexRB = new JRadioButton[numSymbols];
    JPanel [] symbol = new VertexPanel[numSymbols];
    int [] sides = {2,3,4,5,6,8,16,2,3,4,5,6,8,16,0,1,2,3,4,5,6};
    ButtonGroup group;

    public SymbolPanel(ButtonGroup group)
    {
        this.group = group;
        init();
    }

    public int getIndex(int n, int type)
    {
        for (int i=0; i < sides.length ; i++)
        {
           if(i < 7 && type == VertexParams.POLYGON && sides[i] == n) return i;
           if(i >= 7 && type == VertexParams.STAR && sides[i] == n) return i;
           if(i >= 14 && type == VertexParams.ANYSHAPE && sides[i] == n) return i;
        }
        return -1;
    }

    public int getTypeIndex(int n, int type)
    {
        for (int i=0; i < sides.length ; i++)
        {
           if(i < 7 && type == ExternalSymbolsRenderer.POLYS && sides[i] == n) return i;
           if(i >= 7 && type == ExternalSymbolsRenderer.STARS  && sides[i] == n) return i;
           if(i >= 14 && type == ExternalSymbolsRenderer.ANYS  && sides[i] == n) return i;
        }
        return -1;
    }

    public int getImageIndex(String name)
    {
        int index = -1;
        for(int i=0; i < VertexParams.imageNames.length; i++)
        {
            if( VertexParams.imageNames[i].equals(name)) return i;
        }
        return index;
    }

    public int getWKTIndex(String name)
    {
        int index = -1;
        for(int i=0; i < VertexParams.wktNames.length; i++)
        {
            if( VertexParams.wktNames[i].equals(name)) return i;
        }
        return index;
    }

    public void init()
    {
        GridBagDesigner gb = new GridBagDesigner(this);

        
        int col = 0;
        int row = 0;
        for (int i=0; i < numSymbols; i++)
        {
            col = 2*(i / 7);
            row = i % 7;
            vertexRB[i] = new JRadioButton();
            vertexRB[i].setBackground(Color.WHITE);
            group.add(vertexRB[i]);

            gb.setPosition(col+1,row);
            gb.setInsets(0,10,10,30);
            gb.setAnchor(GridBagConstraints.WEST);
            gb.addComponent(vertexRB[i]);

            if(i < 7) symbol[i] = new VertexPanel(sides[i], VertexParams.POLYGON);
            else if(i < 14) symbol[i] = new VertexPanel(sides[i], VertexParams.STAR);
            else symbol[i] = new VertexPanel(sides[i],VertexParams.ANYSHAPE);
            gb.setPosition(col,row);
            //gb.setAnchor(GridBagConstraints.NORTHWEST);
            gb.addComponent(symbol[i]);

        }
    }


    
}
