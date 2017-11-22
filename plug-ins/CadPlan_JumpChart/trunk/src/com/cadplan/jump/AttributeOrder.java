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

import com.cadplan.designer.GridBagDesigner;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.*;

/**
 * User: geoff
 * Date: 10/05/2007
 * Time: 10:03:35
 * Copyright 2005 Geoffrey G Roy.
 */
public class AttributeOrder  extends JDialog implements ActionListener {

    JList attributeList;
    JButton upButton, downButton, acceptButton, cancelButton;
    String [] localList;
    int numItems;
    ChartDialog parent;
    I18NPlug iPlug;

    public AttributeOrder(ChartDialog parent, I18NPlug iPlug)
    {
        super(parent, iPlug.get("JumpChart.Order.Order"), true);
        this.parent = parent;
        this.iPlug = iPlug;
        numItems = 0;
        for (int i=0; i < ChartParams.attributes.length; i++)
        {
            if(ChartParams.includeAttribute[i]) numItems++;
        }
        //System.out.println("number of selected items = "+numItems);
        
        localList = new String[numItems];
        int k = 0;
        for (int i=0; i < ChartParams.attributes.length; i++)
        {
            if(ChartParams.includeAttribute[i])
            {
                String item = ChartParams.attributes[i].name;
                int order = ChartParams.attributeOrder[i];
                if(order >= 0) localList[order] = ChartParams.attributes[i].name;
                else  localList[k] = ChartParams.attributes[i].name;
                k++;
            }
        }
        init();
    }

    public void init()
    {
        GridBagDesigner gb = new GridBagDesigner(this);

        JPanel panel = new JPanel();       
        gb.setPosition(0,0);
        gb.setInsets(10,10,10,10);
        gb.setSpan(1,4);
        attributeList = new JList(localList);
        Border border = new EtchedBorder(EtchedBorder.LOWERED);
        panel.setBorder(border);
        panel.setBackground(Color.WHITE);
        panel.add(attributeList);
        gb.addComponent(panel);

        upButton = new JButton(iPlug.get("JumpChart.Order.MoveUp"));
        gb.setPosition(1,0);
        gb.setInsets(10,0,0,10);
        gb.setFill(GridBagConstraints.HORIZONTAL);
        gb.addComponent(upButton);

        downButton = new JButton(iPlug.get("JumpChart.Order.MoveDown"));
        gb.setPosition(1,1);
        gb.setInsets(10,0,0,10);
        gb.setFill(GridBagConstraints.HORIZONTAL);
        gb.addComponent(downButton);

        acceptButton = new JButton(iPlug.get("JumpChart.Order.Accept"));
        gb.setPosition(1,2);
        gb.setInsets(10,0,0,10);
        gb.setFill(GridBagConstraints.HORIZONTAL);
        gb.addComponent(acceptButton);

        cancelButton = new JButton(iPlug.get("JumpChart.Order.Cancel"));
        gb.setPosition(1,3);
        gb.setInsets(10,0,10,10);
        gb.setFill(GridBagConstraints.HORIZONTAL);
        gb.setAnchor(GridBagConstraints.SOUTH);
        gb.addComponent(cancelButton);

        upButton.addActionListener(this);
        downButton.addActionListener(this);
        acceptButton.addActionListener(this);
        cancelButton.addActionListener(this);

        pack();
        setLocation(parent.getLocation());
        setVisible(true);

    }
    public void actionPerformed(ActionEvent ev)
    {
         if(ev.getSource() == upButton)
         {
             int selectedIndex = attributeList.getSelectedIndex();
             if(selectedIndex < 1) return;
             String temp = localList[selectedIndex-1];
             localList[selectedIndex-1] = localList[selectedIndex];
             localList[selectedIndex] = temp;
             attributeList.setListData(localList);
             attributeList.setSelectedIndex(selectedIndex-1);


         }
        if(ev.getSource() == downButton)
         {
             int selectedIndex = attributeList.getSelectedIndex();
             if(selectedIndex < 0 || selectedIndex > (localList.length-2)) return;
             String temp = localList[selectedIndex+1];
             localList[selectedIndex+1] = localList[selectedIndex];
             localList[selectedIndex] = temp;
             attributeList.setListData(localList);
             attributeList.setSelectedIndex(selectedIndex+1);
         }
        if(ev.getSource() == acceptButton)
         {
              for (int i=0; i < numItems; i++)
              {
                  String item = localList[i];
                  for (int j=0; j < ChartParams.attributes.length; j++)
                  {
                      if(ChartParams.attributes[j].name.equals(item)) ChartParams.attributeOrder[j] = i;
                  }
              }
             dispose();
         }
        if(ev.getSource() == cancelButton)
         {
             dispose();
         }
    }

}
