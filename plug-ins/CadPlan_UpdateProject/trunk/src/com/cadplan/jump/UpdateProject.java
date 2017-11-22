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
import com.cadplan.fileio.FileChooser;
import com.cadplan.fileio.TextFile;
import com.vividsolutions.jump.util.Blackboard;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.*;
import java.util.Vector;
import java.io.File;

/**
 * User: geoff
 * Date: 7/01/2007
 * Time: 08:15:46
 * Copyright 2005 Geoffrey G Roy.
 */
public class UpdateProject extends JDialog implements ActionListener
{
    boolean debug = true;
    public StringBuffer sb = new StringBuffer();
    Vector<LayerInfo> layers = new Vector<LayerInfo>(10,5);
    int index = 0;
    String data;
    JLabel nameLabel, dirLabel, fileLabel, okLabel;
    JButton cancelButton, checkButton, acceptButton, aboutButton;
    JTextField [] nameField, dirField, fileField, okField;
    JButton[] updateButton;
    String dirName = null;
    String fileName = null;
    Blackboard blackboard;
    String [] statusName = {"Unknown", "True", "False"};



    public UpdateProject(Blackboard blackboard, String data)
    {
        super(new JFrame(),"Project Updater", true);
        this.data = data;
        this.blackboard = blackboard;
        extractLayerInfo(data);
        init();

    }

    /**
     * sets up user interface
     */
    public void init()
    {
         GridBagDesigner gb = new GridBagDesigner(this);
         JPanel listPanel = new JPanel();
         GridBagDesigner gbp = new GridBagDesigner(listPanel);

        nameField = new JTextField[layers.size()];
        dirField = new JTextField[layers.size()];
        fileField = new JTextField[layers.size()];
        okField = new JTextField[layers.size()];
        updateButton = new JButton[layers.size()];


         nameLabel = new JLabel("Layer Name");
         gbp.setPosition(0,0);
         gbp.setInsets(0,0,0,0);
         gbp.setAnchor(GridBagConstraints.WEST);
         gbp.addComponent(nameLabel);


         dirLabel = new JLabel("Directory");
         gbp.setPosition(1,0);
         gbp.setInsets(0,0,0,0);
         gbp.setAnchor(GridBagConstraints.WEST);
         gbp.addComponent(dirLabel);


         fileLabel = new JLabel("File Name");
         gbp.setPosition(2,0);
         gbp.setInsets(0,0,0,0);
         gbp.setAnchor(GridBagConstraints.WEST);
         gbp.addComponent(fileLabel);

         okLabel = new JLabel("Status");
         gbp.setPosition(3,0);
         gbp.setInsets(0,0,0,0);
         //gbp.setAnchor(GridBagConstraints.WEST);
         gbp.addComponent(okLabel);



         for (int i=0; i < layers.size(); i++)
         {
             nameField[i] = new JTextField(15);
             gbp.setPosition(0,i+1);
             gbp.setInsets(0,0,0,0);
             gbp.setWeight(1.0,0.0);
             gbp.setFill(GridBagConstraints.HORIZONTAL);
             gbp.addComponent(nameField[i]);
             nameField[i].setText(layers.elementAt(i).name);
             nameField[i].setEditable(false);

             dirField[i] = new JTextField(30);
             gbp.setPosition(1,i+1);
             gbp.setInsets(0,0,0,0);
             gbp.setWeight(2.0,0.0);
             gbp.setFill(GridBagConstraints.HORIZONTAL);
             gbp.addComponent(dirField[i]);
             dirField[i].setText(layers.elementAt(i).dirName);
             dirField[i].addActionListener(this);


             fileField[i] = new JTextField(15);
             gbp.setPosition(2,i+1);
             gbp.setInsets(0,0,0,0);
             gbp.setWeight(1.0,0.0);             
             gbp.setFill(GridBagConstraints.HORIZONTAL);
             gbp.addComponent(fileField[i]);
             fileField[i].setText(layers.elementAt(i).fileName);
             fileField[i].setEditable(false);

             okField[i] = new JTextField(7);
             okField[i].setHorizontalAlignment(SwingConstants.CENTER);
             gbp.setPosition(3,i+1);
             gbp.setInsets(0,0,0,0);
             gbp.addComponent(okField[i]);
             okField[i].setText(statusName[layers.elementAt(i).status]);
             okField[i].setEditable(false);

             colorOK(okField[i],0);

             updateButton[i] = new JButton("Update");
             gbp.setPosition(4,i+1);
             gbp.setInsets(0,0,0,0);
             gbp.addComponent(updateButton[i]);
             updateButton[i].addActionListener(this);

         }
         JScrollPane scrollPane = new JScrollPane(listPanel, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

         gb.setPosition(0,0);
         gb.setInsets(0,0,0,0);
         gb.setSpan(4,1);
         gb.setWeight(1.0,1.0);
         gb.setFill(GridBagConstraints.BOTH);
         gb.setAnchor(GridBagConstraints.NORTHWEST);
         gb.addComponent(scrollPane);

        cancelButton = new JButton("Cancel");
        gb.setPosition(0,1);
        gb.setInsets(0,10,0,0);
        gb.addComponent(cancelButton);
        cancelButton.addActionListener(this);

        aboutButton = new JButton("About");
        gb.setPosition(1,1);
        gb.setInsets(0,10,0,0);
        gb.addComponent(aboutButton);
        aboutButton.addActionListener(this);

        checkButton = new JButton("Check");
        gb.setPosition(2,1);
        gb.setInsets(0,10,0,0);
        gb.addComponent(checkButton);
        checkButton.addActionListener(this);

        acceptButton = new JButton("Accept");
        gb.setPosition(3,1);
        gb.setInsets(0,10,0,10);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.addComponent(acceptButton);
        acceptButton.addActionListener(this);

        for (int i=0; i < layers.size(); i++)
        {
            checkLayer(i);
        }
        int paneHeight = 24*layers.size() + 80;
        if(paneHeight > 500) paneHeight = 500;
        setSize(new Dimension(800,paneHeight));
        //setSize(800,300);
        //pack();
        setVisible(true);

    }

    /**
     * extracts all layers (info) from project file data
     * @param data
     */
    private void extractLayerInfo(String data)
    {

        boolean more = true;

        while(more)
        {
            LayerInfo layerInfo = nextLayer(data);

            if(layerInfo == null) more = false;
            else
            {
                if(debug) System.out.println(layerInfo.toString()+"\n");
                layers.addElement(layerInfo);
            }
        }
    }

    /**
     * extracts the next layer from the project file
     * @param data
     * @return
     */
    private LayerInfo nextLayer(String data)
    {
       int layerIndex = -1;
       int nameIndexStart = -1;
       int nameIndexEnd = -1;
       int fileIndexStart = -1;
       int locationIndexStart = -1;
       int locationIndexEnd = -1;
       int descriptionStart = -1;
       int descriptionEnd = -1;

        String layerKey = "<layer class";
        String nameKeyStart = "name=\"";
        String nameKeyEnd = "\"";
        String descriptionKeyStart = "<description>";
        String descriptionKeyEnd = "</description>";
        String fileKeyStart = ">File</key>";
        String locationKeyStart = "<value class=\"java.lang.String\">";
        String locationKeyEnd = "<";
        String serverURLStart = "<server-url>";
        String serverURLEnd = "</server-url>";
        String rasterNameStart ="<name>";
        String rasterNameEnd = "</name>";
        String rasterDescStart = "<imageFileName>";
        String rasterDescEnd = "</imageFileName>";
        String rasterFileStart = "file name</key>";



         int status = 0;

        layerIndex = data.indexOf(layerKey, index);
        if(layerIndex < 0) return null;

        int classIndexStart = layerIndex+layerKey.length()+2;
        int classIndexEnd = data.indexOf("\"",classIndexStart+1);
        String className = data.substring(classIndexStart, classIndexEnd);
        String layerType = className.substring(className.lastIndexOf(".")+1);
        if(layerType.startsWith("SystemLayerFinder")) layerType = "Layer";
        if(layerType.startsWith("RasterImageLayer")) layerType = "RasterLayer";
        if(debug) System.out.println("className:"+className+" type:"+layerType+"\n");

        if(layerType.equals("Layer"))
        {
            nameIndexStart = data.indexOf(nameKeyStart, layerIndex) + nameKeyStart.length();
            nameIndexEnd = data.indexOf(nameKeyEnd, nameIndexStart);

            descriptionStart = data.indexOf(descriptionKeyStart,nameIndexEnd) + descriptionKeyStart.length();
            descriptionEnd = data.indexOf(descriptionKeyEnd, descriptionStart);


            fileIndexStart = data.indexOf(fileKeyStart, nameIndexEnd);
            locationIndexStart = data.indexOf(locationKeyStart, fileIndexStart) + locationKeyStart.length();
            locationIndexEnd = data.indexOf(locationKeyEnd,locationIndexStart);

            String name = data.substring(nameIndexStart,nameIndexEnd);
            String description = data.substring(descriptionStart, descriptionEnd);
            String location = data.substring(locationIndexStart, locationIndexEnd);
            int n = location.lastIndexOf(File.separator);

            String dirName = location.substring(0,n);
            String fileName = location.substring(n+1);
            LayerInfo layerInfo = new LayerInfo(name, description, dirName, fileName, nameIndexStart, nameIndexEnd,
                    descriptionStart, descriptionEnd, locationIndexStart,
                    locationIndexEnd,status,"Layer");
            index = locationIndexEnd;
            return layerInfo;
        }
        if(layerType.equals("WMSLayer"))
        {
            nameIndexStart = data.indexOf(rasterNameStart, layerIndex) + rasterNameStart.length();
            nameIndexEnd = data.indexOf(rasterNameEnd, nameIndexStart);
            locationIndexStart = data.indexOf(serverURLStart, nameIndexEnd) + serverURLStart.length();
            locationIndexEnd = data.indexOf(serverURLEnd,locationIndexStart);

            String description = "";
            String name = data.substring(nameIndexStart,nameIndexEnd);
            String location = data.substring(locationIndexStart, locationIndexEnd);

            String dirName = location;
            String fileName = "N/A";
            LayerInfo layerInfo = new LayerInfo(name, description, dirName, fileName, nameIndexStart, nameIndexEnd,
                    descriptionStart, descriptionEnd, locationIndexStart,
                    locationIndexEnd,status,"WMSLayer");
            index = locationIndexEnd;
            return layerInfo;
        }
        if(layerType.equals("RasterLayer"))
        {
            nameIndexStart = data.indexOf(rasterNameStart, layerIndex) + rasterNameStart.length();
            nameIndexEnd = data.indexOf(rasterNameEnd, nameIndexStart);

            descriptionStart = data.indexOf(rasterDescStart, nameIndexEnd) + rasterDescStart.length();
            descriptionEnd = data.indexOf(rasterDescEnd, descriptionStart);

            fileIndexStart = data.indexOf(rasterFileStart, nameIndexEnd);
            locationIndexStart = data.indexOf(locationKeyStart, fileIndexStart) + locationKeyStart.length();
            locationIndexEnd = data.indexOf(locationKeyEnd,locationIndexStart);

            String name = data.substring(nameIndexStart,nameIndexEnd);
            String description = data.substring(descriptionStart,descriptionEnd);
            String location = data.substring(locationIndexStart, locationIndexEnd);
            int n = location.lastIndexOf(File.separator);

            String dirName = location.substring(0,n);
            String fileName = location.substring(n+1);
            LayerInfo layerInfo = new LayerInfo(name, description, dirName, fileName, nameIndexStart, nameIndexEnd,
                    descriptionStart, descriptionEnd,locationIndexStart,
                    locationIndexEnd,status,"RasterLayer");
            index = locationIndexEnd;
            return layerInfo;
        }
        return null;
    }

    /**
     * set the color on the OK field
     * @param field
     * @param status
     */
    private void colorOK(JTextField field, int status)
    {
        if(status == 1) field.setBackground(Color.GREEN);
        else if(status == 0) field.setBackground(Color.ORANGE);
        else field.setBackground(Color.RED);
    }

    /**
     * checks the nopminated layer, and updates status
     * @param i
     */
    private void checkLayer(int i)
    {
        LayerInfo layer = layers.elementAt(i);
        if(layer.type.equals("Layer") || layer.type.equals("RasterLayer"))
        {
            File file = new File(tidyName(layer.dirName+File.separator+layer.fileName));
             if(file.exists())
             {
                 layer.status = 1;
                 colorOK(okField[i],1);
             }
             else
             {
                 layer.status = 2;
                 colorOK(okField[i],2);
             }
             okField[i].setText(statusName[layer.status]);
        }
        if(layer.type.equals("WMSLayer"))
        {

        }
    }

    /**
     * tities the file name in case it contains special chars
     * @param name
     * @return
     */
    private String tidyName(String name)
    {
        int pos = 0;
        String s = name.replaceAll("&amp;","&");               
        return s;
    }
    /**
     * handles button events
     * @param ev
     */
    public void actionPerformed(ActionEvent ev)
    {
        if(ev.getSource() ==cancelButton)
        {
            dispose();
        }
         if(ev.getSource() ==acceptButton)
        {
            int result = JOptionPane.showConfirmDialog(this,"Update Project File","Confirm Update", JOptionPane.YES_NO_OPTION);
            if(result == JOptionPane.YES_OPTION)
            {
                String [] ext = {"jmp"};
                FileChooser chooser = new FileChooser(this,dirName, null, ext, "OpenJUMP Project Files",
                              JFileChooser.SAVE_DIALOG);
                String fileName = chooser.getFile();
                String dirName = chooser.getDir();
                TextFile textFile = new TextFile(dirName, fileName);
                textFile.openWrite();
                StringBuffer newData = new StringBuffer();
                int index = 0;
                for (int i=0; i < layers.size(); i++)
                {
                    LayerInfo layer = layers.elementAt(i);
                    if(layer.type.equals("Layer"))
                    {
                        newData.append(data.substring(index,layer.descriptionIndexStart));
                        newData.append(layer.description);
                        index = layer.descriptionIndexEnd;

                        newData.append(data.substring(index,layer.locationIndexStart));
                        newData.append(layer.dirName+File.separator+layer.fileName);
                        index = layer.locationIndexEnd;
                    }
                    if(layer.type.equals("WMSLayer"))
                    {
                        newData.append(data.substring(index,layer.locationIndexStart));
                        newData.append(layer.dirName);
                        index = layer.locationIndexEnd;
                    }
                    if(layer.type.equals("RasterLayer"))
                    {
                        newData.append(data.substring(index,layer.descriptionIndexStart));
                        newData.append(layer.dirName+File.separator+layer.fileName);
                        index = layer.descriptionIndexEnd;

                        newData.append(data.substring(index,layer.locationIndexStart));
                        newData.append(layer.dirName+File.separator+layer.fileName);
                        index = layer.locationIndexEnd;
                    }


                }
                newData.append(data.substring(index));
                textFile.write(newData.toString());
                textFile.close();

            }
            dispose();
        }
        if(ev.getSource() == checkButton)
        {
             for (int i=0; i < layers.size(); i++)
             {
                 LayerInfo layer = layers.elementAt(i);
                 checkLayer(i);
             }
        }

        for (int i=0; i < layers.size(); i++)
        {
            if(ev.getSource() == updateButton[i])
            {
                LayerInfo layer = layers.elementAt(i);
                if(layer.type.equals("Layer") || layer.type.equals("RasterLayer"))
                {
                    int extIndex = layer.fileName.lastIndexOf(".");
                    String[] ext = {layer.fileName.substring(extIndex+1)};
                    FileChooser chooser = new FileChooser(this,dirName, layer.fileName, ext, "OpenJUMP Project Files",
                                  JFileChooser.OPEN_DIALOG);
                    fileName = chooser.getFile();
                    dirName = chooser.getDir();
                    if(fileName != null)
                    {
                        layer.dirName = dirName;
                        dirField[i].setText(dirName);
                    }
                    checkLayer(i);
                }
                if(layer.type.equals("WMSLayer"))
                {
                    String result = JOptionPane.showInputDialog(this,"Enter URL:", layer.dirName);
                    if(result == null) return;
                    layer.dirName = result;
                    dirField[i].setText(result);

                }
            }

            if(ev.getSource() == dirField[i])
            {
                LayerInfo layer = layers.elementAt(i);

                String newDir = dirField[i].getText();
                layer.dirName = newDir;
                checkLayer(i);

            }
        }
        if(ev.getSource() == aboutButton)
        {
            JOptionPane.showMessageDialog(this,"Jump Project Updater Plugin: vers:"+blackboard.get("Version","xxx")+
            "\n© 2006 Cadplan\n"+
            "http://www.cadplan.com.au","About...",JOptionPane.INFORMATION_MESSAGE);

        }
    }
}
