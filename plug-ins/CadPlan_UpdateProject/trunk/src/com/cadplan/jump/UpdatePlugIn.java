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

import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.util.Blackboard;
import com.cadplan.fileio.FileChooser;
import com.cadplan.fileio.TextFile;

import javax.swing.*;
import java.awt.*;
import java.io.File;

/**
 * User: geoff
 * Date: 7/01/2007
 * Time: 07:22:14
 * Copyright 2005 Geoffrey G Roy.
 */
public class UpdatePlugIn extends AbstractPlugIn


{
    String version ="0.6";
    Blackboard blackboard;
    final Throwable[] throwable = new Throwable[] { null };
    boolean cancelled = false;
    String fileName = null;
    String dirName = null;
    String [] ext = {"jmp"};
    StringBuffer sb = new StringBuffer();


    public void initialize(PlugInContext context) throws Exception
    {
         EnableCheckFactory check = new EnableCheckFactory(context.getWorkbenchContext());
         context.getFeatureInstaller().addMainMenuItem(this, new String[] {"File"}, getName(),
                 false, null, null);
         blackboard = new Blackboard();
         blackboard.put("Version",version);




    }


    public boolean execute(PlugInContext context) throws Exception
    {

        FileChooser chooser = new FileChooser(null,dirName, fileName, ext, "OpenJUMP Project Files",
                  JFileChooser.OPEN_DIALOG);
        fileName = chooser.getFile();
        dirName = chooser.getDir();
        if(fileName == null) return true;
        //sb.append("Dir:"+dirName+"  File:"+fileName+"\n");

        TextFile in = new TextFile(dirName, fileName);
        in.openRead();
        String data = null;
        try
        {
              data = in.readAll();
        }
        catch (Exception ex)
        {
            sb.append(ex);
        }
        in.close();
        //sb.append(data);
        
        if(sb.length() > 0) display(context,sb.toString());

        UpdateProject update = new UpdateProject(blackboard, data);
        if(update.sb.length() > 0)display(context, update.sb.toString());

        return true;
    }


    public void display(PlugInContext context, String text)
    {
        context.getWorkbenchFrame().getOutputFrame().createNewDocument();
        context.getWorkbenchFrame().getOutputFrame().addText(text);
        context.getWorkbenchFrame().getOutputFrame().surface();
    }
}
