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
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.EnableCheck;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.WorkbenchToolBar;
import com.cadplan.fileio.IconLoader;


import javax.swing.*;
import java.awt.*;


/**
 * User: geoff
 * Date: 28/04/2007
 * Time: 09:40:22
 * Copyright 2007 Geoffrey G Roy.
 */
public class VertexSymbolsPlugIn extends AbstractPlugIn
{

    private I18NPlug iPlug;
    
    public void initialize(PlugInContext context) throws Exception
    {
        iPlug = new I18NPlug("VertexSymbols","language.VertexSymbolsPlugin");
        EnableCheckFactory check = new EnableCheckFactory(context.getWorkbenchContext());
        MultiEnableCheck mcheck = new MultiEnableCheck();
        mcheck.add(check.createAtLeastNLayersMustExistCheck(1));
        mcheck.add(check.createAtLeastNLayersMustBeEditableCheck(1)); 
        
        
        String menuName = MenuNames.PLUGINS; //iPlug.get("VertexSymbols.MenuName");
        String menuItem = iPlug.get("VertexSymbols.MenuItem");
        context.getFeatureInstaller().addMainMenuItem(this, new String[] {menuName},
                     menuItem, false, null, mcheck);

        String dirName = context.getWorkbenchContext().getWorkbench().getPlugInManager().getPlugInDirectory().getAbsolutePath();
        //IconLoader loader = new IconLoader(dirName,"VertexSymbols");
        //Image image = loader.loadImage("vsicon.gif");
        //ImageIcon icon = new ImageIcon(image);
       // System.out.println("Symbols Resource path: "+this.getClass().getResource("/Resources/vsicon.gif"));
        ImageIcon icon = new ImageIcon(this.getClass().getResource("/Resources/vsicon.gif"));

        WorkbenchToolBar toolBar = context.getWorkbenchFrame().getToolBar();
        
        
        
        
        
        JButton button = toolBar.addPlugIn(icon,this,mcheck,context.getWorkbenchContext());
        LoadImages imageLoader = new LoadImages(context);
        //System.out.println("Initialize plugin");
        VertexParams.context = context.getWorkbenchContext();

    }

    public boolean execute(PlugInContext context) throws Exception
    {
        VertexSymbols vs = new VertexSymbols(context,iPlug);
        return true;
    }
}
