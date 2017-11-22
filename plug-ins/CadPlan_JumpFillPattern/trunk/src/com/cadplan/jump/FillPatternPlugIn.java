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
import com.vividsolutions.jump.workbench.plugin.EnableCheck;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.WorkbenchToolBar;
import com.vividsolutions.jump.workbench.ui.renderer.style.FillPatternFactory;
import com.vividsolutions.jump.workbench.ui.renderer.style.WKTFillPattern;
import com.cadplan.fileio.IconLoader;


import javax.swing.*;
import java.awt.*;
import java.util.Collection;
import java.util.ArrayList;


/**
 * User: geoff
 * Date: 28/04/2007
 * Time: 09:40:22
 * Copyright 2007 Geoffrey G Roy.
 */
public class FillPatternPlugIn extends AbstractPlugIn
{
    private String version = "0.2";

    public void initialize(PlugInContext context) throws Exception
    {

        context.getFeatureInstaller().addMainMenuItem(this, new String[] {MenuNames.PLUGINS},
                     "Fill Patterns", false, null, null);
         LoadFillPatterns loader = new LoadFillPatterns(context);

    }

    public boolean execute(PlugInContext context) throws Exception
    {
        JOptionPane.showMessageDialog(null,"JumpFillPattern Plugin\n(c)2007 Geoffrey G. Roy\n"+
                "http://www.cadplan.com.au\nVers: "+version,"About...", JOptionPane.INFORMATION_MESSAGE);
        return true;
    }
}
