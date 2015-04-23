/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI 
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * JUMP is Copyright (C) 2003 Vivid Solutions
 *
 * This program implements extensions to JUMP and is
 * Copyright (C) 2010 Jorge Almaraz.
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
 * For more information, contact:
 *
 * Jukka Rahkonen
 * jukka.rahkonen@latuviitta.fi
 * 
 */
package org.jam.openjump.spatialiteplugin;

import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MenuNames;

import java.io.PrintStream;


public class SpatialitePluging extends AbstractPlugIn {
	static private PlugInContext pc=null;

	public void initialize(PlugInContext context) throws Exception {

		    context.getFeatureInstaller().addMainMenuItem(this,
		        new String[]
				{MenuNames.LAYER },
		        "Import Spatialite Layer ...",
				false, 
				null, 
				null);
	}
	
	public static MultiEnableCheck createEnableCheck(WorkbenchContext workbenchContext) {
	        EnableCheckFactory checkFactory = new EnableCheckFactory(workbenchContext);
	        
	        return new MultiEnableCheck()
	        .add(checkFactory.createWindowWithLayerNamePanelMustBeActiveCheck());
	}
	
	public boolean execute(PlugInContext context) throws Exception{
		SpatialitePluging.pc = context;
		context.getWorkbenchFrame().getOutputFrame().createNewDocument();
		try {
            //Class.forName("org.sqlite.Driver");
            Class.forName("org.sqlite.JDBC");
		} catch (ClassNotFoundException e1) {
			//System.out.println("Error: no org.sqlite.Driver" );
            System.out.println("Error: no org.sqlite.JDBC" );
			return false;
		}
		if (pc!=null){
			PrintStream ps = new PrintStream(new PrintLog(pc));
			System.setOut(ps);
			System.setErr(ps);
		}
		System.out.println("Executing SpatialitePlugin");
		try{
			SpatialiteDialog sd = new SpatialiteDialog(context);
			GUIUtil.centreOnWindow(sd);
			sd.setVisible(true);
			System.out.println("SpatialitePlugin opened");
		} finally{
			System.setOut(System.out);
			System.setErr(System.err);
		}

		return true;
	}
}
