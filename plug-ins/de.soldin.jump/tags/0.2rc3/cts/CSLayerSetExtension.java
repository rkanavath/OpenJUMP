/**
 * @(#)CSLayersetExtension.java	29.06.2004
 *
 * Copyright 2004 Edgar Soldin
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package de.soldin.jump.cts;

import java.net.URLClassLoader;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import de.soldin.jump.ExtClassLoader;

/**
 * Installs {@link de.soldin.jump.cts.CSSetPlugin}.
 * @see com.vividsolutions.jump.workbench.plugin.Extension
 */
public class CSLayerSetExtension
	extends Extension 
	{
	private static final String NAME = "CTS (Coordinate Transformation Services) Extension";
	private static final String VERSION = "0.2rc3";
	/**
	 * @see com.vividsolutions.jump.workbench.plugin.Configuration#configure(com.vividsolutions.jump.workbench.plugin.PlugInContext)
	 */
	public void configure(PlugInContext context) throws Exception {
		URLClassLoader ecl = createExtClassLoader();
		
		Class clazz = ecl.loadClass("de.soldin.jump.cts.CSSetPlugin");
		PlugIn cts = (PlugIn) clazz.newInstance();
		cts.initialize(context);
	}

	/**
	 * @see com.vividsolutions.jump.workbench.plugin.Extension#getVersion()
	 */
	public String getVersion() {
		return VERSION;
	}

	private URLClassLoader createExtClassLoader() throws Exception{
		
		ExtClassLoader cl = new ExtClassLoader( this.getClass().getClassLoader(), false );
		
		String base = ExtClassLoader.getBase( this.getClass() );
		// add gps.jar
		cl.add( base );
		System.out.println(getName()+" base is: "+base);
		// add gps/
		String libFolder = ExtClassLoader.getLibFolder( this.getClass(), "cts" );
		cl.add( libFolder );
		System.out.println(getName()+" libs are in: "+libFolder);
		// add cts/*.jar
		cl.addAllFiles( libFolder );
		
		return cl;
	}

	public static String getLibFolder() {
		return ExtClassLoader.getLibFolder( CSLayerSetExtension.class, "cts" );
	}
	
}
