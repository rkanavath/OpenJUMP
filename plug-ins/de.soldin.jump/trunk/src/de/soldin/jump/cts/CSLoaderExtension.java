/**
 * @(#)CSExtensionLoader.java
 *
 * Copyright 2011 Edgar Soldin
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

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import de.soldin.jump.ExtClassLoader;
import de.soldin.jump.IExtExtension;

public class CSLoaderExtension extends Extension{
	private static final String NAME = "CTS (Coordinate Transformation Services) Extension";
	private static final String VERSION = "0.2rc6";
	private static ExtClassLoader ecl;
	
	/**
	 * @see com.vividsolutions.jump.workbench.plugin.Extension#getName()
	 */
	public String getName() {
		return NAME;
	}
	
	/**
	 * @see com.vividsolutions.jump.workbench.plugin.Extension#getVersion()
	 */
	public String getVersion() {
		return VERSION;
	}

	/**
	 * @see com.vividsolutions.jump.workbench.plugin.Extension#configure()
	 */
	public void configure(PlugInContext context) throws Exception {
		ExtClassLoader ecl = getClassLoader();
		Class clazz = ecl.loadClass("de.soldin.jump.cts.CSExtension");
		IExtExtension csx = (IExtExtension) clazz.newInstance();
		csx.configure(context);		
	}

	public static ExtClassLoader getClassLoader() throws Exception{
		if (ecl instanceof ExtClassLoader)
			return ecl;
		
		Class clazz = CSLoaderExtension.class;
		ecl = new ExtClassLoader( clazz.getClassLoader(), false );
		// keep interfaces in parent loader
		ecl.blacklist("^de.soldin.jump.[^\\.]+$");
		
		String base = ExtClassLoader.getBase( clazz );
		// add gps.jar
		ecl.add( base );
		//System.out.println(clazz.getName()+" base is: "+base);
		// add gps/
		String libFolder = ExtClassLoader.getLibFolder( clazz, "cts" );
		ecl.add( libFolder );
		System.out.println(clazz.getName()+" libs are in: "+libFolder);
		// add cts/*.jar
		ecl.addAllFiles( libFolder );
		
		return ecl;
	}
}
