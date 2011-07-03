/**
 * @(#)CSExtension.java
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

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import de.soldin.jump.ExtClassLoader;
import de.soldin.jump.IExtExtension;

/**
 * Installs {@link de.soldin.jump.cts.CSSetPlugin}.
 * 
 * @see com.vividsolutions.jump.workbench.plugin.Extension
 */
public class CSExtension implements IExtExtension {
	private static ResourceBundle i18n;

	/**
	 * @see com.vividsolutions.jump.workbench.plugin.Configuration#configure(com.vividsolutions.jump.workbench.plugin.PlugInContext)
	 */
	public void configure(PlugInContext context) throws Exception {
		/*System.out.println(this.getClass().getName() + " cl is: "
				+ this.getClass().getClassLoader());*/
		CSSetPlugin csp = new CSSetPlugin();
		csp.initialize(context);
	}

	public static String getI18N(String key) {
		// System.out.println(i18n +" getI18N: "+key);
		if (!(i18n instanceof ResourceBundle))
			i18n = ResourceBundle.getBundle("language/cts");

		try {
			return i18n.getString(key);
		} catch (MissingResourceException ex) {
			String[] labelpath = key.split("/\\.");
			ex.printStackTrace();
			return labelpath[(labelpath.length - 1)];
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		return "";
	}

	public static String getLibFolder() {
		return ExtClassLoader.getLibFolder(CSExtension.class, "cts");
	}
}
