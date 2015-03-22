/*
 * Library name : kml
 * (C) 2011 Larry Becker (ISA)
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
 */

package com.isa.jump.plugin;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import org.openjump.core.ui.io.file.DataSourceFileLayerLoader;

import java.net.URL;
import java.net.URLClassLoader;

/**
 * This is the entry class to declare the kml driver to JUMP.
 * Put the .jar file containing the driver in your installation ext directory.
 * @author Micha&euml;l Michaud
 * @version 0.2.3
 */
// History
// 0.2.3 (2015-03-22) fix a regression introduced in 4215 with a change in core
//                    OpenJUMP (making xml based drivers charset aware)
// 0.2.2 (2014-12-21) make kml parser charset aware
// 0.2.1 (2014-12-20) fix encoding problem (cf #383)
// 0.2   (2014) version included in OpenJUMP 1.7 and 1.8 PLUS
// 0.1   (2011-09-17)   : first version taken from SkyJUMP source
public class KMLDriverConfiguration extends Extension {

    public void configure(PlugInContext context) throws Exception {
        new KMLDataSourceQueryChooserInstallerPlugIn().initialize(context);
    }
    public String getName() {return "KML driver";}
    public String getVersion() {return "0.2.3 (2015-03-22)";}
}
