/* 
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
package net.refractions.postgis;

import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.Extension;

/**
 * This class installs the PostGIS plugin.
 * @version 1.5.2 (2012-05-12)
 * @author Refractions Research (initial version : chodgson, pramsey, bowens)<br/>
 *         Uwe Dalluege (1.3 version)<br/>
 *         Eric Lemesre (internationalization and openwizard integration)<br/>
 *         Micha&euml;l Michaud (merged Uwe and Eric changes for 1.4.2)<br/>
 */
public class PostGISConfiguration extends Extension {
    public void configure(PlugInContext context) {
        new PostGISPlugIn().initialize(context);    
    }
  
    public String getName() {
        return("PostGIS Driver (Refractions Research/HCU Hamburg, RZCN/E. Lemesre)");
    }
  
    public String getVersion() {
        return("1.5.2 (2012-05-12)");
    }
}