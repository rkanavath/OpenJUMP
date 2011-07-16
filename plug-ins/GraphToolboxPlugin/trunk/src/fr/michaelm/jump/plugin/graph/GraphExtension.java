/*
 * (C) 2011 Micha&euml;l Michaud
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
 * michael.michaud@free.fr
 *
 */

package fr.michaelm.jump.plugin.graph;

import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Graph Extension contains 3 plugins
 * <ul>
 * <li>GraphNodesPlugIn : computes a graph from a linear network and find nodes</li>
 * <li>GraphComponentsPlugIn : computes a graph from a linear network and find
 * connected subgraphs</li>
 * <li>CycleFinderPlugIn : computes a graph from a linear network and find base cycles</li>
 * </ul>
 * @author Micha&euml;l Michaud
 * @version 0.1.2 (2011-07-16)
 */
//version 0.1.2 (2011-07-16) typos and comments
//version 0.1.1 (2010-04-22) first svn version
//version 0.1 (2010-04-22)
public class GraphExtension extends Extension {

    public String getName() {
        return "Graph Extension (Micha\u00EBl Michaud)";
    }

    public String getVersion() {
        return "0.1.2 (2011-07-16)";
    }

    public void configure(PlugInContext context) throws Exception {
        
        boolean missing_libraries = false;
        try {
            getClass().getClassLoader().loadClass("org.jgrapht.UndirectedGraph");
        } catch(ClassNotFoundException cnfe) {
            context.getWorkbenchFrame().warnUser("Graph Extension cannot be initialized : see log windows");
            context.getWorkbenchFrame().log("Missing library : jgrapht-*.jar");
            missing_libraries = true;
        }
        try {
            getClass().getClassLoader().loadClass("fr.michaelm.jump.feature.jgrapht.INode");
        } catch(ClassNotFoundException cnfe) {
            context.getWorkbenchFrame().warnUser("Graph Extension cannot be initialized : see log windows");
            context.getWorkbenchFrame().log("Missing library : jump-jgrapht-*.jar");
            missing_libraries = true;
        }
        if (missing_libraries) return;
        
        new GraphNodesPlugIn().initialize(context);
        new GraphComponentsPlugIn().initialize(context);
        new CycleFinderPlugIn().initialize(context);
    }

}