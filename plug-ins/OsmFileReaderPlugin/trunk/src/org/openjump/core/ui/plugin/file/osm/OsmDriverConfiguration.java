/*
 * Class allowing read capabilities for OpenStreetMap osm format
 * Copyright (C) 2013 Stefan Steiniger
 * sstein@geo.uzh.ch
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

package org.openjump.core.ui.plugin.file.osm;

import java.util.ArrayList;
import java.util.List;

import org.openjump.core.ui.io.file.DataSourceFileLayerLoader;
import org.openjump.core.ui.io.file.FileLayerLoader;

import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;


/**
 * Extension loading a driver for OpenStreetMap xml - osm - files
 * @author Stefan Steiniger
 * @version 1.0.4 (2013-Aug-14)
 */

public class OsmDriverConfiguration extends Extension {

    public String getName() {
        return "OSM Driver";
    }

    public String getVersion() {
        return "1.0.4 (2013-08-13)";
    }

    public void configure(PlugInContext context) throws Exception {

        final WorkbenchContext wcontext = context.getWorkbenchContext();

        ////////////////////////////////////////////////////////////////////////
        // Create File Loader for OSM data
        ////////////////////////////////////////////////////////////////////////
        List<String> osmExtensions = new ArrayList<String>();
        osmExtensions.add("osm");

        DataSourceFileLayerLoader osmFileLoader = new DataSourceFileLayerLoader(
            wcontext, OsmDataSource.class, "osm (OpenStreetMap XML)", osmExtensions);
       
        wcontext.getRegistry().createEntry(FileLayerLoader.KEY, osmFileLoader);
        
    }

}