package org.jam.openjump.spatialiteplugin;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

public class SpatialiteExtension extends Extension {

	@Override
	public void configure(PlugInContext context) throws Exception {
		new SpatialitePluging().initialize(context);

	}

    public String getName() {return "Spatialite plugin";}
    public String getVersion() {return "0.3 (03/2013)";}

}
