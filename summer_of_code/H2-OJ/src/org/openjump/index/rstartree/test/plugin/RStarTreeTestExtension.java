package org.openjump.index.rstartree.test.plugin;


import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

public class RStarTreeTestExtension extends Extension {

	public RStarTreeTestExtension() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void configure(PlugInContext context) throws Exception {
	    new RStarTreeTestPlugin().initialize(context);

	}

}
