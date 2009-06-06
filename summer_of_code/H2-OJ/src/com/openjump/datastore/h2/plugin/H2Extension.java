package com.openjump.datastore.h2.plugin;


import com.openjump.datastore.h2.plugin.i18n.I18NPlug;
import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;



public class H2Extension extends Extension {

	private static String extensionname = "h2extension";



	public void configure(PlugInContext context) throws Exception {
		//I18NPlug.setPlugInRessource(extensionname, "com.openjump.datastore.h2.plugin.i18n.resources.h2extension");
		System.out.println("H2Extension-configure");


		//context.getWorkbenchContext().getRegistry().createEntry(DataStoreDriver.REGISTRY_CLASSIFICATION, new H2DataStoreDriver());
		new H2Plugin().initialize(context);
	}

}




