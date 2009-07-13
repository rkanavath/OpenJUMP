package org.openjump.datastore.h2.plugin;


import org.openjump.datastore.h2.H2DataStoreDriver;
import org.openjump.datastore.h2.plugin.i18n.I18NPlug;

import com.vividsolutions.jump.datastore.DataStoreDriver;
import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;



public class H2Extension extends Extension {

	private static String extensionname = "h2extension";



	public void configure(PlugInContext context) throws Exception {
		//I18NPlug.setPlugInRessource(extensionname, "com.openjump.datastore.h2.plugin.i18n.resources.h2extension");

		
	    try {
	        context.getWorkbenchContext().getRegistry().createEntry(
	                DataStoreDriver.REGISTRY_CLASSIFICATION, new H2DataStoreDriver());
	    } catch (Throwable e) {
	        context.getWorkbenchFrame().warnUser("H2DataStoreDriver not loaded");
	        context.getErrorHandler().handleThrowable(e);
	    }
		
		new H2Plugin().initialize(context);
	}

}


/*
public void configure(PlugInContext context) throws Exception {
    try {
        context.getWorkbenchContext().getRegistry().createEntry(
                DataStoreDriver.REGISTRY_CLASSIFICATION, new SISDBDataStoreDriver());
    } catch (Throwable e) {
        context.getWorkbenchFrame().warnUser("SISDBDataStoreDriver not loaded");
        context.getErrorHandler().handleThrowable(e);
    }
*/