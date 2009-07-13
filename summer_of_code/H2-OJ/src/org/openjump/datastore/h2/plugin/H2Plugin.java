package org.openjump.datastore.h2.plugin;

import org.openjump.datastore.h2.H2DataStoreDriver;

import com.vividsolutions.jump.datastore.DataStoreDriver;
import com.vividsolutions.jump.datastore.postgis.PostgisDataStoreDriver;
import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 * This plugin is a driver for a data source backed by an PostGIS database.
 */
public class H2Plugin implements PlugIn {
  //debugging flag
  public static boolean DEBUG = true;
  public static PlugInContext plgInContext;
  
  /**
   * Initializes the plugin by creating the data source and data source 
   * query choosers.
   * @see PlugIn#initialize(com.vividsolutions.jump.workbench.plugin.PlugInContext)
   */
  public void initialize(PlugInContext context) {

	  System.out.println("H2Plugin-init");
	  

  }

  /**
   * This function does nothing, all the setup is completed in initialize().
   */
  public boolean execute(PlugInContext context) { return(false); }
  
  /**
   * @see PlugIn#getName()
   */
  public String getName() { return("H2 Driver" ); }
}


/*
public void initialize(PlugInContext context) throws Exception {
    super.initialize(context);
    
    context.getWorkbenchContext().getRegistry().createEntry(
            DataStoreDriver.REGISTRY_CLASSIFICATION,
            new SISDBDataStoreDriver());
}
*/