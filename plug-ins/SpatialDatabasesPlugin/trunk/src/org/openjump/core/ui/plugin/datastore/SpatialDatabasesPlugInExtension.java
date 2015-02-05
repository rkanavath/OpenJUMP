/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.openjump.core.ui.plugin.datastore;



import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
/**
 * @description
 *  - this class loads the PlugIn into Jump <p>
 *  - class has to be called "Extension" on the end of classname
 *    to use the PlugIn in Jump
 * 
 *  @author sstein 
 */
public class SpatialDatabasesPlugInExtension extends Extension{

	/**
	 * calls PlugIn using class method xplugin.initialize() 
	 */
	public void configure(PlugInContext context) throws Exception{
		new SpatialDatabasesPlugIn().initialize(context);
	}
	
}