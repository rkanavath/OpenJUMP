/*
 * Created on 10.Junio.2013
 * 
 */
package ch.unizh.geo.pagevis;

import ch.unizh.geo.pagevis.ojplugin.patterns.CharacterizePolygonNeighborhoodPlugIn;
import ch.unizh.geo.pagevis.ojplugin.patterns.CharacterizePolygonsPlugIn;
import ch.unizh.geo.pagevis.ojplugin.patterns.ExtractCoreEdgeAndPatchPlugIn;
import ch.unizh.geo.pagevis.ojplugin.patterns.ExtractCorridorsPlugIn;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 * @author sstein
 *
 *  - this class loads the PlugIn into Jump 
 *
 *  - class has to be called "Extension" on the end of classname
 *    to use the PlugIn in Jump 
 */
public class LEPatternExtension extends Extension{

	private static final String NAME = "Landscape Pattern Analysis Tools (Stefan Steiniger)";
	private static final String VERSION = "10-June-2013";

	public String getName()
	{
		return NAME ;
	}

	public String getVersion()
	{
		return VERSION ;
	}

	/**
	 * calls PlugIn using class method xplugin.initialize() 
	 */
	public void configure(PlugInContext context) throws Exception{
		
		//should show up under >Plugins>LE-Pattern 
		new CharacterizePolygonsPlugIn().initialize(context);
		new CharacterizePolygonNeighborhoodPlugIn().initialize(context);
		new ExtractCoreEdgeAndPatchPlugIn().initialize(context);
		new ExtractCorridorsPlugIn().initialize(context);
	}
	
}
