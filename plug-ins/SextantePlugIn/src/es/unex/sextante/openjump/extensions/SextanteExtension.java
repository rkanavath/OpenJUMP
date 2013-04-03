package es.unex.sextante.openjump.extensions;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

public class SextanteExtension extends Extension {

	
	  private static final String NAME = "Sextante 1.0 (http://www.sextantegis.com/)";
	    private static final String VERSION = "(2013-04-01)";
	 
	    public String getName()
		  {
		    return NAME ;
		  }

		  public String getVersion()
		  {
		    return VERSION ;
		  }
	
	public void configure(PlugInContext context) throws Exception {

		new SextanteToolboxPlugin().initialize(context);
		new SextanteResultsPlugin().initialize(context);
		new SextanteHistoryPlugin().initialize(context);
		new SextanteModelerPlugin().initialize(context);
		new SextanteCommandLinePlugin().initialize(context);

	}

}
