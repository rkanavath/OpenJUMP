package es.unex.sextante.openjump.extensions;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

public class SextanteCommandLineExtension extends Extension {

	public void configure(PlugInContext context) throws Exception {

		new SextanteCommandLinePlugin().initialize(context);

	}

}
