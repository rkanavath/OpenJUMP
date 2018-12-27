package com.geomaticaeambiente.klemgui.plugin;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 *
 * @author Geomatica
 */
public class KlemExtension extends Extension{

    
    private static final String NAME = "OpenKLEM - raster analisys (Geomatica)";
    private static final String VERSION = "1.0 (2018-12-27)";

    public String getName() {
        return NAME;
    }

    public String getVersion() {
        return VERSION;
    }
    
    
    @Override
    public void configure(PlugInContext context) throws Exception {
        new StartPlugIn().initialize(context);
    }
    
    
}
