package com.geomaticaeambiente.klemgui.plugin;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 *
 * @author Geomatica
 */
public class KlemExtension extends Extension{

    @Override
    public void configure(PlugInContext context) throws Exception {
        new StartPlugIn().initialize(context);
    }
    
    
}
