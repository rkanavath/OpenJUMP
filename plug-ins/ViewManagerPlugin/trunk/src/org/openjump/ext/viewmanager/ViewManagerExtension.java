package org.openjump.ext.viewmanager;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 * Created by UMichael on 13/06/2015.
 */
public class ViewManagerExtension extends Extension {

    public String getName() {
        return "View Manager Extension (Micha\u00EBl Michaud)";
    }

    public String getVersion() {
        return "0.3.0 (2015-12-14)";
    }

    public void configure(PlugInContext context) throws Exception {
        new ViewManagerPlugIn().initialize(context);
    }
}
