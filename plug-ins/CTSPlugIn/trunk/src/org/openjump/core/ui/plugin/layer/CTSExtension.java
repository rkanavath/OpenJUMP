package org.openjump.core.ui.plugin.layer;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 * Created by Michaël on 30/11/14.
 */
// 0.1.3 (2017-01-21) suggesttree moved to OpenJUMP, CTSPlugIn 0.1.3 needs OJ 1.10+
// 0.1.2
// 0.1.1
// 0.1.0 (2014-12-06) initial version
public class CTSExtension extends Extension {

    public String getName() {
        return "CTS Extension (Micha\u00EBl Michaud)";
    }

    public String getVersion() {
        return "0.1.3 (2017-01-21)";
    }

    public void configure(PlugInContext context) throws Exception {

        boolean missing_libraries = false;
        try {
            getClass().getClassLoader().loadClass("org.cts.crs.CoordinateReferenceSystem");
        } catch(ClassNotFoundException cnfe) {
            context.getWorkbenchFrame().warnUser("CTS Extension cannot be initialized : see log windows");
            context.getWorkbenchFrame().log("Missing library : cts-*.jar");
            missing_libraries = true;
        }
        if (missing_libraries) return;

        new CTSPlugIn().initialize(context);
    }

}
