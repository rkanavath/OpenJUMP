package org.openjump.ext.setattributes;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 * Extension initializing the SetAttributes plugin, a plugin displaying
 * buttons to "fill" a set of consistent attribute values for selected
 * features
 */
public class SetAttributesExtension extends Extension {

    public String getName() {
        return "Set Attribute Values Extension (Micha\u00EBl Michaud)";
    }

    public String getVersion() {
        return "0.3 (2015-05-05)";
    }

    public void configure(PlugInContext context) throws Exception {
        new SetAttributesPlugIn().initialize(context);
    }

}