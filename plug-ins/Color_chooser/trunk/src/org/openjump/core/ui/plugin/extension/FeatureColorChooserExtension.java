package org.openjump.core.ui.plugin.extension;

import org.openjump.core.ui.plugin.colorchooser.FeatureColorChooserPlugIn;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

public class FeatureColorChooserExtension extends Extension {

    private static final String NAME = "Feature Color Chooser PlugIn (Giuseppe Aruta - adapted from SkyJUMP sourceforge.net/projects/skyjump/)";
    private static final String VERSION = "1.1 (2018-1-28)";

    public String getName() {
        return NAME;
    }

    public String getVersion() {
        return VERSION;
    }

    public void configure(PlugInContext context) throws Exception {

        new FeatureColorChooserPlugIn().initialize(context);

    }
}