package es.unex.sextante.openjump.extensions;

import org.openjump.sigle.plugin.tutorial.HelpPlugIn;

import sun.security.x509.Extension;

import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.plugin.FeatureInstaller;

public class SextanteExtension extends Extension {

    /*
     * Version History
     * 
     * Sextante 1.0 2013-04-01 Sextante 1.0 Internationalized 2013-05-25 (Add
     * Language codes from Kosmo Sextante (ca,es,fi,hr,it), add French)
     */

    private static final String NAME = "Sextante 1.0 Internationalized (http://www.sextantegis.com/)";
    private static final String VERSION = "(2013-05-25)";// Giuseppe Aruta -
                                                         // PlugIn
                                                         // Internationalized
                                                         // 2013_05_25//

    public String getName() {
        return NAME;
    }

    public String getVersion() {
        return VERSION;
    }

    public void configure(PlugInContext context) throws Exception {
        WorkbenchContext workbenchContext = context.getWorkbenchContext();
        FeatureInstaller featureInstaller = new FeatureInstaller(
                workbenchContext);

        new SextanteToolboxPlugin().initialize(context);
        new SextanteResultsPlugin().initialize(context);
        new SextanteHistoryPlugin().initialize(context);
        new SextanteModelerPlugin().initialize(context);
        new SextanteCommandLinePlugin().initialize(context);
        new SextanteDataExplorerPlugin().initialize(context);
        new SextantePickCoordinatesPlugIn().initialize(context);
        featureInstaller.addMenuSeparator(new String[] { "Sextante" });
        new SextanteSettingsPlugin().initialize(context);
        featureInstaller.addMenuSeparator(new String[] { "Sextante" });
        new HelpPlugIn().initialize(context);

    }

}
