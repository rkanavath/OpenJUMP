package es.unex.sextante.openjump.extensions;

import java.io.File;
import java.net.JarURLConnection;
import java.net.URL;

import com.vividsolutions.jump.workbench.Logger;
import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.plugin.FeatureInstaller;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.openjump.core.OpenJUMPOutputFactory;
import es.unex.sextante.openjump.gui.OpenJUMPGUIFactory;
import es.unex.sextante.openjump.gui.OpenJUMPInputFactory;
import es.unex.sextante.openjump.gui.OpenJUMPPostProcessTaskFactory;

public class SextanteExtension extends Extension {

  /*
   * Version History
   * 
   * @ Sextante 1.0 2013-04-01 Sextante 1.0 Internationalized 2013-05-25 (Add
   * Language codes from Kosmo Sextante (ca,es,fi,hr,it), add French)
   * 
   * @ Sextante 1.0.1 2016-08-10 Correct bug #480. Added Sextante Data Explorer
   * 
   * @ Sextante 1.0.2 2016-10-31 Added Pick coordinates plugin. Better
   * integration between OpenJUMP and Sextante data model Upgraded documentation
   * with a new help dialog style Fix bugs #410 and #427
   * 
   * @ Sextante 1.0.2b 2016-10-31 Help dialog now shows all documentation
   * related to algorithm. Convert Help Dialog to Detached windows
   * 
   * @ Sextante 1.0 OpenJUMP binding 2016-11-28 Help framework now works also on
   * other plugins and algorithms. Solved bug related on Advanced option panel.
   * Added new icons to Results, Pick coordinates and Layer explorer. Changed
   * number version as Sextante embedded into GvSIGCE is 1.0.0 and it is newer
   * than OpenJUMP one (2009?)
   */

  private static final String NAME = "Sextante 1.0";
  private static final String VERSION = "OpenJUMP binding 2016-12-27";

  public String getName() {
    return NAME;
  }

  public String getVersion() {
    return VERSION;
  }

  public void configure(PlugInContext context) throws Exception {

    Logger.debug(">> starting Sextante initialization");

    URL sextGUIJarUrl = SextanteGUI.class.getProtectionDomain().getCodeSource()
        .getLocation();
    Logger.debug("SextanteGUI claims to be in " + sextGUIJarUrl);

    if (sextGUIJarUrl == null) {
      Logger.error("Location of SextanteGUI is null.");
      return;
    }

    // strip possible "jar:<file:\path\to\sextante_gui.jar>!.." wrapping
    if (sextGUIJarUrl.getProtocol().equalsIgnoreCase("jar")){
      JarURLConnection connection =
          (JarURLConnection) sextGUIJarUrl.openConnection();
      sextGUIJarUrl = connection.getJarFileURL();
    }

    File sextGUIJarFile = new File(sextGUIJarUrl.getPath());
    if (!sextGUIJarFile.isFile()) {
      Logger.error(sextGUIJarFile + " is not a file!");
      return;
    }

    File sextDir = sextGUIJarFile.getParentFile();
    if (!sextDir.isDirectory()) {
      Logger.error("Sextante folder " + sextDir + " is not a folder!");
      return;
    }

    Logger.debug("found sextante folder " + sextDir);
    String sextDirString = sextDir.toString();
    // String sextHelpDirString = new File(sextDir, "help").toString();

    Sextante.initialize(sextDirString);
    SextanteGUI.setSextantePath(sextDirString);
    SextanteGUI.initialize(sextDirString);

    SextanteGUI.setGUIFactory(new OpenJUMPGUIFactory());
    SextanteGUI.setMainFrame(context.getWorkbenchFrame());

    SextanteGUI.setInputFactory(new OpenJUMPInputFactory(context
        .getWorkbenchContext()));
    SextanteGUI.setOutputFactory(new OpenJUMPOutputFactory(context
        .getWorkbenchContext()));
    SextanteGUI.setPostProcessTaskFactory(new OpenJUMPPostProcessTaskFactory());

    FeatureInstaller featureInstaller = FeatureInstaller.getInstance();
    new SextanteToolboxPlugin().initialize(context);
    new SextanteModelerPlugin().initialize(context);
    new SextanteHistoryPlugin().initialize(context);
    new SextanteCommandLinePlugin().initialize(context);
    new SextanteResultsPlugin().initialize(context);
    new SextanteDataExplorerPlugin().initialize(context);
    new SextantePickCoordinatesPlugIn().initialize(context);
    featureInstaller.addMenuSeparator(new String[] { "Sextante" });
    new SextanteSettingsPlugin().initialize(context);
    featureInstaller.addMenuSeparator(new String[] { "Sextante" });
    new SextanteHelpPlugIn().initialize(context);

    Logger.debug("<< Successfully finished Sextante initialization.");
  }

}
