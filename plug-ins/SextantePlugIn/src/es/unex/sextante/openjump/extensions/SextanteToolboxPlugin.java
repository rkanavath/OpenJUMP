package es.unex.sextante.openjump.extensions;

import java.io.File;

import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.openjump.core.OpenJUMPOutputFactory;
import es.unex.sextante.openjump.gui.OpenJUMPGUIFactory;
import es.unex.sextante.openjump.gui.OpenJUMPInputFactory;
import es.unex.sextante.openjump.gui.OpenJUMPPostProcessTaskFactory;
import es.unex.sextante.openjump.init.OJSextanteApiInitialiser;

public class SextanteToolboxPlugin
         implements
            PlugIn {

   public boolean execute(final PlugInContext context) throws Exception {

      SextanteGUI.getGUIFactory().showToolBoxDialog();

      return true;

   }


   public String getName() {

      return "Toolbox";

   }


   public void initialize(final PlugInContext context) throws Exception {

      Sextante.initialize(getJarsFolder());
      SextanteGUI.setSextantePath(getHelpPath());
      SextanteGUI.initialize();
      SextanteGUI.setGUIFactory(new OpenJUMPGUIFactory());
      SextanteGUI.setMainFrame(context.getWorkbenchFrame());
      SextanteGUI.setInputFactory(new OpenJUMPInputFactory(context.getWorkbenchContext()));
      SextanteGUI.setOutputFactory(new OpenJUMPOutputFactory(context.getWorkbenchContext()));
      SextanteGUI.setPostProcessTaskFactory(new OpenJUMPPostProcessTaskFactory());
      
      OJSextanteApiInitialiser.isInitialized = true;

      context.getFeatureInstaller().addLayerViewMenuItem(this, new String[] { "Sextante" }, getName());

   }


   private String getJarsFolder() {

      final String sPath = System.getProperty("user.dir") + "/lib/ext/sextante";

      return sPath;

   }


   private String getHelpPath() {

      final String sPath = getSextantePath() + File.separator + "sextante_help";;
      return sPath;

   }


   private String getSextantePath() {

      final String sPath = System.getProperty("user.dir") + File.separator + "sextante";
      return sPath;

   }

}
