package es.unex.sextante.openjump.extensions;

import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import es.unex.sextante.gui.core.SextanteGUI;

public class SextanteCommandLinePlugin
         implements
            PlugIn {

   public boolean execute(final PlugInContext context) throws Exception {

      SextanteGUI.getGUIFactory().showCommandLineDialog();

      return true;

   }


   public String getName() {

      return "Command line";

   }


   public void initialize(final PlugInContext context) throws Exception {

      context.getFeatureInstaller().addLayerViewMenuItem(this, new String[] { "Sextante" }, getName());

   }

}
