package es.unex.sextante.openjump.extensions;

import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.history.History;

public class SextanteHistoryPlugin
         implements
            PlugIn {

   public boolean execute(final PlugInContext context) throws Exception {

      SextanteGUI.getGUIFactory().showHistoryDialog();

      return true;

   }


   public String getName() {

      return "History";

   }


   public void initialize(final PlugInContext context) throws Exception {

      History.startSession();

      context.getFeatureInstaller().addLayerViewMenuItem(this, new String[] { "Sextante" }, getName());


   }

}
