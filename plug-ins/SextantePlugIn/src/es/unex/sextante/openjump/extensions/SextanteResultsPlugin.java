package es.unex.sextante.openjump.extensions;

import java.util.ArrayList;

import javax.swing.ImageIcon;

import org.openjump.core.ui.plugin.AbstractUiPlugIn;

import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import es.unex.sextante.gui.additionalResults.AdditionalResults;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.openjump.language.I18NPlug;

public class SextanteResultsPlugin
         extends
            AbstractUiPlugIn {

   @Override
   public boolean execute(final PlugInContext context) throws Exception {

      final ArrayList results = AdditionalResults.getComponents();
      if (results.size() != 0) {
         SextanteGUI.getGUIFactory().showAdditionalResultsDialog(results);
      }

      return true;

   }


   @Override
   public String getName() {//Giuseppe Aruta - PlugIn Internationalized 2013_05_25//

     return   I18NPlug.getI18N("es.unex.sextante.kosmo.extensions.SextanteResultsPlugin.Results");

   }


   @Override
   public ImageIcon getIcon() {

      return new ImageIcon(SextanteGUI.class.getClassLoader().getResource("images/grass.png"));

   }


   @Override
   public void initialize(final PlugInContext context) throws Exception {

	   context.getFeatureInstaller().addMainMenuPlugin(this, new String[] { "Sextante" }, getName(), false,  getIcon(), null); 

   }

}
