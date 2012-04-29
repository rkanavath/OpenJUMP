package es.unex.sextante.openjump.extensions;

import java.util.ArrayList;

import javax.swing.ImageIcon;

import org.openjump.core.ui.plugin.AbstractUiPlugIn;

import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import es.unex.sextante.gui.additionalResults.AdditionalResults;
import es.unex.sextante.gui.core.SextanteGUI;

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
   public String getName() {

      return "Results";

   }


   @Override
   public ImageIcon getIcon() {

      return new ImageIcon(SextanteGUI.class.getClassLoader().getResource("images/grass.png"));

   }


   @Override
   public void initialize(final PlugInContext context) throws Exception {

      context.getFeatureInstaller().addLayerViewMenuItem(this, new String[] { "Sextante" }, getName());

   }

}
