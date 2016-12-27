package es.unex.sextante.openjump.extensions;

import javax.swing.ImageIcon;

import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.openjump.language.I18NPlug;

public class SextanteToolboxPlugin implements PlugIn {

  public boolean execute(final PlugInContext context) throws Exception {

    SextanteGUI.getGUIFactory().showToolBoxDialog();

    return true;

  }

  public String getName() {

    return I18NPlug
        .getI18N("es.unex.sextante.kosmo.extensions.SextanteToolboxPlugin.Sextante-toolbox");

  }

  public void initialize(final PlugInContext context) throws Exception {

    context.getFeatureInstaller().addMainMenuPlugin(this,
        new String[] { "Sextante" }, getName(), false, getIcon(), null);

  }

  public ImageIcon getIcon() {

    return new ImageIcon(SextanteGUI.class.getClassLoader().getResource(
        "images/module2.png"));

  }

}
