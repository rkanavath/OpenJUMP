package es.unex.sextante.openjump.extensions;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;

import com.vividsolutions.jump.workbench.JUMPWorkbench;
import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.WorkbenchFrame;

import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.toolbox.ToolboxFrame;
import es.unex.sextante.openjump.language.I18NPlug;

public class SextanteToolboxPlugin implements PlugIn {
    static WorkbenchFrame wFrame = JUMPWorkbench.getInstance().getFrame();
  public boolean execute(final PlugInContext context) throws Exception {
      //[Giuseppe Aruta 2017-12-12] open as OJ internal frame
      JFrame frame = context.getWorkbenchFrame();
      for (JInternalFrame iFrame : wFrame.getInternalFrames()) {
          if (iFrame instanceof ToolboxFrame) {

              iFrame.toFront();
              return false;

          }
      }
      ToolboxFrame tframe = new ToolboxFrame(frame);
      wFrame.addInternalFrame(tframe, true, true);
      SextanteGUI.getInputFactory().clearDataObjects();

      // SextanteGUI.getGUIFactory().showToolBoxDialog();

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
