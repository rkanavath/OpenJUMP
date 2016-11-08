package es.unex.sextante.openjump.extensions;

import javax.swing.ImageIcon;

import org.openjump.core.ui.plugin.sextante.dialog.SextanteHelpDialog;

import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.images.IconLoader;

import es.unex.sextante.openjump.language.I18NPlug;

public class SextanteHelpPlugIn implements PlugIn {
    public static final ImageIcon ICON = IconLoader
            .icon("information_16x16.png");

    public void run(TaskMonitor monitor, PlugInContext context)
            throws Exception {
    }

    public void initialize(PlugInContext context) throws Exception {
        context.getFeatureInstaller().addMainMenuPlugin(this,
                new String[] { "Sextante" }, getName(), false, ICON, null);
    }

    public String getName() {// Giuseppe Aruta - PlugIn Internationalized
        // 2013_05_25//

        return I18NPlug
                .getI18N("es.unex.sextante.kosmo.extensions.SextanteHelpPlugin.help");

    }

    public boolean execute(PlugInContext context) throws Exception {

        SextanteHelpDialog.createAndShowGUI(context);

        return true;
    }

}
