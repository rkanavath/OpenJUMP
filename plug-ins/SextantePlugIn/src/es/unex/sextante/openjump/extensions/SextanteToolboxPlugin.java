package es.unex.sextante.openjump.extensions;

import java.io.File;

import javax.swing.ImageIcon;

import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.openjump.core.OpenJUMPOutputFactory;
import es.unex.sextante.openjump.gui.OpenJUMPGUIFactory;
import es.unex.sextante.openjump.gui.OpenJUMPInputFactory;
import es.unex.sextante.openjump.gui.OpenJUMPPostProcessTaskFactory;
import es.unex.sextante.openjump.init.OJSextanteApiInitialiser;
import es.unex.sextante.openjump.language.I18NPlug;

public class SextanteToolboxPlugin implements PlugIn {

    public boolean execute(final PlugInContext context) throws Exception {

        SextanteGUI.getGUIFactory().showToolBoxDialog();

        return true;

    }

    public String getName() {// Giuseppe Aruta - PlugIn Internationalized
                             // 2013_05_25//

        return I18NPlug
                .getI18N("es.unex.sextante.kosmo.extensions.SextanteToolboxPlugin.Sextante-toolbox");

    }

    public void initialize(final PlugInContext context) throws Exception {
        System.out.println("starting Sextante initialization >>");
        String jarspath = getJarsFolder();
        try {
            Sextante.initialize(jarspath); // needs to be there for the
                                           // distribution to load the
                                           // algorithms,
                                           // but is not necessary for debugging
                                           // newly created algorithms started
                                           // from an OJ menu
            Sextante.initialize(); // this will load only from classpath not
                                   // from folder
            SextanteGUI.setSextantePath(getHelpPath());
            SextanteGUI.initialize();
            SextanteGUI.setGUIFactory(new OpenJUMPGUIFactory());
            SextanteGUI.setMainFrame(context.getWorkbenchFrame());
            SextanteGUI.setInputFactory(new OpenJUMPInputFactory(context
                    .getWorkbenchContext()));
            SextanteGUI.setOutputFactory(new OpenJUMPOutputFactory(context
                    .getWorkbenchContext()));
            SextanteGUI
                    .setPostProcessTaskFactory(new OpenJUMPPostProcessTaskFactory());

            OJSextanteApiInitialiser.isInitialized = true;

            context.getFeatureInstaller().addMainMenuPlugin(this,
                    new String[] { "Sextante" }, getName(), false, getIcon(),
                    null);

        } catch (Exception e) {// this is most likely thrown while Debugging
                               // with the eclipse IDE.
            System.out.println("Sextante not initialized!");
            System.out
                    .println("Check for problems with Sextante initialization, path for libs and resources: "
                            + jarspath);
            System.out.println("Check if image 'terminal.png' exist");
            System.out.println("Check also for Sextante help classes path: "
                    + getHelpPath());
            System.out.println("=> Will do 2nd try with OJ-IDE-Dev settings: ");
            // == do a second trial ==
            String idePathSextante = System.getProperty("user.dir")
                    + "/lib/plus/sextante";
            String idePathSHelp = System.getProperty("user.dir")
                    + "/lib/plus/sextante_help";
            System.out.println("=> looking for algorithms and image in: "
                    + idePathSextante);
            try {
                Sextante.initialize(idePathSextante);
                Sextante.initialize(); // this will load only from classpath not
                                       // from folder
                SextanteGUI.setSextantePath(idePathSHelp);
                SextanteGUI.initialize();
                SextanteGUI.setGUIFactory(new OpenJUMPGUIFactory());
                SextanteGUI.setMainFrame(context.getWorkbenchFrame());
                SextanteGUI.setInputFactory(new OpenJUMPInputFactory(context
                        .getWorkbenchContext()));
                SextanteGUI.setOutputFactory(new OpenJUMPOutputFactory(context
                        .getWorkbenchContext()));
                SextanteGUI
                        .setPostProcessTaskFactory(new OpenJUMPPostProcessTaskFactory());

                OJSextanteApiInitialiser.isInitialized = true;
                System.out
                        .println("Success initializing Sextante at 2nd trial!");
                context.getFeatureInstaller().addMainMenuPlugin(this,
                        new String[] { "Sextante" }, getName(), false,
                        getIcon(), null);
            } catch (Exception e1) {// this is most likely thrown while
                                    // Debugging with the eclipse IDE.
                System.out
                        .println("No success with Sextante initialization - printing error log:");
                e1.printStackTrace();
            }
        }
    }

    public String getJarsFolder() {

        final String sPath = System.getProperty("user.dir")
                + "/lib/ext/sextante";
        System.out.println("Sextante jar folder: " + sPath);

        return sPath;

    }

    public String getHelpPath() {

        final String sPath = System.getProperty("user.dir")
                + "/lib/ext/sextante_help";
        // final String sPath = getSextantePath() + File.separator +
        // "sextante_help";
        System.out.println("Sextante help path folder: " + sPath);
        return sPath;

    }

    private String getSextantePath() {

        final String sPath = System.getProperty("user.dir") + File.separator
                + "sextante";
        return sPath;

    }

    public ImageIcon getIcon() {

        return new ImageIcon(SextanteGUI.class.getClassLoader().getResource(
                "images/sextante.gif"));

    }

}
