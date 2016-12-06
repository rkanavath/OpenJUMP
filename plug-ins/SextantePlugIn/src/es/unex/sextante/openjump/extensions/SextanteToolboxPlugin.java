package es.unex.sextante.openjump.extensions;

import java.io.File;

import javax.swing.ImageIcon;
import javax.swing.JFrame;

import org.apache.log4j.Logger;

import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.toolbox.ToolboxPanel;
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

    protected static ToolboxPanel m_Toolbox;

    public String getName() {// Giuseppe Aruta - PlugIn Internationalized
                             // 2013_05_25//

        return I18NPlug
                .getI18N("es.unex.sextante.kosmo.extensions.SextanteToolboxPlugin.Sextante-toolbox");

    }

    public void initialize(final PlugInContext context) throws Exception {

        // System.out.println("starting Sextante initialization >>");
        LOGGER.debug("starting Sextante initialization >>");
        String jarspath = getJarsFolder();
        try {

            // [Giuseppe Aruta 6/12/2016] - Begin of First modification

            // --- comment out lines
            // Sextante.initialize(jarspath);
            // Sextante.initialize();
            // SextanteGUI.setSextantePath(getHelpPath());
            // SextanteGUI.initialize();
            // --- end of comment out lines

            // ---- New lines-----
            Sextante.initialize(jarspath);
            SextanteGUI.setSextantePath(jarspath);
            SextanteGUI.initialize(jarspath);
            // ---- End of new lines-----

            // [Giuseppe Aruta 6/12/2016] - End of first modification

            SextanteGUI.setGUIFactory(new OpenJUMPGUIFactory());
            JFrame frame = context.getWorkbenchFrame();
            SextanteGUI.setMainFrame(frame);

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
            // System.out.println("Sextante not initialized!");
            // System.out
            // .println("Check for problems with Sextante initialization, path for libs and resources: "
            // + jarspath);
            // System.out.println("Check if image 'terminal.png' exist");
            // System.out.println("Check also for Sextante help classes path: "
            // + getHelpPath());
            // System.out.println("=> Will do 2nd try with OJ-IDE-Dev settings: ");
            LOGGER.debug("Error: " + e);
            LOGGER.debug("Sextante not initialized!");
            LOGGER.debug("Check for problems with Sextante initialization, path for libs and resources: "
                    + jarspath);
            LOGGER.debug("Check if image 'terminal.png' exist");
            LOGGER.debug("Check also for Sextante help classes path: "
                    + getHelpPath());
            LOGGER.debug("=> Will do 2nd try with OJ-IDE-Dev settings: ");
            // == do a second trial ==

            String idePathSextante = System.getProperty("user.dir")
                    + "/lib/plus/sextante";

            // [Giuseppe Aruta 6/12/2016] - Second modification

            // --- comment out lines
            // String idePathSHelp = System.getProperty("user.dir")
            // + "/lib/plus/sextante_help";
            // --- end of comment out lines

            // ---- new lines-----
            String idePathSHelp = System.getProperty("user.dir")
                    + "/lib/plus/sextante/help";
            // --- end of new lines-----

            // [Giuseppe Aruta 6/12/2016] - End of Second modification

            SextanteGUI.setSextantePath(idePathSHelp);
            System.out.println("=> looking for algorithms and image in: "
                    + idePathSextante);
            try {

                // [Giuseppe Aruta 6/12/2016] - Third modification

                // --- comment out lines
                // Sextante.initialize(idePathSextante);
                // Sextante.initialize();
                // SextanteGUI.setSextantePath(idePathSHelp);
                // --- end of comment out lines

                // --- new lines-----
                Sextante.initialize(idePathSextante);
                SextanteGUI.setSextantePath(idePathSextante);
                SextanteGUI.initialize(idePathSextante);
                // --- end of new lines-----

                // [Giuseppe Aruta 6/12/2016] - End of third modification

                SextanteGUI.setGUIFactory(new OpenJUMPGUIFactory());
                SextanteGUI.setMainFrame(context.getWorkbenchFrame());
                SextanteGUI.setInputFactory(new OpenJUMPInputFactory(context
                        .getWorkbenchContext()));
                SextanteGUI.setOutputFactory(new OpenJUMPOutputFactory(context
                        .getWorkbenchContext()));
                SextanteGUI
                        .setPostProcessTaskFactory(new OpenJUMPPostProcessTaskFactory());

                OJSextanteApiInitialiser.isInitialized = true;

                // System.out
                // .println("Success initializing Sextante at 2nd trial!");
                LOGGER.debug("Success initializing Sextante at 2nd trial!");
                context.getFeatureInstaller().addMainMenuPlugin(this,
                        new String[] { "Sextante" }, getName(), false,
                        getIcon(), null);
            } catch (Exception e1) {// this is most likely thrown while
                                    // Debugging with the eclipse IDE.

                // System.out
                // .println("No success with Sextante initialization - printing error log:");
                LOGGER.debug(
                        "No success with Sextante initialization - printing error log:",
                        e1);
                e1.printStackTrace();
            }
        }
    }

    private String getJarsFolder() {

        // [Giuseppe Aruta 6/12/2016] - fourth modification

        // --- comment out lines
        // final String sPath = System.getProperty("user.dir")
        // + "/lib/ext/sextante";
        // --- end of comment out lines

        // --- new lines-----
        final String sPath = System.getProperty("user.dir")
                .concat(File.separator).concat("lib").concat(File.separator)
                .concat("ext").concat(File.separator).concat("sextante");
        // --- end of new lines-----

        // [Giuseppe Aruta 6/12/2016] - End of third modification

        return sPath;

    }

    private static final Logger LOGGER = Logger
            .getLogger(SextanteToolboxPlugin.class);

    private String getHelpPath() {
        String str = System.getProperty("user.dir") + "/lib/ext/sextante_help";

        System.out.println("Sextante help path folder: " + str);
        return str;
    }

    // [Giuseppe Aruta 6/12/2016] this method is not used
    private String getSextantePath() {
        String str = System.getProperty("user.dir") + File.separator
                + "sextante";
        return str;
    }

    public ImageIcon getIcon() {

        return new ImageIcon(SextanteGUI.class.getClassLoader().getResource(
                "images/module2.png"));

    }

}
