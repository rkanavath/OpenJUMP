package es.unex.sextante.openjump.extensions;

import java.io.File;

import javax.swing.ImageIcon;

import org.apache.log4j.Logger;

import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.toolbox.ToolboxDialog;
import es.unex.sextante.gui.toolbox.ToolboxPanel;
import es.unex.sextante.openjump.core.OpenJUMPOutputFactory;
import es.unex.sextante.openjump.gui.OpenJUMPGUIFactory;
import es.unex.sextante.openjump.gui.OpenJUMPInputFactory;
import es.unex.sextante.openjump.gui.OpenJUMPPostProcessTaskFactory;
import es.unex.sextante.openjump.language.I18NPlug;

public class SextanteToolboxPlugin implements PlugIn {
    protected static ToolboxPanel m_Toolbox;

    public boolean execute(final PlugInContext context) throws Exception {

        SextanteGUI.getInputFactory().createDataObjects();
        final ToolboxDialog toolbox = new ToolboxDialog(
                SextanteGUI.getMainFrame());
        m_Toolbox = toolbox.getToolboxPanel();
        toolbox.pack();
        // toolbox.setModalityType(Dialog.ModalityType.MODELESS);
        toolbox.setVisible(true);
        SextanteGUI.getInputFactory().clearDataObjects();
        m_Toolbox = null;

        // SextanteGUI.getGUIFactory().showToolBoxDialog();

        return true;

    }

    public String getName() {// Giuseppe Aruta - PlugIn Internationalized
                             // 2013_05_25//

        return I18NPlug
                .getI18N("es.unex.sextante.kosmo.extensions.SextanteToolboxPlugin.Sextante-toolbox");

    }

    public void initialize(PlugInContext context) throws Exception {
        // [Giuseppe Aruta] This part activates GRASS/SAGA/R panels on Setting
        // Frame
        // Those part of Sextante has not been implemented in OpenJUMP so it is
        // deactivated for now
        //
        //
        // HashMap<String, String> map = new HashMap();
        // map.put("isFirstTimeUsingSextante" + Sextante.getVersionNumber(),
        // Boolean.FALSE.toString());
        // SextanteGUI.addAlgorithmProvider(new GrassAlgorithmProvider());
        // map.put("GrassActivate", Boolean.TRUE.toString());
        // SextanteGUI.addAlgorithmProvider(new SagaAlgorithmProvider());
        // map.put("SagaActivate", Boolean.TRUE.toString());
        // SextanteGUI.addAlgorithmProvider(new RAlgorithmProvider());
        // map.put("RActivate", Boolean.TRUE.toString());
        // SextanteGUI.setCustomDefaultSettings(map);
        Sextante.initialize(getJarsFolder());
        SextanteGUI.setSextantePath(getJarsFolder());
        SextanteGUI.initialize(getJarsFolder());
        SextanteGUI.setMainFrame(context.getWorkbenchFrame());
        SextanteGUI.setOutputFactory(new OpenJUMPOutputFactory(context
                .getWorkbenchContext()));
        SextanteGUI.setGUIFactory(new OpenJUMPGUIFactory());
        SextanteGUI.setInputFactory(new OpenJUMPInputFactory(context
                .getWorkbenchContext()));
        SextanteGUI
                .setPostProcessTaskFactory(new OpenJUMPPostProcessTaskFactory());

        LOGGER.info(I18NPlug
                .getI18N("es.unex.sextante.kosmo.extensions.SextanteToolboxPlugin.Help-files-placed-on")
                + ": " + SextanteGUI.getSextantePath() + "/help");

        context.getFeatureInstaller().addMainMenuPlugin(this,
                new String[] { "Sextante" }, getName(), false, getIcon(), null);

    }

    private String getJarsFolder() {

        final String sPath = System.getProperty("user.dir")
                .concat(File.separator).concat("lib").concat(File.separator)
                .concat("ext").concat(File.separator).concat("sextante");

        LOGGER.info("Sextante jar folder: " + sPath);

        return sPath;

    }

    private static final Logger LOGGER = Logger
            .getLogger(SextanteToolboxPlugin.class);

    public ImageIcon getIcon() {

        return new ImageIcon(SextanteGUI.class.getClassLoader().getResource(
                "images/module2.png"));

    }

}
