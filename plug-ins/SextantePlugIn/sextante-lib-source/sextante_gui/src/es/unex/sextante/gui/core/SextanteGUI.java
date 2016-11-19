package es.unex.sextante.gui.core;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JOptionPane;

import es.unex.sextante.core.GeoAlgorithm;
import es.unex.sextante.core.IInputFactory;
import es.unex.sextante.core.OutputFactory;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.cmd.ScriptAlgorithmProvider;
import es.unex.sextante.gui.help.HelpIO;
import es.unex.sextante.gui.history.History;
import es.unex.sextante.gui.modeler.ModelerAlgorithmProvider;
import es.unex.sextante.gui.settings.Setting;
import es.unex.sextante.gui.settings.SextanteGeneralSettings;
import es.unex.sextante.gui.settings.SextanteGrassSettings;
import es.unex.sextante.gui.settings.SextanteModelerSettings;
import es.unex.sextante.gui.settings.SextanteRSettings;
import es.unex.sextante.gui.settings.SextanteSagaSettings;
import es.unex.sextante.gui.settings.SextanteScriptsSettings;

/**
 * This class centralizes most actions related to the SextanteGUI, containing
 * methods to show dialogs and retrieve basic values used by GUI elements
 */
public class SextanteGUI {

    public static final int HISTORY = 0;
    public static final int COMMANDLINE = 1;

    private static IInputFactory m_InputFactory;
    private static OutputFactory m_OutputFactory;
    private static IGUIFactory m_GUIFactory = new DefaultGUIFactory();

    private static IPostProcessTaskFactory m_PostProcessTaskFactory;

    private static JDialog m_LastCommandOriginParentPanel;
    private static int m_iLastCommandOrigin;
    private static Frame m_MainFrame;

    private static boolean m_bShowOnlyActiveAlgorithms = false;
    private static String m_sOutputFolder;
    private static String m_sSextantePath;
    private static HashMap<String, String> m_Settings = new HashMap<String, String>();
    private static HashMap<String, String> m_DefaultSettings = new HashMap<String, String>();
    private static IDataRenderer m_Renderer;

    private final static HashMap<String, Class> m_ParametersPanel = new HashMap<String, Class>();
    private final static HashMap<String, Class> m_ModelerParametersPanel = new HashMap<String, Class>();

    private final static ArrayList<IAlgorithmProvider> m_AlgorithmProviders = new ArrayList<IAlgorithmProvider>();

    public final static ImageIcon SEXTANTE_ICON = new ImageIcon(
            SextanteGUI.class.getClassLoader()
                    .getResource("images/module2.png"));

    public final static Color COLOR_WHITE = new Color(255, 255, 255, 255);
    public final static Color COLOR_GREY = new Color(128, 128, 128, 255);
    public final static Color COLOR_BLACK = new Color(0, 0, 0, 255);
    public final static Color COLOR_RED = new Color(255, 51, 51, 255);
    public final static Color COLOR_ORANGE = new Color(255, 103, 51, 255);
    public final static Color COLOR_YELLOW = new Color(255, 255, 51, 255);
    public final static Color COLOR_GREEN_LIGHT = new Color(51, 255, 51, 255);
    public final static Color COLOR_GREEN_DARK = new Color(0, 184, 0, 255);
    public final static Color COLOR_BLUE_LIGHT = new Color(102, 179, 255, 255);
    public final static Color COLOR_BLUE_DARK = new Color(0, 102, 204, 255);
    public final static Color COLOR_PINK = new Color(255, 153, 255, 255);
    public final static Color COLOR_PURPLE = new Color(163, 71, 255, 255);
    public final static Color COLOR_BROWN_LIGHT = new Color(255, 168, 82, 255);
    public final static Color COLOR_BROWN_DARK = new Color(204, 102, 0, 255);

    static {

        m_AlgorithmProviders.add(new ScriptAlgorithmProvider());
        m_AlgorithmProviders.add(new ModelerAlgorithmProvider());

    }

    /**
     * Sets a new main frame. This will be used as the parent frame by SEXTANTE
     * GUI elements
     * 
     * @param frame
     *            The main frame
     */
    public static void setMainFrame(final Frame frame) {

        m_MainFrame = frame;

    }

    /**
     * Returns the current main frame
     * 
     * @return the current main frame
     */
    public static Frame getMainFrame() {

        return m_MainFrame;

    }

    /** ************************************************************ */

    /**
     * Returns the current input factory
     * 
     * @see IInputFactory.
     * @return the current input factory
     */
    public static IInputFactory getInputFactory() {

        return m_InputFactory;

    }

    /**
     * Sets a new input factory as the current one
     * 
     * @param inputFactory
     *            the new input factory
     */
    public static void setInputFactory(final IInputFactory inputFactory) {

        m_InputFactory = inputFactory;

    }

    /**
     * Returns the current OutputFactory
     * 
     * @return the current OutputFactory
     */
    public static OutputFactory getOutputFactory() {

        return m_OutputFactory;

    }

    /**
     * sets a new output factory
     * 
     * @param outputFactory
     *            the new output factory
     */
    public static void setOutputFactory(final OutputFactory outputFactory) {

        m_OutputFactory = outputFactory;
        m_OutputFactory.setDefaultNoDataValue(getDefaultNoDataValue());

    }

    /**
     * Returns the current GUIFactory
     * 
     * @return the current GUIFactory
     */
    public static IGUIFactory getGUIFactory() {

        return m_GUIFactory;

    }

    /**
     * sets a new GUI factory
     * 
     * @param guiFactory
     *            the new GUI factory
     */
    public static void setGUIFactory(final IGUIFactory guiFactory) {

        m_GUIFactory = guiFactory;

    }

    /**
     * Returns the task to post-process the algorithm outputs, usually to add
     * them to the GUI of the GIS app.
     * 
     * @param alg
     *            the algorithm to postprocess. Since this task will mainly deal
     *            with output results, the algorithm should have been previously
     *            executed, so it contains non-null output values
     * @param bShowResultsDialog
     *            if this parameter is true, the task will show the results
     *            dialog if the algorithm has produced some kind of output other
     *            than layers or tables. If it is false, those results will be
     *            added to the set of current results, but the results dialog
     *            will not be shown
     * 
     * @return a task to postprocess the given algorithm
     */
    public static Runnable getPostProcessTask(final GeoAlgorithm alg,
            final boolean bShowResultsDialog) {

        return m_PostProcessTaskFactory.getPostProcessTask(alg,
                bShowResultsDialog);

    }

    /**
     * Sets the current post process task factory
     * 
     * @param factory
     *            the new post-process task factory
     */
    public static void setPostProcessTaskFactory(
            final IPostProcessTaskFactory factory) {

        m_PostProcessTaskFactory = factory;

    }

    /** ******************************************* */

    /**
     * Adds an algorithm provider. This has to be done before calling
     * initialize(), so the providers can be initialized as well.
     * 
     * @param provider
     *            the algorithm provider to add
     */
    public static void addAlgorithmProvider(final IAlgorithmProvider provider) {

        // providers are added at the beginning of the array, because models and
        // scripts might depend on them
        m_AlgorithmProviders.add(0, provider);

    }

    /**
     * Checks if a portable directory exists and has the required access
     * privileges. Non-existing directories will be created if possible.
     * Displays a GUI warning message if there are problems, and exits with a
     * corresponding error code.
     * 
     * @param dir
     *            String with name of folder/directory to be checked. Use only a
     *            relative path here.
     * @param read_only
     *            set to "true" if only read access is required
     * @param provider
     *            name of the algorithm provider for which this folder is being
     *            checked.
     * @return "0" if all is OK, "1" if directory does not exist and could not
     *         be created, "2" if access is forbidden, "3" if a _file_ with that
     *         name already exists
     */
    public static int checkDir(String dir, boolean read_only, String provider) {

        final File f = new File(SextanteGUI.getSextantePath() + File.separator
                + dir);

        if (f.exists()) {
            if (f.isFile()) {
                JOptionPane
                        .showMessageDialog(
                                null,
                                Sextante.getText("portable_dir_error")
                                        + " "
                                        + f.getAbsolutePath()
                                        + ".\n"
                                        + Sextante
                                                .getText("portable_dir_is_file")
                                        + "\n"
                                        + Sextante
                                                .getText("portable_provider_not_usable")
                                        + " <html></i>" + provider
                                        + "+</i>+</html>.", "Inane warning",
                                JOptionPane.WARNING_MESSAGE);
                return (3);
            }
            if (read_only) {
                if (f.canRead()) {
                    return (0);
                }
            }
            if (f.canWrite()) {
                return (0);
            }
            if (read_only) {
                JOptionPane
                        .showMessageDialog(
                                null,
                                Sextante.getText("portable_dir_error")
                                        + " "
                                        + f.getAbsolutePath()
                                        + Sextante
                                                .getText("portable_dir_error_ro")
                                        + "\n"
                                        + ":"
                                        + Sextante
                                                .getText("portable_dir_no_access")
                                        + "\n"
                                        + Sextante
                                                .getText("portable_provider_not_usable")
                                        + " <html></i>" + provider
                                        + "+</i>+</html>.", "Inane warning",
                                JOptionPane.WARNING_MESSAGE);
            } else {
                JOptionPane
                        .showMessageDialog(
                                null,
                                Sextante.getText("portable_dir_error")
                                        + " "
                                        + f.getAbsolutePath()
                                        + Sextante
                                                .getText("portable_dir_error_rw")
                                        + "\n"
                                        + ":"
                                        + Sextante
                                                .getText("portable_dir_no_access")
                                        + "\n"
                                        + Sextante
                                                .getText("portable_provider_not_usable")
                                        + " <html></i>" + provider
                                        + "+</i>+</html>.", "Inane warning",
                                JOptionPane.WARNING_MESSAGE);
            }
            return (2);
        }

        /* directory does not exist: attempt to create it */
        if (f.mkdir() == false) {
            JOptionPane.showMessageDialog(
                    null,
                    Sextante.getText("portable_dir_error") + " "
                            + f.getAbsolutePath() + ".\n"
                            + Sextante.getText("portable_dir_no_create") + "\n"
                            + Sextante.getText("portable_provider_not_usable")
                            + " <html></i>" + provider + "+</i>+</html>.",
                    "Inane warning", JOptionPane.WARNING_MESSAGE);
            return (1);
        }

        return (0);
    }

    /**
     * Checks if a file exists in one of the portable folders, and if it has the
     * required access privileges. Displays a GUI warning message if there are
     * problems, and exits with a corresponding error code.
     * 
     * @param file
     *            String with name of file to be checked. Use only a relative
     *            path here.
     * @param read_only
     *            set to "true" if only read access is required
     * @param provider
     *            name of the algorithm provider for which this file is being
     *            checked.
     * @return "0" if all is OK, "1" if file does not exist, "2" if access is
     *         forbidden, "3" if a _directory_ with that name already exists
     */
    public static int checkFile(String file, boolean read_only, String provider) {

        final File f = new File(SextanteGUI.getSextantePath() + File.separator
                + file);

        if (f.exists()) {
            if (f.isDirectory()) {
                JOptionPane
                        .showMessageDialog(
                                null,
                                Sextante.getText("portable_file_error")
                                        + " "
                                        + f.getAbsolutePath()
                                        + ".\n"
                                        + Sextante
                                                .getText("portable_file_is_dir")
                                        + "\n"
                                        + Sextante
                                                .getText("portable_provider_not_usable")
                                        + " <html></i>" + provider
                                        + "+</i>+</html>.", "Inane warning",
                                JOptionPane.WARNING_MESSAGE);
                return (3);
            }
            if (read_only) {
                if (f.canRead()) {
                    return (0);
                }
            }
            if (f.canWrite()) {
                return (0);
            }
            if (read_only) {
                JOptionPane
                        .showMessageDialog(
                                null,
                                Sextante.getText("portable_file_error")
                                        + " "
                                        + f.getAbsolutePath()
                                        + Sextante
                                                .getText("portable_file_error_ro")
                                        + "\n"
                                        + ":"
                                        + Sextante
                                                .getText("portable_file_no_access")
                                        + "\n"
                                        + Sextante
                                                .getText("portable_provider_not_usable")
                                        + " <html></i>" + provider
                                        + "+</i>+</html>.", "Inane warning",
                                JOptionPane.WARNING_MESSAGE);
            } else {
                JOptionPane
                        .showMessageDialog(
                                null,
                                Sextante.getText("portable_file_error")
                                        + " "
                                        + f.getAbsolutePath()
                                        + Sextante
                                                .getText("portable_file_error_rw")
                                        + "\n"
                                        + ":"
                                        + Sextante
                                                .getText("portable_file_no_access")
                                        + "\n"
                                        + Sextante
                                                .getText("portable_provider_not_usable")
                                        + " <html></i>" + provider
                                        + "+</i>+</html>.", "Inane warning",
                                JOptionPane.WARNING_MESSAGE);
            }
            return (2);
        }

        return (1);
    }

    /**
     * Portable SEXTANTE requires that external providers' (e.g. GRASS)
     * algorithms and user-editable scripts are stored in folders within the
     * SEXTANTE extension folder. In some case, read access to these folders is
     * fine, in others r/w is required. This method checks whether an algorithm
     * provider has been set to be portable and if so, it makes sure that the
     * required folder(s) is/are present with the required access rights. It
     * will attempt to create any missing folders and will display a warning if
     * anything goes wrong.
     */
    public static void checkPortableFolders() {

        String sPath;
        int result;

        sPath = new String("");

        if (Boolean.parseBoolean(SextanteGUI
                .getSettingParameterValue(SextanteRSettings.R_PORTABLE)) == true) {
            /* set R binaries folder */
            result = checkDir(Sextante.PORTABLE_R_FOLDER, true, "R");
            sPath = SextanteGUI.getSextantePath() + File.separator
                    + Sextante.PORTABLE_R_FOLDER;
            SextanteGUI.setSettingParameterValue(SextanteRSettings.R_FOLDER,
                    sPath);

            /* set R user scripts folder */
            result = checkDir(Sextante.PORTABLE_R_SCRIPTS_FOLDER, false,
                    "R user scripts");
            sPath = SextanteGUI.getSextantePath() + File.separator
                    + Sextante.PORTABLE_R_SCRIPTS_FOLDER;
            SextanteGUI.setSettingParameterValue(
                    SextanteRSettings.R_SCRIPTS_FOLDER, sPath);
        }
        if (Boolean
                .parseBoolean(SextanteGUI
                        .getSettingParameterValue(SextanteGrassSettings.GRASS_PORTABLE)) == true) {
            /* set GRASS binaries folder */
            result = checkDir(Sextante.PORTABLE_GRASS_FOLDER, true, "GRASS GIS");
            sPath = SextanteGUI.getSextantePath() + File.separator
                    + Sextante.PORTABLE_GRASS_FOLDER;
            SextanteGUI.setSettingParameterValue(
                    SextanteGrassSettings.GRASS_FOLDER, sPath);
            /* set GRASS shell support (MSYS) binaries folder (Windows only) */
            if (Sextante.isWindows()) {
                result = checkFile(Sextante.PORTABLE_MSYS_FILE, true,
                        "GRASS GIS shell scripts");
                sPath = SextanteGUI.getSextantePath() + File.separator
                        + Sextante.PORTABLE_MSYS_FILE;
                SextanteGUI.setSettingParameterValue(
                        SextanteGrassSettings.GRASS_WIN_SHELL, sPath);
            }
        }
        if (Boolean.parseBoolean(SextanteGUI
                .getSettingParameterValue(SextanteSagaSettings.SAGA_PORTABLE)) == true) {
            /* set SAGA binaries folder */
            result = checkDir(Sextante.PORTABLE_SAGA_FOLDER, true, "SAGA GIS");
            sPath = SextanteGUI.getSextantePath() + File.separator
                    + Sextante.PORTABLE_SAGA_FOLDER;
            SextanteGUI.setSettingParameterValue(
                    SextanteSagaSettings.SAGA_FOLDER, sPath);
            /* set SAGA module libs folder */
            result = checkDir(Sextante.PORTABLE_SAGA_MODS_FOLDER, true,
                    "SAGA GIS");
            sPath = SextanteGUI.getSextantePath() + File.separator
                    + Sextante.PORTABLE_SAGA_MODS_FOLDER;
            SextanteGUI.setSettingParameterValue(
                    SextanteSagaSettings.SAGA_MODS_FOLDER, sPath);
        }
        if (Boolean
                .parseBoolean(SextanteGUI
                        .getSettingParameterValue(SextanteScriptsSettings.SCRIPTS_PORTABLE)) == true) {
            /* set user scripts folder */
            result = checkDir(Sextante.PORTABLE_SCRIPTS_FOLDER, true,
                    "SEXTANTE user scripts");
            sPath = SextanteGUI.getSextantePath() + File.separator
                    + Sextante.PORTABLE_SCRIPTS_FOLDER;
            SextanteGUI.setSettingParameterValue(
                    SextanteScriptsSettings.SCRIPTS_FOLDER, sPath);
        }
        if (Boolean
                .parseBoolean(SextanteGUI
                        .getSettingParameterValue(SextanteModelerSettings.MODELS_PORTABLE)) == true) {
            /* set user models folder */
            result = checkDir(Sextante.PORTABLE_MODELS_FOLDER, true,
                    "SEXTANTE user models");
            sPath = SextanteGUI.getSextantePath() + File.separator
                    + Sextante.PORTABLE_MODELS_FOLDER;
            SextanteGUI.setSettingParameterValue(
                    SextanteModelerSettings.MODELS_FOLDER, sPath);
        }
    }

    /**
     * Initializes the GUI parameters and resources. It takes GUI resources
     * (custom panels, etc.) from classpath jars. Should be called before
     * setting parameters using other methods from this class, except for the
     * setSextantePath() and setCustomDefaultSettings() methods.
     */
    public static void initialize() {

        GUIResources.addResourcesFromClasspath();
        _initialize();

    }

    /**
     * Initializes the GUI parameters and resources. It takes GUI resources
     * (custom panels, etc.) from the specified urls. Should be called before
     * setting parameters using other methods from this class, except for the
     * setSextantePath() method.
     * 
     * @param jars
     *            the urls of the jar files with custom GUI resources
     */
    public static void initialize(final URL[] jars) {

        GUIResources.addResourcesFromURLs(jars);
        _initialize();

    }

    /**
     * Initializes the GUI parameters and resources. It takes GUI resources
     * (custom panels, etc.) from the specified folder. Should be called before
     * setting parameters using other methods from this class, except for the
     * setSextantePath() method.
     * 
     * @param sFolder
     *            the folder with jars with custom GUI resources
     */
    public static void initialize(final String sFolder) {

        GUIResources.addResourcesFromFolder(sFolder);
        _initialize();

    }

    private static void _initialize() {

        setDefaultSettings();
        readConfigFile();

        checkPortableFolders();

        // this loads built-in SEXTANTE algorithms and resources
        loadResources();

        // and this loads additional algorithms from algorithm providers
        for (int i = 0; i < m_AlgorithmProviders.size(); i++) {
            final IAlgorithmProvider provider = m_AlgorithmProviders.get(i);
            provider.initialize();
            if (provider.getAlgorithms().size() > 0) {
                Sextante.addAlgorithmsFromProvider(provider.getName(),
                        provider.getAlgorithms());
                addCustomModelerParametersPanel(provider
                        .getCustomModelerParameterPanels());
                addCustomParametersPanel(provider.getCustomParameterPanels());
            }
        }

    }

    private static void setDefaultSettings() {

        final Setting[] baseSettings = new Setting[] { new SextanteGeneralSettings() };
        final ArrayList<IAlgorithmProvider> providers = SextanteGUI
                .getAlgorithmProviders();
        final Setting[] settings = new Setting[baseSettings.length
                + providers.size()];
        System.arraycopy(baseSettings, 0, settings, 0, baseSettings.length);
        for (int i = 0; i < providers.size(); i++) {
            settings[i + baseSettings.length] = providers.get(i).getSettings();
        }

        for (int i = 0; i < settings.length; i++) {
            final HashMap<String, String> map = settings[i].getInitValues();
            m_Settings.putAll(map);
        }

        if (m_DefaultSettings != null) {
            m_Settings.putAll(m_DefaultSettings);
        }

    }

    /**
     * This methods sets default setting values different from the hard-coded
     * ones. It should be called right before initializing this class
     * 
     * @param map
     *            the map with new default settings
     */
    public static void setCustomDefaultSettings(
            final HashMap<String, String> map) {

        m_DefaultSettings = map;

    }

    private static void loadResources() {

        final String[] sPanelClassName = GUIResources
                .getParameterPanelClassNames();
        for (final String element : sPanelClassName) {
            try {
                final Class panelClass = Class.forName(element);
                final String sAlgClassName = element.substring(0,
                        element.indexOf("ParametersPanel"))
                        + "Algorithm";
                final Class algClass = Class.forName(sAlgClassName);
                final GeoAlgorithm alg = (GeoAlgorithm) algClass.newInstance();
                m_ParametersPanel.put(alg.getCommandLineName(), panelClass);
            } catch (final Exception e) {/* ignore it if there are problems */
            }
        }

        final String[] sModelerPanelClassName = GUIResources
                .getModelerParameterPanelClassNames();
        for (final String element : sModelerPanelClassName) {
            try {
                final Class panelClass = Class.forName(element);
                final String sAlgClassName = element.substring(0,
                        element.indexOf("ModelerParametersPanel"))
                        + "Algorithm";
                final Class algClass = Class.forName(sAlgClassName);
                final GeoAlgorithm alg = (GeoAlgorithm) algClass.newInstance();
                m_ModelerParametersPanel.put(alg.getCommandLineName(),
                        panelClass);
            } catch (final Exception e) {/* ignore it if there are problems */
            }
        }

    }

    /**
     * Returns the path to help files
     * 
     * @return the path to help files
     */
    public static String getHelpPath() {

        return m_sSextantePath + File.separator;// + "sextante_help";

    }

    /**
     * Sets the current path to sextante folder. This is the folder where help
     * files and additional software should be located. This should be done
     * before initializing this class, since this folder is used by some
     * algorithm providers
     * 
     * @param sPath
     *            the path to sextante help and additional programs
     */
    public static void setSextantePath(final String sPath) {

        m_sSextantePath = sPath;

    }

    /**
     * Returns the path to sextante help and additional programs
     * 
     * @return the path to sextante help and additional programs
     */
    public static String getSextantePath() {

        return m_sSextantePath;

    }

    /**
     * Returns the default folder for output data.
     * 
     * @return the default folder for output data.
     */
    public static String getOutputFolder() {

        if ((m_sOutputFolder == null) || m_sOutputFolder.trim().equals("")) {
            return System.getProperty("user.home");
        } else {
            return m_sOutputFolder;
        }

    }

    /**
     * Returns the default value to represent "No Data" (null) raster cells.
     * 
     * @return the current "No Data" value.
     */
    public static Double getDefaultNoDataValue() {

        try {
            final double dNoDataValue = Double
                    .parseDouble(SextanteGUI
                            .getSettingParameterValue(SextanteGeneralSettings.DEFAULT_NO_DATA_VALUE));
            return dNoDataValue;
        } catch (final Exception e) {
            return new Double(Sextante.PRESET_NO_DATA);
        }

    }

    /**
     * Loads configuration parameters from the config file
     */
    private static void readConfigFile() {

        BufferedReader input = null;
        try {
            input = new BufferedReader(new FileReader(getConfigFile()));
            String sLine = null;
            while ((sLine = input.readLine()) != null) {
                final String[] sTokens = sLine.split("=");
                if (sTokens.length == 2) {
                    m_Settings.put(sTokens[0], sTokens[1]);
                }
            }
        } catch (final FileNotFoundException e) {
        } catch (final IOException e) {
        } finally {
            try {
                if (input != null) {
                    input.close();
                }
            } catch (final IOException e) {
                Sextante.addErrorToLog(e);
            }
        }

    }

    /**
     * Saves current settings to the config file
     */
    public static void saveSettings() {

        Writer output = null;
        try {
            output = new BufferedWriter(new FileWriter(getConfigFile()));
            final Set<String> set = m_Settings.keySet();
            final Iterator<String> iter = set.iterator();
            while (iter.hasNext()) {
                final String sKey = iter.next();
                final String sValue = m_Settings.get(sKey);
                output.write(sKey + "=" + sValue + "\n");
            }
        } catch (final IOException e) {
            Sextante.addErrorToLog(e);
        } finally {
            if (output != null) {
                try {
                    output.close();
                } catch (final IOException e) {
                    Sextante.addErrorToLog(e);
                }
            }
        }

    }

    /**
     * Returns the path to the SEXTANTE config file.
     * 
     * @return A string with the full the path and name of the SEXTANTE config
     *         file.
     */
    private static String getConfigFile() {

        final String sPath = Sextante.getUserFolder();

        return sPath + File.separator + "sextante.settings";

    }

    /**
     * Returns the type of the last element from which a command-line command
     * was executed
     * 
     * @return the type of the element from which a command-line command was
     *         executed. SextanteGUI.HISTORY if the last component was the
     *         history panel; SextanteGUI.COMMANDLINE if it was the regular
     *         SEXTANTE console
     */
    public static int getLastCommandOrigin() {

        return m_iLastCommandOrigin;

    }

    /**
     * Sets the type of the last element from which a command-line command was
     * executed. This has to be called from any component that allows execution
     * of commands
     * 
     * @param iLast
     *            one of the following constants: SextanteGUI.HISTORY if the
     *            last component was the history panel; SextanteGUI.COMMANDLINE
     *            if it was the regular SEXTANTE console
     */
    public static void setLastCommandOrigin(final int iLast) {

        m_iLastCommandOrigin = iLast;

    }

    /**
     * Gets the dialog from which the last command--line command was executed.
     * This will be used as the parent dialog for task monitors or message
     * dialogs generated by the execution of that command.
     * 
     * @return the dialog from which the last command--line command was executed
     */
    public static JDialog getLastCommandOriginParentDialog() {

        return m_LastCommandOriginParentPanel;

    }

    /**
     * Sets the dialog (if any) that contains the element from which the last
     * command--line command was executed
     * 
     * @param parent
     *            the dialog (if any) that contains the element from which the
     *            last command--line command was executed
     */
    public static void setLastCommandOriginParentDialog(final JDialog parent) {

        m_LastCommandOriginParentPanel = parent;

    }

    /**
     * Returns true if only active algorithms (those that can be executed with
     * the current data objects) should be shown in the toolbox
     * 
     * @return true if only active algorithms (those that can be executed with
     *         the current data objects) should be shown in the toolbox
     */
    public static boolean getShowOnlyActiveAlgorithms() {

        return m_bShowOnlyActiveAlgorithms;

    }

    /**
     * Sets whether only active algorithms (those that can be executed with the
     * current data objects) should be shown in the toolbox
     * 
     * @param showOnlyActiveAlgorithms
     *            must be true if only active algorithms (those that can be
     *            executed with the current data objects) should be shown in the
     *            toolbox
     */
    public static void setShowOnlyActiveAlgorithms(
            final boolean showOnlyActiveAlgorithms) {

        m_bShowOnlyActiveAlgorithms = showOnlyActiveAlgorithms;

    }

    /**
     * puts a collection of settings into the settings map
     * 
     * @param settings
     *            the map with settings values.
     */
    public static void setSettings(final HashMap<String, String> settings) {

        m_Settings.putAll(settings);

    }

    /**
     * Modifies the passed string, so it can be used as a safe data object name
     * (without special characters)
     * 
     * @param sName
     *            the name to modify
     * @return the modified safe name (with no special characters)
     */
    public static String modifyResultName(String sName) {

        sName = sName.replaceAll("\\.", "_");
        sName = sName.replaceAll(" ", "_");
        sName = sName.replaceAll("\\[", "_");
        sName = sName.replaceAll("\\]", "_");
        sName = sName.replaceAll("\\(", "_");
        sName = sName.replaceAll("\\)", "_");
        sName = sName.replaceAll("á", "a");
        sName = sName.replaceAll("é", "e");
        sName = sName.replaceAll("í", "i");
        sName = sName.replaceAll("ó", "o");
        sName = sName.replaceAll("ú", "u");
        sName = sName.replaceAll("ñ", "n");

        return sName;

    }

    private static void addCustomParametersPanel(
            final HashMap<String, Class> map) {

        m_ParametersPanel.putAll(map);

    }

    private static void addCustomModelerParametersPanel(
            final HashMap<String, Class> map) {

        m_ModelerParametersPanel.putAll(map);

    }

    public static Class getModelerParametersPanel(final String sName) {

        return m_ModelerParametersPanel.get(sName);

    }

    public static Class getParametersPanel(final String sName) {

        return m_ParametersPanel.get(sName);

    }

    public static ImageIcon getAlgorithmIcon(final GeoAlgorithm alg) {

        final String sName = Sextante.getAlgorithmProviderName(alg);
        for (int i = 0; i < m_AlgorithmProviders.size(); i++) {
            if (m_AlgorithmProviders.get(i).getName().equals(sName)) {
                return m_AlgorithmProviders.get(i).getIcon();
            }
        }
        return SEXTANTE_ICON;

    }

    public static ArrayList<IAlgorithmProvider> getAlgorithmProviders() {

        return m_AlgorithmProviders;

    }

    public static String getSettingParameterValue(final String sParameterName) {

        return m_Settings.get(sParameterName);

    }

    public static String setSettingParameterValue(final String sParameterName,
            final String sValue) {

        return m_Settings.put(sParameterName, sValue);

    }

    public static void updateAlgorithmProvider(final Class providerClass) {

        for (int i = 0; i < m_AlgorithmProviders.size(); i++) {
            final IAlgorithmProvider provider = m_AlgorithmProviders.get(i);
            if (providerClass.isInstance(provider)) {
                provider.update();
                Sextante.addAlgorithmsFromProvider(provider.getName(),
                        provider.getAlgorithms());
            }
        }

    }

    public static Object getAlgorithmHelp(final GeoAlgorithm alg) {

        final String sName = Sextante.getAlgorithmProviderName(alg);
        for (int i = 0; i < m_AlgorithmProviders.size(); i++) {
            if (m_AlgorithmProviders.get(i).getName().equals(sName)) {
                return m_AlgorithmProviders.get(i).getAlgorithmHelp(alg);
            }
        }
        String sFilename;
        String sPath;
        if (sName.equals("SEXTANTE")) {
            sFilename = alg.getCommandLineName() + ".xml";
            sPath = HelpIO.getHelpPath(alg, false);
            return HelpIO.getHelpAsHTMLCode(alg, sPath + File.separator
                    + sFilename);
        } else {
            return ""; // TODO:create default help page
        }

    }

    public static String getAlgorithmHelpFilename(final GeoAlgorithm alg,
            final boolean bForceCurrentLocale) {

        final String sName = Sextante.getAlgorithmProviderName(alg);
        for (int i = 0; i < m_AlgorithmProviders.size(); i++) {
            if (m_AlgorithmProviders.get(i).getName().equals(sName)) {
                return m_AlgorithmProviders.get(i).getAlgorithmHelpFilename(
                        alg, bForceCurrentLocale);
            }
        }
        String sFilename;
        String sPath;
        if (sName.equals("SEXTANTE")) {
            sFilename = alg.getCommandLineName() + ".xml";
            sPath = HelpIO.getHelpPath(alg, false);
            return sPath + File.separator + sFilename;
        } else {
            return ""; // TODO:create default help page
        }

    }

    public static GeoAlgorithm findAlgorithmByName(String AlgName) {
        final HashMap<String, HashMap<String, GeoAlgorithm>> algs = Sextante
                .getAlgorithms();
        final Set<String> groupKeys = algs.keySet();
        final Iterator<String> groupIter = groupKeys.iterator();
        while (groupIter.hasNext()) {
            final String groupKey = groupIter.next();
            final HashMap<String, GeoAlgorithm> groupAlgs = algs.get(groupKey);
            final Set keys = groupAlgs.keySet();
            final Iterator iter = keys.iterator();
            while (iter.hasNext()) {
                final GeoAlgorithm alg = groupAlgs.get(iter.next());
                if (alg.getName().equals(AlgName)) {
                    return (alg);
                }
            }
        }
        return (null);
    }

    public static void executeAlgorithm(GeoAlgorithm Alg) {

        try {
            if (Alg != null) {
                SextanteGUI.getInputFactory().clearDataObjects();
                SextanteGUI.getInputFactory().createDataObjects();
                final Object[] objs = SextanteGUI.getInputFactory()
                        .getDataObjects();
                final boolean bMeets = Alg.meetsDataRequirements(objs);
                if (bMeets) {
                    final GeoAlgorithm alg = Alg.getNewInstance();
                    final int iRet = SextanteGUI.getGUIFactory()
                            .showAlgorithmDialog(alg, null, null);
                    if (iRet == IGUIFactory.OK) {
                        final String[] cmd = alg
                                .getAlgorithmAsCommandLineSentences();
                        if (alg.hasIncompleteParams() == true) {
                            JOptionPane.showMessageDialog(null,
                                    Sextante.getText("Warn_empty_input_param"),
                                    Alg.getName(), JOptionPane.ERROR_MESSAGE);
                        }
                        if (cmd != null) {
                            History.addToHistory(cmd);
                        }
                        GeoAlgorithmExecutors.execute(alg, null);
                    }
                } else {
                    JOptionPane.showMessageDialog(null,
                            Sextante.getText("Error_run_alg_reqs_not_met"),
                            Alg.getName(), JOptionPane.ERROR_MESSAGE);
                }
            }
        } catch (final Exception e) {
            Sextante.addErrorToLog(e);
        }
    }

    public static HashMap<NameAndIcon, ArrayList<ToolboxAction>> getToolboxActions() {

        final HashMap<NameAndIcon, ArrayList<ToolboxAction>> map = new HashMap<NameAndIcon, ArrayList<ToolboxAction>>();
        for (int i = 0; i < m_AlgorithmProviders.size(); i++) {
            final IAlgorithmProvider provider = m_AlgorithmProviders.get(i);
            map.putAll(provider.getToolboxActions());
        }
        map.putAll(SextanteGUI.getGUIFactory().getToolboxActions());
        return map;
    }

    public static IToolboxRightButtonAction[] getToolboxRightButtonActions() {

        final ArrayList<IToolboxRightButtonAction> list = new ArrayList<IToolboxRightButtonAction>();
        for (int i = 0; i < m_AlgorithmProviders.size(); i++) {
            final IAlgorithmProvider provider = m_AlgorithmProviders.get(i);
            final IToolboxRightButtonAction[] actions = provider
                    .getToolboxRightButtonActions();
            for (int j = 0; j < actions.length; j++) {
                list.add(actions[j]);
            }
        }
        return list.toArray(new IToolboxRightButtonAction[0]);

    }

    /**
     * Returns the renderer to use for layers created by SEXTANTE algorithms
     * 
     */
    public static IDataRenderer getDataRenderer() {

        return m_Renderer;

    }

    public static void setDataRenderer(final IDataRenderer renderer) {

        m_Renderer = renderer;

    }

    /**
     * This method checks whether the supplied <code>path</code> includes a file
     * extension and, if it does not do so, adds the user-specified
     * <code>extension</code>. <br>
     * Extensions will always be added as lower case strings.
     * <p>
     * This method is very robust. It tries to fix the most weird file names and
     * will return the original path, unmodified, in the worst case.
     * <p>
     * This method should <i>only</i> be used for modifying file names coming
     * from file browser widgets, as it may modify file names in a way
     * unsuitable for automatically generated names.
     * 
     * @param path
     *            Full path to file for which to check extension.
     * @param extension
     *            File extension to add in case it is missing.
     * @return New file path, with extension added as required.
     */
    public static String checkAddExtension(String path, String extension) {

        if (path == null || extension == null) {
            return (path);
        }

        if (extension.trim().equals(".") || extension.trim().length() < 1) {
            return (path);
        }

        String result = new String(path);

        String toAdd = "";
        if (extension.startsWith(".")) {
            toAdd = extension.split("\\.")[extension.split("\\.").length - 1]
                    .trim().toLowerCase();
        } else {
            toAdd = extension.trim().toLowerCase();
        }

        String fileExt[] = result.split("\\.");
        if (fileExt != null) {
            if (fileExt.length < 2) {
                result = fileExt[0] + "." + toAdd;
            } else {
                if (result.endsWith(".")) {
                    result = fileExt[0];
                    for (int i = 1; i < fileExt.length; i++) {
                        result += "." + fileExt[i];
                    }
                    result += "." + toAdd;
                }
            }
        }

        return (result);
    }

    /**
     * From OpenJUMP GUIUtils.jar. It centers the component on the screen
     * 
     * @param componentToMove
     *            Description of the Parameter
     */
    public static void centreOnScreen(Component componentToMove) {
        Rectangle rect = GraphicsEnvironment.getLocalGraphicsEnvironment()
                .getMaximumWindowBounds();
        int newx, newy;
        newx = (rect.width - componentToMove.getWidth()) / 2 + rect.x;
        newy = (rect.height - componentToMove.getHeight()) / 2 + rect.y;

        Dimension screendim = Toolkit.getDefaultToolkit().getScreenSize();
        int offset = 30;
        int neww = componentToMove.getWidth(), newh = componentToMove
                .getHeight();
        // resize smaller if bigger than screen
        if (neww > screendim.width - 2 * offset) {
            neww = screendim.width - 2 * offset;
            newx = 0 + offset;
        }
        if (newh > screendim.height - 2 * offset) {
            newh = screendim.height - 2 * offset;
            newy = 0 + offset;
        }
        componentToMove.setBounds(newx, newy, neww, newh);
    }

}
