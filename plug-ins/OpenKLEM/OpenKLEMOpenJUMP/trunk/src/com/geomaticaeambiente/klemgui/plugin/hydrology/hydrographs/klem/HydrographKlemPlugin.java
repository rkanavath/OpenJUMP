package com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem;

import java.awt.Color;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.NoninvertibleTransformException;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.concurrent.ExecutionException;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.StyledDocument;

import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.rasterimage.TiffTags;
import org.openjump.core.rasterimage.TiffTags.TiffMetadata;

import com.geomaticaeambiente.klemgui.exceptions.WarningException;
import com.geomaticaeambiente.klemgui.ui.CustomComboBox;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.HelpDialog;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.ui.Symbologies;
import com.geomaticaeambiente.klemgui.ui.TextStyles;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.GeometryUtils;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.klemgui.utils.ShapefileUtils;
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsCalculator;
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsStripe;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.upslopearea.UpslopeAreaCalculator;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.Logger;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.ThreadedBasePlugIn;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;
import com.vividsolutions.jump.workbench.ui.task.TaskMonitorManager;

/**
 *
 * @author Paola
 */
public class HydrographKlemPlugin extends AbstractInputKlemPlugin {

    public HydrographKlemPlugin(PlugInContext context,
            InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
        initialDialog.setModalityType(Dialog.ModalityType.MODELESS);
    }

    @Override
    public InitialData setInitialData() {

        // rasterImageLayers = PluginUtils.getRasterImageLayers(context);
        // layers = PluginUtils.getLayers(context);

        return setInputTabInitialData();
    }

    @Override
    public ComponentsTreeMap setComponentsActions(
            ComponentsTreeMap personalTreeMap) {

        return addActionToComponents(personalTreeMap);
    }

    @Override
    public JPanel buildPluginPanel(final ComponentsTreeMap componentsWithActions) {

        if (mainPanel != null) {
            return mainPanel;
        }
        mainPanel = new MainPanel(super.getInitialDialog(),
                componentsWithActions, true, false, true, PluginUtils
                        .getResources().getString(
                                "MainPanel.ExecuteButton.text"), true,
                MainPanel.ExtraSubPanelPosition.INITIAL, layerablesList) {

            /**
                     * 
                     */
            private static final long serialVersionUID = 1L;

            @Override
            public void rightButton() {
                try {

                    AbstractPlugIn.toActionListener(new ThreadedBasePlugIn() {
                        @Override
                        public String getName() {
                            return null;
                        }

                        @Override
                        public boolean execute(PlugInContext context)
                                throws Exception {
                            return true;
                        }

                        @Override
                        public void run(TaskMonitor monitor,
                                PlugInContext context) throws Exception {
                            monitor.report(PluginUtils.getResources()
                                    .getString("OpenKlem.executing-process"));
                            // monitor.allowCancellationRequests();
                            reportNothingToUndoYet(context);
                            try {
                                setNextButton(componentsWithActions);
                            } catch (final Exception ex) {
                                Logger.error(getName(), ex);
                            }
                        }
                    }, context.getWorkbenchContext(), new TaskMonitorManager())
                            .actionPerformed(null);

                    //    setNextButton(componentsWithActions);
                } catch (final Exception ex) {
                    ErrorDialog.show(super.getInitialDialog(),
                            PluginUtils.plugInName, ex.toString(),
                            StringUtil.stackTrace(ex));
                }

            }

            @Override
            public void leftButton() {

                try {

                    final HelpDialog helpDialog = new HelpDialog(
                            super.getInitialDialog(), false);
                    helpDialog.setPreferredSize(new Dimension(300, 500));
                    helpDialog.initComponents(getPrjFileHelpDoc());
                    helpDialog.setLocationRelativeTo(this);
                    helpDialog.setVisible(true);

                } catch (final BadLocationException ex) {
                    ErrorDialog.show(super.getInitialDialog(),
                            PluginUtils.plugInName, ex.toString(),
                            StringUtil.stackTrace(ex));
                }

            }

            @Override
            public void centerButton() {
            }
        };

        mainPanel.setRightButtonText(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.NextButton.label"));
        mainPanel.setEnableExecute(false);

        mainPanel.setLeftButtonText(PluginUtils.getResources().getString(
                "KlemGUI.Help.label"));
        return mainPanel;
    }

    @Override
    public String toString() {
        return PluginUtils.getResources().getString(
                "HyetographPlugIn.KinematicLocalExcessModel");
    }

    private InitialData setInputTabInitialData() {

        final InitialData initialData = new InitialData();

        initialData
                .setParam_MoreActions(
                        // GUIUtils.setGUILabel(PluginUtils.getResources().getString("HydrographKlemPlugin.ChooseMode.label")),

                        new ActionObject[] {
                                new ActionObject(
                                        PluginUtils
                                                .getResources()
                                                .getString(
                                                        "HydrographKlemPlugin.CreateProject.label")),
                                new ActionObject(
                                        PluginUtils
                                                .getResources()
                                                .getString(
                                                        "HydrographKlemPlugin.LoadProjectButton.label")) },
                        GUIUtils.EXTRA);

        /*
         * initialData.setParam_Action(new ActionObject(false, PluginUtils
         * .getResources() .getString("HydrographKlemPlugin.LoadFiles.text")),
         * GUIUtils.EXTRA);
         */

        initialData
                .setParam_Label_TextBox(GUIUtils.setGUILabel(DEM_RASTER),
                        PluginUtils.getRasterImageLayers(layerablesList
                                .getLayerables()), GUIUtils.INPUT);
        initialData
                .setParam_Label_TextBox(GUIUtils.setGUILabel(CN_RASTER),
                        PluginUtils.getRasterImageLayers(layerablesList
                                .getLayerables()), GUIUtils.INPUT);
        // bluelines
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(BLUELINES),
                PluginUtils.getLayers(layerablesList.getLayerables()),
                GUIUtils.INPUT);

        // other
        // project name
        initialData.setParam_Label_TextBox_Button(
                GUIUtils.setGUILabel(PRJ_PATH), "", new ActionObject(""),
                GUIUtils.OTHER);
        initialData.setParam_Label_TextBox(GUIUtils.setGUILabel(PRJ_NAME), "",
                GUIUtils.OTHER);

        return initialData;
    }

    JCheckBox jCheckBox_Extent;

    private ComponentsTreeMap addActionToComponents(
            final ComponentsTreeMap personalTreeMap) {

        // disable next button

        setVisibilityComponent(personalTreeMap);

        final JButton loadButton = (JButton) personalTreeMap.getComponent("00",
                GUIUtils.EXTRA, 1);
        loadButton.setBackground(new Color(51, 153, 255));
        loadButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {

                try {

                    setLoadProjectButton(personalTreeMap, null);
                    // TODO: deactivate next button

                } catch (final Exception ex) {
                    ErrorDialog.show(context.getWorkbenchFrame(),
                            PluginUtils.plugInName, ex.toString(),
                            StringUtil.stackTrace(ex));
                }
            }
        });

        final JButton createButton = (JButton) personalTreeMap.getComponent(
                "00", GUIUtils.EXTRA, 0);
        createButton.setBackground(new Color(51, 153, 255));
        createButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {

                setCreateNewProjectButton(personalTreeMap);
                // TODO: activate next button
            }
        });

        // jCheckBox_Extent = (JCheckBox) personalTreeMap.getComponent("01",
        // GUIUtils.EXTRA, 0);

        final JTextField jTextField_PrjPath = (JTextField) personalTreeMap
                .getComponent("00", GUIUtils.OTHER, 1);

        final JButton folderButton = (JButton) personalTreeMap.getComponent(
                "00", GUIUtils.OTHER, 2);
        folderButton.setIcon(PluginUtils.getFolderIcon());
        folderButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {

                final File projectPath = PluginUtils.openJChooserDialog(
                        context.getActiveInternalFrame(),
                        JFileChooser.DIRECTORIES_ONLY,
                        JFileChooser.SAVE_DIALOG, null, null, false)[0];

                if (projectPath != null) {
                    jTextField_PrjPath.setText(projectPath.getAbsolutePath());
                }
            }
        });

        return personalTreeMap;
    }

    private void setNextButton(ComponentsTreeMap componentsWithActions)
            throws IOException, Exception {

        // create project
        klemProps = new KlemProperties();

        // get data info
        final String demSelected = GUIUtils
                .getStringValue(componentsWithActions.getComponent("00",
                        GUIUtils.INPUT, 1));
        final String cnSelected = GUIUtils.getStringValue(componentsWithActions
                .getComponent("01", GUIUtils.INPUT, 1));
        final String bluelineSelected = GUIUtils
                .getStringValue(componentsWithActions.getComponent("02",
                        GUIUtils.INPUT, 1));

        final String projectPath = GUIUtils
                .getStringValue(componentsWithActions.getComponent("00",
                        GUIUtils.OTHER, 1));
        final String projectName = GUIUtils
                .getStringValue(componentsWithActions.getComponent("01",
                        GUIUtils.OTHER, 1));

        // checks if the fields are filled
        checkInputParameters(demSelected, cnSelected, projectPath, projectName);

        // TODO: check if project folder already exist
        // create project folder
        final File newPrjFolder = new File(projectPath, projectName);
        if (!newPrjFolder.exists()) {
            newPrjFolder.mkdir();
        } else {
            final int ret = JOptionPane.showOptionDialog(
                    super.getInitialDialog(),
                    PluginUtils.getResources().getString(
                            "HydrographKlemPlugin.FolderExist.message"),
                    PluginUtils.plugInName, JOptionPane.YES_NO_OPTION,
                    JOptionPane.INFORMATION_MESSAGE, null, null, null);

            if (ret == JOptionPane.YES_OPTION) {
                // delete folder and files
                GUIUtils.deleteFolder(newPrjFolder);

                newPrjFolder.mkdir();
            } else {
                return;
            }
        }
        // create input and output folder
        final File inputFolder = new File(newPrjFolder, "01_Input");
        inputFolder.mkdir();
        final File outputFolder = new File(newPrjFolder, "02_Output");
        outputFolder.mkdir();

        // create raster file in folder
        final RasterImageLayer demRil = PluginUtils
                .getRasterImageLayerSelected((CustomComboBox.RasterComboBox) componentsWithActions
                        .getComponent("00", GUIUtils.INPUT, 1));
        final RasterImageLayer cnRil = PluginUtils
                .getRasterImageLayerSelected((CustomComboBox.RasterComboBox) componentsWithActions
                        .getComponent("01", GUIUtils.INPUT, 1));

        // check if raster have same spatial data
        RasterUtils.rasterCompare(demRil, cnRil);

        // copy file in folder
        final File demPrjFile = new File(inputFolder.getAbsolutePath()
                + File.separator + KlemProperties.demFileName);
        final File cnPrjFile = new File(inputFolder.getAbsolutePath()
                + File.separator + KlemProperties.cnFileName);
        RasterUtils.saveOutputRasterAsTiff(demRil, demPrjFile);
        RasterUtils.saveOutputRasterAsTiff(cnRil, cnPrjFile);

        // create blueline shapefile in input folder
        File bluelinesFile = null;
        LineString[] bluelines = null;
        if (bluelineSelected != null && !bluelineSelected.isEmpty()
                && !bluelineSelected.equals("")) {
            final Layer bluelinesLayer = PluginUtils
                    .getLayerSelected((CustomComboBox.LayerComboBox) componentsWithActions
                            .getComponent("02", GUIUtils.INPUT, 1));
            bluelinesFile = ShapefileUtils.saveLayerAsShapefile(bluelinesLayer,
                    inputFolder);

            // LineStrings
            bluelines = GeometryUtils.getLineStringsFromFeatures(bluelinesLayer
                    .getFeatureCollectionWrapper());

        }

        System.out.println(newPrjFolder.getAbsolutePath());

        // create flow dir and upslope raster
        final File flowDirFile = new File(inputFolder.getAbsolutePath()
                + File.separator + KlemProperties.flowDirFileName);
        final File upslopeFile = new File(inputFolder.getAbsolutePath()
                + File.separator + KlemProperties.upslopeAreaFileName);
        createGridFiles(demRil, bluelines, 100d, flowDirFile, upslopeFile);

        // TODO: colorModel of upslope when leftButton on OJ
        // Memorize data into the project
        klemProps.setProjectFolder(new File(newPrjFolder.getAbsolutePath()));
        klemProps.setProjectName(projectName);
        // klemProp.setDemFile(new File(RasterUtils.checkRasterName(demPrjFile,
        // "tif").getAbsolutePath()));
        // klemProp.setCNFile(new File(RasterUtils.checkRasterName(cnPrjFile,
        // "tif").getAbsolutePath()));
        if (bluelinesFile != null) {
            klemProps.setWithBluelines(true); // TODO
        }
        // klemProp.setFlowDir(flowDirFile.getAbsolutePath());
        // klemProp.setUpslopeArea(upslopeFile.getAbsolutePath());

        // print project XML
        final XMLCreatorKlem createXML = new XMLCreatorKlem(klemProps);
        createXML.createDocument();

        setParamsTab();
        setAdvancedParamsTab();

        setVetoableChangeListener();

    }

    private void setLoadProjectButton(ComponentsTreeMap componentsWithActions,
            JComponent component) throws IOException,
            NoninvertibleTransformException, Exception {

        final FileNameExtensionFilter filter = new FileNameExtensionFilter(
                "Klem", new String[] { "xml" });
        final File[] loadFiles = PluginUtils.openJChooserDialog3(component,
                JFileChooser.FILES_ONLY, JFileChooser.OPEN_DIALOG, filter,
                null, false);

        if (loadFiles == null) {
            return;
        }

        final File loadFile = loadFiles[0];

        final SAXParserKlem sAXParserKlem = new SAXParserKlem();
        sAXParserKlem.parseDocument(loadFile);
        klemProps = sAXParserKlem.getKlemProjectPropertiesTemp();

        // check if all project element are set
        checkProjectParameters(klemProps);

        // fill the components
        // project path
        final JTextField prjFolder = (JTextField) componentsWithActions
                .getComponent("00", GUIUtils.OTHER, 1);
        prjFolder.setText(klemProps.getProjectFolder().getAbsolutePath());
        prjFolder.setEditable(false);

        final JButton buttonPath = (JButton) componentsWithActions
                .getComponent("00", GUIUtils.OTHER, 2);
        buttonPath.setEnabled(false);

        // project name
        String projectName = loadFile.getName();
        final int index = projectName.lastIndexOf(".xml");
        projectName = projectName.substring(0, index);
        final JTextField prjName = (JTextField) componentsWithActions
                .getComponent("01", GUIUtils.OTHER, 1);
        prjName.setText(projectName);
        prjName.setEditable(false);
        klemProps.setProjectName(projectName);

        // dem file
        final JComboBox dem = (JComboBox) componentsWithActions.getComponent(
                "00", GUIUtils.INPUT, 1);
        dem.removeAllItems();
        dem.addItem(klemProps.getDemFile());
        dem.setEnabled(false);

        // cn file
        final JComboBox cn = (JComboBox) componentsWithActions.getComponent(
                "01", GUIUtils.INPUT, 1);
        cn.removeAllItems();
        cn.addItem(klemProps.getCNFile());
        cn.setEnabled(false);

        // bluelines
        final JComboBox bluelines = (JComboBox) componentsWithActions
                .getComponent("02", GUIUtils.INPUT, 1);
        if (klemProps.getBluelinesFile() != null) {
            bluelines.removeAllItems();
            bluelines.addItem(klemProps.getBluelinesFile());
        }

        bluelines.setEnabled(false);

        // Memroize dem grid
        // RasterImageLayer demRil =
        // RasterUtils.getRasterImageLayerFromFile(context,
        // klemProp.getDemFile());
        // klemProp.setDemRasterImageLayer(demRil);
        // Memorize upslope on project and display on OJ
        // RasterImageLayer upslopeRil =
        // RasterUtils.getRasterImageLayerFromFile(context,
        // klemProp.getUpslopeAreaFile());
        // klemProp.setUpslopeRasterImageLayer(upslopeRil);
        // RasterUtils.addImageToOJ(context.getWorkbenchContext(), upslopeRil);
        loadedPrj = true;

        setParamsTab();
        setAdvancedParamsTab();

        setVetoableChangeListener();
        if (PluginUtils.loadRastersCheckBox.isSelected()) {
            // Load project raster layers into OpenJUMP view if required
            try {
                RasterUtils.addFileToOJ(context, klemProps.getDemFile(),
                        klemProps.getProjectName());
                RasterUtils.addFileToOJ(context, klemProps.getCNFile(),
                        klemProps.getProjectName());

                RasterUtils.addFileToOJ(context, klemProps.getFlowDir(),
                        Symbologies.getFlowDirSymb(),
                        klemProps.getProjectName());

                final TiffMetadata tiffMetadata = TiffTags
                        .readMetadata(klemProps.getUpslopeAreaFile());

                final double num = tiffMetadata.getEnvelope().getWidth()
                        / tiffMetadata.getColsCount();

                RasterUtils.addFileToOJ(context,
                        klemProps.getUpslopeAreaFile(),
                        Symbologies.getUpslopeAreaSymb(num),
                        klemProps.getProjectName());

            } catch (final Exception ex) {
                ErrorDialog.show(super.getInitialDialog(),
                        PluginUtils.plugInName, ex.toString(),
                        StringUtil.stackTrace(ex));
            }

        }

    }

    private void setVetoableChangeListener() {

        final VetoableChangeListener validator = new VetoableChangeListener() {

            @Override
            public void vetoableChange(PropertyChangeEvent evt)
                    throws PropertyVetoException {
                final int oldSelection = (Integer) evt.getOldValue();
                final int newSelection = (Integer) evt.getNewValue();

                if ((oldSelection == -1 || newSelection == -1)
                        || isValidTab(oldSelection)) {
                    return;
                }

                throw new PropertyVetoException("change not valid", evt);

            }

            private boolean isValidTab(int oldSelection) {
                // Validation logic
                final JTabbedPane mainTabbedPane = getInitialDialog()
                        .getTabbedPane();
                switch (oldSelection) {
                case 0:
                    return true;
                case 1:
                    advancedParams
                            .setHyetoControlsVisibility(!(paramsTab
                                    .getRainfallType() == KlemProperties.RainfallType.HISTORICAL));
                    try {
                        paramsTab.setAndCheckParams();
                        return true;
                    } catch (final WarningException ex) {
                        JOptionPane.showMessageDialog(
                                mainTabbedPane.getParent(), ex.getMessage(),
                                PluginUtils.plugInName,
                                JOptionPane.WARNING_MESSAGE);
                        return false;
                    } catch (final Exception ex) {
                        ErrorDialog.show(mainTabbedPane.getParent(),
                                PluginUtils.plugInName, ex.toString(),
                                StringUtil.stackTrace(ex));
                        return false;
                    }
                case 2:
                    try {
                        advancedParams.setAndCheckParams();
                        return true;
                    } catch (final WarningException ex) {
                        JOptionPane.showMessageDialog(
                                mainTabbedPane.getParent(), ex.getMessage(),
                                PluginUtils.plugInName,
                                JOptionPane.WARNING_MESSAGE);
                        return false;
                    } catch (final Exception ex) {
                        ErrorDialog.show(mainTabbedPane.getParent(),
                                PluginUtils.plugInName, ex.toString(),
                                StringUtil.stackTrace(ex));
                        return false;
                    }
                }
                return true;
            }
        };
        ((VetoableSingleSelectionModel) super.getInitialDialog()
                .getTabbedPane().getModel())
                .addVetoableChangeListener(validator);

    }

    private void setCreateNewProjectButton(
            ComponentsTreeMap componentsWithActions) {

        GUIUtils.setEnableComponents(true, GUIUtils.getJComponentFromRowRange(
                componentsWithActions, 0, 2, GUIUtils.INPUT));
        GUIUtils.setEnableComponents(true, GUIUtils.getJComponentFromRowRange(
                componentsWithActions, 0, 1, GUIUtils.OTHER));

        final JButton loadButton = (JButton) componentsWithActions
                .getComponent("00", GUIUtils.EXTRA, 1);
        // loadButton.setEnabled(false); //TODO: verify if it is necessary

        // clear leftButton command
        final JComboBox dem = (JComboBox) componentsWithActions.getComponent(
                "00", GUIUtils.INPUT, 1);
        dem.removeAllItems();
        final JComboBox cn = (JComboBox) componentsWithActions.getComponent(
                "01", GUIUtils.INPUT, 1);
        cn.removeAllItems();

        final RasterImageLayer[] rasterImageLayers = PluginUtils
                .getRasterImageLayers(layerablesList.getLayerables());
        dem.addItem("");
        cn.addItem("");
        for (final RasterImageLayer rasterImageLayer : rasterImageLayers) {
            dem.addItem(rasterImageLayer);
            cn.addItem(rasterImageLayer);
        }

        // dem.setEnabled(true);
        // cn.setEnabled(true);

        final Layer[] layers = PluginUtils.getLayers(layerablesList
                .getLayerables());
        final JComboBox bluelines = (JComboBox) componentsWithActions
                .getComponent("02", GUIUtils.INPUT, 1);
        bluelines.removeAllItems();
        bluelines.addItem("");
        for (final Layer layer : layers) {
            bluelines.addItem(layer);
        }
        bluelines.setEnabled(true);

        final JButton buttonPath = (JButton) componentsWithActions
                .getComponent("00", GUIUtils.OTHER, 2);
        buttonPath.setEnabled(true);
        final JTextField prjFolder = (JTextField) componentsWithActions
                .getComponent("00", GUIUtils.OTHER, 1);
        final JTextField prjName = (JTextField) componentsWithActions
                .getComponent("01", GUIUtils.OTHER, 1);
        prjFolder.setEnabled(true);
        prjName.setEnabled(true);
        prjFolder.setEditable(true);
        prjName.setEditable(true);
        prjFolder.setText("");
        prjName.setText("");

        // disable tabs
        final JTabbedPane mainTabbedPane = super.getInitialDialog()
                .getTabbedPane();
        mainTabbedPane.setEnabledAt(1, false);
        mainTabbedPane.setEnabledAt(2, false);
        mainTabbedPane.setEnabledAt(3, false);
        mainTabbedPane.setSelectedIndex(0);

        if (klemProps != null) {
            klemProps.deleteData();
        }

        mainPanel.setEnableExecute(true);

    }

    public void checkProjectParameters(KlemProperties klemProp) {

        if (klemProp.getDemFile() == null || klemProp.getCNFile() == null
                || klemProp.getProjectFolder() == null
                || klemProp.getUpslopeAreaFile() == null
                || klemProp.getFlowDir() == null) {

            throw new NullPointerException(PluginUtils.getResources()
                    .getString("HydrographKlemPlugin.ProjectNotValid.label"));
        }

        if (klemProp.getDemFile().getAbsolutePath().isEmpty()
                || klemProp.getCNFile().getAbsolutePath().isEmpty()
                || klemProp.getProjectFolder().getAbsolutePath().isEmpty()
                || klemProp.getUpslopeAreaFile().getAbsolutePath().isEmpty()
                || klemProp.getFlowDir().getAbsolutePath().isEmpty()) {

            throw new NullPointerException(PluginUtils.getResources()
                    .getString("HydrographKlemPlugin.ProjectNotValid.label"));
        }

        if (!klemProp.getDemFile().exists() || !klemProp.getCNFile().exists()
                || !klemProp.getProjectFolder().exists()
                || !klemProp.getUpslopeAreaFile().exists()
                || !klemProp.getFlowDir().exists()) {

            throw new NullPointerException(PluginUtils.getResources()
                    .getString("HydrographKlemPlugin.ProjectNotValid.label"));
        }
    }

    private void checkInputParameters(String dem, String cn, String path,
            String name) throws Exception {
        GUIUtils.checkStringValue(dem, DEM_RASTER);
        GUIUtils.checkStringValue(cn, CN_RASTER);
        GUIUtils.checkStringValue(path, PRJ_PATH);
        GUIUtils.checkStringValue(name, PRJ_NAME);
    }

    private void setParamsTab() throws Exception {

        final JTabbedPane mainTabbedPane = super.getInitialDialog()
                .getTabbedPane();

        paramsTab = new ParamsTab(context, super.getInitialDialog(),
                layerablesList);
        paramsTab.setKlemProperties(klemProps);
        paramsTab.setLoadedPrj(loadedPrj);
        mainTabbedPane.setComponentAt(1, paramsTab.getTabPluginComponents());

        // mainTabbedPane.getModel().addChangeListener(new ChangeListener() {
        // int lastTabIndex = -1;
        // @Override
        // public void stateChanged(ChangeEvent e) {
        // int newIndex = mainTabbedPane.getSelectedIndex();
        //
        // // If going to advanced params, set visibility of Hyeto params
        // // And check params
        // if (newIndex == 2) {
        //
        // advancedParams.setHyetoControlsVisibility(
        // !(paramTab.getRainfallType() ==
        // KlemProperties.RainfallType.HISTORICAL));
        //
        // }
        //
        // try{
        // if (lastTabIndex == 1) {
        // paramTab.setAndCheckParams();
        // }
        // } catch (WarningException ex) {
        // JOptionPane.showMessageDialog(
        // mainTabbedPane.getParent(),
        // ex.getMessage(),
        // PluginUtils.plugInName,
        // JOptionPane.WARNING_MESSAGE);
        // } catch (Exception ex) {
        // JOptionPane.showMessageDialog(mainTabbedPane.getParent(), ex,
        // PluginUtils.plugInName, JOptionPane.ERROR_MESSAGE);
        // }
        //
        // lastTabIndex = newIndex;
        //
        // }
        //
        // });

        mainTabbedPane.setEnabledAt(1, true);
        mainTabbedPane.setEnabledAt(2, true);
        mainTabbedPane.setSelectedIndex(1);

    }

    private void setAdvancedParamsTab() throws Exception {

        final JTabbedPane mainTabbedPane = super.getInitialDialog()
                .getTabbedPane();

        advancedParams = new AdvancedParamsOutput(context,
                super.getInitialDialog(), layerablesList);
        advancedParams.setKlemProperties(klemProps);
        advancedParams.setLoadedPrj(loadedPrj);
        mainTabbedPane.setComponentAt(2,
                advancedParams.getTabPluginComponents());

        // mainTabbedPane.getModel().addChangeListener(new ChangeListener() {
        // int lastTabIndex = -1;
        // @Override
        // public void stateChanged(ChangeEvent e) {
        // int newIndex = mainTabbedPane.getSelectedIndex();
        //
        // try{
        // if (lastTabIndex == 2) {
        // advancedParams.setAndCheckParams();
        // }
        // } catch (WarningException ex) {
        // JOptionPane.showMessageDialog(mainTabbedPane.getParent(),
        // ex.getMessage(), PluginUtils.plugInName,
        // JOptionPane.WARNING_MESSAGE);
        //
        // } catch (Exception ex) {
        // JOptionPane.showMessageDialog(mainTabbedPane.getParent(), ex,
        // PluginUtils.plugInName, JOptionPane.ERROR_MESSAGE);
        // }
        //
        // lastTabIndex = newIndex;
        //
        // }
        //
        // });

    }

    private void createGridFiles(RasterImageLayer demRil,
            LineString[] bluelines, Double bluelinesWeight, File flowDirFile,
            File upslopeFile) throws NoninvertibleTransformException,
            InterruptedException, ExecutionException, IOException,
            FileNotFoundException, TiffTags.TiffReadingException, Exception {

        // get input raster as rasterImageLayer from string
        final DoubleBasicGrid demGrid = RasterUtils.getDoubleBasicGrid(demRil);

        // Flow direction
        final FlowDirsCalculator flowDireCalculator = new FlowDirsCalculator(
                demGrid, FlowDirsStripe.FlowDirAlgorithm.D8, bluelines,
                bluelinesWeight);
        final FlowDirBasicGrid flowDirsGrid = flowDireCalculator.calculate();

        // Upslope
        final UpslopeAreaCalculator upslopeArea = new UpslopeAreaCalculator(
                demGrid, bluelines, bluelinesWeight);
        final DoubleBasicGrid upslopeGrid = upslopeArea
                .calc(FlowDirsStripe.FlowDirAlgorithm.D8);

        // Create the output files
        RasterUtils.saveOutputRasterAsTiff(flowDirsGrid, flowDirFile);
        RasterUtils.saveOutputRasterAsTiff(upslopeGrid, upslopeFile);

        // Display
        RasterUtils.getRasterImageLayerFromFile(context.getWorkbenchContext(),
                upslopeFile);

    }

    private void setVisibilityComponent(ComponentsTreeMap personalTreeMap) {
        GUIUtils.setEnableComponents(false, GUIUtils.getJComponentFromRowRange(
                personalTreeMap, 0, 2, GUIUtils.INPUT));
        GUIUtils.setEnableComponents(false, GUIUtils.getJComponentFromRowRange(
                personalTreeMap, 0, 1, GUIUtils.OTHER));
    }

    private StyledDocument getPrjFileHelpDoc() throws BadLocationException {

        final StyledDocument doc = new DefaultStyledDocument();

        TextStyles.addBasicStylesToDocument(doc);
        doc.remove(0, doc.getLength());
        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.Title")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.TITLE1));
        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.Text1")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.REGULAR));

        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.CreatingTitle")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.TITLE2));
        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.CreatingText")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.REGULAR));

        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.SimParamsTitle")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.TITLE2));
        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.SimParamsText")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.REGULAR));

        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.AdvParamsTitle")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.TITLE2));
        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.AdvParamsText")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.REGULAR));

        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.RoutingTimeTitle")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.BOLD));
        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.RoutingTimeText")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.REGULAR));

        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.HyteographTitle")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.BOLD));
        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.HyetographText")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.REGULAR));

        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.HydrographTitle")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.BOLD));
        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.HydrographText")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.REGULAR));

        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.OutputTitle")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.TITLE2));
        doc.insertString(
                doc.getLength(),
                PluginUtils.getResources().getString(
                        "HydrographKlemPlugin.Help.OutputText")
                        + System.getProperty("line.separator"),
                doc.getStyle(TextStyles.REGULAR));

        // stykedDoc.insertString(stykedDoc.getLength(), "TODO", null);
        return doc;

    }

    private final PlugInContext context;
    // private RasterImageLayer[] rasterImageLayers;
    // private Layer[] layers;
    private final String DEM_RASTER = PluginUtils.getResources().getString(
            "KlemGUI.InputFilledDem.label");
    private final String CN_RASTER = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.CNRaster.label");
    private final String BLUELINES = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.Bluelines.label");
    private final String PRJ_NAME = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.ProjectName.label");
    private final String PRJ_PATH = PluginUtils.getResources().getString(
            "HydrographKlemPlugin.ProjectFolder.label");
    // private final String ADVANCED_PARAMS =
    // PluginUtils.getResources().getString("HydrographKlemPlugin.AdvancedParams.label");

    private KlemProperties klemProps;
    private boolean loadedPrj = false;
    // private Coordinate closeSectionCoords;
    private MainPanel mainPanel;
    private ParamsTab paramsTab;

    private final LayerablesList layerablesList;

    private AdvancedParamsOutput advancedParams;
}
