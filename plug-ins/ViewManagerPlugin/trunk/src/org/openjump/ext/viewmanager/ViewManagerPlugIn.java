package org.openjump.ext.viewmanager;

import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.util.Range;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.images.IconLoader;
import com.vividsolutions.jump.workbench.ui.plugin.FeatureInstaller;
import org.apache.log4j.Logger;
import org.openjump.ext.viewmanager.style.*;

import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import javax.xml.bind.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;

/**
 * A plugin to apply a "view" to the project (specific styles on defined layers)
 */
public class ViewManagerPlugIn extends AbstractPlugIn implements ActionListener {

    static Logger LOG = Logger.getLogger(ViewManagerPlugIn.class);
    I18N I18N_ = I18N.getInstance("view_manager");

    final JLabel viewSetNameLabel = new JLabel("", SwingConstants.LEFT);

    File viewDir;
    ViewSetPanel viewSetPanel;
    PlugInContext context;
    ViewSet currentViewSet;
    JDialog dialog;
    JAXBContext jaxbContext;

    public void initialize(PlugInContext context) throws Exception {
        WorkbenchContext workbenchContext = context.getWorkbenchContext();
        FeatureInstaller featureInstaller = new FeatureInstaller(workbenchContext);
        featureInstaller.addMainMenuPlugin(this, new String[]{MenuNames.PLUGINS});
        jaxbContext = JAXBContext.newInstance(
                ViewSet.class,
                PBasicStyle.class,
                PVertexStyle.class,
                PLabelStyle.class,
                PScale.class,
                PColorThemingStyle.class,
                Range.NegativeInfinity.class,
                Range.PositiveInfinity.class);
    }

    public String getName() {
        return I18N_.getText("view_manager","ViewManagerPlugIn");
    }

    //public ImageIcon getIcon(){
    //    return new ImageIcon(this.getClass().getResource("world.png"));
    //}

    public boolean execute(final PlugInContext context) throws Exception {
        this.context = context;
        File pluginDir = context.getWorkbenchContext().getWorkbench().getPlugInManager().getPlugInDirectory();
        viewDir = new File(pluginDir, "views");
        if (currentViewSet == null) currentViewSet = new ViewSet();
        JDialog dialog = createDialog(viewDir);
        GUIUtil.centreOnWindow(dialog);
        dialog.setVisible(true);
        return false;
    }

    JDialog createDialog(final File viewDir) throws IOException {
        dialog = new JDialog(context.getWorkbenchFrame(), getName(), false);

        // Menu bar definition
        JMenuBar menuBar = new JMenuBar();
        JMenu fileMenu = new JMenu(I18N_.getText("view_manager","ViewManagerPlugIn.Menu.File"));

        JMenuItem newViewSetMenuItem = new JMenuItem(I18N_.getText("view_manager","ViewManagerPlugIn.Menu.File.New"), KeyEvent.VK_T);
        newViewSetMenuItem.setActionCommand("newViewSet");
        newViewSetMenuItem.addActionListener(this);

        JMenuItem openViewSetMenuItem = new JMenuItem(I18N_.getText("view_manager","ViewManagerPlugIn.Menu.File.Open"), KeyEvent.VK_O);
        openViewSetMenuItem.setActionCommand("openViewSet");
        openViewSetMenuItem.addActionListener(this);

        JMenuItem saveViewSetMenuItem = new JMenuItem(I18N_.getText("view_manager","ViewManagerPlugIn.Menu.File.Save"), KeyEvent.VK_S);
        saveViewSetMenuItem.setActionCommand("saveViewSet");
        saveViewSetMenuItem.addActionListener(this);

        JMenuItem saveAsViewSetMenuItem = new JMenuItem(I18N_.getText("view_manager","ViewManagerPlugIn.Menu.File.SaveAs")+"...");
        saveAsViewSetMenuItem.setActionCommand("saveViewSetAs");
        saveAsViewSetMenuItem.addActionListener(this);

        menuBar.add(fileMenu);
        fileMenu.add(newViewSetMenuItem);
        fileMenu.add(openViewSetMenuItem);
        fileMenu.add(saveViewSetMenuItem);
        fileMenu.add(saveAsViewSetMenuItem);
        dialog.setJMenuBar(menuBar);

        // toolbar definition
        JToolBar toolBar = new JToolBar(I18N_.getText("view_manager","ViewManagerPlugIn.Toolbar"));

        JButton newViewSetButton = new JButton();
        newViewSetButton.setIcon(IconLoader.icon("fugue/folder.png"));
        newViewSetButton.setActionCommand("newViewSet");
        newViewSetButton.setToolTipText(I18N_.getText("view_manager","ViewManagerPlugIn.Toolbar.New.Tooltip"));
        newViewSetButton.addActionListener(this);
        toolBar.add(newViewSetButton);

        JButton openViewSetButton = new JButton();
        openViewSetButton.setIcon(IconLoader.icon("fugue/folder-horizontal-open_16.png"));
        openViewSetButton.setActionCommand("openViewSet");
        openViewSetButton.setToolTipText(I18N_.getText("view_manager","ViewManagerPlugIn.Toolbar.Open.Tooltip"));
        openViewSetButton.addActionListener(this);
        toolBar.add(openViewSetButton);

        JButton saveViewSetButton = new JButton();
        saveViewSetButton.setIcon(IconLoader.icon("disk.png"));
        saveViewSetButton.setActionCommand("saveViewSet");
        saveViewSetButton.setToolTipText(I18N_.getText("view_manager","ViewManagerPlugIn.Toolbar.Save.Tooltip"));
        saveViewSetButton.addActionListener(this);
        toolBar.add(saveViewSetButton);

        JButton saveViewSetAsButton = new JButton();
        saveViewSetAsButton.setIcon(IconLoader.icon("disk_dots.png"));
        saveViewSetAsButton.setActionCommand("saveViewSetAs");
        saveViewSetAsButton.setToolTipText(I18N_.getText("view_manager","ViewManagerPlugIn.Toolbar.SaveAs.Tooltip"));
        saveViewSetAsButton.addActionListener(this);
        toolBar.add(saveViewSetAsButton);

        dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
        dialog.add(toolBar, BorderLayout.NORTH);

        // Main panel
        JPanel mainPanel = new JPanel(new GridBagLayout());
        dialog.add(mainPanel, BorderLayout.CENTER);
        final GridBagConstraints constraints = new GridBagConstraints();

        constraints.gridy = 0;
        constraints.gridx = 0;
        constraints.weightx = 1.0;
        constraints.anchor = GridBagConstraints.WEST;
        constraints.insets = new Insets(2,2,2,2);

        constraints.gridwidth = 2;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(new JSeparator(), constraints);
        constraints.fill = GridBagConstraints.NONE;

        constraints.gridy++;
        constraints.gridwidth = 1;
        constraints.weightx = 0.0;
        mainPanel.add(new JLabel(I18N_.getText("view_manager","ViewManagerPlugIn.ViewSetName") + ":"), constraints);
        constraints.gridx++;
        constraints.weightx = 1.0;
        viewSetNameLabel.setText(createNewViewSetName());
        viewSetNameLabel.setForeground(Color.RED);
        mainPanel.add(viewSetNameLabel, constraints);

        constraints.gridy++;
        constraints.gridx = 0;
        constraints.weightx = 1.0;
        constraints.gridwidth = 2;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(new JSeparator(), constraints);
        constraints.fill = GridBagConstraints.NONE;

        constraints.gridy++;
        constraints.gridx = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 0.0;
        mainPanel.add(new JLabel(I18N_.getText("view_manager","ViewManagerPlugIn.add-view-from-project")), constraints);
        constraints.gridx++;
        constraints.weightx = 0.0;
        JButton jbAddFromProject = new JButton(I18N_.getText("view_manager","ViewManagerPlugIn.add"));
        jbAddFromProject.addActionListener(this);
        jbAddFromProject.setActionCommand("addFromProject");
        mainPanel.add(jbAddFromProject, constraints);

        constraints.gridy++;
        constraints.gridx = 0;
        constraints.weightx = 0.0;
        mainPanel.add(new JLabel(I18N_.getText("view_manager","ViewManagerPlugIn.add-view-from-selected-layers")), constraints);
        constraints.gridx++;
        constraints.weightx = 0.0;
        JButton jbAddFromSelectedLayers = new JButton(I18N_.getText("view_manager","ViewManagerPlugIn.add"));
        jbAddFromSelectedLayers.addActionListener(this);
        jbAddFromSelectedLayers.setActionCommand("addFromSelectedLayers");
        mainPanel.add(jbAddFromSelectedLayers, constraints);

        constraints.gridy++;
        constraints.gridx = 0;
        constraints.gridwidth = 2;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(new JSeparator(), constraints);
        constraints.fill = GridBagConstraints.NONE;

        constraints.gridy++;
        constraints.gridx = 0;
        constraints.gridwidth = 2;
        constraints.weightx = 1.0;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        viewSetPanel = new ViewSetPanel(context, currentViewSet);
        mainPanel.add(viewSetPanel, constraints);

        return dialog;
    }


    FileFilter xmlFileFilter = new FileFilter() {
        public boolean accept(File file) {
            return file.getName().toLowerCase().endsWith(".xml");
        }
        public String getDescription() {
            return "Filter xml files";
        }
    };

    String createNewViewSetName() throws IOException {
        String newViewSetName = context.getTask().getName() + I18N_.getText("view_manager","ViewManagerPlugIn.-new-viewset");
        if (viewSetNameExists(newViewSetName)) {
            int count = 2;
            while (viewSetNameExists(newViewSetName + " (" + count++ + ")")) {}
            newViewSetName = newViewSetName + " (" + (count-1) + ")";
        }
        return newViewSetName;
    }

    public void actionPerformed(ActionEvent e) {
        String action = e.getActionCommand();
        if (action.equals("newViewSet")) {
            newViewSet();
        } else if (action.equals("openViewSet")) {
            openViewSet();
        } else if (action.equals("saveViewSet")) {
            saveViewSet();
        } else if (action.equals("saveViewSetAs")) {
            saveViewSetAs();
        } else if (action.equals("addFromProject")) {
            addFromProject(false);
        } else if (action.equals("addFromSelectedLayers")) {
            addFromProject(true);
        }
    }

    // new method
    public void newViewSet() {
        try {
            String newViewSetName = createNewViewSetName();
            viewSetNameLabel.setText(newViewSetName);
            viewSetNameLabel.setForeground(Color.RED);
            viewSetNameLabel.setHorizontalAlignment(SwingConstants.LEFT);
            currentViewSet = new ViewSet();
            currentViewSet.setName(newViewSetName);
            viewSetPanel.reset(context, currentViewSet);
            dialog.pack();
            LOG.info("Add new ViewSet");
        } catch(Exception e) {
            LOG.warn("Add new ViewSet", e);
        }
    }

    // new method
    public void openViewSet() {
        JFileChooser jfc = new JFileChooser(viewDir);
        jfc.setDialogTitle(I18N_.getText("view_manager","ViewManagerPlugIn.ChooseViewSet"));
        jfc.setFileFilter(xmlFileFilter);
        int r = jfc.showDialog(dialog, "OK");
        if (r == JFileChooser.APPROVE_OPTION) {
            File viewSetFile = jfc.getSelectedFile();
            LOG.info(viewSetFile);
            try {
                if (viewSetFile.exists()) {
                    LOG.info("Unmarshall " + viewSetFile);
                    Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
                    currentViewSet = (ViewSet) unmarshaller.unmarshal(viewSetFile);
                    viewSetNameLabel.setText(viewSetFile.getName());
                    viewSetNameLabel.setForeground(Color.BLACK);
                    viewSetPanel.reset(context, currentViewSet);
                }
                dialog.pack();
            } catch (UnmarshalException ex) {
                ex.printStackTrace();
                LOG.warn(viewSetFile, ex);
            } catch (JAXBException ex) {
                ex.printStackTrace();
                LOG.warn(viewSetFile, ex);
            }
        }
    }

    public void saveViewSet() {
        try {
            if (!new File(viewDir, viewSetNameLabel.getText()).exists()) {
                LOG.error("" + viewSetNameLabel.getName() + " does not exists");
                return;
            }
            if (currentViewSet == null) {
                currentViewSet = new ViewSet();
            }
            currentViewSet.setName(viewSetNameLabel.getText());
            Marshaller marshaller = jaxbContext.createMarshaller();
            marshaller.marshal(currentViewSet, viewSetNameToFile(viewSetNameLabel.getText()));
            viewSetNameLabel.setForeground(Color.BLACK);
            dialog.pack();
            LOG.info("Save ViewSet " + currentViewSet);
        } catch(JAXBException ex) {
            ex.printStackTrace();
            LOG.warn("saveViewSet", ex);
        }
    }

    // new
    public void saveViewSetAs() {
        JFileChooser jfc = new JFileChooser(viewDir);
        jfc.setDialogType(JFileChooser.SAVE_DIALOG);
        jfc.setFileFilter(xmlFileFilter);
        int r = jfc.showSaveDialog(dialog);
        if (r == JFileChooser.APPROVE_OPTION) {
            File viewSetFile = jfc.getSelectedFile();
            try {
                if (currentViewSet == null) {
                    context.getWorkbenchFrame().warnUser("No current view set");
                    return;
                }
                currentViewSet.setName(viewSetFile.getName());
                Marshaller marshaller = jaxbContext.createMarshaller();
                marshaller.marshal(currentViewSet, viewSetNameToFile(currentViewSet.getName()));
                viewSetNameLabel.setText(viewSetFile.getName());
                viewSetNameLabel.setForeground(Color.BLACK);
                dialog.pack();
                LOG.info("Save ViewSet " + currentViewSet);
            } catch(JAXBException ex) {
                ex.printStackTrace();
                LOG.warn("saveViewSet", ex);
            }
        }
    }


    // new method using file system
    private boolean viewSetNameExists(String name) throws IOException {
        if (viewDir != null && viewDir.exists()) {
            for (File file : viewDir.listFiles()) {
                if (file.getName().toLowerCase().replaceAll("\\.xml$","")
                        .equalsIgnoreCase(name.toLowerCase().replaceAll("\\.xml$", ""))) {
                    return true;
                }
            }
            return false;
        } else if (viewDir != null) {
            if (viewDir.mkdirs()) {
                return false;
            }
            throw new IOException("" + viewDir + " has not been created");
        } else {
            throw new IOException("No directory defined to store views");
        }
    }

    public void addFromProject(boolean selectedLayersOnly) {
        try {
            //System.out.println("Create view");
            View view = new View(context, selectedLayersOnly);
            if (view.name.equals(context.getTask().getName())) {
                view.name = context.getTask().getName() + " (" + currentViewSet.views.size() + ")";
            }
            //System.out.println("Add project's view to ViewSet");
            currentViewSet.addView(view);
            viewSetNameLabel.setForeground(Color.RED);
            dialog.repaint();
            LOG.info("Add project's view to ViewSet");
        } catch(Exception e) {
            LOG.warn("addFromProject", e);
        }
    }

    private String fileToViewSetName(File file) {
        String fileName = file.getName();
        int dotIndex = fileName.indexOf(".");
        return dotIndex < 0 ? fileName : fileName.substring(0, dotIndex);
    }

    private File viewSetNameToFile(String name) {
        if (name.toLowerCase().endsWith(".xml")) {
            return new File(viewDir, name);
        } else {
            return new File(viewDir, name + ".xml");
        }
    }

}
