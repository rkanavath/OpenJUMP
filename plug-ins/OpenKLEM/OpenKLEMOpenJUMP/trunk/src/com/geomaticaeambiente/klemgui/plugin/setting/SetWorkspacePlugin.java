package com.geomaticaeambiente.klemgui.plugin.setting;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.plugin.PersistentBlackboardPlugIn;

/**
 *
 * @author Geomatica
 */
public class SetWorkspacePlugin extends AbstractInputKlemPlugin {

    public SetWorkspacePlugin(PlugInContext context,
            InitialDialog initialDialog, LayerablesList layerablesList) {
        super(context, initialDialog);
        this.context = context;
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

        String workspacePath = "";
        if (PluginUtils.getWorkspacePath() != null) {
            workspacePath = PluginUtils.getWorkspacePath().getAbsolutePath();
        }

        final InitialData initialData = new InitialData();
        initialData.setParam_Label_TextBox_Button(
                GUIUtils.setGUILabel(WORKSPACE), workspacePath,
                // new ActionObject(bundle.getString("KlemGUI.SetLabel.label")),
                new ActionObject(""), GUIUtils.INPUT);

        return initialData;

    }

    @Override
    public ComponentsTreeMap setComponentsActions(
            ComponentsTreeMap personalTreeMap) {

        final JTextField field = (JTextField) personalTreeMap.getComponent(
                "00", GUIUtils.INPUT, 1);

        final JButton button = (JButton) personalTreeMap.getComponent("00",
                GUIUtils.INPUT, 2);
        button.setIcon(PluginUtils.getFolderIcon());
        button.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {

                final String folderPath = (String) PersistentBlackboardPlugIn
                        .get(context.getWorkbenchContext()).get(
                                FILE_CHOOSER_DIRECTORY_KEY);

                File folder = null;
                if (folderPath != null) {
                    folder = new File(folderPath);
                }

                final File path = PluginUtils.openJChooserDialog(
                        context.getActiveInternalFrame(),
                        JFileChooser.DIRECTORIES_ONLY,
                        JFileChooser.SAVE_DIALOG, null, folder, false)[0];

                if (path == null) {
                    return;
                }

                PersistentBlackboardPlugIn.get(context.getWorkbenchContext())
                        .put(FILE_CHOOSER_DIRECTORY_KEY, path.toString());

                field.setText(path.getAbsolutePath());
            }
        });

        return personalTreeMap;
    }

    @Override
    public JPanel buildPluginPanel(final ComponentsTreeMap componentsWithActions) {
        if (mainPanel != null) {
            return mainPanel;
        }
        mainPanel = new MainPanel(null, componentsWithActions, false, false,
                true, PluginUtils.getResources().getString(
                        "MainPanel.SetButton.text"), layerablesList) {

            /**
                             * 
                             */
            private static final long serialVersionUID = 1L;

            @Override
            public void rightButton() {

                final String path = GUIUtils
                        .getStringValue(componentsWithActions.getComponent(
                                "00", GUIUtils.INPUT, 1)); // 00>row Input>panel
                                                           // 1>element column

                if (path != null && !path.isEmpty()) {

                    final File pathFile = new File(path);
                    if (pathFile.exists()) {
                        PluginUtils.setWorkspacePath(new File(path));
                        JOptionPane.showMessageDialog(
                                this,
                                PluginUtils.getResources().getString(
                                        "SetWorkspacePlugin.Done.message"),
                                PluginUtils.plugInName,
                                JOptionPane.INFORMATION_MESSAGE);
                    } else {
                        final int n = JOptionPane
                                .showConfirmDialog(
                                        this,
                                        PluginUtils
                                                .getResources()
                                                .getString(
                                                        "SetWorkspacePlugin.CreateNewDirectory.message"),
                                        PluginUtils.plugInName,
                                        JOptionPane.YES_NO_OPTION);

                        if (n == JOptionPane.YES_OPTION) {

                            final boolean createdDir = pathFile.mkdirs();

                            if (createdDir == true) {
                                PluginUtils.setWorkspacePath(pathFile);
                            } else {
                                JOptionPane
                                        .showMessageDialog(
                                                this,
                                                PluginUtils
                                                        .getResources()
                                                        .getString(
                                                                "SetWorkspacePlugin.SetWorspace.message"),
                                                PluginUtils.plugInName,
                                                JOptionPane.WARNING_MESSAGE);
                            }

                        }

                    }
                }

            }

            @Override
            public void leftButton() {
            }

            @Override
            public void centerButton() {
            }
        };
        return mainPanel;
    }

    @Override
    public String toString() {
        return PluginUtils.getResources().getString(
                "SetWorkspacePlugin.PlugInName.label");
    }

    private MainPanel mainPanel;
    private final PlugInContext context;
    private final String WORKSPACE = PluginUtils.getResources().getString(
            "SetWorkspacePlugin.workspaceLabel.label");
    private final LayerablesList layerablesList;

    public static final String KEY = SetWorkspacePlugin.class.getName();
    public static final String FILE_CHOOSER_DIRECTORY_KEY = KEY
            + " - FILE CHOOSER DIRECTORY";

}
