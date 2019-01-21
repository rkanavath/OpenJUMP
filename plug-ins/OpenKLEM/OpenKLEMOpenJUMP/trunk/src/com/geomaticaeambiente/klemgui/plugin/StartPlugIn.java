package com.geomaticaeambiente.klemgui.plugin;

import static com.geomaticaeambiente.klemgui.plugin.setting.SetWorkspacePlugin.FILE_CHOOSER_DIRECTORY_KEY;
import static com.vividsolutions.jump.I18N.get;
import static com.vividsolutions.jump.I18N.getMessage;

import java.io.File;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JComponent;

import org.openjump.core.model.TaskEvent;
import org.openjump.core.model.TaskListener;

import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.CategoryEvent;
import com.vividsolutions.jump.workbench.model.FeatureEvent;
import com.vividsolutions.jump.workbench.model.LayerEvent;
import com.vividsolutions.jump.workbench.model.LayerEventType;
import com.vividsolutions.jump.workbench.model.LayerListener;
import com.vividsolutions.jump.workbench.model.LayerManager;
import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheck;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.plugin.FeatureInstaller;
import com.vividsolutions.jump.workbench.ui.plugin.PersistentBlackboardPlugIn;

/**
 *
 * @author Geomatica
 */
public class StartPlugIn extends AbstractPlugIn {

    private String sErrorSeeOutputWindow;

    @Override
    public void initialize(PlugInContext context) throws Exception {

        super.initialize(context);

        FeatureInstaller
                .getInstance()
                .addMainMenuPlugin(
                        this,
                        new String[] { MenuNames.PLUGINS },
                        getName(),
                        false,
                        new ImageIcon(
                                getClass()
                                        .getResource(
                                                "/com/geomaticaeambiente/klemgui/images/OpenKlem.png")),
                        null);

        context.getWorkbenchFrame()
                .getToolBar()
                .addPlugIn(
                        new ImageIcon(
                                getClass()
                                        .getResource(
                                                "/com/geomaticaeambiente/klemgui/images/OpenKlem.png")),
                        this, createEnableCheck(context.getWorkbenchContext()),
                        context.getWorkbenchContext());

    }

    @Override
    public boolean execute(final PlugInContext context) throws Exception {

        // Retrieve previous workspace folder
        final String folderPath = (String) PersistentBlackboardPlugIn.get(
                context.getWorkbenchContext()).get(FILE_CHOOSER_DIRECTORY_KEY);

        if (folderPath != null) {
            final File folder = new File(folderPath);
            PluginUtils.setWorkspacePath(folder);
        }

        final List<Layerable> layerables_l = context.getTask()
                .getLayerManager().getLayerables(Layerable.class);

        if (layerablesList == null) {
            layerablesList = new LayerablesList(
                    layerables_l.toArray(new Layerable[layerables_l.size()]));

        }

        if (context.getWorkbenchFrame().getTaskListeners().isEmpty()) {

            context.getWorkbenchFrame().addTaskListener(new TaskListener() {

                @Override
                public void taskAdded(TaskEvent taskEvent) {
                    addListeners(context);
                }

                @Override
                public void taskLoaded(TaskEvent taskEvent) {
                    addListeners(context);
                }

            });

        }
        addListeners(context);

        try {

            if (dialog == null) {
                dialog = new InitialDialog(context.getWorkbenchFrame(), true,
                        context, layerablesList);
                dialog.setLocationByPlatform(true);
            }

            dialog.setVisible(true);

        } catch (final Exception e) {
            context.getWorkbenchFrame().warnUser(sErrorSeeOutputWindow);
            context.getWorkbenchFrame().getOutputFrame().createNewDocument();
            context.getWorkbenchFrame().getOutputFrame()
                    .addText("Errore: " + e.toString());

            e.printStackTrace(System.out);
        }

        return true;
    }

    public MultiEnableCheck createEnableCheck(
            final WorkbenchContext workbenchContext) {
        final EnableCheckFactory checkFactory = new EnableCheckFactory(
                workbenchContext);
        return new MultiEnableCheck()

        .add(checkFactory.createTaskWindowMustBeActiveCheck());
    }

    public EnableCheck atLeastNLRastersMustExistCheck(
            final WorkbenchContext workbenchContext, final int n) {
        return new EnableCheck() {
            @Override
            public String check(JComponent component) {
                final LayerManager layerManager = workbenchContext
                        .getLayerManager();
                String msg;
                if (n == 1) {
                    msg = get("com.vividsolutions.jump.workbench.plugin.At-least-one-layer-must-exist");
                } else {
                    msg = getMessage(
                            "com.vividsolutions.jump.workbench.plugin.At-least-n-layers-must-exist",
                            n);
                }
                return (layerManager == null || n > layerManager
                        .getRasterImageLayers().size()) ? msg : null;
            }
        };
    }

    private void addListeners(final PlugInContext context) {

        context.getWorkbenchContext().getLayerManager()
                .addLayerListener(new LayerListener() {

                    @Override
                    public void featuresChanged(FeatureEvent e) {

                    }

                    @Override
                    public void layerChanged(LayerEvent e) {

                        final LayerEventType lEvType = e.getType();
                        if (lEvType == LayerEventType.ADDED
                                || lEvType == LayerEventType.REMOVED
                                || lEvType == LayerEventType.APPEARANCE_CHANGED
                                || lEvType == LayerEventType.METADATA_CHANGED) {

                            layerablesList.listHasChanged(PluginUtils
                                    .getLayerables(context));

                        }

                    }

                    @Override
                    public void categoryChanged(CategoryEvent e) {

                    }
                });

    }

    @Override
    public String getName() {
        return PluginUtils.plugInName;
    }

    private InitialDialog dialog;
    private LayerablesList layerablesList;

}
