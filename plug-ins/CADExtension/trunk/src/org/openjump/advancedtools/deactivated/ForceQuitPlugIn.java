package org.openjump.advancedtools.deactivated;

import javax.swing.ImageIcon;

import org.openjump.advancedtools.icon.IconLoader;
import org.openjump.advancedtools.utils.WorkbenchUtils;

import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;

public class ForceQuitPlugIn extends AbstractPlugIn {

    /**
     * measures angle;
     * 
     * @author Giuseppe Aruta - Sept 1th 2015
     */

    public static ImageIcon ICON = GUIUtil.resize(IconLoader.icon("test.png"),
            20);

    public static final String NAME = "Force Quit";

    @Override
    public boolean execute(PlugInContext context) throws Exception {
        reportNothingToUndoYet(context);

        WorkbenchUtils.quitOpenJUMP();

        return true;
    }

    @Override
    public String getName() {
        return NAME;
    }

    public ImageIcon getIcon() {
        return ICON;
    }

    public static MultiEnableCheck createEnableCheck(
            WorkbenchContext workbenchContext) {
        EnableCheckFactory checkFactory = new EnableCheckFactory(
                workbenchContext);

        return new MultiEnableCheck().add(checkFactory
                .createWindowWithSelectionManagerMustBeActiveCheck());
    }
}
