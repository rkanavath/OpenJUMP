package com.cadplan.jump;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheck;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.LayerViewPanel;
import com.vividsolutions.jump.workbench.ui.MenuNames;

/**
 * User: geoff Date: 1/08/2007 Time: 08:24:50 Copyright 2005 Geoffrey G Roy.
 */
public class VertexNotePlugin extends AbstractPlugIn {

    private I18NPlug iPlug;

    @Override
    public void initialize(PlugInContext context) throws Exception {
        iPlug = new I18NPlug("VertexSymbols", "language.VertexSymbolsPlugin");

        final EnableCheckFactory check = new EnableCheckFactory(
                context.getWorkbenchContext());
        final EnableCheck scheck = check
                .createAtLeastNFeaturesMustBeSelectedCheck(1);
        final MultiEnableCheck mcheck = new MultiEnableCheck();

        mcheck.add(check.createAtLeastNLayersMustExistCheck(1));
        mcheck.add(check.createAtLeastNLayersMustBeEditableCheck(1));
        // mcheck.add(check.createAtLeastNFeaturesMustBeSelectedCheck(1));

        final String menuName = MenuNames.PLUGINS; // iPlug.get("VertexSymbols.MenuName");
        context.getFeatureInstaller().addMainMenuPlugin(this,
                new String[] {menuName, MenuNames.STYLE}, getName(), false, getIcon(), mcheck);
        context.getWorkbenchFrame()
                .getToolBar()
                .addPlugIn(getIcon(), this, mcheck,
                        context.getWorkbenchContext());
        context.getFeatureInstaller().addPopupMenuPlugin(
                LayerViewPanel.popupMenu(), this, getName(), false, getIcon(),
                scheck);

        // final String menuItem = "Vertex Note"; //
        // iPlug.get("VertexSymbols.MenuItem");
        // context.getFeatureInstaller().addMainMenuItem(this,
        // new String[] { menuName }, menuItem, false, null, scheck);

        // final String dirName = context.getWorkbenchContext().getWorkbench()
        // .getPlugInManager().getPlugInDirectory().getAbsolutePath();
        // final IconLoader loader = new IconLoader(dirName, "VertexSymbols");

        // Image image = loader.loadImage("noteicon.gif");
        // ImageIcon icon = new ImageIcon(image);
        // System.out.println("Note Resource path: "+this.getClass().getResource("/Resources/noteicon.gif"));
        // final ImageIcon icon = new ImageIcon(this.getClass().getResource(
        // "/Resources/noteicon.gif"));

        // context.getFeatureInstaller()
        // .addPopupMenuItem(LayerViewPanel.popupMenu(), this,menuItem,
        // false, icon, scheck);

        // final WorkbenchToolBar toolBar = context.getWorkbenchFrame()
        // .getToolBar();

        // final JButton button = toolBar.addPlugIn(icon, this, mcheck,
        // context.getWorkbenchContext());
        // JButton button = toolBar.addPlugIn(new
        // ImageIcon(image),this,check.createAtLeastNFeaturesMustBeSelectedCheck(1),context.getWorkbenchContext());

    }

    public Icon getIcon() {
        return new ImageIcon(getClass().getResource("/Resources/noteicon.gif"));
    }

    @Override
    public String getName() {
        iPlug = new I18NPlug("VertexSymbols", "language.VertexSymbolsPlugin");
        return iPlug.get("VertexNote.MenuItem");
    }

    @Override
    public boolean execute(PlugInContext context) throws Exception {
        final VertexNote vn = new VertexNote(context, iPlug);
        return true;
    }
}
