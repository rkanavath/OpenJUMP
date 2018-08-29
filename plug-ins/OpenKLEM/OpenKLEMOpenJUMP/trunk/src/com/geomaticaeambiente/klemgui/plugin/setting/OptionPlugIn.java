package com.geomaticaeambiente.klemgui.plugin.setting;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBox;
import javax.swing.JPanel;

import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ActionObject;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.vividsolutions.jump.workbench.JUMPWorkbench;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.plugin.PersistentBlackboardPlugIn;

/**
 *
 * @author Geomatica
 */
public class OptionPlugIn extends AbstractInputKlemPlugin {

    public OptionPlugIn(PlugInContext context, InitialDialog initialDialog,
            LayerablesList layerablesList) {
        super(context, initialDialog);
        this.layerablesList = layerablesList;
    }

    @Override
    public InitialData setInitialData() {

        final InitialData initialData = new InitialData();
        initialData.setParam_Action(new ActionObject(false, LOAD_RASTER),
                GUIUtils.OUTPUT);
        initialData.setParam_Action(new ActionObject(false, APPLY_SYMBOLOGY),
                GUIUtils.OUTPUT);
        // initialData.setParam_Label_TextBox(LOAD_RASTER, PluginUtils
        // .getRasterImageLayers(layerablesList.getLayerables()),
        // GUIUtils.INPUT);
        return initialData;

    }

    private final String LOAD_RASTER = PluginUtils.getResources().getString(
            "OptionPlugin.Load-raster");

    private final String APPLY_SYMBOLOGY = PluginUtils.getResources()
            .getString("OptionPlugin.Load-symbology");

    /** Key for drawing center as point */
    public static final String KEY = OptionPlugIn.class.getName();
    public final static String LOAD_OUTPUT_RASTER = KEY
            + " - LOAD OUTPUT RASTER";
    public final static String APPLY_OUTPUT_SYMBOLOGY = KEY
            + " - LOAD RASTER SYMBOLOGY";

    @Override
    public ComponentsTreeMap setComponentsActions(
            ComponentsTreeMap personalTreeMap) {
        final JCheckBox box = (JCheckBox) personalTreeMap.getComponent("00",
                GUIUtils.OUTPUT, 0);
        final JCheckBox box2 = (JCheckBox) personalTreeMap.getComponent("00",
                GUIUtils.OUTPUT, 0);
        box.setEnabled(true);
        box.setSelected(true);
        box.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                PersistentBlackboardPlugIn.get(
                        JUMPWorkbench.getInstance().getFrame().getContext())
                        .put(LOAD_OUTPUT_RASTER, box.isSelected());
            }
        });

        box2.setEnabled(true);
        box2.setSelected(true);
        box2.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                PersistentBlackboardPlugIn.get(
                        JUMPWorkbench.getInstance().getFrame().getContext())
                        .put(APPLY_OUTPUT_SYMBOLOGY, box.isSelected());
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
                false, PluginUtils.getResources().getString(
                        "MainPanel.SetButton.text"), layerablesList) {

            /**
                             * 
                             */
            private static final long serialVersionUID = 1L;

            @Override
            public void rightButton() {

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
                "OptionPlugin.PlugInName.label");
    }

    public static final boolean loadOutputRaster() {
        return PersistentBlackboardPlugIn.get(
                JUMPWorkbench.getInstance().getFrame().getContext()).get(
                LOAD_OUTPUT_RASTER, false);
    }

    public static final boolean appySymbologyRaster() {
        return PersistentBlackboardPlugIn.get(
                JUMPWorkbench.getInstance().getFrame().getContext()).get(
                APPLY_OUTPUT_SYMBOLOGY, false);
    }

    private MainPanel mainPanel;
    private final LayerablesList layerablesList;

}
