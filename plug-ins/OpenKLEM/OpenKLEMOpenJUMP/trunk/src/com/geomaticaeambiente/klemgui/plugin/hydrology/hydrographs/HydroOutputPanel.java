package com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs;

import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.MainPanel;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import com.geomaticaeambiente.klemgui.utils.AbstractInputKlemPlugin;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.HydroUtils;
import com.geomaticaeambiente.klemgui.utils.InitialData;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.openjump.klem.hydrology.Hydrology.EffectiveRainfall;
import com.geomaticaeambiente.openjump.klem.hydrology.UnitHydrograph;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import it.geomaticaeambiente.klem.Hyetograph;
import it.geomaticaeambiente.klem.TimeInterval;
import javax.swing.JPanel;

/**
 *
 * @author Geomatica
 */
public class HydroOutputPanel extends AbstractInputKlemPlugin {

    public HydroOutputPanel(PlugInContext context, InitialDialog initialDialog,
            Hyetograph hyetograph, EffectiveRainfall effectiveRainfall,
            UnitHydrograph unitHydrograph, TimeInterval.TimeIntervalUnit xUnit,
            HydroUtils.HydroType type, LayerablesList layerablesList) {
        super(context, initialDialog);

        this.context = context;
        this.hyetograph = hyetograph;
        this.effectiveRainfall = effectiveRainfall;
        this.unitHydrograph = unitHydrograph;
        this.xUnit = xUnit;
        this.type = type;
        this.layerablesList = layerablesList;

    }

    @Override
    public InitialData setInitialData() {

        return HydroUtils.getOuputHydrographComponents(
                hyetograph,
                effectiveRainfall,
                unitHydrograph,
                xUnit,
                type);
    }

    @Override
    public ComponentsTreeMap setComponentsActions(ComponentsTreeMap personalTreeMap) {
        return personalTreeMap;
    }

    @Override
    public JPanel buildPluginPanel(ComponentsTreeMap componentsWithActions) {
        if(this.mainPanel != null) {
            return this.mainPanel;
        }
        this.mainPanel = new MainPanel(super.getInitialDialog(), componentsWithActions, false, false, false,
                PluginUtils.getResources().getString("MainPanel.ExecuteButton.text"), layerablesList) {

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
        return this.mainPanel;
    }

    private MainPanel mainPanel;
    private final Hyetograph hyetograph;
    private final EffectiveRainfall effectiveRainfall;
    private final UnitHydrograph unitHydrograph;
    private final TimeInterval.TimeIntervalUnit xUnit;
    private final PlugInContext context;
    private final HydroUtils.HydroType type;
    private final LayerablesList layerablesList;

}
