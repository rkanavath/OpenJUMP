package com.geomaticaeambiente.klemgui.ui;

import java.util.ArrayList;
import java.util.List;

import org.openjump.core.rasterimage.RasterImageLayer;

import com.vividsolutions.jump.workbench.JUMPWorkbench;
import com.vividsolutions.jump.workbench.model.Layerable;

/**
 *
 * @author AdL
 */
public class LayerablesList {

    public LayerablesList(Layerable[] layerable) {
        layerables = layerable;
        listeners = new ArrayList<LayersChangedListener>();
    }

    public LayerablesList() {
        listeners = new ArrayList<LayersChangedListener>();
    }

    public void addListener(LayersChangedListener toAdd) {
        final List<RasterImageLayer> rLayers = JUMPWorkbench.getInstance()
                .getContext().getTask().getLayerManager()
                .getLayerables(RasterImageLayer.class);
        boolean found = false;
        for (final LayersChangedListener listener : listeners) {
            if (listener.getClass().equals(toAdd.getClass())) {
                found = true;
                break;
            }
        }
        if (!found) {
            listeners.add(toAdd);
        }

        /*       for (final RasterImageLayer name2 : rLayers) {
                   final RasterImageLayer slayer = name2;
                   if (!rLayers.contains(slayer)) {

                       listeners.remove(slayer);

                   }
               }*/
        /*    if (JUMPWorkbench.getInstance().getContext().getTask()
                    .getLayerManager().getLayerables(RasterImageLayer.class)
                    .isEmpty()) {
                for (int i = 0; i < listeners.size(); i++) {
                    listeners.remove(i);
                    listHasChanged(getLayerables());
                    // getLayerables()[i] = null;
                }

            }*/
    }

    public void listHasChanged(Layerable[] newList) {

        layerables = newList;

        // Notify everybody that may be interested.
        for (final LayersChangedListener listener : listeners) {
            listener.layerablesChanged(layerables);
        }
    }

    public Layerable[] getLayerables() {
        return layerables;
    }

    private Layerable[] layerables = new Layerable[0];
    private final List<LayersChangedListener> listeners;

}
