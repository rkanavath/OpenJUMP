package com.geomaticaeambiente.klemgui.ui;

import java.util.ArrayList;
import java.util.List;

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
