package com.geomaticaeambiente.klemgui.ui;

import com.vividsolutions.jump.workbench.model.Layerable;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author AdL
 */
public class LayerablesList {

    public LayerablesList(Layerable[] layerable) {
        this.layerables = layerable;
        this.listeners = new ArrayList<LayersChangedListener>();
    }
    
    public LayerablesList() {        
        this.listeners = new ArrayList<LayersChangedListener>();
    }
    
    public void addListener(LayersChangedListener toAdd) {
        
        boolean found = false;
        for(LayersChangedListener listener : listeners) {
            if(listener.getClass().equals(toAdd.getClass())) {
                found = true;
                break;
            }
        }      
        if(!found) {
           listeners.add(toAdd);
        }
        
    }
    
    public void listHasChanged(Layerable[] newList) {

        this.layerables = newList;
        
        // Notify everybody that may be interested.
        for (LayersChangedListener listener : listeners) {
            listener.layerablesChanged(layerables);
        }
    }

    public Layerable[] getLayerables() {
        return layerables;
    }
    
    private Layerable[] layerables = new Layerable[0];
    private final List<LayersChangedListener> listeners;
    
}
