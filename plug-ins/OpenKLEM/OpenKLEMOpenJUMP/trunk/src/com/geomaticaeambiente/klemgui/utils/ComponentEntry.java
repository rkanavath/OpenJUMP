package com.geomaticaeambiente.klemgui.utils;

import javax.swing.JComponent;

/**
 * Class to set a single component that will be added to ComposeComponents class and
 * than in subPanel.
 * @author Geomatica
 */
public class ComponentEntry {
    
    public ComponentEntry(int id, JComponent component){
        this.id = id;
        this.component = component;
    }
    
    public ComponentEntry(int id, JComponent component, int width){
        this.id = id;
        this.component = component;
        this.width = width;
    }

    public int getId() {
        return id;
    }

    public JComponent getComponent() {
        return component;
    }
    
    public void setComponent(JComponent component){
        this.component = component;
    }
    
    public int getWidth(){
        return width;
    }
    
    private int id;
    private JComponent component;
    private int width = 0;
    
}
