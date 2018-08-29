package com.geomaticaeambiente.klemgui.utils;

import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;
import javax.swing.JComponent;

/**
 * Map that contains all components for the plugin. 
 * @author Geomatica
 */
public class ComponentsTreeMap {

    public ComponentsTreeMap() {
        this.componentEntry_tm = new TreeMap<String, ComponentEntry[]>();
    }

    /**
     * Add element to componentEntry_tm.
     * @param id
     * @param componentEntry Array of ComponentEmtry. Typically derived from ComposeComponents.
     * @throws Exception 
     */
    public void addComponentsEntry(String id, ComponentEntry[] componentEntry) throws Exception {
        componentEntry_tm.put(getId(id), componentEntry);
        
    }

    
    private String getId(String value) {
        if(value.toLowerCase().contains(GUIUtils.INPUT.toLowerCase())){
            String id_ = PluginUtils.producerKeyMap(value, countInput);
            countInput++;
            return id_;
        } else if(value.toLowerCase().contains(GUIUtils.OUTPUT.toLowerCase())){
             String id_ = PluginUtils.producerKeyMap(value, countOutput);
            countOutput++;
            return id_;
        } else if(value.toLowerCase().contains(GUIUtils.OTHER.toLowerCase())){
            String id_ = PluginUtils.producerKeyMap(value, countOther);
            countOther++;
            return id_;
        } else if(value.toLowerCase().contains(GUIUtils.EXTRA.toLowerCase())){
            String id_ = PluginUtils.producerKeyMap(value, countExtra);
            countExtra++;
            return id_;
        }
        
        return null;

    }
    
    /**
     * Get a component from TreeMap<String, ComponentEntry[]>. 
     * @param row, String Id of ComponentEntry[] that contains the desired component.
     * @param type
     * @param positionColumn Position the element on the ComponentEntry[] array and then
     * on the panel row.
     * @return 
     */
    public JComponent getComponent(String row, String type, int positionColumn){
        
        ComponentEntry[] rowComponent = componentEntry_tm.get(row + type);
        JComponent component = rowComponent[positionColumn].getComponent();

        return component;
        
    }
    
    public JComponent[] getLineComponents(String row, String type){
        ComponentEntry[] rowComponent = componentEntry_tm.get(row + type);
        JComponent[] lineComponents = new JComponent[rowComponent.length];
        
        for(int n=0; n<lineComponents.length; n++){
            lineComponents[n] = rowComponent[n].getComponent();
        }
        
        return lineComponents;
    }
    
    public void setComponentInMap (JComponent component, String id, int element){
        
        ComponentEntry[] rowComponent = componentEntry_tm.get(id);
        
        rowComponent[element].setComponent(component);
        
        componentEntry_tm.put(id, rowComponent);
        
    }
    
    
    public List<ComponentEntry[]> getInputComponents(){
        return cicle(GUIUtils.INPUT);
    }
    
    public List<ComponentEntry[]> getOuputComponents(){
        return cicle(GUIUtils.OUTPUT);
    }
    
    public List<ComponentEntry[]> getOtherComponents(){
        return cicle(GUIUtils.OTHER);
    }
    
    public List<ComponentEntry[]> getExtraComponents(){
        return cicle(GUIUtils.EXTRA);
    }
    
    private List<ComponentEntry[]> cicle(String value_){
        
        Set<String> keys = componentEntry_tm.keySet();
        List component = new ArrayList<ComponentEntry[]>();
        
        Iterator iter = keys.iterator();
        while(iter.hasNext()){
            String value = (String) iter.next();
            if(value.toLowerCase().contains(value_.toLowerCase())){
                component.add(componentEntry_tm.get(value));
            }
        }
        
        return  component;
    }
    
    
   
    private final TreeMap<String, ComponentEntry[]> componentEntry_tm;
    private String id;
    private int countInput = 0;
    private int countOutput = 0;
    private int countOther = 0;
    private int countExtra = 0;

}
