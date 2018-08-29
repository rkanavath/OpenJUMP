package com.geomaticaeambiente.klemgui.utils;

import com.geomaticaeambiente.klemgui.ui.PersonalComponentAbstract;

/**
 *
 * @author Geomatica
 */
public class ActionObject extends PersonalComponentAbstract{

    // constructor for button
    public ActionObject(String label){
        this.label = label;
        type = ActionObjectType.BUTTON;
    }
    
    // constructor for radio button
    public ActionObject(String[] labels){
        this.labels = labels;
        type = ActionObjectType.RADIOBUTTON;
    }
    
    // constructor for checkbox
    public ActionObject(boolean value, String label){
        this.label = label;
        this.value = value;
        type = ActionObjectType.CHECKBOX;
    }
    
    
    public String getLabel(){
        return label;
    }
    
    public String[] getLabels(){
        return labels;
    }
    
    public ActionObjectType getType(){
        return type;
    }
    
    public boolean getvalue(){
        return value;
    }
//    @Override
//    public Object getPersonalComponent() {
//        return null;
//    }
//
//    @Override
//    public PersonalComponentType getPersonalComponentType() {
//        return PersonalComponentType.ACTION_OBJECT;
//    }
//    
    private String label;
    private String[] labels;
    public enum ActionObjectType {BUTTON, RADIOBUTTON, CHECKBOX};
    private ActionObjectType type;
    private boolean value = false;
}
