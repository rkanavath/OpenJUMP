package com.geomaticaeambiente.klemgui.utils;

/**
 *
 * @author Geomatica
 */
public class PersonalRadioButtons {

    public PersonalRadioButtons(String...labels) {
        this.labels = labels;
    }
    
    public String[] getLabels(){
        return labels;
    }
    
    private String[] labels;
    
}
