package com.geomaticaeambiente.klemgui.utils;

import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import java.util.TreeMap;
import javax.swing.filechooser.FileNameExtensionFilter;

/**
 * Class for setting initial parameters used to create GUIs. Initial parameters 
 * are input parameters, other parameter (that are not classified as input or 
 * output parameters) and output parameters.
 * This parameters division is realized to insert in their panel 
 * (input panel, other panel, output panel) the components that will form 
 * the GUIs.
 * Input, other and output parameters are memorized in a TreeMap, so the elements
 * are sorted in the order of insertion. 
 * 
 * @author geomatica
 */
public class InitialData {

    public InitialData() {
        inputHm = new TreeMap();
        outputHm = new TreeMap();
        otherHm = new TreeMap();
        extraHm = new TreeMap();
    }
        
    //Labels
    public void setParam_Labels(String[] labels, String type){
        CombinationComponents cc = new CombinationComponents(labels);
        addToTreeMap(type, cc);
    }
    
    //Personal Table
    public void setParam_PersonalTable(PersonalTable table, FileNameExtensionFilter fileNameExtFilter, String type){
        CombinationComponents cc = new CombinationComponents(table, fileNameExtFilter);
        addToTreeMap(type, cc);
    }
    
    // ActionObject > for Button, radio button (on horizontal row) or checkbox
    public void setParam_Action(ActionObject object, String type){
        CombinationComponents cc = new CombinationComponents(object);
        addToTreeMap(type, cc);
    }
    
    // Label ActionObject > for Button or radio button or checkbox   
    public void setParam_Action(String label, ActionObject object, String type){
        CombinationComponents cc = new CombinationComponents(label, object);
        addToTreeMap(type, cc);
    }
    
    //More action objects 
    public void setParam_MoreActions(ActionObject[] object, String type){
        CombinationComponents cc = new CombinationComponents(object);
        addToTreeMap(type, cc);
    }
    
    // Label + ActionObject > for two or more Button or radio button or checkbox   
    public void setParam_MoreActions(String label, ActionObject[] actionObjects, String type){
        CombinationComponents cc = new CombinationComponents(label, actionObjects);
        addToTreeMap(type, cc);
    }
    
    // Two components > Label JTextField or label JComboBox
    public void setParam_Label_TextBox(String label, Object object, String type){
        CombinationComponents cc = new CombinationComponents(label, object);
        addToTreeMap(type, cc);
    }
    
    //three parameters> label, object (combobox, jtextField), label 
    public void setParam_Label_TextBox_Label(String label, Object object, String label2, String type){ //nel caso in cui ho bisogno di una label un componente ed un altra label (es unita' di misura)
        CombinationComponents cc = new CombinationComponents(label, object, label2);
        addToTreeMap(type, cc);
    }
    
    //three parameters> label, object (combobox, jtextField), button (or radio button or checkbox) 
    public void setParam_Label_TextBox_Button(String label, Object object, ActionObject doAction, String type){ //nel caso in cui ho bisogno di un bottone come terzo elemento
        CombinationComponents cc = new CombinationComponents(label, object, doAction);
        addToTreeMap(type, cc);
    }
    
    //Specific for RasterCombinationComponent
    public void setParam_RasterComb(WorkbenchContext wc, RasterCombinationComponent panel, String type){
        CombinationComponents cc = new CombinationComponents(wc, panel);
        addToTreeMap(type, cc);
    }
    
    //Specific for PersonalRadioButton (vertical radio button list)
    public void setParam_RadioButtons(PersonalRadioButtons personalRadioButton, String type){
        CombinationComponents cc = new CombinationComponents(personalRadioButton);
        addToTreeMap(type, cc);
    }
    
    //specific for personal chart
    public void setParam_ChartPanel(PersonalChart personalChart, String type){
        CombinationComponents cc = new CombinationComponents(personalChart);
        addToTreeMap(type, cc);
    }
    
     //specific for Hydrograph Data components
    public void setParam_CommonHydroData(CommonHydrographData commonHydroData, String type){
        CombinationComponents cc = new CombinationComponents(commonHydroData);
        addToTreeMap(type, cc);
    }
    
    //Create a separator line
    public void setParam_Line(CombinationComponents.CombinationElement element, String type){
        CombinationComponents cc = new CombinationComponents(element);
        addToTreeMap(type, cc);
    }
    
    private void addToTreeMap(String type, CombinationComponents cc){
        
        if(type.equals(GUIUtils.INPUT)){
            
            String id_ = PluginUtils.producerKeyMap(type, countInput);
            inputHm.put(id_ + countInput, cc);
            countInput++;
        }else if(type.equals(GUIUtils.OTHER)){
            
            String id_ = PluginUtils.producerKeyMap(type, countOther);
            
            otherHm.put(id_, cc);
            countOther++;            
        }else if(type.equals(GUIUtils.OUTPUT)){
            
            String id_ = PluginUtils.producerKeyMap(type, countOutput);

            outputHm.put(id_, cc);
            countOutput++;
        } else if(type.equals(GUIUtils.EXTRA)){
            
            String id_ = PluginUtils.producerKeyMap(type, countExtra);

            extraHm.put(id_, cc);
            countExtra++;
        }
        
        
    }
      
        
    public TreeMap getInputParams (){
        return inputHm;
    }
    
    public TreeMap getOutputParams(){
        return outputHm;
    }
    
    public TreeMap getOtherParams(){
        return otherHm;
    }
    
    public TreeMap getExtraParams(){
        return extraHm;
    }
    
    
    private final TreeMap inputHm;
    private final TreeMap outputHm;
    private final TreeMap otherHm;
    private final TreeMap extraHm;
    private int countInput = 0;
    private int countOutput = 0;
    private int countOther = 0;
    private int countExtra = 0;

    
}
