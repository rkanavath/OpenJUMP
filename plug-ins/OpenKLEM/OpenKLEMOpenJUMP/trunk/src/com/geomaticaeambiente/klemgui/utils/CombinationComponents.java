package com.geomaticaeambiente.klemgui.utils;

import com.geomaticaeambiente.klemgui.ui.CommonHydroPanel;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.PersonalChartPanelComponent;
import com.geomaticaeambiente.klemgui.ui.PersonalRasterCombPanel;
import com.geomaticaeambiente.klemgui.ui.PersonalTableComponents;
import com.geomaticaeambiente.klemgui.ui.RadioButtonsPanel;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import java.util.ArrayList;
import java.util.List;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.text.BadLocationException;
import org.openjump.core.rasterimage.RasterImageLayer;

/**
 * Class for creating an object formed by a label and and object.
 * The class is used to connect a parameter to a label. This union of elements is 
 * useful in SetInitialData class to set initial parameters (input parameters, 
 * other parameters and output parameters) for the creation of plugin GUI
 * 
 * @author geomatica
 */
public class CombinationComponents {
    
//    public CombinationComponents(String label){
//        this.label = label;
//        type = CombinationType.SINGLE_STRING;
//    } 
    
    public CombinationComponents(PersonalTable table, FileNameExtensionFilter fileNameExtFilter){
        this.table = table;
        this.fileNameExtFilter = fileNameExtFilter;
        type = CombinationType.SINGLE_TABLE;
    }
    
    public CombinationComponents(WorkbenchContext wc, RasterCombinationComponent panel){
        this.wc = wc;
        this.panel = panel;
        type = CombinationType.COMB_RASTER_PANEL;
    }
    
    
    public CombinationComponents(String label, Object obj){
        this.label = label;
        this.object = obj; 
        type = CombinationType.STRING_OBJECTS;
    } 
    
    public CombinationComponents(String[] labels){
        this.labels = labels;        
        type = CombinationType.LABELS;
    } 
    
    public CombinationComponents(ActionObject actionObject){
        this.actionObject = actionObject;        
        type = CombinationType.ACTION_OBJECT;
    } 
    
    public CombinationComponents(ActionObject[] actionObjects){
        this.actionObjects = actionObjects;        
        type = CombinationType.MORE_ACTION;
    }
    
    public CombinationComponents(String label, ActionObject actionObject){
        this.label = label;
        this.actionObject = actionObject;        
        type = CombinationType.ACTION_OBJECT;
    }
    
     public CombinationComponents(String label, Object object, String label2){
        this.label = label;    
        this.object = object;
        this.label2 = label2;
        type = CombinationType.LABELS_OBJS_LABELS;
    } 
    
     //more radioButton in vertical list
     public CombinationComponents(PersonalRadioButtons personalRadioButtons){
        this.personalRadioButtons = personalRadioButtons;
        type = CombinationType.VERT_RADIOBUTTON;
    } 
     
    public CombinationComponents(PersonalChart chartPanel){
        this.chartPanel = chartPanel;
        type = CombinationType.CHART;
    }
     
     public CombinationComponents(CommonHydrographData hydroData){
        this.hydroData = hydroData;
        type = CombinationType.COMMON_HYDRO_DATA;
    }
    
    
    public CombinationComponents(String label, Object object, ActionObject actionObject){
        this.label = label;
        this.object = object;
        this.actionObject = actionObject;
        type = CombinationType.STRING_OBJECT_ACTION;
    }
    
    public CombinationComponents(String label, ActionObject[] actionObjects){
        this.label = label;
        
        this.actionObjects = actionObjects;
        type = CombinationType.MORE_ACTION;
    }
     
    public CombinationComponents(CombinationElement element){
        this.element = element;       
        type = CombinationType.LINE;
    }
     
    public ComponentEntry[] createComponents(CombinationComponents components_Mod) throws BadLocationException{
        
        List<ComponentEntry> componentEntry_list = new ArrayList<ComponentEntry>();
        
        CombinationComponents.CombinationType type = components_Mod.getType();
        int count = 1;
        
        switch (type){           
                      
              case COMB_RASTER_PANEL:{
                PersonalRasterCombPanel prcp = new PersonalRasterCombPanel(
                        wc, panel.getMainComponent());
                componentEntry_list.add(new ComponentEntry(count, prcp));
                break;
                
            }
              
            //------new or modified------------  
              
            case STRING_OBJECTS:{
                JLabel label_ = new JLabel(label);
                componentEntry_list.add(new ComponentEntry(count, label_));
                count++;
//                for(int n=0; n<objects.length; n++){
                JComponent component = GUIUtils.getComponent(object);
                componentEntry_list.add(new ComponentEntry(count, component));
                count++;                    
//                }
                break;
            }
            
            case LABELS:{
                for(int n=0; n<labels.length; n++){
                    JLabel label = new JLabel(labels[n]);
                    componentEntry_list.add(new ComponentEntry(count, label));
                    count++;                    
                }
                break;
            }
            
            case SINGLE_TABLE:{
                PersonalTableComponents ptc = new PersonalTableComponents(table, fileNameExtFilter);
                componentEntry_list.add(new ComponentEntry(count, ptc));
                break;
                
            }
            
            case VERT_RADIOBUTTON:{
                RadioButtonsPanel rbp = new RadioButtonsPanel(personalRadioButtons.getLabels());
                componentEntry_list.add(new ComponentEntry(count, rbp));
                break;
                
            }
            
            case ACTION_OBJECT:{
                
                if(label!=null){
                    JLabel label_ = new JLabel(label);
                    componentEntry_list.add(new ComponentEntry(count, label_));
                    count++;
                }
                
               if(actionObject.getType() == ActionObject.ActionObjectType.BUTTON){
           JButton pb = new JButton(actionObject.getLabel());
            componentEntry_list.add(new ComponentEntry(count, pb));
            count++;    
        } else if(actionObject.getType() == ActionObject.ActionObjectType.RADIOBUTTON){
            JRadioButton[] radioButtons = new JRadioButton[actionObject.getLabels().length];
            ButtonGroup buttonGroup = new ButtonGroup();
            for(int r=0; r<actionObject.getLabels().length; r++){
                radioButtons[r] = new JRadioButton(actionObject.getLabels()[r]);
                componentEntry_list.add(new ComponentEntry(count, radioButtons[r]));
                buttonGroup.add(radioButtons[r]);
                count++;
            }     
            radioButtons[0].setSelected(false);

        } else if (actionObject.getType() == ActionObject.ActionObjectType.CHECKBOX){
            JCheckBox jCheckBox = new JCheckBox(actionObject.getLabel());
            jCheckBox.setSelected(actionObject.getvalue());
            componentEntry_list.add(new ComponentEntry(count, jCheckBox));
            count++;
        }
                break;
            }
            
                       
            case LABELS_OBJS_LABELS:{
                
                JLabel first_label = new JLabel(label);
                componentEntry_list.add(new ComponentEntry(count, first_label));
                count++;
                
                
                JComponent component = GUIUtils.getComponent(object);
                componentEntry_list.add(new ComponentEntry(count, component));
                count++;                   

                JLabel second_label = new JLabel(label2);
                componentEntry_list.add(new ComponentEntry(count, second_label));
                count++;
                
                break;
            }
            
            case STRING_OBJECT_ACTION:{
                
                JLabel label_ = new JLabel(label);
                componentEntry_list.add(new ComponentEntry(count, label_));
                count++;
                
                JComponent component = GUIUtils.getComponent(object);
                componentEntry_list.add(new ComponentEntry(count, component));
                count++;
                
                if(actionObject.getType() == ActionObject.ActionObjectType.BUTTON){
                    JButton pb = new JButton(actionObject.getLabel());
                    componentEntry_list.add(new ComponentEntry(count, pb));
                    count++;    
                 } else if(actionObject.getType() == ActionObject.ActionObjectType.RADIOBUTTON){
                     JRadioButton[] radioButtons = new JRadioButton[actionObject.getLabels().length];
                     ButtonGroup buttonGroup = new ButtonGroup();
                     for(int r=0; r<actionObject.getLabels().length; r++){
                         radioButtons[r] = new JRadioButton(actionObject.getLabels()[r]);
                         componentEntry_list.add(new ComponentEntry(count, radioButtons[r]));
                         buttonGroup.add(radioButtons[r]);
                         count++;
                     }     
                     radioButtons[0].setSelected(false);

                 } else if (actionObject.getType() == ActionObject.ActionObjectType.CHECKBOX){
                     JCheckBox jCheckBox = new JCheckBox(actionObject.getLabel());
                     jCheckBox.setSelected(actionObject.getvalue());
                     componentEntry_list.add(new ComponentEntry(count, jCheckBox));
                     count++;
                 }
                break;
            }
                
            case CHART :{
                PersonalChartPanelComponent pcpc = new PersonalChartPanelComponent(chartPanel);
                componentEntry_list.add(new ComponentEntry(count, pcpc));
                break;
            }
            
            case COMMON_HYDRO_DATA:{
                CommonHydroPanel pchd = new CommonHydroPanel(hydroData);
                componentEntry_list.add(new ComponentEntry(count, pchd));
                break;
            }
            
             case LINE:{
                javax.swing.JSeparator separator = new javax.swing.JSeparator();
                componentEntry_list.add(new ComponentEntry(count, separator));
                break;
            }
             case MORE_ACTION:{
                 
                if(label != null){ 
                    JLabel label_ = new JLabel(label);
                    componentEntry_list.add(new ComponentEntry(count, label_));
                    count++;
                }
                
                for(int n=0; n<actionObjects.length; n++){
                
                    if(actionObjects[n].getType() == ActionObject.ActionObjectType.BUTTON){
                        JButton pb = new JButton(actionObjects[n].getLabel());
                        componentEntry_list.add(new ComponentEntry(count, pb));
                        count++;    
                     } else if(actionObjects[n].getType() == ActionObject.ActionObjectType.RADIOBUTTON){
                         JRadioButton[] radioButtons = new JRadioButton[actionObjects[n].getLabels().length];
                         ButtonGroup buttonGroup = new ButtonGroup();
                         for(int r=0; r<actionObjects[n].getLabels().length; r++){
                             radioButtons[r] = new JRadioButton(actionObjects[n].getLabels()[r]);
                             componentEntry_list.add(new ComponentEntry(count, radioButtons[r]));
                             buttonGroup.add(radioButtons[r]);
                             count++;
                         }     
                         radioButtons[0].setSelected(false);

                     } else if (actionObjects[n].getType() == ActionObject.ActionObjectType.CHECKBOX){
                         JCheckBox jCheckBox = new JCheckBox(actionObjects[n].getLabel());
                         jCheckBox.setSelected(actionObjects[n].getvalue());
                         componentEntry_list.add(new ComponentEntry(count, jCheckBox));
                         count++;
                     }
                
                }
                break;
                 
             }
             
        }
        
        return componentEntry_list.toArray(new ComponentEntry[componentEntry_list.size()]);
        
    }   
    
    
    
//    private List<ComponentEntry> setActionButton( List<ComponentEntry> componentEntry_list){
//        if(actionObject.getType() == ActionObject.ActionObjectType.BUTTON){
//           JButton pb = new JButton(actionObject.getLabel());
//            componentEntry_list.add(new ComponentEntry(count, pb));
//            count++;    
//        } else if(actionObject.getType() == ActionObject.ActionObjectType.RADIOBUTTON){
//            JRadioButton[] radioButtons = new JRadioButton[actionObject.getLabels().length];
//            ButtonGroup buttonGroup = new ButtonGroup();
//            for(int r=0; r<actionObject.getLabels().length; r++){
//                radioButtons[r] = new JRadioButton(actionObject.getLabels()[r]);
//                componentEntry_list.add(new ComponentEntry(count, radioButtons[r]));
//                buttonGroup.add(radioButtons[r]);
//                count++;
//            }     
//            radioButtons[0].setSelected(false);
//
//        } else if (actionObject.getType() == ActionObject.ActionObjectType.CHECKBOX){
//            JCheckBox jCheckBox = new JCheckBox(actionObject.getLabel());
//            jCheckBox.setSelected(actionObject.getvalue());
//            componentEntry_list.add(new ComponentEntry(count, jCheckBox));
//            count++;
//        }
//        
//        return componentEntry_list;
//    }
    
    
    public String getLabel() {
        return label;
    }

    public Object getObject() {
        return object;
    }
    
    public Object[] getObjects() {
        return objects;
    }

//    public Object getObjectConnected() {
//        return label2;
//    }

    public String getLabel2() {
        return label2;
    }

    public ActionObject getActionObject() {
        return actionObject;
    }

    public String getLabelAction() {
        return labelAction;
    }

    public CombinationType getType() {
        return type;
    }
    
    public boolean getSelectedInformation() {
        return booleanValue;
    }
    
    public boolean[] getMoreSelectedInformation() {
        return booleanValues;
    }
    
    public String[] getLabels() {
        return labels;
    }
    
    public CommonHydrographData getHydrodata(){
        return hydroData;
    }
    
    
    private WorkbenchContext wc;
    private String label = null;
    private String label2 = null;
    private Object object = null;
    private Object[] objects = null;
    private String[] labels2 = null;
    private ActionObject actionObject = null;
    private ActionObject[] actionObjects = null;
    private String labelAction = null;
    private PersonalTable table;
    private FileNameExtensionFilter fileNameExtFilter;
    private RasterCombinationComponent panel;
    private PersonalRadioButtons personalRadioButtons= null;
    private boolean booleanValue = false;
    private boolean[] booleanValues;
    private String[] labels;
    private PersonalChart chartPanel;
    private CommonHydrographData hydroData;
     
    
    public static enum CombinationElement{LINE};
    private CombinationElement element;
    
    public static enum CombinationType{ 
             STRING_OBJECT_ACTION, SINGLE_TABLE, COMB_RASTER_PANEL, STRING_OBJECTS, ACTION_OBJECT,
             LABELS, LABELS_OBJS_LABELS, VERT_RADIOBUTTON, CHART, COMMON_HYDRO_DATA, LINE,MORE_ACTION};
    private CombinationType type;
}
