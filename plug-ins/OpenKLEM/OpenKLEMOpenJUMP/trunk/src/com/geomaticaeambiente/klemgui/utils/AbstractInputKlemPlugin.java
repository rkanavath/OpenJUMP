package com.geomaticaeambiente.klemgui.utils;

import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import java.util.Collection;
import java.util.Iterator;
import java.util.TreeMap;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

/**
 * Abstract class to create the extension's plugin classes.
 * The method setInitialData() implements the initial parameter for building the
 plugin's GUI. Initial parameter contain input, other and output parameters.
 The method setComponetsTreeMap create a ComponentsTreeMap put each input output and other 
 parameters in a TreeMap creating a ComponentsTreeMap.
 The method setComponentsAction set the single action for the components. 
 To extract the components from ComponentsTreeMap, use the method getComponentEntry(String identif)
 of ComponentsTreeMap, in identif set panel name plus row on which there is the component
 and the position on the row that it take up.
 The method buildPluginPanel(ComponentsTreeMap componentsWithActions)) implements the MainPanel
 on which will be added the panels for input and output parameters.
 The method  getTabPluginComponents() implements InitialData with the MainPanel
 for returning a JScrollComponent to add to InitialDialog.
 * @author Geomatica
 */
public abstract class AbstractInputKlemPlugin implements PluginGUIComponentsInterface{

    public AbstractInputKlemPlugin(PlugInContext context, InitialDialog initialDialog) {
        this.initialDialog = initialDialog;
    }
        
    
    /**
     * Implements the initial parameter for building the plugin's GUI.
     * @return 
     */
    public abstract InitialData setInitialData() throws Exception;
    
    @Override
    public ComponentsTreeMap setComponetsTreeMap(InitialData initialtData) throws Exception{
        
        allComponents = new ComponentsTreeMap();//ho tutti i parametri di imput suddivisi per componenti (una riga un componente)
        
        //ho tutti i parametri di input suddivisi per componenti (una riga un componente)
        allComponents = getComponentsTreeMapBase(allComponents, initialtData.getInputParams(), GUIUtils.INPUT);
        allComponents = getComponentsTreeMapBase(allComponents, initialtData.getOutputParams(), GUIUtils.OUTPUT);
        allComponents = getComponentsTreeMapBase(allComponents, initialtData.getOtherParams(), GUIUtils.OTHER);
        allComponents = getComponentsTreeMapBase(allComponents, initialtData.getExtraParams(), GUIUtils.EXTRA);
        
        return allComponents;
        
    }
    
    public abstract ComponentsTreeMap setComponentsActions(ComponentsTreeMap personalTreeMap);
    
    
    /**
     * Implements the MainPanel on which will be added the panels for input and output parameters.
     * @param componentsWithActions     * 
     * @return 
     */
    public abstract JPanel buildPluginPanel(ComponentsTreeMap componentsWithActions);
    

    
    /**
     * Implements all component with the MainPanel for returning a 
     * JScrollComponent to add to InitialDialog
     * @return 
     * @throws java.lang.Exception 
     */
    @Override
    public JScrollPane getTabPluginComponents() throws Exception{
        InitialData idm = setInitialData();
        ComponentsTreeMap componentsMap = setComponetsTreeMap(idm);
        ComponentsTreeMap componentsMapPlusActions = setComponentsActions(componentsMap);
        
        JPanel panel = buildPluginPanel(componentsMapPlusActions);
        
        JScrollPane sp = new JScrollPane(panel);
        return sp;
    }    
   
    
    public ComponentsTreeMap getComponentsTreeMapBase(ComponentsTreeMap personalTreeMap, TreeMap treeMap, String key ) throws Exception{
        
        Collection combinationComponentColl_ = treeMap.values();
        Iterator iter = combinationComponentColl_.iterator();
        
        while (iter.hasNext()){
            CombinationComponents ccm = (CombinationComponents) iter.next();
            ComponentEntry[] componentEntries = ccm.createComponents(ccm);
            personalTreeMap.addComponentsEntry(key, componentEntries);
        }
        return personalTreeMap;
        
    } 
    
    public ComponentsTreeMap getComponentsTreeMapBase(TreeMap treeMap, String key ) throws Exception{
        
        Collection combinationComponentColl_ = treeMap.values();
        Iterator iter = combinationComponentColl_.iterator();
        
        while (iter.hasNext()){
            CombinationComponents ccm = (CombinationComponents) iter.next();
            ComponentEntry[] componentEntries = ccm.createComponents(ccm);
            allComponents.addComponentsEntry(key, componentEntries);
        }
        return allComponents;
        
    } 
    
    
    public InitialDialog getInitialDialog(){
        return initialDialog;
    }    
    
    private ComponentsTreeMap allComponents;
    private final InitialDialog initialDialog;
}
