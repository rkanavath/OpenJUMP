package com.geomaticaeambiente.klemgui.utils;

import com.geomaticaeambiente.klemgui.ui.PersonalComponentAbstract;
import javax.swing.JDialog;

/**
 *
 * @author Geomatica
 */
public class RasterCombinationComponent extends PersonalComponentAbstract {

    public RasterCombinationComponent(JDialog mainComponent) {
        this.mainComponent = mainComponent;
    }

    public JDialog getMainComponent(){
        return mainComponent;
    }
    
    private JDialog mainComponent;
    
//    @Override
//    public Object getPersonalComponent() {
//        return  rasterImageLayers;
//    }
//
//    @Override
//    public PersonalComponentType getPersonalComponentType() {
//        return PersonalComponentType.COMBINE_RASTER;
//    }
//    
//    private final RasterImageLayer[] rasterImageLayers = null;
}
