/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.geomaticaeambiente.klemgui.ui;

import com.vividsolutions.jump.workbench.model.Layer;
import javax.swing.JComboBox;
import org.openjump.core.rasterimage.RasterImageLayer;

/**
 *
 * @author Paola
 */
public class CustomComboBox {
    public class RasterComboBox extends JComboBox{
        public RasterComboBox(RasterImageLayer[] rils){
            removeAllItems();
            addItem("");
            for(int n=0; n<rils.length; n++){
                addItem(rils[n]);
            }
        }
    }
    
    public class LayerComboBox extends JComboBox{
        public LayerComboBox(Layer[] layers){
            removeAllItems();
            addItem("");
            for(int n=0; n<layers.length; n++){
                addItem(layers[n]);
            }
        }
    }
}
