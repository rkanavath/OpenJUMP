package org.openjump.ext.viewmanager;

import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.LayerManager;
import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.renderer.style.ColorThemingStyle;
import org.apache.log4j.Logger;
import org.openjump.ext.viewmanager.style.PBasicStyle;
import org.openjump.ext.viewmanager.style.PColorThemingStyle;
import org.openjump.ext.viewmanager.style.PScale;
import org.openjump.ext.viewmanager.style.PStyle;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import java.util.ArrayList;
import java.util.List;

/**
 * A view is a set of styles applied on specified layers.
 */
@XmlRootElement
public class View {

    @XmlAttribute
    String name;

    @XmlElement(name="styledLayer")
    List<StyledLayer> styledLayers;

    public View(){}

    public View(PlugInContext context, boolean selectedOnly) {
        LayerManager layerManager = context.getLayerManager();
        List<Layerable> layerables = layerManager.getLayerables(Layerable.class);
        name = context.getTask().getName();
        for (Layerable layerable : layerables) {
            //System.out.println("    layerable " + layerable);
            if ((!selectedOnly) || (selectedOnly && context.getSelectedLayerables().contains(layerable))) {
                StyledLayer styledLayer = new StyledLayer();
                styledLayer.layer = layerable.getName();
                styledLayer.visible = layerable.isVisible();
                //System.out.println("    styledLayer : " + styledLayer);
                if (layerable.isScaleDependentRenderingEnabled()) {
                    PScale scale = new PScale();
                    scale.scaleDependent = true;
                    scale.minScale = layerable.getMinScale();
                    scale.maxScale = layerable.getMaxScale();
                    styledLayer.scale = scale;
                }
                if (layerable instanceof Layer) {
                    styledLayer.selectable = ((Layer)layerable).isSelectable();
                    styledLayer.editable = ((Layer)layerable).isEditable();
                    styledLayer.setBasicStyle(((Layer)layerable).getBasicStyle());
                    styledLayer.setVertexStyle(((Layer) layerable).getBasicStyle(), ((Layer) layerable).getVertexStyle());
                    styledLayer.setLabelStyle(((Layer)layerable).getLabelStyle());
                    styledLayer.setColorThemingStyle((ColorThemingStyle)((Layer) layerable).getStyle(ColorThemingStyle.class));
                }
                addStyledLayer(styledLayer);
            }
        }
    }

    public void addStyledLayer(StyledLayer styledLayer) {
        if (styledLayers == null) styledLayers = new ArrayList<StyledLayer>();
        styledLayers.add(styledLayer);
    }

    public String toString() {
        return name;
    }

}
