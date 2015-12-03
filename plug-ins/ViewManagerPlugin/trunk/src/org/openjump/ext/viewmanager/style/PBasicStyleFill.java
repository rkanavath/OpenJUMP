package org.openjump.ext.viewmanager.style;

import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import org.apache.log4j.Logger;

import javax.xml.bind.annotation.XmlAttribute;

/**
 * Created by UMichael on 13/06/2015.
 */
public class PBasicStyleFill {

    @XmlAttribute
    boolean enabled = true;

    @XmlAttribute
    String color;

    //@XmlAttribute
    //float pattern = 1;

    PBasicStyleFill() {}

    public PBasicStyleFill setFillStyle(BasicStyle style) {
        enabled = style.isRenderingFill();
        color = ColorUtil.encode(style.getFillColor());
        return this;
    }
}
