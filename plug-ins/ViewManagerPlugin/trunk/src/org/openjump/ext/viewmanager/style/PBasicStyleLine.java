package org.openjump.ext.viewmanager.style;

import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import org.apache.log4j.Logger;

import javax.xml.bind.annotation.XmlAttribute;

/**
 * Created by UMichael on 13/06/2015.
 */
public class PBasicStyleLine {

    @XmlAttribute
    boolean enabled = true;

    @XmlAttribute
    String color;

    @XmlAttribute
    float width = 1f;

    @XmlAttribute
    boolean renderingLinePattern = false;

    @XmlAttribute
    String pattern;

    PBasicStyleLine() {}

    public PBasicStyleLine setLineStyle(BasicStyle style) {
        enabled = style.isRenderingLine();
        color = ColorUtil.encode(style.getLineColor());
        width = style.getLineWidth();
        renderingLinePattern = style.isRenderingLinePattern();
        pattern = style.getLinePattern();
        return this;
    }
}
