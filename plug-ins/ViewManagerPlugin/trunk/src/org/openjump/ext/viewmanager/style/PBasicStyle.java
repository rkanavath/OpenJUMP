package org.openjump.ext.viewmanager.style;

import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.Style;
import org.apache.log4j.Logger;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Created by UMichael on 13/06/2015.
 */
@XmlRootElement (name="basicStyle")
public class PBasicStyle extends AbstractPStyle {

    @XmlAttribute
    float opacity = 0.5f;

    @XmlElement
    PBasicStyleLine line;

    @XmlElement
    PBasicStyleFill fill;

    public PBasicStyle() {}

    public PBasicStyle(BasicStyle style) {
        setEnabled(style.isEnabled());
        line = new PBasicStyleLine().setLineStyle(style);
        fill = new PBasicStyleFill().setFillStyle(style);
        opacity = (float) (style).getAlpha() / 255f;
    }

    public Style getStyle(Layerable layerable) {
        BasicStyle style = new BasicStyle();
        style.setEnabled(isEnabled());
        style.setRenderingFill(fill.enabled);
        style.setRenderingLine(line.enabled);
        style.setFillColor(ColorUtil.decode(fill.color));
        style.setLineColor(ColorUtil.decode(line.color));
        style.setFractionalLineWidth(line.width);
        style.setRenderingLinePattern(line.renderingLinePattern);
        style.setLinePattern(line.pattern);
        style.setAlpha((int) (opacity * 255));
        return style;
    }

}
