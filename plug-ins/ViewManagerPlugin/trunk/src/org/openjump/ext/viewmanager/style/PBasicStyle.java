package org.openjump.ext.viewmanager.style;

import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.Style;
import com.vividsolutions.jump.workbench.ui.renderer.style.VertexStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.XBasicStyle;
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

    @XmlElement
    PVertexStyle vertexStyle;

    public PBasicStyle() {}

    public PBasicStyle(BasicStyle style) {
        setEnabled(style.isEnabled());
        line = new PBasicStyleLine().setLineStyle(style);
        fill = new PBasicStyleFill().setFillStyle(style);
        opacity = (float) (style).getAlpha() / 255f;
        if (style instanceof XBasicStyle) {
            vertexStyle = new PVertexStyle(style, ((XBasicStyle)style).getVertexStyle());
        }
    }

    public Style getStyle(Layerable layerable) {
        XBasicStyle style = new XBasicStyle();
        style.setEnabled(isEnabled());
        style.setRenderingFill(fill.enabled);
        style.setRenderingLine(line.enabled);
        style.setFillColor(ColorUtil.decode(fill.color));
        style.setLineColor(ColorUtil.decode(line.color));
        style.setFractionalLineWidth(line.width);
        style.setRenderingLinePattern(line.renderingLinePattern);
        style.setLinePattern(line.pattern);
        style.setAlpha((int) (opacity * 255));
        if (vertexStyle != null) {
            style.setVertexStyle((VertexStyle)vertexStyle.getStyle(layerable));
        }
        return style;
    }

}
