package org.openjump.ext.viewmanager.style;

import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.ui.renderer.style.*;
import de.latlon.deejump.plugin.style.CircleVertexStyle;
import de.latlon.deejump.plugin.style.CrossVertexStyle;
import de.latlon.deejump.plugin.style.StarVertexStyle;
import de.latlon.deejump.plugin.style.TriangleVertexStyle;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import java.awt.*;

/**
 * Persistent vertex style
 */
@XmlRootElement(name="vertexStyle")
public class PVertexStyle extends AbstractPStyle {

    @XmlAttribute
    float opacity = 0.5f;

    @XmlElement
    PBasicStyleLine line;

    @XmlElement
    PBasicStyleFill fill;

    @XmlElement
    int size;

    @XmlElement
    String shape;

    public PVertexStyle() {}

    public PVertexStyle(BasicStyle basic, VertexStyle vertexStyle) {
        setEnabled(vertexStyle.isEnabled());
        line = new PBasicStyleLine().setLineStyle(basic);
        fill = new PBasicStyleFill().setFillStyle(basic);
        opacity = (float) basic.getAlpha() / 255f;
        size = vertexStyle.getSize();
        shape = vertexStyle.getClass().getSimpleName().replaceAll("VertexStyle","");
    }

    public Style getStyle(Layerable layerable) {
        VertexStyle vStyle;
        if (shape.equals("Circle")) {
            vStyle = new CircleVertexStyle();
        } else if (shape.equals("Cross")) {
            vStyle = new CrossVertexStyle();
        } else if (shape.equals("Ring")) {
            vStyle = new RingVertexStyle();
        } else if (shape.equals("Star")) {
            vStyle = new StarVertexStyle();
        } else if (shape.equals("Triangle")) {
            vStyle = new TriangleVertexStyle();
        } else {
            vStyle = new SquareVertexStyle();
        }
        vStyle.setEnabled(isEnabled());
        vStyle.setFillColor(ColorUtil.decode(fill.color));
        vStyle.setLineColor(ColorUtil.decode(line.color));
        vStyle.setAlpha((int) (opacity * 255));
        vStyle.setSize(size);
        return vStyle;
    }

}
