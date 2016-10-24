package org.openjump.ext.viewmanager.style;

import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.ui.renderer.style.ColorThemingStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.LabelStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.Style;
import org.apache.log4j.Logger;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import java.awt.*;

/**
 * Created by UMichael on 13/06/2015.
 */
@XmlRootElement(name="labelStyle")
public class PLabelStyle extends AbstractPStyle {

    final Logger LOG = Logger.getLogger(PLabelStyle.class);

    @XmlAttribute
    String attribute = LabelStyle.FID_COLUMN;

    @XmlAttribute
    boolean hideOverlaps = true;

    @XmlAttribute
    boolean scaling = false;

    @XmlAttribute
    boolean hideAtScale = false;

    @XmlAttribute
    double maxScale;

    @XmlElement
    Font font = new Font();

    @XmlElement
    Outline outline;

    @XmlElement
    Alignment alignment;

    public PLabelStyle() {}

    public PLabelStyle(LabelStyle labelStyle) {
        super();
        setEnabled(labelStyle.isEnabled());
        this.attribute = labelStyle.getAttribute();
        this.hideOverlaps = labelStyle.isHidingOverlappingLabels();
        this.scaling = labelStyle.isScaling();
        this.hideAtScale = labelStyle.getHideAtScale();
        this.maxScale = labelStyle.getScaleToHideAt();

        this.font.name = labelStyle.getFont().getName();
        this.font.bold = (labelStyle.getFont().getStyle() & 1) == 1;
        this.font.italic = (labelStyle.getFont().getStyle() & 2) == 2;
        this.font.height = labelStyle.getHeight();
        this.font.heightAttribute = labelStyle.getHeightAttribute();
        this.font.color = ColorUtil.encode(labelStyle.getColor());

        if (labelStyle.getOutlineShowing()) {
            this.outline = new Outline();
            this.outline.enabled = labelStyle.getOutlineShowing();
            this.outline.color = ColorUtil.encode(labelStyle.getOutlineColor());
            this.outline.width = labelStyle.getOutlineWidth();
            this.outline.opacity = labelStyle.getOutlineColor().getAlpha() / 255f;
        }

        if(labelStyle.getHorizontalAlignment() != LabelStyle.JUSTIFY_CENTER ||
                !labelStyle.getHorizontalPosition().equals(LabelStyle.CENTER) ||
                !labelStyle.getVerticalAlignment().equals(LabelStyle.DEFAULT)) {
            this.alignment = new Alignment();
            this.alignment.angleAttribute = labelStyle.getAngleAttribute();
            this.alignment.horizontalAlignment.alignment = labelStyle.getHorizontalAlignment();
            this.alignment.horizontalAlignment.position = labelStyle.getHorizontalPosition();
            this.alignment.verticalAlignment.alignment = labelStyle.getVerticalAlignment();
        }
    }

    public Style getStyle(Layerable layerable) {
        LabelStyle labelStyle = new LabelStyle();
        //LOG.info("    getLabelStyle");
        if (layerable instanceof Layer) {
            labelStyle.initialize((Layer) layerable);
            labelStyle.setEnabled(isEnabled());
            labelStyle.setAttribute(attribute);
            labelStyle.setHidingOverlappingLabels(hideOverlaps);
            labelStyle.setScaling(scaling);
            labelStyle.setHideAtScale(hideAtScale);
            labelStyle.setScaleToHideAt(maxScale);

            //LOG.info("    getLabelStyle 1");

            int s = 0;
            s += font.bold ? java.awt.Font.BOLD : 0;
            s += font.italic ? java.awt.Font.ITALIC : 0;
            labelStyle.setFont(new java.awt.Font(font.name, s, (int)font.height));
            labelStyle.setHeight(font.height);
            labelStyle.setColor(ColorUtil.decode(font.color));
            labelStyle.setHeightAttribute(font.heightAttribute);

            //LOG.info("    getLabelStyle 2");

            if (outline != null) {
                labelStyle.setOutlineShowing(outline.enabled);
                labelStyle.setOutlineColor(ColorUtil.decode(outline.color), (int)(outline.opacity * 255f));
                labelStyle.setOutlineWidth(outline.width);
            } else labelStyle.setOutlineShowing(false);

            //LOG.info("    getLabelStyle 3");

            if (alignment != null) {
                labelStyle.setHorizontalAlignment(alignment.horizontalAlignment.alignment);
                labelStyle.setHorizontalPosition(alignment.horizontalAlignment.position);
                labelStyle.setVerticalAlignment(alignment.verticalAlignment.alignment);
                labelStyle.setAngleAttribute(alignment.angleAttribute);
            }
        }
        //LOG.info(labelStyle);
        //LOG.info("    " + labelStyle.getAttribute());
        //LOG.info("    " + labelStyle.isEnabled());
        //LOG.info("    " + labelStyle.isScaling());
        //LOG.info("    " + labelStyle.getHeight());
        return labelStyle;
    }


    @XmlRootElement(name="font")
    public static class Font {
        @XmlAttribute
        String name = "Dialog";
        @XmlAttribute
        boolean bold=false;
        @XmlAttribute
        boolean italic=false;
        @XmlAttribute
        String color = "#000000";
        @XmlAttribute
        double height = 12;
        @XmlAttribute
        String heightAttribute = "";
    }

    @XmlRootElement (name="outline")
    public static class Outline {
        @XmlAttribute
        boolean enabled = false;
        @XmlAttribute
        String color = "#FFFFFF";
        @XmlAttribute
        double width = 1;
        @XmlAttribute
        float opacity = 1f;
    }

    @XmlRootElement (name="alignment")
    public static class Alignment {
        @XmlAttribute
        String angleAttribute = "";
        @XmlElement
        HAlignment horizontalAlignment = new HAlignment();
        @XmlElement
        VAlignment verticalAlignment = new VAlignment();
    }

    @XmlRootElement(name="horizontal")
    public static class HAlignment {
        @XmlAttribute
        int alignment = LabelStyle.JUSTIFY_CENTER;
        @XmlAttribute
        String position = LabelStyle.CENTER;
    }

    @XmlRootElement(name="vertical")
    public static class VAlignment {
        @XmlAttribute
        String alignment = LabelStyle.DEFAULT;
    }

}
