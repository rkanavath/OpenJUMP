package org.openjump.ext.viewmanager.style;

import com.vividsolutions.jump.util.Range;
import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.ui.renderer.style.*;

import javax.xml.bind.annotation.*;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.math.BigDecimal;
import java.util.*;

/**
 * Created by UMichael on 13/06/2015.
 */
@XmlRootElement(name="colorThemingStyle")
public class PColorThemingStyle extends AbstractPStyle {

    @XmlAttribute
    boolean vertexStyleEnabled = false;

    @XmlAttribute
    String attribute = "";

    @XmlElement
    PBasicStyle defaultStyle = new PBasicStyle();

    @XmlElement (name="theme")
    Set<Theme> themes = new TreeSet<Theme>();

    public PColorThemingStyle() {}

    public PColorThemingStyle(ColorThemingStyle style) {
        super();
        setEnabled(style.isEnabled());
        attribute = style.getAttributeName();
        vertexStyleEnabled = style.isVertexStyleEnabled();
        defaultStyle = new PBasicStyle(style.getDefaultStyle());
        for (Map.Entry entry : (Set<Map.Entry>)style.getAttributeValueToBasicStyleMap().entrySet()) {
            if (entry.getKey() instanceof com.vividsolutions.jump.util.Range) {
                themes.add(new Theme(new RangeKey(
                        ((Range)entry.getKey()).getMin(),
                        ((Range)entry.getKey()).getMax()),
                        new PBasicStyle((BasicStyle)entry.getValue()),
                        (String)style.getAttributeValueToLabelMap().get(entry.getKey())));
            } else {
                themes.add(new Theme(new ValueKey(entry.getKey()),
                        new PBasicStyle((BasicStyle)entry.getValue()),
                        (String)style.getAttributeValueToLabelMap().get(entry.getKey())));
            }
        }
    }

    public Style getStyle(Layerable layerable) {
        ColorThemingStyle colorThemingStyle = new ColorThemingStyle();
        colorThemingStyle.setEnabled(isEnabled());
        colorThemingStyle.setVertexStyleEnabled(vertexStyleEnabled);
        colorThemingStyle.setAttributeName(attribute);
        colorThemingStyle.setDefaultStyle((BasicStyle)defaultStyle.getStyle(layerable));
        Map<Object,BasicStyle> mapStyles = new HashMap<Object, BasicStyle>();
        Map<Object,String> mapLabels = new HashMap<Object, String>();
        if (themes != null && themes.size()>0 && themes.iterator().next().key instanceof Range) {
            mapStyles = new com.vividsolutions.jump.util.Range.RangeTreeMap<Object, BasicStyle>();
            for (Theme theme : themes) {
                Object min = ((RangeKey)theme.key).getMin();
                Object max = ((RangeKey)theme.key).getMax();
                com.vividsolutions.jump.util.Range range =
                        new com.vividsolutions.jump.util.Range(min, true, max, false);
                mapStyles.put(range, (BasicStyle)theme.style.getStyle(layerable));
                mapLabels.put(range, theme.label);
            }
        } else {
            mapStyles = new HashMap<Object, BasicStyle>();
            for (Theme theme : themes) {
                mapStyles.put(((ValueKey)theme.key).value, (BasicStyle)theme.style.getStyle(layerable));
                mapLabels.put(((ValueKey)theme.key).value, theme.label);
            }
        }
        colorThemingStyle.setAttributeValueToBasicStyleMap(mapStyles);
        colorThemingStyle.setAttributeValueToLabelMap(mapLabels);
        return colorThemingStyle;
    }


    @XmlRootElement(name="theme")
    public static class Theme implements Comparable<Theme> {

        //@XmlElement
        @XmlElements(value = {
            @XmlElement(name="value", type=ValueKey.class),
            @XmlElement(name="range", type=RangeKey.class),
        })
        Key key;

        @XmlElement
        PBasicStyle style;

        @XmlElement
        String label;

        Theme() {}

        Theme(Key key, PBasicStyle style, String label) {
            this.key = key;
            this.style = style;
            this.label = label;
        }

        public int compareTo(Theme theme) {
            return key.compareTo(theme.key);
        }
    }

    @XmlRootElement(name="value")
    public static class ValueKey implements Key {

        @XmlElement
        String value;

        ValueKey() {}

        ValueKey(Object object) {
            this.value = object.toString();
        }

        public int compareTo(Key key) {
            if (key instanceof ValueKey)
                return this.value.compareTo(((ValueKey)key).value);
            return getClass().getName().compareTo(key.getClass().getName());
        }

    }

    @XmlRootElement(name="range")
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class RangeKey extends com.vividsolutions.jump.util.Range implements Key {

        RangeKey() {}

        RangeKey(Object min, Object max) {
            super(min, true, max, false);
        }

        public int compareTo(Key key) {
            if (key instanceof Range)
                return RANGE_COMPARATOR.compare(this, (RangeKey)key);
            return getClass().getName().compareTo(key.getClass().getName());
        }

    }

    public interface Key extends Comparable<Key> {}


}
