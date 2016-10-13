package org.openjump.ext.viewmanager.style;

import com.vividsolutions.jump.util.Range;
import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.ui.renderer.style.*;

import javax.xml.bind.annotation.*;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
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
            } else if (entry.getKey() instanceof Integer) {
                themes.add(new Theme(new IntKey((Integer)entry.getKey()),
                        new PBasicStyle((BasicStyle)entry.getValue()),
                        (String)style.getAttributeValueToLabelMap().get(entry.getKey())));
            } else if (entry.getKey() instanceof Long){
                themes.add(new Theme(new LongKey((Long)entry.getKey()),
                        new PBasicStyle((BasicStyle)entry.getValue()),
                        (String)style.getAttributeValueToLabelMap().get(entry.getKey())));
            } else if (entry.getKey() instanceof Double){
                themes.add(new Theme(new DoubleKey((Double)entry.getKey()),
                        new PBasicStyle((BasicStyle)entry.getValue()),
                        (String)style.getAttributeValueToLabelMap().get(entry.getKey())));
            } else if (entry.getKey() instanceof String){
                themes.add(new Theme(new StringKey((String)entry.getKey()),
                        new PBasicStyle((BasicStyle)entry.getValue()),
                        (String)style.getAttributeValueToLabelMap().get(entry.getKey())));
            } else if (entry.getKey() instanceof Timestamp){
                themes.add(new Theme(new TimestampKey((Timestamp)entry.getKey()),
                        new PBasicStyle((BasicStyle)entry.getValue()),
                        (String)style.getAttributeValueToLabelMap().get(entry.getKey())));
            } else if (entry.getKey() instanceof Date){
                themes.add(new Theme(new DateKey((Date)entry.getKey()),
                        new PBasicStyle((BasicStyle)entry.getValue()),
                        (String)style.getAttributeValueToLabelMap().get(entry.getKey())));
            } else if (entry.getKey() instanceof Boolean){
                themes.add(new Theme(new BooleanKey((Boolean)entry.getKey()),
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
        }
        else if (themes != null && themes.size()>0 && themes.iterator().next().key instanceof IntKey) {
            mapStyles = new HashMap<Object, BasicStyle>();
            for (Theme theme : themes) {
                mapStyles.put(((IntKey)theme.key).value, (BasicStyle)theme.style.getStyle(layerable));
                mapLabels.put(((IntKey)theme.key).value, theme.label);
            }
        }
        else if (themes != null && themes.size()>0 && themes.iterator().next().key instanceof LongKey) {
            mapStyles = new HashMap<Object, BasicStyle>();
            for (Theme theme : themes) {
                mapStyles.put(((LongKey)theme.key).value, (BasicStyle)theme.style.getStyle(layerable));
                mapLabels.put(((LongKey)theme.key).value, theme.label);
            }
        }
        else if (themes != null && themes.size()>0 && themes.iterator().next().key instanceof DoubleKey) {
            mapStyles = new HashMap<Object, BasicStyle>();
            for (Theme theme : themes) {
                mapStyles.put(((DoubleKey)theme.key).value, (BasicStyle)theme.style.getStyle(layerable));
                mapLabels.put(((DoubleKey)theme.key).value, theme.label);
            }
        }
        else if (themes != null && themes.size()>0 && themes.iterator().next().key instanceof StringKey) {
            mapStyles = new HashMap<Object, BasicStyle>();
            for (Theme theme : themes) {
                mapStyles.put(((StringKey)theme.key).value, (BasicStyle)theme.style.getStyle(layerable));
                mapLabels.put(((StringKey)theme.key).value, theme.label);
            }
        }
        else if (themes != null && themes.size()>0 && themes.iterator().next().key instanceof TimestampKey) {
            mapStyles = new HashMap<Object, BasicStyle>();
            for (Theme theme : themes) {
                mapStyles.put(((TimestampKey)theme.key).value, (BasicStyle)theme.style.getStyle(layerable));
                mapLabels.put(((TimestampKey)theme.key).value, theme.label);
            }
        }
        else if (themes != null && themes.size()>0 && themes.iterator().next().key instanceof DateKey) {
            mapStyles = new HashMap<Object, BasicStyle>();
            for (Theme theme : themes) {
                mapStyles.put(((DateKey)theme.key).value, (BasicStyle)theme.style.getStyle(layerable));
                mapLabels.put(((DateKey)theme.key).value, theme.label);
            }
        }
        else if (themes != null && themes.size()>0 && themes.iterator().next().key instanceof BooleanKey) {
            mapStyles = new HashMap<Object, BasicStyle>();
            for (Theme theme : themes) {
                mapStyles.put(((BooleanKey)theme.key).value, (BasicStyle)theme.style.getStyle(layerable));
                mapLabels.put(((BooleanKey)theme.key).value, theme.label);
            }
        }
        //else {
        //    mapStyles = new HashMap<Object, BasicStyle>();
        //    for (Theme theme : themes) {
        //        mapStyles.put(((ValueKey)theme.key).value, (BasicStyle)theme.style.getStyle(layerable));
        //        mapLabels.put(((ValueKey)theme.key).value, theme.label);
        //    }
        //}
        colorThemingStyle.setAttributeValueToBasicStyleMap(mapStyles);
        colorThemingStyle.setAttributeValueToLabelMap(mapLabels);
        return colorThemingStyle;
    }


    @XmlRootElement(name="theme")
    public static class Theme implements Comparable<Theme> {

        //@XmlElement
        @XmlElements(value = {
            @XmlElement(name="int-value", type=IntKey.class),
            @XmlElement(name="long-value", type=LongKey.class),
            @XmlElement(name="double-value", type=DoubleKey.class),
            @XmlElement(name="date-value", type=DateKey.class),
            @XmlElement(name="timestamp-value", type=TimestampKey.class),
            @XmlElement(name="boolean-value", type=BooleanKey.class),
            @XmlElement(name="string-value", type=StringKey.class),
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


    // Interface that all types of keys must implement
    public interface Key extends Comparable<Key> {}


    @XmlRootElement(name="int-value")
    public static class IntKey implements Key {

        @XmlElement
        Integer value;

        IntKey() {}

        IntKey(Integer object) {
            this.value = object;
        }

        public int compareTo(Key key) {
            if (value == null) return -1;
            else if (key == null) return 1;
            if (key instanceof IntKey) {
                return value.compareTo(((IntKey) key).value);
            }
            return getClass().getName().compareTo(key.getClass().getName());
        }

    }


    @XmlRootElement(name="long-value")
    public static class LongKey implements Key {

        @XmlElement
        Long value;

        LongKey() {}

        LongKey(Long object) {
            this.value = object;
        }

        public int compareTo(Key key) {
            if (value == null) return -1;
            else if (key == null) return 1;
            if (key instanceof LongKey) {
                return value.compareTo(((LongKey) key).value);
            }
            return getClass().getName().compareTo(key.getClass().getName());
        }

    }


    @XmlRootElement(name="double-value")
    public static class DoubleKey implements Key {

        @XmlElement
        Double value;

        DoubleKey() {}

        DoubleKey(Double object) {
            this.value = object;
        }

        public int compareTo(Key key) {
            if (value == null) return -1;
            else if (key == null) return 1;
            if (key instanceof DoubleKey) {
                return value.compareTo(((DoubleKey) key).value);
            }
            return getClass().getName().compareTo(key.getClass().getName());
        }

    }


    @XmlRootElement(name="string-value")
    public static class StringKey implements Key {

        @XmlElement
        String value;

        StringKey() {}

        StringKey(String object) {
            this.value = object;
        }

        public int compareTo(Key key) {
            if (value == null) return -1;
            else if (key == null) return 1;
            if (key instanceof StringKey) {
                return value.compareTo(((StringKey) key).value);
            }
            return getClass().getName().compareTo(key.getClass().getName());
        }

    }


    @XmlRootElement(name="boolean-value")
    public static class BooleanKey implements Key {

        @XmlElement
        Boolean value;

        BooleanKey() {}

        BooleanKey(Boolean object) {
            this.value = object;
        }

        public int compareTo(Key key) {
            if (value == null) return -1;
            else if (key == null) return 1;
            if (key instanceof BooleanKey) {
                return value.compareTo(((BooleanKey) key).value);
            }
            return getClass().getName().compareTo(key.getClass().getName());
        }

    }


    @XmlRootElement(name="date-value")
    public static class DateKey implements Key {

        @XmlElement
        @XmlJavaTypeAdapter(DateAdapter.class)
        Date value;

        DateKey() {}

        DateKey(Date object) {
            this.value = object;
        }

        public int compareTo(Key key) {
            if (key instanceof DateKey) {
                return value.compareTo(((DateKey) key).value);
            }
            return getClass().getName().compareTo(key.getClass().getName());
        }

    }


    @XmlRootElement(name="timestamp-value")
    public static class TimestampKey implements Key {

        @XmlElement
        @XmlJavaTypeAdapter(TimestampAdapter.class)
        Timestamp value;

        TimestampKey() {}

        TimestampKey(Timestamp object) {
            this.value = object;
        }

        public int compareTo(Key key) {
            if (key instanceof TimestampKey) {
                return value.compareTo(((TimestampKey) key).value);
            }
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



    private static class DateAdapter extends XmlAdapter<String, Date> {

        private final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");

        @Override
        public String marshal(Date v) throws Exception {
            synchronized (dateFormat) {
                return dateFormat.format(v);
            }
        }

        @Override
        public Date unmarshal(String v) throws Exception {
            synchronized (dateFormat) {
                return dateFormat.parse(v);
            }
        }

    }

    private static class TimestampAdapter extends XmlAdapter<String, Timestamp> {

        @Override
        public String marshal(Timestamp v) throws Exception {
            return v.toString();
        }

        @Override
        public Timestamp unmarshal(String v) throws Exception {
            return Timestamp.valueOf(v);
        }

    }


}
