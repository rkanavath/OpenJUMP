package org.openjump.ext.setattributes;

import com.vividsolutions.jump.feature.Feature;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Set "value" for attribute "name" if "pre-requisite"
 */
@XmlRootElement (name="Attribute")
public class SetAttribute {

    public static String IS_NULL = "isNull";
    public static String IS_NULL_OR_EMPTY = "isNullOrEmpty";

    @XmlAttribute (name="name", required=true)
    String name;

    @XmlAttribute (name="value", required=false)
    String value;

    @XmlAttribute
    String prerequisite;

    public SetAttribute() {}

    public String getName() {
        return name;
    }

    public String getValue() {
        return value;
    }

    public String getPrerequisite() {
        return prerequisite;
    }

    boolean checkPrerequisite(Object obj) {
        if (prerequisite != null) {
            if (prerequisite.equals(IS_NULL)) {
                return obj == null;
            }
            if (prerequisite.equals(IS_NULL_OR_EMPTY)) {
                return obj == null || obj.toString().trim().equals("");
            }
            if (prerequisite.startsWith("=")) {
                String ref = prerequisite.substring(1);
                return obj!=null && obj.toString().equals(ref);
            }
            if (prerequisite.startsWith("#")) {
                String ref = prerequisite.substring(1);
                return obj!=null && !obj.toString().equals(ref);
            }
            return true;
        } else return true;
    }
}
