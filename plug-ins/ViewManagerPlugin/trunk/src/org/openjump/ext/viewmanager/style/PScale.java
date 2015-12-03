package org.openjump.ext.viewmanager.style;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Created by UMichael on 21/06/2015.
 */
@XmlRootElement (name="scale")
public class PScale {

    @XmlAttribute
    public boolean scaleDependent = false;

    @XmlAttribute
    public Double minScale = Double.valueOf(1000000);

    @XmlAttribute
    public Double maxScale = Double.valueOf(1);

}
