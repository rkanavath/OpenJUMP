package org.openjump.ext.viewmanager.style;

import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.ui.renderer.style.Style;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Partial implementation of PStyle defining only the common enabled property
 */
public abstract class AbstractPStyle implements PStyle {

    private boolean enabled;

    abstract public Style getStyle(Layerable layerable);

    @XmlAttribute
    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

}
