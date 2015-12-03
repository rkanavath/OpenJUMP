package org.openjump.ext.viewmanager.style;

import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.ui.renderer.style.Style;

import javax.xml.bind.annotation.XmlRootElement;

/**
 * Persistent style
 */
@XmlRootElement(name="style")
public interface PStyle {

    Style getStyle(Layerable layerable);

    boolean isEnabled();

    void setEnabled(boolean enabled);

}
