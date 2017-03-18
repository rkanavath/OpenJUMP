package org.openjump.advancedtools.language;

import java.io.File;

import com.vividsolutions.jump.I18N;

/**
 * Lightweight wrapper reusing OJ's I18N class
 * @author ed
 *
 */
public class I18NPlug {
    private static I18N i18n = I18N.getInstance(new File("org/openjump/advancedtools/language/cadtoolbox"));

    public static String getI18N(String key) {
        return i18n.getText(key);
    }

    public static String getMessage(String label, Object[] objects) {
        return i18n.getMessage(label, objects);
    }

}
