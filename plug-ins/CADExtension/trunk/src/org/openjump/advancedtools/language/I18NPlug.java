/* 
 * Kosmo - Sistema Abierto de Información Geográfica
 * Kosmo - Open Geographical Information System
 *
 * http://www.saig.es
 * (C) 2011, SAIG S.L.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation;
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *
 * For more information, contact:
 * 
 * Sistemas Abiertos de Información Geográfica, S.L.
 * Avnda. República Argentina, 28
 * Edificio Domocenter Planta 2ª Oficina 7
 * C.P.: 41930 - Bormujos (Sevilla)
 * España / Spain
 *
 * Teléfono / Phone Number
 * +34 954 788876
 * 
 * Correo electrónico / Email
 * info@saig.es
 *
 */

package org.openjump.advancedtools.language;

/**
 * @author Giuseppe Aruta, adapted from Kosmo org.saig.jump.lang.I18N class
 * @since OpenJUMP 1.10
 */

import java.text.MessageFormat;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import com.vividsolutions.jump.I18N;

public class I18NPlug {
    private static final ResourceBundle I18N_RESOURCE = ResourceBundle
            .getBundle("org/openjump/advancedtools/language/cadtoolbox",
                    new Locale(I18N.getLocale()));

    private final static Logger LOGGER = Logger.getLogger(I18N.class);
    // Default language file location
    public final static String DEFAULT_LANG_FILE = "org/openjump/advancedtools/language/cadtoolbox";
    public final static String NO_STRING_FOUND = "<No string found>";

    public static String getI18N(String key) {
        try {
            return I18N_RESOURCE.getString(key);
        } catch (MissingResourceException ex) {
            String[] labelpath = key.split("\\.");
            ex.printStackTrace();
            return labelpath[(labelpath.length - 1)];
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return "";
    }

    /**
     * Process text with the locale 'kosmo_<locale>.properties' file
     * 
     * @param label
     *            with argument insertion : {0}
     * @param objects
     * @return i18n label
     */
    public static String getMessage(String label, Object[] objects) {

        try {
            String translation = StringUtils.replace(
                    I18N_RESOURCE.getString(label), "'", "''");
            MessageFormat mf = new MessageFormat(translation,
                    I18N_RESOURCE.getLocale());
            return mf.format(objects);
        } catch (java.util.MissingResourceException e) {

            try {
                String default_translation = StringUtils.replace(ResourceBundle
                        .getBundle(DEFAULT_LANG_FILE).getString(label), "'",
                        "''");
                ;
                LOGGER.debug(e.getMessage() + " default_value:"
                        + default_translation);
                MessageFormat mf = new MessageFormat(default_translation,
                        I18N_RESOURCE.getLocale());
                return mf.format(objects);
            } catch (java.util.MissingResourceException ex) {
                LOGGER.warn(NO_STRING_FOUND + " " + label);
                return NO_STRING_FOUND;
            }
        }
    }

}
