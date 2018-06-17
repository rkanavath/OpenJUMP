/*
 * (C) 2017 Michaël Michaud
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * 
 * For more information, contact:
 *
 * m.michael.michaud@orange.fr
 */

package fr.michaelm.jump.plugin.match;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.apache.log4j.Logger;

import com.vividsolutions.jump.I18N;

public class I18NPlug {
    
    private static final Logger LOG = Logger.getLogger(I18NPlug.class);
    
    // Use the same locale as the main program
    private static final ResourceBundle I18N_RESOURCE =
        ResourceBundle.getBundle("fr/michaelm/jump/plugin/match/matching", new Locale(I18N.getLocale()));
        
    public static String getI18N(String key) {
        try { return I18N_RESOURCE.getString(key); }
        catch (MissingResourceException ex) {
            String[] labelpath = key.split("\\.");
            ex.printStackTrace();
            return labelpath[labelpath.length-1];
        }
        catch (Exception ex) {
            ex.printStackTrace();
            return "";
        }
    }
    
  /**
   * Get a formatted message with argument insertion.
   *
   * @param label with argument insertion : {0}
   * @param objects values to insert in the message
   * @return i18n label
   */
  public static String getMessage(final String label, final Object[] objects) {
    try {
      final MessageFormat mformat = new MessageFormat(I18N_RESOURCE.getString(label));
      return mformat.format(objects);
    } catch (java.util.MissingResourceException e) {
      final String[] labelpath = label.split("\\.");
      LOG.warn(e.getMessage() + " no default value, the resource key is used: "
        + labelpath[labelpath.length - 1]);
      final MessageFormat mformat = new MessageFormat(
        labelpath[labelpath.length - 1]);
      return mformat.format(objects);
    }
  }
    
}