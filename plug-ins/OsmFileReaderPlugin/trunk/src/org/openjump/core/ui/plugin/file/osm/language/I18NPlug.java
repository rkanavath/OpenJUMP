/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * Copyright (C) 2003 Vivid Solutions
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * For more information, contact:
 *
 * Vivid Solutions Suite #1A 2328 Government Street Victoria BC V8T 5G5 Canada
 *
 * (250)385-6040 www.vividsolutions.com
 */

package org.openjump.core.ui.plugin.file.osm.language;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import com.vividsolutions.jump.I18N;

public class I18NPlug {
    
    // Use the same locale as the main program
    private static final ResourceBundle I18N_RESOURCE =
        ResourceBundle.getBundle("org/openjump/core/ui/plugin/file/osm/language/Osmreader", new Locale(I18N.getLocale()));
        
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
    
}