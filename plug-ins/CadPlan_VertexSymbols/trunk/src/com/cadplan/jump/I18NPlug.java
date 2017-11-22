/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI 
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * Copyright (C) 2003 Vivid Solutions
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
 * Vivid Solutions
 * Suite #1A
 * 2328 Government Street
 * Victoria BC  V8T 5G5
 * Canada
 *
 * (250)385-6040
 * www.vividsolutions.com
 */
package com.cadplan.jump;

import java.text.MessageFormat;
import java.util.Hashtable;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.StringTokenizer;


import com.vividsolutions.jump.workbench.JUMPWorkbench;

//import org.apache.log4j.Logger;

public final class I18NPlug
{
    boolean debug = false;
    private String language = "en";
    private String country = "AU";
    private Locale locale;
    ResourceBundle rb;
    String pluginName;


    public I18NPlug(String pluginName, String bundle)
    {
        this.pluginName = pluginName;

            if (JUMPWorkbench.I18N_SETLOCALE == "")
            {
               locale = null;
            }
            else
            {
                StringTokenizer st = new StringTokenizer(JUMPWorkbench.I18N_SETLOCALE,"_");
                language = st.nextToken();
                country = "";
                if (st.hasMoreTokens())
                {
                    country = st.nextToken();
                    locale = new Locale(language,country);
                }
                else
                {
                   locale = new Locale(language);
                }

            }
       
        //System.out.println("plugin name: "+pluginName+"  bundle:"+bundle);
        if(locale == null)
        {
            rb = ResourceBundle.getBundle(bundle);
        }
        else
        {
             rb = ResourceBundle.getBundle(bundle, locale);
        }
        
    }



	public String get(String label)
	{
        try
        {
              String text = rb.getString(label);
              return text;
        }
        catch (Exception ex)
        {
             System.out.println("ERROR Get - Missing language resource: "+label);
             return "<"+label+">";
        }

    }
	  



}
