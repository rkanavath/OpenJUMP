/*
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
 */
package net.refractions.postgis;

import java.awt.*;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.text.PlainDocument;
import com.vividsolutions.jump.workbench.ui.*;

/**
 * Panel dedicated to a PostGIS table query for loading.
 * Extends PostGISCommonDriverPanel to be able to add a "where" clause
 * @author mmichaud
 */
public class PostGISLoadDriverPanel extends PostGISCommonDriverPanel {
    
    private static final String WHERE = I18N.getString(KEY + ".where");
    
    private static PlainDocument whereDoc = null;
  
    JTextArea whereField;
    
    public PostGISLoadDriverPanel() {
        super();
        if (serverDoc == null) {
            whereDoc = new PlainDocument();
        }
        JLabel theLabel;
        GridBagConstraints c = new GridBagConstraints();
        Insets labelInsets = new Insets( 2, 6, 2, 2 );
        Insets fieldsInsets = new Insets( 2, 2, 2, 6 );
        
        // forth row of fields
        c.gridy = 3;
        c.gridx = 0;
        c.insets = labelInsets;
        c.anchor = GridBagConstraints.EAST;
        theLabel = new JLabel( WHERE + ":" );
        gbLayout.setConstraints( theLabel, c );
        this.add( theLabel );
        c.gridx = 1;
        c.gridwidth = 3;
        c.insets = fieldsInsets;
        c.anchor = GridBagConstraints.WEST;
        whereField = new JTextArea( whereDoc,"", 4, 40 );
        whereField.setText("");
        gbLayout.setConstraints( whereField, c );
        this.add( whereField );
    }

    public void setCache(DriverPanelCache cache) {
        super.setCache(cache);
        if(cache.get(PostGISDataSource.WHERE_KEY) != null ) {
            whereField.setText((String)cache.get(PostGISDataSource.WHERE_KEY));
        }
    }
    
    public void putCache( DriverPanelCache cache ) {
        super.putCache(cache);
        cache.put(PostGISDataSource.WHERE_KEY, whereField.getText());
    }

    public String getWhere() {
        return whereField.getText();
    }

}
