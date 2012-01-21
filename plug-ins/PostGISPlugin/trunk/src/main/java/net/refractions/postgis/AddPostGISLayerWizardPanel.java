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

/**
 * @author Eric Lemesre eric.lemesre@gmail.com
 * Direction          : AGFFinanceConseil
 * Direction Régional : Paris Normandie Centre
 * 
 * 5 nov. 07
 */
package net.refractions.postgis;

import java.awt.BorderLayout;
import java.awt.LayoutManager;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JPanel;

import com.vividsolutions.jump.workbench.ui.InputChangedListener;
import com.vividsolutions.jump.workbench.ui.wizard.WizardPanel;

/**
 * A Wizard Panel for the new Open framework.
 * @author Eric Lemesre
 */
public class AddPostGISLayerWizardPanel extends JPanel implements WizardPanel {
    
	private static final String KEY = AddPostGISLayerWizardPanel.class.getName();

	private static final String PKG_KEY = "net.refractions.postgis";
	private static final String TITLE = I18N.getString(KEY);
	private static final String INSTRUCTIONS = I18N.getString(KEY + ".instructions");

	private PostGISLoadDriverPanel postGISPanel;
	private HashMap properties;

	/**
	 * Constructs a new Wizard Panel for Postgis table
	 */
	public AddPostGISLayerWizardPanel() {
		super(new BorderLayout());
		postGISPanel = new PostGISLoadDriverPanel();
		add(postGISPanel, BorderLayout.CENTER);
	}

	/**
	 * @param layout
	 */
	public AddPostGISLayerWizardPanel(LayoutManager layout) {
		super(layout);
	}

	/**
	 * @param isDoubleBuffered
	 */
	public AddPostGISLayerWizardPanel(boolean isDoubleBuffered) {
		super(isDoubleBuffered);
	}

	/**
	 * @param layout
	 * @param isDoubleBuffered
	 */
	public AddPostGISLayerWizardPanel(LayoutManager layout, boolean isDoubleBuffered) {
		super(layout, isDoubleBuffered);
	}

	public void add(InputChangedListener listener) {
	}

	/**
	 * @see com.vividsolutions.jump.workbench.ui.wizard.WizardPanel#enteredFromLeft(java.util.Map)
	 */
	public void enteredFromLeft(Map arg0) {
		// TODO Populate field by connection information
	}

	/**
	 * @see com.vividsolutions.jump.workbench.ui.wizard.WizardPanel#exitingToRight()
	 */
	public void exitingToRight() throws Exception {
	}

	/**
	 * @see com.vividsolutions.jump.workbench.ui.wizard.WizardPanel#getID()
	 */
	public String getID() {
		return KEY;
	}

	/**
	 * @see com.vividsolutions.jump.workbench.ui.wizard.WizardPanel#getInstructions()
	 */
	public String getInstructions() {
		return INSTRUCTIONS;
	}

	/**
	 * @see com.vividsolutions.jump.workbench.ui.wizard.WizardPanel#getNextID()
	 */
	public String getNextID() {
		return null;
	}

	/**
	 * @see com.vividsolutions.jump.workbench.ui.wizard.WizardPanel#getTitle()
	 */
	public String getTitle() {
		return TITLE;
	}

	/**
	 * @see com.vividsolutions.jump.workbench.ui.wizard.WizardPanel#isInputValid()
	 */
	public boolean isInputValid() {
// add listener before uncomment 
//	    if (postGISPanel.getServer().equals("")) return(false);
//	    if (postGISPanel.getPort().equals("")) return(false);
//	    if (postGISPanel.getDatabase().equals("")) return(false);
//	    if (postGISPanel.getTable().equals("")) return(false);
//	    if (postGISPanel.getUsername().equals("")) return(false);
	    return(true);
	}

	/**
	 * @see com.vividsolutions.jump.workbench.ui.wizard.WizardPanel#remove(com.vividsolutions.jump.workbench.ui.InputChangedListener)
	 */
	public void remove(InputChangedListener arg0) {
	}

	/**
	 * @return the postGISPanel
	 */
	public PostGISCommonDriverPanel getPostGISPanel() {
		return postGISPanel;
	}

	public String getDatasetName() {
		return (String)postGISPanel.getTable();
	}
	
	protected HashMap getProperties() {
		if (properties == null) properties = new HashMap();
		properties.put(PostGISDataSource.SERVER_KEY, postGISPanel.getServer());
		properties.put(PostGISDataSource.PORT_KEY, postGISPanel.getPort());
		properties.put(PostGISDataSource.DATABASE_KEY, postGISPanel.getDatabase());
		properties.put(PostGISDataSource.TABLE_KEY, postGISPanel.getTable());
		if (!postGISPanel.getWhere().trim().equals("")) {
			properties.put(PostGISDataSource.WHERE_KEY, postGISPanel.getWhere());
		} else {
			properties.put(PostGISDataSource.WHERE_KEY, "true");
		}
		properties.put(PostGISDataSource.USERNAME_KEY, postGISPanel.getUsername());
		properties.put(PostGISDataSource.PASSWORD_KEY, postGISPanel.getPassword());
		return(properties);
	}
}
