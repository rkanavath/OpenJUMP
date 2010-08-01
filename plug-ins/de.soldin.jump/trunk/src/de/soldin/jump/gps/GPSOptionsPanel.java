/**
 *
 * Copyright 2004 Edgar Soldin
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package de.soldin.jump.gps;

import javax.swing.JPanel;

import com.vividsolutions.jump.workbench.ui.OptionsPanel;

/**
 * Implementation of the <code>OptionsPanel</code> interface.
 * Just for the use of this class in the <code>optionspanel.xml</code>
 * swixML descriptor.
 * 
 * @see com.vividsolutions.jump.workbench.ui.OptionsPanel
 */
public class GPSOptionsPanel 
	extends JPanel
	implements OptionsPanel
	{

	/**
	 * @see com.vividsolutions.jump.workbench.ui.OptionsPanel#validateInput()
	 */
	public String validateInput() {	return null; }

	/**
	 * @see com.vividsolutions.jump.workbench.ui.OptionsPanel#okPressed()
	 */
	public void okPressed() {}

	/**
	 * @see com.vividsolutions.jump.workbench.ui.OptionsPanel#init()
	 */
	public void init() {}

}
