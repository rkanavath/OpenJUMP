/**
 * @(#)CSComboBox.java
 *
 * Copyright 2011 Edgar Soldin
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
package de.soldin.jump.cts;

import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.JComboBox;

import org.geotools.cs.CoordinateSystem;

/**
 * Class <code>CSComboBox</code> is an ui component that 
 * offers cs's for selection.
 */
public class CSComboBox extends JComboBox implements Cloneable {
	private Object[] keys;

	public CSComboBox(Object[] keys) {
		super(keys);
	}

	/**
	 * Creates an instance and loads cs's from {@link WKTCSLoader}
	 * 
	 * @throws Exception
	 */
	public CSComboBox() throws Exception {
		super();
		Collection css = new WKTCSLoader().values();
		Collection keys = new Vector();
		this.addItem("");
		for (Iterator iter = css.iterator(); iter.hasNext();) {
			CoordinateSystem cs = (CoordinateSystem) iter.next();
			String key = CSSetting.getKey(cs);
			this.addItem(key);
			keys.add(key);
		}

		this.keys = keys.toArray();
	}

	public Object clone() {
		return new CSComboBox(this.keys);
	}
}

