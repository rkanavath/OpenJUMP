/**
 * @(#)CSSetting.java	29.06.2004
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
package de.soldin.jump.cts;

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;
import java.util.Vector;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.geotools.cs.CoordinateSystem;

import com.vividsolutions.jump.workbench.model.Layer;

/**
 * The class <code>CSSetting</code> represents the assigned cs
 * for a layer. It preserves the last cs for undo purposes and
 * contains the next cs if a setting process is on the go.
 * <p>
 * <code>CSSetting</code> is an implementation of {@link java.awt.event.ItemListener 
 * AWT's Itemlistener} in order to be attached to user interface components.
 * </p>
 */
public class CSSetting implements ItemListener {
	public Layer layer;
	private CoordinateSystem prev_prev_cs;
	public CoordinateSystem prev_cs;
	public CoordinateSystem cs;
	public CoordinateSystem next_cs;
	protected Object key, prev_key;
	private Collection listeners = new Vector();
	private JComboBox cbox;

	public CSSetting() {}

	public CSSetting(Layer layer) {
		this.layer = layer;
	}

	// catch select cs events
	public void itemStateChanged(ItemEvent e) {
		//System.out.println("CSS:"+e);

		if (!(e.getSource() instanceof JComboBox))
			return;

		if (e.getStateChange() == ItemEvent.DESELECTED) {
			prev_key = e.getItem();
		} else if (e.getStateChange() == ItemEvent.SELECTED) {
			key = e.getItem();
			setCS(key);
		}
	}

	public boolean setCS(Object key) {
		try {

			CoordinateSystem in_cs;
			try {
				in_cs = new WKTCSLoader().get((String) key);
			} catch (ClassCastException e) {
				try {
					in_cs = (CoordinateSystem) key;
				} catch (ClassCastException e1) {
					throw new Exception("CSS:setCS() argument is not string nor cs.");
				}
			}

			if ((in_cs == null && cs == null) | (in_cs != null && in_cs.equals(cs)))
				return true;

			/*if(!fromGUI && cbox!=null){
				cbox.setSelectedItem(getKey(in_cs));
				return true;
			}*/
			next_cs = in_cs;
			if (fireSetTryEvent()) {
				prev_prev_cs = prev_cs;
				prev_cs = cs;
				cs = next_cs;
				next_cs = null;
				fireSetDoneEvent();
				initCbox();
				return true;
			} else {
				fireSetFailedEvent();
				return false;
			}
		} catch (Exception e1) {
			e1.printStackTrace();
			return false;
		}
	}

	public void rollback() {
		next_cs = cs;
		cs = prev_cs;
		prev_cs = prev_prev_cs;
		prev_prev_cs = null;
		initCbox();
	}

	public void addSetListener(CSSetListener set_li) {
		if (!listeners.contains(set_li))
			listeners.add(set_li);
	}

	protected boolean fireSetTryEvent() {
		boolean success = true;
		for (Iterator it = listeners.iterator(); it.hasNext();) {
			CSSetListener listener = (CSSetListener) it.next();
			success = success && listener.setTry(this);
		}
		return success;
	}

	protected void fireSetDoneEvent() {
		for (Iterator it = listeners.iterator(); it.hasNext();) {
			CSSetListener listener = (CSSetListener) it.next();
			listener.setDone(this);
		}
	}

	protected void fireSetFailedEvent() {
		for (Iterator it = listeners.iterator(); it.hasNext();) {
			CSSetListener listener = (CSSetListener) it.next();
			listener.setFailed(this);
		}
	}

	public void listenItemEventsFrom(JComboBox cbox) {
		this.cbox = cbox;
		initCbox();
		cbox.addItemListener(this);
	}

	private void initCbox() {
		if (cbox != null && !cbox.getSelectedItem().equals(getKey(cs))) {
			for (int i = 0; i < cbox.getItemCount(); i++) {
				Object key = cbox.getItemAt(i);
				if (key.equals(getKey(cs))) {
					cbox.setSelectedIndex(i);
					return;
				}
			}
		}
	}

	public void listenEventsFrom(JPanel panel) {
		JLabel label = (JLabel) panel.getComponent(0);
		label.setText(layer == null ? "no layer set" : layer.getName());

		CSComboBox cobox = (CSComboBox) panel.getComponent(1);
		this.listenItemEventsFrom(cobox);
	}

	public String toString() {
		return layer
			+ "\t"
			+ (prev_cs == null ? "null" : getKey(prev_cs))
			+ "\t"
			+ (cs == null ? "null" : getKey(cs))
			+ "\t"
			+ (next_cs == null ? "null" : getKey(next_cs));
	}

	public static String getKey(CoordinateSystem a_cs) {
		if (a_cs == null)
			return new String();
		return a_cs.getName(Locale.getDefault());
	}

	public static CoordinateSystem getCS(String key) {
		try {
			return new WKTCSLoader().get((String) key);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
}
