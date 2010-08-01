/**
 * @(#)CSTransform.java	29.06.2004
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
import java.util.Collections;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JPanel;

import com.vividsolutions.jump.workbench.WorkbenchContext;

/**
 * a {@link CSSetListener} and {@link java.awt.event.ItemListener} implementation
 * which itself calls a {@link CSTransformFilter} if a setting is about to happen.
 * <p>
 * the <code>CSSetting</code> also works in batch mode. so if it contains more than
 * one <code>CSSetting</code>, it loops until all of them are set to the new cs.
 * </p>
 */
public class CSTransform implements ItemListener, CSSetListener {
	protected Collection translisteners = new Vector();
	public Collection affected_cs_settings = new Vector();
	final protected WorkbenchContext context;
	protected JComboBox combobox;
	protected JCheckBox checkbox;
	private boolean transform = false;
	private CSSetting local_css = null;

	private CSTransform(WorkbenchContext wbc) {
		this(new Vector(), wbc);
	}

	public CSTransform(CSSetting affected_cs_setting, WorkbenchContext wbc) {
		this(Collections.singleton(affected_cs_setting), wbc);
	}

	public CSTransform(Collection affected_cs_settings, WorkbenchContext wbc) {
		setAffectedCSS(affected_cs_settings);
		this.context = wbc;
	}

	public Collection getAffectedCSS() {
		return affected_cs_settings;
	}

	private void initLocalCSS() {
		if (local_css == null) {
			local_css = new CSSetting();
			local_css.addSetListener(this);
		}

	}

	public void setAffectedCSS(Collection collection) {
		if (collection.size() > 1)
			initLocalCSS();

		affected_cs_settings = collection;
		for (Iterator iter = affected_cs_settings.iterator(); iter.hasNext();) {
			CSSetting css = (CSSetting) iter.next();
			css.addSetListener(this);
		}
	}

	public void setTransform(boolean b) {
		transform = b;
	}

	// catch select cs events
	public void itemStateChanged(ItemEvent e) {
		Object source = e.getSource();
		int change = e.getStateChange();

		if (source instanceof JCheckBox) {
			transform = (change == ItemEvent.SELECTED) ? true : false;
		} else {
			System.err.println("CSTransform::itemStateChanged() wrong ui component, can't set status");
		}
	}

	public void listenEventsFrom(JPanel panel) {

		try {
			checkbox = (JCheckBox) panel.getComponent(2);
			if (checkbox != null) {
				//checkbox.setSelected(transform);
				checkbox.addItemListener(this);
			} else {
				throw new Exception("keine checkbox da");
			}
		} catch (ArrayIndexOutOfBoundsException ae) {
			System.err.println(ae.getMessage());
		} catch (Exception e) {
			System.err.println(e.getMessage());
		}

		//hmm were in batch mode, do some extra stuff
		if (local_css != null) {
			combobox = (JComboBox) panel.getComponent(1);
			if (combobox != null) {
				//combobox.setEnabled(validateList());
				combobox.addItemListener(this);
			} else {
				System.err.println("keine combobox da");
			}

			local_css.listenEventsFrom(panel);
		}

		updateGUI();

	}

	private boolean validateList() {
		for (Iterator iter = affected_cs_settings.iterator(); iter.hasNext();) {
			CSSetting css = (CSSetting) iter.next();
			if (css.cs == null)
				return false;
		}
		return true;
	}

	private boolean boxChecked() {
		return (checkbox != null) ? checkbox.isSelected() : true;
	}

	public void addTransformFilter(CSTransformFilter tr_li) {
		if (!translisteners.contains(tr_li))
			translisteners.add(tr_li);
	}

	public boolean setTry(CSSetting in_css) {
		if (validateList()
			&& local_css != null
			&& local_css.equals(in_css)
			&& local_css.next_cs != null) {
			return localTransformation();
		} else if (
			validateList()
				&& affected_cs_settings.size() == 1
				&& ((CSSetting) affected_cs_settings.iterator().next()).equals(in_css)
				&& boxChecked()) {
			return fireTransformation();
		} else {
			return true;
		}
	}

	public void setDone(CSSetting css) {
		updateGUI();
	}

	public void setFailed(CSSetting css) {
		//System.out.println("CSST::setFailed() "+this+css);
	}

	public boolean localTransformation() {
		boolean success = true;
		// TODO: allemann in einen Undoable und schwupps
		for (Iterator iter = affected_cs_settings.iterator(); iter.hasNext();) {
			CSSetting element = (CSSetting) iter.next();
			success = success && element.setCS(local_css.next_cs);
		}
		return success;
	}

	private void updateGUI() {
		boolean enabled = validateList();
		if (combobox != null && local_css != null)
			combobox.setEnabled(enabled);
		if (checkbox != null) {
			checkbox.setEnabled(enabled);
			checkbox.setSelected(enabled);
			transform = enabled;
		}
	}

	public boolean fireTransformation() {
		// all transaction get stuffed into transall
		Collection transall = new Vector();
		UndoableSetGeometry transformation = new UndoableSetGeometry("");

		for (Iterator it = translisteners.iterator();
			affected_cs_settings.size() > 0 && it.hasNext();
			) {
			CSTransformFilter listener = (CSTransformFilter) it.next();
			transformation.addAll(listener.transform(affected_cs_settings));
		}

		transformation.execute();
		context.getLayerManager().getUndoableEditReceiver().receive(transformation);
		return true;
	}

	public String toString() {
		return super.toString()
			+ "\t"
			+ affected_cs_settings
			+ "\t"
			+ ((affected_cs_settings == null) ? 0 : affected_cs_settings.size());
	}

}
