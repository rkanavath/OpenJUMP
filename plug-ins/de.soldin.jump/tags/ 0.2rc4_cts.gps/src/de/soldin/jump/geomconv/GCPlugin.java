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
package de.soldin.jump.geomconv;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.EnableCheck;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.SelectionManagerProxy;

import de.soldin.jump.*;

/**
 * A jump plugin for the conversion of geometries into one another.
 * Currently supported are single geometries of the type
 * <ul>
 * <li>{@link com.vividsolutions.jts.geom.LineString}</li>
 * <li>{@link com.vividsolutions.jts.geom.LinearRing}</li>
 * <li>{@link com.vividsolutions.jts.geom.MultiPoint}</li>
 * <li>{@link com.vividsolutions.jts.geom.Point}</li>
 * <li>{@link com.vividsolutions.jts.geom.Polygon}</li>
 * </ul>
 * 
 * Currently only one whole geometry of one feature can be converted
 * at one time.
 * 
 * @see com.vividsolutions.jump.workbench.plugin.PlugIn
 */
public class GCPlugin implements PlugIn, ActionListener {

	private String NAME = "Convert Selected Geometry to";
	private WorkbenchContext wbc;
	private GeometryFactory factory = new GeometryFactory();
	private String lastcmd = "";

	public void initialize(PlugInContext context) throws Exception {
		this.wbc = context.getWorkbenchContext();
		// get possible geometry keys
		List types = new ArrayList(getCreatableGeoms().keySet());
		Collections.sort(types);
		// create menu items
		for (Iterator iter = types.iterator(); iter.hasNext();) {
			String string = (String) iter.next();
			context.getFeatureInstaller().addMainMenuItem(
				this,
				new String[] { "Edit", getName()},
				string,
				false,
				null,
				createEnableCheck(context));
		}
		// get submenu menu item for processing
		JMenu menu = context.getFeatureInstaller().menuBarMenu("Edit");
		// add listener for status of submenu item
		addMenuListener(menu, createEnableCheck(context));
		// register action listener with the menu items
		for (int i = 0; i < menu.getItemCount(); i++) {
			if (menu.getItem(i) == null)
				continue;
			if (menu.getItem(i).getText().equals(getName())) {
				JMenu submenu;
				try {
					submenu = (JMenu) menu.getItem(i);
				} catch (ClassCastException cexc) {
					context.getWorkbenchContext().getErrorHandler().handleThrowable(
						new Exception(
							"Menuitem '" + getName() + "' is an unexpected object type."));
					return;
				}
				// add listener for selection of submenu items
				for (int j = 0; j < submenu.getItemCount(); j++) {
					submenu.getItem(j).addActionListener(this);
				}
				break;
			}
		}
	}

	public void actionPerformed(ActionEvent e) {
		if (e != null) {
			lastcmd = e.getActionCommand();
		}
	}

	public boolean execute(PlugInContext context) throws Exception {
		context
			.getLayerManager()
			.getUndoableEditReceiver()
			.reportNothingToUndoYet();

		Method method = (Method) getCreatableGeoms().get(lastcmd);
		Class[] cparams = method.getParameterTypes();
		Collection feats = getFeatures();
		UndoableSetGeometry action =
			new UndoableSetGeometry((Layer) getLayers().iterator().next(), getName());
		//		try {
		if (feats.size() < 1) {
			context.getLayerViewPanel().getContext().warnUser(
			"No feature selected!");
			return false;
		} else// if (feats.size() == 1) {
		{
			for (Iterator i = feats.iterator(); i.hasNext();)
			{
				try {
					//Feature feat = (Feature) feats.iterator().next();
					Feature feat = (Feature) i.next();
					Geometry geom_src = action.getGeom(feat);
					Geometry geom_new = null;
					if (cparams.length == 1) {
						if (cparams[0]
									.getName()
									.equals("com.vividsolutions.jts.geom.Coordinate")) {
							//System.out.println("eine koordinate");
							if (geom_src.getCoordinates().length == 1) {
								geom_new =
									(Geometry) method.invoke(
											factory,
											new Object[] { geom_src.getCoordinates()[0] });
							} else {
								context.getLayerViewPanel().getContext().warnUser(
								"Too many points selected. Only one point needed.");
								continue;
								//return false;
							}
						} else if (
								cparams[0].isArray()
								&& cparams[0].getComponentType().getName().equals(
								"com.vividsolutions.jts.geom.Coordinate")) {
							//System.out.println("mehrere koordinaten");
							geom_new =
								(Geometry) method.invoke(
										factory,
										new Object[] { action.getGeom(feat).getCoordinates()});
						} else {
							context.getLayerViewPanel().getContext().warnUser(
									"This conversion '" + lastcmd + "' is not implemented yet!");
							continue;
							//return false;
						}
					} else if (cparams.length > 1) {
						if (cparams[0]
									.getName()
									.equals("com.vividsolutions.jts.geom.LinearRing")) {
							//System.out.println("linearring");
							if (!(geom_src
									instanceof com.vividsolutions.jts.geom.LinearRing)) {
								Method method2 = (Method) getCreatableGeoms().get("LinearRing");
								geom_src =
									(Geometry) method2.invoke(
											factory,
											new Object[] { geom_src.getCoordinates()});
							}
							
							if (geom_src instanceof com.vividsolutions.jts.geom.LinearRing) {
								Object[] params = new Object[cparams.length];
								params[0] = geom_src;
								geom_new = (Geometry) method.invoke(factory, params);
							} else {
								context.getLayerViewPanel().getContext().warnUser(
										"Selected features geometry is not a '"
										+ cparams[0].getName()
										+ "'.");
								continue;
								//							return false;
							}
						} else {
							context.getLayerViewPanel().getContext().warnUser(
									"This conversion '" + lastcmd + "' is not implemented yet!");
							continue;
							//						return false;
						}
					}
					if (geom_new != null) {
						action.setGeom(feat, geom_new);
						action.execute();
						wbc.getLayerManager().getUndoableEditReceiver().receive(action);
					}
					//			} else {
					//				context.getLayerViewPanel().getContext().warnUser(
					//					"Conversion of more than one feature not supported yet.");
				} catch (InvocationTargetException ie) {
					if (ie.getCause() != null)
						context.getLayerViewPanel().getContext().warnUser(
								ie.getCause().getLocalizedMessage());
				}
			}
		}
		//		} catch (InvocationTargetException ie) {
		//			if (ie.getCause() != null)
		//				context.getLayerViewPanel().getContext().warnUser(
		//					ie.getCause().getLocalizedMessage());
		//		}
		return true;
	}

	public String getName() {
		return NAME;
	}

	public EnableCheck createEnableCheck(PlugInContext context) {
        EnableCheckFactory checkFactory = new EnableCheckFactory(context.getWorkbenchContext());
		MultiEnableCheck checker = new MultiEnableCheck().add(checkFactory.createWindowWithLayerViewPanelMustBeActiveCheck());
		// is plugin active?
		checker.add(new EnableCheck() {
			public String check(JComponent component) {
				return null;
			}
		});

		MultiEnableCheck checker2 = new MultiEnableCheck();
		checker2.add(checkFactory.createExactlyNLayersMustHaveSelectedItemsCheck(1));
//		// is a layer or feature selected?
//		checker2.add(new EnableCheck() {
//			public String check(JComponent component) {
//				boolean featureok = (getFeatures().size() == 1);
//
//				return (featureok)
//					? null
//					: "Exactly one feature in one layer must be selected for conversion.";
//			}
//		});
		// is selected (features) layer editable?
		checker2.add(new EnableCheck() {
			public String check(JComponent component) {
				boolean editable = false;
				Collection layers = getLayers();
				if (layers == null)
					return "No layers in list";
				editable = ((Layer) layers.iterator().next()).isEditable();
				return (editable) ? null : "Layer with selection is not editable.";
			}
		});
		checker.add(checker2);

		return checker;
	}

	public EnableCheck createEnableCheckMenu(PlugInContext context) {
		EnableCheck check = new EnableCheck() {
			public String check(JComponent component) {
				Component[] comps = component.getComponents();
				for (int i = 0; i < comps.length; i++) {
					if (comps[i].isEnabled())
						return null;
				}
				return "";
			}
		};
		return check;
	}

	public void addMenuListener(
		final JMenu menu,
		final EnableCheck enableCheck) {

		Component[] comps = menu.getMenuComponents();
		JMenuItem item = null;
		for (int i = 0; i < comps.length; i++) {
			try {
				JMenuItem pitem = (JMenuItem) comps[i];
				if (pitem.getText().equals(getName()))
					item = (JMenuItem) comps[i];
			} catch (ClassCastException e) {
				continue;
			}
		}
		if (item == null) {
			wbc.getErrorHandler().handleThrowable(
				new Exception(
					"Could not find menu item '"
						+ getName()
						+ "' in menu '"
						+ menu.getName()
						+ "'."));
			return;
		}
		final JMenuItem item2 = item;

		menu.addMenuListener(new MenuListener() {
			public void menuSelected(MenuEvent e) {
				String errorMessage = null;
				try {
					errorMessage = enableCheck.check(menu);
				} catch (Exception ex) {
					wbc.getWorkbench().getFrame().handleThrowable(ex);
				}
				if (errorMessage != null) {
					item2.setEnabled(false);
					item2.setToolTipText(errorMessage);
					return;
				}
				item2.setEnabled(true);
				item2.setToolTipText(null);
			}
			public void menuDeselected(MenuEvent e) {
			}
			public void menuCanceled(MenuEvent e) {
			}
		});
	}

	private Collection getFeatures() {
		return (
			(SelectionManagerProxy) wbc
				.getWorkbench()
				.getFrame()
				.getActiveInternalFrame())
			.getSelectionManager()
			.getFeatureSelection()
			.getFeaturesWithSelectedItems();
	}

	private Collection getLayers() {
		// all layers with selected items (parts of geometries)
		Collection layers =
			((SelectionManagerProxy) wbc
				.getWorkbench()
				.getFrame()
				.getActiveInternalFrame())
				.getSelectionManager()
				.getFeatureSelection()
				.getLayersWithSelectedItems();
		return layers.isEmpty()
			? Arrays.asList(wbc.getLayerNamePanel().getSelectedLayers())
			: layers;
	}

	private Map getCreatableGeoms() throws ClassNotFoundException {
		Map geoms = new Hashtable();
		Class cfactory = factory.getClass();
		Method[] methods = cfactory.getMethods();
		for (int i = 0; i < methods.length; i++) {
			Method method = (Method) methods[i];
			if (!method.getName().startsWith("create")) {
				continue;
			}
			// check if parameters match
			Class[] cparams = methods[i].getParameterTypes();
			boolean wrong = false;
			for (int j = 0; j < cparams.length; j++) {
				Class cparam = cparams[j];
				if (!validType(cparam)) {
					wrong = true;
					break;
				}
			}
			if (wrong)
				continue;

			geoms.put(method.getName().replaceFirst("create", ""), method);
		}

		return geoms;
	}

	private boolean validType(Class clazz) {
		String name;
		if (clazz.isArray())
			name = clazz.getComponentType().getName();
		else
			name = clazz.getName();
		return name.equals("com.vividsolutions.jts.geom.Coordinate")
			|| name.equals("com.vividsolutions.jts.geom.LinearRing");
	}

}