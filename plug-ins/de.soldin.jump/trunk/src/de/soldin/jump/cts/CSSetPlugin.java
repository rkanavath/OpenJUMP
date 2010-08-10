/**
 * @(#)CSSetPlugin.java	29.06.2004
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

import java.awt.Component;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;

import org.geotools.ct.CannotCreateTransformException;
import org.swixml.SwingEngine;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.util.Blackboard;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Category;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.EnableCheck;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.TaskFrame;

import de.soldin.jump.gps.GPSExtension;

/**
 * <code>CSSetPlugin</code> acts as plugin and therefore is a
 * listener to this extensions ui components.
 * <p>
 * The plugin itself only creates <code>CSSetting</code> objects which are 
 * saved in the workbenchs blackboard. If there is a layer assigned to a cs, 
 * then there is a matching <code>CSSetting</code> in the blackboard.
 * </p><p>
 * For the setting to take effect there are {@link CSTransform} objects 
 * registered as {@link CSSetListener} which on a setting trial get informed
 * and transform the {@link CSSetting}s geometries, speak the geometries of
 * all features in the <code>CSSettings</code> layer.
 * </p>
 * @see com.vividsolutions.jump.workbench.plugin.Extension
 * @see com.vividsolutions.jump.workbench.plugin.PlugIn
 */
public class CSSetPlugin
	implements ActionListener, CSTransformFilter, WindowListener, PlugIn {
	private static final String NAME = "Set Coordinate System";

	protected JDialog dialog;
	public JComboBox chooser_src, chooser_trg;
	public JPanel p_layers, p_categories, p_buttons;
	private static final String STORAGE = "Coordinate System Data Storage";
	protected HashMap cs_settings;
	private boolean cancelled = false;
	protected PlugInContext context;

	protected static String TEMPLATES = "de/soldin/jump/cts/";
	protected SwingEngine swix;

	/**
	 * @see com.vividsolutions.jump.workbench.plugin.Configuration#configure(com.vividsolutions.jump.workbench.plugin.PlugInContext)
	 */
	public void configure(PlugInContext context) throws Exception {
		new CSSetPlugin().initialize(context);
	}


	/**
	 * Adds the menu entries, intializes swix stuff
	 * 
	 * @see com.vividsolutions.jump.workbench.plugin.PlugIn#initialize(com.vividsolutions.jump.workbench.plugin.PlugInContext)
	 */
	public void initialize(PlugInContext context) {
		EnableCheckFactory checkFactory =
			new EnableCheckFactory(context.getWorkbenchContext());

		JPopupMenu categoryPopups =
			context
				.getWorkbenchContext()
				.getWorkbench()
				.getFrame()
				.getCategoryPopupMenu();
		context.getFeatureInstaller().addPopupMenuItem(
			categoryPopups,
			this,
			NAME,
			false,
			null,
			new MultiEnableCheck()
				.add(checkFactory.createAtLeastNCategoriesMustBeSelectedCheck(1))
				.add(
					createCategorySelectedMustHaveAtLeastNItems(
						1,
						context.getWorkbenchContext())));

		JPopupMenu layerNamePopupMenu =
			context
				.getWorkbenchContext()
				.getWorkbench()
				.getFrame()
				.getLayerNamePopupMenu();
		context.getFeatureInstaller().addPopupMenuItem(
			layerNamePopupMenu,
			this,
			NAME,
			false,
			null,
			new MultiEnableCheck().add(
				checkFactory.createAtLeastNLayersMustBeSelectedCheck(1)));

		// swix stuff	
		this.swix = new SwingEngine(this);
		this.swix.getTaglib().registerTag("cscombobox", CSComboBox.class);

		// init blackboard
		Blackboard blacky =
			context.getWorkbenchContext().getWorkbench().getBlackboard();
		if (blacky.get(STORAGE) == null) {
			blacky.put(STORAGE, new HashMap());
		}
		cs_settings = (HashMap) blacky.get(STORAGE);
	}

	/**
	 * @see com.vividsolutions.jump.workbench.plugin.Extension#getName()
	 */
	public String getName() {
		return NAME;
	}

	/**
	 * builds the dialog, adds the necessary listeners, which take care of 
	 * setting the cs's for the layers and transforms if requested.  
	 * 
	 * @see com.vividsolutions.jump.workbench.plugin.PlugIn#execute(com.vividsolutions.jump.workbench.plugin.PlugInContext)
	 */
	public boolean execute(PlugInContext context) throws Exception {
		this.context = context;
		context.getLayerManager().getUndoableEditReceiver().reportNothingToUndoYet();
		
		//create dialog
		dialog = new JDialog(context.getWorkbenchFrame(), true);
		dialog.setTitle(NAME);
		// position dialog
		Point location;
		Component frame = context.getActiveInternalFrame();
		if (frame instanceof TaskFrame) {
			location = frame.getLocationOnScreen();
		} else {
			frame = context.getWorkbenchFrame();
			location = frame.getLocationOnScreen();
		}
		location.translate(125, 25);
		//System.out.print(context.getActiveInternalFrame()+"\n"+context.getWorkbenchFrame()+"\n"+location);

		// initialize dialog's main panel
		JPanel main = (JPanel) swix.render(TEMPLATES + "dialog.xml");

		// create layers settings & add layers comboboxes		
		Collection layers = getAffectedLayers();
		Collection affected_cs_settings = new Vector();

		for (Iterator iter = layers.iterator(); iter.hasNext();) {
			Layer layer = (Layer) iter.next();

			// get/initialize cs_setting
			HashMap safe = (HashMap) cs_settings.get(layer);
			if (safe == null) {
				safe = new HashMap();

				// new setting object
				CSSetting css = new CSSetting(layer);

				// new transformer
				CSTransform cst = new CSTransform(css, context.getWorkbenchContext());
				// tell it who is transforming
				cst.addTransformFilter(this);

				// initialize combobox panel
				JPanel chooser = (JPanel) swix.render(TEMPLATES + "combobox.xml");
				// add CSSetting as Listener
				css.listenEventsFrom(chooser);
				// add CSTransform as Listener
				cst.listenEventsFrom(chooser);

				JLabel label = (JLabel) chooser.getComponent(0);
				label.setText(layer.getName());

				safe.put("CSS", css);
				safe.put("CST", cst);
				safe.put("PANEL", chooser);
				cs_settings.put(layer, safe);
			}

			CSSetting css = (CSSetting) safe.get("CSS");
			//CSTransform cst = (CSTransform)safe.get("CST");
			JPanel chooser = (JPanel) safe.get("PANEL");

			// eventually add it to panel
			p_layers.add(chooser);

			// add all css to list for all_cbox
			affected_cs_settings.add(css);
		}

		if (affected_cs_settings.size() > 1) {

			HashMap safe = (HashMap) cs_settings.get("CST_ALL");
			if (safe == null) {
				safe = new HashMap();
				// create all modifier panel
				JPanel all_p_chooser = (JPanel) swix.render(TEMPLATES + "combobox.xml");
				// get combobox & label
				//CSComboBox all_cbox = (CSComboBox)all_p_chooser.getComponent(1);			
				JLabel all_label = (JLabel) all_p_chooser.getComponent(0);
				// remove checkbox
				all_p_chooser.remove(2);

				// create CST
				CSTransform all_cs_transform =
					new CSTransform(affected_cs_settings, context.getWorkbenchContext());
				// add CSSetting as Listener
				all_cs_transform.listenEventsFrom(all_p_chooser);
				// add transformfilter
				all_cs_transform.addTransformFilter(this);

				// name this t'ing
				all_label.setText("Set all to:");

				safe.put("CST", all_cs_transform);
				safe.put("PANEL", all_p_chooser);
				cs_settings.put("CST_ALL", safe);
			}

			CSTransform cst = (CSTransform) safe.get("CST");
			cst.setAffectedCSS(affected_cs_settings);

			JPanel all_p_chooser = (JPanel) safe.get("PANEL");
			p_categories.add(all_p_chooser);
		} else {
			p_categories.getParent().remove(p_categories);
		}

		// add actionlistener for buttons
		swix.setActionListener(p_buttons, this);

		dialog.getContentPane().add(main);

		// windowlistener for closing action CANCELLATION
		dialog.addWindowListener(this);
		dialog.validate();
		dialog.pack();

		dialog.setLocation(location);
		dialog.setVisible(true);
		dialog.dispose();

		return false;
	}

	/**
	 * implementation of changing the cs by transforming the coordinates of 
	 * different geometries, parameter is a {@link java.util.Collection} of 
	 * {@link CSSetting CSSettings}
	 * 
	 * @see de.soldin.jump.cts.CSTransformFilter#transform(java.util.Collection)
	 */
	public Collection transform(Collection csss) {
		// all changes get stuffed into transforms
		Collection transforms = new Vector();

		for (Iterator t_iter = csss.iterator(); t_iter.hasNext();) {
			CSSetting css = (CSSetting) t_iter.next();
			//System.out.println("New::transform() "+css.toString());

			// hop over empty entries and same cs&next_cs
			if (css.layer
				== null | css.cs
				== null | css.next_cs
				== null | css.cs.equals(css.next_cs))
				continue;

			Layer layer = (Layer) css.layer;
			CoordinateTransformFilter transfilter;
			try {
				transfilter = new CoordinateTransformFilter(css.cs, css.next_cs);
				//transfilter.setYx(true);

				Collection features = layer.getFeatureCollectionWrapper().getFeatures();

				UndoableSetGeometry transformation =
					new UndoableSetGeometry(
						"Transformation: ",
						css.layer.getName()
							+ "("
							+ CSSetting.getKey(css.cs)
							+ "->"
							+ CSSetting.getKey(css.next_cs)
							+ ")",
						css);

				for (Iterator iter = features.iterator(); iter.hasNext();) {
					Feature feature = (Feature) iter.next();

					Geometry geom = transformation.getGeom(feature);
					geom.apply(transfilter);
					geom.geometryChanged();
					transformation.setGeom(feature, geom);
				}

				transforms.add(transformation);

			} catch (CannotCreateTransformException e) {
				e.printStackTrace();
			}

		}

		return transforms;
	}

	/**
	 * implementation of changing the cs by transforming the coordinates of 
	 * different geometries, parameter is a {@link CSSetting}
	 * 
	 * @see de.soldin.jump.cts.CSTransformFilter#transform(de.soldin.jump.cts.CSSetting)
	 */
	public Collection transform(CSSetting css) {
		Collection foo = new Vector();
		foo.add(css);
		return transform(foo);
	}

	/**
	 * implementation of the actionlisteners method 
	 * {@link java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)}
	 * to catch events from the dialog's buttons.
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	// catch ok and cancel
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("AC_CANCEL")) {
			cancelled = true;
			dialog.setVisible(false);
		} else if (e.getActionCommand().equals("AC_OK")) {
			dialog.setVisible(false);
		}
		//System.out.println(e);
	}

	/**
	 * does nothing
	 * 
	 * @see java.awt.event.WindowListener#windowActivated(java.awt.event.WindowEvent)
	 */
	public void windowActivated(WindowEvent e) {}

	/**
	 * does nothing
	 * 
	 * @see java.awt.event.WindowListener#windowClosed(java.awt.event.WindowEvent)
	 */
	public void windowClosed(WindowEvent e) {}
	
	
	/**
	 * of window closes the event will be cancelled
	 * 
	 * @see java.awt.event.WindowListener#windowClosing(java.awt.event.WindowEvent)
	 */
	public void windowClosing(WindowEvent e) {
		cancelled = true;
	}
	
	public void windowDeactivated(WindowEvent e) {}
	public void windowDeiconified(WindowEvent e) {}
	public void windowIconified(WindowEvent e) {}
	public void windowOpened(WindowEvent e) {}

	/**
	 * Returns a collection of selected layers in the current workbench
	 * frame.
	 * 
	 * @return Collection of {@link com.vividsolutions.jump.workbench.model.Layer Layers}
	 */
	protected Collection getAffectedLayers() {
		Collection layers = new ArrayList();

		for (Iterator iter =
			context.getLayerNamePanel().getSelectedCategories().iterator();
			iter.hasNext();
			) {
			Category category = (Category) iter.next();
			for (Iterator iterator = category.getLayerables().iterator();
				iterator.hasNext();
				) {
				Layer layer = (Layer) iterator.next();
				layers.add(layer);
			}
		}
		// set cs for layers	
		Layer[] sel_layers = context.getLayerNamePanel().getSelectedLayers();
		for (int i = 0; i < sel_layers.length; i++) {
			Layer layer = sel_layers[i];
			if (!layers.contains(layer))
				layers.add(layer);
		}
		return layers;
	}

	/**
	 * Returns a collection of the selected {@link com.vividsolutions.jump.workbench.model.Category categories} 
	 * 
	 * @return Collection
	 */
	protected Collection getAffectedCategories() {
		Collection cats = new ArrayList();
		for (Iterator iter =
			context.getLayerNamePanel().getSelectedCategories().iterator();
			iter.hasNext();
			) {
			Category category = (Category) iter.next();
			cats.add(category);
		}
		return cats;
	}

	/**
	 * Creates a {@link com.vividsolutions.jump.workbench.plugin.EnableCheck}
	 * with the given count of layers necessary for a category.
	 * 
	 * @param i count of necessary layers, for the check not to return a errormessage
	 * @param workbenchContext
	 * @return the enable check object
	 * 
	 * @see com.vividsolutions.jump.workbench.plugin.EnableCheck
	 * @see com.vividsolutions.jump.workbench.plugin.MultiEnableCheck
	 */
	public EnableCheck createCategorySelectedMustHaveAtLeastNItems(
		final int i,
		final WorkbenchContext workbenchContext) {
		return new EnableCheck() {
			//WorkbenchContext workbenchContext = workbenchContext;
			//final int layersneeded = i;

			public String check(JComponent component) {

				int layercount = 0;
				for (Iterator iter =
					workbenchContext
						.getLayerNamePanel()
						.getSelectedCategories()
						.iterator();
					iter.hasNext();
					) {
					Category element = (Category) iter.next();
					layercount += element.getLayerables().size();
				}

				return (layercount < i)
					? "At least " + i + " layers must exist in selected categories!"
					: null;
			}
		};
	}

	
}
