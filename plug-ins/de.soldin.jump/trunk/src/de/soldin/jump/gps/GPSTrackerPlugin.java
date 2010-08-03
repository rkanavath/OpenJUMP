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

import gnu.io.CommPortIdentifier;
import gnu.io.SerialPort;
import gnu.io.SerialPortEvent;
import gnu.io.SerialPortEventListener;
import gnu.io.UnsupportedCommOperationException;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.swixml.SwingEngine;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureUtil;
import com.vividsolutions.jump.util.CoordinateArrays;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.UndoableCommand;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheck;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.GeometryEditor;
import com.vividsolutions.jump.workbench.ui.LayerViewPanelProxy;
import com.vividsolutions.jump.workbench.ui.OptionsDialog;
import com.vividsolutions.jump.workbench.ui.SelectionManagerProxy;
import com.vividsolutions.jump.workbench.ui.Viewport;
import com.vividsolutions.jump.workbench.ui.plugin.FeatureInstaller;
import com.vividsolutions.jump.workbench.ui.snap.GridRenderer;

import de.soldin.jump.UndoableSetGeometry;
import de.soldin.jump.cts.WKTCSLoader;

/**
 * Another one size fits all class. It acts as a listener to the 
 * {@link de.soldin.jump.gps.GPSOptionsPanel} and adds functionality
 * to it. Also it starts/stops the {@link de.soldin.jump.gps.GPSTracker}
 * for the tracking process.
 * <p>
 * Further on the file includes three other plugins, which are closely related
 * to the <code>GPSTracker</code> plugin and will only work properly if the
 * <code>GPSTracker</code> is installed and functional. They are
 * <ul>
 * <li>SetTaskPlugin - switches the task frame to track</li>
 * <li>AddPointPlugin - adds a point at *current coordinate to 
 * selected features geometry or creates a new feature with one point</li>
 * <li>InsertPointPlugin - insert a point to the segment closest 
 * to the *current coordinate</li>
 * * current coordinate from gps receiver
 * </ul>
 * </p>
 * PS: ok next time i'll split everything into separate files. word!
 */
public class GPSTrackerPlugin
	implements
		PlugIn,
		ChangeListener,
		ItemListener,
		ActionListener,
		Comparator,
		SerialPortEventListener {
	public static final String NAME = "GPS Tracker";

	private WorkbenchContext wbc;

	private static String TEMPLATES = "de/soldin/jump/gps/";
	private SwingEngine swix = new SwingEngine(this);
	public JSlider sld_update;
	public JLabel lbl_update;
	public JPanel p_cts;
	public JComboBox cbox_processor,
		cbox_cs_in,
		cbox_cs_out,
		cbox_port,
		cbox_baudrate,
		cbox_flowcontrol,
		cbox_databits,
		cbox_stopbits,
		cbox_parity;
	public JScrollPane ta_scroll;
	public JTextArea ta_output;
	public JButton btn_test;
	private JCheckBoxMenuItem menuitem;

	private InputStream serial_is;
	//private boolean serial_testing = false;
	private SerialPort serial_port;

	private Hashtable gps_dataprocessors,
		coordinatesystems,
		portnames,
		baudrates,
		flowcontrols,
		databits,
		stopbits,
		parities;
	private HashMap possibles;
	private Properties props;

	private static final String FILE = "gps.conf";
	//private FileInputStream file_in;
	//private FileOutputStream file_out;

	private GPSTracker gps;

	public void init_gps() {

		//String base = (String) wbc.getBlackboard().get( GPSExtension.BASEFOLDER );
		//gnu.io.Loader.setLibraryPath( base );
		
		gps_dataprocessors = new Hashtable();
		gps_dataprocessors.put(
			"NMEA",
			org.dinopolis.gpstool.gpsinput.nmea.GPSNmeaDataProcessor.class);
		gps_dataprocessors.put(
			"Garmin",
			org.dinopolis.gpstool.gpsinput.garmin.GPSGarminDataProcessor.class);
		gps_dataprocessors.put(
			"Sirf",
			org.dinopolis.gpstool.gpsinput.sirf.GPSSirfDataProcessor.class);

		try {
			coordinatesystems = new Hashtable(new WKTCSLoader());
		} catch (Exception e) {
			e.printStackTrace();
		} catch (Error e) {
			// no cts installed
			e.printStackTrace();
		}

		portnames = new Hashtable();
		CommPortIdentifier portId;
		Enumeration en = null;
		try {
			en = CommPortIdentifier.getPortIdentifiers();
		} catch (Throwable t) {
			//System.out.println(e.getCause().getMessage()+"\n"+e.getCause().getStackTrace().toString());
			//System.out.println(e.getException().getMessage()+"\n"+e.getException().getStackTrace().toString());
			System.out.println( t.getMessage() +"\n" + t.getStackTrace() );
		}
		
		
		// iterate through the ports.
		while (en != null && en.hasMoreElements()) {
			portId = (CommPortIdentifier) en.nextElement();
			if (portId.getPortType() == CommPortIdentifier.PORT_SERIAL) {
				portnames.put(portId.getName(), portId);
			}
		}

		baudrates = new Hashtable();
		baudrates.put("4800", new Integer(4800));
		baudrates.put("9600", new Integer(9600));

		flowcontrols = new Hashtable();
		flowcontrols.put("None", new Integer(SerialPort.FLOWCONTROL_NONE));
		flowcontrols.put(
			"Xon/Xoff",
			new Integer(SerialPort.FLOWCONTROL_XONXOFF_IN));
		flowcontrols.put("RTS/CTS", new Integer(SerialPort.FLOWCONTROL_RTSCTS_IN));

		databits = new Hashtable();
		databits.put("8", new Integer(SerialPort.DATABITS_8));
		databits.put("7", new Integer(SerialPort.DATABITS_7));
		databits.put("6", new Integer(SerialPort.DATABITS_6));
		databits.put("5", new Integer(SerialPort.DATABITS_5));

		stopbits = new Hashtable();
		stopbits.put("1", new Integer(SerialPort.STOPBITS_1));
		stopbits.put("1.5", new Integer(SerialPort.STOPBITS_1_5));
		stopbits.put("2", new Integer(SerialPort.STOPBITS_2));

		parities = new Hashtable();
		parities.put("None", new Integer(SerialPort.PARITY_NONE));
		parities.put("Even", new Integer(SerialPort.PARITY_EVEN));
		parities.put("Odd", new Integer(SerialPort.PARITY_ODD));
		parities.put("Mark", new Integer(SerialPort.PARITY_MARK));
		parities.put("Space", new Integer(SerialPort.PARITY_SPACE));

		this.swix.getTaglib().registerTag("optionspanel", GPSOptionsPanel.class);
	}

	public void initialize(PlugInContext context) {
		
		this.wbc = context.getWorkbenchContext();
		
		// init options panel & gps stuff
		init_gps();
		
		if (this.portnames.isEmpty()) {
			context.getWorkbenchContext().getErrorHandler().handleThrowable(
				new Exception("Found no serial ports. Most likely the RXTX Comm API couldn't load the native library for your OS. GPSExtension will be disabled for now."));
			return;
		}

		// insert options panel in OPTIONS dialog 
		try {
			GPSOptionsPanel panel =
				(GPSOptionsPanel) swix.render(TEMPLATES + "optionspanel.xml");
			initUI();
			readFile();
			panel.validate();
			OptionsDialog d_options =
				OptionsDialog.instance(context.getWorkbenchContext().getWorkbench());
			swix.setActionListener(d_options, this);
			d_options.addTab(GPSExtension.MENUENTRY, panel);
		} catch (Exception e) {
			context.getWorkbenchContext().getErrorHandler().handleThrowable(e);
			return;
		}

		// insert menu items in menu
		context.getFeatureInstaller().addMainMenuItem(
			this,
			new String[] { GPSExtension.MENUENTRY },
			getName(),
			true,
			null,
			new MultiEnableCheck());

		try {
			new SetTaskPlugin(this).initialize(context);
		} catch (Exception e1) {
			context.getErrorHandler().handleThrowable(e1);
		}
		
		context.getFeatureInstaller().addMenuSeparator( GPSExtension.MENUENTRY);

		try {
			new AddPointPlugin(this).initialize(context);
		} catch (Exception e1) {
			context.getErrorHandler().handleThrowable(e1);
		}
		
		try {
			new InsertPointPlugin(this).initialize(context);
		} catch (Exception e1) {
			context.getErrorHandler().handleThrowable(e1);
		}

		context.getFeatureInstaller().addMenuSeparator( GPSExtension.MENUENTRY);
		
		try {
			new GPSOptionsPlugin().initialize(context);
		} catch (Exception e1) {
			context.getErrorHandler().handleThrowable(e1);
		}
	}

	public boolean execute(PlugInContext context) throws Exception {
		//System.out.println(getName()+" execute " + gps);
		context.getLayerManager().getUndoableEditReceiver().reportNothingToUndoYet();

		if ( isActive() && ( gps == null || gps.thread == null ) ) {
			//System.out.println("start");
			gps = new GPSTracker(this, context);

			gps.setViewport(context.getLayerViewPanel().getViewport());
			gps.start();
		} else {
			//System.out.println("stop");
			if (gps != null)
				gps.stop();
		}
		
		return true;
	}

	public String getName() {
		return NAME;
	}

	public void setViewport(Viewport vp) {
		gps.setViewport(vp);
	}

	public boolean isActive() {
		return isActive(null);
	}
	
	public boolean isActive(Boolean bool) {
		if (menuitem == null) {
			FeatureInstaller finster =
				wbc.createPlugInContext().getFeatureInstaller();
			JMenu menu = finster.menuBarMenu( GPSExtension.MENUENTRY );
			for (int i = 0; i < menu.getItemCount(); i++) {
				if (menu.getItem(i) == null)
					continue;
				if (menu.getItem(i).getText().equals(getName())) {
					try {
						menuitem = (JCheckBoxMenuItem) menu.getItem(i);
					} catch (ClassCastException cexc) {
						return false;
					}
				}
			}
		}
		// set state
		if (bool != null)
			menuitem.setSelected(bool.booleanValue());
		// report state
		return menuitem.getSelectedObjects() == null ? false : true;
	}

	public void initUI() {

		keysFromMap(cbox_processor, gps_dataprocessors);
		cbox_processor.addItemListener(this);

		sld_update.addChangeListener(this);

		if (coordinatesystems == null || coordinatesystems.isEmpty()) {
			p_cts.getParent().remove(p_cts);
		} else {
			keysFromMap(cbox_cs_in, coordinatesystems);
			keysFromMap(cbox_cs_out, coordinatesystems);
		}

		keysFromMap(cbox_port, portnames);
		cbox_port.addItemListener(this);

		keysFromMap(cbox_baudrate, baudrates);
		cbox_baudrate.addItemListener(this);

		keysFromMap(cbox_flowcontrol, flowcontrols);
		cbox_flowcontrol.addItemListener(this);

		keysFromMap(cbox_databits, databits, true);
		cbox_databits.addItemListener(this);

		keysFromMap(cbox_stopbits, stopbits);
		cbox_stopbits.addItemListener(this);

		keysFromMap(cbox_parity, parities);
		cbox_parity.addItemListener(this);

		swix.setActionListener(btn_test, this);
	}

	private void keysFromMap(JComboBox cbox, Map map, boolean reverse) {
		List keys = new Vector(map.keySet());
		Collections.sort(keys, this);
		if (reverse)
			Collections.reverse(keys);
		for (Iterator iter = keys.iterator(); iter.hasNext();) {
			String name = (String) iter.next();
			cbox.addItem(name);
		}
	}

	private void keysFromMap(JComboBox cbox, Map map) {
		keysFromMap(cbox, map, false);
	}

	public void itemStateChanged(ItemEvent e) {
		//System.out.println(e);
	}

	public void actionPerformed(ActionEvent e) {
		String cmd = e.getActionCommand();
		Object src = e.getSource();
		//System.out.println(cmd+"\t"+src+"\t"+e);

		if (src instanceof JButton && cmd.equals("AC_TEST")) {
			JButton button = (JButton) e.getSource();
			if (serial_port == null
				/*button.getText().equalsIgnoreCase("Test Settings")*/
				) {
				try {
					openConnection();
				} catch (Exception e1) {
					wbc.getErrorHandler().handleThrowable(e1);
					closeConnection();
					//e1.printStackTrace();
				}
			} else {
				closeConnection();
			}
		} else if (src instanceof JButton && cmd.equals("OK")) {
			if (gps!=null && isActive()) gps.restart();
			closeConnection();
			writeFile();
		} else if (src instanceof JButton && cmd.equals("Cancel")) {
			closeConnection();
		}
	}

	public void stateChanged(ChangeEvent e) {
		if (e.getSource().equals(sld_update))
			lbl_update.setText(
				"Update every " + ((JSlider) e.getSource()).getValue() + "s");
	}

	public int compare(Object o1, Object o2) {
		if (o1.toString().equalsIgnoreCase("none"))
			return -1;
		if (o2.toString().equalsIgnoreCase("none"))
			return +1;

		return o1.toString().compareTo(o2.toString());
	}

	public void serialEvent(SerialPortEvent e) {
		// Create a StringBuffer and int to receive input data.
		StringBuffer inputBuffer = new StringBuffer();
		int newData = 0;

		// Determine type of event.
		switch (e.getEventType()) {
			// Read data until -1 is returned. If \r is received substitute
			// \n for correct newline handling.
			case SerialPortEvent.DATA_AVAILABLE :
				//while (newData != -1) {
				try {
					do {
						//newData = serial_is.read();

						if (newData == -1) {
							addToOutput(inputBuffer.toString());
							break;
						} else if ('\r' == (char) newData || '\n' == (char) newData) {
							if (inputBuffer.length() > 0) {
								addToOutput(inputBuffer.toString() + "\n");
								inputBuffer = new StringBuffer();
							}
						} else {
							inputBuffer.append((char) newData);
						}

					} while ((newData = serial_is.read()) != -1);
				} catch (IOException ex) {
					System.err.println(ex);
					return;
				}

				// Append received data to messageAreaIn.
				//ta_output.append(new String(inputBuffer).replaceAll("[\n]{2}","\n"));
				//Dimension dim = ta_output.size();

				//ta_scroll.getViewport().setViewPosition(new Point(ta_scroll.getViewport().getViewPosition().x,dim.height));

				//((JScrollPane)ta_output.getParent()).getViewport().set
				break;
		}
	}

	private void addToOutput(String text) {
		ta_output.append(text);
		Dimension dim = ta_output.size();
		ta_scroll.getViewport().setViewPosition(
			new Point(ta_scroll.getViewport().getViewPosition().x, dim.height));
	}

	public void openConnection() throws Exception {
		try {
			// Obtain a CommPortIdentifier object for the port you want to open.
			CommPortIdentifier portId =
				(CommPortIdentifier) portnames.get(cbox_port.getSelectedItem());

			// Open the port represented by the CommPortIdentifier object. Give
			// the open call a relatively long timeout of 30 seconds to allow
			// a different application to reliquish the port if the user 
			// wants to.
			serial_port = (SerialPort) portId.open(getName(), 30000);
			int baudrate = Integer.parseInt((String) cbox_baudrate.getSelectedItem());
			int databit =
				((Integer) databits.get(cbox_databits.getSelectedItem())).intValue();
			int stopbit =
				((Integer) stopbits.get(cbox_stopbits.getSelectedItem())).intValue();
			int parity =
				((Integer) parities.get(cbox_parity.getSelectedItem())).intValue();
			serial_port.setSerialPortParams(baudrate, databit, stopbit, parity);

			serial_port.setFlowControlMode(
				((Integer) flowcontrols.get(cbox_flowcontrol.getSelectedItem()))
					.intValue());

			// Open the input and output streams for the connection. If they won't
			// open, close the port before throwing an exception.
			serial_is = serial_port.getInputStream();

			// Set notifyOnDataAvailable to true to allow event driven input.
			serial_port.notifyOnDataAvailable(true);

			// Set notifyOnBreakInterrup to allow event driven break handling.
			serial_port.notifyOnBreakInterrupt(true);

			// Set receive timeout to allow breaking out of polling loop during
			// input handling.
			try {
				serial_port.enableReceiveTimeout(30);
			} catch (UnsupportedCommOperationException e) {
			}

			ta_output.setText("");

			serial_port.addEventListener(this);

			btn_test.setText("Stop Test");
			//serial_testing = true;
		} catch (Exception e) {
			if(serial_port!=null)serial_port.close();
			//throw new Exception("Error opening i/o streams");
			throw e;
		}

	}

	public void closeConnection() {

		// Check to make sure sPort has reference to avoid a NPE.
		if (serial_port != null) {
			serial_port.removeEventListener();
			try {
				// close the i/o streams.
				serial_is.close();
			} catch (IOException e) {
				System.err.println(e);
			}

			// Close the port.
			serial_port.close();
			serial_port = null;
		}

		btn_test.setText("Test Settings");
		//serial_testing = false;
	}

	private File getFile() throws Exception{
		//String base = (String) wbc.getBlackboard().get( GPSExtension.BASEFOLDER );
		String base = GPSExtension.getLibFolder();
		File file = new File( base + FILE );
		if ( ! file.canRead() || ! file.canWrite() )
			throw new FileNotFoundException(
					"Can't read/write settings file '"
						+ file.getAbsolutePath()
						+ "' for plugin '"
						+ NAME
						+ "'. \n\nHint: Check that it is at the above location and you are allowed to read/write it!");
		return file;
	}
	
	private void writeFile() {

		String value = (String) cbox_processor.getSelectedItem();
		if (value != null)
			props.put("processor", value);

		value = new Integer(sld_update.getValue()).toString();
		props.put("update", value);

		value = (String) cbox_cs_in.getSelectedItem();
		if (value != null)
			props.put("cs_in", value);

		value = (String) cbox_cs_out.getSelectedItem();
		if (value != null)
			props.put("cs_out", value);

		value = (String) cbox_port.getSelectedItem();
		if (value != null)
			props.put("portname", value);

		value = (String) cbox_baudrate.getSelectedItem();
		if (value != null)
			props.put("baudrate", value);

		value = (String) cbox_flowcontrol.getSelectedItem();
		if (value != null)
			props.put("flowctrl", value);

		value = (String) cbox_parity.getSelectedItem();
		if (value != null)
			props.put("parity", value);

		value = (String) cbox_databits.getSelectedItem();
		if (value != null)
			props.put("databits", value);

		value = (String) cbox_stopbits.getSelectedItem();
		if (value != null)
			props.put("stopbits", value);

		try {
			File file = getFile();
			//FileInputStream  file_in = new FileInputStream(file);
			FileOutputStream file_out = new FileOutputStream(file);
			props.store(file_out, getName() + " Settings");
			file_out.close();
		} catch (Exception e) {
			wbc.getErrorHandler().handleThrowable(e);
		}

	}

	private void readFile() {
		props = new Properties();
		try {
			File file = getFile();
			FileInputStream file_in = new FileInputStream(file);
			props.load(file_in);
			file_in.close();

			String foo = (String) props.get("processor");
			if (foo != null)
				cbox_processor.setSelectedItem(foo);
			foo = (String) props.get("update");
			if (foo != null)
				sld_update.setValue(Integer.parseInt(foo));
			foo = (String) props.get("cs_in");
			if (foo != null)
				cbox_cs_in.setSelectedItem(foo);
			foo = (String) props.get("cs_out");
			if (foo != null)
				cbox_cs_out.setSelectedItem(foo);
			foo = (String) props.get("portname");
			if (foo != null)
				cbox_port.setSelectedItem(foo);
			foo = (String) props.get("baudrate");
			if (foo != null)
				cbox_baudrate.setSelectedItem(foo);
			foo = (String) props.get("flowctrl");
			if (foo != null)
				cbox_flowcontrol.setSelectedItem(foo);
			foo = (String) props.get("parity");
			if (foo != null)
				cbox_parity.setSelectedItem(foo);
			foo = (String) props.get("databits");
			if (foo != null)
				cbox_databits.setSelectedItem(foo);
			foo = (String) props.get("stopbits");
			if (foo != null)
				cbox_stopbits.setSelectedItem(foo);

		} catch (Exception e) {
			wbc.getErrorHandler().handleThrowable(e);
		}

	}

	public Object getSetting(String key) {
		//System.out.println("plugin::getSetting() "+key);
		if (props != null) {
			try {
				if (key.equalsIgnoreCase("processor"))
					return gps_dataprocessors.get(props.getProperty(key));
				else if (
					(key.equalsIgnoreCase("cs_in") || key.equalsIgnoreCase("cs_out"))
						&& coordinatesystems != null)
					return coordinatesystems.get(props.getProperty(key));
				else
					return props.getProperty(key);
			} catch (NullPointerException e) {
				wbc.getErrorHandler().handleThrowable(
					new Exception(
						"Missing Setting for '"
							+ getName()
							+ "'. Check settings under Edit->Options->"
							+ getName()
							+ "."));
				return null;
			}
		} else
			return null;
	}
	
	public Coordinate getCurrentCoordinate(){
/*		Viewport viewport = this.wbc.getLayerViewPanel().getViewport();
		Envelope current = viewport.getEnvelopeInModelCoordinates();
		double width = current.getWidth();
		double height = current.getHeight();
		return new Coordinate(current.getMinX()+(width/2),current.getMinY()+(height/2));
*/		
		if (isActive() && gps!=null)
			return gps.getPosition();
		else
			return null;
	}

}

class SetTaskPlugin extends AbstractPlugIn
//implements PlugIn
{
	private static String NAME = "Select Project to track";
	private GPSTrackerPlugin plugin;

	public SetTaskPlugin(GPSTrackerPlugin plugin) {
		this.plugin = plugin;
	}

	public void initialize(PlugInContext context) throws Exception {
		context.getFeatureInstaller().addMainMenuItem(
			this,
			new String[] { GPSExtension.MENUENTRY },
			NAME,
			false,
			null,
			createEnableCheck());
	}

	public boolean execute(PlugInContext context) throws Exception {
		context.getLayerManager().getUndoableEditReceiver().reportNothingToUndoYet();
		plugin.setViewport(context.getLayerViewPanel().getViewport());
		return true;
	}

	public String getName() {
		return NAME;
	}

	public EnableCheck createEnableCheck() {
		return new EnableCheck() {
			public String check(JComponent component) {
				return (plugin.isActive())
					? null
					: plugin.getName() + " must be active!";
			}
		};
	}

}

class AddPointPlugin 
	extends AbstractPlugIn
{
	protected String NAME = "Add coordinate to layer or feature";
	protected GPSTrackerPlugin plugin;
	protected WorkbenchContext wbc;

	// force the use of super(GPSTrackerPlugin)
	private AddPointPlugin(){}
	
	public AddPointPlugin(GPSTrackerPlugin plugin) {
		this.plugin = plugin;
	}

	public void initialize(PlugInContext context) throws Exception {
		this.wbc = context.getWorkbenchContext();
		context.getFeatureInstaller().addMainMenuItem(
			this,
			new String[] { GPSExtension.MENUENTRY },
			NAME,
			false,
			null,
			createEnableCheck(context));
	}

	public boolean execute(PlugInContext context) throws Exception {
		context.getLayerManager().getUndoableEditReceiver().reportNothingToUndoYet();

		Coordinate coord = plugin.getCurrentCoordinate();
		if (coord==null){
			context.getLayerViewPanel().getContext().warnUser("Problem: No coordinate from GPS!");
			return false;
		}

		if (!getFeatures().isEmpty()){
			if (getFeatures().size()>1)
				context.getLayerViewPanel().getContext().warnUser("More than 1 features selected. Add point to the first one.");
			
			Feature feature = (Feature)getFeatures().toArray()[0];
			Geometry geom = feature.getGeometry();
			if (!(geom instanceof LineString))
				context.getLayerViewPanel().getContext().warnUser("Attention: Converted features geometry to linestring for adding points.");
			
			UndoableSetGeometry action = new UndoableSetGeometry((Layer)getLayers().toArray()[0], 
																														getName());
			Geometry geom_new;
			GeometryFactory factory = new GeometryFactory();
			Coordinate[] coords = geom.getCoordinates();
			Coordinate[] coords_new = new Coordinate[coords.length+1];
			System.arraycopy(coords,0,coords_new,0,coords.length);
			coords_new[coords_new.length-1] = coord;
			geom_new = factory.createLineString(coords_new);

			action.setGeom(feature,geom_new);
			action.execute();
			wbc.getLayerManager().getUndoableEditReceiver().receive(action);
		
		}else	if (!getLayers().isEmpty()){
			final Layer layer = (Layer)getLayers().iterator().next();
			//System.out.print("L: "+layer);
			
			GeometryFactory factory = new GeometryFactory();
			Geometry geom = factory.createPoint(coord);
			
			final Feature feature =	FeatureUtil.toFeature(geom,layer.getFeatureCollectionWrapper().getFeatureSchema());

			UndoableCommand action = new UndoableCommand(getName()) {
					public void execute() {
							layer.getFeatureCollectionWrapper().add(feature);
					}
					public void unexecute() {
							layer.getFeatureCollectionWrapper().remove(feature);
					}
			};
		
			action.execute();
			wbc.getLayerManager().getUndoableEditReceiver().receive(action.toUndoableEdit());
		}else{
			context.getLayerViewPanel().getContext().warnUser("No layer or feature selected to add point.");
			return false;
		}
			
		
		return true;
	}

	public String getName() {
		return NAME;
	}

	public EnableCheck createEnableCheck(PlugInContext context) {
		MultiEnableCheck checker = new MultiEnableCheck();
		
		// is plugin active?
		checker.add( new EnableCheck() {
				public String check(JComponent component) {
					return (plugin.isActive())
						? null
						: plugin.getName() + " must be active!";
				}
			}
		);
		
		MultiEnableCheck checker2 = new MultiEnableCheck();
		// is a layer or feature selected?
		checker2.add( new EnableCheck() {
				public String check(JComponent component) {
					boolean layerok = (getLayers().size() == 1);
					boolean featureok = (getFeatures().size() == 1);
					
					return (layerok || featureok)
						? null
						:"Exactly one layer must be selected to add a feature from GPS. \n" +						 "OR \n" +						 "Exactly one feature in one layer must be selected to add a point from GPS.";
				}
			}
		);
		// is selected (features) layer editable?
		checker2.add( new EnableCheck() {
				public String check(JComponent component) {
					boolean editable = ((Layer)getLayers().iterator().next()).isEditable();
					return (editable)
						? null
						:"Selected layer is not editable.";
				}
			}
		);
		checker.add(checker2);
		
		return checker;
	}
	
	protected Collection getFeatures(){
		return ((SelectionManagerProxy) wbc.getWorkbench().getFrame().getActiveInternalFrame())
														.getSelectionManager().getFeatureSelection().getFeaturesWithSelectedItems();
	}

	protected Collection getLayers(){
		// all layers with selected items (parts of geometries)
		Collection layers = ((SelectionManagerProxy) wbc.getWorkbench().getFrame().getActiveInternalFrame())
																.getSelectionManager().getFeatureSelection().getLayersWithSelectedItems();
		return layers.isEmpty() 
			?	Arrays.asList(wbc.getLayerNamePanel().getSelectedLayers()) 
			:	layers;
	}
	
}

class InsertPointPlugin 
	extends AddPointPlugin
{

	public InsertPointPlugin(GPSTrackerPlugin plugin) {
		super(plugin);
		NAME = "Insert coordinate to closest segment";
	}
	
	public boolean execute(PlugInContext context) throws Exception {
		context.getLayerManager().getUndoableEditReceiver().reportNothingToUndoYet();

		Coordinate coord = plugin.getCurrentCoordinate();
		if (coord==null){
			context.getLayerViewPanel().getContext().warnUser("Problem: No coordinate from GPS!");
			return false;
		}

		if (!getFeatures().isEmpty()){
			if (getFeatures().size()>1)
				context.getLayerViewPanel().getContext().warnUser("More than 1 features selected. Add point to the first one.");
			
			Feature feature = (Feature)getFeatures().toArray()[0];
			Geometry geom = feature.getGeometry();

			if (geom instanceof com.vividsolutions.jts.geom.Point 
					|| geom instanceof com.vividsolutions.jts.geom.MultiPoint){
				context.getLayerViewPanel().getContext().warnUser("Selected geometry must not be a point or multipoint.");
				return false;
			}
			
			UndoableSetGeometry action = new UndoableSetGeometry((Layer)getLayers().toArray()[0], 
																														getName());
			Geometry geom_new;
			// for Point and Multipoint the check will always produce null
			// so I took care that they'll treated in the else condition
			LineSegment line = segmentInRange(geom,coord);
			if (geom.getCoordinates().length>1 && line!=null){			
				Geometry geom_orig = action.getGeom(feature);
				geom_new  = new GeometryEditor().insertVertex(geom_orig, line.p0,line.p1, coord);
			}else {
				context.getLayerViewPanel().getContext().warnUser("Selected geometry consists of less than 2 points, or .");
				return false;
			}
			
			action.setGeom(feature,geom_new);
			action.execute();
			wbc.getLayerManager().getUndoableEditReceiver().receive(action);
		
		}else{
			
			context.getLayerViewPanel().getContext().warnUser("Problem: No feature selected!");
			return false;	
		}
		
		return true;
	}

	public EnableCheck createEnableCheck(PlugInContext context) {
		MultiEnableCheck checker = new MultiEnableCheck();
		
		// is plugin active?
		checker.add( new EnableCheck() {
				public String check(JComponent component) {
					return (plugin.isActive())
						? null
						: plugin.getName() + " must be active!";
				}
			}
		);
		
		MultiEnableCheck checker2 = new MultiEnableCheck();
		// is a layer or feature selected?
		checker2.add( new EnableCheck() {
				public String check(JComponent component) {
					boolean featureok = (getFeatures().size() == 1);
					
					return (featureok)
						? null
						: "Exactly one feature in one layer must be selected to insert a point from GPS.";
				}
			}
		);
		// is selected (features) layer editable?
		checker2.add( new EnableCheck() {
				public String check(JComponent component) {
					boolean editable = ((Layer)getLayers().iterator().next()).isEditable();
					return (editable)
						? null
						:"Selected layer is not editable.";
				}
			}
		);
		checker.add(checker2);
		
		return checker;
	}

	protected LineSegment segmentInRange(Geometry geometry, Coordinate target) {
			LineSegment closest = null;
			//CoordinateArray ignores Point and Multipoint, for obvious reasons
			List coordArrays = CoordinateArrays.toCoordinateArrays(geometry, false);
			for (Iterator i = coordArrays.iterator(); i.hasNext();) {
					Coordinate[] coordinates = (Coordinate[]) i.next();
					for (int j = 1; j < coordinates.length; j++) { //1
							LineSegment candidate = new LineSegment(coordinates[j - 1], coordinates[j]);
							if ((closest == null) || (candidate.distance(target) < closest.distance(target))) {
									closest = candidate;
							}
					}
			}
			return closest;
	}

}

class GPSOptionsPlugin extends AbstractPlugIn {

    public boolean execute(PlugInContext context) throws Exception {
        reportNothingToUndoYet(context);
        GUIUtil.centreOnWindow(dialog(context));
        JTabbedPane pane = dialog(context).getTabbedPane();
        pane.setSelectedIndex(pane.indexOfTab( GPSExtension.MENUENTRY ));
        dialog(context).setVisible(true);
        if (dialog(context).wasOKPressed()) {
            JInternalFrame[] frames = context.getWorkbenchFrame().getInternalFrames();
            for (int i = 0; i < frames.length; i++) {
                if (frames[i] instanceof LayerViewPanelProxy) {
                    ((LayerViewPanelProxy) frames[i])
                        .getLayerViewPanel()
                        .getRenderingManager()
                        .render(
                        GridRenderer.CONTENT_ID,
                        true);
                }
            }
        }
        return dialog(context).wasOKPressed();
    }
    private OptionsDialog dialog(PlugInContext context) {
        return OptionsDialog.instance(context.getWorkbenchContext().getWorkbench());
    }
    
    public void initialize(PlugInContext context) throws Exception {
		// insert menu items in VIEW menu
		context.getFeatureInstaller().addMainMenuItem(
			this,
			new String[] { GPSExtension.MENUENTRY },
			"GPS Options",
			false, null, null);
    }

}