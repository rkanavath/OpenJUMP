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

import java.awt.Component;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.geom.NoninvertibleTransformException;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.InputStream;
import java.text.DecimalFormat;
import java.util.Hashtable;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Spring;
import javax.swing.SpringLayout;
import javax.xml.transform.stream.StreamSource;

import org.dinopolis.gpstool.gpsinput.GPSDataProcessor;
import org.dinopolis.gpstool.gpsinput.GPSException;
import org.dinopolis.gpstool.gpsinput.GPSPosition;
import org.dinopolis.gpstool.gpsinput.GPSSerialDevice;
import org.geotools.cs.CoordinateSystem;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.Viewport;
import com.vividsolutions.jump.workbench.ui.WorkbenchFrame;

import de.soldin.jump.cts.*;

/**
 * The <code>GPSTracker</code> combines the capabilities of 
 * <ul>
 * <li>{@link java.awt.event.ComponentListener}</li>
 * <li>{@link java.beans.PropertyChangeListener}</li>
 * <li>{@link java.lang.Runnable}</li>.
 * </ul>
 * When this threads {@link #run()} method is called, it initializes
 * the <code>org.dinopolis.gpstool</code> components necessary for 
 * gps reception.
 * <br>
 * It registers itself as listener to a {@link org.dinopolis.gpstool.gpsinput.GPSDataProcessor}
 * and runs then in background receiving the gps events.
 */
public class GPSTracker
	implements ComponentListener, PropertyChangeListener, Runnable {
	Thread thread;

	GPSTrackerPlugin parent;
	WorkbenchContext wbc;
	Viewport viewport;
	Component marker;

	GPSDataProcessor gps_data_processor;
	GPSPosition position = null;
	long last_updt = 0;

	WKTCSLoader loader;

	public GPSTracker(GPSTrackerPlugin parent, PlugInContext context) {
		super();
		this.parent = parent;
		this.wbc = context.getWorkbenchContext();
		setViewport(context.getLayerViewPanel().getViewport());

	}

	public void setViewport(Viewport vp) {
		removeMarker();
		this.viewport = vp;
		addMarker();
	}

	private void addMarker() {
		if (viewport != null) {
			JPanel panel = viewport.getPanel();
			panel.setLayout(new SpringLayout());
			ClassLoader cl = this.getClass().getClassLoader();
			InputStream is = cl.getResourceAsStream( "de/soldin/jump/gps/marker.gif" );
			//System.out.println("Stream : " + is );
			//System.out.println("Stream1 : " + cl.getResource("/de/soldin/jump/gps/marker.gif") );

			//wbc.getBlackboard().get( GPSExtension.BASEFOLDER ) + "/gps/marker.gif"
			try {
				marker =
					new JPanel().add(
						new JLabel(
							new ImageIcon( ImageIO.read(is) ) ) );
				//System.out.println(marker.getHeight() +" "+ marker.getWidth());
				panel.add(
					marker,
					new SpringLayout.Constraints(
						Spring.constant(viewport.getPanel().getWidth() / 2 - 100 / 2),
						Spring.constant(viewport.getPanel().getHeight() / 2 - 100 / 2)));
	
				//panel.add(marker);
				//marker.setLocation(viewport.getPanel().getWidth()/2-marker.getWidth()/2,viewport.getPanel().getHeight()/2-marker.getHeight()/2);
			} catch (Exception e) {
				this.wbc.getErrorHandler().handleThrowable(e);
			}
			
			panel.addComponentListener(this);
			panel.validate();
		}
	}

	private void removeMarker() {
		try {
			marker.getParent().remove(marker);
		} catch (NullPointerException ignore) {
		}
	}

	public void start() {
		//position = new GPSPosition();
		//position.setLatitude(10);position.setLongitude(50);last_updt = System.currentTimeMillis();
		thread = new Thread(this, this.getClass().getName());
		thread.start();
	}

	public void run() {
		String port = (String) parent.cbox_port.getSelectedItem();
		Integer speed =
			Integer.valueOf((String) parent.cbox_baudrate.getSelectedItem());

		Hashtable environment = new Hashtable();
		environment.put(GPSSerialDevice.PORT_NAME_KEY, port);
		environment.put(GPSSerialDevice.PORT_SPEED_KEY, speed);
		GPSSerialDevice gps_device = new GPSSerialDevice();

		//long update = 0;
		//long last_track = 0;

		try {
			Class prozzi = (Class) parent.getSetting("processor");
			gps_data_processor = (GPSDataProcessor) prozzi.newInstance();

			gps_device.init(environment);
			gps_data_processor.setGPSDevice(gps_device);
			gps_data_processor.addGPSDataChangeListener(this);
			gps_data_processor.open();

			final WorkbenchFrame wbf = wbc.getWorkbench().getFrame();
			long update, last_tracked;
			Coordinate coord = null;
			do {
				//System.out.println(i++);
				coord = getPosition();
				update = 1000 * Long.parseLong((String) parent.getSetting("update"));
				// into millisecs
				last_tracked = 0;

				if (coord != null && update > 0) {

					double pos_x = coord.x; //position.getLongitude();
					double pos_y = coord.y; //position.getLatitude();
					Envelope current = viewport.getEnvelopeInModelCoordinates();
					double width = current.getWidth();
					double height = current.getHeight();
					Envelope next =
						new Envelope(
							pos_x - (width / 2),
							pos_x + (width / 2),
							pos_y - (height / 2),
							pos_y + (height / 2));
					try {
						viewport.zoom(next);
					} catch (NoninvertibleTransformException e2) {
						e2.printStackTrace();
					}
					last_tracked = System.currentTimeMillis();
				}

				// sleep miss ukubit

				if (update > 0) {
					if (coord == null) {
						//wbf.setTimeMessage("No Position to track to.");
						try {
							Thread.sleep(15 * 1000);
						} catch (InterruptedException e) {
						}
					} else if (last_tracked > 0) {
						while (true && thread != null) {
							//System.out.println(last_tracked+update-System.currentTimeMillis());
							try {
								Thread.sleep(1000);
							} catch (InterruptedException e1) {
							}
							int next_track =
								Math.round(
									(last_tracked + update - System.currentTimeMillis()) / 1000);
							if (next_track <= 0) {
								wbf.setTimeMessage("Track now...");
								break;
							} else
								wbf.setTimeMessage("Next Track in " + next_track + "s.");
						}
					}
				} else {
					// per default update display every 15s
					wbf.setTimeMessage("Task Tracking disabled");
					try {
						Thread.sleep(15 * 1000);
					} catch (InterruptedException e) {
					}
				}

			}
			while (thread == Thread.currentThread());

		} catch (GPSException e) {
			wbc.getErrorHandler().handleThrowable(e);
			e.printStackTrace();
		} catch (InstantiationException e3) {
			e3.printStackTrace();
		} catch (IllegalAccessException e3) {
			e3.printStackTrace();
		}

		stop();

	}

	public void stop() {
		wake();
		thread = null;
		try {
			if (gps_data_processor != null) {
				gps_data_processor.removeGPSDataChangeListener(this);
				//TODO: fix this
				gps_data_processor.close();
				gps_data_processor = null;
			}
		} catch (GPSException e) {
			e.printStackTrace();
		}
		wbc.getWorkbench().getFrame().setStatusMessage("");
		wbc.getWorkbench().getFrame().setTimeMessage("");
		parent.isActive(new Boolean(false));
		removeMarker();
	}

	public void wake() {
		if (thread != null)
			thread.interrupt();
	}

	public void restart() {
		stop();
		setViewport(viewport);
		start();
	}

	public void propertyChange(PropertyChangeEvent event) {
		wake();
		//System.out.println(event);
		String name = event.getPropertyName();
		if (name.equals(GPSDataProcessor.LOCATION)) {
			position = (GPSPosition) event.getNewValue();
			last_updt = System.currentTimeMillis();
			
			showPosition();
			//System.out.println("receive pos: "+position.toString());
		}
	}

	private final DecimalFormat decformat = new DecimalFormat("#.000000");
	
	public void showPosition(){
		Coordinate coord = getPosition();
		wbc.getWorkbench().getFrame().setStatusMessage(
				"GPS: "
				+ (coord == null
					? "N/A"
					: decformat.format(coord.x) + " / " + decformat.format(coord.y)));
	}

	public Coordinate getPosition() {

		// Test
		//if (true) return new Coordinate(3432596.8159249444, 5794847.076351206);

		// check if position is valid still, must be age 10s or younger
		if ( position == null
			|| !(System.currentTimeMillis() - last_updt < 10000)) {
			return null;
		} else {
			Coordinate coord =
				new Coordinate(position.getLongitude(), position.getLatitude());

			//String cs_in_key  = (String)parent.getSetting("cs_in");//(String)parent.cbox_cs_in.getSelectedItem();
			//String cs_out_key = (String)parent.getSetting("cs_out");//(String)parent.cbox_cs_out.getSelectedItem();
			//if (cs_in_key!=null && cs_out_key!=null){
			try {
				//if (loader==null) loader = new WKTCSLoader();
				Object o = parent.getSetting("cs_in");
				//System.out.println(this.getClass().getName() + " classloaders are: " + o.getClass().getClassLoader() + " / " + CoordinateSystem.class.getClassLoader());

				CoordinateSystem cs_in = (CoordinateSystem) o;
				//loader.get(cs_in_key);
				CoordinateSystem cs_out =
					(CoordinateSystem) parent.getSetting("cs_out");
				//System.out.println( this.getClass().getSimpleName() + " : " + cs_in + " / " + cs_out);
				//loader.get(cs_out_key);
				if (cs_in != null && cs_out != null) {
					CoordinateTransformFilter csf =
						new CoordinateTransformFilter(cs_in, cs_out);
					csf.setYx(true);
					csf.filter(coord);
				}
			} catch (Throwable t) {
				// no cts properly installed?
				t.printStackTrace();
			}

			return coord;
		}

	}

	// workaround to keep cross in the center of screen
	public void componentHidden(ComponentEvent e) {
	}
	public void componentMoved(ComponentEvent e) {
	}
	public void componentResized(ComponentEvent e) {
		if (thread != null && viewport != null)
			setViewport(viewport);
	}
	public void componentShown(ComponentEvent e) {
	}
}
