package org.jam.openjump.spatialiteplugin;

import javax.swing.*;

public class PuglinTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
	    try {
	        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
	    } catch(Exception e) {
	        System.out.println("Error setting native LAF: " + e);
	    }
		try {
            Class.forName("org.sqlite.Driver");
		} catch (ClassNotFoundException e1) {
			System.out.println("no org.sqlite.Driver" );
		}
		SpatialiteDialog sd =new SpatialiteDialog();
		sd.setVisible(true);
	}

	

}
