/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI 
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * JUMP is Copyright (C) 2003 Vivid Solutions
 *
 * This class implements extensions to JUMP and is
 * Copyright (C) Stefan Steiniger.
 * 
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
 * For more information, contact:
 * Stefan Steiniger
 * perriger@gmx.de
 */
/*****************************************************
 * created:  		19.Jan.2009
 * last modified:   					
 * 					
 * 
 * @author sstein
 * 
 * description: Calculates several polygon neighborhood metrics.
 * 	
 *  
 *****************************************************/

package ch.unizh.geo.pagevis.ojplugin.patterns;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;

import ch.unizh.geo.pagevis.patterndetection.MultiPolygonMetrics;

import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.ThreadedPlugIn;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;
import com.vividsolutions.jump.workbench.ui.plugin.FeatureInstaller;

/**
 * @description: Calculates several polygon neighborhood metrics.
 *	
 * @author sstein
 *
 **/
public class CharacterizePolygonNeighborhoodPlugIn extends AbstractPlugIn implements ThreadedPlugIn{


    private String sSidebar ="Select the metrics to characterize the single polygons.\n" +
    		"MBR-length-based buffer: the buffer is calculated based on the max dimension " +
    		"of the polygon MBR. By default a normal absolute buffer is calculated.\n" +
    		"Existing attributes with similar names will be overwritten. Multi-Polygons will not be processed and their output value is set to 'NAN'. However, you can 'explode' Multi-Polygon geometries before processing.";
    private String sSelectAll = "Select all metrics below";
    private String sBuffer = "use MBR-length-based buffer";
    private String sBufferRadius = "buffer value (or MBR-length factor)";
    private String sBufferGeoms = "return buffer geometries";
    private final String LAYERREGIONS = "Layer with polygons";
    private FeatureCollection regions = null;        
    private Layer input = null;
    private MultiInputDialog dialog;
    private double bufferValue = 0.0;
    private ArrayList<Boolean> useMetrics = null;
    private int bufferType = 0;
    private boolean outputBufferGeoms = false;
  
    public void initialize(PlugInContext context) throws Exception {
    				
	        FeatureInstaller featureInstaller = new FeatureInstaller(context.getWorkbenchContext());
	    	featureInstaller.addMainMenuItem(
	    	        this,								//exe
	                new String[] {MenuNames.PLUGINS, "LE-Pattern"}, 	//menu path
	                "Characterize Polygons with Polygon Neighborhood Metrics",
	                false,			//checkbox
	                null,			//icon
	                createEnableCheck(context.getWorkbenchContext())); //enable check
    }
    
    public static MultiEnableCheck createEnableCheck(WorkbenchContext workbenchContext) {
        EnableCheckFactory checkFactory = new EnableCheckFactory(workbenchContext);

        return new MultiEnableCheck()
                        .add(checkFactory.createAtLeastNLayersMustExistCheck(1));
    }
    
	public boolean execute(PlugInContext context) throws Exception{
        //Unlike ValidatePlugIn, here we always call #initDialog because we want
        //to update the layer comboboxes.
        initDialog(context);
        dialog.setVisible(true);
        if (!dialog.wasOKPressed()) {
            return false;
        }
        else{
        	this.getDialogValues(dialog);
        }
        return true;	    
	}
	
    public void run(TaskMonitor monitor, PlugInContext context) throws Exception{            		
	    	System.gc(); //flush garbage collector
	    	monitor.allowCancellationRequests();
	    	FeatureCollection resultC = MultiPolygonMetrics.characterizeSinglePolygons(this.regions, 
	    			this.getNameOfNeighborhoodMetrics(), this.useMetrics, this.bufferValue, 
	    			this.bufferType, this.outputBufferGeoms, monitor);
	        context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-char", resultC);
	
    	    System.gc();    		
    	}
		
    private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "polygon characterization", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.LAYERREGIONS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        JCheckBox bufferCheckbox = dialog.addCheckBox(sBuffer, false);
        dialog.addDoubleField(this.sBufferRadius, this.bufferValue, 7);
        JCheckBox returnBufferCheckbox = dialog.addCheckBox(sBufferGeoms, false);        
        JCheckBox allCheckbox = dialog.addCheckBox(sSelectAll, false);
        allCheckbox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateControls();
            }
        });
        dialog.addSeparator();
        dialog.addSubTitle("Neighborhood Metrics:");
        ArrayList<String> singleMetrics = this.getNameOfNeighborhoodMetrics();
        this.useMetrics = new ArrayList<Boolean>();
        for (Iterator iterator = singleMetrics.iterator(); iterator.hasNext();) {
			String string = (String) iterator.next();
			Boolean checkboxval = false; 
			this.useMetrics.add(checkboxval);
	    	dialog.addCheckBox(string, checkboxval);			
		}
        GUIUtil.centreOnWindow(dialog);
    }
    
    public ArrayList<String> getNameOfNeighborhoodMetrics(){
    	ArrayList<String> metrics = new ArrayList<String>();
    	metrics.add(MultiPolygonMetrics.NUM_POLYS);
    	metrics.add(MultiPolygonMetrics.CONVEXHULL_DENSITY);
    	metrics.add(MultiPolygonMetrics.BUFFERHULL_DENSITY);
    	metrics.add(MultiPolygonMetrics.R_INDEX);
    	return metrics;
    }    
    
    protected void updateControls() {
	    getDialogValues(dialog);
	    boolean selected = dialog.getBoolean(sSelectAll);
	    if (selected){
	        ArrayList<String> singleMetrics = this.getNameOfNeighborhoodMetrics();
	        for (Iterator iterator = singleMetrics.iterator(); iterator.hasNext();) {
				String string = (String) iterator.next();
				JCheckBox tempCB = dialog.getCheckBox(string);
				tempCB.setSelected(true);
	        }
	    }
	    else{
	        ArrayList<String> singleMetrics = this.getNameOfNeighborhoodMetrics();
	        for (Iterator iterator = singleMetrics.iterator(); iterator.hasNext();) {
				String string = (String) iterator.next();
				JCheckBox tempCB = dialog.getCheckBox(string);
				tempCB.setSelected(false);
	        }	    	
	    }
    }
    
    private void getDialogValues(MultiInputDialog dialog){
    	this.input =  dialog.getLayer(this.LAYERREGIONS);
    	this.regions = this.input.getFeatureCollectionWrapper();
        ArrayList<String> singleMetrics = this.getNameOfNeighborhoodMetrics();
        int i=0;
        for (Iterator iterator = singleMetrics.iterator(); iterator.hasNext();) {
			String string = (String) iterator.next();
			boolean checkboxval = dialog.getBoolean(string);
			this.useMetrics.set(i,checkboxval);
			i++;
        }
        this.bufferValue = dialog.getDouble(sBufferRadius);
        boolean bufferCheckBox = dialog.getBoolean(sBuffer);
        if (bufferCheckBox == true){
        	//-- use MBR-length based buffer
        	this.bufferType = 2;
        }
        else{//-- absolute buffer
        	this.bufferType = 1;
        }
        this.outputBufferGeoms = dialog.getBoolean(this.sBufferGeoms);
    }
    
}
