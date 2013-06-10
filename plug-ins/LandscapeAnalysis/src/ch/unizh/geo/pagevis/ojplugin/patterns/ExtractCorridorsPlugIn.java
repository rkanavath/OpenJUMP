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
 * created:  		26.June.2008
 * last modified:   					
 * 					
 * 
 * @author sstein
 * 
 * description:
 * 	
 *  
 *****************************************************/

package ch.unizh.geo.pagevis.ojplugin.patterns;

import javax.swing.JComboBox;

import ch.unizh.geo.pagevis.patterndetection.ForestPatternAnalysisFunctions;

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
 * @description: The function extracts corridors, branches, shortcut areas, and patches. Corridors 
    		are narrow areas within habitat polygons. Not yet implemented is the detection of shortcut-branches
    		and corridor branches (see Vogt et al. 2007, Eco. Indicators). The option 'remove small branches' 
    		eliminates those branches with an area smaller than a square with buffer-radius side length.
 *	
 * @author sstein
 *
 **/
public class ExtractCorridorsPlugIn extends AbstractPlugIn implements ThreadedPlugIn{

    private String sBufferDistance ="Buffer Distance";
    private String sRemoveSmall ="remove small branches";
    private String sSidebar ="The function extracts corridors, branches, shortcut areas, and patches. Corridors " +
    		"are narrow areas within habitat polygons. Not yet implemented is the detection of shortcut-branches +" +
    		"and corridor branches (see Vogt et al. 2007, Eco. Indicators). \n" +
    		"the option 'remove small branches' eliminates those branches with an area smaller than a square with" +
    		"buffer-radius side length.";
    public static final String PTYPE ="ptype";
    private double radius = 100;    
    private final String LAYERREGIONS = "Layer with Habitat Polygons";
    private boolean removeSmallCorridors = true; 
    private FeatureCollection regions = null;        
    private Layer input = null;
    private MultiInputDialog dialog;
        
    public void initialize(PlugInContext context) throws Exception {
    				
	        FeatureInstaller featureInstaller = new FeatureInstaller(context.getWorkbenchContext());
	    	featureInstaller.addMainMenuItem(
	    	        this,								//exe
	                new String[] {MenuNames.PLUGINS, "LE-Pattern"}, 	//menu path
	                "Extract Corridors",
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
        	this.radius = dialog.getDouble(sBufferDistance);
        	this.removeSmallCorridors = dialog.getBoolean(sRemoveSmall);
        	this.input =  dialog.getLayer(this.LAYERREGIONS);
        	this.regions = this.input.getFeatureCollectionWrapper();        	
        }
        return true;	    
	}
	
    public void run(TaskMonitor monitor, PlugInContext context) throws Exception{            		
	    	System.gc(); //flush garbage collector
	    	monitor.allowCancellationRequests();
    	    FeatureCollection resultC = ForestPatternAnalysisFunctions.identifyCooridors(this.regions, this.radius, 
    	    		this.removeSmallCorridors, context, monitor);
	        context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-corridors", resultC);
	
    	    System.gc();    		
    	}
		
    private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "extract corrdiors", true);
        dialog.setSideBarDescription(sSidebar);
    	dialog.addDoubleField(sBufferDistance,radius,4,sBufferDistance);
    	dialog.addCheckBox(sRemoveSmall, this.removeSmallCorridors);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.LAYERREGIONS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
	    //dialog.addDoubleField(T1, 20.0, 4);
        GUIUtil.centreOnWindow(dialog);
    }	
}
