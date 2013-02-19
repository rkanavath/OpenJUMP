/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI 
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * JUMP is Copyright (C) 2003 Vivid Solutions
 *
 * This program implements extensions to JUMP and is
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
 * created:  		25.June.2008
 * last modified:   					
 * 					
 * 
 * @author sstein
 * 
 * description:
 * 	
 *  
 *****************************************************/

package ca.ucalgary.engg.moveantools.ojplugin.hranalysis;

import javax.swing.JComboBox;
import javax.swing.JMenuItem;

import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.patterndetect.ForestPatternAnalysisFunctions;

import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;

/**
 * @description: Extracts Core, Patch and Edges for a given polygon layer (see Vogt et al. 2007, Eco. Indicators).
 *	
 * @author sstein
 *
 **/
public class ExtractCoreEdgeAndPatchPlugIn extends AbstractThreadedUiPlugIn{

    private String sBufferDistance ="Buffer Distance";
    private String sSidebar="The Function will classify patches and extracts edges and " +
    		"core area of (forest-)polyogns based on buffer operations." +
    		"The given buffer distance will define the width of the edges.";
    public static final String PTYPE ="ptype";
    private double radius = 100;    
    private final String LAYERREGIONS = "Layer with Forest Polygons"; 
    private FeatureCollection regions = null;        
    private Layer input = null;
    private MultiInputDialog dialog;

    
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HR Analysis"}, 	//menu path
                    this,
                    new JMenuItem("Extract Core, Patch, Edge ...", null),
                    createEnableCheck(context.getWorkbenchContext()), -1); 
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
        	this.input =  dialog.getLayer(this.LAYERREGIONS);
        	this.regions = this.input.getFeatureCollectionWrapper();                	
        }
        return true;	    
	}
	
    public void run(TaskMonitor monitor, PlugInContext context) throws Exception{            		
	    	System.gc(); //flush garbage collector
	    	monitor.allowCancellationRequests();
    	    FeatureCollection resultC = ForestPatternAnalysisFunctions.extractCoreEdgeAndPatch(this.regions, this.radius, context, monitor);
	        context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-classified", resultC);   		
    	}
		
    private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Extract Core, Patch, Edge", true);
        //dialog.setSideBarImage(IconLoader.icon("Overlay.gif"));
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.LAYERREGIONS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
    	dialog.addDoubleField(sBufferDistance,radius,4,sBufferDistance);
	    //dialog.addDoubleField(T1, 20.0, 4);
        GUIUtil.centreOnWindow(dialog);
    }	
}
