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
 * created:  		08.July.2010
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JMenuItem;

import org.openjump.core.apitools.FeatureCollectionTools;
import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.LayerEventType;
import com.vividsolutions.jump.workbench.model.LayerManager;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.ThreadedPlugIn;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.LayerViewPanel;
import com.vividsolutions.jump.workbench.ui.LayerViewPanelContext;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;
import com.vividsolutions.jump.workbench.ui.plugin.FeatureInstaller;
import com.vividsolutions.jump.workbench.ui.renderer.RenderingManager;

/**
 * @description:
 *	
 * @author sstein
 *
 **/
public class AsymptoteMoviePlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Displays the Home Ranges created with the Asymptote Anlysis Function";   
    private final String sLAYERPTS = "Layer with Home Ranges";
    private FeatureCollection points = null;        
    private Layer input = null;
    private MultiInputDialog dialog;
    private PlugInContext context = null;
    private JComboBox jcb_attributeA = null;
    
	private String locAttribute = "";
    private String sATTRIBUTEA = "time attribute [for ordering of features]";
    
	private RenderingManager renderingManager;
	private LayerViewPanel panel = null;
        
    public void initialize(PlugInContext context) throws Exception {

        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HR Analysis"}, 	//menu path
                    this,
                    new JMenuItem("Asymptote Movie...", null),
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
        	this.getDialogValues(dialog); 
        }
        return true;	    
	}
    
    public void run(TaskMonitor monitor, PlugInContext context) throws Exception{            		
	    	System.gc(); //flush garbage collector
	    	this.context = context;
	    	monitor.allowCancellationRequests();
	    	FeatureCollection resultC = new FeatureDataset(this.points.getFeatureSchema());
	    	//-- clone the original layers style & make invisible
	        Collection stylesBuffer = this.input.cloneStyles();
	        this.input.setVisible(false);
	    	//-- order the objects
			ArrayList<Feature> sortedFeatures = FeatureCollectionTools.sortFeatureListByAttributeBeginWithSmallest(this.points.getFeatures(), 
					locAttribute);
			//-- add the first object
			Feature oldFeature = sortedFeatures.get(0);
			Feature oldoldFeature = oldFeature;
	    	resultC.add(oldFeature);
	    	int numFeatures = sortedFeatures.size();
	    	Layer movieLayer = null;
	    	if((resultC != null) && (resultC.size() > 0)){
	        	movieLayer = context.addLayer(StandardCategoryNames.SYSTEM, "movie", resultC);
	        	movieLayer.setDrawingLast(true);
	        	movieLayer.setStyles(stylesBuffer);
	        	for (int i = 1; i < numFeatures; i++) {
	        		monitor.report(i, numFeatures, "");
	        		if(monitor.isCancelRequested()){
	        	    	//-- remove the temporary layer & make the original layer visible
	        	    	LayerManager lm = movieLayer.getLayerManager();
	        	    	lm.remove(movieLayer);
	        	        this.input.setVisible(true);
	        			return;
	        		}
	        		Feature newFeature = sortedFeatures.get(i);
					movieLayer.setFeatureCollectionModified(false);
					//-- remove the secondLast item
					//   hence we will always display the new and the old item
					//   this may be smoother?
					if(i > 1){
						resultC.remove(oldoldFeature);	
					}
					//-- add the next item
					resultC.add(newFeature);
					movieLayer.setFeatureCollectionModified(true);
					movieLayer.fireLayerChanged(LayerEventType.APPEARANCE_CHANGED);
					oldoldFeature = oldFeature;
					oldFeature = newFeature;
					//-- waste some time by drawing the same thing again completly new 
					// to ensure rendering is finished
					/**
					 * TODO:
					 * put stuff for testing in a function for now. 
					 * The problem is to get info about 
					 * when rendering is finished. Larry said that SkyJUMP has a listener 
					 * for that - but this one works not in all cases. [see his email from 
					 * 9 July 2010: Plug-in showing change over time]
					 */
					//this.wasteTime(resultC);
					Thread.sleep(30);
				}
	        } 
	    	//-- remove the temporary layer & make the original layer visible
	    	LayerManager lm = movieLayer.getLayerManager();
	    	lm.remove(movieLayer);
	        this.input.setVisible(true);
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Asymptote Movie", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERPTS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        
        List list = FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(context.getCandidateLayer(0));
        Object valA = list.size()>0?list.iterator().next():null;
        jcb_attributeA = dialog.addComboBox(this.sATTRIBUTEA, valA, list,this.sATTRIBUTEA);
        if (list.size() == 0) jcb_attributeA.setEnabled(false);
        
        dialog.getComboBox(this.sLAYERPTS).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometryAndString();
                if (list.size() == 0) {
                    jcb_attributeA.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeA.setEnabled(false);
                }
                jcb_attributeA.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
            }            
        });
 	    //dialog.addDoubleField(T1, 20.0, 4);
        GUIUtil.centreOnWindow(dialog);
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.points= this.input.getFeatureCollectionWrapper();
    	this.locAttribute = dialog.getText(this.sATTRIBUTEA); 
      }
	
    private List getFieldsFromLayerWithoutGeometryAndString() {
        return FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(dialog.getLayer(this.sLAYERPTS));
    }
	
 
	private void wasteTime(FeatureCollection fc) throws InterruptedException{
    	//-- create a new dummy pannel to have a clue when things are drawn
		final Throwable[] throwable = new Throwable[] { null };
		LayerManager dummyLM = new LayerManager();
		LayerViewPanel dummyPanel = 
			new LayerViewPanel( 
					new LayerManager(),
							new LayerViewPanelContext() {
						public void setStatusMessage(String message) {
						}

						public void warnUser(String warning) {
						}

						public void handleThrowable(Throwable t) {
							throwable[0] = t;
						}
					});
		//-- here I render a dummy Layer so I know when things are drawn for sure.
		//-- copied the part below from org.openjump.core.ui.plugin.file.LayerPrinter2
		Layer dummyLayer = dummyLM.addLayer("huhu", "dummyLayer", fc);
		renderingManager = dummyPanel.getRenderingManager();
		renderingManager.renderAll();
		/*
		ThreadQueue runningThreads = renderingManager.getDefaultRendererThreadQueue();
		while (runningThreads.getRunningThreads()>0){
			//-- just wait
			//System.out.println("waiting, i: " + i);
			Thread.sleep(10);
		}
		dummyLM.remove(dummyLayer);
		*/
	}
}