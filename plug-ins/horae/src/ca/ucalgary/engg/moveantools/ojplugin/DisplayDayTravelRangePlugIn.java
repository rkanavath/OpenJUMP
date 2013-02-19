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
 * created:  		06.Oct.2009
 * last modified:   					
 * 					
 * 
 * @author sstein
 * 
 * description:
 * 	
 *  
 *****************************************************/

package ca.ucalgary.engg.moveantools.ojplugin;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JMenuItem;

import org.math.array.DoubleArray;
import org.math.array.StatisticSample;
import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.ui.plot.Plot2DPanelOJ;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.KernelDensityUtil;
import ca.ucalgary.engg.moveantools.util.TrackCalculationUtil;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jump.feature.Feature;
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
 * @description: displays the range traveled per day either as MBR or circle geometry.
 *	
 * @author sstein
 *
 **/
public class DisplayDayTravelRangePlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Visualizes the points of the same day by creating an MBR of the convex " +
    		"hull (day). If there are only 2 points per day, then a circle is drawn.";   
    private final String sLAYERPTS = "Layer with Points";
    private String sATTRIBUTEA = "Location Attribute (for sorting)";
    private String sATTRIBUTEDay = "Day Attribute (for avg. travel distance, no string fields, unique per year!)";
    
	private String locAttribute = "";
	private String dayAttribute = "";
    private FeatureCollection points = null;        
    private Layer input = null;
    private MultiInputDialog dialog;
    private PlugInContext context = null;
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN"}, 	//menu path
                    this,
                    new JMenuItem("Display Day Travel Ranges...", null),
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
	    	//-- sort points by location
	    	FeatureCollection sortedPoints = TrackCalculationUtil.sortPointsByLocationId(this.points, this.locAttribute);
	    	//-- generate tracks
	    	FeatureCollection singleLines = TrackCalculationUtil.convertToLines(sortedPoints, 
	    			this.locAttribute, this.dayAttribute, context, monitor);
	        context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-tracks", singleLines);
	        double[] activityDists = TrackCalculationUtil.calcOneDayActivityRadiusDistances(sortedPoints, this.dayAttribute, 
	        		this.input.getName(), true, context);
	        //-- calculate median
    	    double[] sortvals = DoubleArray.sort(activityDists);
    	    int index = (int)Math.ceil(activityDists.length/2.0);	                
	        double mediandist = sortvals[index-1];
	        double meanDist = StatisticSample.mean(activityDists);
	        //-- calc h_ref
	    	double hrefNormal = KernelDensityUtil.calculateHref(this.input);
	        //-- calc Leave one out error
	        Coordinate[] pointCoords = new Coordinate[sortedPoints.size()];
	        int i = 0;
	        for (Iterator iterator = sortedPoints.iterator(); iterator.hasNext();) {
				Feature ftemp = (Feature) iterator.next();
				Geometry geom = ftemp.getGeometry();
				if (geom instanceof Point){
					pointCoords[i] = geom.getCoordinate();
				}
				else{
					context.getWorkbenchFrame().warnUser("geom not point, use random vertex, geom-id:" +i);
					//-- not sure this may return an arbitrary vertex
					pointCoords[i] = geom.getCoordinate();
				}
				i++;
			}			
	        double[] errorDists = TrackCalculationUtil.calcLeaveOneOutDistance(pointCoords);
	        //-- calculate median
    	    double[] sortedErrVals = DoubleArray.sort(errorDists);
    	    int indexEV = (int)Math.ceil(sortedErrVals.length/2.0);	                
	        double medianErrDist = sortedErrVals[indexEV-1];
	        double meanErrDist = StatisticSample.mean(errorDists);
	        
	        //--
	        //context.getWorkbenchFrame().warnUser("traveldist median: " + bufferDist + "-- mean: " + meanDist);
	       /*
	        MultiInputDialogWithoutCancel resultDialog = new MultiInputDialogWithoutCancel(context.getWorkbenchFrame(), "Track Statistics", true);
	        resultDialog.addLabel("daily traveldist median: " + mediandist);
	        resultDialog.addLabel("daily traveldist mean: " + meanDist);	        
	        resultDialog.addLabel("h_ref (normal Kernel): " + hrefNormal);
	        resultDialog.addLabel("Leave-One-Oout error median: " + medianErrDist);
	        resultDialog.addLabel("Leave-One-Oout error mean: " + meanErrDist);	  
	        GUIUtil.centreOnWindow(resultDialog);
	        resultDialog.setVisible(true);
	        */
			context.getWorkbenchFrame().getOutputFrame().createNewDocument();
			context.getWorkbenchFrame().getOutputFrame().addText("Track Statistics - layer: " + input.getName());
			context.getWorkbenchFrame().getOutputFrame().addText("GPS points: " + sortedPoints.size());
			context.getWorkbenchFrame().getOutputFrame().addText("different days evaluated: " + activityDists.length);
			context.getWorkbenchFrame().getOutputFrame().addText("daily traveldist median: " + mediandist);
			context.getWorkbenchFrame().getOutputFrame().addText("daily traveldist mean: " + meanDist);
			context.getWorkbenchFrame().getOutputFrame().addText("h_ref (normal Kernel): " + hrefNormal);
			context.getWorkbenchFrame().getOutputFrame().addText("Leave-One-Oout error median: " + medianErrDist);
			context.getWorkbenchFrame().getOutputFrame().addText("Leave-One-Oout error mean: " + meanErrDist);
			//context.getWorkbenchFrame().getOutputFrame().surface();

	    	//-- Display results in a plot
	    	//-- stuff for jmathplot

			ShowTravelDistPlot myScorePlot = new ShowTravelDistPlot(activityDists, mediandist, meanDist);

	    	Plot2DPanelOJ plot = myScorePlot.getPlot();

	    	// FrameView fv = new FrameView(plot);
	    	// -- replace the upper line by:
	    	JInternalFrame frame = new JInternalFrame("Daily Travel Distance Plot");
	    	frame.setLayout(new BorderLayout());
	    	frame.add(plot, BorderLayout.CENTER);
	    	frame.setClosable(true);
	    	frame.setResizable(true);
	    	frame.setMaximizable(true);
	    	frame.setSize(450, 450);
	    	frame.setVisible(true);

	    	context.getWorkbenchFrame().addInternalFrame(frame);
	          		
	        System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Visualize Day MBRs", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERPTS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        List list = FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(context.getCandidateLayer(0));
        Object valA = list.size()>0?list.iterator().next():null;
        Object valB = list.size()>0?list.iterator().next():null;
        final JComboBox jcb_attributeA = dialog.addComboBox(this.sATTRIBUTEA, valA, list,this.sATTRIBUTEA);
        if (list.size() == 0) jcb_attributeA.setEnabled(false);
        final JComboBox jcb_attributeB = dialog.addComboBox(this.sATTRIBUTEDay, valB, list,this.sATTRIBUTEDay);
        if (list.size() == 0) jcb_attributeB.setEnabled(false);
	    //dialog.addDoubleField(T1, 20.0, 4);
        //-- add listener stuff
        dialog.getComboBox(this.sLAYERPTS).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometryAndString();
                if (list.size() == 0) {
                    jcb_attributeA.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeA.setEnabled(false);
                    jcb_attributeB.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeB.setEnabled(false);
                }
                jcb_attributeA.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
                jcb_attributeB.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
            }            
        });
        GUIUtil.centreOnWindow(dialog);
    }
	
    private List getFieldsFromLayerWithoutGeometryAndString() {
        return FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(dialog.getLayer(this.sLAYERPTS));
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.points= this.input.getFeatureCollectionWrapper(); 
        this.locAttribute = dialog.getText(this.sATTRIBUTEA);
        this.dayAttribute = dialog.getText(this.sATTRIBUTEDay);
      }

	
}

final class ShowTravelDistPlot extends JFrame{
	
	Plot2DPanelOJ plot = null;

	public ShowTravelDistPlot(double[] activityDists, double medianDist, double meanDist){
			
		int bins = 20;
		// Build a 2D data set	    
		double[][] datasMed = new double [1][2];
		double[][] datasMean = new double [1][2];
		for (int j = 0; j < datasMed.length; j++) {	
			datasMed[j][0] = medianDist;
			datasMed[j][1] = 1.0;	
			datasMean[j][0] = meanDist;
			datasMean[j][1] = 1.0;
		}	
		double[][] datasDist = new double [activityDists.length][2];
		for (int j = 0; j < datasDist.length; j++) {	
			datasDist[j][0] = activityDists[j];
			datasDist[j][1] = 0.5;	
		}	
		// Build the 2D scatterplot of the datas in a Panel
		// LINE, SCATTER, BAR, QUANTILE, STAIRCASE, (HISTOGRAMM?)		
		Plot2DPanelOJ plot2dA = new Plot2DPanelOJ();
		plot2dA.addHistogramPlot("median travel distances", activityDists, bins);
		plot2dA.addScatterPlot("dists",datasDist);
		plot2dA.addScatterPlot("median",datasMed);
		plot2dA.addScatterPlot("mean",datasMean);
		//====================
		plot2dA.setAxisLabel(0,"daily travel distance [m]");
		plot2dA.setAxisLabel(1,"days");
		// Display a Frame containing the plot panel
		//new FrameView(plot2dA);		
		this.plot = plot2dA;
		
	}   
	
	public Plot2DPanelOJ getPlot(){
		return this.plot;
	}
}
