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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JMenuItem;

import org.openjump.core.apitools.FeatureCollectionTools;
import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.TrackCalculationUtil;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
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
 * @description: Displays the track of an animal based on points given.
 *	
 * @author sstein
 *
 **/
public class DisplayMovementTracksPlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Displays the tracks of animals (converts points to lines)";   
    private final String sLAYERPTS = "Layer with Points";
    private FeatureCollection points = null;        
    private Layer input = null;
    private MultiInputDialog dialog;
    private PlugInContext context = null;
	private String idAttribute = "";
	private String timeYearAttribute = "";
	private String timeMonthAttribute = "";
	private String timeDayAttribute = "";
	private String locationAttribute = "";
	private String[] timeAttributeNames = new String[3];
    private String sATTRIBUTEA = "Animal id Attribute";
    private String sATTRIBUTElocation = "location field (unique values, subsequently visited)";
    private String sATTRIBUTEyear = "year field";
    private String sATTRIBUTEmonth = "month field";
    private String sATTRIBUTEday = "day field";
    private String sGENERATELAYERS = "one output layer per indiviual";
    private String sGENERATESINGLELINES = "generate single lines (check to avoid display delays)";
    private boolean generateLayerPerObject = false;
    private boolean generateSingleSegments = true;
    
    GeometryFactory gfactory = new GeometryFactory();
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN"}, 	//menu path
                    this,
                    new JMenuItem("Display Movement Tracks...", null),
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
	    	this.timeAttributeNames[0] = this.timeYearAttribute;
	    	this.timeAttributeNames[1] = this.timeMonthAttribute;
	    	this.timeAttributeNames[2] = this.timeDayAttribute;
	    	FeatureCollection resultC = this.convertToLines(this.points, this.idAttribute , this.locationAttribute, this.timeAttributeNames , this.generateLayerPerObject, context, monitor);
	        if(this.generateLayerPerObject == false){
	        	context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-tracks", resultC);
	        }
    	    System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Convert", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERPTS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        List list = FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(context.getCandidateLayer(0));
        Object valA = list.size()>0?list.iterator().next():null;
        final JComboBox jcb_attributeA = dialog.addComboBox(this.sATTRIBUTEA, valA, list,this.sATTRIBUTEA);
        if (list.size() == 0) jcb_attributeA.setEnabled(false);
        final JComboBox jcb_attributeLocation = dialog.addComboBox(this.sATTRIBUTElocation, valA, list,this.sATTRIBUTElocation);
        if (list.size() == 0) jcb_attributeLocation.setEnabled(false); 
        final JComboBox jcb_attributeYear = dialog.addComboBox(this.sATTRIBUTEyear, valA, list,this.sATTRIBUTEyear);
        if (list.size() == 0) jcb_attributeYear.setEnabled(false); 
        final JComboBox jcb_attributeMonth = dialog.addComboBox(this.sATTRIBUTEmonth, valA, list,this.sATTRIBUTEmonth);
        if (list.size() == 0) jcb_attributeMonth.setEnabled(false); 
        final JComboBox jcb_attributeDay = dialog.addComboBox(this.sATTRIBUTEday, valA, list,this.sATTRIBUTEday);
        if (list.size() == 0) jcb_attributeDay.setEnabled(false); 
        dialog.addCheckBox(this.sGENERATELAYERS, this.generateLayerPerObject, this.sGENERATELAYERS);
        dialog.addCheckBox(this.sGENERATESINGLELINES, this.generateSingleSegments, this.sGENERATESINGLELINES);
        
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
        //-- location stuff
        dialog.getComboBox(this.sLAYERPTS).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometryAndString();
                if (list.size() == 0) {
                	jcb_attributeLocation.setModel(new DefaultComboBoxModel(new String[0]));
                	jcb_attributeLocation.setEnabled(false);
                }
                jcb_attributeLocation.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
            }            
        });
        //-- time stuff
        dialog.getComboBox(this.sLAYERPTS).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometryAndString();
                if (list.size() == 0) {
                    jcb_attributeYear.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeYear.setEnabled(false);
                }
                jcb_attributeYear.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
            }            
        });
        dialog.getComboBox(this.sLAYERPTS).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometryAndString();
                if (list.size() == 0) {
                    jcb_attributeMonth.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeMonth.setEnabled(false);
                }
                jcb_attributeMonth.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
            }            
        });
        dialog.getComboBox(this.sLAYERPTS).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometryAndString();
                if (list.size() == 0) {
                    jcb_attributeDay.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeDay.setEnabled(false);
                }
                jcb_attributeDay.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
            }            
        });
        

	    //dialog.addDoubleField(T1, 20.0, 4);
        GUIUtil.centreOnWindow(dialog);
    }
	
    private List getFieldsFromLayerWithoutGeometryAndString() {
        return FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(dialog.getLayer(this.sLAYERPTS));
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.points= this.input.getFeatureCollectionWrapper(); 
        this.idAttribute = dialog.getText(this.sATTRIBUTEA);
        this.timeYearAttribute = dialog.getText(sATTRIBUTEyear); 
        this.timeMonthAttribute = dialog.getText(sATTRIBUTEmonth); 
        this.timeDayAttribute = dialog.getText(sATTRIBUTEday); 
        this.generateLayerPerObject = dialog.getBoolean(this.sGENERATELAYERS); 
        this.generateSingleSegments = dialog.getBoolean(this.sGENERATESINGLELINES);
        this.locationAttribute = dialog.getText(this.sATTRIBUTElocation);
      }
    
	/**
	 * TODO
	 * @param pointFeatures
	 * @param idAttributeName
	 * @param locationAttributeName
	 * @param timeAttributeNames[yearField, monthField, dayField]
	 * @param context used to draw several result layers.
	 * @param monitor can be null
	 * @return lines in a FeatureCollection.
	 */
    private FeatureCollection convertToLines(FeatureCollection pointFeatures, String idAttributeName, 
    		String locationAttributeName, String[] timeAttributeNames, boolean individualLayers, PlugInContext context, TaskMonitor monitor) {
    	
    	//-- featureschema for all points in one track
    	FeatureSchema fsNewAllInOne = new FeatureSchema();
    	fsNewAllInOne.addAttribute("geometry", AttributeType.GEOMETRY);
    	fsNewAllInOne.addAttribute("individual_id", AttributeType.INTEGER);
		//-- featureschema for two point one segment
    	FeatureSchema fsNewSingle = new FeatureSchema();
    	fsNewSingle.addAttribute("geometry", AttributeType.GEOMETRY);
    	fsNewSingle.addAttribute("individual_id", AttributeType.INTEGER);
    	fsNewSingle.addAttribute("starttime", AttributeType.DOUBLE);
    	fsNewSingle.addAttribute("endtime", AttributeType.DOUBLE);
    	//--
    	FeatureCollection fcResult = null;
    	if(this.generateSingleSegments){
    		fcResult = new FeatureDataset(fsNewSingle);
    	}
    	else{
    		fcResult = new FeatureDataset(fsNewAllInOne);
    	}
    	//-- convert every point
		Feature firstFeature = (Feature)pointFeatures.getFeatures().get(0);
		if(firstFeature.getGeometry() instanceof Point){
			if(monitor != null){
				monitor.report("generating tracks");
			}	
			//-- sort according to individual id
			if (monitor != null){
				monitor.report("sort by individual");
			}
			ArrayList<Feature>[] individualPts = null;
			Object[] myReturns = FeatureCollectionTools.sortFeaturesIntoListsByAttributeValue(pointFeatures, idAttribute); 
			individualPts = (ArrayList[])myReturns[0]; 
			int[] uniqueValues = (int[])myReturns[1];
			
			//-- sort the individual points by time
			if (monitor != null){
				monitor.report("sort by location/time");
			}
			for (int i = 0; i < individualPts.length; i++) {
				//-- sorting by time is not good, as we don't have the hours stored..
				//individualPts[i] = TrackCalculationUtil.sortFeatureListByTimeBeginWithSmallest(individualPts[i], timeAttributeNames);
				//-- ...so lets sort by location attribute				
				individualPts[i] = FeatureCollectionTools.sortFeatureListByAttributeBeginWithSmallest(individualPts[i], locationAttributeName);
			}
			//-- generate the lines from the points
			if (monitor != null){
				monitor.report("generate tracks");
			}
			for (int i = 0; i < individualPts.length; i++) {
				if(this.generateSingleSegments){
					FeatureCollection lines = createSingleLineFeaturesFromFeatures(individualPts[i], uniqueValues[i], fsNewSingle, timeAttributeNames);
					fcResult.addAll(lines.getFeatures());
					if (individualLayers){
						//-- display
						FeatureCollection fcIndiv = new FeatureDataset(fsNewSingle);
						fcIndiv.addAll(lines.getFeatures());						
						if (context != null){
							context.addLayer(StandardCategoryNames.RESULT, 
								this.input.getName() + "_" + uniqueValues[i], fcIndiv);
						}
					}
				}
				else{
					LineString line = createLineFromFeatures(individualPts[i]);
					if (line != null){
						Feature fnew = new BasicFeature(fsNewAllInOne); 
						fnew.setGeometry(line);
						fnew.setAttribute("individual_id", uniqueValues[i]);
						fcResult.add(fnew);
						if (individualLayers){
							//-- display
							FeatureCollection fcIndiv = new FeatureDataset(fsNewAllInOne);
							fcIndiv.add(fnew.clone(true));
							if (context != null){
								context.addLayer(StandardCategoryNames.RESULT, 
									this.input.getName() + "_" + uniqueValues[i], fcIndiv);
							}
						}
					}
					else{
						System.out.println("convertToLines: could not create LineString");
					}
				}
			}			
		}	
		else{
			if (context != null){
				context.getWorkbenchFrame().warnUser("convertToLines: first feature not a point");
			}
			System.out.println("convertToLines: first feature not a point");
			return null;
		}   
		return fcResult;
	}

    /**
     * creates line features from the points, where ever LineString connects only two points. The animal id and 
     * the time attributes are attached as feature attributes.
     * @param points
     * @param sortingValue
     * @param fsNew
     * @param timeAttributes
     * @return
     */
	private FeatureCollection createSingleLineFeaturesFromFeatures(
			ArrayList<Feature> points, int sortingValue, FeatureSchema fsNew, String[] timeAttributes) {
    	FeatureDataset fd = new FeatureDataset(fsNew);
    	int i = 0; Feature lastFeature = null;
		for (Iterator iterator = points.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			if (i>0){ // start building lines when having the second point
				if(f.getGeometry() instanceof Point){ 
					Coordinate[] coords = new Coordinate[2];
					coords[0] = ((Point)lastFeature.getGeometry()).getCoordinate();
					coords[1] = ((Point)f.getGeometry()).getCoordinate();
					LineString ls = gfactory.createLineString(coords);
					//--create and add the feature
					Feature newF = new BasicFeature(fsNew);
					newF.setGeometry(ls);
					newF.setAttribute("individual_id", sortingValue);

					double starttime = TrackCalculationUtil.getTime(lastFeature, timeAttributeNames[0], timeAttributeNames[1], timeAttributeNames[2]);
					double endtime = TrackCalculationUtil.getTime(f, timeAttributeNames[0], timeAttributeNames[1], timeAttributeNames[2]);
					newF.setAttribute("starttime", starttime);
					newF.setAttribute("endtime", endtime);
					
					fd.add(newF);
					//--
				}
				else{
					System.out.println("createSingleLineFeaturesFromFeatures: feature not a point, ID: " + f.getID());
				}
			}//--end if(i>0)
			lastFeature = f.clone(true);
			i++;
		}
		return fd;
	}

	/**
     * Creates a single LineString out of all points delivered 
     * @param points
     * @return
     */
    private LineString createLineFromFeatures(ArrayList<Feature> points) {
    	LineString ls = null;
    	Coordinate[] coords = new Coordinate[points.size()];
    	int i = 0;
		for (Iterator iterator = points.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			if(f.getGeometry() instanceof Point){ 
				coords[i] = ((Point)f.getGeometry()).getCoordinate();
				i++;
			}
			else{
				System.out.println("createLineFromFeatures: feature not a point, ID: " + f.getID());
			}
		}
		ls = gfactory.createLineString(coords);
		return ls;
	}
  
}
