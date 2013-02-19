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
 * created:  		12.Nov.2012
 * last modified:   					
 * 					
 * 
 * @author sstein
 * 
 * description: Creates (i) year, (ii) month, (iii) day (iv) unique day, (v) hour of day, and (vi) unique seconds attribute for the datatset and a given date attribute.
 * 	
 *  
 *****************************************************/

package ca.ucalgary.engg.moveantools.ojplugin;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JMenuItem;

import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.TrackCalculationUtil;

import com.vividsolutions.jump.feature.AttributeType;
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
 * @description: Creates (i) year, (ii) month, (iii) day (iv) unique day, (v) hour of day, and (vi) unique seconds attribute for the datatset and a given date attribute.
 *	
 * @author sstein
 *
 **/
public class CreateTimeAttributesFromTimeFieldPlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Given the time field in Date format, the function will create (i) year, (ii) month, (iii) day (iv) unique day, " +
    		"(v) hour of day, and (vi) unique seconds fields for the datatset. Note, the first feature/point in the dataset is used as day 1" +
    		" for unique_day.";   
    private final String sLAYERPTS = "Layer with (GPS) Points";
    private String sATTRIBUTEA = "Recording Time Attribute in 'Date' Format ";
    private String sATTRIBUTEB = "Location Attribute (for sorting)";
    
	private String dayAttribute = "";
	private String locAttribute = "";
    private FeatureCollection points = null;        
    private Layer input = null;
    private MultiInputDialog dialog;
    private PlugInContext context = null;
    
    private static String YEAR = "year";
    private static String MONTH = "month";
    private static String DAY = "day";
    private static String UNIQUEDAYS = "unique_days";
    private static String HOURS = "hoursOfDay";
    private static String UNIQUESECONDS = "unique_seconds"; 
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN"}, 	//menu path
                    this,
                    new JMenuItem("Create (unique) Time Attributes from Date Field...", null),
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
	    	// create new FeatureSchema and Dataset
	    	FeatureCollection resultC = createDatatsetWithNewTimeAttributes(sortedPoints, this.dayAttribute, this.context, monitor);  
	        if(resultC.size() > 0){
	        	context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-time", resultC);
	        }
	        System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Create Time Attributes", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERPTS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        List listA = getDateFields();
        Object valA = listA.size()>0?listA.iterator().next():null;
        final JComboBox jcb_attributeA = dialog.addComboBox(this.sATTRIBUTEA, valA, listA,this.sATTRIBUTEA);
        if (listA.size() == 0) jcb_attributeA.setEnabled(false);
        List listB = FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(context.getCandidateLayer(0));
        Object valB = listB.size()>0?listB.iterator().next():null;
        final JComboBox jcb_attributeB = dialog.addComboBox(this.sATTRIBUTEB, valB, listB,this.sATTRIBUTEB);
        if (listB.size() == 0) jcb_attributeB.setEnabled(false);
        //-- add listener stuff
        dialog.getComboBox(this.sLAYERPTS).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List listA = getDateFields();
                if (listA.size() == 0) {
                    jcb_attributeA.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeA.setEnabled(false);
                }
                jcb_attributeA.setModel(new DefaultComboBoxModel(listA.toArray(new String[0])));
                
                List listB = getFieldsFromLayerWithoutGeometryAndString();
                if (listB.size() == 0) {
                    jcb_attributeB.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeB.setEnabled(false);
                }
                jcb_attributeB.setModel(new DefaultComboBoxModel(listB.toArray(new String[0])));
            }            
        });
        GUIUtil.centreOnWindow(dialog);
    }
	
    private List getDateFields() {
        return getDateFieldsFromLayer(dialog.getLayer(this.sLAYERPTS));
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.points= this.input.getFeatureCollectionWrapper(); 
        this.dayAttribute = dialog.getText(this.sATTRIBUTEA);
        this.locAttribute = dialog.getText(this.sATTRIBUTEB);
      }

    /**
     * extends a feature collection by adding time attributes based on the given date field
     * @param timeAttribute
     * @param timeAttribute
     * @param context can be null
     * @param monitor can be null
     * @return the new featureCollection with the date attributes
     */
	private FeatureCollection createDatatsetWithNewTimeAttributes(
			FeatureCollection pointFeatures, String timeAttribute,
			PlugInContext context, TaskMonitor monitor) {
		
		// create new FeatureSchema
		FeatureSchema fsOld = pointFeatures.getFeatureSchema();
		FeatureSchema fsNew = (FeatureSchema)fsOld.clone();
		fsNew.addAttribute(YEAR, AttributeType.DOUBLE);
		fsNew.addAttribute(MONTH, AttributeType.DOUBLE);
		fsNew.addAttribute(DAY, AttributeType.DOUBLE);
		fsNew.addAttribute(UNIQUEDAYS, AttributeType.DOUBLE);
		fsNew.addAttribute(HOURS, AttributeType.DOUBLE);
		fsNew.addAttribute(UNIQUESECONDS, AttributeType.DOUBLE);
		
		// create the new FC and copy all features and calcualte thd values
		FeatureDataset fd = new FeatureDataset(fsNew);
		boolean exThrown = false;
		
		Calendar calendarDateOfFirstItem = null;
		
		int i = 0;
		for (Iterator iterator = pointFeatures.iterator(); iterator.hasNext();) {
			Feature fOld = (Feature) iterator.next();
			//-- copy old values
			Feature fNew = FeatureSchemaTools.copyFeature(fOld, fsNew);
			//-- read date value
			Date fdate = null;
			try{
				fdate = (Date)fOld.getAttribute(timeAttribute);
			}
			catch(Exception e){
				exThrown = true;
				//eat until end
			}
			//-- calculate and set value
			if(fdate != null){
				Calendar cal = Calendar.getInstance();
				cal.setTime(fdate);
				
				int thisyear = cal.get(Calendar.YEAR);
				if(i == 0){  
					calendarDateOfFirstItem = cal;
				}
				
				fNew.setAttribute(YEAR, new Double(thisyear));
				fNew.setAttribute(MONTH, new Double(1 + cal.get(Calendar.MONTH)));
				fNew.setAttribute(DAY, new Double(cal.get(Calendar.DAY_OF_MONTH)));
				
				//assuming the first time stamp is day number 1
			    long milliseconds1 = calendarDateOfFirstItem.getTimeInMillis();
			    long milliseconds2 = cal.getTimeInMillis();
			    long diff = milliseconds2 - milliseconds1;
			    //long diffSeconds = diff / 1000;
			    long diffDays = diff / (24 * 60 * 60 * 1000);
				int uniqueDays = 1 + (int)Math.floor(diffDays);
				fNew.setAttribute(UNIQUEDAYS, new Double(uniqueDays));
				
				int secs = cal.get(Calendar.SECOND);
				int mins = cal.get(Calendar.MINUTE);
				int hours = cal.get(Calendar.HOUR_OF_DAY);
				double decHours = hours + (mins / 60.0) + (secs / 3600.0);
				fNew.setAttribute(HOURS, new Double(decHours));
				
				double uniqueSeconds = Math.floor(milliseconds2 / 1000.0); 
				fNew.setAttribute(UNIQUESECONDS, new Double(uniqueSeconds));
				fd.add(fNew);
			}
			i++;
		}
		if(exThrown && (context != null)){
			context.getWorkbenchFrame().warnUser("Could not read some or all date fields!");
		}
		return fd;
	}
	
    private List getFieldsFromLayerWithoutGeometryAndString() {
        return FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(dialog.getLayer(this.sLAYERPTS));
    }
    public static List getDateFieldsFromLayer(Layer lyr) {
        List fields = new ArrayList();
        FeatureSchema schema = lyr.getFeatureCollectionWrapper().getFeatureSchema();
        for (int i = 0 ; i < schema.getAttributeCount() ; i++) {
            if (schema.getAttributeType(i) == AttributeType.DATE)
            {
                fields.add(schema.getAttributeName(i));  
           }
        }
        return fields;
    }
}
