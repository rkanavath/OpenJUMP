/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI 
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * JUMP is Copyright (C) 2003 Vivid Solutions
 *
 * This program implements extensions to JUMP and is
 * Copyright (C) 2005 Integrated Systems Analysts, Inc.
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
 *
 * Integrated Systems Analysts, Inc.
 * 630C Anchors St., Suite 101
 * Fort Walton Beach, Florida
 * USA
 *
 * (850)862-7321
 */

package com.isa.jump.plugin;

import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.io.datasource.DataSourceQuery;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.datasource.*;

import java.util.Collection;
import java.util.ArrayList;

import javax.swing.JFileChooser;

import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;

/**
 * Adds to the JUMP Workbench the UIs for opening and saving files with the
 * CGDEF format.
 */
public class KMLDataSourceQueryChooserInstallerPlugIn extends AbstractPlugIn {
	
    private static I18N _I18N = I18N.getInstance("com.isa.jump.plugin");
    
    private static final String MAP_PROJECTION        = _I18N.getText("map-projection");
	private static final String CHOOSE_MAP_PROJECTION = _I18N.getText("choose-map-projection");
	private static final String FIRST_CHOICE          = _I18N.getText("lat-long");
	private static final String MISSING_RESOURCE      = _I18N.getText("missing-projection-file");
	
	public static String KMLDESCRIPTION = "KML 2.0";
	public static String COMPRESSED = "Compressed ";
	
	private PlugInContext context;
	private String projectionPath;
	private boolean projectionPathExists = false;
	
	private boolean getUTM = true;

    public void initialize(final PlugInContext context) throws Exception {
    	this.context = context;
    	DataSourceQueryChooserManager.get(context.getWorkbenchContext()
    			.getBlackboard())
				.addSaveDataSourceQueryChooser(new SaveFileDataSourceQueryChooser(
						KMLWriter.KML.class, "KML 2.0",
						InstallStandardDataSourceQueryChoosersPlugIn.extensions(
								KMLWriter.KML.class),
								context.getWorkbenchContext()) {
					public boolean isInputValid() {
						return super.isInputValid();
					}
					
					protected Map toProperties(File file) {
						HashMap properties = new HashMap(super.toProperties(file));
						//getUTM = !getUTM;
						if (getUTM)
							return setUTMParameters(properties);
						else
							return properties;
					}
				});

    	DataSourceQueryChooserManager.get(context.getWorkbenchContext()
    			.getBlackboard()).addLoadDataSourceQueryChooser( 
    					new LoadFileDataSourceQueryChooser(KMLReader.KML.class, KMLDESCRIPTION, 
    							InstallStandardDataSourceQueryChoosersPlugIn.extensions(
    									KMLReader.KML.class), context.getWorkbenchContext()){
    						protected void addFileFilters(JFileChooser chooser) {
    							super.addFileFilters(chooser);
    							chooser.addChoosableFileFilter(GUIUtil.createFileFilter(
    								COMPRESSED + KMLDESCRIPTION, new String[] { "kmz", "gz" }));
     						}
    					});
        
        projectionPath = context.getWorkbenchContext().getWorkbench().getPlugInManager().getPlugInDirectory().getPath();
        projectionPath = projectionPath + File.separator + "kml" + File.separator + "ProjectStringsList.pjl";
        if (new File(projectionPath).exists()) projectionPathExists = true;
	}

    private HashMap setUTMParameters(HashMap properties) {   
        //per LDB projection files only associated with .shp files; confirmed 8/16/05
    	//we are going to look for a projection file
    	//if we find one and it is a UTM projection then we set the properties
    	//if we find one and it is not a UTM projection then return properties with null UTM parameters
    	//this will signal KML writer that it cannot write the KML
    	//if we find no projection files then we ask user for UTM projection data
    	//null UTM parameters mean 'do not write KML'
    	//empty UTM parameters mean 'write KML with no projection transformation'
    	//presumably, source already in lat/long
    	
    	Layer[] layers = context.getWorkbenchContext().getLayerNamePanel().getSelectedLayers();
    	Layer outputLayer = layers[0];
    	DataSourceQuery dsqOut = outputLayer.getDataSourceQuery();
    	
    	if (dsqOut != null)  {
    	    //file exists; not a new layer
    		String outputFileName = dsqOut.getDataSource().getProperties().get("File").toString();
    		if ((outputFileName.toLowerCase()).endsWith(".shp")) {
    			String outputPrjFileName = "";
    			int pos = outputFileName.lastIndexOf('.');
    			outputPrjFileName = outputFileName.substring(0, pos) + ".prj";
    			
    			if ((new File(outputPrjFileName).exists())) {
    				UTM_Projection projection = new UTM_Projection(context, outputPrjFileName);
    				if (projection.isUTM()) {
    					properties.put("UTM_Zone", projection.getZone());
    					properties.put("Central_Meridian", projection.getCentralMeridian());
    				}
    				return properties;
    			}
    			else {
    			    //loop through all layers to find a project file
    	            Collection layerCollection = (Collection) context.getWorkbenchContext().getLayerNamePanel().getLayerManager().getLayers();
    				for (Iterator i = layerCollection.iterator(); i.hasNext();) {
    					Layer layer = (Layer) i.next();
    					DataSourceQuery dsq = layer.getDataSourceQuery();
    					
    					if (dsq != null) {
    						String inputFileName = dsq.getDataSource().getProperties().get("File").toString();
    						
    						if ((inputFileName.toLowerCase()).endsWith(".shp")) {
    							String inputPrjFileName = "";
    							pos = inputFileName.lastIndexOf('.');
    							inputPrjFileName = inputFileName.substring(0, pos) + ".prj";
    							
    							if (new File(inputPrjFileName).exists()) {
    			    				UTM_Projection projection = new UTM_Projection(context, outputPrjFileName);
    			    				if (projection.isUTM()) {
    			    					properties.put("UTM_Zone", projection.getZone());
    			    					properties.put("Central_Meridian", projection.getCentralMeridian());
    			    				}
    			    				return properties;
    							}
    						}
    					}
    				}
    			} 
    		}
    		else if ((outputFileName.toLowerCase()).endsWith(".kml")) {
    		    if (dsqOut.getDataSource().getProperties().get("UTM_Zone") != null) {
    		        // We save the layer in a file which is already a datasource
    		        // of this layer
    		        // Do not ask the projection again 
    		        return properties;
    		    }
    		}
    	}
    	
    	//if here then no projection file found; ask user for projection data
    	//setDialogValues(projectionDialog, context);
    	
    	MultiInputDialog projectionDialog = new MultiInputDialog(context.getWorkbenchFrame(), MAP_PROJECTION, true);
    	Collection methodNames = new ArrayList();
    	methodNames.add(FIRST_CHOICE);
    	if (projectionPathExists) {
    	    for (int i = 1; i <= 60; i++) {
    	        methodNames.add(new String(i + "N"));
    	        methodNames.add(new String(i + "S"));
    	    }
    	}
    	else {
    	    context.getWorkbenchFrame().warnUser(MISSING_RESOURCE);
    	}
    	projectionDialog.addComboBox(CHOOSE_MAP_PROJECTION, FIRST_CHOICE , methodNames, null);
    	GUIUtil.centreOnWindow(projectionDialog);
    	projectionDialog.setVisible(true);
     	
    	if (projectionDialog.wasOKPressed()) { 
    		String UTMZone = getDialogValues(projectionDialog);
    		if (UTMZone.equals(FIRST_CHOICE)) {
    			properties.put("UTM_Zone", "");
    			properties.put("Central_Meridian", "");
    		}
    		else {
    			properties.put("UTM_Zone", UTMZone);
    			String centralMeridian = new UTM_Projection_List(projectionPath).getCentralMeridian(UTMZone);
    			properties.put("Central_Meridian", centralMeridian);
    		}
    	}
    		//else
    		//properties will have null UTM parameters
    		//means cancel write
    	projectionDialog.dispose();
    	return properties;
	}
    
    private String getDialogValues(MultiInputDialog dialog) {
        return dialog.getText(CHOOSE_MAP_PROJECTION);
    }
    
}