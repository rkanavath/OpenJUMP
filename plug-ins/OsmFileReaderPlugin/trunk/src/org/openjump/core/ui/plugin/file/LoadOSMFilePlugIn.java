/**
 * @author sstein
 * license GPL
 * 
 * created: 3. July 2013
 */
package org.openjump.core.ui.plugin.file;

import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;

import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.filechooser.FileFilter;

import org.openjump.core.openstreetmap.model.OjOsmPrimitive;
import org.openjump.core.openstreetmap.model.OjOsmRelation;
import org.openjump.core.openstreetmap.model.OjOsmWay;
import org.openjump.core.openstreetmap.reader.OJOsmReader;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MenuNames;

public class LoadOSMFilePlugIn extends AbstractThreadedUiPlugIn{
	
	public static FileFilter OSM_FILE_FILTER = null; 

	private JFileChooser fileChooser;  
	String filePath = "";
	File selFile = null;

	public void initialize(PlugInContext context) throws Exception {	
		
		LoadOSMFilePlugIn.OSM_FILE_FILTER = GUIUtil.createFileFilter("OpenStreetMap osm file", new String[]{"osm"});

        context.getFeatureInstaller().addMainMenuPlugin(
        		this,
                new String[] {MenuNames.PLUGINS}, 	//menu path
                "Load OSM File ...", 
                false,
                null,
                createEnableCheck(context.getWorkbenchContext()), -1);     

		fileChooser = GUIUtil.createJFileChooserWithExistenceChecking();
		fileChooser.setDialogTitle("Choose OSM *.osm file");
		fileChooser.setDialogType(JFileChooser.OPEN_DIALOG);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.setMultiSelectionEnabled(false);
		GUIUtil.removeChoosableFileFilters(fileChooser);
		fileChooser.addChoosableFileFilter(OSM_FILE_FILTER);
		fileChooser.addChoosableFileFilter(GUIUtil.ALL_FILES_FILTER);
		fileChooser.setFileFilter(OSM_FILE_FILTER);
	}
	
	public static MultiEnableCheck createEnableCheck(WorkbenchContext workbenchContext) {
        EnableCheckFactory checkFactory = new EnableCheckFactory(workbenchContext);
        return new MultiEnableCheck()
            .add(checkFactory.createTaskWindowMustBeActiveCheck());
}
	
	public String getName(){
		//return I18N.get("org.openjump.plugin.loadOTPgraph");
		return "Load OSM File";
	}
	
	public boolean execute(PlugInContext context) throws Exception{
		reportNothingToUndoYet(context);
		
		if (JFileChooser.APPROVE_OPTION != fileChooser.showOpenDialog(context
					.getWorkbenchFrame())) {
			return false;
		}
		
        this.selFile = fileChooser.getSelectedFile();
		this.filePath = selFile.getAbsolutePath();
		
		return true;
	}
	
	public void run(TaskMonitor monitor, PlugInContext context) throws Exception {
	
		monitor.allowCancellationRequests();
		
		monitor.report("reading OSM file");

        FileInputStream in = null;
        boolean worked = false;
        ArrayList data = null;
        try {
            in = new FileInputStream(selFile);
            OJOsmReader osmr = new OJOsmReader();
            System.out.println("LoadOSMFilePlugin: Start reading OSM File: " + selFile.getName());
            worked = osmr.doParseDataSet(in, monitor);
            if(worked){
            	data = osmr.getDataset();
            }
            else{
            	return;
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new IOException("File " + selFile.getName() + " does not exist.");
        } finally {
            close(in);
        }
		//create the FeatureSchema
		FeatureSchema fsvx = new FeatureSchema();
		fsvx.addAttribute("Geometry", AttributeType.GEOMETRY);

		String sfieldID = "osm_id";		
		AttributeType t0 = AttributeType.INTEGER;
		fsvx.addAttribute(sfieldID, t0);
		
		String sfieldType = "osm_primitive";		
		AttributeType t1 = AttributeType.STRING;
		fsvx.addAttribute(sfieldType, t1);
	
		String sfieldTime = "timestamp";		
		AttributeType t2 = AttributeType.DATE;
		fsvx.addAttribute(sfieldTime, t2);
		
		String sfieldUser = "osm_user";		
		AttributeType t3 = AttributeType.STRING;
		fsvx.addAttribute(sfieldUser, t3);
		
		String sfieldUserID = "osm_userid";		
		AttributeType t4 = AttributeType.INTEGER;
		fsvx.addAttribute(sfieldUserID, t4);
		
		String sfieldVersion = "osm_version";		
		AttributeType t5 = AttributeType.INTEGER;
		fsvx.addAttribute(sfieldVersion, t5);
				
		String sTags = "osm_tags";		
		AttributeType t6 = AttributeType.STRING;
		fsvx.addAttribute(sTags, t6);

		String sLuType = "lu_type";		
		AttributeType t7 = AttributeType.STRING;
		fsvx.addAttribute(sLuType, t7);
		
		String sName = "name";		
		AttributeType t8 = AttributeType.STRING;
		fsvx.addAttribute(sName, t8);
		
		String sUsedInRelation = "part of relation";		
		AttributeType t9 = AttributeType.STRING;
		fsvx.addAttribute(sUsedInRelation, t9);
		
		String sfieldVisible = "visible";		
		AttributeType t10 = AttributeType.INTEGER;
		fsvx.addAttribute(sfieldVisible, t10);
		
		String sfieldDelete = "action_deleted";		
		AttributeType t11 = AttributeType.INTEGER;
		fsvx.addAttribute(sfieldDelete, t11);
		
		String sfieldModify = "action_modified";		
		AttributeType t12 = AttributeType.INTEGER;
		fsvx.addAttribute(sfieldModify, t12);
		
		String sfieldChangeID = "osm_changeset_id";		
		AttributeType t13 = AttributeType.INTEGER;
		fsvx.addAttribute(sfieldChangeID, t13);

		
		FeatureDataset fdOsmObjects = new FeatureDataset(fsvx);	
		
        for (Iterator iterator = data.iterator(); iterator.hasNext();) {
			OjOsmPrimitive osmPrim = (OjOsmPrimitive) iterator.next();
			
			Feature fNew = new BasicFeature(fsvx);

			fNew.setGeometry(osmPrim.getGeom());
			Long lid = osmPrim.getId();
			fNew.setAttribute(sfieldID, new Integer(lid.intValue()));
			String osmPrimType = osmPrim.getOsmTypeAsString();
			if (osmPrim instanceof OjOsmRelation){
				OjOsmRelation rel = (OjOsmRelation)osmPrim;
				osmPrimType = osmPrimType + " - " + rel.getRelationType();
			}
			fNew.setAttribute(sfieldType, osmPrimType);
			fNew.setAttribute(sfieldTime, osmPrim.getTimestamp());
			fNew.setAttribute(sfieldUser, osmPrim.getUser().getName());
			Long uid = osmPrim.getUser().getId();
			fNew.setAttribute(sfieldUserID, new Integer(uid.intValue()));
			fNew.setAttribute(sfieldVersion, new Integer(osmPrim.getVersion()));
			int valVis = osmPrim.isVisible() ? 1 : 0;
			fNew.setAttribute(sfieldVisible, new Integer(valVis));
			int valDel = osmPrim.isDeleted() ? 1 : 0;
			fNew.setAttribute(sfieldDelete, new Integer(valDel));
			int valMod = osmPrim.isModified() ? 1 : 0;
			fNew.setAttribute(sfieldModify, new Integer(valMod));
			fNew.setAttribute(sfieldChangeID, new Integer(osmPrim.getChangesetId()));
			String tagText = "";
			String nameText = "";
			String luTypeText = "";
			if (osmPrim.hasKeys()){
				tagText = osmPrim.getAllKeyValueTagsAsOneString();
				if(osmPrim.hasKey("name")){
					nameText = osmPrim.get("name");
				}
			}
			if(osmPrim.hasLandUseDescription()){
				luTypeText = osmPrim.getLandUseDescription();
			}
			fNew.setAttribute(sTags, tagText);
			fNew.setAttribute(sName, nameText);
			fNew.setAttribute(sLuType, luTypeText);
			String usedInRelationText = "";
			if(osmPrim.isUsedInARelation()){
				usedInRelationText="yes";
			}
			else{
				usedInRelationText="no";
			}
			if(osmPrim instanceof OjOsmRelation){
				OjOsmRelation rel = (OjOsmRelation)osmPrim;
				if(rel.isMissingMembers()){
					usedInRelationText = usedInRelationText + " - missing own members.";
				}
				else{
					usedInRelationText = usedInRelationText + " - all members found";
				}
			}
			fNew.setAttribute(sUsedInRelation, usedInRelationText);
			
			fdOsmObjects.add(fNew);
			
			if(monitor.isCancelRequested()){
				if(fdOsmObjects.size() > 0){
					context.addLayer(StandardCategoryNames.WORKING, "OSM - stopped", fdOsmObjects);
				}
			}
		}
				
		//display the result FCs
		if(fdOsmObjects.size() > 0){
			context.addLayer(StandardCategoryNames.WORKING, "OSM_" + this.selFile.getName() , fdOsmObjects);
		}
		System.gc(); 
	}
	
    /**
     * <p>Utility method for closing a {@link Closeable} object.</p>
     *
     * @param c the closeable object. May be null.
     */
    public static void close(Closeable c) {
        if (c == null) return;
        try {
            c.close();
        } catch(IOException e) {
            // ignore
        }
    }
	
}
