package org.openjump.core.ui.plugin.file.osm;

import java.io.Closeable;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.logging.Logger;

import org.openjump.core.openstreetmap.model.OjOsmPrimitive;
import org.openjump.core.openstreetmap.model.OjOsmRelation;
import org.openjump.core.openstreetmap.reader.OJOsmReader;

import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.io.datasource.Connection;
import com.vividsolutions.jump.io.datasource.DataSource;
import com.vividsolutions.jump.task.TaskMonitor;

public class OsmDataSource extends DataSource {

	private static final Logger LOG = Logger.getLogger("org.openjump.core.ui.plugin.file.osm.OsmDataSource");
	
	@Override
	public Connection getConnection() {

            try {

                return new Connection() {

                    public FeatureCollection executeQuery(String query, Collection exceptions, TaskMonitor monitor) {

                        try {
                        	String selFile = (String)getProperties().get(FILE_KEY);
                        	/*
                            csv.setFeatureSchema();
                            FeatureCollection fc = new FeatureDataset(csv.getFeatureSchema());
                            //System.out.println(csv);
                            Iterator<Feature> iterator = csv.iterator();
                            while (iterator.hasNext()) {
                                try {
                                    Feature f = iterator.next();
                                    if (f!=null) {
                                        fc.add(f);
                                    } else {
                                        System.out.println("Could't read feature");
                                    }
                                }
                                catch(Exception e) {
                                    e.printStackTrace();
                                }
                            }
                            exceptions.addAll(csv.getExceptions());
                            */
                        	
                    		monitor.allowCancellationRequests();
                    		
                    		monitor.report("reading OSM file");

                            FileInputStream in = null;
                            boolean worked = false;
                            ArrayList data = null;
                            try {
                                in = new FileInputStream(selFile);
                                OJOsmReader osmr = new OJOsmReader();
                                LOG.info("LoadOSMFilePlugin: Start reading OSM File: " + selFile);
                                worked = osmr.doParseDataSet(in, monitor);
                                if(worked){
                                	data = osmr.getDataset();
                                }
                                else{
                                	return null;
                                }
                            } catch (FileNotFoundException e) {
                                e.printStackTrace();
                                throw new IOException("File " + selFile + " does not exist.");
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
                    					return fdOsmObjects;
                    				}
                    				else{
                    					return null;
                    				}
                    			}
                    		}
                            return fdOsmObjects;

                        } catch (Exception e) {
                            exceptions.add(e);
                            return null;
                        }
                    }
                   
                    
                    public void close() {}
                    
                    /**
                     * <p>Utility method for closing a {@link Closeable} object.</p>
                     *
                     * @param c the closeable object. May be null.
                     */
                    public void close(Closeable c) {
                        if (c == null) return;
                        try {
                            c.close();
                        } catch(IOException e) {
                            // ignore
                        }
                    }
                    
                    public FeatureCollection executeQuery(String query, TaskMonitor monitor) throws Exception {
                        ArrayList exceptions = new ArrayList();
                        FeatureCollection featureCollection = executeQuery(query, exceptions, monitor);
                        if (!exceptions.isEmpty()) {
                            throw (Exception) exceptions.iterator().next();
                        }
                        return featureCollection;
                    }


					@Override
					public void executeUpdate(String query,
							FeatureCollection featureCollection,
							TaskMonitor monitor) throws Exception {
						throw new Exception("Update is not authorized for this DataSource");
						
					}
                };
            }
            catch(Exception e) {
                LOG.throwing("OsmDataSource", "getConnection", e);
                return null;
            }
	}
	
    public boolean isWritable() {
        return false;
    }

}
