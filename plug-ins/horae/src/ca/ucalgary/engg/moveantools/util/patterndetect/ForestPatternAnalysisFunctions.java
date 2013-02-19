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
 * description: the class provides functions for the recognition 
 * and analysis of landscape patterns
 * 	
 *  
 *****************************************************/
 
package ca.ucalgary.engg.moveantools.util.patterndetect;

import java.util.ArrayList;
import java.util.Iterator;

import org.openjump.core.apitools.FeatureCollectionTools;
import org.openjump.core.geomutils.algorithm.GeometryConverter;

import ca.ucalgary.engg.moveantools.ojplugin.hranalysis.ExtractCoreEdgeAndPatchPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hranalysis.ExtractCorridorsPlugIn;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

public class ForestPatternAnalysisFunctions {

	/**
	 * Extracts from the delivered features/polygons I) patches, II) edges, III) perforated and IV) core area.
	 * For a description of these pattern types see: Vogt et al. (Landscape Ecology 22: 171-177, 2007).    
	 * @param pFeatures containing polygon geometries
	 * @param bufferRadius , i.e. the edge width to be used
	 * @param context can be null
	 * @param monitor can be null
	 * @return a collection of features that will contain a new attribute ptype of type string with the classification information
	 * @throws Exception
	 */
	public static FeatureCollection extractCoreEdgeAndPatch(FeatureCollection pFeatures, double bufferRadius, PlugInContext context, TaskMonitor monitor) throws Exception{
	    FeatureCollection resultFC = null;
	    int size = pFeatures.size();
	    GeometryFactory gf = new GeometryFactory(); //needed later for polygons with holes
	    //-- extend featureSchema
	    FeatureSchema fsNew = (FeatureSchema)pFeatures.getFeatureSchema().clone();
	    fsNew.addAttribute(ExtractCoreEdgeAndPatchPlugIn.PTYPE, AttributeType.STRING);
	    resultFC = new FeatureDataset(fsNew);
	    //-- analyse every single object
	    int count=0;
	    for (Iterator iterator = pFeatures.iterator(); iterator.hasNext();) {
	    	if (monitor != null){	    	
		    	if (monitor.isCancelRequested()){
		    		return resultFC;
		    	}	    	
		    		monitor.report(count + "/" + size);
	    	}
	    	count++;
			Feature ftemp = (Feature) iterator.next();
			String type = "n.n.";
			Geometry g = ftemp.getGeometry();
			if(g instanceof Polygon){
				Polygon p=(Polygon)g;			
				//-- check if polygon disappears for inward buffer
				Geometry core = p.buffer(-1*bufferRadius);
				//-- check if it is a patch (no area after inward buffer)
				if (core.getArea() > 0){
					//-- it is not a patch: we have now extracted the core 
					//-- store the core
					type = "core";
					Feature fcore = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
					fcore.setGeometry(core);
					fcore.setAttribute(ExtractCoreEdgeAndPatchPlugIn.PTYPE, type);
					resultFC.add(fcore);
					//-- check if the polygon has holes
					int numHoles = p.getNumInteriorRing();
					if(numHoles == 0){
						//-- if there are no holes just return the edges as difference op(orggeom-core)
						Geometry edge = p.difference(core);
						type = "edge";
						Feature fedge = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
						fedge.setGeometry(edge);
						fedge.setAttribute(ExtractCoreEdgeAndPatchPlugIn.PTYPE, type);
						resultFC.add(fedge);
					}
					else{
						//-- if there are holes we need to check their size
						//   to distinguish between Edge and Perforated
						//-- prepare lists for later union 
						ArrayList<Geometry> holeEdges = new ArrayList<Geometry>();
						ArrayList<Geometry> holePerforated = new ArrayList<Geometry>();
						ArrayList<Geometry> toSubtract = new ArrayList<Geometry>(); //-- to subtract from outerEdge 
						for (int i = 0; i < numHoles; i++){//iterate over all holes
							LineString ringLS = p.getInteriorRingN(i);
							LinearRing ringLR = gf.createLinearRing(ringLS.getCoordinates());
							//-- make a polygon from the hole, as it is conceptually easier
							Polygon phole = gf.createPolygon(ringLR, null); 
							//-- replace this by toSubtract.add(bufferPos) below (i.e. uncomment)
							//   if outer-edges should be overruled by hole-edges/perforated
							//   now all three edge types are allowed to overlap
							//   the overlaps could be later used to identify corridors 
							toSubtract.add(phole);
							Geometry bufferHole = phole.buffer(-1*bufferRadius);							
							if(bufferHole.getArea() > 0){
								//-- this is a large hole.. so we need to retrieve an Edge
								// 1. do a positive buffer and get the difference area
								// 2. intersect with original geometry
								Geometry bufferPos = phole.buffer(bufferRadius);
								Geometry edgeBuffer = bufferPos.difference(phole);
								Geometry edgeH = edgeBuffer.intersection(p);
								holeEdges.add(edgeH);
								//toSubtract.add(bufferPos); //use this if edge should be overruled by hole-edges/perforated
								//-- store
								/*
								type = "edgeI";
								Feature fHEdge = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
								fHEdge.setGeometry(edgeH);
								fHEdge.setAttribute(CreateRiittersPatternPlugIn.PTYPE, type);
								resultFC.add(fHEdge);
								*/
							}
							else{
								//-- this is a small hole.. so we need to retrieve Perforated
								// 1. do a positive buffer and get the difference area
								// 2. intersect with original geometry
								Geometry bufferPos = phole.buffer(bufferRadius);
								Geometry perforatedBuffer = bufferPos.difference(phole);
								Geometry perforatedH = perforatedBuffer.intersection(p);
								holePerforated.add(perforatedH);
								//toSubtract.add(bufferPos); //use this if edge should be overruled by hole-edges/perforated
								//-- store
								/*
								type = "perforated";
								Feature fPerf = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
								fPerf.setGeometry(perforatedH);
								fPerf.setAttribute(CreateRiittersPatternPlugIn.PTYPE, type);
								resultFC.add(fPerf);
								*/								
							}
						}//-- end loop over holes
						//-- make union of overlapping result geoms 
						//    (by 1. do union: resulting in multipolys, then explode the multipolys )
						//    ..start with perforated
						ArrayList<Geometry> perfgeoms = new ArrayList<Geometry>();
						if (holePerforated.size() > 0){
							Geometry unionPerf = holePerforated.get(0);
							for (int i=1; i < holePerforated.size(); i++) {
								Geometry gt = holePerforated.get(i);
								unionPerf = unionPerf.union(gt); 
							} 
							perfgeoms = GeometryConverter.explodeGeomsIfMultiG(unionPerf);
							type = "perforated";
							for (Iterator iterator2 = perfgeoms.iterator(); iterator2.hasNext();) {
								Geometry gt = (Geometry) iterator2.next();
								Feature fPerf = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
								fPerf.setGeometry(gt);
								fPerf.setAttribute(ExtractCoreEdgeAndPatchPlugIn.PTYPE, type);
								resultFC.add(fPerf);															
							}
						}
						//	.. do the same for the HoleEdges
						ArrayList<Geometry> edgegeoms = new ArrayList<Geometry>();
						if (holeEdges.size() > 0){
							Geometry unionEdge = holeEdges.get(0);
							for (int i=1; i < holeEdges.size(); i++) {
								Geometry gt = holeEdges.get(i);
								unionEdge = unionEdge.union(gt); 
							} 
							edgegeoms = GeometryConverter.explodeGeomsIfMultiG(unionEdge);
							type = "edgeI";
							for (Iterator iterator2 = edgegeoms.iterator(); iterator2.hasNext();) {
								Geometry gt = (Geometry) iterator2.next();
								Feature fE = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
								fE.setGeometry(gt);
								fE.setAttribute(ExtractCoreEdgeAndPatchPlugIn.PTYPE, type);
								resultFC.add(fE);															
							}						
						}
						//----------------------------------
						//-- now add the outer edge
						LineString ringLS = p.getExteriorRing();
						LinearRing ringLR = gf.createLinearRing(ringLS.getCoordinates());
						//-- make a polygon from the hole, as it is conceptually easier
						Polygon pouter = gf.createPolygon(ringLR, null); 
						Geometry coreOuter = pouter.buffer(-1*bufferRadius);							
						
						//-- get the edge as difference op(original-buffer)
						//Geometry edgeOuter = p.difference(core);
						Geometry edgeOuter = pouter.difference(coreOuter);					
						//-- subtract all inner edges/perforated
						for (Iterator iterator2 = toSubtract.iterator(); iterator2.hasNext();) {
							Geometry holeAndEdge = (Geometry) iterator2.next();
							edgeOuter = edgeOuter.difference(holeAndEdge);
						}						
						//-- store
						type = "edge";
						Feature fOEdge = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
						fOEdge.setGeometry(edgeOuter);
						fOEdge.setAttribute(ExtractCoreEdgeAndPatchPlugIn.PTYPE, type);
						resultFC.add(fOEdge);						
						/*
						type = "coreOuter";
						Feature fOEdge2 = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
						fOEdge2.setGeometry(coreOuter);
						fOEdge2.setAttribute(CreateRiittersPatternPlugIn.PTYPE, type);
						resultFC.add(fOEdge2);
						type = "pOuter";						
						Feature fOEdge3 = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
						fOEdge3.setGeometry(pOuter);
						fOEdge3.setAttribute(CreateRiittersPatternPlugIn.PTYPE, type);
						resultFC.add(fOEdge3);
						*/						
					}
				}
				else{
					//-- if it is a patch we are done and need to store only 
					//-- store
					type = "patch";
					Feature fpatch = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
					fpatch.setAttribute(ExtractCoreEdgeAndPatchPlugIn.PTYPE, type);
					resultFC.add(fpatch);
				}
			}
			else{
				if (context != null){
					context.getWorkbenchFrame().warnUser("feature " + ftemp.getID() + " not a polygon");
				}
				else{
					System.out.println("feature " + ftemp.getID() + " not a polygon");
				}
			}
		}
	    //--
		return resultFC;        
	}

	/**
	 * Extracts from the delivered features/polygons I) patches, II) corridors, III) branches.
	 * For a description of these pattern types see for instance: Vogt et al. (Ecological Indicators 7: 481-488, 2007).\n
	 * What is not detected yet are Branches of Corridors and Branches of Shortcuts.   
	 * @TODO: detected Branches of Corridors and Branches of Shortcuts 
	 * 
	 * @param pFeatures containing polygon geometries
	 * @param bufferRadius , i.e. the edge width to be used
	 * @param removeSmall, removes small branches/corridors found iff area(branch) < bufferRadius^2 
	 * @param context can be null
	 * @param monitor can be null
	 * @return a collection of features that will contain a new attribute ptype of type string with the classification information
	 * @throws Exception
	 */
	public static FeatureCollection identifyCooridors(FeatureCollection pFeatures, double bufferRadius, boolean removeSmall, PlugInContext context, TaskMonitor monitor){
	    FeatureCollection resultFC = null;
	    int size = pFeatures.size();
	    double minArea = bufferRadius*bufferRadius; //used later to eliminate small corridors
	    GeometryFactory gf = new GeometryFactory(); //needed later for polygons with holes
	    //-- extend featureSchema
	    FeatureSchema fsNew = (FeatureSchema)pFeatures.getFeatureSchema().clone();
	    fsNew.addAttribute(ExtractCorridorsPlugIn.PTYPE, AttributeType.STRING);
	    resultFC = new FeatureDataset(fsNew);
	    //-- analyse every single object
	    int count=0;
	    for (Iterator iterator = pFeatures.iterator(); iterator.hasNext();) {
	    	count++;	    	
	    	if (monitor != null){	    	
		    	if (monitor.isCancelRequested()){
		    		return resultFC;
		    	}	    	
		    		monitor.report(count + "/" + size);
	    	}
			Feature ftemp = (Feature) iterator.next();
			String type = "n.n.";
			Geometry g = ftemp.getGeometry();
			if(g instanceof Polygon){
				Polygon p=(Polygon)g;
				int numHoles = p.getNumInteriorRing();
				ArrayList<Polygon> allHolesAsP = new ArrayList<Polygon>();
				if (numHoles > 0){
					//-- make polygons from holes, as it is conceptually easier
					for (int j = 0; j < numHoles; j++){
						LineString ringLSh = p.getInteriorRingN(j);
						LinearRing ringLRh = gf.createLinearRing(ringLSh.getCoordinates());
						Polygon phole = gf.createPolygon(ringLRh, null); 
						allHolesAsP.add(phole);
					}				
				}
				//-- create a new polygon from the the outline
				//   we will add corridors that emerge from holes later
				//--------------------------------
				// get exterior corridors
				//--------------------------------
				LineString ringLS = p.getExteriorRing();
				LinearRing ringLR = gf.createLinearRing(ringLS.getCoordinates());
				//-- make a polygon from the hole, as it is conceptually easier
				Polygon pouter = gf.createPolygon(ringLR, null); 
				//-- check if polygon disappears for inward buffer
				Geometry coreOuter = pouter.buffer(-1*bufferRadius);							  
				//-- check if it is a patch (no area after inward buffer)
				if (coreOuter.getArea() > 0){
					//-- it is not a patch: we have now extracted the core
					//-- store the core
					/*
					type = "core";
					Feature fcore = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
					fcore.setGeometry(coreOuter);
					fcore.setAttribute(ExtractCorridorsPlugIn.PTYPE, type);
					resultFC.add(fcore);
					*/					
					//-- check if this is a Branch or a corridor
					//   it is a corridor if the core geometry is a multipolygon
					ArrayList cores = GeometryConverter.explodeGeomsIfMultiG(coreOuter);
					if (cores.size() == 1){
						//-- this will be a Branch of an Edge
						//-- we add a positive buffer to the core to get the original but
						//   the cut-off shapes
						Geometry cutoff = coreOuter.buffer(bufferRadius,1); 
						//-- the difference between original and cutoff will be the outer corridors
						Geometry outerCorridor = pouter.difference(cutoff);
						//-- as this may be multipolygons split them up
						ArrayList corridorGeoms = GeometryConverter.explodeGeomsIfMultiG(outerCorridor);
						//-- store them
						type = "branchEdge";
						boolean store = true;
						for (Iterator iterator2 = corridorGeoms.iterator(); iterator2.hasNext();) {
							Geometry gt = (Geometry) iterator2.next();
							store = true;
							if (removeSmall){
								//-- old test (MBR related)
								//OrientationMBR mbr = new OrientationMBR(gt);
								//mbr.calcWHfromMBR();
								//if (mbr.getMbrLength() < (2*bufferRadius)){
								//-- new test (area related) 
								if (gt.getArea() < minArea){
									store = false;
								}
							}
							if (store){
								Geometry diff = gt; 
								//-- intersection can only be inside, but we need to check that 
								//   holes are still observed 
								for (int j = 0; j < numHoles; j++){
									diff = diff.difference(allHolesAsP.get(j));
								}
								Feature fCor = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
								fCor.setGeometry(diff);
								fCor.setAttribute(ExtractCoreEdgeAndPatchPlugIn.PTYPE, type);
								resultFC.add(fCor);
							}
						}	
					}
					else{
						//-- we identified several cores so this will be a corridor (but with branches too)
						//-- we add a positive buffer to the core to get the original but
						//   the cut-off shapes
						Geometry cutoff = coreOuter.buffer(bufferRadius,1); 
						//-- the difference between original and cutoff will be the outer corridors
						Geometry outerCorridor = pouter.difference(cutoff);
						//-- as this may be multipolygons split them up
						ArrayList corridorGeoms = GeometryConverter.explodeGeomsIfMultiG(outerCorridor);
						//-- store them
						//-- buffer the core back
						Geometry bufferCoreOuter = coreOuter.buffer(bufferRadius,1);
						boolean store = true;
						for (Iterator iterator2 = corridorGeoms.iterator(); iterator2.hasNext();) {
							Geometry gt = (Geometry) iterator2.next();
							store = true;
							//-- check if this is a branch or a corridor by buffering the "piece" (with a 
							//   small distance, i.e. 1/10*BufferRadius) and
							//   test how many intersection geometries are obtained when intersecting
							//   it with the core (multi-)geometry
							//   if it only one intersection geometry, then it is a branch
							//   note: the test fails for hook-shapes branches that return to the core!!!
							Geometry bufferpiece = gt.buffer(0.1*bufferRadius,1);
							Geometry intersect = bufferpiece.intersection(bufferCoreOuter);
							ArrayList allIntersect = GeometryConverter.explodeGeomsIfMultiG(intersect);
							if (allIntersect.size() == 1){
								type = "branchEdge";
							}
							else{
								type = "corridor";
							}
							if (removeSmall){
								//-- old test (MBR related)
								//OrientationMBR mbr = new OrientationMBR(gt);
								//mbr.calcWHfromMBR();
								//if (mbr.getMbrLength() < (2*bufferRadius)){
								//-- new test (area related) 
								if (gt.getArea() < minArea){
									store = false;
								}
							}
							if (store){
								Geometry diff = gt; 
								//-- intersection can only be inside, but we need to check that 
								//   holes are still observed 
								for (int j = 0; j < numHoles; j++){
									diff = diff.difference(allHolesAsP.get(j));
								}
								Feature fCor = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
								fCor.setGeometry(diff);
								//fCor.setGeometry(bufferpiece);
								fCor.setAttribute(ExtractCoreEdgeAndPatchPlugIn.PTYPE, type);
								resultFC.add(fCor);
							}
						}							
					}
					//---------------------------------
					// get interior corridors if holes exists
					// (i.e. intersections between edges)
					//--------------------------------
					ArrayList<Geometry> innerEdges = new ArrayList<Geometry>();
					ArrayList<Geometry> innerEdgesIntersections = new ArrayList<Geometry>();
					if (numHoles > 0){
						//-- get Exterior edge and edges from holes
						Geometry outerEdge = pouter.difference(coreOuter);
						//-- get all inner edges
						for (int i = 0; i < numHoles; i++){//iterate over all holes
							Polygon phole = allHolesAsP.get(i);
							Geometry bufferHole = phole.buffer(bufferRadius,1);
							//-- get inner edge
							Geometry innerEdge = bufferHole.difference(phole);
							//-- test outer-edges against inner-edges for intersection
							Geometry outerIntersect = outerEdge.intersection(innerEdge);
							//-- this can be a multi-polygon
							ArrayList geoms = GeometryConverter.explodeGeomsIfMultiG(outerIntersect);
							type = "cooridorEI";
							for (Iterator iterator2 = geoms.iterator(); iterator2.hasNext();) {
								Geometry gt = (Geometry) iterator2.next();
								if (gt.getArea() > 0){
									//-- subtract holes
									Geometry diff = gt; 
									for (int j = 0; j < numHoles; j++){
										diff = diff.difference(allHolesAsP.get(j));
									}
									Feature fE = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
									fE.setGeometry(diff);
									fE.setAttribute(ExtractCoreEdgeAndPatchPlugIn.PTYPE, type);
									resultFC.add(fE);
								}
							}
							//-- test inner edges against each other
							type = "shortcut";
							if (innerEdges.size() > 0){
								for (Iterator iterator2 = innerEdges.iterator(); iterator2
										.hasNext();) {
									Geometry iETemp = (Geometry) iterator2.next();
									Geometry innerIntersect = innerEdge.intersection(iETemp);
									//-- this can be a multi-polygon
									ArrayList geoms2 = GeometryConverter.explodeGeomsIfMultiG(innerIntersect);
									for (Iterator iterator3 = geoms2.iterator(); iterator3.hasNext();) {
										Geometry gt = (Geometry) iterator3.next();
										if (gt.getArea() > 0){
											innerEdgesIntersections.add(gt);
											/*
											Feature fE = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
											fE.setGeometry(gt);
											fE.setAttribute(ExtractCoreEdgeAndPatchPlugIn.PTYPE, type);
											resultFC.add(fE);
											*/
										}
									}									
								}
							}
							//-- add the inner edge after the check
							innerEdges.add(innerEdge);
						}// end loop over all holes
						//-- union of inner-edges intersections and store
						if (innerEdgesIntersections.size() > 0){
							Geometry unionPerf = innerEdgesIntersections.get(0);
							for (int i=1; i < innerEdgesIntersections.size(); i++) {
								Geometry gt = innerEdgesIntersections.get(i);
								unionPerf = unionPerf.union(gt); 
							} 
							ArrayList<Geometry> perfgeoms = GeometryConverter.explodeGeomsIfMultiG(unionPerf);
							for (Iterator iterator2 = perfgeoms.iterator(); iterator2.hasNext();) {
								Geometry gt = (Geometry) iterator2.next();
								//-- get only those portions that are within the original polygon 								
								Geometry diff = gt.intersection(p);							
								Feature fPerf = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
								fPerf.setGeometry(diff);
								fPerf.setAttribute(ExtractCoreEdgeAndPatchPlugIn.PTYPE, type);
								resultFC.add(fPerf);															
							}
						}
						//---------------------------------
						// get interior branches 
						//--------------------------------
						type="branchInner";
						for (int i = 0; i < numHoles; i++){//iterate over all holes
							Polygon phole = allHolesAsP.get(i);
							Geometry bufferHole = phole.buffer(bufferRadius,1);
							Geometry coreBuffer = bufferHole.buffer(-1*bufferRadius,1);
							Geometry branch = coreBuffer.difference(phole);
							ArrayList branchGeoms = GeometryConverter.explodeGeomsIfMultiG(branch);
							//-- store them
							for (Iterator iterator2 = branchGeoms.iterator(); iterator2.hasNext();) {
								Geometry gt = (Geometry) iterator2.next();
								boolean store = true;
								if (removeSmall){
									//-- old test (MBR related)
									//OrientationMBR mbr = new OrientationMBR(gt);
									//mbr.calcWHfromMBR();
									//if (mbr.getMbrLength() < (2*bufferRadius)){
									//-- new test (area related) 
									if (gt.getArea() < minArea){
										store = false;
									}
								}
								if (store){
									Geometry diff = gt; 
									Feature fCor = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
									fCor.setGeometry(gt);
									fCor.setAttribute(ExtractCoreEdgeAndPatchPlugIn.PTYPE, type);
									resultFC.add(fCor);
								}
							}	
															
						}
					}//-- end if(holes exists)
				}
				else{
					//-- if it is a patch we are done and need to store only 
					//-- store
					
					type = "patch";
					Feature fpatch = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(ftemp, fsNew);
					fpatch.setAttribute(ExtractCorridorsPlugIn.PTYPE, type);
					resultFC.add(fpatch);
					
				}
			}
			else{
				if (context != null){
					context.getWorkbenchFrame().warnUser("feature " + ftemp.getID() + " not a polygon");
				}
				else{
					System.out.println("feature " + ftemp.getID() + " not a polygon");
				}				
			}
		}
	    //--
		return resultFC;        
	}

}
