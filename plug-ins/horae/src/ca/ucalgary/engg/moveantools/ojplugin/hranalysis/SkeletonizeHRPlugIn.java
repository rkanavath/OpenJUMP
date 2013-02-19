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
 * created:  		29.July.2010
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

import java.io.File;
import java.util.Iterator;
import java.util.List;

import javax.swing.JComboBox;
import javax.swing.JMenuItem;

import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.rasterimage.sextante.OpenJUMPSextanteRasterLayer;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import com.vividsolutions.jts.geom.Polygon;
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

import es.unex.sextante.core.AnalysisExtent;
import es.unex.sextante.core.OutputFactory;
import es.unex.sextante.core.OutputObjectsSet;
import es.unex.sextante.core.ParametersSet;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.dataObjects.IVectorLayer;
import es.unex.sextante.imageAnalysis.thinning.ThinningAlgorithm;
import es.unex.sextante.openjump.core.OpenJUMPOutputFactory;
import es.unex.sextante.openjump.core.OpenJUMPRasterLayer;
import es.unex.sextante.openjump.core.OpenJUMPVectorLayer;
import es.unex.sextante.openjump.init.OJSextanteApiInitialiser;
import es.unex.sextante.outputs.Output;
import es.unex.sextante.rasterize.rasterizeVectorLayer.RasterizeVectorLayerAlgorithm;
import es.unex.sextante.vectorize.contourLines.ContourLinesAlgorithm;
import es.unex.sextante.vectorize.vectorizeLines.VectorizeLinesAlgorithm;

/**
 * @description: Creates skeletons (or: medial axis transform) for input polygons.
 *	
 * @author sstein
 *
 **/
public class SkeletonizeHRPlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Calculates the skeleton for each home range polygon. The Skeletonization is based on thinning a raster image.";   
    private final String sLAYERHR = "Layer with Home Range polygons";
    private final String sCellSize = "Raster cell size [m]";
    
    private double cellSize = 200;
    private FeatureCollection hrPolyFC = null;  
    private Layer inputHR = null;
    private MultiInputDialog dialog;
    private PlugInContext context = null;
    
    private FeatureSchema fsNewInput = null;
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HR Analysis"}, 	//menu path
                    this,
                    new JMenuItem("Skeletonization of HR...", null),
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
			
	    	this.fsNewInput = new FeatureSchema();
			fsNewInput.addAttribute("geometry", AttributeType.GEOMETRY);
			fsNewInput.addAttribute("rastervalue", AttributeType.INTEGER);
			
			FeatureSchema fsNewOutput = null;
			FeatureCollection outputFC = null;
			int counter = 0; boolean fsIsSet = false;
			int numFeatures = this.hrPolyFC.size();
			FeatureCollection featuresWithoutSkeleton = new FeatureDataset(this.hrPolyFC.getFeatureSchema());
			
			if(OJSextanteApiInitialiser.isInitialized == false){
				OJSextanteApiInitialiser.initializeSextante(context);
			}
			
	    	for (Iterator iterator = this.hrPolyFC.iterator(); iterator.hasNext();) {
				Feature ft = (Feature) iterator.next();
				monitor.report(counter, numFeatures, "features skeletonized");
				if (ft.getGeometry() instanceof Polygon){
					//--------------------------
					// rasterize the polygon
					//-------------------------
					//monitor.report("rasterize");
					RasterImageLayer rt = rasterize(ft, this.cellSize); 
					/*
			        if(rt != null){
			        	context.getLayerManager().addLayerable(StandardCategoryNames.RESULT, rt);
			        }
			        */
					
					//---------------------
					// create the skeleton
					//--------------------
					//monitor.report("thinning");
					RasterImageLayer skell = thinning(rt);
					/*
			        if(skell != null){
			        	context.getLayerManager().addLayerable(StandardCategoryNames.RESULT, skell);
			        }
					*/
					// do some stats, to see if we may have an empty raster (due to cell size etc)
					OpenJUMPSextanteRasterLayer rstLayer = new OpenJUMPSextanteRasterLayer();
					rstLayer.create(skell);
					rstLayer.setFullExtent();  // not sure why this needs to be done but it seems to 
													// be necessary (otherwise I get an NPE when 
                    								// doing this.rstLayer.getWindowCellSize())		
					double maxValue = rstLayer.getMaxValue();
					//-- I assume that if maxValue > 0 we have some skeleton
					if (maxValue > 0){
						//-----------------------
						// vectorize the skeleton
						//-----------------------
						//monitor.report("vectorize");
						FeatureCollection fcout = vectorize(skell);
						/*
						if((fcout != null) && (fcout.size() > 0)){
							context.addLayer(StandardCategoryNames.RESULT, "skeleton", fcout);
						}
						*/
						//----------------------
						if(fsIsSet == false){
							fsNewOutput = fcout.getFeatureSchema();
							outputFC = new FeatureDataset(fsNewOutput);
							fsIsSet = true;
						}
						//-- in case there may be null geometries
						//outputFC.addAll(fcout.getFeatures());
						for (Iterator iterator2 = fcout.iterator(); iterator2.hasNext();) {
							Feature ftSkell = (Feature) iterator2.next();
							if(ftSkell.getGeometry() != null){
								ftSkell.setAttribute("ID", ft.getID());
								outputFC.add(ftSkell);
							}
						}
					}
					else{
						featuresWithoutSkeleton.add(ft);
					}
					counter++;
					
					//--------------------------
					// delete the raster files
					//-------------------------
					// (note, the profile tool will not work if files are deleted)
					String rasterFileName = rt.getImageFileName();
					String rasterFileNameTfw = rt.getImageFileName(); rasterFileNameTfw = rasterFileNameTfw.replace(".tif", ".tfw");
					String rasterFileNameThin = skell.getImageFileName();
					String rasterFileNameThinTfw = skell.getImageFileName(); rasterFileNameTfw = rasterFileNameTfw.replace(".tif", ".tfw");
					try{
						File rasterFile = new File(rasterFileName);
						boolean rasterDeleted = rasterFile.delete();
						File rasterFileTfw = new File(rasterFileNameTfw); rasterFileTfw.delete();
						//System.out.println("deleted rasterFile: " + rasterDeleted);
						File rasterFileThin = new File(rasterFileName);
						boolean rasterThinDeleted = rasterFileThin.delete();
						File rasterFileThinTfw = new File(rasterFileNameThinTfw); 
						rasterFileThinTfw.delete();
					}
					catch(Exception e){
						e.printStackTrace();
						//eat
					}
				}
				else{
					context.getWorkbenchFrame().warnUser("Features not Polygons");
					return;
				}
			}
			if((outputFC != null) && (outputFC.size() > 0)){
				context.addLayer(StandardCategoryNames.RESULT, inputHR.getName() + "_skeletons", outputFC);
			}
			context.getWorkbenchFrame().getOutputFrame().createNewDocument();
			context.getWorkbenchFrame().getOutputFrame().addText("Features without Skeleton: " + featuresWithoutSkeleton.size());
	        System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Skeletonization", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERHR, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        dialog.addDoubleField(this.sCellSize, this.cellSize, 8);
        GUIUtil.centreOnWindow(dialog);
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.inputHR =  dialog.getLayer(this.sLAYERHR);
    	this.hrPolyFC= this.inputHR.getFeatureCollectionWrapper(); 
    	this.cellSize = dialog.getDouble(this.sCellSize);
      }
	
	private RasterImageLayer rasterize(Feature ft, double cellSize2) throws Exception {
		OutputFactory outputFactory = new OpenJUMPOutputFactory(context.getWorkbenchContext());
		OpenJUMPVectorLayer ojpolyLayer = new OpenJUMPVectorLayer();
		
		FeatureCollection singleFeature = new FeatureDataset(this.fsNewInput); 
		Feature f = new BasicFeature(this.fsNewInput);
		f.setGeometry(ft.getGeometry());
		f.setAttribute("rastervalue", 1.0);
		singleFeature.add(f);
		Layer polyLayer = new Layer("tracks", context.getLayerManager().generateLayerFillColor(),
				singleFeature, context.getLayerManager());
		//--
		ojpolyLayer.create(polyLayer);
		
		RasterizeVectorLayerAlgorithm alg = new RasterizeVectorLayerAlgorithm();
		
		ParametersSet params = alg.getParameters();
		boolean worked = params.getParameter(RasterizeVectorLayerAlgorithm.LAYER).setParameterValue(ojpolyLayer);
		if(worked){
			params.getParameter(RasterizeVectorLayerAlgorithm.FIELD).setParameterValue(ojpolyLayer.getFieldIndexByName("rastervalue"));			
			//-- we will use a cell size of x meters
			AnalysisExtent extent = new AnalysisExtent(ojpolyLayer);
			extent.setCellSize(cellSize);
			//-- enlarge by kernel size (otherwise the outer points are missed)
			int ntimes = 3; // plus two reserve
			for( int i = 0; i < ntimes; i++){
				extent.enlargeOneCell();
			}
			//-- And now we set the extent as the one to use to create new raster
			// layers within the rasterizing algorithm.
			alg.setAnalysisExtent(extent);
			
			OutputObjectsSet outputs = alg.getOutputObjects();
			Output raster = outputs.getOutput(RasterizeVectorLayerAlgorithm.RESULT);		

			alg.execute(null, outputFactory);		
	
			IRasterLayer result = (IRasterLayer)raster.getOutputObject();
			RasterImageLayer resultOJLayer = (RasterImageLayer)result.getBaseDataObject();

			return resultOJLayer;
		}
		else{
			context.getWorkbenchFrame().warnUser("error in processing");
			return null;
		}	
	}
	
	private FeatureCollection vectorize(RasterImageLayer rt) throws Exception {
		//-- assign the datasources
		OutputFactory outputFactory = new OpenJUMPOutputFactory(context.getWorkbenchContext());		
		OpenJUMPRasterLayer raster = new OpenJUMPRasterLayer();
		raster.create(rt);

		VectorizeLinesAlgorithm alg = new VectorizeLinesAlgorithm();
		ParametersSet params = alg.getParameters();
		params.getParameter(VectorizeLinesAlgorithm.LAYER).setParameterValue(raster);
		
		OutputObjectsSet outputs = alg.getOutputObjects();
		Output result = outputs.getOutput(ContourLinesAlgorithm.RESULT);
		
		alg.execute(null, outputFactory);
		
		IVectorLayer resultLayer = (IVectorLayer)result.getOutputObject();
		OpenJUMPVectorLayer resultOJLayer = (OpenJUMPVectorLayer)result.getOutputObject();
		Layer newResultLayer = (Layer)resultOJLayer.getBaseDataObject();
		List<Feature> features = newResultLayer.getFeatureCollectionWrapper().getWrappee().getFeatures();
		FeatureSchema fsnew = newResultLayer.getFeatureCollectionWrapper().getFeatureSchema();
		FeatureDataset fd = new FeatureDataset(fsnew);
		fd.addAll(features);

		//-delete the files
		String contourFileName = resultOJLayer.getFilename();
		String contourFileNameShx = resultOJLayer.getFilename(); contourFileNameShx = contourFileNameShx.replace(".shp", ".shx");
		String contourFileNameDbf = resultOJLayer.getFilename(); contourFileNameDbf = contourFileNameDbf.replace(".shp", ".dbf");
		try{
			File vectorFile = new File(contourFileName);
			boolean vectorDeleted = vectorFile.delete();
			File vectorFileShx = new File(contourFileNameShx); vectorFileShx.delete();
			File vectorFileDbf = new File(contourFileNameDbf); vectorFileDbf.delete();
			System.out.println("deleted line files: " + vectorDeleted);
		}
		catch(Exception e){
			e.printStackTrace();
			//eat
		}
		return fd;
	}
	
	private RasterImageLayer thinning(RasterImageLayer rt) throws Exception {
		OutputFactory outputFactory = new OpenJUMPOutputFactory(context.getWorkbenchContext());
		
		OpenJUMPRasterLayer ojraster = new OpenJUMPRasterLayer();
		ojraster.create(rt);
		
		ThinningAlgorithm alg = new ThinningAlgorithm();
		
		ParametersSet params = alg.getParameters();
		boolean worked = params.getParameter(ThinningAlgorithm.LAYER).setParameterValue(ojraster);
		if(worked){		
			//-- we will use a cell size of x meters
			AnalysisExtent extent = new AnalysisExtent(ojraster);
			extent.setCellSize(ojraster.getLayerCellSize());
			//-- And now we set the extent as the one to use to create new raster
			// layers within the rasterizing algorithm.
			alg.setAnalysisExtent(extent);
			
			OutputObjectsSet outputs = alg.getOutputObjects();
			Output resraster = outputs.getOutput(ThinningAlgorithm.RESULT);		

			alg.execute(null, outputFactory);		
	
			IRasterLayer result = (IRasterLayer)resraster.getOutputObject();
			RasterImageLayer resultOJLayer = (RasterImageLayer)result.getBaseDataObject();

			return resultOJLayer;
		}
		else{
			context.getWorkbenchFrame().warnUser("error in processing");
			return null;
		}	
	}
}
