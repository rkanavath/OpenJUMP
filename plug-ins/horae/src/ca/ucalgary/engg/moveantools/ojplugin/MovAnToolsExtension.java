/*
 * Created on 09.Aug.2010
 * 
 */
package ca.ucalgary.engg.moveantools.ojplugin;

import org.openjump.core.ui.plugin.raster.CreateContourLineFromSelectedImageLayerPlugIn;
import org.openjump.core.ui.plugin.raster.LineKernelDensityPlugIn;

import ca.ucalgary.engg.moveantools.ojplugin.hranalysis.AsymptoteAnalysisPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hranalysis.AsymptoteMoviePlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hranalysis.CalculateCoreBufferDistancePlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hranalysis.CreateCoreAreaFromSelectedImageLayerPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hranalysis.CreateProbabilityContoursFromSelectedImageLayerPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hranalysis.ExtractCoreEdgeAndPatchPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hranalysis.ExtractCorridorsPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hranalysis.SkeletonizeHRPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hrestimator.BrownianBridgeDensityPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hrestimator.BrownianBridgeDensityRasterVersionPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hrestimator.CalculateMinimumConvexPolygonPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hrestimator.DisplayKernelDensityLSCVFunctionPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hrestimator.DisplayLikerFunctionForBBPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hrestimator.GeoEllipseSimplePlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hrestimator.LineBufferbasedHomeRangesPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hrestimator.LineKernelDensityForMovementTracksPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hrestimator.LoCoHHomeRangesPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hrestimator.ScaledLineKernelDensityForMovementTracksPlugIn;
import ca.ucalgary.engg.moveantools.ojplugin.hrestimator.SextanteKernelDensityPlugIn;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 * @author sstein
 *
 *  - this class loads the PlugIn into Jump 
 *
 *  - class has to be called "Extension" on the end of classname
 *    to use the PlugIn in Jump 
 */
public class MovAnToolsExtension extends Extension{

	/**
	 * calls PlugIn using class method xplugin.initialize() 
	 */
	public void configure(PlugInContext context) throws Exception{
		
		//MoveAn menu
		new DisplayMovementTracksPlugIn().initialize(context);
		new DisplayDayTravelRangePlugIn().initialize(context);
		new CreateTimeAttributesFromTimeFieldPlugIn().initialize(context);
		new CalculateMinimumConvexPolygonPlugIn().initialize(context);
		new CreateProbabilityContoursFromSelectedImageLayerPlugIn().initialize(context);
		new CreateCoreAreaFromSelectedImageLayerPlugIn().initialize(context);
		new AsymptoteAnalysisPlugIn().initialize(context);
		new AsymptoteMoviePlugIn().initialize(context);
		new SextanteKernelDensityPlugIn().initialize(context);
		new DisplayKernelDensityLSCVFunctionPlugIn().initialize(context);
		new LineBufferbasedHomeRangesPlugIn().initialize(context);
		new LoCoHHomeRangesPlugIn().initialize(context);
		new LineKernelDensityForMovementTracksPlugIn().initialize(context);
		new ScaledLineKernelDensityForMovementTracksPlugIn().initialize(context);
		new DisplayLikerFunctionForBBPlugIn().initialize(context);
		new BrownianBridgeDensityPlugIn().initialize(context);
		new BrownianBridgeDensityRasterVersionPlugIn().initialize(context);
		new CalculateCoreBufferDistancePlugIn().initialize(context);
		new SkeletonizeHRPlugIn().initialize(context);
		new ExtractCoreEdgeAndPatchPlugIn().initialize(context);
		new ExtractCorridorsPlugIn().initialize(context);
		new GeoEllipseSimplePlugIn().initialize(context);
		
		//OJ-Sextante Menu
		//================
		//new LineKernelDensityPlugIn().initialize(context);
		new CreateContourLineFromSelectedImageLayerPlugIn().initialize(context);
		new LineKernelDensityPlugIn().initialize(context);
	}
	
}
