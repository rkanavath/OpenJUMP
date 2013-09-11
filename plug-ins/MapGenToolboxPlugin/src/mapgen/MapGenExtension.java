/*
 * Created on 10.01.2005
 *
 */
package mapgen;
import mapgen.ui.onselecteditems.ChangeElongationSelectedBuildingPlugIn;
import mapgen.ui.onselecteditems.DisplaceLinesPlugIn;
import mapgen.ui.onselecteditems.EliminatePointsInLineOfBuildingPlugIn;
import mapgen.ui.onselecteditems.EliminateSmallBuildingsPlugIn;
import mapgen.ui.onselecteditems.BuildingSpreadNarrowPartsPlugIn;
import mapgen.ui.onselecteditems.EnlargeBuildingToRectanglePlugIn;
import mapgen.ui.onselecteditems.LineSimplifyJTS15AlgorithmPlugIn;
import mapgen.ui.onselecteditems.LineSmoothSimpleVersionPlugIn;
import mapgen.ui.onselecteditems.MergeSelectedPolygonsPlugIn;
import mapgen.ui.onselecteditems.SimplifyBuildingToRectanglePlugIn;
import mapgen.ui.onselecteditems.SimplifyOutlineSelectedBuildingPlugIn;
import mapgen.ui.onselecteditems.SquareBuildingPlugIn;

import com.isa.jump.plugin.OrthogonalizePlugIn;
import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import fr.michaelm.jump.plugin.smooth.BezierSmootherPlugIn;

/**
 * @author sstein
 * @author mmichaud
 * @author mbedward
 * @author lbecker
 *   
 *  Loads several functions useful for map generalization 
 */
public class MapGenExtension extends Extension{

    public final static String SMOOTH = "fr.michaelm.jump.plugin.smooth";
    public final static String SMOOTHING = I18N.getText(SMOOTH, "smoothing");
    
    public String getName() {
        return "Map Generalization Toolbox (Stefan Steiniger, Micha&euml;l Michaud, Michael Bedward, Larry Becker)";
    }

    public String getVersion() {
        return "1.1 (2013-09-11)";
    }
    
	/**
	 * calls PlugIn using class method xplugin.initialize() 
	 */
	public void configure(PlugInContext context) throws Exception{
		
		new BuildingSpreadNarrowPartsPlugIn().initialize(context);
		new EnlargeBuildingToRectanglePlugIn().initialize(context);
		new SquareBuildingPlugIn().initialize(context);
		new EliminateSmallBuildingsPlugIn().initialize(context);
		new EliminatePointsInLineOfBuildingPlugIn().initialize(context);
		new SimplifyOutlineSelectedBuildingPlugIn().initialize(context);
		new SimplifyBuildingToRectanglePlugIn().initialize(context);
		new ChangeElongationSelectedBuildingPlugIn().initialize(context);
		new DisplaceLinesPlugIn().initialize(context);
		new LineSmoothSimpleVersionPlugIn().initialize(context);
		new LineSimplifyJTS15AlgorithmPlugIn().initialize(context);
		new MergeSelectedPolygonsPlugIn().initialize(context);
		new BezierSmootherPlugIn().initialize(context);
		new OrthogonalizePlugIn().initialize(context);
	}
	
}
