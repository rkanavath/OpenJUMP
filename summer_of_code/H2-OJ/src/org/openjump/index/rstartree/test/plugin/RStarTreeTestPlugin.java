package org.openjump.index.rstartree.test.plugin;


import java.util.Collection;
import java.util.List;

import org.openjump.index.rstartree.RStarTree;
import org.openjump.index.rstartree.RStarTreeItemNode;

import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.ThreadedBasePlugIn;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;

public class RStarTreeTestPlugin extends ThreadedBasePlugIn {


	private String menuName = "Test performance of the R*Tree vs the STRTree";
	private String chooseIndexedLayer = "select layer to index";
	private String chooseComparisonLayer = "select layer to to test for inclusion of each element";
	private String sideBarText = "This is a testing plugin to compare an R*Tree vs an STRTree";
	private String msgIndexing = "Indexing";
	private String msgQuery = "Testing query speed of R*Tree";
	private Layer indexedLayer = null;
	private Layer comparisonLayer = null;

	private static String pluginname = "rstartreetestplugin";


	public RStarTreeTestPlugin() {
		// TODO Auto-generated constructor stub
	}



	public void initialize (PlugInContext context) throws Exception {
		/*
		I18NPlug.setPlugInRessource(pluginname, "org.openjump.tin.i18n.resources.createtinfromvectorlayerplugin");

		//-- initialize country specific strings 
		if (I18NPlug.jumpi18n == true) {
			this.menuName = I18NPlug.get(pluginname, "CreateTinFromVectorLayerPlugin.CreateTinFromVectorLayer");
			this.chooseLayer = I18NPlug.get(pluginname, "CreateTinFromVectorLayerPlugin.SelectPointLayer");
			this.sideBarText = I18NPlug.get(pluginname, "CreateTinFromVectorLayerPlugin.CreatesATINLayerFromADelaunayTriangulationOfSelectedPoints");
			this.msgCreateDG = I18NPlug.get(pluginname, "CreateTinFromVectorLayerPlugin.Triangulating");
			this.msgSaveTIN = I18NPlug.get(pluginname, "CreateTinFromVectorLayerPlugin.SavingTINToDisk");
			this.msgErrorSavingTIN = I18NPlug.get(pluginname, "CreateTinFromVectorLayerPlugin.ErrorSavingTINToDisk");
			this.msgCreatePolys = I18NPlug.get(pluginname, "CreateTinFromVectorLayerPlugin.BuildingTIN");
			this.msgNoPoint = I18NPlug.get(pluginname, "CreateTinFromVectorLayerPlugin.NoPointGeometry");
			this.fileNameBox = I18NPlug.get(pluginname, "CreateTinFromVectorLayerPlugin.FileNameForTIN");
			this.tinFileDescription = I18NPlug.get(pluginname, "CreateTinFromVectorLayerPlugin.OpenJUMPTINFile");
		}
		 */

		context.getFeatureInstaller().addMainMenuItem(this, 
				new String[] {MenuNames.LAYER}, 
				getName(), false, null, 
				createEnableCheck(context.getWorkbenchContext()));


	}

	public static MultiEnableCheck createEnableCheck(WorkbenchContext workbenchContext) {
		EnableCheckFactory checkFactory = new EnableCheckFactory(workbenchContext);

		return new MultiEnableCheck()
		.add(checkFactory.createAtLeastNLayersMustExistCheck(2));
	}


	public boolean execute(PlugInContext context) throws Exception{
		this.reportNothingToUndoYet(context);

		MultiInputDialog dialog = new MultiInputDialog(
				context.getWorkbenchFrame(), getName(), true);
		setDialogValues(dialog, context);
		GUIUtil.centreOnWindow(dialog);
		dialog.setVisible(true);
		if (! dialog.wasOKPressed()) { return false; }
		getDialogValues(dialog);	    
		return true;
	}

	private void setDialogValues(MultiInputDialog dialog, PlugInContext context){
		dialog.setSideBarDescription(this.sideBarText);	 
		//dialog.setSideBarImage(new ImageIcon( CreateTinFromVectorLayerPlugin.class.getResource( "CreateTinFromVectorLayer.png" ) ));

		dialog.addLayerComboBox(this.chooseIndexedLayer, context.getCandidateLayer(0), null, context.getLayerManager());
		dialog.addLayerComboBox(this.chooseComparisonLayer, context.getCandidateLayer(0), null, context.getLayerManager());

	}

	private void getDialogValues(MultiInputDialog dialog) {
		this.indexedLayer = dialog.getLayer(this.chooseIndexedLayer);
		this.comparisonLayer = dialog.getLayer(this.chooseComparisonLayer);
	}


	public String getName() {
		return this.menuName;
	}

	public void run(TaskMonitor monitor, PlugInContext context) throws Exception{            		
		this.compareRTrees(context, monitor);
		System.gc();    		
	}



	private void compareRTrees(PlugInContext context, TaskMonitor monitor) {
		System.gc(); //flush garbage collector

		final Collection<Feature> indexedFeatures = this.indexedLayer.getFeatureCollectionWrapper().getFeatures();
		final Collection<Feature> comparisonFeatures = this.comparisonLayer.getFeatureCollectionWrapper().getFeatures();

		// two indexes
		RStarTree rstarindex = new RStarTree();
		STRtree strindex = new STRtree(256);

		// timers
		long rstartimer;
		long strtimer;

		int i=0;
		int numFeatures = indexedFeatures.size();



		// load the STRtree
		strtimer = System.currentTimeMillis();
		for (Feature feature : indexedFeatures) {
			i++;
			monitor.report(i, numFeatures, "Loading STRtree");
			strindex.insert(feature.getGeometry().getEnvelopeInternal(), feature);
		}
		strindex.build();
		strtimer = System.currentTimeMillis() - strtimer;

		System.out.println("STRtree load time = "+strtimer);

		monitor.report("Flushing garbage collector");
		System.gc(); //flush garbage collector


		// load the RStarTree
		rstartimer = System.currentTimeMillis();
		i=0;
		for (Feature feature : indexedFeatures) {
			i++;
			//monitor.report(i, numFeatures, "Loading R*Tree");
			rstarindex.insert(feature.getGeometry().getEnvelopeInternal(), feature);
		}
		rstartimer = System.currentTimeMillis() - rstartimer;

		System.out.println("R*Tree load time = "+rstartimer);

		monitor.report("Flushing garbage collector");
		System.gc(); //flush garbage collector

		// report the load times
		monitor.report("R*Tree load time: "+rstartimer+"\tSTRtree load time:"+strtimer);

		// test the query speed 
		// STRtree
		monitor.report("Testing query speed of STRtree");
		strtimer = System.currentTimeMillis();
		for (Feature feature : comparisonFeatures) {
			strindex.query(feature.getGeometry().getEnvelopeInternal());
		}
		strtimer = System.currentTimeMillis() - strtimer;

		// R*Tree
		monitor.report("Testing query speed of R*Tree");
		rstartimer = System.currentTimeMillis();
		for (Feature feature : comparisonFeatures) {
			rstarindex.query(feature.getGeometry().getEnvelopeInternal());
		}
		rstartimer = System.currentTimeMillis() - rstartimer;

		// report query times
		monitor.report("R*Tree query time: "+rstartimer+"\tSTRtree query time:"+strtimer);
		System.out.println("R*Tree query time: "+rstartimer+"\tSTRtree query time:"+strtimer);

		//test query accuracy of R*Tree
		monitor.report("Testing query accuracy of R*Tree");
		List strmatches, rstarmatches;
		for (Feature feature : comparisonFeatures) {
			strmatches = strindex.query(feature.getGeometry().getEnvelopeInternal());
			rstarmatches = rstarindex.query(feature.getGeometry().getEnvelopeInternal());
			if (rstarmatches == null) {
				System.out.println("R*Tree doesn't contain objects that intersect feature "+feature.getAttribute(1));
				//continue;
			}
			if (strmatches == null) {
				System.out.println("STRtree doesn't contain objects that intersect feature "+feature.getAttribute(1));
				//continue;
			}
			if (rstarmatches != null) {
				for (Object match : rstarmatches) {
					if (match instanceof RStarTreeItemNode) {
						RStarTreeItemNode rMatch = (RStarTreeItemNode)match;
						if (!strmatches.contains(rMatch.getItem())) {
							System.out.println("R*Tree returns an object not in STRtree for the same query");
						}
					}
				}
			}

			if (strmatches != null) {
				for (Object match : strmatches) {
					if (match instanceof RStarTreeItemNode) {
						RStarTreeItemNode strMatch = (RStarTreeItemNode)match;
						if (!strmatches.contains(strMatch.getItem())) {
							System.out.println("STRtree returns an object not in R*Tree for the same query");
						}
					}
				}
			}
		}


	}

	/*
	@Override
	public boolean execute(PlugInContext arg0) throws Exception {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String getName() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void initialize(PlugInContext arg0) throws Exception {
		// TODO Auto-generated method stub

	}

	@Override
	public void run(TaskMonitor arg0, PlugInContext arg1) throws Exception {
		// TODO Auto-generated method stub

	}
	 */

}
