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
 * created:  		31.May.2010
 * last modified:   					
 * 					
 * 
 * @author sstein
 * 
 * description:
 * 	Displays the Least Squares Cross Validation function
 *  
 *****************************************************/

package ca.ucalgary.engg.moveantools.ojplugin.hrestimator;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JMenuItem;

import org.math.array.DoubleArray;
import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.ui.plot.Plot2DPanelOJ;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.BrownianBridgeUtil;
import ca.ucalgary.engg.moveantools.util.TrackCalculationUtil;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;

/**
 * @description: Displays the Liker function for Brownian Bridge calculation - i.e. the 
 * max value for sigma 1
 *	
 * @author sstein
 *
 **/
public class DisplayLikerFunctionForBBPlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Displays the Sigma 1 Estimation function for the Brownian Bridge";   
    private final String sLAYERPTS = "Layer with Points"; 
    private FeatureCollection points = null;
    private Layer input = null;
    private double sigma2 = 30;
    private double startValue = 0.5;
    private double endValue = 70; //as we use dt=10'000 instead of dt=1 the values should be below 100
    private int steps = 1000;
	private String locAttribute = "";
	private String timeAttribute = "";
    private boolean useDTone = true;
    //private double cellSize = 25;
    
    private MultiInputDialog dialog;
    //private JTextField cellSizeField;
    private JCheckBox useDTOneBox = null;
    private JComboBox jcb_attributeB = null;
    private PlugInContext context = null;
    
    
    private String sSigma2 = "Sigma 2 - i.e. StDev of Point Accuraccy";
    private String sStart = "Start value (note, value will be squared)";
    private String sEnd = "End value (note, value will be squared)";
    private String sSteps = "Number of Grid Points to calculated LSCV function values for";   
    private String sATTRIBUTEA = "Location Attribute (for ordering points, i.e. calc time diff)";
    private String sATTRIBUTETime = "Time Attribute (double)";
    private final String sDTOne = "equal time difference between points, i.e. dT = 10'000 sec";
    //private String sHRefLabel = "h_ref (?normal? Kernel, not biweight K2) = ";  
    //private String sCellSize = "anticipated cellsize; needed to stop the golden search optimization."; 
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HRE", "Brownian Bridge"}, 	//menu path
                    this,
                    new JMenuItem("Display Liker Function for BB...", null),
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
    	
    	if(this.useDTone){
    		this.timeAttribute = null;
    	}
    	FeatureCollection sortedPoints = TrackCalculationUtil.sortPointsByLocationId(this.points, this.locAttribute);
    	//-- get the results
    	double[] gridHValues = new double[this.steps];
    	double start = this.startValue * this.startValue;
    	double end = this.endValue * this.endValue;
    	double stepLength = Math.abs(end - start) / this.steps;
    	for (int i = 0; i < gridHValues.length; i++) {
    		double sigma1 = start + (stepLength * i);
    		gridHValues[i] = sigma1;
    	}
    	int timeAttIndex = 0;
    	if(this.useDTone == false){
    		timeAttIndex = sortedPoints.getFeatureSchema().getAttributeIndex(this.timeAttribute);
    	}
    	Iterator iter = sortedPoints.iterator();
    	double[][] xyloc = new double[sortedPoints.size()+1][3];
    	double[] time = new double[sortedPoints.size()+1];
    	int i = 1;
    	while (iter.hasNext()) {
    		Feature feature = (Feature)iter.next();
    		Geometry geometry = feature.getGeometry();   	  
    		//System.out.println("geometry " + i + " of " + sortedPoints.size());
    		// note, the inout for the adehabitat algorithm start not with 0
    		xyloc[i][1] = geometry.getCoordinate().x;
    		xyloc[i][2] = geometry.getCoordinate().y;	
    		if(this.useDTone){
    			time[i] = (double)i*10000; //we multiply to avoid numerical problems (i.e. e^730 doesn't work)
    		}
    		else{
    			time[i] = feature.getDouble(timeAttIndex);
    		}
    		i++;
    	}

    	double sigma22 = this.sigma2 * this.sigma2;
    	double[] gridScoreValues = BrownianBridgeUtil.calcCVL(xyloc, time, sortedPoints.size(), 
    			gridHValues, gridHValues.length, sigma22);
    	
    	int minIdx = DoubleArray.minIndex(gridScoreValues);
    	double minVal = gridScoreValues[minIdx];
    	double sigma12 = gridHValues[minIdx];
    	double sigma1 = ((int)(Math.sqrt(sigma12)*1000.0))/1000.0;
    	
    	context.getWorkbenchFrame().warnUser("calculated minima sigma 1: " + sigma1 + " - with " +  minVal);
    	
    	//-- Display results in a plot
    	//-- stuff for jmathplot (now replaced by JFreeChart plots

    	ShowLikerScores myScorePlot = new ShowLikerScores(gridHValues, gridScoreValues);

    	Plot2DPanelOJ plot = myScorePlot.getPlot();

    	// FrameView fv = new FrameView(plot);
    	// -- replace the upper line by:
    	JInternalFrame frame = new JInternalFrame("liker function plot");
    	frame.setLayout(new BorderLayout());
    	frame.add(plot, BorderLayout.CENTER);
    	frame.setClosable(true);
    	frame.setResizable(true);
    	frame.setMaximizable(true);
    	frame.setSize(450, 450);
    	frame.setVisible(true);

    	context.getWorkbenchFrame().addInternalFrame(frame);
    	/*
    	    	//----- using JFreeChart ---
    			//-- add the grid data
    			XYSeries series1 = new XYSeries("lscv grid");
    			for (int i = 0; i < gridHValues.length; i++){
    				series1.add(gridHValues[i], gridScoreValues[i]);
    			}
    	    	//-- and the optimization data
    			XYSeries series2 = new XYSeries("lscv opt");
    			for (int i = 0; i < optHValues.length; i++){
    				series2.add(optHValues[i], optScores[i]);
    			}
    			//-- and href
    			XYSeries series3 = new XYSeries("h_ref");
    			series3.add(hRef, hRefScore);
    			//-- and hopt
    			int lastValue = optHValues.length - 1;
    			XYSeries series4 = new XYSeries("h_opt");
    			series4.add(optHValues[lastValue], optScores[lastValue]);

    			XYSeriesCollection dataset1 = new XYSeriesCollection();
    			dataset1.addSeries(series1);
    			dataset1.addSeries(series3);
    			dataset1.addSeries(series4);   			

    			JFreeChart chart = ChartFactory.createXYLineChart(
    				"LSCV plot", 
    				"h values", 
    				"Score values", 
    				dataset1, 
    				PlotOrientation.VERTICAL, 
    				true, 
    				true, 
    				false);

    	    	XYPlot jfplot = chart.getXYPlot();
    			XYLineAndShapeRenderer renderer1 = new XYLineAndShapeRenderer();
    			renderer1.setSeriesLinesVisible(0, true);
    			renderer1.setSeriesShapesVisible(0, false);
    	        renderer1.setSeriesPaint(0, Color.red);
    			renderer1.setSeriesLinesVisible(1, false);
    			renderer1.setSeriesShapesVisible(1, true);
    	        renderer1.setSeriesPaint(1, Color.green);  
    			renderer1.setSeriesLinesVisible(2, false);
    			renderer1.setSeriesShapesVisible(2, true);
    	        renderer1.setSeriesPaint(2, Color.black); 
    	    	jfplot.setRenderer(renderer1);    	     

    	    	//-- add the optimization data with a different renderer
    	    	//   since I want to have correct point connections
    	    	//   somehow this doesn't work (it connects only with the next point)
    			XYSeriesCollection dataset2 = new XYSeriesCollection();
    			dataset2.addSeries(series2);
    			StandardXYItemRenderer renderer2 = new StandardXYItemRenderer(StandardXYItemRenderer.SHAPES_AND_LINES);    	        
    			renderer2.setSeriesPaint(0, Color.blue);
    			//renderer2.setDrawSeriesLineAsPath(true); //this doesn't help and is useful for lots of data = something else
    	        jfplot.setDataset(1, dataset2);
    	        jfplot.setRenderer(1, renderer2);
    			//--
    			jfplot.setSeriesRenderingOrder(SeriesRenderingOrder.FORWARD);
    			jfplot.setDomainCrosshairVisible(true);
    			jfplot.setRangeCrosshairVisible(true);

    			//-- add to OJ UI
    	        ChartPanel jPanelChart = new ChartPanel(chart);

    	        JInternalFrame frameJF = new JInternalFrame("LSCV plot");
    	        frameJF.setLayout(new BorderLayout());
    	        frameJF.add(jPanelChart, BorderLayout.CENTER);
    	        frameJF.setClosable(true);
    	        frameJF.setResizable(true);
    	        frameJF.setMaximizable(true);
    	        frameJF.setSize(500, 400);
    	        frameJF.setVisible(true);

    	        context.getWorkbenchFrame().addInternalFrame(frameJF);
    	 */
    	//--    
    	System.gc();    		
    }

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Calculate Sigma 1 - Liker function", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERPTS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        List list = FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(context.getCandidateLayer(0));
        Object valA = list.size()>0?list.iterator().next():null;
        Object valB = list.size()>0?list.iterator().next():null;
        final JComboBox jcb_attributeA = dialog.addComboBox(this.sATTRIBUTEA, valA, list,this.sATTRIBUTEA);
        if (list.size() == 0) jcb_attributeA.setEnabled(false);
        useDTOneBox = dialog.addCheckBox(sDTOne, useDTone);
        jcb_attributeB = dialog.addComboBox(this.sATTRIBUTETime, valB, list,this.sATTRIBUTETime);
        if (list.size() == 0) jcb_attributeB.setEnabled(false);

        dialog.addDoubleField(this.sSigma2, this.sigma2, 8, this.sSigma2);
        dialog.addIntegerField(this.sSteps, this.steps, 6, this.sSteps);
        dialog.addDoubleField(this.sStart, this.startValue, 8, this.sStart);
        dialog.addDoubleField(this.sEnd, this.endValue, 8, this.sEnd);
        //this.endField = dialog.addDoubleField(this.sCellSize, this.cellSize, 8, this.sCellSize);
	    if (useDTOneBox.isSelected()){
	    	jcb_attributeB.setEnabled(false);
	    }
	    else{
	    	jcb_attributeB.setEnabled(true);
	    }
        //-- add listener stuff
        dialog.getComboBox(this.sLAYERPTS).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometryAndString();
                if (list.size() == 0) {
                    jcb_attributeA.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeA.setEnabled(false);
                    jcb_attributeB.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeB.setEnabled(false);
                }
                jcb_attributeA.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
                jcb_attributeB.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
            }            
        });
 	    //-- add listener
        useDTOneBox.addActionListener(new ActionListener() {
 	        public void actionPerformed(ActionEvent e) {
 	            updateControls();
 	        }
 	    });
        GUIUtil.centreOnWindow(dialog);
    }

	private void updateControls() {
	    if (useDTOneBox.isSelected()){
	    	jcb_attributeB.setEnabled(false);
	    }
	    else{
	    	jcb_attributeB.setEnabled(true);
	    }
	}
	
    private List getFieldsFromLayerWithoutGeometryAndString() {
        return FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(dialog.getLayer(this.sLAYERPTS));
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.points= this.input.getFeatureCollectionWrapper(); 
    	this.sigma2 = dialog.getDouble(this.sSigma2);
    	this.steps = dialog.getInteger(this.sSteps);
    	this.startValue = dialog.getDouble(this.sStart);
    	this.endValue = dialog.getDouble(this.sEnd);
    	this.useDTone = useDTOneBox.isSelected();
        this.locAttribute = dialog.getText(this.sATTRIBUTEA);
        this.timeAttribute = dialog.getText(this.sATTRIBUTETime);
    	//this.cellSize = dialog.getDouble(this.sCellSize);
      }

}

final class ShowLikerScores extends JFrame{
	
	Plot2DPanelOJ plot = null;

	public ShowLikerScores(double[] testedh, double[] testedScores){
			
		// Build a 2D data set	    
		double[][] datas1 = new double [testedh.length][2];
		for (int j = 0; j < testedh.length; j++) {			
			datas1[j][0] = Math.sqrt(testedh[j]);
			datas1[j][1] = testedScores[j];			
		}
    	int minIdx = DoubleArray.minIndex(testedScores);
    	double minVal = testedScores[minIdx];
    	double sigma12 = Math.sqrt(testedh[minIdx]);
    	double[][] datas2 = new double [1][2];
		datas2[0][0] = sigma12;
		datas2[0][1] = minVal;		
		// Build the 2D scatterplot of the datas in a Panel
		// LINE, SCATTER, BAR, QUANTILE, STAIRCASE, (HISTOGRAMM?)		
		Plot2DPanelOJ plot2dA = new Plot2DPanelOJ();
		plot2dA.addLinePlot("Scores",datas1);
		//plot2dA.addScatterPlot("Scores dots",datas1);
		//plot2dA.addLinePlot("Max Score",datas2);
		plot2dA.addScatterPlot("sigma 1",datas2);
		//====================
		plot2dA.setAxisLabel(0,"tested values sigma1");
		plot2dA.setAxisLabel(1,"score values");
		// Display a Frame containing the plot panel
		//new FrameView(plot2dA);		
		this.plot = plot2dA;
		
	}   
	
	public Plot2DPanelOJ getPlot(){
		return this.plot;
	}
	
}