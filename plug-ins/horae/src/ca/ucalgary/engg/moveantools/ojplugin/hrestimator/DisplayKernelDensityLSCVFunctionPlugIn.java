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
 * created:  		24.Nov.2009
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
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JTextField;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.SeriesRenderingOrder;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.StandardXYItemRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.openjump.core.ui.plot.Plot2DPanelOJ;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.KernelDensityLSCV;
import ca.ucalgary.engg.moveantools.util.KernelDensityUtil;

import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;

/**
 * @description: Displays the Least Squares Cross Validation function
 *	
 * @author sstein
 *
 **/
public class DisplayKernelDensityLSCVFunctionPlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Displays the Least Squares Cross Validation function for the " +
    		"selected point dataset.\n Note, the values will be the original ones. However, any " +
    		"LSCV calculation performed with OJs Kernel Density Function will use " +
    		"the Biweight Kernel (K2). " +
    		"In this case the value of h_opt will be multiplied with AK=2.78 to adjust for K2";   
    private final String sLAYERPTS = "Layer with Points";    
    private Layer input = null;
    private double href = 0;
    private double startMultiplier = 0.01;
    private double endMultiplier = 1.5;
    private int steps = 100;
    private double cellSize = 25;
    
    private MultiInputDialog dialog;
    private JTextField stepsField;
    private JTextField startField;
    private JTextField endField;
    private JTextField cellSizeField;
    private PlugInContext context = null;
    private JLabel hrefLabel = null;
    
    private String sMultiplierStart = "Start h_ref Multiplier";
    private String sMultiplierEnd = "End h_ref Multiplier";
    private String sSteps = "Number of Grid Points to calculated LSCV function values for";   
    private String sHRefLabel = "h_ref (?normal? Kernel, not biweight K2) = ";  
    private String sCellSize = "anticipated cellsize; needed to stop the golden search optimization."; 
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HRE", "Point KDE"}, 	//menu path
                    this,
                    new JMenuItem("Display LSCV Function for Dataset...", null),
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

    		KernelDensityLSCV kdlscv = new KernelDensityLSCV();
    		kdlscv.sethStartMultiplierForHref(this.startMultiplier);
    		kdlscv.sethEndMultiplierForHref(this.endMultiplier);
    		kdlscv.setNumberOfHs(this.steps);
    		kdlscv.calculateLSCVFunction(this.input, 
    				this.cellSize, true, context, monitor);
    		//-- get the results
    		double[][] distances = kdlscv.getDistances();
    		double hRef = kdlscv.getHref();
    		double hRefScore = KernelDensityUtil.calculateCVScore(hRef, distances);
    		double[] gridHValues = kdlscv.getGridHValues();
    		double[] gridScoreValues = kdlscv.getGridScoresValues();
    		double[] optHValues = kdlscv.gethValuesFromOptimization();
    		double[] optScores = kdlscv.getScoresFromOptimization();
    		
    		//-- Display results in a plot
    			//-- stuff for jmathplot (now replaced by JFreeChart plots
    			
    	    	ShowScores myScorePlot = new ShowScores(gridHValues, gridScoreValues, 
    	    		hRef, hRefScore, optHValues, optScores);

    	    	Plot2DPanelOJ plot = myScorePlot.getPlot();
	    	
    	        // FrameView fv = new FrameView(plot);
    	        // -- replace the upper line by:
    	        JInternalFrame frame = new JInternalFrame("LSCV plot");
    	        frame.setLayout(new BorderLayout());
    	        frame.add(plot, BorderLayout.CENTER);
    	        frame.setClosable(true);
    	        frame.setResizable(true);
    	        frame.setMaximizable(true);
    	        frame.setSize(450, 450);
    	        frame.setVisible(true);
    	        
    	        context.getWorkbenchFrame().addInternalFrame(frame);
    	        
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
    	    //--    
    	    System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Diplay LSCV", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERPTS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        dialog.getComboBox(this.sLAYERPTS).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateHref();
            }            
        });     
        this.stepsField = dialog.addIntegerField(this.sSteps, this.steps, 6, this.sSteps);
        this.startField = dialog.addDoubleField(this.sMultiplierStart, this.startMultiplier, 
        		8, this.sMultiplierStart);
        this.endField = dialog.addDoubleField(this.sMultiplierEnd, this.endMultiplier, 
        		8, this.sMultiplierEnd);
        this.endField = dialog.addDoubleField(this.sCellSize, this.cellSize, 
        		8, this.sCellSize);
        dialog.addSeparator();
 	    this.hrefLabel = dialog.addLabel(this.sHRefLabel);
 	    this.updateHref(); // to calc the initial h_ref value

 	    GUIUtil.centreOnWindow(dialog);
    }
	
	private void updateHref() {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.href = KernelDensityUtil.calculateHref(this.input);
    	this.sHRefLabel = "h_ref (?normal? Kernel, not biweight K2) = " + this.href; 
    	this.hrefLabel.setText(this.sHRefLabel);
		//System.out.print("process h_ref");
	}
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.steps = dialog.getInteger(this.sSteps);
    	this.startMultiplier = dialog.getDouble(this.sMultiplierStart);
    	this.endMultiplier = dialog.getDouble(this.sMultiplierEnd);
    	this.cellSize = dialog.getDouble(this.sCellSize);
      }
    	
}

final class ShowScores extends JFrame{
	
	Plot2DPanelOJ plot = null;

	public ShowScores(double[] testedh, double[] testedScores, double href, double hrefScore, 
			double[] optimizationHs, double[] optimizationScores){
			
		// Build a 2D data set	    
		double[][] datas1 = new double [testedh.length][2];
		for (int j = 0; j < testedh.length; j++) {			
			datas1[j][0] = testedh[j];
			datas1[j][1] = testedScores[j];			
		}
		double[][] datas2 = new double [1][2];
		datas2[0][0] = href;
		datas2[0][1] = hrefScore;
		double[][] datas3 = new double [optimizationScores.length][2];
		for (int j = 0; j < optimizationScores.length; j++) {			
			datas3[j][0] = optimizationHs[j];
			datas3[j][1] = optimizationScores[j];			
		}
		// Build the 2D scatterplot of the datas in a Panel
		// LINE, SCATTER, BAR, QUANTILE, STAIRCASE, (HISTOGRAMM?)		
		Plot2DPanelOJ plot2dA = new Plot2DPanelOJ();
		plot2dA.addLinePlot("LSCV Scores",datas1);
		//plot2dA.addScatterPlot("LSCV Scores dots",datas1);
		plot2dA.addLinePlot("h_optimization path",datas3);
		plot2dA.addScatterPlot("h_ref",datas2);
		//====================
		plot2dA.setAxisLabel(0,"h values");
		plot2dA.setAxisLabel(1,"Score values");
		// Display a Frame containing the plot panel
		//new FrameView(plot2dA);		
		this.plot = plot2dA;
		
	}   
	
	public Plot2DPanelOJ getPlot(){
		return this.plot;
	}
	
}