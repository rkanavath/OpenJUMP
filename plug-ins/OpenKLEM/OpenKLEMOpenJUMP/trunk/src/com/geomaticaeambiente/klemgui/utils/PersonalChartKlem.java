package com.geomaticaeambiente.klemgui.utils;

import it.geomaticaeambiente.klem.SimulationOutput;
import it.geomaticaeambiente.klem.TimeInterval;
import java.awt.BasicStroke;
import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.StandardXYBarPainter;
import org.jfree.chart.renderer.xy.StandardXYItemRenderer;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.DefaultXYDataset;
import org.jfree.data.xy.XYBarDataset;
import org.jfree.data.xy.XYSeries;

/**
 *
 * @author Paola
 */
public class PersonalChartKlem extends PersonalChart {
    
    public PersonalChartKlem(SimulationOutput simOut){
        this.simOut = simOut;
    }
    
    @Override
    public PersonalChart.Type getType() {
        return PersonalChart.Type.KLEM;
    }
    
    @Override
    public TimeInterval.TimeIntervalUnit getGraphUnit() {
        return TimeInterval.TimeIntervalUnit.HOUR;
    }
    
    @Override
    public ChartPanel buildGraph() {        
       
        double rainUpper = 0;
        double dischargeUpper = 0;
        double timeUpper = 0;
        
        // Prepare data matrices
        double[][] totalRainfallData = new double[2][simOut.getSimulationRainfall().getTotalRain().length];
        for(int r=0; r<totalRainfallData[1].length; r++) {
            totalRainfallData[0][r] = simOut.getSimulationRainfall().getTimeInterval().getInterval(TimeInterval.TimeIntervalUnit.HOUR)* r;
            totalRainfallData[1][r] = simOut.getSimulationRainfall().getTotalRain()[r];
            
            if(totalRainfallData[1][r] > rainUpper) {
                rainUpper = totalRainfallData[1][r];                
            }
        }
        
        double[][] effectiveRainfallData = new double[2][simOut.getSimulationRainfall().getEffectiveRain().length];
            for(int d=0; d<simOut.getSimulationRainfall().getEffectiveRain().length; d++) {
                effectiveRainfallData[0][d] = simOut.getSimulationRainfall().getTimeInterval().getInterval(TimeInterval.TimeIntervalUnit.HOUR)*d;
                effectiveRainfallData[1][d] = simOut.getSimulationRainfall().getEffectiveRain()[d];
            }

        double[][] dischargeDataDirect = new double[2][simOut.getSimulationDischarge().getDflo_out().length];
            
            for(int d=0; d<dischargeDataDirect[1].length; d++) {
                dischargeDataDirect[0][d] = simOut.getSimulationDischarge().getTimeInterval().getInterval(TimeInterval.TimeIntervalUnit.HOUR)*d;
                dischargeDataDirect[1][d] = simOut.getSimulationDischarge().getDflo_out()[d];    
            }

        double[][] dischargeDataTotal = new double[2][simOut.getSimulationDischarge().getTflo_out().length];

        for(int d=0; d<dischargeDataTotal[1].length; d++) {
            dischargeDataTotal[0][d] = simOut.getSimulationDischarge().getTimeInterval().getInterval(TimeInterval.TimeIntervalUnit.HOUR)*d;
            dischargeDataTotal[1][d] = simOut.getSimulationDischarge().getTflo_out()[d];
            if(dischargeDataTotal[1][d] > dischargeUpper) {
                dischargeUpper = dischargeDataTotal[1][d];

            }
        }

        double[][] dischargeDataBase = new double[2][simOut.getSimulationDischarge().getBflo_out().length];

        for(int d=0; d<dischargeDataBase[1].length; d++) {
            dischargeDataBase[0][d] = simOut.getSimulationDischarge().getTimeInterval().getInterval(TimeInterval.TimeIntervalUnit.HOUR)*d;
            dischargeDataBase[1][d] = simOut.getSimulationDischarge().getBflo_out()[d];
        }
            
        
        double rainfallBarWidth = totalRainfallData[0][1] - totalRainfallData[0][0];
        
        // Units are mins: convert
        if(getGraphUnit() == TimeInterval.TimeIntervalUnit.MINUTE) {
            rainfallBarWidth *= 60;
            for(int d=0; d<totalRainfallData[0].length; d++) {
                totalRainfallData[0][d] *= 60;
                if(effectiveRainfallData != null) {
                    effectiveRainfallData[0][d] *= 60;
                }
            }
        }
       
        // Discharge dataset        
        XYSeries dischargeSeries = new XYSeries(PluginUtils.getResources().getString("PersonalChartHydrograph.Discharge.label"));
        if(dischargeDataDirect != null) {
            for(int d=0; d<dischargeDataDirect[0].length; d++) {
                dischargeSeries.add(dischargeDataDirect[0][d], dischargeDataDirect[1][d]);
                dischargeSeries.add(dischargeDataTotal[0][d], dischargeDataTotal[1][d]);
                dischargeSeries.add(dischargeDataBase[0][d], dischargeDataBase[1][d]);
            }        
        }
        
        // Units are mins: convert
        boolean start = false;
        for(int i=0; i<dischargeDataDirect[0].length; i++){
            if(dischargeDataDirect[1][i] > 0){
                start = true;
            }
            if(start == true && dischargeDataDirect[1][i] == 0){
                timeUpper = dischargeDataDirect[0][i];
                break;
            }
        }
        
        
        DefaultXYDataset dischargeXyDatasetTotal = new DefaultXYDataset();
        dischargeXyDatasetTotal.addSeries("Qd", dischargeDataDirect);
        dischargeXyDatasetTotal.addSeries("Qt", dischargeDataTotal);
        dischargeXyDatasetTotal.addSeries("Qb", dischargeDataBase);
        
        // Rainfall dataset
        DefaultXYDataset rainfallXyDataset = new DefaultXYDataset();
        rainfallXyDataset.addSeries("PEff", effectiveRainfallData);
        rainfallXyDataset.addSeries("PTot", totalRainfallData);
     
        XYItemRenderer xyItemRenFlow = new StandardXYItemRenderer();
        xyItemRenFlow.setSeriesPaint(0, new Color(255, 0, 0));
        xyItemRenFlow.setSeriesStroke(0, new BasicStroke(2));
        xyItemRenFlow.setSeriesPaint(1, new Color(64, 128, 0));
        xyItemRenFlow.setSeriesStroke(1, new BasicStroke(2));
        xyItemRenFlow.setSeriesPaint(2, new Color(0, 0, 255));
        xyItemRenFlow.setSeriesStroke(2, new BasicStroke(2));
        
        NumberAxis domainAxis = new NumberAxis(PluginUtils.getResources().getString("PersonalChartHydrograph.Date.label"));
        if(getGraphUnit() == TimeInterval.TimeIntervalUnit.MINUTE) {
            domainAxis = new NumberAxis(PluginUtils.getResources().getString("PersonalChartHydrograph.TimeMin.label"));
        }
         
        ValueAxis axis1 = new NumberAxis(PluginUtils.getResources().getString("PersonalChartHydrograph.Discharge.label"));
        axis1.setRange(0, dischargeUpper * 1.3);
        
        XYPlot plot = new XYPlot(dischargeXyDatasetTotal, domainAxis, axis1, xyItemRenFlow);
        
        // Rainfall dataset
        XYItemRenderer xyItemRendRain = new XYBarRenderer();
        XYBarRenderer xyBarRenderer = (XYBarRenderer) xyItemRendRain;
        xyBarRenderer.setDrawBarOutline(false);
        xyBarRenderer.setShadowVisible(false);
        xyBarRenderer.setSeriesPaint(0, new Color(255,0,0));
        if(effectiveRainfallData != null) {
            xyBarRenderer.setSeriesPaint(1, new Color(0,255,0));
        }
        xyBarRenderer.setBarPainter(new StandardXYBarPainter());
        
        XYBarDataset xyBarDatasetRain = new XYBarDataset(rainfallXyDataset, rainfallBarWidth);
        plot.setDataset(1, xyBarDatasetRain);
        plot.setRenderer(1, xyItemRendRain);

        ValueAxis axis2 = new NumberAxis(PluginUtils.getResources().getString("PersonalChartHydrograph.Rainfall.label"));
        axis2.setInverted(true);
        axis2.setRange(0, rainUpper * 3);
        
        plot.setRangeAxis(1, axis2);        
        plot.mapDatasetToRangeAxis(1, 1);
        
        plot.getDomainAxis().setRange(0, timeUpper);

        
        JFreeChart jFreeChart = new JFreeChart(null, JFreeChart.DEFAULT_TITLE_FONT, plot, true);
        ChartPanel chartPanel = new ChartPanel(jFreeChart);
        
        super.chartPanel = chartPanel;
        
        return chartPanel;
    }

    private final SimulationOutput simOut;
    
}
