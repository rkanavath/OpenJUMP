package com.geomaticaeambiente.klemgui.utils;

import com.geomaticaeambiente.openjump.klem.hydrology.Hydrology.EffectiveRainfall;
import com.geomaticaeambiente.openjump.klem.hydrology.UnitHydrograph;
import it.geomaticaeambiente.klem.Hyetograph;
import it.geomaticaeambiente.klem.TimeInterval;
import java.awt.BasicStroke;
import java.awt.Color;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.StandardXYItemRenderer;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.DefaultXYDataset;
import org.jfree.data.xy.XYBarDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 *
 * @author Geomatica
 */
public class PersonalChartHydrograph extends PersonalChart{

    public PersonalChartHydrograph (Hyetograph hyetograph, EffectiveRainfall effectiveRainfall, UnitHydrograph unitHydrograph, TimeInterval.TimeIntervalUnit units){
        this.hyetograph = hyetograph;
        this.effectiveRainfall = effectiveRainfall;
        this.unitHydrograph = unitHydrograph;
        this.units = units;
    }
    
    @Override
    public Type getType() {
        return PersonalChart.Type.HYDROGRAPH;
    }
    
    @Override
    public TimeInterval.TimeIntervalUnit getGraphUnit() {
        return units;
    }
    

    @Override
    public ChartPanel buildGraph() {        
       
        double rainUpper = 0;
        double dischargeUpper = 0;
        
        // Prepare data matrices
        double[][] totalRainfallData = new double[2][hyetograph.getRainfall().length];
        for(int r=0; r<totalRainfallData[1].length; r++) {
            totalRainfallData[0][r] = hyetograph.getStep().getInterval(TimeInterval.TimeIntervalUnit.HOUR) * r;
            totalRainfallData[1][r] = hyetograph.getRainfall()[r];
            
            if(totalRainfallData[1][r] > rainUpper) {
                rainUpper = totalRainfallData[1][r];
            }
        }
        
        double[][] effectiveRainfallData = null;
        if(effectiveRainfall != null) {
            effectiveRainfallData = new double[2][effectiveRainfall.getEffectiveRainfall()[0].length];
            for(int d=0; d<effectiveRainfall.getEffectiveRainfall()[0].length; d++) {
                effectiveRainfallData[0][d] = effectiveRainfall.getEffectiveRainfall()[0][d];
                effectiveRainfallData[1][d] = effectiveRainfall.getEffectiveRainfall()[1][d];
            }
        } 
        double[][] dischargeData = null;
        if(unitHydrograph != null) {
            dischargeData = new double[2][unitHydrograph.getUnitHydrograph()[0].length];
            
            for(int d=0; d<dischargeData[1].length; d++) {
                dischargeData[0][d] = unitHydrograph.getUnitHydrograph()[0][d];
                dischargeData[1][d] = unitHydrograph.getUnitHydrograph()[1][d];
                if(dischargeData[1][d] > dischargeUpper) {
                    dischargeUpper = dischargeData[1][d];
                }
            }
            
        }
        
        double rainfallBarWidth = totalRainfallData[0][1] - totalRainfallData[0][0];
        
        // Units are mins: convert
        if(units == TimeInterval.TimeIntervalUnit.MINUTE) {
            rainfallBarWidth *= 60;
            for(int d=0; d<totalRainfallData[0].length; d++) {
                totalRainfallData[0][d] *= 60;
                if(effectiveRainfallData != null) {
                    effectiveRainfallData[0][d] *= 60;
                }
            }
            
            if(dischargeData != null) {
                for(int d=0; d<dischargeData[0].length; d++) {
                    dischargeData[0][d] *= 60;
                }
            }
        }

        // Discharge dataset        
        XYSeries dischargeSeries = new XYSeries(PluginUtils.getResources().getString("PersonalChartHydrograph.Discharge.label"));
        if(dischargeData != null) {
            for(int d=0; d<dischargeData[0].length; d++) {
                dischargeSeries.add(dischargeData[0][d], dischargeData[1][d]);
            }        
        }
        
        DefaultXYDataset dischargeXyDataset = new DefaultXYDataset();
        if(dischargeData != null) {
            dischargeXyDataset.addSeries("Qd", dischargeData);
        }
        
        XYSeriesCollection dischargeDataset = new XYSeriesCollection();
        dischargeDataset.addSeries(dischargeSeries);
        
        // Rainfall dataset
        DefaultXYDataset rainfallXyDataset = new DefaultXYDataset();
        if(effectiveRainfallData != null) {
            rainfallXyDataset.addSeries(PluginUtils.getResources().getString("PersonalChartHydrograph.EffectiveRain.label"),
                    effectiveRainfallData);
        }
        rainfallXyDataset.addSeries(PluginUtils.getResources().getString("PersonalChartHydrograph.TotalRain.label"),
                totalRainfallData);
     
        XYItemRenderer xyItemRenFlow = new StandardXYItemRenderer();
        xyItemRenFlow.setSeriesPaint(0, new Color(0, 0, 255));
        xyItemRenFlow.setSeriesStroke(0, new BasicStroke(3));
        
        NumberAxis domainAxis = new NumberAxis(PluginUtils.getResources().getString("PersonalChartHydrograph.Date.label"));
        if(units == TimeInterval.TimeIntervalUnit.MINUTE) {
            domainAxis = new NumberAxis(PluginUtils.getResources().getString("PersonalChartHydrograph.TimeMin.label"));
        }
         
        ValueAxis axis1 = new NumberAxis(PluginUtils.getResources().getString("PersonalChartHydrograph.Discharge.label"));
        axis1.setRange(0, dischargeUpper * 1.3);
        
        XYPlot plot = new XYPlot(dischargeDataset, domainAxis, axis1, xyItemRenFlow);
        
        // add a second dataset and renderer... --------------------------------
        XYItemRenderer xyItemRendRain = new XYBarRenderer();
        XYBarRenderer xyBarRenderer = (XYBarRenderer) xyItemRendRain;
        xyBarRenderer.setDrawBarOutline(false);
//        xyBarRenderer.setShadowVisible(false);
//       XYBarRenderer.setDefaultShadowsVisible(false);
        xyBarRenderer.setSeriesPaint(0, new Color(255,0,0));
        if(effectiveRainfallData != null) {
            xyBarRenderer.setSeriesPaint(1, new Color(0,255,0));
        }
//        xyBarRenderer.setBarPainter(new StandardXYBarPainter());
//        XYBarRenderer.setDefaultBarPainter(new StandardXYBarPainter());
        
        XYBarDataset xyBarDatasetRain = new XYBarDataset(rainfallXyDataset, rainfallBarWidth);
        plot.setDataset(1, xyBarDatasetRain);
        plot.setRenderer(1, xyItemRendRain);

        ValueAxis axis2 = new NumberAxis(PluginUtils.getResources().getString("PersonalChartHydrograph.Rainfall.label"));
        axis2.setInverted(true);
        axis2.setRange(0, rainUpper * 3);
        
        plot.setRangeAxis(1, axis2);        
        plot.mapDatasetToRangeAxis(1, 1);
        
        plot.getDomainAxis().setRange(0, plot.getDomainAxis().getRange().getUpperBound());

        
        JFreeChart jFreeChart = new JFreeChart(null, JFreeChart.DEFAULT_TITLE_FONT, plot, true);
        ChartPanel chartPanel = new ChartPanel(jFreeChart);
        
        return chartPanel;

    }
        
    private final Hyetograph hyetograph; 
    private final EffectiveRainfall effectiveRainfall;
    private final UnitHydrograph unitHydrograph;
    private final TimeInterval.TimeIntervalUnit units;
    
}
