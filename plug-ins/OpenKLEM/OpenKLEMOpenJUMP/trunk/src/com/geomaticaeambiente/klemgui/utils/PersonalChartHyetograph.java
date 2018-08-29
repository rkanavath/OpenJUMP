package com.geomaticaeambiente.klemgui.utils;

import it.geomaticaeambiente.klem.Hyetograph;
import it.geomaticaeambiente.klem.TimeInterval;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.GradientPaint;
import java.awt.Paint;
import java.util.Date;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.category.IntervalBarRenderer;
import org.jfree.chart.renderer.xy.StandardXYBarPainter;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.data.xy.DefaultXYDataset;
import org.jfree.data.xy.XYBarDataset;

/**
 *
 * @author Geomatica
 */
public class PersonalChartHyetograph extends PersonalChart{
    
     public PersonalChartHyetograph (Hyetograph hyetograph, double step, TimeInterval.TimeIntervalUnit units){
        
        this.hyetograph = hyetograph;
        this.step = step;
        this.units = units;
//        type = Type.HYETOGRAPH;
        
    }

    public Hyetograph getHyetograph() {
        return hyetograph;
    }

    public double getStep() {
        return step;
    }

    public TimeInterval.TimeIntervalUnit getUnits() {
        return units;
    }
    
     @Override
    public PersonalChart.Type getType(){
        return PersonalChart.Type.HYETOGRAPH;
    }
      
    @Override
    public TimeInterval.TimeIntervalUnit getGraphUnit() {
        return units;
    }
    
    private Hyetograph hyetograph;
    private double step;
    private TimeInterval.TimeIntervalUnit units;   

    public enum Type {HYETOGRAPH};
   
    private Type type;
    
    
     @Override
    public ChartPanel buildGraph() { //units 1 = hour 2=mins

//        Hyetograph hyetograph = personalChart.getHyetograph();
//        double step = personalChart.getStep();
//        TimeInterval.TimeIntervalUnit units = personalChart.getUnits();
        
        // Build graph ---------------------------------------------------------
        DefaultXYDataset xyDataset = new DefaultXYDataset();        
        double barWidth = step;        

        // Build graph data
        double[][] data = new double[2][hyetograph.getRainfall().length];
        for (int s = 0; s < hyetograph.getRainfall().length; s++) {
            data[0][s] = hyetograph.getStep().getInterval(TimeInterval.TimeIntervalUnit.MINUTE) * s;
            data[1][s] = hyetograph.getRainfall()[s];
        }

        // Units are hours: convert
        if (units == TimeInterval.TimeIntervalUnit.HOUR) {
            barWidth /= 60;
            for (int d = 0; d < data[0].length; d++) {
                data[0][d] /= 60;
            }
        }
        
        XYBarDataset xyBarDataset = new XYBarDataset(xyDataset, barWidth);
        xyDataset.addSeries(PluginUtils.getResources().getString("HyetographPlugin.Graph_Rain.label"), data);

        // Distance between series
        IntervalBarRenderer intervalBarRenderer = new IntervalBarRenderer();
        intervalBarRenderer.setItemMargin(-0.3);
        
        // Labels
        String graphTimeLabel = "";
        if (units == TimeInterval.TimeIntervalUnit.HOUR) {
            graphTimeLabel = PluginUtils.getResources().getString("HyetographPlugin.Graph_TimeHour.label");
        } else if (units == TimeInterval.TimeIntervalUnit.MINUTE){
            graphTimeLabel = PluginUtils.getResources().getString("HyetographPlugin.Graph_TimeMin.label");
        }
        
        DateAxis xAxis = new DateAxis(graphTimeLabel);
        xAxis.setTickLabelFont(new Font(xAxis.getLabelFont().getName(), Font.PLAIN, 8));
        xAxis.setMinimumDate(new Date(0));
        
        NumberAxis yAxis = new NumberAxis(PluginUtils.getResources().getString("HyetographPlugin.Graph_Rainfall.label"));
        
        JFreeChart chart = ChartFactory.createXYBarChart(
                null,
                xAxis.getLabel(),
                false,
                yAxis.getLabel(),
                xyBarDataset,
                PlotOrientation.VERTICAL,
                false,
                false,
                false);

        // Rendering
        XYPlot plot = chart.getXYPlot();
        chart.setTitle(PluginUtils.getResources().getString("HyetographPlugIn.PlugInName.label"));
        
        Paint backgroundPaint = new GradientPaint(
                0.0f, 0.0f, new Color(255, 255, 255), 0.0f, 0.0f, new Color(128, 128, 128));
        plot.setBackgroundPaint(backgroundPaint);
        
        XYBarRenderer xyBarRenderer = (XYBarRenderer)plot.getRenderer();
        
        Paint blueGradientPaint = new GradientPaint(
                0.0f, 0.0f, new Color(50, 50, 250), 0.0f, 0.0f, new Color(10, 10, 250));        
        xyBarRenderer.setSeriesPaint(0, blueGradientPaint);
        
        Paint outlinePaint = new Color(0, 0, 0);
        xyBarRenderer.setSeriesOutlinePaint(0, outlinePaint);
        xyBarRenderer.setSeriesOutlineStroke(0, new BasicStroke(1));
        xyBarRenderer.setDrawBarOutline(true);

        StandardXYBarPainter xyBarPainter = new StandardXYBarPainter();        
//        xyBarRenderer.setBarPainter(xyBarPainter);

        plot.setRenderer(xyBarRenderer);
        ChartPanel chartPanel = new ChartPanel(chart);
       
                
        return chartPanel;
    }
     
    
}
