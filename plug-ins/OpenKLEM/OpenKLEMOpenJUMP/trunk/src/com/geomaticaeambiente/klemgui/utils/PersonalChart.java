package com.geomaticaeambiente.klemgui.utils;

import com.geomaticaeambiente.klemgui.ui.PersonalComponentAbstract;
import it.geomaticaeambiente.klem.TimeInterval;
import org.jfree.chart.ChartPanel;

/**
 *
 * @author Geomatica
 */
public abstract class PersonalChart extends PersonalComponentAbstract {
    
    protected ChartPanel chartPanel;
    
    public abstract Type getType();
    
    public abstract ChartPanel buildGraph();

    public enum Type {HYETOGRAPH, HYDROGRAPH, KLEM};
    
    public abstract TimeInterval.TimeIntervalUnit getGraphUnit();

    public ChartPanel getChartPanel() {
        return chartPanel;
    } 
    
}
