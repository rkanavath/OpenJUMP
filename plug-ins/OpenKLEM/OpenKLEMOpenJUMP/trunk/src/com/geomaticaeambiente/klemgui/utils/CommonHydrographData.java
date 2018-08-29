package com.geomaticaeambiente.klemgui.utils;

import com.geomaticaeambiente.openjump.klem.hydrology.Hydrology;
import com.geomaticaeambiente.openjump.klem.hydrology.Hydrology.EffectiveRainfall;
import it.geomaticaeambiente.klem.Hyetograph;
import it.geomaticaeambiente.klem.TimeInterval;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Geomatica
 */
public class CommonHydrographData {

    public CommonHydrographData() {
    }

    public CommonHydrographData(File hyetoFile, double duration, double basinArea, 
            double flowFraction, double cn, double flowCoeff, double deepFlow) {
        this.hyetoFile = hyetoFile;
        this.duration = duration;
        this.basinArea = basinArea;
        this.flowFraction = flowFraction;
        this.cn = cn;
        this.flowCoeff = flowCoeff;
        this.deepFlow = deepFlow;
    }
    
    
    public File getHyetoFile() {
        return hyetoFile;
    }

    public void setHyetoFile(File hyetoFile) {
        this.hyetoFile = hyetoFile;
    }

    public double getDuration() {
        return duration;
    }

    public void setDuration(double duration) {
        this.duration = duration;
    }

    public double getWatershedArea() {
        return basinArea;
    }

    public void setBasinArea(double basinArea) {
        this.basinArea = basinArea;
    }

    public double getFlowFraction() {
        return flowFraction;
    }

    public void setFlowFraction(double flowFraction) {
        this.flowFraction = flowFraction;
    }

    public double getCn() {
        return cn;
    }

    public void setCn(double cn) {
        this.cn = cn;
    }

    public double getFlowCoeff() {
        return flowCoeff;
    }

    public void setFlowCoeff(double flowCoeff) {
        this.flowCoeff = flowCoeff;
    }

    public double getDeepFlow() {
        return deepFlow;
    }

    public void setDeepFlow(double deepFlow) {
        this.deepFlow = deepFlow;
    }

    public Hyetograph getHyetograph() {
        return hyetograph;
    }

    public void setHyetograph(File hyetoFile) throws Exception {
        
        hyetograph = readHyetograph(hyetoFile);
    }
    
    public EffectiveRainfall getEffectiveRainfall(){
        
        return Hydrology.calcEffectiveRainfall(hyetograph, cn, flowFraction, flowCoeff, deepFlow, basinArea);
        
    }
        
    private Hyetograph readHyetograph(File hyetoPath) throws FileNotFoundException, IOException, Exception {
        
        BufferedReader buffReader = new BufferedReader(new FileReader(hyetoPath));

        String line;
        List<Double> time_l = new ArrayList<Double>();
        List<Double> rainfall_l = new ArrayList<Double>();
        buffReader.readLine();
        buffReader.readLine();
        while((line = buffReader.readLine()) != null){
            String[] lines = line.trim().split(","); //NOI18N
            time_l.add(Double.parseDouble(lines[0]));
            rainfall_l.add(Double.parseDouble(lines[1]));
        }
        buffReader.close();
        
        double[] rainfall = new double[rainfall_l.size()];
        for(int r=0; r<rainfall.length; r++) {
            rainfall[r] = rainfall_l.get(r);
        }
        
        TimeInterval timeInterval = new TimeInterval(time_l.get(1)-time_l.get(0), TimeInterval.TimeIntervalUnit.HOUR);
        return new Hyetograph(timeInterval, rainfall);
    }
    
    
    
    private File hyetoFile;
    private double duration;
    private double basinArea;
    private double flowFraction;
    private double cn;
    private double flowCoeff;
    private double deepFlow;
    private Hyetograph hyetograph;
    
}
