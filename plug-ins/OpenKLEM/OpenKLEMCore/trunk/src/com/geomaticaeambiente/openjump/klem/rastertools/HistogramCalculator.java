package com.geomaticaeambiente.openjump.klem.rastertools;

import com.geomaticaeambiente.openjump.klem.rastertools.classifiers.ClassAlgo;
import com.geomaticaeambiente.openjump.klem.cn.ValuesRange;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 *
 * @author deluca
 */
public class HistogramCalculator {
    
    public Histogram calcStatsContinuous(DoubleBasicGrid inputGrid, ClassAlgo algo) {
        
        double[] breakVals = algo.getBreakValues();
        Arrays.sort(breakVals);
        
        long cellsCount = 0;
        long[] cellCountVals = new long[breakVals.length + 1];
        
        double minVal = Double.MAX_VALUE;
        double maxVal = -Double.MAX_VALUE;
        double sumVal = 0;
        
        int lastPos = cellCountVals.length - 1;
        for(int r=0; r<inputGrid.getRowCount(); r++) {
            for(int c=0; c<inputGrid.getColumnCount(); c++) {
                
                double val = inputGrid.getValue(c, r);
                if(!inputGrid.isNoData(val)) {
                    cellsCount++;
                    sumVal += val;
                    boolean found = false;
                    for(int bv=0; bv<breakVals.length; bv++) {
                        
                        if(val < minVal) {
                            minVal = val;
                        }
                        if(val > maxVal) {
                            maxVal = val;
                        }
                        
                        if(val <= breakVals[bv]) {
                            cellCountVals[bv]++;
                            found = true;
                            break;
                        }
                    }
                    if(!found) {
                        cellCountVals[lastPos]++;
                    }
                }
                
            }
        }
               
        LinkedHashMap<ValuesRange, Long> cellCountVals_hm = new LinkedHashMap<ValuesRange, Long>();
        LinkedHashMap<ValuesRange, Double> areaVals_hm = new LinkedHashMap<ValuesRange, Double>();
        LinkedHashMap<ValuesRange, Double> relativeAreaVals_hm = new LinkedHashMap<ValuesRange, Double>();
        
        for(int v=0; v<cellCountVals.length; v++) {
            double startVal;
            if(v==0) {
                startVal = minVal;
            } else {
                startVal = breakVals[v-1];
            }
            
            double endVal;
            if(v==cellCountVals.length-1) {
                endVal = maxVal;
            } else {
                endVal = breakVals[v];
            }
            
            ValuesRange valRange = new ValuesRange(startVal, endVal);
            cellCountVals_hm.put(valRange, cellCountVals[v]);
            areaVals_hm.put(valRange, cellCountVals[v] * inputGrid.getCellSize() * inputGrid.getCellSize());
            relativeAreaVals_hm.put(valRange, cellCountVals[v] / (double) cellsCount);
            
        }
        
        double meanVal = sumVal / cellsCount;
        
        // Std dev.
        double squaresSum = 0;
        for(int r=0; r<inputGrid.getRowCount(); r++) {
            for(int c=0; c<inputGrid.getColumnCount(); c++) {
                
                double val = inputGrid.getValue(c, r);
                if(!inputGrid.isNoData(val)) {
                    
                    squaresSum += Math.pow(val - meanVal, 2);
                    
                }
            }
        }
        double stdDevVal = Math.sqrt(squaresSum / cellsCount);
        
        double areaVal = cellsCount * inputGrid.getCellSize() * inputGrid.getCellSize();
        return new Histogram(false, 
                cellCountVals_hm, areaVals_hm, relativeAreaVals_hm,
                minVal, maxVal, meanVal, stdDevVal, sumVal, areaVal, null);
        
    }
    
    public Histogram calcStatsUnique(DoubleBasicGrid inputGrid) {
        
        SortedMap<Double, Long> cellCount_hm = new TreeMap<Double, Long>();
        long cellsCount = 0;
        
        double minVal = Double.MAX_VALUE;
        double maxVal = -Double.MAX_VALUE;
        double sumVal = 0;
        
        for(int r=0; r<inputGrid.getRowCount(); r++) {
            for(int c=0; c<inputGrid.getColumnCount(); c++) {
                double val = inputGrid.getValue(c, r);
                if(!inputGrid.isNoData(val)) {
                    cellsCount++;
                    sumVal += val;
                    
                    if(val < minVal) {
                            minVal = val;
                    }
                    if(val > maxVal) {
                        maxVal = val;
                    }
                    
                    if(cellCount_hm.get(val) == null) {
                        cellCount_hm.put(val, 1l);
                    } else {
                        cellCount_hm.put(val, cellCount_hm.get(val) + 1);
                    }
                    
                }
            }
        }
        
        LinkedHashMap<ValuesRange, Long> cellCountVals_hm = new LinkedHashMap<ValuesRange, Long>();
        LinkedHashMap<ValuesRange, Double> areaVals_hm = new LinkedHashMap<ValuesRange, Double>();
        LinkedHashMap<ValuesRange, Double> relativeAreaVals_hm = new LinkedHashMap<ValuesRange, Double>();
        
        double maxCountVal = 0;
        long maxCount = 0;
        for(Map.Entry<Double,Long> entry : cellCount_hm.entrySet()) {
            
            double startVal = entry.getKey();
            double endVal = entry.getKey();
            
            if(entry.getValue() > maxCount) {
                maxCount = entry.getValue();
                maxCountVal = entry.getKey();
            }
            
            ValuesRange valRange = new ValuesRange(startVal, endVal);
            cellCountVals_hm.put(valRange, entry.getValue());
            areaVals_hm.put(valRange, entry.getValue() * inputGrid.getCellSize() * inputGrid.getCellSize());
            relativeAreaVals_hm.put(valRange, entry.getValue() / (double) cellsCount);
            
        }
        
        double meanVal = sumVal / cellsCount;
        
        // Std dev.
        double squaresSum = 0;
        for(int r=0; r<inputGrid.getRowCount(); r++) {
            for(int c=0; c<inputGrid.getColumnCount(); c++) {
                
                double val = inputGrid.getValue(c, r);
                if(!inputGrid.isNoData(val)) {
                    
                    squaresSum += Math.pow(val - meanVal, 2);
                    
                }
            }
        }
        double stdDevVal = Math.sqrt(squaresSum / cellsCount);
        
        double areaVal = cellsCount * inputGrid.getCellSize() * inputGrid.getCellSize();        
        return new Histogram(true, 
                cellCountVals_hm, areaVals_hm, relativeAreaVals_hm,
                minVal, maxVal, meanVal, stdDevVal, sumVal, areaVal, maxCountVal);
        
    }
    
    public class Histogram {

        public Histogram(boolean uniqueVals, LinkedHashMap<ValuesRange, Long> cellCountValues, LinkedHashMap<ValuesRange, Double> areaValues, LinkedHashMap<ValuesRange, Double> relativeAreaValues, Double min, Double max, Double mean, Double stdDev, Double sum, Double area, Double mode) {
            this.uniqueVals = uniqueVals;
            this.cellCountValues = cellCountValues;
            this.areaValues = areaValues;
            this.relativeAreaValues = relativeAreaValues;
            this.min = min;
            this.max = max;
            this.mean = mean;
            this.stdDev = stdDev;
            this.sum = sum;
            this.area = area;
            this.mode = mode;
        }

        public boolean isUniqueVals() {
            return uniqueVals;
        }

        public LinkedHashMap<ValuesRange, Long> getCellCountValues() {
            return cellCountValues;
        }

        public LinkedHashMap<ValuesRange, Double> getAreaValues() {
            return areaValues;
        }

        public LinkedHashMap<ValuesRange, Double> getRelativeAreaValues() {
            return relativeAreaValues;
        }

        public Double getMin() {
            return min;
        }

        public Double getMax() {
            return max;
        }

        public Double getMean() {
            return mean;
        }

        public Double getStdDev() {
            return stdDev;
        }

        public Double getSum() {
            return sum;
        }

        public Double getArea() {
            return area;
        }

        public Double getMode() {
            return mode;
        }
        
        private final boolean uniqueVals;
        private final LinkedHashMap<ValuesRange, Long> cellCountValues;
        private final LinkedHashMap<ValuesRange, Double> areaValues;
        private final LinkedHashMap<ValuesRange, Double> relativeAreaValues;
        private final Double min;
        private final Double max;
        private final Double mean;
        private final Double stdDev;
        private final Double sum;
        private final Double area;
        private final Double mode;
        
    }
    
}
