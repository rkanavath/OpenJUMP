package com.geomaticaeambiente.openjump.klem.rastertools;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RasterAggregator {
    
    /**
     * Finds the overall envelope of a series of grids
     * @param sourceGrids
     * @return 
     */
    public static Envelope findOverallEnvelope(DoubleBasicGrid[] sourceGrids) {
    
        Envelope destEnvelope = sourceGrids[0].getEnvelope();
        
        for(int g=1; g<sourceGrids.length; g++) {
            destEnvelope.expandToInclude(sourceGrids[g].getEnvelope());
        }
        
        return destEnvelope;
        
    }
    
    /**
     * Aggregates a series of grids to a given destination cell size
     * @param sourceGrids
     * @param destCellSize Must be bigger than the cell size of every source Grid
     * @param aggregationMethod How small source cells are aggregated into one big destination cell
     * @param overlayMethod How overlapping source cells are handled
     * @return
     * @throws SourceCellSizeException
     * @throws Exception 
     */
    public static DoubleBasicGrid aggregateRasters(DoubleBasicGrid[] sourceGrids,
            double destCellSize, AggregationMethod aggregationMethod,
            OverlayMethod overlayMethod) throws SourceCellSizeException, OutOfMemoryError, Exception {
        
        Envelope destEnvelope = findOverallEnvelope(sourceGrids);
        
        int destNCols = (int) Math.ceil(destEnvelope.getWidth() / destCellSize);
        int destNRows = (int) Math.ceil(destEnvelope.getHeight() / destCellSize);
        
        double[][] destRaster = new double[destNRows][destNCols];
        
        DoubleBasicGrid destGrid = new DoubleBasicGrid(
                destRaster,
                destCellSize,
                noDataVal,
                new Coordinate(destEnvelope.getMinX(), destEnvelope.getMinY()));
        
        int[][] destRasterOverlapsCount = null;
        if(overlayMethod == OverlayMethod.MEAN) {
            destRasterOverlapsCount = new int[destNRows][destNCols];
        }
        
        for(int r=0; r<destNRows; r++) {
            Arrays.fill(destRaster[r], RasterAggregator.noDataVal);
            if(overlayMethod == OverlayMethod.MEAN) {
                Arrays.fill(destRasterOverlapsCount[r], 0);
            }
        }
        
        // Resample every input grid
        for (DoubleBasicGrid sourceGrid : sourceGrids) {
            
            if(sourceGrid.getCellSize() > destCellSize) {
                throw new SourceCellSizeException("The resolution of one of theinput grids is smaller than the the ouput cell size.");
            }
            
            int sourceNRows = sourceGrid.getRowCount();
            int sourceNCols = sourceGrid.getColumnCount();
            
            int kernelSize = (int) Math.ceil(destCellSize / sourceGrid.getCellSize());
            int halfKernelSize;
            if(kernelSize%2 == 0) {
                halfKernelSize = kernelSize / 2;
            } else {
                halfKernelSize = (kernelSize - 1) / 2;
            }
            
            //double[][] sourceRaster = sourceGrid.getRas();
            // Find cells of low res raster covered by high res raster
            for(int r=0; r<destNRows; r++) {
                for(int c=0; c<destNCols; c++) {
                    
                    Coordinate destCellCentreCoord = destGrid.fromCellToCoordinate(
                            new java.awt.Point(c, r));
                    
                    java.awt.Point sourceKernelCentre = sourceGrid
                            .fromCoordinateToCell(destCellCentreCoord);
                    
                    if(sourceKernelCentre.x < 0 || sourceKernelCentre.x > sourceNCols ||
                            sourceKernelCentre.y < 0 || sourceKernelCentre.y > sourceNRows) {
                        continue;
                    }
                    
                    List<Double> vals_l = new ArrayList<Double>();
                    double minVal = Double.MAX_VALUE;
                    double maxVal = -Double.MAX_VALUE;
                    
                    for(int kr=-halfKernelSize; kr<=halfKernelSize; kr++) {
                        for(int kc=-halfKernelSize; kc<=halfKernelSize; kc++) {
                            
                            if(sourceKernelCentre.y+kr >= 0 && sourceKernelCentre.y+kr < sourceGrid.getRowCount() &&
                                    sourceKernelCentre.x+kc >= 0 && sourceKernelCentre.x+kc < sourceGrid.getColumnCount()) {
                            
                                // There's no 0 position when kernel size is even
                                if(kernelSize%2 == 0 && (kr == 0 || kc == 0)) {
                                    continue;
                                }

                                double val = sourceGrid.getValue(sourceKernelCentre.x+kc, sourceKernelCentre.y+kr);
                                if(!sourceGrid.isNoData(val)) {
                                    vals_l.add(val);
                                    if(val < minVal) { minVal = val; }
                                    if(val > maxVal) { maxVal = val; }
                                }
                            }
                        }
                    }
                    
                    // Aggregate values
                    double outVal = 0;
                    if(!vals_l.isEmpty()) {
                        switch (aggregationMethod) {
                            case SUM:
                                for(Double val : vals_l) {
                                    outVal += val;
                                }
                                break;
                            case MIN:
                                outVal = minVal;
                                break;
                            case MAX:
                                outVal = maxVal;
                                break;
                            case MEAN:
                                for(Double val : vals_l) {
                                    outVal += (val / vals_l.size());
                                }
                                break;
                            case MAJORITY:
                                outVal = getMostFrequent(vals_l.toArray(new Double[vals_l.size()]));
                                break;
                        }
                        
                        //System.out.println(c + "," + r);
                        switch (overlayMethod) {
                            
                            case MOST_SIGNIFICANT_FIRST:
                                if(destRaster[r][c] == noDataVal) {
                                    destRaster[r][c] = outVal;
                                }
                                break;
                            case MOST_SIGNIFICANT_LAST:
                                destRaster[r][c] = outVal;
                                break;
                            case MEAN:
                                if(destRaster[r][c] == RasterAggregator.noDataVal) {
                                    destRaster[r][c] = outVal;
                                } else {
                                    destRaster[r][c] += outVal;
                                }
                                destRasterOverlapsCount[r][c] ++;
                                break;
                            case MAX:
                                if(destRaster[r][c] == noDataVal) {
                                    destRaster[r][c] = outVal;
                                } else {
                                    if(outVal > destRaster[r][c]) {
                                        destRaster[r][c] = outVal;
                                    }
                                }
                                break;
                            case MIN:
                                if(destRaster[r][c] == noDataVal) {
                                    destRaster[r][c] = outVal;
                                } else {
                                    if(outVal < destRaster[r][c]) {
                                        destRaster[r][c] = outVal;
                                    }
                                }
                                break;                            
                        }
                        
                    } else {
                        continue;
                    }
                    
                }
            }
        }
        
        /* Only for mean overlap management */
        if(overlayMethod == OverlayMethod.MEAN) {
            for(int r=0; r<destNRows; r++) {
                for(int c=0; c<destNCols; c++) {
                    if(destRaster[r][c] != RasterAggregator.noDataVal) {
                        destRaster[r][c] /= destRasterOverlapsCount[r][c];
                    }
                }
            }
        }        
        
        return destGrid;
        
    }
    
    public enum AggregationMethod {
        
        MIN, MAX, MEAN, SUM, MAJORITY;
        
    }
    
    public enum OverlayMethod {
        
        MOST_SIGNIFICANT_FIRST, MOST_SIGNIFICANT_LAST, MEAN, MAX, MIN
        
    }
    
    private static class SourceCellSizeException extends Exception {
        
        protected SourceCellSizeException(String message) {
            super(message);
        }
        
    }
    
    private static double getMostFrequent(Double[] data) {
        
        Map<Double, Integer> map = new HashMap<Double, Integer>();

        for (double a : data) {
            Integer freq = map.get(a);
            map.put(a, (freq == null) ? 1 : freq + 1);
        }

        int max = -1;
        double mostFrequent = Double.NaN;

        for (Map.Entry<Double, Integer> e : map.entrySet()) {
            if (e.getValue() > max) {
                mostFrequent = e.getKey();
                max = e.getValue();
            }
        }

        if(Double.isNaN(mostFrequent)) {
            mostFrequent = noDataVal;
        }
        
        return mostFrequent;
        
    }
    
    public static double noDataVal = -9999;
    
}
