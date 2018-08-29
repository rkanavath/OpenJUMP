package com.geomaticaeambiente.openjump.klem.fill;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.IntBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
/**
 *
 * @author AdL
 */
public class FlatAreasProcessorStripe {
    
    public DoubleBasicGrid process(DoubleBasicGrid filledDem) {

        columnCount = filledDem.getColumnCount();
        rowCount = filledDem.getRowCount();        
        
        /* 0 - Convert to integer */
        int[][] demIntData = new int[rowCount][columnCount];
        boolean[][] flatsMap = new boolean[rowCount][columnCount];
              
        int[][] totalGradient = new int[rowCount][columnCount];
        
        // Initialize
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                
                if(filledDem.isNoData(filledDem.getValue(c, r))) {
                    demIntData[r][c] = Integer.MIN_VALUE;
                } else {
                    demIntData[r][c] = (int) Math.round(filledDem.getValue(c, r) * 100) * 1000;
                }
                totalGradient[r][c] = Integer.MIN_VALUE;
                flatsMap[r][c] = false;
                
            }
        }
        
        IntBasicGrid filledIntDem = new IntBasicGrid(demIntData,
                filledDem.getCellSize(), Integer.MIN_VALUE, filledDem.getLowerLeftCoord());
        
        /* 1 - Find flat cells */
        List<java.awt.Point> flatCells_l = findFlats(filledIntDem, flatsMap);

        // Find flats outlets and uphill limit
        Gradient[] gradients = findOutletsAndInlets(flatCells_l, filledIntDem, flatsMap, 2);
        Gradient gradientUpwards = gradients[0];
        Gradient gradientDownwards = gradients[1];
        
        /* 2 - Gradient towards lower terrain */
        Gradient gradientUpwards1 = new Gradient(gradientUpwards.getFlatCells_l(), gradientUpwards.getGradient());
        Gradient gradientUpwards2 = null;
        
        boolean terminated = false;
        boolean theSwitch = true;
        
        while(!terminated) {

            if(theSwitch) {
                gradientUpwards2 = processUpwards(filledIntDem, gradientUpwards1, 2);
                if(gradientUpwards2.getFlatCells_l().isEmpty()) {
                    terminated = true;
                }
                gradientUpwards = gradientUpwards2;
            } else {
                gradientUpwards1 = processUpwards(filledIntDem, gradientUpwards2, 2);
                if(gradientUpwards1.getFlatCells_l().isEmpty()) {
                    terminated = true;
                }
                gradientUpwards = gradientUpwards1;
            }
            theSwitch = !theSwitch;
            
        }
           
        /* 3 - Gradient away from lower terrain */
        processDownwards(filledIntDem, gradientDownwards, 2);
        
        /* 4 - Combine */
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                if(!filledIntDem.isNoData(gradientUpwards.getGradient()[r][c]) &&
                        !filledIntDem.isNoData(gradientDownwards.getGradient()[r][c])) {
                    
                    totalGradient[r][c] = gradientUpwards.getGradient()[r][c]
                            + gradientDownwards.getGradient()[r][c] - filledIntDem.getValue(c, r);

                }
            }
        }
        
        IntBasicGrid totalGradientGrid = new IntBasicGrid(totalGradient,
                filledIntDem.getCellSize(), filledIntDem.getNoData(), filledIntDem.getLowerLeftCoord());
        
        /* 5 - Find remaining unresolved cells */              
        totalGradient = new int[rowCount][columnCount];
        
        // Initialize
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                
                if(filledDem.isNoData(filledDem.getValue(c, r))) {
                    demIntData[r][c] = Integer.MIN_VALUE;
                } else {
                    demIntData[r][c] = (int) Math.round(filledDem.getValue(c, r) * 100);
                }
                
                totalGradient[r][c] = Integer.MIN_VALUE;
                flatsMap[r][c] = false;
            }
        }
        
        
        flatCells_l = findFlats(totalGradientGrid, flatsMap);
        gradients = findOutletsAndInlets(flatCells_l, totalGradientGrid, flatsMap, 1);
        gradientUpwards = gradients[0];
        
        gradientUpwards1 = new Gradient(gradientUpwards.getFlatCells_l(), gradientUpwards.getGradient());
        gradientUpwards2 = null;
        
        terminated = false;
        theSwitch = true;
        
        while(!terminated) {

            if(theSwitch) {
                gradientUpwards2 = processUpwards(totalGradientGrid, gradientUpwards1, 1);
                if(gradientUpwards2.getFlatCells_l().isEmpty()) {
                    terminated = true;
                }
                gradientUpwards = gradientUpwards2;
            } else {
                gradientUpwards1 = processUpwards(totalGradientGrid, gradientUpwards2, 1);
                if(gradientUpwards1.getFlatCells_l().isEmpty()) {
                    terminated = true;
                }
                gradientUpwards = gradientUpwards1;
            }
            theSwitch = !theSwitch;
            
        }     
        
        // Replace in DEM
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                if(!filledIntDem.isNoData(gradientUpwards.getGradient()[r][c]) &&
                        !filledIntDem.isNoData(gradientDownwards.getGradient()[r][c]) &&
                        !filledIntDem.isNoData(gradientUpwards.getGradient()[r][c])) {
                    
                    totalGradientGrid.setValue(c, r, gradientUpwards.getGradient()[r][c]);

                }
            }
        }
        
        /* Re-convert DEM to actual units */
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                
                if(!totalGradientGrid.isNoData(totalGradientGrid.getValue(c, r))) {
                    
                    filledDem.setValue(c, r, totalGradientGrid.getValue(c, r) / 100d / 1000d);
                    
                }
                
            }   
        }
        
        return filledDem;
        
        
    }
    
    private List<java.awt.Point> findFlats(IntBasicGrid intDem, boolean[][] flatsMap) {
        
        List<java.awt.Point> flatCells_l = new ArrayList<java.awt.Point>();
        
        // Find flats
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                
                if(intDem.isNoData(intDem.getValue(c, r))) {
                    continue;
                }

                boolean isFlat = true;
                for(int i=0; i<8; i++) {
                    
                    int ic = c + Shifter.getColShift(i);
                    int ir = r + Shifter.getRowShift(i);
                    
                    if(ir < 0 || ir >= rowCount || ic < 0 || ic >= columnCount  ||
                            intDem.isNoData(intDem.getValue(ic, ir))) {
                        isFlat = false;
                        break;
                    }

                    if(intDem.getValue(ic, ir) < intDem.getValue(c, r)) {
                        isFlat = false;
                        break;
                    }
                    
                }
                if(isFlat) {
                    flatsMap[r][c] = true;
                    flatCells_l.add(new java.awt.Point(c, r));
                }
                                
            }
        }
        
        return flatCells_l;
        
    }
    
    private Gradient[] findOutletsAndInlets(
            List<java.awt.Point> flatCells_l,
            IntBasicGrid intDem,
            boolean[][] flatsMap,
            
            int increment) {
        
        List<java.awt.Point> flatCellsDown_l = new ArrayList<java.awt.Point>();;
        List<java.awt.Point> flatCellsUp_l = new ArrayList<java.awt.Point>();
        
        int[][] gradientUpwards = new int[rowCount][columnCount];
        int[][] gradientDownwards = new int[rowCount][columnCount];
        
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<columnCount; c++) {
                gradientUpwards[r][c] = intDem.getNoData();
                gradientDownwards[r][c] = intDem.getNoData();
            }
        }
        
        for(java.awt.Point flatCell : flatCells_l) {
            
            int c = flatCell.x;
            int r = flatCell.y;
            
            boolean upChecked = false;
            
            for(int i=0; i<8; i++) {
                    
                int ic = c + Shifter.getColShift(i);
                int ir = r + Shifter.getRowShift(i);

                if(intDem.getValue(ic, ir) == intDem.getValue(c, r) &&
                        flatsMap[ir][ic] == false) {
                    flatCellsDown_l.add(new java.awt.Point(ic, ir));
                    gradientUpwards[ir][ic] = intDem.getValue(ic, ir);
                }

                if(intDem.getValue(ic, ir) > intDem.getValue(c, r) &&
                        upChecked == false) {
                    flatCellsUp_l.add(new java.awt.Point(c, r));
                    upChecked = true;
                    gradientDownwards[r][c] = intDem.getValue(c, r) + increment;
                }

            }
            
        }
        
        return new Gradient[] {
            new Gradient(flatCellsDown_l, gradientUpwards),
            new Gradient(flatCellsUp_l, gradientDownwards)};
        
    }
    
    private Gradient processUpwards(
            IntBasicGrid intDem,
            Gradient gradientUpwards,
            int increment) {
        
        List<java.awt.Point> toCue = new ArrayList<java.awt.Point>();
        
        while(!gradientUpwards.getFlatCells_l().isEmpty()) {
        
            java.awt.Point cellToProcess = gradientUpwards.getFlatCells_l().get(gradientUpwards.getFlatCells_l().size() - 1);
            gradientUpwards.getFlatCells_l().remove(gradientUpwards.getFlatCells_l().size() - 1);

            // Find flat cells around the cell
            for(int i=0; i<8; i++) {

                int ic = cellToProcess.x + Shifter.getColShift(i);
                int ir = cellToProcess.y + Shifter.getRowShift(i);

                if(ir < 0 || ir >= rowCount || ic < 0 || ic >= columnCount  ||
                        intDem.isNoData(intDem.getValue(ic, ir))) {
                    continue;
                }

                if(!intDem.isNoData(intDem.getValue(ic, ir))) {
                    if(intDem.getValue(ic, ir) == intDem.getValue(cellToProcess.x, cellToProcess.y) &&
                            gradientUpwards.getGradient()[ir][ic] == Integer.MIN_VALUE) {

                        toCue.add(new java.awt.Point(ic, ir));
                        gradientUpwards.getGradient()[ir][ic] = gradientUpwards.getGradient()[cellToProcess.y][cellToProcess.x] + increment;

                    }
                }

            }
        }
        
        return new Gradient(toCue, gradientUpwards.getGradient());
        
    }
    
    private Gradient processDownwards(
            IntBasicGrid intDem,
            Gradient gradientDownwards,
            int increment) {
        
        boolean moreToProcess = true;
        while(moreToProcess) {
                      
            List<java.awt.Point> newCells_l = new ArrayList<java.awt.Point>();
            
            for(java.awt.Point cellToProcess : gradientDownwards.getFlatCells_l()) {
                
                moreToProcess = false;

                if(gradientDownwards.getGradient()[cellToProcess.y][cellToProcess.x] == Integer.MIN_VALUE) {    
                    gradientDownwards.getGradient()[cellToProcess.y][cellToProcess.x] = intDem.getValue(cellToProcess) + increment;
                } else {
                    gradientDownwards.getGradient()[cellToProcess.y][cellToProcess.x] += increment;
                }
                
                // Find flat cells around the cell
                for(int i=0; i<8; i++) {

                    int ic = cellToProcess.x + Shifter.getColShift(i);
                    int ir = cellToProcess.y + Shifter.getRowShift(i);

                    if(ir < 0 || ir >= rowCount || ic < 0 || ic >= columnCount  ||
                            intDem.isNoData(intDem.getValue(ic, ir))) {
                        continue;
                    }

                    if(!intDem.isNoData(intDem.getValue(ic, ir))) {
                        if(intDem.getValue(ic, ir) == intDem.getValue(cellToProcess.x, cellToProcess.y) &&
                                gradientDownwards.getGradient()[ir][ic] == Integer.MIN_VALUE) {

                            gradientDownwards.getGradient()[ir][ic] = intDem.getValue(cellToProcess) + increment;
                            newCells_l.add(new java.awt.Point(ic, ir));
                            moreToProcess = true;
                        }
                    }

                }

            }
            
            gradientDownwards.getFlatCells_l().addAll(newCells_l);
            
        }
        
        return gradientDownwards;
        
    }
    
    private class Gradient {

        public Gradient(List<Point> flatCells_l, int[][] gradient) {
            this.flatCells_l = flatCells_l;
            this.gradient = gradient;
        }

        public List<Point> getFlatCells_l() {
            return flatCells_l;
        }

        public int[][] getGradient() {
            return gradient;
        }
        
        private final List<java.awt.Point> flatCells_l;
        private final int[][] gradient;
        
    }

    private int rowCount;
    private int columnCount;
    
}
