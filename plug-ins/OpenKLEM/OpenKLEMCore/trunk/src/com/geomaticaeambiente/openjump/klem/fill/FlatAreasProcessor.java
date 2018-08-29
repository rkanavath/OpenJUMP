package com.geomaticaeambiente.openjump.klem.fill;

import com.geomaticaeambiente.openjump.klem.Log;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.IntBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author deluca
 */
public class FlatAreasProcessor {
    
    public Results process(DoubleBasicGrid demGrid, FlowDirBasicGrid flowDirGrid) throws Exception {
        
        this.demGrid = demGrid;
        this.flowDirGrid = flowDirGrid;
        
        rowCount = demGrid.getRowCount();
        colCount = demGrid.getColumnCount();
        
        // 1 - Calculate flow directions
        //calcFlowDirs();
        
        // 2 - Flat edges
        calcFlatEdges();
        
        // 3 - Label flats
        if(Log.log) System.out.println("Labelling flats");
        labelFlats();
        
//        DoubleBasicGrid flatMaskGridDouble = new DoubleBasicGrid(labelsGrid.getData(), labelsGrid.getCellSize(), -9999, labelsGrid.getLowerLeftCoord());
//        if(1==1) return flatMaskGridDouble;
        
        // 4 - Process away from higer
        if(Log.log) System.out.println("Process away from higher");
        awayFromHigher();
        
//        DoubleBasicGrid flatMaskGridDouble = new DoubleBasicGrid(flatMaskGrid.getData(), flatMaskGrid.getCellSize(), -9999, flatMaskGrid.getLowerLeftCoord());
//        if(1==1) return flatMaskGridDouble;
        
        // 5 - Process towards lower
        if(Log.log) System.out.println("Process towards lower");
        towardsLower();

//        DoubleBasicGrid flatMaskGridDouble = new DoubleBasicGrid(flatMaskGrid.getData(), flatMaskGrid.getCellSize(), -9999, flatMaskGrid.getLowerLeftCoord());
//        if(1==1) return flatMaskGridDouble;
        
        for(int r=0; r<flatMaskGrid.getRowCount(); r++) {
            for(int c=0; c<flatMaskGrid.getColumnCount(); c++) {
                if(flatMaskGrid.getValue(c, r) == 0) {
                    flatMaskGrid.setValue(c, r, flatMaskGrid.getNoData());
                }
            }
        }
        
        return new Results(flatMaskGrid, labelsGrid);
        
    }
    
    private void calcFlatEdges() {
        
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<colCount; c++) {
                
                if(!demGrid.isNoData(demGrid.getValue(c, r))) {                    
                    
                    for(int i=0; i<8; i++) {
                    
                        int ic = c + Shifter.getColShift(i);
                        int ir = r + Shifter.getRowShift(i);
                        
                        if(!flowDirGrid.belongsToGrid(ic, ir)) {
                            continue;
                        }
                        if(flowDirGrid.isNoData(ic, ir)) {
                            continue;
                        }
                        
                        if(!flowDirGrid.isOutlet(c, r)
                                && flowDirGrid.isOutlet(ic, ir)
                                && demGrid.getValue(c, r) == demGrid.getValue(ic, ir)) {
                            
                            lowEdges_l.add(new java.awt.Point(c, r));
                            break;
                            
                        } else if(flowDirGrid.isOutlet(c, r)
                                && demGrid.getValue(c, r) < demGrid.getValue(ic, ir)) {
                            
                            highEdges_l.add(new java.awt.Point(c, r));
                            break;
                        }
                    
                    }
                    
                }
            }
        }
        
    }
    
    private void labelFlats() {
        
        List<java.awt.Point> toFill = new ArrayList<java.awt.Point>();
        
        labelsGrid = new IntBasicGrid(new int[rowCount][colCount], demGrid.getCellSize(), -Integer.MAX_VALUE, demGrid.getLowerLeftCoord());

        lMax = 0;
        for(Point lowEdge : lowEdges_l) {
            
            lMax++;
            
            toFill.add(lowEdge);
            double E = demGrid.getValue(lowEdge);
            while(toFill.size() > 0) {
                
                Point cCell = toFill.get(toFill.size()-1);
                toFill.remove(toFill.size()-1);
                
                if(!demGrid.belongsToGrid(cCell)) {
                    continue;
                }
                
                if(demGrid.isNoData(demGrid.getValue(cCell))) {
                    continue;
                }
                
                if(demGrid.getValue(cCell) != E) {
                    continue;
                }
                if(labelsGrid.getValue(cCell) != 0) {
                    continue;
                }

                labelsGrid.setValue(cCell, lMax);

                for(int i=0; i<8; i++) {

                    int ic = cCell.x + Shifter.getColShift(i);
                    int ir = cCell.y + Shifter.getRowShift(i);

                    if(!flowDirGrid.belongsToGrid(ic, ir)) {
                        continue;
                    }

                    if(demGrid.getValue(cCell) != demGrid.getValue(ic, ir)) {
                        continue;
                    }
                    
                    toFill.add(new java.awt.Point(ic, ir));
                    
                }

            }
        
        }
        
        // Purge highEdges        
        for(Iterator<Point> it = highEdges_l.iterator(); it.hasNext(); ){
            Point highEdge = it.next();
            if(labelsGrid.getValue(highEdge) == 0) {
                it.remove();
            }
        }
        
        
    }
    
    public void awayFromHigher() {
        
        flatMaskGrid = new DoubleBasicGrid(
                new int[rowCount][colCount], demGrid.getCellSize(), -9999, demGrid.getLowerLeftCoord());

        flatHeight = new int[lMax];
        
        int loops = 1;

        List<Point> highEdges2_l = new ArrayList<Point>();
        
        boolean somethingToDo = true;
        while(somethingToDo) {
            
            while(highEdges_l.size() > 0) {
                
                Point cCell = highEdges_l.get(highEdges_l.size()-1);
                highEdges_l.remove(highEdges_l.size()-1);
                
                if(flatMaskGrid.getValue(cCell) > 0) {
                    continue;
                }

                flatMaskGrid.setValue(cCell.x, cCell.y, loops);
                flatHeight[labelsGrid.getValue(cCell) - 1] = loops;

                for(int i=0; i<8; i++) {

                        int ic = cCell.x + Shifter.getColShift(i);
                        int ir = cCell.y + Shifter.getRowShift(i);
                        
                        if(!flowDirGrid.belongsToGrid(ic, ir)) {
                            continue;
                        }

                        if(labelsGrid.getValue(ic, ir) == labelsGrid.getValue(cCell)
                                && flowDirGrid.isOutlet(ic, ir)
                                && flatMaskGrid.getValue(ic, ir) == 0) {
                            highEdges2_l.add(new Point(ic, ir));
                        } 

                }

            }

            // Switch cues
            loops++;
            highEdges_l = new ArrayList<java.awt.Point>(highEdges2_l);
            highEdges2_l.clear();
            if(highEdges_l.isEmpty()) {
                somethingToDo = false;
            }
        
        }
        
        // Reverse gradient values and make flatMask negative
        for(int r=0; r<rowCount; r++) {
            for(int c=0; c<colCount; c++) {
                
                if(!flatMaskGrid.isNoData(flatMaskGrid.getValue(c, r))
                        && flatMaskGrid.getValue(c, r) > 0) {
                    flatMaskGrid.setValue(c, r, -(flatHeight[labelsGrid.getValue(c, r) - 1] - flatMaskGrid.getValue(c, r)));
                }
                
            }
        }
        
        
    }
    
    private void towardsLower() {
     
        // flatMask already negative!
        int loops = 1;
        
        List<Point> lowEdges2_l = new ArrayList<Point>();
        
        boolean somethingToDo = true;
        while(somethingToDo) {
            
            while(lowEdges_l.size() > 0) {

                Point cCell = lowEdges_l.get(lowEdges_l.size()-1);
                lowEdges_l.remove(lowEdges_l.size()-1);
                
                if(flatMaskGrid.getValue(cCell) > 0) {
                    continue;
                }
                
                if(flatMaskGrid.getValue(cCell) < 0) {
                    flatMaskGrid.setValue(cCell.x, cCell.y,//flatHeight[labelsGrid.getValue(cCell) - 1]
                            - flatMaskGrid.getValue(cCell) + 2 * loops);
                } else {
                    flatMaskGrid.setValue(cCell.x, cCell.y, 2 * loops);
                }

                for(int i=0; i<8; i++) {

                    int ic = cCell.x + Shifter.getColShift(i);
                    int ir = cCell.y + Shifter.getRowShift(i);

                    if(!flowDirGrid.belongsToGrid(ic, ir)) {
                        continue;
                    }

                    if(labelsGrid.getValue(ic, ir) == labelsGrid.getValue(cCell)
                            && flowDirGrid.isOutlet(ic, ir)) {
                                lowEdges2_l.add(new Point(ic, ir));
                    }

                }

            }
        
            loops++;
            lowEdges_l = new ArrayList<java.awt.Point>(lowEdges2_l);
            lowEdges2_l.clear();
            if(lowEdges_l.isEmpty()) {
                somethingToDo = false;
            }
                
        }
        
        
    }
    
    public class Results {

        public Results(DoubleBasicGrid flowDirGrid, IntBasicGrid labelsGrid) {
            this.gradientGrid = flowDirGrid;
            this.labelsGrid = labelsGrid;
            this.flatsCount = lMax; 
        }

        public DoubleBasicGrid getGradientGrid() {
            return gradientGrid;
        }

        public IntBasicGrid getLabelsGrid() {
            return labelsGrid;
        }

        public int getFlatsCount() {
            return flatsCount;
        }
        
        private final DoubleBasicGrid gradientGrid;
        private final IntBasicGrid labelsGrid;
        private final int flatsCount;
        
    }
    
    
    private int rowCount = 0;
    private int colCount = 0;
    private int lMax = 0;
    
    private DoubleBasicGrid demGrid;
    private FlowDirBasicGrid flowDirGrid;
    private IntBasicGrid labelsGrid;
    private DoubleBasicGrid flatMaskGrid;
    
    private List<java.awt.Point> lowEdges_l = new ArrayList<java.awt.Point>();
    private List<java.awt.Point> highEdges_l = new ArrayList<java.awt.Point>();
    
    private int[] flatHeight;
    
    
}
