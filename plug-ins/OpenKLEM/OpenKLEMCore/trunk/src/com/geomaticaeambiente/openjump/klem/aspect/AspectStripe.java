package com.geomaticaeambiente.openjump.klem.aspect;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;
import java.io.IOException;
import java.util.concurrent.Callable;

/**
 *
 * @author AdL
 */
public class AspectStripe implements Callable<DoubleBasicGrid> {

    public AspectStripe(DoubleBasicGrid demStripe, double[] upRow,
            double[] bottomRow, boolean[][] bluelinesGrid, Double bluelinesWeight) {

        this.demStripe = demStripe;
        this.upRow = upRow;
        this.bottomRow = bottomRow;
        this.bluelinesGrid = bluelinesGrid;
        this.bluelinesWeight = bluelinesWeight;
        
    }
    
    @Override
    public DoubleBasicGrid call() throws Exception {
        return calcAspect(); 
    }
    
    private DoubleBasicGrid calcAspect() throws IOException {
        
        int nRows = demStripe.getRowCount();
        int nCols = demStripe.getColumnCount();

        DoubleBasicGrid aspectDegsGrid = new DoubleBasicGrid(new byte[nRows][nCols],
                demStripe.getCellSize(), -9999, demStripe.getLowerLeftCoord());
        
        for(int row=0; row<nRows; row++) {
            for(int col=0; col<nCols; col++) {          

                aspectDegsGrid.setValue(col, row, aspectDegsGrid.getNoData());
                
                // Bluelines
                double[] elev = new double[8];
                for(int i=0; i<8; i++) {

                    int c1 = col + Shifter.getColShift(i);
                    int r1 = row + Shifter.getRowShift(i);                         
                    if(c1>=0 && c1<nCols && r1>=0 && r1<nRows) {
                        if(!demStripe.isNoData(demStripe.getData()[r1][c1])) {
                            elev[i] = demStripe.getData()[r1][c1];
                        }
                    } else if(c1>=0 && c1<nCols && r1<0) {
                        if(!demStripe.isNoData(upRow[c1])) {
                            elev[i] = upRow[c1];
                        }
                    } else if(c1>=0 && c1<nCols && r1==nRows) {
                        if(!demStripe.isNoData(bottomRow[c1])) {
                            elev[i] = bottomRow[c1];
                        }
                    }

                    if(bluelinesGrid != null) {
                        if(c1>0 && c1<nCols && r1>0 && r1<nRows && bluelinesGrid[r1][c1]) {
                            if(elev[i] < demStripe.getData()[row][col] && !demStripe.isNoData(elev[i])) {
                                elev[i] -= bluelinesWeight;
                            }
                        }
                    }

                }
                
                if(!demStripe.isNoData(demStripe.getData()[row][col])) {
                    
                    // a b c
                    // d e f
                    // g h i
                    
                    // e = current cell
                    
                    double a = elev[3];
                    double b = elev[2];
                    double c = elev[1];
                    double d = elev[4];
                    double f = elev[0];
                    double g = elev[5];
                    double h = elev[6];
                    double i = elev[7];
                    
                    if(demStripe.isNoData(a) || demStripe.isNoData(b) ||
                            demStripe.isNoData(c) || demStripe.isNoData(d) ||
                            demStripe.isNoData(f) || demStripe.isNoData(g) ||
                            demStripe.isNoData(h) || demStripe.isNoData(i)) {
                        
                        aspectDegsGrid.setValue(col, row, demStripe.getNoData());
                        continue;
                        
                    }
                    
                    double dz_dx = ((c + 2*f + i) - (a + 2*d + g)) / 8;
                    double dz_dy = ((g + 2*h + i) - (a + 2*b + c)) / 8;
                    
                    double aspect = Math.toDegrees(Math.atan2(dz_dy, -dz_dx));

                    if(aspect < 0) {
                        aspect = 90.0 - aspect;
                    } else if (aspect > 90.0) {
                        aspect = 360.0 - aspect + 90.0;
                    } else {
                        aspect = 90.0 - aspect;
                    }

                    aspectDegsGrid.setValue(col, row, aspect);
                    
                }
            }
        }
        
        return aspectDegsGrid;
        
    }
    
    private final DoubleBasicGrid demStripe;
    private final double[] upRow;
    private final double[] bottomRow;
    private final boolean[][] bluelinesGrid;
    private final Double bluelinesWeight;
    
}
