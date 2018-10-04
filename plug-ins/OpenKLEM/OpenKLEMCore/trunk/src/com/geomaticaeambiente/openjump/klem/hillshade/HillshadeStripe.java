package com.geomaticaeambiente.openjump.klem.hillshade;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;

import java.io.IOException;
import java.util.concurrent.Callable;

/**
 *
 * @author AdL
 */
public class HillshadeStripe implements Callable<DoubleBasicGrid> {

    public HillshadeStripe(int stripeEffectiveRows, DoubleBasicGrid slopeDegsGrid,
            DoubleBasicGrid aspectDegsGrid,
            int yOffset, double zenithDegs, double azimuthDegs) {

        this.stripeEffectiveRows = stripeEffectiveRows;
        this.slopeDegsGrid = slopeDegsGrid;
        this.aspectDegsGrid = aspectDegsGrid;
        this.yOffset = yOffset;
        this.zenithRad = Math.toRadians(zenithDegs);
        this.azimuthRad = Math.toRadians(azimuthDegs);
        
    }
    
    @Override
    public DoubleBasicGrid call() throws Exception {
        return calcAspect(); 
    }
    
    private DoubleBasicGrid calcAspect() throws IOException {
        
        int nRows = stripeEffectiveRows;
        int nCols = slopeDegsGrid.getColumnCount();
        
        DoubleBasicGrid hillshadeGrid = new DoubleBasicGrid(new byte[nRows][nCols],
                slopeDegsGrid.getCellSize(), -9999, slopeDegsGrid.getLowerLeftCoord());
        
        for(int row=0; row<nRows; row++) {
            for(int col=0; col<nCols; col++) {          

                hillshadeGrid.setValue(col, row, aspectDegsGrid.getNoData());
                
                java.awt.Point cell = new java.awt.Point(col, row + yOffset);
                
                if(!this.slopeDegsGrid.isNoData(slopeDegsGrid.getValue(cell)) &&
                        !aspectDegsGrid.isNoData(aspectDegsGrid.getValue(cell))) {
                    
                    double slopeRad = Math.toRadians(slopeDegsGrid.getValue(col, row));
                    double aspectRad = Math.toRadians(aspectDegsGrid.getValue(col, row));
                    
                    double aspectRad2;
                    /*
                     * [Giuseppe Aruta =ct 4 2018] correctios from:
                     * http://edndoc.esri.com/arcobjects/9.2/net/shared/
                     * geoprocessing
                     * /spatial_analyst_tools/how_hillshade_works.htm
                     */
                    //
                    if (aspectRad < 0) {
                        aspectRad2 = 2 * Math.PI + aspectRad;
                    } else {
                        aspectRad2 = aspectRad;
                    }

                    final double hillshade = 255.0 * ((Math.cos(zenithRad) * Math
                            .cos(slopeRad)) + (Math.sin(zenithRad)
                            * Math.sin(slopeRad) * Math.cos(azimuthRad
                            - aspectRad2)));
                    long hillshade2;
                    if (hillshade < 0) {
                        hillshade2 = 0;
                    } else {
                        hillshade2 = Math.round(hillshade);
                    }
                    // hillshadeGrid.setValue(col, row,
                    // Math.toDegrees(hillshade));
                    hillshadeGrid.setValue(col, row, hillshade2);
                }
            }
        }
        
        return hillshadeGrid;
        
    }
    
    private final int stripeEffectiveRows;
    private final DoubleBasicGrid slopeDegsGrid;
    private final DoubleBasicGrid aspectDegsGrid;
    private final int yOffset;
    private final double zenithRad;
    private final double azimuthRad;
    
}
