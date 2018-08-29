package com.geomaticaeambiente.openjump.klem.cn;

import com.geomaticaeambiente.openjump.klem.exceptions.NotSpatiallyConsistentGridsException;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;

/**
 *
 * @author AdL
 */
public class CurveNumberCalculator {
    
    /**
     * Calculates the Curve Number grid from land use and hydrological soil
     * group grids, with the given reclassification parameters.
     * @param landCoverGrid
     * @param soilGroupGrid
     * @param soilGroupLandUseTable
     * @return
     * @throws NotSpatiallyConsistentGridsException 
     */
    public DoubleBasicGrid calculateCn(
            DoubleBasicGrid landCoverGrid, DoubleBasicGrid soilGroupGrid,
            SoilGroupLandUseTable soilGroupLandUseTable) throws NotSpatiallyConsistentGridsException {
        
        if(!landCoverGrid.isSpatiallyConsistentWith(soilGroupGrid)) {
            throw new NotSpatiallyConsistentGridsException();
        }
        
        int columnCount = landCoverGrid.getColumnCount();
        int rowCount = landCoverGrid.getRowCount();
        
        double[][] cnData = new double[rowCount][columnCount];
        
        for (int r=0; r<rowCount; r++) {
            for (int c=0; c<columnCount; c++) {
                cnData[r][c] = landCoverGrid.getNoData();
                if(!landCoverGrid.isNoData(landCoverGrid.getData()[r][c]) &&
                        !soilGroupGrid.isNoData(soilGroupGrid.getData()[r][c])) {
                
                    cnData[r][c] = soilGroupLandUseTable.getCnValue(
                            landCoverGrid.getData()[r][c],
                            soilGroupGrid.getData()[r][c]);
                    
                }
            } 
        }
        
        return new DoubleBasicGrid(
                cnData, landCoverGrid.getCellSize(),
                landCoverGrid.getNoData(), landCoverGrid.getLowerLeftCoord());
        
    }
    
}
