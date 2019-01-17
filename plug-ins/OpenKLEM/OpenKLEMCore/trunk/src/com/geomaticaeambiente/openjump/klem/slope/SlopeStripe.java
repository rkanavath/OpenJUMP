package com.geomaticaeambiente.openjump.klem.slope;

import java.io.IOException;
import java.util.concurrent.Callable;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;

/**
 *
 * @author AdL
 */
public class SlopeStripe implements Callable<DoubleBasicGrid> {

    public SlopeStripe(DoubleBasicGrid demStripe, double[] upRow,
            double[] bottomRow, boolean[][] bluelinesGrid,
            Double bluelinesWeight, SlopeAlgo slopeAlgo, SlopeUnits slopeUnits) {

        this.demStripe = demStripe;
        this.upRow = upRow;
        this.bottomRow = bottomRow;
        this.bluelinesGrid = bluelinesGrid;
        this.bluelinesWeight = bluelinesWeight;
        this.slopeAlgo = slopeAlgo;
        this.slopeUnits = slopeUnits;

    }

    @Override
    public DoubleBasicGrid call() throws Exception {
        return calcSlope();
    }

    private DoubleBasicGrid calcSlope() throws IOException {

        final int nRows = demStripe.getRowCount();
        final int nCols = demStripe.getColumnCount();

        final DoubleBasicGrid slopeGrid = new DoubleBasicGrid(
                new byte[nRows][nCols], demStripe.getCellSize(), -9999,
                demStripe.getLowerLeftCoord());

        final double[] weights = new double[8];
        for (int i = 0; i < 8; i++) {
            final int rowShift = Shifter.getRowShift(i);
            final int colShift = Shifter.getColShift(i);
            if (rowShift == 0 || colShift == 0) {
                weights[i] = demStripe.getCellSize();
            } else {
                weights[i] = Math.sqrt(2) * demStripe.getCellSize();
            }
        }

        for (int r = 0; r < nRows; r++) {
            for (int c = 0; c < nCols; c++) {

                slopeGrid.setValue(c, r, slopeGrid.getNoData());

                // Bluelines
                final double[] elev = new double[8];
                for (int i = 0; i < 8; i++) {

                    //[Giuseppe Aruta 2019-1-17] added correction to slopes of borders
                    //in the Travis at al algorithm
                    final int c1 = c + Shifter.getColShift(i);
                    final int r1 = r + Shifter.getRowShift(i);
                    if (c1 >= 0 && c1 < nCols && r1 >= 0 && r1 < nRows) {
                        if (!demStripe.isNoData(demStripe.getData()[r1][c1])) {
                            elev[i] = demStripe.getData()[r1][c1];
                        } else {
                            elev[i] = demStripe.getNoData();
                        }
                    } else if (c1 >= 0 && c1 < nCols && r1 < 0) {
                        if (!demStripe.isNoData(upRow[c1])) {
                            elev[i] = upRow[c1];
                        } else {
                            elev[i] = demStripe.getNoData();
                        }
                    } else if (c1 >= 0 && c1 < nCols && r1 == nRows) {
                        if (!demStripe.isNoData(bottomRow[c1])) {
                            elev[i] = bottomRow[c1];
                        } else {
                            elev[i] = demStripe.getNoData();
                        }
                    }

                    if (c1 < 0 || c1 == nCols) {
                        elev[i] = demStripe.getNoData();
                    }


                    if (bluelinesGrid != null) {
                        if (c1 > 0 && c1 < nCols && r1 > 0 && r1 < nRows
                                && bluelinesGrid[r1][c1]) {
                            if (elev[i] < demStripe.getData()[r][c]
                                    && !demStripe.isNoData(elev[i])) {
                                elev[i] -= bluelinesWeight;
                            }
                        }
                    }

                }

                if (!demStripe.isNoData(demStripe.getData()[r][c])) {

                    // Horn's method
                    if (slopeAlgo == SlopeAlgo.HORN) {
                        final double[] z = new double[9];
                        int cellCount = 0;

                        /* Check domain */
                        for (int kr = -1; kr <= +1; kr++) {
                            for (int kc = -1; kc <= +1; kc++) {

                                final int c1 = c + kc;
                                final int r1 = r + kr;

                                final int ix = (kr + 1) * 3 + (kc + 1);

                                if (c1 >= 0 && c1 < nCols && r1 >= 0
                                        && r1 < nRows) {
                                    if (!demStripe
                                            .isNoData(demStripe.getData()[r1][c1])) {
                                        z[ix] = (demStripe.getData()[r + kr][c
                                                + kc] - demStripe.getData()[r][c]);
                                        cellCount++;
                                    }
                                } else if (c1 >= 0 && c1 < nCols && r1 < 0) {
                                    if (!demStripe.isNoData(upRow[c1])) {
                                        z[ix] = (upRow[c + kc] - demStripe
                                                .getData()[r][c]);
                                        cellCount++;
                                    }
                                } else if (c1 >= 0 && c1 < nCols && r1 == nRows) {
                                    if (!demStripe.isNoData(bottomRow[c1])) {
                                        z[ix] = (bottomRow[c + kc] - demStripe
                                                .getData()[r][c]);
                                        cellCount++;
                                    }
                                }

                            }
                        }

                        cellCount--;
                        final double parb = (z[2] + 2 * z[5] + z[8] - z[0] - 2
                                * z[3] - z[6])
                                / (cellCount * demStripe.getCellSize());
                        final double parc = (z[0] + 2 * z[1] + z[2] - z[6] - 2
                                * z[7] - z[8])
                                / (cellCount * demStripe.getCellSize());

                        final double slopeRatio = Math.sqrt(parb * parb + parc
                                * parc);
                        if (slopeUnits == SlopeUnits.DEGREES) {
                            slopeGrid.setValue(c, r,
                                    Math.toDegrees(Math.atan(slopeRatio)));
                        } else if (slopeUnits == SlopeUnits.PERCENT) {
                            slopeGrid.setValue(c, r, slopeRatio * 100);
                        } else if (slopeUnits == SlopeUnits.RADIANS) {
                            slopeGrid.setValue(c, r, Math.atan(slopeRatio));
                        }

                        // Travis et al. 1975. Local Maximun Slope
                    } else if (slopeAlgo == SlopeAlgo.LOCAL) {

                       // double maxLocalSlope = -Double.MAX_VALUE;
                        double maxLocalSlope = 0.0D;
                        for (int i = 0; i < 8; i++) {

                            if (!demStripe.isNoData(elev[i])) {
                                final double delta = demStripe.getData()[r][c]
                                        - elev[i];
                                final double localSlope = delta / weights[i];
                                if (localSlope > maxLocalSlope) {
                                    maxLocalSlope = localSlope;
                                }
                            }

                        }

                        if (slopeUnits == SlopeUnits.DEGREES) {
                            slopeGrid.setValue(c, r,
                                    Math.toDegrees(Math.atan(maxLocalSlope)));
                        } else if (slopeUnits == SlopeUnits.PERCENT) {
                            slopeGrid.setValue(c, r, maxLocalSlope * 100);
                        } else if (slopeUnits == SlopeUnits.RADIANS) {
                            slopeGrid.setValue(c, r, Math.atan(maxLocalSlope));
                        }

                    }
                }
            }
        }

        return slopeGrid;

    }

    public enum SlopeAlgo {
        HORN, LOCAL
    }

    public enum SlopeUnits {
        DEGREES, PERCENT, RADIANS
    }

    private final DoubleBasicGrid demStripe;
    private final double[] upRow;
    private final double[] bottomRow;
    private final boolean[][] bluelinesGrid;
    private final Double bluelinesWeight;
    private final SlopeAlgo slopeAlgo;
    private final SlopeUnits slopeUnits;

}
