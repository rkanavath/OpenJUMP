package com.geomaticaeambiente.openjump.klem.fill;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.geomaticaeambiente.openjump.klem.Log;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.IntBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.Shifter;

/**
 *
 * @author deluca
 */
public class FlatAreasProcessor2 {

    @SuppressWarnings("unused")
    public DoubleBasicGrid process(DoubleBasicGrid demGrid) throws Exception {

        inputDemGrid = demGrid;

        rowCount = demGrid.getRowCount();
        colCount = demGrid.getColumnCount();

        // 1 - Calculate flow directions
        calcFlowDirs();

        // 2 - Flat edges
        calcFlatEdges();

        // 3 - Label flats
        if (Log.log) {
            System.out.println("Labelling flats");
        }
        labelFlats();

        // DoubleBasicGrid flatMaskGridDouble = new
        // DoubleBasicGrid(labelsGrid.getData(), labelsGrid.getCellSize(),
        // -9999, labelsGrid.getLowerLeftCoord());
        // if(1==1) return flatMaskGridDouble;

        // 4 - Process away from higer
        if (Log.log) {
            System.out.println("Process away from higher");
        }
        awayFromHigher();

        // DoubleBasicGrid flatMaskGridDouble = new
        // DoubleBasicGrid(flatMaskGrid.getData(), flatMaskGrid.getCellSize(),
        // -9999, flatMaskGrid.getLowerLeftCoord());
        // if(1==1) return flatMaskGridDouble;

        // 5 - Process towards lower
        if (Log.log) {
            System.out.println("Process towards lower");
        }
        towardsLower();

        final DoubleBasicGrid flatMaskGridDouble = new DoubleBasicGrid(
                flatMaskGrid.getData(), flatMaskGrid.getCellSize(), -9999,
                flatMaskGrid.getLowerLeftCoord());
        if (1 == 1) {
            return flatMaskGridDouble;
        }

        // 6 - Alter DEM
        if (Log.log) {
            System.out.println("Alter DEM");
        }
        alterDem();

        return demGrid;

    }

    // TODO: replace with parallel version
    private void calcFlowDirs() {

        final int flowDirNoData = -Integer.MAX_VALUE;

        flowDirGrid = new IntBasicGrid(new int[rowCount][colCount],
                inputDemGrid.getCellSize(), flowDirNoData,
                inputDemGrid.getLowerLeftCoord());

        for (int r = 0; r < rowCount; r++) {
            for (int c = 0; c < colCount; c++) {

                if (inputDemGrid.isNoData(inputDemGrid.getValue(c, r))) {
                    flowDirGrid.setValue(c, r, flowDirGrid.getNoData());
                } else {

                    double eMin = inputDemGrid.getValue(c, r);
                    int nMin = noFlow;

                    for (int i = 0; i < 8; i++) {

                        final int ic = c + Shifter.getColShift(i);
                        final int ir = r + Shifter.getRowShift(i);

                        if (!inputDemGrid.belongsToGrid(ic, ir)) {
                            continue;
                        }

                        if (inputDemGrid.getValue(ic, ir) < eMin) {
                            eMin = inputDemGrid.getValue(ic, ir);
                            nMin = i;
                        }

                    }

                    flowDirGrid.setValue(c, r, nMin);

                }

            }
        }

    }

    private void calcFlatEdges() {

        for (int r = 0; r < rowCount; r++) {
            for (int c = 0; c < colCount; c++) {

                if (!flowDirGrid.isNoData(flowDirGrid.getValue(c, r))) {

                    for (int i = 0; i < 8; i++) {

                        final int ic = c + Shifter.getColShift(i);
                        final int ir = r + Shifter.getRowShift(i);

                        if (!flowDirGrid.belongsToGrid(ic, ir)) {
                            continue;
                        }
                        if (flowDirGrid.isNoData(flowDirGrid.getValue(ic, ir))) {
                            continue;
                        }

                        if (flowDirGrid.getValue(c, r) != noFlow
                                && flowDirGrid.getValue(ic, ir) == noFlow
                                && inputDemGrid.getValue(c, r) == inputDemGrid
                                        .getValue(ic, ir)) {

                            lowEdges_l.add(new java.awt.Point(c, r));
                            break;

                        } else if (flowDirGrid.getValue(c, r) == noFlow
                                && inputDemGrid.getValue(c, r) < inputDemGrid
                                        .getValue(ic, ir)) {

                            highEdges_l.add(new java.awt.Point(c, r));
                            break;
                        }

                    }

                }
            }
        }

    }

    private void labelFlats() {

        final List<java.awt.Point> toFill = new ArrayList<java.awt.Point>();

        labelsGrid = new IntBasicGrid(new int[rowCount][colCount],
                inputDemGrid.getCellSize(), -Integer.MAX_VALUE,
                inputDemGrid.getLowerLeftCoord());

        lMax = 0;
        for (final Point lowEdge : lowEdges_l) {

            lMax++;

            toFill.add(lowEdge);
            final double E = inputDemGrid.getValue(lowEdge);
            while (toFill.size() > 0) {

                final Point cCell = toFill.get(toFill.size() - 1);
                toFill.remove(toFill.size() - 1);

                if (!inputDemGrid.belongsToGrid(cCell)) {
                    continue;
                }

                if (inputDemGrid.isNoData(inputDemGrid.getValue(cCell))) {
                    continue;
                }

                if (inputDemGrid.getValue(cCell) != E) {
                    continue;
                }
                if (labelsGrid.getValue(cCell) != 0) {
                    continue;
                }

                labelsGrid.setValue(cCell, lMax);

                for (int i = 0; i < 8; i++) {

                    final int ic = cCell.x + Shifter.getColShift(i);
                    final int ir = cCell.y + Shifter.getRowShift(i);

                    if (!flowDirGrid.belongsToGrid(ic, ir)) {
                        continue;
                    }

                    if (inputDemGrid.getValue(cCell) != inputDemGrid.getValue(
                            ic, ir)) {
                        continue;
                    }

                    toFill.add(new java.awt.Point(ic, ir));

                }

            }

        }

        // Purge highEdges
        for (final Iterator<Point> it = highEdges_l.iterator(); it.hasNext();) {
            final Point highEdge = it.next();
            if (labelsGrid.getValue(highEdge) == 0) {
                it.remove();
            }
        }

    }

    public void awayFromHigher() {

        flatMaskGrid = new IntBasicGrid(new int[rowCount][colCount],
                inputDemGrid.getCellSize(), -Integer.MAX_VALUE,
                inputDemGrid.getLowerLeftCoord());
        flatHeight = new int[lMax];

        int loops = 1;

        final List<Point> highEdges2_l = new ArrayList<Point>();

        boolean somethingToDo = true;
        while (somethingToDo) {

            while (highEdges_l.size() > 0) {

                final Point cCell = highEdges_l.get(highEdges_l.size() - 1);
                highEdges_l.remove(highEdges_l.size() - 1);

                if (flatMaskGrid.getValue(cCell) > 0) {
                    continue;
                }

                flatMaskGrid.setValue(cCell, loops);
                flatHeight[labelsGrid.getValue(cCell) - 1] = loops;

                for (int i = 0; i < 8; i++) {

                    final int ic = cCell.x + Shifter.getColShift(i);
                    final int ir = cCell.y + Shifter.getRowShift(i);

                    if (!flowDirGrid.belongsToGrid(ic, ir)) {
                        continue;
                    }

                    if (labelsGrid.getValue(ic, ir) == labelsGrid
                            .getValue(cCell)
                            && flowDirGrid.getValue(ic, ir) == noFlow
                            && flatMaskGrid.getValue(ic, ir) == 0) {
                        highEdges2_l.add(new Point(ic, ir));
                    }

                }

            }

            // Switch cues
            loops++;
            highEdges_l = new ArrayList<java.awt.Point>(highEdges2_l);
            highEdges2_l.clear();
            if (highEdges_l.isEmpty()) {
                somethingToDo = false;
            }

        }

        // Reverse gradient values and make flatMask negative
        for (int r = 0; r < rowCount; r++) {
            for (int c = 0; c < colCount; c++) {

                if (!flatMaskGrid.isNoData(flatMaskGrid.getValue(c, r))
                        && flatMaskGrid.getValue(c, r) > 0) {
                    flatMaskGrid
                            .setValue(c, r, -(flatHeight[labelsGrid.getValue(c,
                                    r) - 1] - flatMaskGrid.getValue(c, r)));
                }

            }
        }

    }

    private void towardsLower() {

        // flatMask already negative!
        int loops = 1;

        final List<Point> lowEdges2_l = new ArrayList<Point>();

        boolean somethingToDo = true;
        while (somethingToDo) {

            while (lowEdges_l.size() > 0) {

                final Point cCell = lowEdges_l.get(lowEdges_l.size() - 1);
                lowEdges_l.remove(lowEdges_l.size() - 1);

                if (flatMaskGrid.getValue(cCell) > 0) {
                    continue;
                }

                if (flatMaskGrid.getValue(cCell) < 0) {
                    flatMaskGrid.setValue(cCell, // flatHeight[labelsGrid.getValue(cCell)
                                                 // - 1]
                            -flatMaskGrid.getValue(cCell) + 2 * loops);
                } else {
                    flatMaskGrid.setValue(cCell, 2 * loops);
                }

                for (int i = 0; i < 8; i++) {

                    final int ic = cCell.x + Shifter.getColShift(i);
                    final int ir = cCell.y + Shifter.getRowShift(i);

                    if (!flowDirGrid.belongsToGrid(ic, ir)) {
                        continue;
                    }

                    if (labelsGrid.getValue(ic, ir) == labelsGrid
                            .getValue(cCell)
                            && flowDirGrid.getValue(ic, ir) == noFlow) {
                        lowEdges2_l.add(new Point(ic, ir));
                    }

                }

            }

            loops++;
            lowEdges_l = new ArrayList<java.awt.Point>(lowEdges2_l);
            lowEdges2_l.clear();
            if (lowEdges_l.isEmpty()) {
                somethingToDo = false;
            }

        }

    }

    private void alterDem() throws Exception {

        for (int r = 0; r < rowCount; r++) {
            for (int c = 0; c < colCount; c++) {

                if (inputDemGrid.isNoData(inputDemGrid.getValue(c, r))
                        || labelsGrid.getValue(c, r) == 0
                        || flatMaskGrid.getValue(c, r) == 0) {
                    continue;
                }

                final boolean[] nIsLower = new boolean[8];
                for (int i = 0; i < 8; i++) {

                    final int ic = c + Shifter.getColShift(i);
                    final int ir = r + Shifter.getRowShift(i);

                    if (!inputDemGrid.belongsToGrid(ic, ir)
                            || inputDemGrid.isNoData(inputDemGrid.getValue(ic,
                                    ir))
                            || labelsGrid.getValue(c, r) == labelsGrid
                                    .getValue(ic, ir)) {
                        continue;
                    }

                    if (inputDemGrid.getValue(c, r) > inputDemGrid.getValue(ic,
                            ir)) {
                        nIsLower[i] = true;
                    }

                }

                if (labelsGrid.getValue(c, r) == 8064) {
                    System.out.println("wait");
                }

                // Correction
                for (int j = 0; j < flatMaskGrid.getValue(c, r); j++) {
                    inputDemGrid.setValue(c, r, Math.nextAfter(
                            (float) inputDemGrid.getValue(c, r),
                            Double.MAX_VALUE));
                }

                // Check
                for (int i = 0; i < 8; i++) {

                    final int ic = c + Shifter.getColShift(i);
                    final int ir = r + Shifter.getRowShift(i);

                    if (!labelsGrid.belongsToGrid(ic, ir)) {
                        continue;
                    }

                    if (labelsGrid.getValue(ic, ir) == labelsGrid
                            .getValue(c, r)) {
                        continue;
                    }

                    if (inputDemGrid.getValue(c, r) > inputDemGrid.getValue(ic,
                            ir)) {
                        if (!nIsLower[i]) {
                            if (Log.log) {
                                System.out.println("Problem with col " + c
                                        + " row " + r);
                            }
                        }
                    }

                }

            }
        }

    }

    public IntBasicGrid getFlowDirGrid() {
        return flowDirGrid;
    }

    public IntBasicGrid getLabelsGrid() {
        return labelsGrid;
    }

    private int rowCount = 0;
    private int colCount = 0;
    private final int noFlow = -1;
    private int lMax = 0;

    private DoubleBasicGrid inputDemGrid;
    private IntBasicGrid flowDirGrid;
    private IntBasicGrid labelsGrid;
    private IntBasicGrid flatMaskGrid;

    private List<java.awt.Point> lowEdges_l = new ArrayList<java.awt.Point>();
    private List<java.awt.Point> highEdges_l = new ArrayList<java.awt.Point>();

    private int[] flatHeight;

}
