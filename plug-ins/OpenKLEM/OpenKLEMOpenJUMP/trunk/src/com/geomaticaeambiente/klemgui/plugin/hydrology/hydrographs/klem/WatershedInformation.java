package com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem;

import java.awt.Point;
import java.io.File;
import java.io.IOException;

import javax.swing.JTextField;

import org.openjump.core.rasterimage.RasterImageIO;

import com.geomaticaeambiente.klemgui.exceptions.WarningException;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.utils.WatershedTool;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 *
 * @author Geomatica
 */
public class WatershedInformation {

    public WatershedInformation(PlugInContext context, JTextField textFieldX,
            JTextField textFieldY) {
        this.context = context;
        this.textFieldX = textFieldX;
        this.textFieldY = textFieldY;
        calcArea = false;
        calcElevation = false;
    }

    public WatershedInformation(PlugInContext context, JTextField textFieldX,
            JTextField textFieldY, JTextField textFieldArea,
            JTextField texFieldElevation
    /* RasterImageLayer demRaster, RasterImageLayer upslopeRaster */) {
        this.context = context;
        this.textFieldX = textFieldX;
        this.textFieldY = textFieldY;
        this.textFieldArea = textFieldArea;
        textFieldElevation = texFieldElevation;
        calcArea = true;
        calcElevation = true;
    }

    public void setCoordinateText() throws IOException, Exception {

        textFieldX.setText(PluginUtils.getThreeDecimalFormatToString(coord.x));
        textFieldY.setText(PluginUtils.getThreeDecimalFormatToString(coord.y));
    }

    public void setAreaText() throws IOException, Exception {

        final double value = RasterImageIO.readCellValue(
                rasterForArea.getAbsolutePath(), coord, 0);
        final RasterImageIO.CellSizeXY cellSize = RasterImageIO
                .getCellSize(rasterForArea.getAbsolutePath());

        // convert to km2
        final double area = value
                * (cellSize.getCellSizeX() * cellSize.getCellSizeY()) / 1000000;

        textFieldArea.setText(PluginUtils.getFourDecimalFormatToString(area));

        // textFieldArea.setText(Double.toString(area));
    }

    public void setElevationText() throws IOException, Exception {

        final double value = RasterImageIO.readCellValue(
                rasterForElevation.getAbsolutePath(), coord, 0);

        textFieldElevation.setText(Double.toString(value));
    }

    public void setCoordinate(Coordinate coord) throws Exception {

        if (rasterEnvelope != null) {
            if (rasterEnvelope.contains(coord)) {
                this.coord = coord;

            } else {
                throw new WarningException(
                        PluginUtils
                                .getResources()
                                .getString(
                                        "HydrographKlemPlugin.CoordsOutsideRaster.message"));
            }
            // TODO: delete stack trace when there is the error
        } else {
            this.coord = coord;
        }

    }

    public void getCoordinate() {
        final WatershedTool wt = new WatershedTool(context, this);
        context.getLayerViewPanel().setCurrentCursorTool(wt);
    }

    public boolean isCalcArea() {
        return calcArea;
    }

    public boolean isCalcElevation() {
        return calcElevation;
    }

    public void setRasterForArea(File rasterForArea) {
        this.rasterForArea = rasterForArea;
    }

    public void setRasterForElevation(File rasterForElevation)
            throws IOException, Exception {

        // Memorize raster envelope
        final Point point = RasterImageIO.getImageDimensions(rasterForElevation
                .getAbsolutePath());
        rasterEnvelope = RasterImageIO.getGeoReferencing(
                rasterForElevation.getAbsolutePath(), true, point);

        this.rasterForElevation = rasterForElevation;
    }

    public void setRasterEnvelope(Envelope env) {
        rasterEnvelope = env;
    }

    private Coordinate coord;
    private final PlugInContext context;
    private final JTextField textFieldX;
    private final JTextField textFieldY;
    private JTextField textFieldArea;
    private JTextField textFieldElevation;
    private final boolean calcArea;
    private final boolean calcElevation;
    private File rasterForArea;
    private File rasterForElevation;
    private Envelope rasterEnvelope;
}
