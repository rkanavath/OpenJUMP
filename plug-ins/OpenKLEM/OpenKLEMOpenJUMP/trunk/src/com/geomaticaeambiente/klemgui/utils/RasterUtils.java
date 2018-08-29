package com.geomaticaeambiente.klemgui.utils;

import java.awt.Point;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.image.BandedSampleModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferFloat;
import java.awt.image.Raster;
import java.awt.image.SampleModel;
import java.awt.image.WritableRaster;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.openjump.core.rasterimage.ImageAndMetadata;
import org.openjump.core.rasterimage.RasterImageIO;
import org.openjump.core.rasterimage.RasterImageIO.CellSizeXY;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.rasterimage.RasterSymbology;
import org.openjump.core.rasterimage.Resolution;
import org.openjump.core.rasterimage.TiffTags;
import org.openjump.core.rasterimage.TiffTags.TiffReadingException;
import org.openjump.util.metaData.MetaInformationHandler;

import com.geomaticaeambiente.klemgui.ui.CustomComboBox.RasterComboBox;
import com.geomaticaeambiente.klemgui.ui.Symbologies;
import com.geomaticaeambiente.openjump.klem.grid.ByteBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel2.DoubleStripeGrid2;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.LayerEventType;
import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.Viewport;

/**
 *
 * @author Geomatica
 */
public class RasterUtils {

    public static File lastVisitedFolder = null;

    public static double[] getSingleArray(double[][] doubleArray) {

        final double[] data = new double[doubleArray.length
                * doubleArray[0].length];
        int count = 0;

        for (final double[] element : doubleArray) {
            for (int c = 0; c < doubleArray[0].length; c++) {

                data[count] = element[c];
                count++;

            }
        }

        return data;

    }

    public static float[] getSingleFloatArray(double[][] doubleArray) {

        final float[] data = new float[doubleArray.length
                * doubleArray[0].length];
        int count = 0;

        for (final double[] element : doubleArray) {
            for (int c = 0; c < doubleArray[0].length; c++) {

                data[count] = (float) element[c];
                count++;

            }
        }

        return data;

    }

    /**
     * Method to display a RasterImageLayer on OpenJUMP
     * 
     * @param WorkbenchContext
     * @param RasterImageLayer
     * @throws NoninvertibleTransformException
     */
    public static void addImageToOJ(WorkbenchContext context,
            RasterImageLayer ril) throws NoninvertibleTransformException {

        final String catName = StandardCategoryNames.WORKING;
        final int layersAsideImage = context.getLayerManager()
                .getLayerables(Layerable.class).size();
        // #################################
        final MetaInformationHandler mih = new MetaInformationHandler(ril);

        mih.addMetaInformation(I18N.get("file-name"), "");
        mih.addMetaInformation(I18N.get("resolution"), "" + " (px) x " + ""
                + " (px)");
        mih.addMetaInformation(I18N.get("real-world-width"), ril
                .getWholeImageEnvelope().getWidth());
        mih.addMetaInformation(I18N.get("real-world-height"), ril
                .getWholeImageEnvelope().getHeight());
        // ###################################
        context.getLayerManager().addLayerable(catName, ril);

        // context.getLayerViewPanel().getViewport().zoom(ril.getOriginalImageEnvelope());

    }

    /**
     * Load TIF file into OpenJUMP workbench
     * 
     * @param File
     *            file to load es D:/Openjump/test.tif
     * @param PlugInContext
     *            Plugin Context
     * @param String
     *            Category to locate rasters
     * @throws NoninvertibleTransformException
     *             , TiffReadingException, Exception
     */

    public static void addFileToOJ(PlugInContext context, File file,
            String category) throws NoninvertibleTransformException,
            TiffReadingException, Exception {

        final RasterImageIO rasterImageIO = new RasterImageIO();
        final Viewport viewport = context.getLayerViewPanel().getViewport();
        final Resolution requestedRes = RasterImageIO
                .calcRequestedResolution(viewport);
        final ImageAndMetadata imageAndMetadata = rasterImageIO.loadImage(
                context.getWorkbenchContext(), file.getAbsolutePath(), null,
                viewport.getEnvelopeInModelCoordinates(), requestedRes);
        final Point point = RasterImageIO.getImageDimensions(file
                .getAbsolutePath());
        final Envelope env = RasterImageIO.getGeoReferencing(
                file.getAbsolutePath(), true, point);

        final RasterImageLayer ril = new RasterImageLayer(file.getName(),
                context.getLayerManager(), file.getAbsolutePath(),
                imageAndMetadata.getImage(), env);
        // String catName = StandardCategoryNames.RESULT;
        // ########Metadata#############
        final MetaInformationHandler mih = new MetaInformationHandler(ril);

        mih.addMetaInformation(I18N.get("file-name"), "");
        mih.addMetaInformation(I18N.get("resolution"), "" + " (px) x " + ""
                + " (px)");
        mih.addMetaInformation(I18N.get("real-world-width"), ril
                .getWholeImageEnvelope().getWidth());
        mih.addMetaInformation(I18N.get("real-world-height"), ril
                .getWholeImageEnvelope().getHeight());
        // mih.addMetaInformation("srid", ril.getSRSInfo().getCode());
        // mih.addMetaInformation("srid-location",
        // ril.getSRSInfo().getSource());
        // #################################
        context.getLayerManager().addLayerable(category, ril);
        context.getLayerManager().fireLayerChanged(ril,
                LayerEventType.APPEARANCE_CHANGED);

    }

    public static void addFileToOJ(PlugInContext context, File file,
            RasterSymbology symbology, String category)
            throws NoninvertibleTransformException, TiffReadingException,
            Exception {

        final RasterImageIO rasterImageIO = new RasterImageIO();
        final Viewport viewport = context.getLayerViewPanel().getViewport();
        final Resolution requestedRes = RasterImageIO
                .calcRequestedResolution(viewport);
        final ImageAndMetadata imageAndMetadata = rasterImageIO.loadImage(
                context.getWorkbenchContext(), file.getAbsolutePath(), null,
                viewport.getEnvelopeInModelCoordinates(), requestedRes);
        final Point point = RasterImageIO.getImageDimensions(file
                .getAbsolutePath());
        final Envelope env = RasterImageIO.getGeoReferencing(
                file.getAbsolutePath(), true, point);

        final RasterImageLayer ril = new RasterImageLayer(file.getName(),
                context.getLayerManager(), file.getAbsolutePath(),
                imageAndMetadata.getImage(), env);
        ril.setSymbology(symbology);
        // String catName = StandardCategoryNames.RESULT;
        // ########Metadata#############
        final MetaInformationHandler mih = new MetaInformationHandler(ril);

        mih.addMetaInformation(I18N.get("file-name"), "");
        mih.addMetaInformation(I18N.get("resolution"), "" + " (px) x " + ""
                + " (px)");
        mih.addMetaInformation(I18N.get("real-world-width"), ril
                .getWholeImageEnvelope().getWidth());
        mih.addMetaInformation(I18N.get("real-world-height"), ril
                .getWholeImageEnvelope().getHeight());
        // mih.addMetaInformation("srid", ril.getSRSInfo().getCode());
        // mih.addMetaInformation("srid-location",
        // ril.getSRSInfo().getSource());
        // #################################
        context.getLayerManager().addLayerable(category, ril);
        context.getLayerManager().fireLayerChanged(ril,
                LayerEventType.APPEARANCE_CHANGED);

    }

    public static void addUpSlopeFileToOJAndSym(PlugInContext context,
            File file, String category) throws NoninvertibleTransformException,
            TiffReadingException, Exception {

        final RasterImageIO rasterImageIO = new RasterImageIO();
        final Viewport viewport = context.getLayerViewPanel().getViewport();
        final Resolution requestedRes = RasterImageIO
                .calcRequestedResolution(viewport);
        final ImageAndMetadata imageAndMetadata = rasterImageIO.loadImage(
                context.getWorkbenchContext(), file.getAbsolutePath(), null,
                viewport.getEnvelopeInModelCoordinates(), requestedRes);
        final Point point = RasterImageIO.getImageDimensions(file
                .getAbsolutePath());
        final Envelope env = RasterImageIO.getGeoReferencing(
                file.getAbsolutePath(), true, point);

        final RasterImageLayer ril = new RasterImageLayer(file.getName(),
                context.getLayerManager(), file.getAbsolutePath(),
                imageAndMetadata.getImage(), env);
        ril.setSymbology(Symbologies.getUpslopeAreaSymb(ril.getMetadata()
                .getOriginalCellSize()));

        // String catName = StandardCategoryNames.RESULT;
        // ########Metadata#############
        final MetaInformationHandler mih = new MetaInformationHandler(ril);

        mih.addMetaInformation(I18N.get("file-name"), "");
        mih.addMetaInformation(I18N.get("resolution"), "" + " (px) x " + ""
                + " (px)");
        mih.addMetaInformation(I18N.get("real-world-width"), ril
                .getWholeImageEnvelope().getWidth());
        mih.addMetaInformation(I18N.get("real-world-height"), ril
                .getWholeImageEnvelope().getHeight());
        // mih.addMetaInformation("srid", ril.getSRSInfo().getCode());
        // mih.addMetaInformation("srid-location",
        // ril.getSRSInfo().getSource());
        // #################################
        context.getLayerManager().addLayerable(category, ril);

    }

    // NOT CORRECT TO MODIFY
    /**
     * Method to produce a DoubleStripGrid2 from a rasterImageLayer
     * 
     * @param rasterImageLayer
     * @return
     */
    public static DoubleStripeGrid2 getDoubleStripeGrid(
            RasterImageLayer rasterImageLayer) throws IOException {

        final DoubleBasicGrid doubleBasicGrid = RasterUtils
                .getDoubleBasicGrid(rasterImageLayer);

        final DoubleStripeGrid2 doubleStripGrid = new DoubleStripeGrid2(
                doubleBasicGrid, doubleBasicGrid, doubleBasicGrid);

        return doubleStripGrid;
    }

    // for rasters
    /**
     * Method to produce a DoubleBasicGrid from a RasterImageLayer
     * 
     * @param rasterImageLayer
     * @return
     */
    public static DoubleBasicGrid getDoubleBasicGrid(
            RasterImageLayer rasterImageLayer) throws IOException {

        final int nRows = rasterImageLayer.getRasterData(null).getHeight();
        final int nCols = rasterImageLayer.getRasterData(null).getWidth();
        final double noData = rasterImageLayer.getNoDataValue();
        final Envelope env = rasterImageLayer.getWholeImageEnvelope();
        final double cellsize = (env.getMaxX() - env.getMinX()) / nCols;

        final Raster raster = rasterImageLayer.getRasterData(null);
        final DataBuffer databuffer = raster.getDataBuffer();

        // System.out.println(databuffer.getSize());

        final double[][] rasterData = new double[nRows][nCols];
        int countElements = 0;

        for (int r = 0; r < rasterData.length; r++) {
            for (int c = 0; c < rasterData[0].length; c++) {
                rasterData[r][c] = databuffer.getElemDouble(countElements);
                countElements++;
            }
        }
        final Coordinate coords = new Coordinate(env.getMinX(), env.getMinY());
        final DoubleBasicGrid doubleBasicGrid = new DoubleBasicGrid(rasterData,
                cellsize, noData, coords);

        return doubleBasicGrid;
    }

    public static Envelope getGridEnvelope(File gridFile) throws IOException,
            Exception {

        final Point imageDims = RasterImageIO.getImageDimensions(gridFile
                .getAbsolutePath());
        return RasterImageIO.getGeoReferencing(gridFile.getAbsolutePath(),
                true, imageDims);

    }

    public static DoubleBasicGrid getDoubleBasicGridFromFile(File gridFile)
            throws IOException, Exception {

        final Point imageDims = RasterImageIO.getImageDimensions(gridFile
                .getAbsolutePath());
        final CellSizeXY cellSize = RasterImageIO.getCellSize(gridFile
                .getAbsolutePath());
        final Envelope envelope = RasterImageIO.getGeoReferencing(
                gridFile.getAbsolutePath(), true, imageDims);

        final DataBufferFloat dataBuffer = (DataBufferFloat) RasterImageIO
                .loadRasterData(gridFile.getAbsolutePath(), null)
                .getDataBuffer();
        return new DoubleBasicGrid(getDataArray(dataBuffer, imageDims.x,
                imageDims.y), cellSize.getCellSizeX(), -9999, new Coordinate(
                envelope.getMinX(), envelope.getMinY()));

    }

    public static double[][] getDataArray(DataBufferFloat dataBuffer,
            int colsCount, int rowsCount) {

        final double[][] data = new double[rowsCount][colsCount];
        int pos = 0;
        for (int r = 0; r < rowsCount; r++) {
            for (int c = 0; c < colsCount; c++) {
                data[r][c] = dataBuffer.getElemFloat(pos);
                pos++;
            }
        }
        return data;

    }

    public static DoubleBasicGrid getDoubleBasicGrid(RasterComboBox comboBox)
            throws IOException {

        final RasterImageLayer inputRasterSelected = PluginUtils
                .getRasterImageLayerSelected(comboBox);
        return RasterUtils.getDoubleBasicGrid(inputRasterSelected);
    }

    public static ByteBasicGrid getByteBasicGrid(RasterComboBox comboBox)
            throws IOException {

        final RasterImageLayer inputRasterSelected = PluginUtils
                .getRasterImageLayerSelected(comboBox);
        return RasterUtils.getByteBasicGrid(inputRasterSelected);
    }

    public static ByteBasicGrid getByteBasicGrid(
            RasterImageLayer rasterImageLayer) throws IOException {

        final DoubleBasicGrid doubleBasicGrid = getDoubleBasicGrid(rasterImageLayer);

        final Double noData = doubleBasicGrid.getNoData();
        final byte noDataValue = noData.byteValue();

        return new ByteBasicGrid(doubleBasicGrid.getData(),
                doubleBasicGrid.getCellSize(), noDataValue,
                doubleBasicGrid.getLowerLeftCoord());
    }

    public static FlowDirBasicGrid getFlowDirBasicGrid(RasterComboBox comboBox)
            throws IOException {

        final RasterImageLayer inputRasterSelected = PluginUtils
                .getRasterImageLayerSelected(comboBox);
        return RasterUtils.getFlowDirBasicGrid(inputRasterSelected);
    }

    public static FlowDirBasicGrid getFlowDirBasicGrid(
            RasterImageLayer rasterImageLayer) throws IOException {

        final DoubleBasicGrid doubleBasicGrid = getDoubleBasicGrid(rasterImageLayer);
        final double noData = doubleBasicGrid.getNoData();

        final byte[][] flowDirData = new byte[doubleBasicGrid.getRowCount()][doubleBasicGrid
                .getColumnCount()];
        final boolean[][] noDataData = new boolean[doubleBasicGrid
                .getRowCount()][doubleBasicGrid.getColumnCount()];

        for (int r = 0; r < flowDirData.length; r++) {
            for (int c = 0; c < flowDirData[r].length; c++) {
                if (doubleBasicGrid.isNoData(doubleBasicGrid.getValue(c, r))) {
                    noDataData[r][c] = true;
                } else {
                    noDataData[r][c] = false;
                    flowDirData[r][c] = (new Double(doubleBasicGrid.getValue(c,
                            r))).byteValue();
                }
            }
        }

        return new FlowDirBasicGrid(flowDirData, noDataData,
                doubleBasicGrid.getCellSize(),
                doubleBasicGrid.getLowerLeftCoord());
    }

    public static void displayRasterFileOnOJ(WorkbenchContext context,
            File rasterFile, RasterSymbology symbology)
            throws NoninvertibleTransformException, IOException,
            TiffTags.TiffReadingException, Exception {
        // Deactivated option to load/unload output raster
        // if (OptionPlugIn.loadOutputRaster()) {
        // convert DoubleBasicGrid in RasterImageLayer
        final RasterImageLayer ril = getRasterImageLayerFromFile(context,
                rasterFile);

        // add reclassified raster to OJ
        RasterUtils.addImageToOJ(context, ril);

        if (symbology != null) {
            ril.setSymbology(symbology);
        }
        // }
    }

    public static double[][] getRasterAsArray(Raster raster) {

        final int nRow = raster.getHeight();
        final int nCols = raster.getWidth();

        final DataBuffer dataBuffer = raster.getDataBuffer();
        final int dataType = dataBuffer.getDataType();
        final int size = dataBuffer.getSize();

        final double[][] arrayValues = new double[nRow][nCols];
        if (dataType == DataBuffer.TYPE_DOUBLE
                || dataType == DataBuffer.TYPE_FLOAT) {

            int countElem = 0;
            while (countElem < size) {
                for (int r = 0; r < nRow; r++) {
                    for (int c = 0; c < nCols; c++) {
                        arrayValues[r][c] = dataBuffer.getElemFloat(countElem);
                        countElem++;
                    }
                }
            }
        }

        return arrayValues;

    }

    // public static void getColorMapEntries(RasterImageLayer rasterImageLayer)
    // throws Exception {

    // double[] upslopeColorModelValues = new double[17];
    // Color[] upslopeColorModelColors= new Color[17];
    //
    // upslopeColorModelValues[0] = 0;
    // for(int n=1; n<16; n++){
    // upslopeColorModelValues[n-1] = Math.pow(2, n);
    // }
    //
    //
    // upslopeColorModelColors[0] = new Color(255, 255, 255);
    // upslopeColorModelColors[1] = new Color(237, 237, 255);
    // upslopeColorModelColors[2] = new Color(219, 219, 255);
    // upslopeColorModelColors[3] = new Color(200, 200, 255);
    // upslopeColorModelColors[4] = new Color(182, 182, 255);
    // upslopeColorModelColors[5] = new Color(164, 164, 255);
    // upslopeColorModelColors[6] = new Color(146, 146, 255);
    // upslopeColorModelColors[7] = new Color(128, 128, 255);
    // upslopeColorModelColors[8] = new Color(109, 109, 255);
    // upslopeColorModelColors[9] = new Color(91, 91, 255);
    // upslopeColorModelColors[10] = new Color(73, 73, 255);
    // upslopeColorModelColors[11] = new Color(55, 55, 255);
    // upslopeColorModelColors[12] = new Color(36, 36, 255);
    // upslopeColorModelColors[13] = new Color(18,18, 255);
    // upslopeColorModelColors[14] = new Color(9,9, 255);
    // upslopeColorModelColors[15] = new Color(0, 0, 255);
    //
    // // upslopeColorModelColors[16] = null;
    // // upslopeColorModelValues[16] = -9999.0;
    //
    // ColorMapEntry[] colorMapEntries;
    // try {
    // colorMapEntries = new ColorMapEntry[upslopeColorModelValues.length];
    // for(int r=0; r<colorMapEntries.length; r++) {
    // colorMapEntries[r] = new ColorMapEntry(
    // upslopeColorModelValues[r],
    // upslopeColorModelColors[r]);
    // }
    //
    //
    // RasterStyler rasterSymbolizer = new
    // RasterStyler(RasterStyler.ColorMapType.INTERVALS);
    // for (ColorMapEntry colorMapEntry : colorMapEntries) {
    // rasterSymbolizer.addColorMapEntry(colorMapEntry);
    // }
    //
    // StylePlugger stylePlugger = new StylePlugger(rasterImageLayer);
    // stylePlugger.plug(rasterSymbolizer);
    //
    // rasterImageLayer.setEnvelope(rasterImageLayer.getEnvelope());
    //
    // } catch (Exception ex) {
    // throw new Exception("Error in table: " + ex);
    // }

    // }

    public static void saveOutputRasterAsTiff(ByteBasicGrid byteBasicGrid,
            File outputRaster) throws IOException {

        final DoubleBasicGrid doubleBasicGrid = new DoubleBasicGrid(
                byteBasicGrid.getData(), byteBasicGrid.getCellSize(),
                byteBasicGrid.getNoData(), byteBasicGrid.getLowerLeftCoord());

        saveOutputRasterAsTiff(doubleBasicGrid, outputRaster);

    }

    public static void saveOutputRasterAsTiff(
            FlowDirBasicGrid flowDirBasicGrid, File outputRaster)
            throws IOException {

        // Convert from signed to unsigned
        final int[][] intFlowDirs = new int[flowDirBasicGrid.getRowCount()][flowDirBasicGrid
                .getColumnCount()];

        final int noData = -9999;
        for (int r = 0; r < intFlowDirs.length; r++) {
            for (int c = 0; c < intFlowDirs[r].length; c++) {
                if (flowDirBasicGrid.getNoDataData()[r][c] == true) {
                    intFlowDirs[r][c] = noData;
                } else {
                    int outVal = flowDirBasicGrid.getByteFlowdirValue(c, r);
                    if (outVal < 0) {
                        outVal += 256;
                    }
                    intFlowDirs[r][c] = outVal;
                }
            }
        }

        final DoubleBasicGrid doubleBasicGrid = new DoubleBasicGrid(
                intFlowDirs, flowDirBasicGrid.getCellSize(), noData,
                flowDirBasicGrid.getLowerLeftCoord());

        saveOutputRasterAsTiff(doubleBasicGrid, outputRaster);

    }

    public static void saveOutputRasterAsTiff(DoubleBasicGrid doubleBaseGrid,
            File outputRaster) throws IOException {

        final int nCols = doubleBaseGrid.getColumnCount();
        final int nRows = doubleBaseGrid.getRowCount();
        final double cellSize = doubleBaseGrid.getCellSize();
        final double noData = doubleBaseGrid.getNoData();

        final SampleModel sampleModel = new BandedSampleModel(
                DataBuffer.TYPE_FLOAT, nCols, nRows, 1);
        // SampleModel sampleModel =
        // RasterFactory.createBandedSampleModel(DataBuffer.TYPE_FLOAT, nCols,
        // nRows, 1);
        final DataBuffer db = new DataBufferFloat(
                RasterUtils.getSingleFloatArray(doubleBaseGrid.getData()),
                nCols * nRows);
        final java.awt.Point point = new java.awt.Point();
        point.setLocation(0, 0);
        final WritableRaster writableRaster = WritableRaster
                .createWritableRaster(sampleModel, db, point);

        // Set Envelope
        final double xmin = doubleBaseGrid.getLowerLeftCoord().x;
        final double ymin = doubleBaseGrid.getLowerLeftCoord().y;

        final double xmax = xmin
                + (doubleBaseGrid.getCellSize() * doubleBaseGrid
                        .getColumnCount());
        final double ymax = ymin
                + (doubleBaseGrid.getCellSize() * doubleBaseGrid.getRowCount());

        final Envelope env = new Envelope(xmin, xmax, ymin, ymax);

        // check output file raster
        final File rasterFile = checkRasterName(outputRaster, "tif");

        final RasterImageIO rasterImageIO = new RasterImageIO();
        rasterImageIO.writeImage(rasterFile, writableRaster, env,
                rasterImageIO.new CellSizeXY(cellSize, cellSize), noData);

    }

    public static void saveOutputRasterAsTiff(
            RasterImageLayer rasterImageLayer, File file) throws IOException {

        final Raster raster = rasterImageLayer.getRasterData(null);
        final Envelope env = rasterImageLayer.getWholeImageEnvelope();
        final int nRows = rasterImageLayer.getMetadata().getOriginalSize().x;
        final int nCols = rasterImageLayer.getMetadata().getOriginalSize().y;
        final double noData = rasterImageLayer.getMetadata().getNoDataValue();
        final double cellSize = rasterImageLayer.getMetadata()
                .getOriginalCellSize();

        file = checkRasterName(file, "tif");

        final RasterImageIO rasterImageIO = new RasterImageIO();
        rasterImageIO.writeImage(file, raster, env,
                rasterImageIO.new CellSizeXY(cellSize, cellSize), noData);
    }

    public static RasterImageLayer getRasterImageLayerFromFile(
            WorkbenchContext wContext, File rasterFile) throws IOException,
            NoninvertibleTransformException, FileNotFoundException,
            TiffTags.TiffReadingException, Exception {

        final RasterImageIO rasterImageIO = new RasterImageIO();
        // Raster raster =
        // RasterImageIO.loadWholeRasterData(rasterFile.getAbsolutePath());
        //
        // Viewport viewport = wContext.getLayerViewPanel().getViewport();
        // ImageAndMetadata imageAdnMetadata = rasterImageIO.loadImage(wContext,
        // rasterFile.getAbsolutePath(), null,
        // viewport.getEnvelopeInModelCoordinates(),
        // RasterImageIO.calcRequestedResolution(viewport));
        // BufferedImage bufferedImage = imageAdnMetadata.getImage();

        final Point point = RasterImageIO.getImageDimensions(rasterFile
                .getAbsolutePath());
        final Envelope env = RasterImageIO.getGeoReferencing(
                rasterFile.getAbsolutePath(), true, point);

        final Viewport viewport = wContext.getLayerViewPanel().getViewport();
        final Resolution requestedRes = RasterImageIO
                .calcRequestedResolution(viewport);
        final ImageAndMetadata imageAndMetadata = rasterImageIO.loadImage(
                wContext, rasterFile.getAbsolutePath(), null,
                viewport.getEnvelopeInModelCoordinates(), requestedRes);
        return new RasterImageLayer(rasterFile.getName(),
                wContext.getLayerManager(), rasterFile.getAbsolutePath(),
                imageAndMetadata.getImage(), env);
    }

    public static File checkRasterName(File rasterFile, String extension) {

        if (rasterFile.getAbsolutePath().toLowerCase().endsWith(extension)) {
            return rasterFile;
        } else {
            return new File(rasterFile.getAbsolutePath()
                    .concat("." + extension));
        }
    }

    public static void rasterCompare(File raster1, File raster2)
            throws Exception {

        raster1 = checkRasterName(raster1, "tif");
        raster2 = checkRasterName(raster2, "tif");

        if (raster1.equals(raster2)) {
            throw new Exception(
                    PluginUtils
                            .getResources()
                            .getString("HydrographKlemPlugin.Raster1.message")
                            .concat(raster1.getName())
                            .concat(PluginUtils
                                    .getResources()
                                    .getString(
                                            "HydrographKlemPlugin.Raster1And.message")
                                    .concat(raster2.getName())
                                    .concat(PluginUtils
                                            .getResources()
                                            .getString(
                                                    "HydrographKlemPlugin.SameRaster.message"))));
        }

        final RasterImageIO.CellSizeXY cellSizeX_1 = RasterImageIO
                .getCellSize(raster1.getAbsolutePath());
        final RasterImageIO.CellSizeXY cellSizeX_2 = RasterImageIO
                .getCellSize(raster2.getAbsolutePath());

        if (cellSizeX_1.getCellSizeX() != cellSizeX_2.getCellSizeX()) {
            throw new Exception(
                    PluginUtils
                            .getResources()
                            .getString("HydrographKlemPlugin.Raster1.message")
                            .concat(raster1.getName())
                            .concat(PluginUtils
                                    .getResources()
                                    .getString(
                                            "HydrographKlemPlugin.Raster1And.message")
                                    .concat(raster2.getName())
                                    .concat(PluginUtils
                                            .getResources()
                                            .getString(
                                                    "HydrographKlemPlugin.DifferentCellSize.message"))));
        }

        final Point point_1 = RasterImageIO.getImageDimensions(raster1
                .getAbsolutePath());
        final Point point_2 = RasterImageIO.getImageDimensions(raster2
                .getAbsolutePath());

        if (point_1.getX() != point_2.getX()
                || point_1.getY() != point_2.getY()) {
            throw new Exception(
                    PluginUtils
                            .getResources()
                            .getString("HydrographKlemPlugin.Raster1.message")
                            .concat(raster1.getName())
                            .concat(PluginUtils
                                    .getResources()
                                    .getString(
                                            "HydrographKlemPlugin.Raster1And.message")
                                    .concat(raster2.getName())
                                    .concat(PluginUtils
                                            .getResources()
                                            .getString(
                                                    "HydrographKlemPlugin.DifferentRowCol.message"))));
        }

        final Envelope env_1 = RasterImageIO.getGeoReferencing(
                raster1.getAbsolutePath(), true, point_1);
        final Envelope env_2 = RasterImageIO.getGeoReferencing(
                raster2.getAbsolutePath(), true, point_2);

        final Coordinate llC_1 = new Coordinate(env_1.getMinX(),
                env_1.getMinY());
        final Coordinate llC_2 = new Coordinate(env_2.getMinX(),
                env_2.getMinY());

        if (!llC_1.equals(llC_2)) {
            throw new Exception(
                    PluginUtils
                            .getResources()
                            .getString("HydrographKlemPlugin.Raster1.message")
                            .concat(raster1.getName())
                            .concat(PluginUtils
                                    .getResources()
                                    .getString(
                                            "HydrographKlemPlugin.Raster1And.message")
                                    .concat(raster2.getName())
                                    .concat(PluginUtils
                                            .getResources()
                                            .getString(
                                                    "HydrographKlemPlugin.DifferentOrigin.message"))));
        }
    }

    public static void rasterCompare(RasterImageLayer raster1,
            RasterImageLayer raster2) throws Exception {

        // check if raster1 and raster2 are the same file
        if (raster1.getImageFileName().equals(raster2.getImageFileName())) {
            throw new Exception(
                    PluginUtils
                            .getResources()
                            .getString("HydrographKlemPlugin.Raster1.message")
                            .concat(raster1.getName())
                            .concat(PluginUtils
                                    .getResources()
                                    .getString(
                                            "HydrographKlemPlugin.Raster1And.message")
                                    .concat(raster2.getName())
                                    .concat(PluginUtils
                                            .getResources()
                                            .getString(
                                                    "HydrographKlemPlugin.SameRaster.message"))));
        }

        // check cell size
        if (raster1.getMetadata().getOriginalCellSize() != raster2
                .getMetadata().getOriginalCellSize()) {
            throw new Exception(
                    PluginUtils
                            .getResources()
                            .getString("HydrographKlemPlugin.Raster1.message")
                            .concat(raster1.getName())
                            .concat(PluginUtils
                                    .getResources()
                                    .getString(
                                            "HydrographKlemPlugin.Raster1And.message")
                                    .concat(raster2.getName())
                                    .concat(PluginUtils
                                            .getResources()
                                            .getString(
                                                    "HydrographKlemPlugin.DifferentCellSize.message"))));
        }

        if (raster1.getMetadata().getOriginalSize().x != raster2.getMetadata()
                .getOriginalSize().x
                || raster1.getMetadata().getOriginalSize().y != raster1
                        .getMetadata().getOriginalSize().y) {
            throw new Exception(
                    PluginUtils
                            .getResources()
                            .getString("HydrographKlemPlugin.Raster1.message")
                            .concat(raster1.getName())
                            .concat(PluginUtils
                                    .getResources()
                                    .getString(
                                            "HydrographKlemPlugin.Raster1And.message")
                                    .concat(raster2.getName())
                                    .concat(PluginUtils
                                            .getResources()
                                            .getString(
                                                    "HydrographKlemPlugin.DifferentRowCol.message"))));
        }

        if (!raster1.getMetadata().getOriginalImageLowerLeftCoord()
                .equals(raster2.getMetadata().getOriginalImageLowerLeftCoord())) {
            throw new Exception(
                    PluginUtils
                            .getResources()
                            .getString("HydrographKlemPlugin.Raster1.message")
                            .concat(raster1.getName())
                            .concat(PluginUtils
                                    .getResources()
                                    .getString(
                                            "HydrographKlemPlugin.Raster1And.message")
                                    .concat(raster2.getName())
                                    .concat(PluginUtils
                                            .getResources()
                                            .getString(
                                                    "HydrographKlemPlugin.DifferentOrigin.message"))));
        }

    }

    // //metodo promemoria per creare un file e viasualizzare il raster in OJ
    // non dal file
    // public static RasterImageLayer getRasterImageLayerTemp(WorkbenchContext
    // context, DoubleBasicGrid doubleBaseGrid, double minValue, double
    // maxValue) throws FileNotFoundException, IOException{
    //
    // //get PlanarImage
    // double[] singlearray =
    // RasterUtils.getSingleArray(doubleBaseGrid.getData());
    // int nCols = doubleBaseGrid.getColumnCount();
    // int nRows = doubleBaseGrid.getRowCount();
    // DataBuffer db = new DataBufferDouble(singlearray, nCols*nRows);
    // SampleModel sampleModel =
    // RasterFactory.createBandedSampleModel(DataBuffer.TYPE_DOUBLE, nCols,
    // nRows, 1);
    //
    // Point rasterOriginPoint = new Point(0, 0);
    // Raster raster = RasterFactory.createRaster(sampleModel, db,
    // rasterOriginPoint );
    //
    // ColorModel colorModel = PlanarImage.createColorModel(sampleModel);
    // TiledImage tiledImage = new TiledImage(0, 0, nCols, nRows, 0, 0,
    // sampleModel, colorModel); //DA MODIFICARE I PRIMI DUE VALORI
    // tiledImage.setData(raster);
    //
    //
    // // WRITE OUTPUT FILE CODE
    // // try{
    // // File fileHeader = new
    // File("C:\\Users\\Geomatica\\Documents\\Prova_Out.hdr");
    // // BufferedWriter buffWrite = new BufferedWriter(new
    // FileWriter(fileHeader));
    // //
    // // buffWrite.write("ncols" + " " + nCols);
    // // buffWrite.newLine();
    // //
    // // buffWrite.write("nrows" + " " + nRows);
    // // buffWrite.newLine();
    // //
    // // buffWrite.write("xllcorner" + " " +
    // doubleBaseGrid.getLowerLeftCoord().x);
    // // buffWrite.newLine();
    // //
    // // buffWrite.write("yllcorner" + " " +
    // doubleBaseGrid.getLowerLeftCoord().y);
    // // buffWrite.newLine();
    // //
    // // buffWrite.write("cellsize" + " " + doubleBaseGrid.getCellSize());
    // // buffWrite.newLine();
    // //
    // // buffWrite.write("NODATA_value" + " " + doubleBaseGrid.getNoData());
    // // buffWrite.newLine();
    // //
    // // buffWrite.write("byteorder" + " " + "LSBFIRST");
    // // buffWrite.newLine();
    // //
    // // buffWrite.close();
    // // buffWrite = null;
    // //
    // //
    // // }catch(IOException IOExc){
    // // JOptionPane.showMessageDialog(null, "Error while reading hdr file: " +
    // IOExc, "Error", JOptionPane.ERROR_MESSAGE);
    // // }
    // //
    // // File fileOut = new
    // File("C:\\Users\\Geomatica\\Documents\\Prova_Out.flt");
    // // FileOutputStream fileOutStream = new FileOutputStream(fileOut);
    // // FileChannel fileChannelOut = fileOutStream.getChannel();
    // //
    // //
    // // ByteBuffer bb = ByteBuffer.allocateDirect(nCols * 4);
    // // bb.order(ByteOrder.LITTLE_ENDIAN);
    // //
    // // for(int r=0; r<nRows; r++){
    // // for(int c=0; c<nCols; c++){
    // // if(bb.hasRemaining()){
    // // bb.putFloat(raster.getSampleFloat(c, r, 0));
    // // }else{
    // // c--;
    // // bb.compact();
    // // fileChannelOut.write(bb);
    // // bb.clear();
    // // }
    // // }
    // // }
    // //
    // // bb.compact();
    // // fileChannelOut.write(bb);
    // // bb.clear();
    // //
    //
    //
    // //Set Envelope
    // double xmin = doubleBaseGrid.getLowerLeftCoord().x;
    // double ymin = doubleBaseGrid.getLowerLeftCoord().y;
    //
    // double xmax = xmin + (doubleBaseGrid.getCellSize() *
    // doubleBaseGrid.getColumnCount());
    // double ymax = ymin + (doubleBaseGrid.getCellSize() *
    // doubleBaseGrid.getRowCount());
    //
    // Envelope env = new Envelope(xmin, xmax, ymin, ymax);
    //
    // RasterImageLayer ril = new RasterImageLayer("Prova",
    // context.getLayerManager(), null, tiledImage.getData(), env );
    // ril.setImage(tiledImage);
    // // RasterImageLayerRendererFactory rirf = new
    // RasterImageLayerRendererFactory(context);
    // // rirf.create(ril, context.getLayerViewPanel(), 1);
    //
    // return ril;
    // }
    //

    // public static void displayRasterFromFile(PlugInContext context, File
    // rasterFile) throws IOException, NoninvertibleTransformException{
    //
    // RasterImageLayer ril = getRasterImageLayerFromFile(context, rasterFile);
    //
    // if (ril != null){
    // addImageToOJ(context.getWorkbenchContext(), ril);
    // }
    // }

    // public static RasterImageLayer getRasterImageLayer(WorkbenchContext
    // context,
    // DoubleBasicGrid doubleBaseGrid, String layerName) throws IOException {
    //
    // int nCols = doubleBaseGrid.getColumnCount();
    // int nRows = doubleBaseGrid.getRowCount();
    // double cellSize = doubleBaseGrid.getCellSize();
    // double noData = doubleBaseGrid.getNoData();
    //
    // SampleModel sampleModel =
    // RasterFactory.createBandedSampleModel(DataBuffer.TYPE_FLOAT, nCols,
    // nRows, 1);
    // DataBuffer db = new
    // DataBufferFloat(RasterUtils.getSingleFloatArray(doubleBaseGrid.getData()),
    // nCols*nRows);
    // java.awt.Point point = new java.awt.Point();
    // point.setLocation(0, 0);
    // WritableRaster writableRaster =
    // WritableRaster.createWritableRaster(sampleModel, db, point);
    //
    // //SampleModel sm = writableRaster.getSampleModel();
    // ColorModel colorModel = PlanarImage.createColorModel(sampleModel);
    // BufferedImage bufferImage = new BufferedImage(colorModel, writableRaster,
    // false, null);
    // // return image;
    // //
    // // //get PlanarImage
    // // double[] singlearray =
    // RasterUtils.getSingleArray(doubleBaseGrid.getData());
    // // int nCols = doubleBaseGrid.getColumnCount();
    // // int nRows = doubleBaseGrid.getRowCount();
    // // double cellSize = doubleBaseGrid.getCellSize();
    // // double noData = doubleBaseGrid.getNoData();
    // // DataBuffer db = new DataBufferDouble(singlearray, nCols * nRows);
    // // SampleModel sampleModel =
    // RasterFactory.createBandedSampleModel(DataBuffer.TYPE_DOUBLE, nCols,
    // nRows, 1);
    // //
    // // Point rasterOriginPoint = new Point(0, 0);
    // // Raster raster = RasterFactory.createRaster(sampleModel, db,
    // rasterOriginPoint);
    // //
    // // ColorModel colorModel = PlanarImage.createColorModel(sampleModel);
    // // WritableRaster writableRaster =
    // WritableRaster.createWritableRaster(sampleModel, db, rasterOriginPoint);
    // //
    // // BufferedImage bufferImage = new BufferedImage(colorModel,
    // writableRaster, false, null);
    // //
    // // TiledImage tiledImage = new TiledImage(0, 0, nCols, nRows, 0, 0,
    // sampleModel, colorModel); //DA MODIFICARE I PRIMI DUE VALORI
    // // tiledImage.setData(raster);
    //
    // //Set Envelope
    // double xmin = doubleBaseGrid.getLowerLeftCoord().x;
    // double ymin = doubleBaseGrid.getLowerLeftCoord().y;
    //
    // double xmax = xmin + (doubleBaseGrid.getCellSize() *
    // doubleBaseGrid.getColumnCount());
    // double ymax = ymin + (doubleBaseGrid.getCellSize() *
    // doubleBaseGrid.getRowCount());
    //
    // Envelope env = new Envelope(xmin, xmax, ymin, ymax);
    //
    // RasterImageLayer ril = new RasterImageLayer(layerName,
    // context.getLayerManager(), bufferImage, writableRaster, env);
    //
    // return ril;
    // }
    // //

    // /**
    // * Method to find the minimum value in an array double.
    // * @param dataArray
    // * @param noDataValue
    // * @return
    // */
    // public static double getMinRasterValue(double[][] dataArray, double
    // noDataValue) {
    //
    // double minValue = Double.MAX_VALUE;
    // for (int r = 0; r < dataArray.length; r++) {
    // for (int c = 0; c < dataArray[0].length; c++) {
    // double val = dataArray[r][c];
    //
    // if (val < minValue && val != noDataValue) {
    // minValue = val;
    // }
    // }
    // }
    //
    // return minValue;
    // }

    // // /**
    // // * Method to find the maximum value in an array.
    // // * @param dataArray
    // // * @param noDataValue
    // // * @return
    // // */
    // // public static double getMaRasterValue(double[] dataArray, double
    // noDataValue) {
    // //
    // // double minValue = Double.MAX_VALUE;
    // // for (int n = 0; n < dataArray.length; n++) {
    // //
    // // double val = dataArray[n];
    // //
    // // if (val < minValue && val != noDataValue) {
    // // minValue = val;
    // // }
    // // }
    // //
    // // return minValue;
    // // }

    // /**
    // *
    // * Method to find the maximum value in an array double.
    // * @param dataArray
    // * @param noDataValue
    // * @return
    // */
    //
    // public static double getMaxRasterValue(double[][] dataArray, double
    // noDataValue) {
    //
    // double maxValue = -Double.MAX_VALUE;
    // for (int r = 0; r < dataArray.length; r++) {
    // for (int c = 0; c < dataArray[0].length; c++) {
    // double val = dataArray[r][c];
    //
    // if (val != noDataValue && val > maxValue) {
    // maxValue = val;
    // }
    // }
    // }
    //
    // return maxValue;
    // }

    /**
     * Method to find the minumim value in an array double.
     * 
     * @param dataArray
     * @param noDataValue
     * @return
     */

    // public static double getMinRasterValue(double[] dataArray, double
    // noDataValue) {
    //
    // double maxValue = -Double.MAX_VALUE;
    // for (int n = 0; n < dataArray.length; n++) {
    //
    // double val = dataArray[n];
    //
    // if (val != noDataValue && val > maxValue) {
    // maxValue = val;
    // }
    // }
    //
    // return maxValue;
    // }

    // public static void displayRasterFileOnOJ(WorkbenchContext context,
    // DoubleBasicGrid doubleBaseGrid, String rasterLayerName) throws
    // NoninvertibleTransformException, IOException{
    //
    // // convert DoubleBasicGrid in RasterImageLayer
    // RasterImageLayer ril = RasterUtils.getRasterImageLayer(context,
    // doubleBaseGrid, rasterLayerName);
    // ril.setImageFileName(rasterLayerName);
    //
    // //add reclassified raster to OJ
    // RasterUtils.addImageToOJ(context, ril);
    // }

    // public static RasterImageLayer getRasterImageLayer(WorkbenchContext
    // context,
    // ByteBasicGrid byteBasicGrid, String layerName) throws IOException{
    //
    // DoubleBasicGrid doubleBasicGrid = new
    // DoubleBasicGrid(byteBasicGrid.getData(),
    // byteBasicGrid.getCellSize(),
    // byteBasicGrid.getNoData(),
    // byteBasicGrid.getLowerLeftCoord());
    //
    // return getRasterImageLayer(context, doubleBasicGrid, layerName);
    //
    // }

}
