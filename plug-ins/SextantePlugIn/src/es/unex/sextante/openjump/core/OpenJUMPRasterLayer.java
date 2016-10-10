package es.unex.sextante.openjump.core;

import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

import javax.media.jai.PlanarImage;
import javax.media.jai.RasterFactory;

import org.openjump.core.rasterimage.GeoTiffConstants;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.rasterimage.TiffTags;
import org.openjump.core.rasterimage.WorldFileHandler;

import com.sun.media.jai.codec.TIFFEncodeParam;
import com.sun.media.jai.codec.TIFFField;
import com.sun.media.jai.codecimpl.TIFFCodec;
import com.sun.media.jai.codecimpl.TIFFImageEncoder;
import com.vividsolutions.jts.geom.Envelope;

import es.unex.sextante.core.AnalysisExtent;
import es.unex.sextante.dataObjects.AbstractRasterLayer;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.outputs.FileOutputChannel;
import es.unex.sextante.outputs.IOutputChannel;

public class OpenJUMPRasterLayer extends AbstractRasterLayer {

    private static final double DEFAULT_NO_DATA_VALUE = -99999.;

    private String m_sFilename;
    private String m_sName = "";
    private AnalysisExtent m_LayerExtent;
    private double m_dNoDataValue;
    private Raster m_Raster;
    RasterImageLayer m_Layer;

    public void create(final RasterImageLayer layer) throws IOException {

        // [Giuseppe Aruta 8 Oct. 2016] add few modification to correct bug #410

        /*
         * [sstein 26 Oct. 2010] - don't use code below because the raster data
         * should be loaded new from file. It happened in tests that with the
         * code below data from another raster, created last, was used instead.
         * (e.g. calculated Point-KDE first, and then Line-KDE=> then getting
         * the polygon grid from the point-KDE raster delivered the Line-KDE
         * raster as poly grid)
         * 
         * m_BaseDataObject = layer; //[sstein 2 Aug 2010], changed so we work
         * now with the raster and not the image, which may be scaled for
         * display. //m_Raster = layer.getImage().getData(); m_Raster =
         * layer.getRasterData(); //-- end m_sName = layer.getName();
         * m_sFilename = layer.getImageFileName(); Envelope env =
         * layer.getEnvelope(); m_LayerExtent = new GridExtent();
         * m_LayerExtent.setXRange(env.getMinX(), env.getMaxX());
         * m_LayerExtent.setYRange(env.getMinY(), env.getMaxY());
         * m_LayerExtent.setCellSize((env.getMaxX() - env.getMinX()) /
         * (double)m_Raster.getWidth()); m_dNoDataValue = DEFAULT_NO_DATA_VALUE;
         */

        // [sstein 26 Oct. 2010] using the new method instead
        // so I do not need to change the code in all the cases
        // where #.create(layer) is used
        System.out.println("creation of raster layer");
        create(layer, true);

    }

    public void create(final RasterImageLayer layer, final boolean loadFromFile)
            throws IOException {

        if (loadFromFile == false) {
            m_Layer = layer;
            // [sstein 2 Aug 2010], changed so we work now with the raster and
            // not the image, which may be scaled for display.
            // m_Raster = layer.getImage().getData();
            m_Raster = layer.getRasterData(null);
            // -- end
            m_sName = layer.getName();
            m_sFilename = layer.getImageFileName();
            final Envelope env = layer.getWholeImageEnvelope();
            m_LayerExtent = new AnalysisExtent();
            // [sstein 18 Mar 2013], set cell size first, and then the extent,
            // otherwise maxX and maxY will be reset
            m_LayerExtent.setCellSize((env.getMaxX() - env.getMinX())
                    / m_Raster.getWidth());
            m_LayerExtent.setXRange(env.getMinX(), env.getMaxX(), true);
            m_LayerExtent.setYRange(env.getMinY(), env.getMaxY(), true);
            // [Giuseppe Aruta 8 Oct. 2016] using selected rasterlayer no data
            // value instead
            m_dNoDataValue = layer.getNoDataValue();
            // m_dNoDataValue = DEFAULT_NO_DATA_VALUE;
        } else {
            final RasterImageLayer rasterLayer = new RasterImageLayer(
                    layer.getName(), layer.getLayerManager(),
                    layer.getImageFileName(), null,
                    layer.getWholeImageEnvelope());
            m_Layer = rasterLayer;
            m_Raster = rasterLayer.getRasterData(null);
            // -- end
            m_sName = rasterLayer.getName();
            m_sFilename = rasterLayer.getImageFileName();
            final Envelope env = rasterLayer.getWholeImageEnvelope();
            m_LayerExtent = new AnalysisExtent();
            // [sstein 18 Mar 2013], set cell size first, and then the extent,
            // otherwise maxX and maxY will be reset
            m_LayerExtent.setCellSize((env.getMaxX() - env.getMinX())
                    / m_Raster.getWidth());
            m_LayerExtent.setXRange(env.getMinX(), env.getMaxX(), true);
            m_LayerExtent.setYRange(env.getMinY(), env.getMaxY(), true);
            // [Giuseppe Aruta 8 Oct. 2016] using selected rasterlayer no data
            // value instead
            m_dNoDataValue = layer.getNoDataValue();
            // m_dNoDataValue = DEFAULT_NO_DATA_VALUE;
        }

    }

    public void create(final String name, final String filename,
            final AnalysisExtent ge, final int dataType, final int numBands,
            final Object crs) {

        m_Raster = RasterFactory.createBandedRaster(dataType, ge.getNX(),
                ge.getNY(), numBands, null);

        final OpenJUMPOutputFactory fact = (OpenJUMPOutputFactory) SextanteGUI
                .getOutputFactory();

        final Envelope envelope = new Envelope();
        envelope.init(ge.getXMin(), ge.getXMax(), ge.getYMin(), ge.getYMax());
        final ColorModel colorModel = PlanarImage.createColorModel(m_Raster
                .getSampleModel());
        final BufferedImage bufimg = new BufferedImage(colorModel,
                (WritableRaster) m_Raster, false, null);
        // final PlanarImage pimage = PlanarImage.wrapRenderedImage(bufimg);
        m_Layer = new RasterImageLayer(name, fact.getContext()
                .getLayerManager(), filename, bufimg, envelope);
        m_sName = name;
        m_sFilename = filename;
        m_LayerExtent = ge;
        // [Giuseppe Aruta 8 Oct. 2016] using Sextante GUI to get no data value
        // instead
        m_dNoDataValue = SextanteGUI.getOutputFactory().getDefaultNoDataValue();
        // m_dNoDataValue = DEFAULT_NO_DATA_VALUE;

    }

    public int getBandsCount() {

        if (m_Raster != null) {
            return m_Raster.getNumBands();
        } else {
            return 0;
        }

    }

    public double getCellValueInLayerCoords(final int x, final int y,
            final int band) {

        try {
            if (m_Raster != null) {
                return m_Raster.getSampleDouble(x, y, band);
            } else {
                return getNoDataValue();
            }
        } catch (final Exception e) {
            return getNoDataValue();
        }

    }

    public int getDataType() {

        if (m_Raster != null) {
            return m_Raster.getDataBuffer().getDataType();
        } else {
            return DataBuffer.TYPE_DOUBLE;
        }

    }

    public double getLayerCellSize() {

        if (m_LayerExtent != null) {
            return m_LayerExtent.getCellSize();
        } else {
            return 0;
        }

    }

    public AnalysisExtent getLayerGridExtent() {

        return m_LayerExtent;

    }

    public double getNoDataValue() {

        return m_dNoDataValue;

    }

    public void setCellValue(final int x, final int y, final int band,
            final double value) {

        if (m_Raster instanceof WritableRaster) {
            if (this.getWindowGridExtent().containsCell(x, y)) {
                ((WritableRaster) m_Raster).setSample(x, y, band, value);
            }
        }

    }

    public void setNoDataValue(final double noDataValue) {

        m_dNoDataValue = noDataValue;

    }

    public Object getCRS() {

        return null;

    }

    /**
     * Returns the extent covered by the layer
     * 
     * @return the extent of the layer
     */
    public Rectangle2D getFullExtent() {

        if (m_Layer != null) {
            final Envelope envelope = m_Layer.getWholeImageEnvelope();
            return new Rectangle2D.Double(envelope.getMinX(),
                    envelope.getMinY(), envelope.getWidth(),
                    envelope.getHeight());
        } else {
            return null;
        }

    }

    public void open() {
    }

    public void close() {
    }

    public void postProcess() throws Exception {

        if (m_Layer != null) {

            final FileOutputStream tifOut = new FileOutputStream(m_sFilename);
            final TIFFEncodeParam param = new TIFFEncodeParam();
            param.setCompression(TIFFEncodeParam.COMPRESSION_NONE);
            TIFFField[] tiffFields = new TIFFField[2];

            // [Giuseppe Aruta 8 Oct. 2016] the following parameters come from
            // RasterImageIO class
            // and add cell size/no data value and Tie point to the new created
            // file
            // Cell size
            tiffFields[0] = new TIFFField(GeoTiffConstants.ModelPixelScaleTag,
                    TIFFField.TIFF_DOUBLE, 2, getLayerCellSize());
            // No data
            String noDataS = Double.toString(getNoDataValue());
            byte[] bytes = noDataS.getBytes();
            tiffFields[0] = new TIFFField(TiffTags.TIFFTAG_GDAL_NODATA,
                    TIFFField.TIFF_BYTE, noDataS.length(), bytes);
            // Tie point
            final Envelope envelope = m_Layer.getWholeImageEnvelope();
            tiffFields[1] = new TIFFField(GeoTiffConstants.ModelTiepointTag,
                    TIFFField.TIFF_DOUBLE, 6, new double[] { 0, 0, 0,
                            envelope.getMinX(), envelope.getMaxY(), 0 });
            param.setExtraFields(tiffFields);

            final TIFFImageEncoder encoder = (TIFFImageEncoder) TIFFCodec
                    .createImageEncoder("tiff", tifOut, param);
            // -- [sstein 2 Aug 2010]
            // BufferedImage image = layer.getImage().getAsBufferedImage();
            final ColorModel colorModel = PlanarImage.createColorModel(m_Raster
                    .getSampleModel());
            final BufferedImage image = new BufferedImage(colorModel,
                    (WritableRaster) m_Raster, false, null);
            // -- end
            encoder.encode(image);
            tifOut.close();

            /* save geodata: */
            // final Envelope envelope = m_Layer.getWholeImageEnvelope();

            final WorldFileHandler worldFileHandler = new WorldFileHandler(
                    m_sFilename, false);
            worldFileHandler.writeWorldFile(envelope, image.getWidth(),
                    image.getHeight());

            // Switch RAM mode of the RasterImage
            m_Layer.setImageFileName(m_sFilename);
            m_Layer.setNeedToKeepImage(false);

        }

    }

    public boolean export(final String sFilename) {

        if (sFilename.endsWith("asc")) {
            return exportToArcInfoASCIIFile(sFilename);
        } else if (sFilename.endsWith("tif")) {
            return exportToGeoTIFFFile(sFilename);
        } else {
            return exportToGeoTIFFFile(sFilename);
        }

    }

    /*
     * public void postProces() throws Exception {
     * 
     * if (m_Layer != null) {
     * 
     * final FileOutputStream tifOut = new FileOutputStream(m_sFilename); final
     * TIFFEncodeParam param = new TIFFEncodeParam();
     * param.setCompression(TIFFEncodeParam.COMPRESSION_NONE); TIFFField[]
     * tiffFields = new TIFFField[2];
     * 
     * // Cell size
     * 
     * 
     * 
     * // No data String noDataS = Double.toString(getNoDataValue()); byte[]
     * bytes = noDataS.getBytes(); tiffFields[0] = new
     * TIFFField(TiffTags.TIFFTAG_GDAL_NODATA, TIFFField.TIFF_BYTE,
     * noDataS.length(), bytes);
     * 
     * // Tie point final Envelope envelope = m_Layer.getWholeImageEnvelope();
     * tiffFields[1] = new TIFFField(GeoTiffConstants.ModelTiepointTag,
     * TIFFField.TIFF_DOUBLE, 6, new double[] { 0, 0, 0, envelope.getMinX(),
     * envelope.getMaxY(), 0 }); param.setExtraFields(tiffFields); final
     * TIFFImageEncoder encoder = (TIFFImageEncoder) TIFFCodec
     * .createImageEncoder("tiff", tifOut, param); // -- [sstein 2 Aug 2010] //
     * BufferedImage image = layer.getImage().getAsBufferedImage(); final
     * ColorModel colorModel = PlanarImage.createColorModel(m_Raster
     * .getSampleModel()); final BufferedImage image = new
     * BufferedImage(colorModel, (WritableRaster) m_Raster, false, null); // --
     * end encoder.encode(image); tifOut.close();
     * 
     * 
     * 
     * final WorldFileHandler worldFileHandler = new WorldFileHandler(
     * m_sFilename, false); worldFileHandler.writeWorldFile(envelope,
     * image.getWidth(), image.getHeight());
     * 
     * // Switch RAM mode of the RasterImage
     * m_Layer.setImageFileName(m_sFilename); m_Layer.setNeedToKeepImage(false);
     * 
     * }
     * 
     * }
     */

    private boolean exportToGeoTIFFFile(final String sFilename) {
        try {
            final FileOutputStream tifOut = new FileOutputStream(m_sFilename);
            final TIFFEncodeParam param = new TIFFEncodeParam();
            param.setCompression(TIFFEncodeParam.COMPRESSION_NONE);
            TIFFField[] tiffFields = new TIFFField[3];

            // [Giuseppe Aruta 8 Oct. 2016] the following parameters come from
            // RasterImageIO class
            // and add cell size, nodata value and Tie point to the new created
            // file.

            // Cell size
            tiffFields[0] = new TIFFField(GeoTiffConstants.ModelPixelScaleTag,
                    TIFFField.TIFF_DOUBLE, 2, new double[] {
                            getLayerCellSize(), getLayerCellSize() });
            // No data
            String noDataS = Double.toString(getNoDataValue());
            byte[] bytes = noDataS.getBytes();
            tiffFields[1] = new TIFFField(TiffTags.TIFFTAG_GDAL_NODATA,
                    TIFFField.TIFF_BYTE, noDataS.length(), bytes);
            // Tie point
            final Envelope envelope = m_Layer.getWholeImageEnvelope();
            tiffFields[2] = new TIFFField(GeoTiffConstants.ModelTiepointTag,
                    TIFFField.TIFF_DOUBLE, 6, new double[] { 0, 0, 0,
                            envelope.getMinX(), envelope.getMaxY(), 0 });
            param.setExtraFields(tiffFields);
            final TIFFImageEncoder encoder = (TIFFImageEncoder) TIFFCodec
                    .createImageEncoder("tiff", tifOut, param);
            // -- [sstein 2 Aug 2010]
            // BufferedImage image = layer.getImage().getAsBufferedImage();
            final ColorModel colorModel = PlanarImage.createColorModel(m_Raster
                    .getSampleModel());
            final BufferedImage image = new BufferedImage(colorModel,
                    (WritableRaster) m_Raster, false, null);
            // -- end
            encoder.encode(image);
            tifOut.close();

            /* save geodata: */
            // final Envelope envelope = m_Layer.getWholeImageEnvelope();

            final WorldFileHandler worldFileHandler = new WorldFileHandler(
                    m_sFilename, false);
            worldFileHandler.writeWorldFile(envelope, image.getWidth(),
                    image.getHeight());

            // Switch RAM mode of the RasterImage
            m_Layer.setImageFileName(m_sFilename);
            m_Layer.setNeedToKeepImage(false);

        } catch (final Exception e) {
            return false;
        }

        return true;
    }

    private boolean exportToArcInfoASCIIFile(final String sFilename) {

        try {
            final FileWriter f = new FileWriter(sFilename);
            final BufferedWriter fout = new BufferedWriter(f);
            final DecimalFormat df = new DecimalFormat("##.###");
            df.setDecimalFormatSymbols(new DecimalFormatSymbols(Locale.US));
            df.setDecimalSeparatorAlwaysShown(true);

            fout.write("ncols " + Integer.toString(m_LayerExtent.getNX()));
            fout.newLine();
            fout.write("nrows " + Integer.toString(m_LayerExtent.getNY()));
            fout.newLine();
            fout.write("xllcorner " + Double.toString(m_LayerExtent.getXMin()));
            fout.newLine();
            fout.write("yllcorner " + Double.toString(m_LayerExtent.getYMin()));
            fout.newLine();
            // ArcInfo ASCII does not support individual x/y cell sizes. But who
            // cares?
            fout.write("cellsize "
                    + Double.toString(m_LayerExtent.getCellSize()));
            fout.newLine();
            fout.write("nodata_value " + Double.toString(getNoDataValue()));
            fout.newLine();

            for (int i = 0; i < m_LayerExtent.getNY(); i++) {
                for (int j = 0; j < m_LayerExtent.getNX(); j++) {
                    fout.write(df.format(getCellValueAsDouble(j, i)) + " ");
                }
                fout.newLine();
            }
            fout.close();
            f.close();
        } catch (final Exception e) {
            return false;
        }

        return true;

    }

    public String getName() {

        return m_sName;

    }

    public void setName(final String sName) {

        m_sName = sName;

        if (m_Layer != null) {
            m_Layer.setName(sName);
        }

    }

    public void free() {
        // TODO Auto-generated method stub

    }

    public Object getBaseDataObject() {

        return m_Layer;

    }

    public IOutputChannel getOutputChannel() {

        return new FileOutputChannel(m_sFilename);

    }

    // [sstein 26. Oct. 2012] added method back-in
    public String getFilename() {
        return m_sFilename;
    }

}
