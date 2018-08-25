package es.unex.sextante.openjump.core;

import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.ByteOrder;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

import javax.media.jai.PlanarImage;
import javax.media.jai.RasterFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.openjump.core.rasterimage.GeoTiffConstants;
import org.openjump.core.rasterimage.GridAscii;
import org.openjump.core.rasterimage.GridFloat;
import org.openjump.core.rasterimage.RasterImageIO;
import org.openjump.core.rasterimage.RasterImageIO.CellSizeXY;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.rasterimage.Stats;
import org.openjump.core.rasterimage.TiffTags;
import org.openjump.core.rasterimage.WorldFileHandler;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

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
    private String m_sFilename;
    private String m_sName = "";
    private AnalysisExtent m_LayerExtent;
    private double m_dNoDataValue;
    private Raster m_Raster;
    RasterImageLayer m_Layer;

    public void create(RasterImageLayer layer) throws IOException {
        m_Layer = layer;
        m_Raster = layer.getRasterData(null);
        m_sName = layer.getName();
        m_sFilename = layer.getImageFileName();
        final Envelope env = layer.getWholeImageEnvelope();
        m_LayerExtent = new AnalysisExtent();
        m_LayerExtent.setCellSize((env.getMaxX() - env.getMinX())
                / m_Raster.getWidth());
        m_LayerExtent.setXRange(env.getMinX(), env.getMaxX(), true);
        m_LayerExtent.setYRange(env.getMinY(), env.getMaxY(), true);
        m_dNoDataValue = layer.getNoDataValue();

        
        
        // [Giuseppe Aruta 25 Aug. 2018] Moved raster file export to OpenJUMP
        //inner methods (RasterImageIO class) so any enhencement in OJ raster
        //output will affect Sextante raster output
        // ------------------------------------------
        // [Giuseppe Aruta 30 Gen. 2018] deactivated as OJ calculate anyhow
        // statistics (and writes .xml file) when loads raster
        // m_Stats = stats(layer);
        // ------------------------------------------
        // [Giuseppe Aruta 30 Gen. 2018] - Uncomment [8 Oct. 2016] and reused
        // the previous version (Sextante
        // 0.6) as the previous method duplicates layer name ("rastername" ->
        // "rastername (2)").
        // this behaviour used to create confusion on some Sextante Algorithm,
        // like Calculus>Raster Calculator
        // ------------------------------------------
        // [sstein 26 Oct. 2010] using the new method instead
        // so I do not need to change the code in all the cases
        // where #.create(layer) is used

        // System.out.println("creation of raster layer");
        // create(layer, true);
    }

    public void create(RasterImageLayer layer, boolean loadFromFile)
            throws IOException {
        if (!loadFromFile) {
            m_Layer = layer;

            // [sstein 2 Aug 2010], changed so we work now with the raster and
            // not the image, which may be scaled for display.
            // m_Raster = layer.getImage().getData();
            m_Raster = layer.getRasterData(null);

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
            // m_dNoDataValue = DEFAULT_NO_DATA_VALUE;
            m_dNoDataValue = layer.getNoDataValue();
        } else {
            final RasterImageLayer rasterLayer = new RasterImageLayer(
                    layer.getName(), layer.getLayerManager(),
                    layer.getImageFileName(), null,
                    layer.getWholeImageEnvelope());
            m_Layer = rasterLayer;
            m_Raster = rasterLayer.getRasterData(null);

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

    public void create(String name, String filename, AnalysisExtent ge,
            int dataType, int numBands, Object crs) {
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

    @Override
    public int getBandsCount() {
        if (m_Raster != null) {
            return m_Raster.getNumBands();
        }
        return 0;
    }

    // @Override
    // public double getCellValueInLayerCoords(int x, int y, int band) {
    // try {
    // if (m_Raster != null) {
    // return m_Raster.getSampleDouble(x, y, band);
    // }
    // return getNoDataValue();
    // } catch (final Exception e) {
    // }
    // return getNoDataValue();
    // }

    @Override
    public double getCellValueInLayerCoords(int x, int y, int band) {
        final DataBuffer db = m_Raster.getDataBuffer();
        try {
            switch (db.getDataType()) {
            case 5:
                return m_Raster.getSampleDouble(x, y, band);
            case 4:
                return m_Raster.getSampleFloat(x, y, band);
            case 3:
                return m_Raster.getSample(x, y, band);
            case 1:
            case 2:
                return (short) m_Raster.getSampleDouble(x, y, band);
            case 0:
                return (byte) m_Raster.getSampleDouble(x, y, band) & 0xFF;
            }
            return m_Layer.getNoDataValue();
        } catch (final Exception e) {
            throw new RuntimeException(
                    "Interrupted while getting value of cell x = " + x
                            + ", y = " + y + ", band = " + band, e);
        }
    }

    @Override
    public int getDataType() {
        if (m_Raster != null) {
            return m_Raster.getDataBuffer().getDataType();
        }
        return 5;
    }

    @Override
    public double getLayerCellSize() {
        if (m_LayerExtent != null) {
            return m_LayerExtent.getCellSize();
        }
        return 0.0D;
    }

    @Override
    public AnalysisExtent getLayerGridExtent() {
        return m_LayerExtent;
    }

    @Override
    public double getNoDataValue() {
        return m_dNoDataValue;
    }

    // public void setCellValue(int x, int y, int band, double value) {
    // if (((this.m_Raster instanceof WritableRaster))
    // && (getWindowGridExtent().containsCell(x, y))) {
    // ((WritableRaster) this.m_Raster).setSample(x, y, band, value);
    // }
    // }

    @Override
    public void setCellValue(int x, int y, int band, double dValue) {
        if (((m_Raster instanceof WritableRaster))
                && (getWindowGridExtent().containsCell(x, y))) {
            final DataBuffer db = m_Raster.getDataBuffer();

            try {
                switch (db.getDataType()) {
                case 5:
                    ((WritableRaster) m_Raster).setSample(x, y, band, dValue);
                    break;
                case 4:
                    ((WritableRaster) m_Raster).setSample(x, y, band,
                            (float) dValue);
                    break;
                case 3:
                    ((WritableRaster) m_Raster).setSample(x, y, band,
                            (int) dValue);
                    break;
                case 1:
                case 2:
                    ((WritableRaster) m_Raster).setSample(x, y, band,
                            (short) dValue);
                    break;
                case 0:
                    ((WritableRaster) m_Raster).setSample(x, y, band,
                            (byte) dValue);
                }
            } catch (final Exception e) {
                throw new RuntimeException(
                        "Interrupted while setting value of cell x = " + x
                                + ", y = " + y + ", band = " + band
                                + ", value = " + dValue, e);

            }
        }
    }

    @Override
    public void setNoDataValue(double noDataValue) {
        m_dNoDataValue = noDataValue;
    }

    @Override
    public Object getCRS() {
        return null;
    }

    /**
     * Returns the extent covered by the layer
     * 
     * @return the extent of the layer
     */
    @Override
    public Rectangle2D getFullExtent() {
        if (m_Layer != null) {
            final Envelope envelope = m_Layer.getWholeImageEnvelope();
            return new Rectangle2D.Double(envelope.getMinX(),
                    envelope.getMinY(), envelope.getWidth(),
                    envelope.getHeight());
        }
        return null;
    }

    @Override
    public void open() {
    }

    @Override
    public void close() {
    }

    public void postProcess_old() throws Exception {

        if (m_Layer != null) {

            final FileOutputStream tifOut = new FileOutputStream(m_sFilename);
            final TIFFEncodeParam param = new TIFFEncodeParam();
            param.setCompression(TIFFEncodeParam.COMPRESSION_NONE);
            final TIFFField[] tiffFields = new TIFFField[2];

            // [Giuseppe Aruta 8 Oct. 2016] the following parameters come from
            // RasterImageIO class
            // and add cell size/no data value and Tie point to the new created
            // file
            // Cell size
            tiffFields[0] = new TIFFField(GeoTiffConstants.ModelPixelScaleTag,
                    TIFFField.TIFF_DOUBLE, 2, getLayerCellSize());
            // No data
            final String noDataS = Double.toString(getNoDataValue());
            final byte[] bytes = noDataS.getBytes();
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
            // [Giuseppe Aruta 30 Gen. 2018] deactivated as OJ calculate anyhow
            // statistics (and writes .xml file) when loads raster
            // String outXML = m_sFilename + ".aux.xml";
            // writeXLM(new File(outXML));
            // Switch RAM mode of the RasterImage
            m_Layer.setImageFileName(m_sFilename);
            m_Layer.setNeedToKeepImage(false);

        }

    }

    @Override
    public void postProcess() throws Exception {

        if (m_Layer != null) {

            exportToTIFF();

            // Switch RAM mode of the RasterImage
            m_Layer.setImageFileName(m_sFilename);
            m_Layer.setNeedToKeepImage(false);

        }

    }

    private void exportToTIFF() throws Exception {
        if (m_Layer != null) {

            final RasterImageIO rasterImageIO = new RasterImageIO();
            final Envelope env = m_Layer.getWholeImageEnvelope();

            final File file = new File(m_sFilename);
            rasterImageIO.writeImage(
                    file,
                    m_Raster,
                    env,
                    rasterImageIO.new CellSizeXY(env.getWidth()
                            / m_Raster.getWidth(), env.getHeight()
                            / m_Raster.getHeight()), m_Layer.getNoDataValue());

            // Switch RAM mode of the RasterImage
            m_Layer.setImageFileName(m_sFilename);
            m_Layer.setNeedToKeepImage(false);

        }

    }

    private void exportToASC() throws Exception {
        if (m_Layer != null) {
            final RasterImageIO rasterImageIO = new RasterImageIO();
            final Envelope env = m_Layer.getWholeImageEnvelope();
            final CellSizeXY cellsize = rasterImageIO.new CellSizeXY(
                    env.getWidth() / m_Raster.getWidth(), env.getHeight()
                            / m_Raster.getHeight());

            final GridAscii ga = new GridAscii(m_sFilename,
                    m_Raster.getWidth(), m_Raster.getHeight(), true,
                    env.getMinX(), env.getMinY(),
                    cellsize.getAverageCellSize(), m_Layer.getNoDataValue());
            ga.setRas(m_Raster);
            ga.writeGrid();
        }

    }

    private void exportToFLT() throws Exception {
        if (m_Layer != null) {
            final RasterImageIO rasterImageIO = new RasterImageIO();
            final Envelope env = m_Layer.getWholeImageEnvelope();
            final CellSizeXY cellsize = rasterImageIO.new CellSizeXY(
                    env.getWidth() / m_Raster.getWidth(), env.getHeight()
                            / m_Raster.getHeight());

            final GridFloat gf = new GridFloat(m_sFilename,
                    m_Raster.getWidth(), m_Raster.getHeight(), true,
                    env.getMinX(), env.getMinY(),
                    cellsize.getAverageCellSize(), m_Layer.getNoDataValue(),
                    ByteOrder.LITTLE_ENDIAN);
            gf.setRas(m_Raster);
            gf.writeGrid();
        }

    }

    private boolean exportToArcInfoASCIIFile(String sFilename) {
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

    @Override
    public String getName() {
        return m_sName;
    }

    @Override
    public void setName(String sName) {
        m_sName = sName;
        if (m_Layer != null) {
            m_Layer.setName(sName);
        }
    }

    @Override
    public void free() {
    }

    @Override
    public Object getBaseDataObject() {
        return m_Layer;
    }

    @Override
    public IOutputChannel getOutputChannel() {
        return new FileOutputChannel(m_sFilename);
    }

    public String getFilename() {
        return m_sFilename;
    }

    // [Giuseppe Aruta 30 Gen. 2018] The following code is used to a) calculate
    // statistics of the layer
    // b) resume srs from input raster c) write sidecar .xml file with statistic
    // e srs.
    // I deactivated for now as OJ will rewrite .xml file anyhow when loads
    // output file and srs writing need more
    // test.
    private Stats m_Stats;

    public static Stats stats(RasterImageLayer layer) {
        return layer.getMetadata().getStats();
    }

    public void writeXLM(File auxXmlFile) throws Exception {
        final Stats stats = m_Stats;
        final DocumentBuilderFactory docFactory = DocumentBuilderFactory
                .newInstance();
        final DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
        Document doc;

        Element pamDatasetElement;
        NodeList pamRasterBandNodeList;
        doc = docBuilder.newDocument();

        // Check if PAMDataset element exists and, if not, create it
        final String pamDatasetTagName = "PAMDataset";
        pamDatasetElement = (Element) doc.getElementsByTagName(
                pamDatasetTagName).item(0);
        if (pamDatasetElement == null) {
            pamDatasetElement = doc.createElement(pamDatasetTagName);
        }

        final String pamRasterBandTagName = "PAMRasterBand";
        final String pamRasterSridTagName = "SRS";
        final String bandAttribute = "band";
        final String metadataElementName = "Metadata";

        final String SRID = null;
        // String fileSourcePath = m_Layer.getImageFileName();
        // //// String srsCode = m_Layer.getSRSInfo().getCode();

        // SRSInfo srsInfo = SridLookupTable.getSrsAndUnitFromCode(srsCode);

        /*
         * String extension = FileUtil.getExtension(m_sFilename).toLowerCase();
         * 
         * if (extension.equals("tif") || extension.equals("tiff")) {
         * TiffTags.TiffMetadata metadata = TiffTags.readMetadata(new File(
         * fileSourcePath)); if (metadata.isGeoTiff()) {
         * 
         * srsInfo = metadata.getSRSInfo(); } else { srsInfo =
         * ProjUtils.getSRSInfoFromAuxiliaryFile(fileSourcePath);
         * 
         * } } else { srsInfo =
         * ProjUtils.getSRSInfoFromAuxiliaryFile(fileSourcePath);
         * 
         * }
         */
        // m_Srid = srsCode;

        // //// if (!srsCode.equals("0")) {
        // //// SRID = SridLookupTable.getOGCWKTFromWkidCode(srsCode);
        // //// if (!SRID.isEmpty()) {
        // ////
        // //// Element SRS = doc.createElement(pamRasterSridTagName);
        // //// SRS.appendChild(doc.createTextNode(SRID));
        // //// pamDatasetElement.appendChild(doc);
        // //// }
        // //// }

        pamRasterBandNodeList = pamDatasetElement
                .getElementsByTagName(pamRasterBandTagName);
        if (pamRasterBandNodeList != null
                && pamRasterBandNodeList.getLength() > 0) {
            for (int b = 0; b < pamRasterBandNodeList.getLength(); b++) {
                final Element pamRasterBandElement = (Element) pamRasterBandNodeList
                        .item(b);
                final int bandNr = Integer.parseInt(pamRasterBandElement
                        .getAttribute(bandAttribute));

                if (bandNr == b + 1) {

                    Element metadataElement = (Element) pamRasterBandElement
                            .getElementsByTagName(metadataElementName).item(0);
                    metadataElement = updateMetadataElement(doc,
                            metadataElement, m_Layer, bandNr);

                    pamRasterBandElement.appendChild(metadataElement);
                    pamDatasetElement.appendChild(pamRasterBandElement);

                }
            }
        } else {
            for (int b = 0; b < stats.getBandCount(); b++) {

                final Element pamRasterBandElement = doc
                        .createElement(pamRasterBandTagName);
                final Attr attr = doc.createAttribute(bandAttribute);
                attr.setValue(Integer.toString(b + 1));
                pamRasterBandElement.setAttributeNode(attr);

                Element metadataElement = doc
                        .createElement(metadataElementName);
                metadataElement = updateMetadataElement(doc, metadataElement,
                        m_Layer, b + 1);
                pamRasterBandElement.appendChild(metadataElement);
                pamDatasetElement.appendChild(pamRasterBandElement);
            }

            doc.appendChild(pamDatasetElement);
        }

        // write the content into xml file
        final TransformerFactory transformerFactory = TransformerFactory
                .newInstance();
        final Transformer transformer = transformerFactory.newTransformer();
        transformer.setOutputProperty(OutputKeys.INDENT, "yes");
        transformer.setOutputProperty(
                "{http://xml.apache.org/xslt}indent-amount", "2");
        final DOMSource source = new DOMSource(doc);
        final StreamResult result = new StreamResult(auxXmlFile);
        transformer.transform(source, result);

    }

    private Element updateMetadataElement(Document doc,
            Element metadataElement, RasterImageLayer layer, int band) {
        final Stats stats = m_Stats;
        ;
        Element mdi = doc.createElement("MDI");
        mdi.setAttribute("key", "STATISTICS_MINIMUM");
        mdi.setTextContent(Double.toString(stats.getMin(band)));
        metadataElement.appendChild(mdi);

        mdi = doc.createElement("MDI");
        mdi.setAttribute("key", "STATISTICS_MAXIMUM");
        mdi.setTextContent(Double.toString(stats.getMax(band)));
        metadataElement.appendChild(mdi);

        mdi = doc.createElement("MDI");
        mdi.setAttribute("key", "STATISTICS_MEAN");
        mdi.setTextContent(Double.toString(stats.getMean(band)));
        metadataElement.appendChild(mdi);

        mdi = doc.createElement("MDI");
        mdi.setAttribute("key", "STATISTICS_STDDEV");
        mdi.setTextContent(Double.toString(stats.getStdDev(band)));
        metadataElement.appendChild(mdi);

        return metadataElement;

    }

}
