package es.unex.sextante.rasterize.rasterizeTracks;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;

import com.amd.aparapi.Range;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

import es.unex.sextante.additionalInfo.AdditionalInfoNumericalValue;
import es.unex.sextante.additionalInfo.AdditionalInfoVectorLayer;
import es.unex.sextante.core.AnalysisExtent;
import es.unex.sextante.core.GeoAlgorithm;
import es.unex.sextante.core.ITaskMonitor;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.dataObjects.IFeature;
import es.unex.sextante.dataObjects.IFeatureIterator;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.dataObjects.IVectorLayer;
import es.unex.sextante.exceptions.GeoAlgorithmExecutionException;
import es.unex.sextante.exceptions.IteratorException;
import es.unex.sextante.rasterWrappers.GridCell;
import es.unex.sextante.rasterize.rasterizeTracks.TrackPartIterable.TrackPart;
import es.unex.sextante.rasterize.rasterizeTracks.TrackPartIterable.TrackPartIterator;

public class RasterizeTracksAlgorithm extends GeoAlgorithm {
    /**
     * The double Value which represents NO_DATA
     */
    private double NO_DATA;

    /**
     * The Key for the Dictionary to figure out the translation of Layer used in
     * the GUI
     */
    public static final String LAYER = "LAYER";

    /**
     * The Key for the Dictionary to figure out the translation of Field used in
     * the GUI
     */
    public static final String FIELD = "FIELD";

    /**
     * The Key for the Dictionary to figure out the translation of TrackID used
     * in the GUI
     */
    public static final String TRACKID = "TRACKID";

    /**
     * The Key for the Dictionary to figure out the translation of SensorID used
     * in the GUI
     */
    public static final String SENSORID = "SENSORID";

    /**
     * The Key for the Dictionary to figure out the translation of timestamp
     * used in the GUI
     */
    public static final String TIMESTAMP = "TIME_STAMP";

    /**
     * The Key for the Dictionary to figure out the translation of lowcut used
     * in the GUI
     */
    public static final String LOW_CUT = "LOW_CUT";

    /**
     * The Key for the Dictionary to figure out the translation of highcut used
     * in the GUI
     */
    public static final String HIGH_CUT = "HIGH_CUT";

    /**
     * The Key for the Dictionary to figure out the translation of threadcount
     * used in the GUI
     */
    public static final String THREADS = "THREADS";

    /**
     * The Key for the Dictionary to figure out the translation of FILL_GAP used
     * in the GUI
     */
    public static final String FILL_GAP = "FILL_GAP";
    /**
     * The Key for the Dictionary to figure out the translation of Usage of
     * SensorIDs used in the GUI
     */
    public static final String USE_SENSORIDS = "USE_SENSORIDS";

    /**
     * The Key for the Dictionary to figure out the translation of result used
     * in the GUI
     */
    public static final String RESULT = "RESULT";

    /**
     * The Key for the Dictionary to figure out the translation of
     * WEIGHTING_METHOD used in the GUI
     */
    private static final String WEIGHTING_METHOD = "WEIGHTING_METHOD";

    private static final double BARYCENTRIC_CUTOFF = 0;

    private static final String SUPERSAMLING_WINDOW_SIZE = "SUPERSAMLING_WINDOW_SIZE";

    private static final String RASTER_LOWPASS_FILTER = "RASTER_LOWPASS_FILTER";

    private static final String ROWS_AFTER = "ROWS_AFTER";

    private static final String LOWER_CPU_USAGE = "LOWER_CPU_USAGE";

    private static final String RESULT_RMF = "RESULT_RMF";

    private static final String IGNORE_TIMESTAMPS = "IGNORE_TIMESTAMPS";

    private static final String PROTOTYPE = "PROTOTYPE";

    private static final String CREATE_RMF = "CREATE_RMF";

    /**
     * The given IVectorLayer which should be rasterized
     */
    private IVectorLayer m_Layer;

    /**
     * The columnnumber of the intensity of an entry in the record of the given
     * IFeatures
     */
    private int m_iField;

    /**
     * The columnnumber of the trackid of an entry in the record of the given
     * IFeatures
     */
    private int m_iTrackID;

    /**
     * The columnnumber of the sensorid of an entry in the record of the given
     * IFeatures
     */
    private int m_iSensorID;

    /**
     * The columnnumber of the timestamp of an entry in the record of the given
     * IFeatures
     */
    private int m_iTimestamp;

    /**
     * The count of threads for rasterization
     */
    private int m_iThreads = 8;

    /**
     * The count of pixels for filling the gaps after rasterization
     */
    private int m_iFillGap = 0;

    /**
     * The lowcut, all values below will be set to the value of lowcut itself
     */
    private double m_dLowCut;

    /**
     * The highcut, all values above will be set to the value of highcut itself
     */
    private double m_dHighCut;

    /**
     * Says if there is a field for sensorIDs or not
     */
    private boolean m_bUseSensorIDs;

    /**
     * In the case where the rows 1 and 3 have sensors which are missing in row
     * 2, m_bUseRowInBetweenInterpolation says if those should be interpolated
     * out of the rows 1 and 3
     */
    private boolean m_bUseRowInBetweenInterpolation;

    /**
     * allows/disallows to do a preprocessing, which interpolates data between
     * multiple measurements of the same sensor in the same track
     */
    private boolean m_bUsePreprocessing;

    /**
     * Says which interpolation Method should be used (simple=0, supersampled=1,
     * rotated inputs + scaled)
     */
    private int m_iFilledInterpolationMethod;

    /**
     * Says how many Rows after the current Row should be accessible by the
     * trackpartiterator
     */
    private int m_iRowsAfter;
    /**
     * The Rasterlayer where the results should b stored to For Homography it
     * should be 2
     */

    private IRasterLayer m_Result;

    /**
     * The extent for rasterization
     */
    private AnalysisExtent m_Extent;

    /**
     * The overall shapecount of the given Tracks for rasterization
     */
    private int shapecount;

    private int rasteredTrackCount = 0;
    /**
     * the algorithm used for interpolation
     */
    private int m_iWeightingMethod;

    private int m_iSuperSamplingWindowSize;

    private boolean m_bUseReversedData;

    private boolean m_bUseNormalData;

    private final String USE_QUICK_CHECK = "USE_QUICK_CHECK";

    private boolean m_bQuickCheck;

    private double m_dRasterLowpassFilter;

    private Path tmpPath;

    private short trackcount = 0;

    private short processedTrackCount = 0;

    private boolean m_bUseLowerCPUUsage;

    private String m_ResultRMF;

    private boolean m_bIgnoreTimestamps;

    private boolean m_bPrototype;

    private double DETAREA_CUTTOFF;

    private boolean m_bCreateRMF;

    private static GeometryFactory geometryFactory = new GeometryFactory();

    /**
     * defines the Characteristics of the Algorithm within name, group, inputs
     * and output
     */
    @Override
    public void defineCharacteristics() {
        setName(Sextante.getText("FRAME Rasterize"));
        setGroup(Sextante.getText("Rasterization_and_interpolation"));
        setUserCanDefineAnalysisExtent(true);
        try {
            m_Parameters.addInputVectorLayer(LAYER,
                    Sextante.getText("Vector_layer"),
                    AdditionalInfoVectorLayer.SHAPE_TYPE_POINT, true);
            m_Parameters.addTableField(FIELD, Sextante.getText("Field"), LAYER);
            m_Parameters.addTableField(TIMESTAMP, Sextante.getText(TIMESTAMP),
                    LAYER);
            m_Parameters.addTableField(TRACKID, Sextante.getText(TRACKID),
                    LAYER);
            m_Parameters.addTableField(SENSORID, Sextante.getText(SENSORID),
                    LAYER);
            m_Parameters.addNumericalValue(LOW_CUT, Sextante.getText(LOW_CUT),
                    AdditionalInfoNumericalValue.NUMERICAL_VALUE_DOUBLE, 2,
                    Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY);
            m_Parameters.addNumericalValue(HIGH_CUT,
                    Sextante.getText(HIGH_CUT),
                    AdditionalInfoNumericalValue.NUMERICAL_VALUE_DOUBLE, -2,
                    Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY);
            m_Parameters.addNumericalValue(FILL_GAP,
                    Sextante.getText(FILL_GAP),
                    AdditionalInfoNumericalValue.NUMERICAL_VALUE_INTEGER, 0, 0,
                    Integer.MAX_VALUE);
            m_Parameters.addBoolean(LOWER_CPU_USAGE,
                    Sextante.getText(LOWER_CPU_USAGE), true);
            m_Parameters.addNumericalValue(SUPERSAMLING_WINDOW_SIZE,
                    Sextante.getText(SUPERSAMLING_WINDOW_SIZE),
                    AdditionalInfoNumericalValue.NUMERICAL_VALUE_INTEGER, 0, 0,
                    Integer.MAX_VALUE);
            m_Parameters.addNumericalValue(RASTER_LOWPASS_FILTER,
                    Sextante.getText(RASTER_LOWPASS_FILTER),
                    AdditionalInfoNumericalValue.NUMERICAL_VALUE_DOUBLE, 0, 0,
                    Double.MAX_VALUE);
            m_Parameters.addBoolean(USE_QUICK_CHECK,
                    Sextante.getText(USE_QUICK_CHECK), true);
            m_Parameters.addNumericalValue(WEIGHTING_METHOD,
                    Sextante.getText(WEIGHTING_METHOD),
                    AdditionalInfoNumericalValue.NUMERICAL_VALUE_INTEGER, 0, 0,
                    Integer.MAX_VALUE);
            m_Parameters.addNumericalValue(ROWS_AFTER,
                    Sextante.getText(ROWS_AFTER),
                    AdditionalInfoNumericalValue.NUMERICAL_VALUE_INTEGER, 0, 0,
                    Integer.MAX_VALUE);
            m_Parameters.addBoolean(IGNORE_TIMESTAMPS,
                    Sextante.getText(IGNORE_TIMESTAMPS), true);
            m_Parameters.addBoolean(PROTOTYPE, Sextante.getText(PROTOTYPE),
                    true);
            m_Parameters.addBoolean(CREATE_RMF, Sextante.getText(CREATE_RMF),
                    true);
            addOutputRasterLayer(RESULT, Sextante.getText("Result"));
            m_Parameters.addFilepath(RESULT_RMF,
                    Sextante.getText("Result RMF"), false, false, false, "rmf");

        } catch (final Exception e) {
            Sextante.addErrorToLog(e);
        }
    }

    /**
     * Tooks the Arguments from the m_Parameters, computes the different tracks.
     * The different tracks then will be rasterized by
     * distributeAndRunTrackRasterizations
     */
    @Override
    public boolean processAlgorithm() throws GeoAlgorithmExecutionException {
        NO_DATA = m_OutputFactory.getDefaultNoDataValue();
        NO_DATA = -99999;
        DETAREA_CUTTOFF = 1E-22;
        m_Layer = m_Parameters.getParameterValueAsVectorLayer(LAYER);
        m_iField = m_Parameters.getParameterValueAsInt(FIELD);
        m_iTimestamp = m_Parameters.getParameterValueAsInt(TIMESTAMP);
        m_iTrackID = m_Parameters.getParameterValueAsInt(TRACKID);
        m_iSensorID = m_Parameters.getParameterValueAsInt(SENSORID);
        m_dLowCut = m_Parameters.getParameterValueAsInt(LOW_CUT);
        m_dHighCut = m_Parameters.getParameterValueAsInt(HIGH_CUT);
        m_iThreads = 1;
        m_iFillGap = m_Parameters.getParameterValueAsInt(FILL_GAP);
        m_bUseLowerCPUUsage = m_Parameters
                .getParameterValueAsBoolean(LOWER_CPU_USAGE);
        m_iSuperSamplingWindowSize = m_Parameters
                .getParameterValueAsInt(SUPERSAMLING_WINDOW_SIZE);
        m_dRasterLowpassFilter = m_Parameters
                .getParameterValueAsDouble(RASTER_LOWPASS_FILTER);
        m_bQuickCheck = m_Parameters
                .getParameterValueAsBoolean(USE_QUICK_CHECK);
        m_bUseSensorIDs = true;// m_Parameters.getParameterValueAsBoolean(USE_SENSORIDS);
        m_bUseRowInBetweenInterpolation = false;// m=
                                                // m_Parameters.getParameterValueAsBoolean(USE_ROW_IN_BETWEEN_INTERPOLATION);
        m_bUsePreprocessing = false;// =
                                    // m_Parameters.getParameterValueAsBoolean(USE_PREPROCESSING);
        m_bUseReversedData = false;// =
                                   // m_Parameters.getParameterValueAsBoolean(USE_REVERSED_DATA);
        m_bUseNormalData = true;// =
                                // m_Parameters.getParameterValueAsBoolean(USE_NORMAL_DATA);
        m_iWeightingMethod = m_Parameters
                .getParameterValueAsInt(WEIGHTING_METHOD);
        m_iFilledInterpolationMethod = 0;// =
                                         // m_Parameters.getParameterValueAsInt(FILLING_INTERPOLATION_METHOD);
        m_iRowsAfter = m_Parameters.getParameterValueAsInt(ROWS_AFTER);
        m_bIgnoreTimestamps = m_Parameters
                .getParameterValueAsBoolean(IGNORE_TIMESTAMPS);
        m_bPrototype = m_Parameters.getParameterValueAsBoolean(PROTOTYPE);
        m_bCreateRMF = m_Parameters.getParameterValueAsBoolean(CREATE_RMF);
        m_Result = getNewRasterLayer(RESULT,
                m_Layer.getName() + Sextante.getText("[rasterizedo]"),
                IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
        m_Result.setNoDataValue(NO_DATA);
        m_Result.assignNoData();
        m_ResultRMF = m_Parameters.getParameterValueAsString(RESULT_RMF);
        Sextante.addInfoToLog(m_ResultRMF);

        Sextante.addInfoToLog("NO_DATA: " + NO_DATA);
        final Runtime rt = Runtime.getRuntime();
        if (m_bUseLowerCPUUsage) {
            if (rt.availableProcessors() <= 2) {
                m_iThreads = 1;
            } else if (rt.availableProcessors() <= 4) {
                m_iThreads = 2;
            } else {
                m_iThreads = rt.availableProcessors() - 2;
            }
        } else {
            m_iThreads = rt.availableProcessors() * 2;
        }
        // m_iThreads=1;
        try {
            String tmppath = (GeoAlgorithm.getTempFolder() + "\\RasterizeTracksAlgorithm\\tmp");
            File f = new File(tmppath);
            if (f.exists()) {
                for (String path : f.list()) {
                    Files.delete(new File(tmppath + "\\" + path).toPath());
                }
            }
            tmppath = (GeoAlgorithm.getTempFolder() + "\\RasterizeTracksAlgorithm\\");
            if (!Files.exists(new File(tmppath).toPath(),
                    LinkOption.NOFOLLOW_LINKS)) {
                Path p = Files.createDirectory(new File(tmppath).toPath());
                Sextante.addInfoToLog("" + p.toString() + " exists?"
                        + p.toFile().exists());

            }
            tmppath = (GeoAlgorithm.getTempFolder() + "\\RasterizeTracksAlgorithm\\tmp");
            if (!Files.exists(new File(tmppath).toPath(),
                    LinkOption.NOFOLLOW_LINKS)) {
                tmpPath = Files.createDirectory(new File(tmppath).toPath());
                Sextante.addInfoToLog("" + tmpPath.toString() + " exists?"
                        + tmpPath.toFile().exists());
            }

        } catch (IOException e) {
            Sextante.addErrorToLog(e);
            e.printStackTrace();
        }
        m_Extent = m_Result.getWindowGridExtent();
        IFeatureIterator shapeiter = m_Layer.iterator();

        shapecount = m_Layer.getShapesCount();

        GridCell offset = m_Extent.getGridCoordsFromWorldCoords(
                m_Extent.getXMin(), m_Extent.getYMin());
        Sextante.addInfoToLog("Grid starts on " + offset.getX() + ","
                + offset.getY());
        GridCell offset2 = m_Extent.getGridCoordsFromWorldCoords(
                m_Extent.getXMax(), m_Extent.getYMax());
        Sextante.addInfoToLog("Grid ends on " + offset2.getX() + ","
                + offset2.getY());
        Sextante.addInfoToLog("Grid boundsare " + offset.getX() + "x"
                + offset2.getY() + " - " + offset2.getX() + "x" + offset.getY());
        short trackID = Short.MAX_VALUE;

        setProgressText("Check trackcount");
        int shapes = 0;
        while (shapeiter.hasNext() && !m_Task.isCanceled()) {

            IFeature feature = shapeiter.next();
            short curTrackID = Short.parseShort(feature.getRecord()
                    .getValue(m_iTrackID).toString());
            if (curTrackID != trackID) {
                trackID = curTrackID;
                trackcount++;

                setProgress(shapes, shapecount);
            }
            shapes++;
        }
        shapeiter = m_Layer.iterator();
        setProgressText("Load data from Layer");
        splitIntoTracks(shapeiter, shapecount);
        if (m_iFillGap != 0 && !m_Task.isCanceled()) {
            setProgressText("filling gaps");
            fillgap(offset, offset2);

        }
        if (!m_Task.isCanceled() && m_bCreateRMF) {
            generateAveragedHistogramColorScheme(offset, offset2);
        }
        Sextante.addInfoToLog("getTempFolder():" + getTempFolder());
        return !m_Task.isCanceled();
    }

    private void generateAveragedHistogramColorScheme(GridCell offset,
            GridCell offset2) {
        try {
            setProgressText("generate averaged histogram color scheme");
            List<Double> d = new ArrayList<Double>();
            for (int dx = offset.getX(); dx < offset2.getX(); dx++) {
                for (int dy = offset2.getY(); dy < offset.getY(); dy++) {
                    double value = m_Result.getCellValueAsDouble(dx, dy);
                    if (!m_Result.isNoDataValue(value)
                            && (value < -m_dRasterLowpassFilter || m_dRasterLowpassFilter < value)) {
                        d.add(value);
                    }
                }
                setProgress(dx, offset2.getX());
            }
            Collections.sort(d);
            Collections.reverse(d);
            File file = new File(m_ResultRMF);
            PrintWriter pw = new PrintWriter(file, "ISO-8859-15");
            pw.println("<?xml version=\"1.0\" encoding=\"ISO-8859-15\"?>");
            pw.println("<RasterMetaFile>");
            pw.println("<ColorTable name=\"New scheme\" interpolated=\"1\" version=\"1.1\">");
            List<Double> d2 = new ArrayList<Double>(d.subList(0, d.size() / 4));
            d2.addAll(d.subList(d.size() * 3 / 4, d.size()));
            for (int i = 0; i < 5; i++) {
                d.addAll(d2);
            }
            Collections.sort(d);
            Collections.reverse(d);
            int stepwidth = d.size() / 255;
            for (int i = 0; i < 256; i++) {
                pw.println("\t<Color value=\""
                        + (i == 128 ? 0 : d.get(i * stepwidth)) + "\" name=\""
                        + i + "\" rgb=\"" + i + "," + i + "," + i
                        + "\" interpolated=\"50.0\"/>");
            }
            for (int i = 0; i < 256; i++) {
                pw.println("\t<Alpha value=\""
                        + (i == 128 ? 0 : d.get(i * stepwidth))
                        + "\" alpha=\"255\" interpolated=\"50.0\"/>");
            }
            pw.println("</ColorTable>");
            pw.println("</RasterMetaFile>");
            pw.flush();
            pw.close();
        } catch (FileNotFoundException | UnsupportedEncodingException e) {
            Sextante.addErrorToLog(e);
        }
    }

    private void fillgap(GridCell offset, GridCell offset2) {
        int iThreads = 1;
        if (iThreads < 2) {
            FillGapsAlgorithmWorker worker = new FillGapsAlgorithmWorker(
                    m_iFillGap, m_Result, m_Extent, m_iThreads, this,
                    m_bQuickCheck, m_iFilledInterpolationMethod, m_dLowCut,
                    m_dHighCut, m_iSuperSamplingWindowSize, DETAREA_CUTTOFF);
            worker.run();
        } else {
            List<FillGapsAlgorithmWorker> fillGapsAlgorithmWorkers = new ArrayList<FillGapsAlgorithmWorker>();
            List<Thread> fillGapsAlgorithmWorkersThreads = new ArrayList<Thread>();
            for (int i = 0; i < m_iThreads; i++) {
                FillGapsAlgorithmWorker worker = new FillGapsAlgorithmWorker(
                        m_iFillGap, m_Result, m_Extent, m_iThreads, this,
                        m_bQuickCheck, m_iFilledInterpolationMethod, m_dLowCut,
                        m_dHighCut, m_iSuperSamplingWindowSize, DETAREA_CUTTOFF);
                fillGapsAlgorithmWorkers.add(worker);
                Thread thread = new Thread(worker);
                fillGapsAlgorithmWorkersThreads.add(thread);
                thread.start();
            }
            for (Thread thread : fillGapsAlgorithmWorkersThreads) {
                try {
                    thread.join();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Creates for each Track an RasterizeTracksAlgorithmWorker and executes
     * them. if m_iThreads > 1 -> creates m_iThreads Threads and distributes the
     * RasterizeTracksAlgorithmWorker to them. else -> runs the workers
     * sequentially
     * 
     * @param tracks
     *            - tracks which has to be rasterized
     */
    private void distributeAndRunTrackRasterizations(
            ArrayList<ArrayList<IFeature>> tracks) {
        List<RasterizeTracksAlgorithmWorker> workers = new ArrayList<RasterizeTracksAlgorithmWorker>();
        for (ArrayList<IFeature> track : tracks) {
            // Sextante.addInfoToLog("Raster Track with size:"+track.size());
            TrackPartIterable tpi = new TrackPartIterable(track, 16,
                    m_iRowsAfter, m_iField, m_iSensorID, m_iTrackID,
                    m_iTimestamp, m_bUseSensorIDs,
                    m_bUseRowInBetweenInterpolation, m_Extent, m_iFillGap > 0,
                    m_bIgnoreTimestamps);
            workers.add(new RasterizeTracksAlgorithmWorker(tpi, this,
                    m_bUsePreprocessing, m_bUseNormalData, m_bUseReversedData,
                    5, m_bPrototype));
        }
        List<Thread> startedWorkerThreads = new ArrayList<Thread>();
        List<RasterizeTracksAlgorithmWorker> startedworkers = new ArrayList<RasterizeTracksAlgorithmWorker>();
        while (!workers.isEmpty() && !m_Task.isCanceled()) {

            // processstep = 0;

            setProgressText("Rasterize tracks " + rasteredTrackCount + "-"
                    + (rasteredTrackCount + m_iThreads));
            if (m_iThreads > 1) {
                for (int i = 0; i < m_iThreads; i++) {
                    if (workers.isEmpty() || m_Task.isCanceled())
                        break;
                    RasterizeTracksAlgorithmWorker worker = workers.remove(0);
                    Thread t = new Thread(worker);
                    startedWorkerThreads.add(t);
                    startedworkers.add(worker);
                    t.start();
                }
                for (Thread startedworkerThread : startedWorkerThreads) {
                    try {
                        startedworkerThread.join();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            } else {
                RasterizeTracksAlgorithmWorker worker = workers.remove(0);
                startedworkers.add(worker);
                worker.run();
            }
            if (!m_Task.isCanceled()) {
                for (RasterizeTracksAlgorithmWorker startedworker : startedworkers) {
                    startedworker.writeResult(m_Result);
                    rasteredTrackCount++;
                }
            }
            startedworkers.clear();
            startedWorkerThreads.clear();
        }

    }

    /**
     * Iterates of a given IFeatureIterator and splits the IFeature 's according
     * to there m_iTrackID. If the trackID is -1 the data is invalid and has to
     * be dropped.
     * 
     * @param shapeiter
     *            - the Iterator of the given Shapes of all tracks
     * @param shapecount
     *            - the count of estimated shapes, used to show the progress bar
     * @return an ArrayList of Tracks (Arraylist of IFeature 's)
     */
    private ArrayList<ArrayList<IFeature>> splitIntoTracks(
            final IFeatureIterator shapeiter, int shapecount) {
        ArrayList<ArrayList<IFeature>> finishedTracks = new ArrayList<ArrayList<IFeature>>();
        ArrayList<ArrayList<IFeature>> curTracks = splitTracks(shapeiter);
        Callable<ArrayList<ArrayList<IFeature>>> splitter = null;
        if (1 < m_iThreads) {
            splitter = new Callable<ArrayList<ArrayList<IFeature>>>() {
                @Override
                public ArrayList<ArrayList<IFeature>> call() throws Exception {
                    return splitTracks(shapeiter);
                }
            };
        }
        while (shapeiter.hasNext() && !m_Task.isCanceled()) {
            if (m_iThreads < 2) {
                distributeAndRunTrackRasterizations(curTracks);
                finishedTracks.addAll(curTracks);
                curTracks = splitTracks(shapeiter);
            } else {
                FutureTask<ArrayList<ArrayList<IFeature>>> splittertask = new FutureTask<ArrayList<ArrayList<IFeature>>>(
                        splitter);
                Thread t = new Thread(splittertask);
                t.start();
                distributeAndRunTrackRasterizations(curTracks);

                try {
                    curTracks = splittertask.get();
                } catch (InterruptedException e) {
                    Sextante.addErrorToLog(e);
                    e.printStackTrace();
                } catch (ExecutionException e) {
                    Sextante.addErrorToLog(e);
                    e.printStackTrace();
                }
            }
        }
        distributeAndRunTrackRasterizations(curTracks);
        return finishedTracks;
    }

    private ArrayList<ArrayList<IFeature>> splitTracks(
            IFeatureIterator shapeiter) {
        ArrayList<ArrayList<IFeature>> tracks = new ArrayList<ArrayList<IFeature>>();
        IFeature feature;
        ArrayList<IFeature> curtrack = new ArrayList<IFeature>();
        short curtrackID = Short.MIN_VALUE;
        short trackID = -1;
        while (shapeiter.hasNext() && !m_Task.isCanceled()) {
            try {
                feature = shapeiter.next();
                trackID = Short.parseShort(feature.getRecord()
                        .getValue(m_iTrackID).toString());
                if (trackID == -1) {
                    continue;
                }
                if (trackID != curtrackID) {
                    if (!curtrack.isEmpty()) {
                        tracks.add(curtrack);
                        if (tracks.size() == m_iThreads) {
                            return tracks;
                        }
                    }
                    curtrack = new ArrayList<IFeature>();
                    curtrackID = trackID;
                }
                curtrack.add(feature);
            } catch (IteratorException e) {
                Sextante.addInfoToLog(e.toString());
            }
        }
        if (!curtrack.isEmpty()) {
            tracks.add(curtrack);
        }
        return tracks;
    }

    /**
     * The rasterization of a given Track (TrackPartIterable) within the offset
     * where the data has to be stored to the calcValues , calcCounts
     * 
     * @param tpi
     *            - the TrackPartIterable of the given Track, which has to be
     *            rasterized
     * @param rasterosset
     *            - the offset because of shifting the data of the track to 0,0
     * @param calcValues
     *            - the memory where the results of the computation are stored
     *            to
     * @param calcCounts
     *            - the memory where the count results of the computation are
     *            stored to (averaging on the end)
     */
    public void rasterize(TrackPartIterable tpi, int[] rasteroffset,
            double[] calcValues, int[] calcCounts, int maxX, int maxY) {
        TrackPartIterator iter = tpi.iterator();
        TrackPart[] step = null;

        try {
            iter.init();
        } catch (GeoAlgorithmExecutionException e) {
            Sextante.addErrorToLog("Error during initialzation");
            Sextante.addErrorToLog(e);
        }
        int[] caseCount = new int[3];
        BarycentricInterpolationKernel bik5 = null;
        try {
            bik5 = new BarycentricInterpolationKernel(m_Extent.getCellSizeX(),
                    m_Extent.getCellSizeY(), m_Extent.getXMin(),
                    m_Extent.getYMax(), m_dHighCut, m_dLowCut, DETAREA_CUTTOFF);
            Sextante.addInfoToLog("m_Extent.getCellSizeX()"
                    + m_Extent.getCellSizeX() + "m_Extent.getCellSizeY()"
                    + m_Extent.getCellSizeY());
        } catch (Exception e) {
            Sextante.addErrorToLog(e);
        }

        while (iter.hasNext() && !m_Task.isCanceled()) {

            step = iter.next();
            if (step == null)
                continue;
            int lastStep = step.length - 1;
            delauneyBarycentricApproach(rasteroffset, calcValues, calcCounts,
                    step, caseCount, lastStep, iter, maxX, maxY, bik5);

        }
        if (m_iFillGap > 0) {

            try {

                short trackNr = iter.getTrack();
                String path = (GeoAlgorithm.getTempFolder() + "\\RasterizeTracksAlgorithm\\tmp");
                Path outputPath = new File(path + "\\" + trackNr + ".tmp")
                        .toPath();
                Path outputFile = null;
                if (Files.exists(outputPath, LinkOption.NOFOLLOW_LINKS)) {
                    outputFile = outputPath;
                } else {
                    outputFile = Files.createFile(outputPath);
                }
                ByteArrayOutputStream db = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(db);
                Sensor[] bounds = tpi.getCompleteBounds();
                // Sextante.addInfoToLog("bounds of "+trackNr+ " contains
                // "+bounds.length+" points");
                for (int i = 0; i < bounds.length; i++) {
                    dos.writeDouble(bounds[i].x);
                    dos.writeDouble(bounds[i].y);

                    dos.writeDouble(bounds[i].value);
                }
                // Sextante.addInfoToLog("bounds[0].z"+bounds[0].z);
                // Sextante.addInfoToLog("generated:"+outputFile.toString());
                FileOutputStream fos = new FileOutputStream(outputFile.toFile());
                db.writeTo(fos);
                db.flush();
                db.close();
                fos.flush();
                fos.close();
                // Sextante.addInfoToLog(tmp.getAbsolutePath()+" saved");
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
                Sextante.addErrorToLog(e);
            }
        }
        setProgress(++processedTrackCount, trackcount);
    }

    private void delauneyBarycentricApproach(int[] rasteroffset,
            double[] calcValues, int[] calcCounts, TrackPart[] step,
            int[] caseCount, int lastStep, TrackPartIterator iter, int maxX,
            int maxY, BarycentricInterpolationKernel bik5) {
        List<Sensor> sensorsAsList = new ArrayList<Sensor>();
        Map<Coordinate, Sensor> values = new HashMap<Coordinate, Sensor>();

        for (int j = 0; j < step[0].getSensors().length; j++) {
            if (step[0].getSensors()[j] != null) {
                sensorsAsList.add(step[0].getSensors()[j]);
                values.put(step[0].getSensors()[j], step[0].getSensors()[j]);
            }
        }
        for (int i = 1; i <= lastStep; i++) {
            if (step[i - 1].distance(step[i]) > iter.getStepDist() * 1.5 * i)
                continue;
            for (int j = 0; j < step[i].getSensors().length; j++) {
                if (step[i].getSensors()[j] != null) {
                    sensorsAsList.add(step[i].getSensors()[j]);
                    values.put(step[i].getSensors()[j], step[i].getSensors()[j]);
                }
            }
        }
        if (sensorsAsList.isEmpty())
            return;
        Sensor[] sensors = sensorsAsList.toArray(new Sensor[1]);
        Geometry triangles = FillGapsAlgorithmWorker
                .delaunayTriangulation(geometryFactory
                        .createMultiPoint(sensors));
        if (m_iWeightingMethod == 0) {
            for (int j = 0; j < triangles.getNumGeometries(); j++) {
                Coordinate[] cs = triangles.getGeometryN(j).getCoordinates();
                idw(values.get(cs[0]), values.get(cs[1]), values.get(cs[2]),
                        rasteroffset, calcValues, calcCounts, maxX, maxY);
            }
        } else {
            List<Sensor> sensorVerteciesAsList = new ArrayList<Sensor>();
            for (int j = 0; j < triangles.getNumGeometries(); j++) {
                Coordinate[] cs = triangles.getGeometryN(j).getCoordinates();
                sensorVerteciesAsList.add(values.get(cs[0]));
                sensorVerteciesAsList.add(values.get(cs[1]));
                sensorVerteciesAsList.add(values.get(cs[2]));
            }
            Sensor[] sensorVerteciesAsArray = sensorVerteciesAsList
                    .toArray(new Sensor[sensorVerteciesAsList.size()]);
            if (m_iWeightingMethod == 1) {
                barycentricInterpolation4GPU(sensorVerteciesAsArray,
                        rasteroffset, calcValues, calcCounts, maxX, maxY);
            } else if (m_iWeightingMethod == 2) {
                barycentricInterpolation4GPU_2(sensorVerteciesAsArray,
                        rasteroffset, calcValues, calcCounts, maxX, maxY, bik5);
            }
        }
    }

    public int[] idw(Sensor sensor0, Sensor sensor1, Sensor sensor2,
            int[] rasteroffset, double[] calcValues, int[] calcCounts,
            int maxX, int maxY) {
        Sensor[] coords = new Sensor[] { sensor0, sensor1, sensor2 };
        double[] computedOffsets = computeOffsets(coords);
        Coordinate[] normedCoords = new Coordinate[coords.length];
        for (int i = 0; i < coords.length; i++) {
            normedCoords[i] = new Coordinate((coords[i].x + computedOffsets[0])
                    / computedOffsets[2], (coords[i].y + computedOffsets[1])
                    / computedOffsets[3]);
        }
        int[] raster = computeRasterbounds(coords);
        double detArea = (normedCoords[0].x - normedCoords[2].x)
                * (normedCoords[1].y - normedCoords[2].y)
                - (normedCoords[1].x - normedCoords[2].x)
                * (normedCoords[0].y - normedCoords[2].y);
        double scaledDetArea = (detArea * m_Result.getLayerCellSizeX())
                * m_Result.getLayerCellSizeY();
        if (scaledDetArea > DETAREA_CUTTOFF) {
            Sextante.addInfoToLog(scaledDetArea + ">" + DETAREA_CUTTOFF);
            return raster;
        }
        double a = Math.max(Math.min(sensor0.value, m_dHighCut), m_dLowCut);
        double b = Math.max(Math.min(sensor1.value, m_dHighCut), m_dLowCut);
        double c = Math.max(Math.min(sensor2.value, m_dHighCut), m_dLowCut);
        int halfwindowssize = m_iSuperSamplingWindowSize;
        double windowStep = Math.min(m_Extent.getCellSizeX(),
                m_Extent.getCellSizeY())
                / (2 * halfwindowssize);
        for (int x = raster[0]; x < raster[2]; x++) {
            for (int y = raster[1]; y < raster[3]; y++) {
                Coordinate worldPos = getWorldCoordsFromGridCoords(x, y);
                double total = 0;
                int count = 0;
                for (int dx = -halfwindowssize; dx <= halfwindowssize; dx++) {
                    for (int dy = -halfwindowssize; dy <= halfwindowssize; dy++) {
                        double centerx = ((worldPos.x + dx * windowStep) + computedOffsets[0])
                                / computedOffsets[2];
                        double centery = ((worldPos.y + dy * windowStep) + computedOffsets[1])
                                / computedOffsets[3];
                        double alpha = (centerx - normedCoords[2].x)
                                * (normedCoords[1].y - normedCoords[2].y)
                                - (normedCoords[1].x - normedCoords[2].x)
                                * (centery - normedCoords[2].y);
                        double beta = (centerx - normedCoords[0].x)
                                * (normedCoords[2].y - normedCoords[0].y)
                                - (normedCoords[2].x - normedCoords[0].x)
                                * (centery - normedCoords[0].y);
                        double gamma = (centerx - normedCoords[1].x)
                                * (normedCoords[0].y - normedCoords[1].y)
                                - (normedCoords[0].x - normedCoords[1].x)
                                * (centery - normedCoords[1].y);

                        if (alpha < BARYCENTRIC_CUTOFF
                                || beta < BARYCENTRIC_CUTOFF
                                || gamma < BARYCENTRIC_CUTOFF)
                            continue;
                        alpha = Math.hypot(coords[0].x - centerx, coords[0].y
                                - centery);
                        beta = Math.hypot(coords[1].x - centerx, coords[1].y
                                - centery);
                        gamma = Math.hypot(coords[2].x - centerx, coords[2].y
                                - centery);
                        double sum = alpha + beta + gamma;
                        alpha = 1 - (alpha / sum);
                        beta = 1 - (beta / sum);
                        gamma = 1 - (gamma / sum);
                        total += (alpha * a + beta * b + gamma * c);
                        count++;
                    }
                }
                int dx = (int) (x + rasteroffset[0]);
                int dy = (int) (y + rasteroffset[1]);

                if (0 <= dx && dx < maxX && 0 <= dy && dy < maxY && count > 0) {
                    calcValues[dy * maxX + dx] += total / count;
                    ++calcCounts[dy * maxX + dx];
                }

            }
        }
        return raster;
    }

    public int[] barycentricInterpolation4GPU(Sensor[] sensor0,
            int[] trackRasterOffset, double[] calcValues, int[] calcCounts,
            int maxX, int maxY) {
        int[] globalrastersize = computeRasterbounds(sensor0);

        int globalWidth = globalrastersize[2] - globalrastersize[0];
        int globalHeight = globalrastersize[3] - globalrastersize[1];
        ;
        double[] globalCalcValues = new double[globalWidth * globalHeight];
        double[] globalCalcCount = new double[globalWidth * globalHeight];
        for (int triangle = 0; triangle < sensor0.length / 3; triangle++) {
            Sensor[] coords = Arrays.copyOfRange(sensor0, triangle * 3,
                    (triangle + 1) * 3);
            int[] raster = computeRasterbounds(coords);
            if (!(globalrastersize[0] <= raster[0]
                    && raster[2] <= globalrastersize[2]
                    && globalrastersize[1] <= raster[1] && raster[3] <= globalrastersize[3]))
                continue;
            Sensor[] normedCoords = coords;
            double detArea = (normedCoords[0].x - normedCoords[2].x)
                    * (normedCoords[1].y - normedCoords[2].y)
                    - (normedCoords[1].x - normedCoords[2].x)
                    * (normedCoords[0].y - normedCoords[2].y);
            double scaledDetArea = (detArea * m_Result.getLayerCellSizeX())
                    * m_Result.getLayerCellSizeY();
            if (scaledDetArea > DETAREA_CUTTOFF) {
                Sextante.addInfoToLog(scaledDetArea + ">" + DETAREA_CUTTOFF);
                continue;
            }
            double a = Math.max(Math.min(coords[0].value, m_dHighCut),
                    m_dLowCut);
            double b = Math.max(Math.min(coords[1].value, m_dHighCut),
                    m_dLowCut);
            double c = Math.max(Math.min(coords[2].value, m_dHighCut),
                    m_dLowCut);

            for (int x = globalrastersize[0]; x < globalrastersize[2]; x++) {
                for (int y = globalrastersize[1]; y < globalrastersize[3]; y++) {
                    double total = 0;
                    int count = 0;
                    Coordinate dscaled = getWorldCoordsFromGridCoords(x, y);
                    double centerx = dscaled.x;
                    double centery = dscaled.y;
                    double alpha = (centerx - normedCoords[2].x)
                            * (normedCoords[1].y - normedCoords[2].y)
                            - (normedCoords[1].x - normedCoords[2].x)
                            * (centery - normedCoords[2].y);
                    double beta = (centerx - normedCoords[0].x)
                            * (normedCoords[2].y - normedCoords[0].y)
                            - (normedCoords[2].x - normedCoords[0].x)
                            * (centery - normedCoords[0].y);
                    double gamma = (centerx - normedCoords[1].x)
                            * (normedCoords[0].y - normedCoords[1].y)
                            - (normedCoords[0].x - normedCoords[1].x)
                            * (centery - normedCoords[1].y);
                    if (alpha > BARYCENTRIC_CUTOFF && beta > BARYCENTRIC_CUTOFF
                            && gamma > BARYCENTRIC_CUTOFF) {
                        total += (alpha * a + beta * b + gamma * c) / detArea;
                        count++;
                    } else if (m_iSuperSamplingWindowSize > 0) {
                        for (int halfwindowssize = 1; halfwindowssize <= m_iSuperSamplingWindowSize; halfwindowssize++) {
                            for (int dx = -halfwindowssize; dx <= halfwindowssize; dx++) {
                                for (int dy = -halfwindowssize; dy <= halfwindowssize; dy++) {
                                    dscaled = getWorldCoordsFromGridCoords(x, y);
                                    centerx = dscaled.x;
                                    centery = dscaled.y;
                                    alpha = (centerx - normedCoords[2].x)
                                            * (normedCoords[1].y - normedCoords[2].y)
                                            - (normedCoords[1].x - normedCoords[2].x)
                                            * (centery - normedCoords[2].y);
                                    beta = (centerx - normedCoords[0].x)
                                            * (normedCoords[2].y - normedCoords[0].y)
                                            - (normedCoords[2].x - normedCoords[0].x)
                                            * (centery - normedCoords[0].y);
                                    gamma = (centerx - normedCoords[1].x)
                                            * (normedCoords[0].y - normedCoords[1].y)
                                            - (normedCoords[0].x - normedCoords[1].x)
                                            * (centery - normedCoords[1].y);

                                    if (alpha < BARYCENTRIC_CUTOFF
                                            || beta < BARYCENTRIC_CUTOFF
                                            || gamma < BARYCENTRIC_CUTOFF)
                                        continue;

                                    total += (alpha * a + beta * b + gamma * c)
                                            / detArea;
                                    count++;
                                }
                            }
                            if (count > 0) {
                                break;
                            }
                        }
                    }
                    int dx = (int) (x - globalrastersize[0]);
                    int dy = (int) (y - globalrastersize[1]);
                    if (count > 0) {
                        globalCalcValues[dy * globalWidth + dx] += total;
                        globalCalcCount[dy * globalWidth + dx] += count;
                    }
                }
            }
        }
        for (int x = globalrastersize[0]; x < globalrastersize[2]; x++) {
            for (int y = globalrastersize[1]; y < globalrastersize[3]; y++) {
                int dx = (int) (x + trackRasterOffset[0]);
                int dy = (int) (y + trackRasterOffset[1]);
                int sx = (int) (x - globalrastersize[0]);
                int sy = (int) (y - globalrastersize[1]);
                if (globalCalcCount[sy * globalWidth + sx] > 0) {
                    calcValues[dy * maxX + dx] += globalCalcValues[sy
                            * globalWidth + sx]
                            / globalCalcCount[sy * globalWidth + sx];
                    calcCounts[dy * maxX + dx] = calcCounts[dy * maxX + dx] + 1;
                }
            }
        }
        return globalrastersize;
    }

    public int[] barycentricInterpolation4GPU_2(Sensor[] sensor0,
            int[] trackRasterOffset, double[] calcValues, int[] calcCounts,
            int maxX, int maxY, BarycentricInterpolationKernel bik5) {
        int[] globalrastersize = computeRasterbounds(sensor0);
        int globalWidth = globalrastersize[2] - globalrastersize[0];
        int globalHeight = globalrastersize[3] - globalrastersize[1];
        double[] coordsx = new double[sensor0.length];
        double[] coordsy = new double[sensor0.length];
        double[] values = new double[sensor0.length];
        for (int i = 0; i < sensor0.length; i++) {
            coordsx[i] = sensor0[i].x;
            coordsy[i] = sensor0[i].y;
            values[i] = sensor0[i].value;
        }
        bik5.init(globalrastersize, coordsx, coordsy, values);

        Range range = Range.create(globalWidth * globalHeight);
        bik5.setExplicit(true);
        try {
            bik5.execute(range);
            // Thread.sleep(bik5.getExecutionTime()*m_iThreads);
        } catch (Exception e) {
            Sextante.addErrorToLog(e);
        }

        double[] globalCalcCount = bik5.getGlobalCalcCount();
        double[] globalCalcValues = bik5.getGlobalCalcValues();
        for (int x = globalrastersize[0]; x < globalrastersize[2]; x++) {
            for (int y = globalrastersize[1]; y < globalrastersize[3]; y++) {
                int dx = (int) (x + trackRasterOffset[0]);
                int dy = (int) (y + trackRasterOffset[1]);
                int sx = (int) (x - globalrastersize[0]);
                int sy = (int) (y - globalrastersize[1]);
                // Sextante.addInfoToLog("dx:"+dx+"dy:"+dy);
                // if (0 <= dx && dx < maxX && 0 <= dy && dy < maxY &&
                // globalCalcCount[y*globalWidth+x] > 0) {
                if (globalCalcCount[sy * globalWidth + sx] > 0) {
                    calcValues[dy * maxX + dx] += globalCalcValues[sy
                            * globalWidth + sx]
                            / globalCalcCount[sy * globalWidth + sx];
                    // if(d==calcValues[dy * maxX + dx])
                    // Sextante.addInfoToLog("dafuq x:"+x+"y:"+y+","+calcValues[dy
                    // * maxX +
                    // dx]+" + "+globalCalcValues[sy*globalWidth+sx]/globalCalcCount[sy*globalWidth+sx]+" with v:"+globalCalcValues[sy*globalWidth+sx]+" and c:"+globalCalcCount[sy*globalWidth+sx]);
                    calcCounts[dy * maxX + dx] = calcCounts[dy * maxX + dx] + 1;
                }
            }
        }
        return globalrastersize;
    }

    private Coordinate getWorldCoordsFromGridCoords(int x, int y) {
        return m_Extent.getWorldCoordsFromGridCoords(x, y, 0);
    }

    public int[] barycentricInterpolation3(Sensor sensor0, Sensor sensor1,
            Sensor sensor2, int[] rasteroffset, double[][] calcValues,
            int[][] calcCounts) {
        int multiplier = 1;
        Sensor[] sensors = new Sensor[] { sensor0, sensor1, sensor2 };
        Arrays.sort(sensors);

        int[] raster = computeRasterbounds(sensors);
        Coordinate[] coords = new Coordinate[] {
                new Coordinate(sensors[0].x * multiplier, sensors[0].y
                        * multiplier),
                new Coordinate(sensors[1].x * multiplier, sensors[1].y
                        * multiplier),
                new Coordinate(sensors[2].x * multiplier, sensors[2].y
                        * multiplier) };
        double detArea = (coords[0].x - coords[2].x)
                * (coords[1].y - coords[2].y) - (coords[1].x - coords[2].x)
                * (coords[0].y - coords[2].y);
        double a = Math.max(Math.min(sensors[0].value, m_dHighCut), m_dLowCut);
        double b = Math.max(Math.min(sensors[1].value, m_dHighCut), m_dLowCut);
        double c = Math.max(Math.min(sensors[2].value, m_dHighCut), m_dLowCut);
        int halfwindowssize = m_iSuperSamplingWindowSize;
        double windowStep = Math.min(m_Extent.getCellSizeX(),
                m_Extent.getCellSizeY())
                / (2 * halfwindowssize);
        for (int x = raster[0]; x < raster[2]; x++) {
            for (int y = raster[1]; y < raster[3]; y++) {
                Coordinate dscaled = getWorldCoordsFromGridCoords(x, y);
                double total = 0;
                int count = 0;
                for (int dx = -halfwindowssize; dx <= halfwindowssize; dx++) {
                    for (int dy = -halfwindowssize; dy <= halfwindowssize; dy++) {

                        double centerx = (dscaled.x + dx * windowStep)
                                * multiplier;
                        double centery = (dscaled.y + dy * windowStep)
                                * multiplier;
                        double alpha = (centerx - coords[2].x)
                                * (coords[1].y - coords[2].y)
                                - (coords[1].x - coords[2].x)
                                * (centery - coords[2].y);
                        double beta = (centerx - coords[0].x)
                                * (coords[2].y - coords[0].y)
                                - (coords[2].x - coords[0].x)
                                * (centery - coords[0].y);
                        double gamma = (centerx - coords[1].x)
                                * (coords[0].y - coords[1].y)
                                - (coords[0].x - coords[1].x)
                                * (centery - coords[1].y);
                        if (alpha < BARYCENTRIC_CUTOFF
                                || beta < BARYCENTRIC_CUTOFF
                                || gamma < BARYCENTRIC_CUTOFF)
                            continue;

                        total += (alpha * a + beta * b + gamma * c) / detArea;
                        count++;
                    }
                }
                int dx = (int) (x + rasteroffset[0]);
                int dy = (int) (y + rasteroffset[1]);

                if (0 <= dx && dx < calcCounts[0].length && 0 <= dy
                        && dy < calcCounts.length && count > 0) {
                    calcValues[dy][dx] += total / count;
                    ++calcCounts[dy][dx];
                }

            }
        }

        Sensor tmp = sensors[0];
        sensors[0] = sensors[2];
        sensors[2] = tmp;
        coords = new Coordinate[] {
                new Coordinate(sensors[0].x * multiplier, sensors[0].y
                        * multiplier),
                new Coordinate(sensors[1].x * multiplier, sensors[1].y
                        * multiplier),
                new Coordinate(sensors[2].x * multiplier, sensors[2].y
                        * multiplier) };
        detArea = (coords[0].x - coords[2].x) * (coords[1].y - coords[2].y)
                - (coords[1].x - coords[2].x) * (coords[0].y - coords[2].y);
        a = Math.max(Math.min(sensors[0].value, m_dHighCut), m_dLowCut);
        b = Math.max(Math.min(sensors[1].value, m_dHighCut), m_dLowCut);
        c = Math.max(Math.min(sensors[2].value, m_dHighCut), m_dLowCut);
        halfwindowssize = m_iSuperSamplingWindowSize;
        windowStep = Math.min(m_Extent.getCellSizeX(), m_Extent.getCellSizeY())
                / (2 * halfwindowssize);
        for (int x = raster[0]; x < raster[2]; x++) {
            for (int y = raster[1]; y < raster[3]; y++) {
                Coordinate dscaled = getWorldCoordsFromGridCoords(x, y);
                double total = 0;
                int count = 0;
                for (int dx = -halfwindowssize; dx <= halfwindowssize; dx++) {
                    for (int dy = -halfwindowssize; dy <= halfwindowssize; dy++) {
                        double centerx = (dscaled.x + dx * windowStep)
                                * multiplier;
                        double centery = (dscaled.y + dy * windowStep)
                                * multiplier;
                        double alpha = (centerx - coords[2].x)
                                * (coords[1].y - coords[2].y)
                                - (coords[1].x - coords[2].x)
                                * (centery - coords[2].y);
                        double beta = (centerx - coords[0].x)
                                * (coords[2].y - coords[0].y)
                                - (coords[2].x - coords[0].x)
                                * (centery - coords[0].y);
                        double gamma = (centerx - coords[1].x)
                                * (coords[0].y - coords[1].y)
                                - (coords[0].x - coords[1].x)
                                * (centery - coords[1].y);

                        if (alpha < BARYCENTRIC_CUTOFF
                                || beta < BARYCENTRIC_CUTOFF
                                || gamma < BARYCENTRIC_CUTOFF)
                            continue;

                        total += (alpha * a + beta * b + gamma * c) / detArea;
                        count++;
                    }
                }
                int dx = (int) (x + rasteroffset[0]);
                int dy = (int) (y + rasteroffset[1]);

                if (0 <= dx && dx < calcCounts[0].length && 0 <= dy
                        && dy < calcCounts.length && count > 0) {
                    calcValues[dy][dx] += total / count;
                    ++calcCounts[dy][dx];
                }

            }
        }
        return raster;
    }

    private double[] computeOffsets(Coordinate[] cs) {
        double minX = Double.MAX_VALUE;
        double maxX = Double.MIN_VALUE;
        double minY = Double.MAX_VALUE;
        double maxY = Double.MIN_VALUE;
        for (Coordinate coord : cs) {
            minX = coord.x < minX ? coord.x : minX;
            maxX = coord.x > maxX ? coord.x : maxX;
            minY = coord.y < minY ? coord.y : minY;
            maxY = coord.y > maxY ? coord.y : maxY;
        }
        double xoffset = minX < 0 ? -minX : 0;
        double yoffset = minY < 0 ? -minY : 0;
        maxX += xoffset;
        maxY += yoffset;
        return new double[] { xoffset, yoffset, maxX, maxY };
    }

    /**
     * computes the bounds for rasterizing of a track given as an List of
     * IFeatures
     * 
     * @param features
     *            of the given track
     * @return an int[4] { minX, minY, maxX, maxY};
     */
    public int[] computeRasterbounds(ArrayList<IFeature> features) {
        int minX = Integer.MAX_VALUE;
        int maxX = Integer.MIN_VALUE;
        int minY = Integer.MAX_VALUE;
        int maxY = Integer.MIN_VALUE;
        for (IFeature feature : features) {
            Coordinate coord = feature.getGeometry().getCoordinate();
            GridCell coordinate = m_Extent.getGridCoordsFromWorldCoords(
                    coord.x, coord.y);
            minX = coordinate.getX() < minX ? coordinate.getX() : minX;
            maxX = coordinate.getX() > maxX ? coordinate.getX() : maxX;
            minY = coordinate.getY() < minY ? coordinate.getY() : minY;
            maxY = coordinate.getY() > maxY ? coordinate.getY() : maxY;
        }
        return new int[] { minX, minY, maxX, maxY };
    }

    /**
     * computes the bounds for rasterizing of a track given as an Array of
     * coordinates
     * 
     * @param coordinates
     *            of the given track
     * @return an int[4] { minX, minY, maxX, maxY};
     */
    public int[] computeRasterbounds(Coordinate[] coords) {
        int minX = Integer.MAX_VALUE;
        int maxX = Integer.MIN_VALUE;
        int minY = Integer.MAX_VALUE;
        int maxY = Integer.MIN_VALUE;
        for (Coordinate coord : coords) {
            GridCell coordinate = m_Extent.getGridCoordsFromWorldCoords(
                    coord.x, coord.y);
            minX = coordinate.getX() < minX ? coordinate.getX() : minX;
            maxX = coordinate.getX() > maxX ? coordinate.getX() : maxX;
            minY = coordinate.getY() < minY ? coordinate.getY() : minY;
            maxY = coordinate.getY() > maxY ? coordinate.getY() : maxY;
        }
        return new int[] { minX, minY, maxX, maxY };
    }

    public static void main(String[] args) {

        File tmp = new File("tmp/" + 1 + ".tmp");
        System.out.println(tmp.getAbsolutePath());

    }

    @Override
    public boolean setProgress(int iStep, int iTotalNumberOfSteps) {
        // TODO Auto-generated method stub
        return super.setProgress(iStep, iTotalNumberOfSteps);
    }

    public ITaskMonitor getTask() {
        return m_Task;
    }
}