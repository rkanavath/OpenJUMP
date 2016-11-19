package es.unex.sextante.rasterize.rasterizeTracks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.dataObjects.IFeature;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.dataObjects.IRecord;

public class RasterizeTracksAlgorithmWorker implements Runnable {

    private static GeometryFactory geometryFactory = new GeometryFactory();
    private TrackPartIterable tpi;
    private RasterizeTracksAlgorithm rasterizeTracksAlgorithm;
    private int[] raster;
    private double[] m_CalcValues;
    private int[] m_CalcCounts;
    private int xoffset;
    private int yoffset;
    private boolean usePreprocessing;
    private boolean useNormalData;
    private boolean useReversedData;
    private int m_fillgap;
    private int maxX;
    private int maxY;
    private boolean prototype;

    public RasterizeTracksAlgorithmWorker(TrackPartIterable tpi,
            RasterizeTracksAlgorithm rasterizeTracksAlgorithm,
            boolean usePreprocessing, boolean useNormalData,
            boolean useReversedData, int fillgap, boolean prototype) {
        this.tpi = tpi;
        this.rasterizeTracksAlgorithm = rasterizeTracksAlgorithm;
        this.usePreprocessing = usePreprocessing;
        this.useNormalData = useNormalData;
        this.useReversedData = useReversedData;
        this.m_fillgap = fillgap;
        this.prototype = prototype;
    }

    /**
     * runs a new worker for rasterisation of a specific track - computes the
     * size of the necessary track - uses the size for creating the buffer -
     * rasterizes the track to the given buffer This is done because the
     * outputraster seems to be synchronized, otherwise the threading itself
     * doesn't change anything
     */
    @Override
    public void run() {
        if (usePreprocessing) {
            preprocessData(tpi);
        }
        if (prototype) {
            adjust(tpi);
        }
        // compute the size of the buffer and offset for minimizing the
        // necessary memory used for the current track
        raster = rasterizeTracksAlgorithm.computeRasterbounds(tpi.getData());
        xoffset = -raster[0];
        yoffset = -raster[1];
        raster[0] = xoffset;
        raster[1] = yoffset;
        raster[2] = raster[2] + xoffset;
        raster[3] = raster[3] + yoffset;
        maxX = raster[2];
        maxY = raster[3];
        m_CalcValues = new double[maxY * maxX];
        m_CalcCounts = new int[maxY * maxX];
        // compute the track using the ordered data
        if (useNormalData) {
            rasterizeTracksAlgorithm.rasterize(tpi, raster, m_CalcValues,
                    m_CalcCounts, maxX, maxY);
        }
        // compute the track using the reversed data
        if (useReversedData) {
            tpi.reverse();
            rasterizeTracksAlgorithm.rasterize(tpi, raster, m_CalcValues,
                    m_CalcCounts, maxX, maxY);
        }
    }

    private void adjust(TrackPartIterable tpi2) {
        int sensorCol = tpi.getSensorCol();
        HashMap<Integer, ArrayList<IFeature>> sensorlines = new HashMap<Integer, ArrayList<IFeature>>();
        for (IFeature feature : tpi.getData()) {
            IRecord record = feature.getRecord();
            int sensor = Integer
                    .parseInt(record.getValue(sensorCol).toString()) - 1;
            ArrayList<IFeature> list = sensorlines.getOrDefault(sensor,
                    new ArrayList<IFeature>());
            list.add(feature);
            sensorlines.put(sensor, list);
        }

        // 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16
        // int[] offsets = new
        // int[]{0,25,25,25,25,11,11,11,11,11,11,19,19,19,19,19}; //counted at
        // the beginning
        // int[] offsets = new int[]{0,19,19,19,19, 9, 9, 9, 9, 9,
        // 9,14,14,14,14,14}; //counted somewhere in the white space
        int a = 0, b = 10, c = 20;
        int[] offsets = new int[] { 0, b, b, b, b, a, a, a, a, a, a, c, c, c,
                c, c }; // counted at the beginning
        offsets = computeOffsets(sensorlines);
        int[] sizes = new int[16];
        for (int sensor = 0; sensor < 16; sensor++) {
            ArrayList<IFeature> list = sensorlines.getOrDefault(sensor,
                    new ArrayList<IFeature>());
            for (int erase = 0; erase < offsets[sensor]; erase++) {
                list.remove(0);
            }
            for (int erase = offsets[sensor]; erase <= c; erase++) {
                list.remove(list.size() - 1);
            }
            sensorlines.put(sensor, list);
            sizes[sensor] = list.size();
        }
        Sextante.addInfoToLog("sizes:" + Arrays.toString(sizes));
        ArrayList<IFeature> features = new ArrayList<IFeature>();
        int size = sensorlines.get(0).size();
        for (int measurement = 0; measurement < size; measurement++) {
            for (int sensor = 1; sensor < 16; sensor++) {
                features.add(sensorlines.get(sensor).get(measurement));
            }
        }
        tpi.setData(features);
    }

    private int[] computeOffsets(
            HashMap<Integer, ArrayList<IFeature>> sensorlines) {
        int[] offsets = new int[sensorlines.size()];
        Coordinate pos00 = sensorlines.get(0).get(0).getGeometry()
                .getCoordinate();
        for (int i = 1; i < sensorlines.size(); i++) {
            ArrayList<IFeature> sensors = sensorlines.get(i);
            double dist = Double.MAX_VALUE;
            int sensor = 0;
            for (; sensor < sensorlines.size(); sensor++) {
                double newDist = sensors.get(sensor).getGeometry()
                        .getCoordinate().distance(pos00);
                if (newDist < dist) {
                    dist = newDist;
                } else {

                    break;
                }
            }
            offsets[i] = sensor - 1 > 0 ? sensor - 1 : 0;
        }
        return offsets;
    }

    /**
     * @param tpi
     *            this function interpolates along each sensor and fill up the
     *            missing datapoints
     */
    private static void preprocessData(TrackPartIterable tpi) {
        int timestampCol = tpi.getTimestampCol();
        int sensorCol = tpi.getSensorCol();
        int sensorCount = tpi.getSensorcount();
        int trackCol = tpi.getTrackCol();
        int valueCol = tpi.getValueCol();
        int track = -1;
        HashMap<Double, ArrayList<double[]>> data = new HashMap<Double, ArrayList<double[]>>();
        // Sextante.addInfoToLog("tpi.getData().size() before bootstraping"+tpi.getData().size());
        for (IFeature feature : tpi.getData()) {
            IRecord record = feature.getRecord();
            double timestamp = Double.parseDouble(record.getValue(timestampCol)
                    .toString());
            double value = Double.parseDouble(record.getValue(valueCol)
                    .toString());
            ArrayList<double[]> entries = data.getOrDefault(timestamp,
                    new ArrayList<double[]>());
            int sensor = Integer
                    .parseInt(record.getValue(sensorCol).toString()) - 1;
            if (track == -1) {
                track = Integer.parseInt(record.getValue(trackCol).toString());
            }
            while (entries.size() < sensor) {
                entries.add(null);
            }
            entries.add(new double[] { timestamp, sensor + 1, value,
                    feature.getGeometry().getCentroid().getX(),
                    feature.getGeometry().getCentroid().getY() });

            data.put(timestamp, entries);
        }

        ArrayList<Double> keyTimestamps = new ArrayList<Double>(data.keySet());

        for (Double keyTimestamp : keyTimestamps) {

            ArrayList<double[]> entries = data.getOrDefault(keyTimestamp,
                    new ArrayList<double[]>());
            while (entries.size() < sensorCount) {
                entries.add(null);
            }
            data.put(keyTimestamp, entries);
        }
        double minDiff = Double.MAX_VALUE;
        for (int y = 0; y < keyTimestamps.size() - 1; y++) {
            minDiff = Math.min(keyTimestamps.get(y + 1) - keyTimestamps.get(y),
                    minDiff);
        }
        Collections.sort(keyTimestamps);
        for (int x = 0; x < sensorCount; x++) {
            double lastKey = -1;
            // Sextante.addInfoToLog("fix Sensorline:" +x);
            for (int y = 0; y < keyTimestamps.size(); y++) {
                double curKey = keyTimestamps.get(y);
                if (data.get(curKey).get(x) == null && lastKey > 0) {
                    int y2 = y;
                    double nextKey = keyTimestamps.get(y2);
                    for (; y2 < keyTimestamps.size(); y2++) {
                        nextKey = keyTimestamps.get(y2);
                        if (data.get(nextKey).get(x) != null)
                            break;
                    }
                    // Sextante.addInfoToLog("check for linearRepair");

                    if (y2 != y && y2 < keyTimestamps.size()
                            && nextKey - curKey < 20 * minDiff) {
                        data = linearRepair(x, y - 1, y2, keyTimestamps, data);
                    }
                    y = y2;
                } else if (data.get(curKey).get(x) != null) {
                    lastKey = curKey;
                }
            }
        }
        ArrayList<IFeature> features = new ArrayList<IFeature>();
        for (int y = 0; y < keyTimestamps.size(); y++) {
            final double curKey = keyTimestamps.get(y);
            for (int x = 0; x < sensorCount; x++) {
                if (data.get(curKey).get(x) == null)
                    continue;
                final Object[] object = new Object[4];
                object[trackCol] = "" + track;
                object[timestampCol] = "" + data.get(curKey).get(x)[0];
                object[sensorCol] = "" + ((int) data.get(curKey).get(x)[1]);
                object[valueCol] = "" + data.get(curKey).get(x)[2];
                final double posx = data.get(curKey).get(x)[3];
                final double posy = data.get(curKey).get(x)[4];
                features.add(new IFeature() {

                    IRecord record = new IRecord() {

                        @Override
                        public Object[] getValues() {
                            // TODO Auto-generated method stub
                            return object;
                        }

                        @Override
                        public Object getValue(int iField) {
                            return object[iField];
                        }
                    };

                    @Override
                    public IRecord getRecord() {
                        return record;
                    }

                    Geometry geo = geometryFactory.createPoint(new Coordinate(
                            posx, posy));

                    @Override
                    public Geometry getGeometry() {
                        // TODO Auto-generated method stub
                        return geo;
                    }
                });
            }
        }
        tpi.setData(features);
        // Sextante.addInfoToLog("tpi.getData().size() after bootstraping"+tpi.getData().size());
    }

    /**
     * this function interpolates between y and y2 on the given sensor x
     * 
     * @param x
     * @param y
     * @param y2
     * @param keyTimestamps
     *            - contains the keys for the data
     * @param data
     *            - contains a mapping from timestamp to sensors line values
     * @return the new data(base) of sensors, containing the interpolated ones
     */
    private static HashMap<Double, ArrayList<double[]>> linearRepair(int x,
            int y, int y2, List<Double> keyTimestamps,
            HashMap<Double, ArrayList<double[]>> data) {
        for (int i = 1; i < y2 - y; i++) {
            double keyBegin = keyTimestamps.get(y);
            double[] begin = data.get(keyBegin).get(x);
            double keyEnd = keyTimestamps.get(y2);
            double[] end = data.get(keyEnd).get(x);
            double key = keyTimestamps.get(y + i);
            double[] entry = new double[end.length];

            for (int pos = 2; pos < entry.length; pos++) {
                entry[pos] = (1 - (i / (double) (y2 - y))) * begin[pos]
                        + (i / (double) (y2 - y)) * end[pos];
            }
            entry[0] = key;
            entry[1] = begin[1];
            data.get(key).set(x, entry);
        }
        return data;
    }

    /**
     * should be called after run this will write the computes data from the
     * buffer using the computed offset to the resulting Layer
     * 
     * @param m_Result
     */
    public void writeResult(IRasterLayer m_Result) {
        // Sextante.addInfoToLog("write data to raster of size "+m_CalcCounts[0].length+" x "+m_CalcCounts.length);
        for (int i = 0; i < m_fillgap; i++) {
            for (int y = 1; y < maxY - 1; y++) {
                for (int x = 1; x < maxX - 1; x++) {
                    if (m_CalcCounts[y * maxX + x] == 0) {

                        int neighbours = m_CalcCounts[(y - 1) * maxX + (x - 1)]
                                + m_CalcCounts[(y - 1) * maxX + (x)]
                                + m_CalcCounts[(y - 1) * maxX + (x + 1)]
                                + m_CalcCounts[(y) * maxX + (x - 1)]
                                + m_CalcCounts[(y) * maxX + (x + 1)]
                                + m_CalcCounts[(y + 1) * maxX + (x - 1)]
                                + m_CalcCounts[(y + 1) * maxX + (x)]
                                + m_CalcCounts[(y + 1) * maxX + (x + 1)];
                        double neighboursVal = m_CalcValues[(y - 1) * maxX
                                + (x - 1)]
                                + m_CalcValues[(y - 1) * maxX + (x)]
                                + m_CalcValues[(y - 1) * maxX + (x + 1)]
                                + m_CalcValues[(y) * maxX + (x - 1)]
                                + m_CalcValues[(y) * maxX + (x + 1)]
                                + m_CalcValues[(y + 1) * maxX + (x - 1)]
                                + m_CalcValues[(y + 1) * maxX + (x)]
                                + m_CalcValues[(y + 1) * maxX + (x + 1)];
                        if (neighbours > 5) {
                            m_CalcValues[y * maxX + x] += neighboursVal
                                    / neighbours;
                            m_CalcCounts[y * maxX + x] = 1;
                        }
                    }
                }
            }
        }

        for (int y = 0; y < maxY; y++) {
            for (int x = 0; x < maxX; x++) {
                if (m_CalcCounts[y * maxX + x] > 0) {
                    double value = m_Result.getCellValueAsDouble(x - xoffset, y
                            - yoffset);
                    if (m_Result.isNoDataValue(value)) {
                        m_Result.setCellValue(x - xoffset, y - yoffset,
                                m_CalcValues[y * maxX + x]
                                        / m_CalcCounts[y * maxX + x]);
                    } else {
                        m_Result.setCellValue(x - xoffset, y - yoffset,
                                ((m_CalcValues[y * maxX + x] / m_CalcCounts[y
                                        * maxX + x]) + value) / 2.0);
                    }
                }
            }
        }
        m_CalcValues = null;
        m_CalcCounts = null;
    }

}
