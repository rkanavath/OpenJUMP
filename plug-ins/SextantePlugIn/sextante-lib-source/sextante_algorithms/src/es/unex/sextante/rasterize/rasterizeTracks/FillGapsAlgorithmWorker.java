package es.unex.sextante.rasterize.rasterizeTracks;

import java.awt.Rectangle;
import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.triangulate.DelaunayTriangulationBuilder;

import es.unex.sextante.core.AnalysisExtent;
import es.unex.sextante.core.GeoAlgorithm;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.rasterWrappers.GridCell;

public class FillGapsAlgorithmWorker implements Runnable {
    private final static Coordinate[] EMPTY_COORDINATES = new Coordinate[0];
    private static final double BARYCENTRIC_CUTOFF = 0;
    private static GeometryFactory geometryFactory = new GeometryFactory();
    private IRasterLayer m_Result;
    private int fillGap;
    private AnalysisExtent m_Extent;
    private RasterizeTracksAlgorithm rasterizeTracksAlgorithm;
    boolean m_bQuickCheck;
    private int m_iThreads;
    private int m_iInterpolationMethod;
    private double m_dLowCut;
    private double m_dHighCut;
    private int m_iSuperSamplingWindowSize;
    private volatile int curCalc;
    private volatile int maxCalc;
    private double m_dDetAreaCutoff;

    public FillGapsAlgorithmWorker(int m_iFillGap, IRasterLayer result,
            AnalysisExtent m_Extent, int iThreads,
            RasterizeTracksAlgorithm rasterizeTracksAlgorithm,
            boolean quickCheck, int interpolationMethod, double lowCut,
            double highCut, int superSamplingWindowSize, double detAreaCutoff) {
        this.fillGap = m_iFillGap;
        this.m_Result = result;
        this.m_Extent = m_Extent;
        this.rasterizeTracksAlgorithm = rasterizeTracksAlgorithm;
        m_bQuickCheck = quickCheck;
        m_iThreads = iThreads;
        m_iInterpolationMethod = interpolationMethod;
        m_dLowCut = lowCut;
        m_dHighCut = highCut;
        m_iSuperSamplingWindowSize = superSamplingWindowSize;
        m_dDetAreaCutoff = detAreaCutoff;
    }

    @Override
    public void run() {
        /*
         * data = new double[ymax][xmax]; double[][] data2 = new
         * double[ymax][xmax]; int[][] count = new int[ymax][xmax]; for (int y =
         * ymin; y < ymax; y++) { for (int x = xmin; x < xmax; x++) {
         * data[y][x]=m_Result.getCellValueAsDouble(x, y); } }
         */
        fillBetweenPaths3();

        // Sextante.addInfoToLog("an filling process has been finished");
    }

    private void fillBetweenPaths3() {

        File f = new File(GeoAlgorithm.getTempFolder()
                + "\\RasterizeTracksAlgorithm\\tmp");
        final File[] files = f.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.endsWith(".tmp");
            }
        });
        Sextante.addInfoToLog("Fillgap directory(" + f.getAbsolutePath()
                + ") exists: " + f.exists());
        /*
         * Arrays.sort(files, new Comparator<File>() {
         * 
         * @Override public int compare(File o1, File o2) { return
         * -Long.compare(o1.length(), o2.length()); } });
         */
        Sextante.addInfoToLog("Fillgap directory(" + f.getAbsolutePath()
                + ") contains: " + files.length + " files");
        final double maxDist = Math.min(m_Extent.getCellSizeX(),
                m_Extent.getCellSizeY())
                * fillGap;
        List<Runnable> runables = new ArrayList<Runnable>();
        curCalc = 0;
        maxCalc = (files.length * files.length) / 2;
        // Sextante.addInfoToLog("m_Extent.getCellSize("+m_Extent.getCellSize()+") * fillGap("+fillGap+") = maxDist("+maxDist+")");
        rasterizeTracksAlgorithm.setProgress(0, maxCalc);
        for (int i = 0; i < files.length - 1; i++) {
            final int trackNr1 = i;
            // Sextante.addInfoToLog("compute Pairs with"+files[i]);
            runables.add(new Runnable() {

                @Override
                public void run() {
                    // TODO Auto-generated method stub
                    // Sextante.addInfoToLog("compute Pairs with"+files[trackNr1]);
                    Map<Coordinate, Double> track1 = loadTrack(files[trackNr1]);
                    MultiPoint multiPointTrack1 = geometryFactory
                            .createMultiPoint(track1.keySet().toArray(
                                    EMPTY_COORDINATES));
                    MTRACK: for (int j = trackNr1 + 1; j < files.length; j++) {
                        ++curCalc;
                        if (curCalc % 100 == 0) {
                            rasterizeTracksAlgorithm.setProgress(curCalc,
                                    maxCalc);
                        }
                        // final int trackNr1 = i;
                        final int trackNr2 = j;

                        Map<Coordinate, Double> track2 = loadTrack(files[trackNr2]);

                        MultiPoint multiPointTrack2 = geometryFactory
                                .createMultiPoint(track2.keySet().toArray(
                                        EMPTY_COORDINATES));
                        // quickcheck of bounds
                        int[] raster1 = null;
                        int[] raster2 = null;
                        if (m_bQuickCheck) {
                            raster1 = computeRasterbounds(track1.keySet());
                            raster2 = computeRasterbounds(track2.keySet());
                            Rectangle r1 = new Rectangle(raster1[0],
                                    raster1[1], raster1[2] - raster1[0],
                                    raster1[3] - raster1[1]);
                            Rectangle r2 = new Rectangle(raster2[0],
                                    raster2[1], raster2[2] - raster2[0],
                                    raster2[3] - raster2[1]);
                            r1.grow(fillGap, fillGap);
                            // r2.grow(fillGap, fillGap);
                            if (!r1.intersects(r2))
                                continue MTRACK;
                        }
                        boolean inDistance = false;

                        // detailed check of bounds
                        M1: for (Coordinate c1 : track1.keySet()) {
                            for (Coordinate c2 : track2.keySet()) {
                                if (c1.distance(c2) < maxDist) {
                                    inDistance = true;
                                    break M1;
                                }
                            }
                        }
                        if (!inDistance)
                            continue MTRACK;

                        // combine and raster data
                        track2.putAll(track1);
                        Geometry triangles = delaunayTriangulation(
                                multiPointTrack1, multiPointTrack2);
                        // Sextante.addInfoToLog("gap between "+files[trackNr1]+" and "+files[trackNr2]+" should be rastered");
                        if (m_bQuickCheck) {
                            int[] bounds = new int[] {
                                    Math.min(raster1[0], raster2[0]),
                                    Math.min(raster1[1], raster2[1]),
                                    Math.max(raster1[2], raster2[2]),
                                    Math.max(raster1[3], raster2[3]) };
                            rasterizeTriangles(maxDist, track2, triangles,
                                    bounds);
                        } else {
                            rasterizeTriangles(maxDist, track2, triangles,
                                    computeRasterbounds(track2.keySet()));
                        }

                    }

                }
            });
            // if (!rasterizeTracksAlgorithm.getTask().isCanceled() &&
            // runables.size() >= m_iThreads) {
            // Sextante.addInfoToLog("!rasterizeTracksAlgorithm.getTask().isCanceled()?"+!rasterizeTracksAlgorithm.getTask().isCanceled()+"runables.size()("+runables.size()+") >= m_iThreads*10("+(m_iThreads*10)+")?"+(runables.size()
            // >= m_iThreads*10));
            if (!rasterizeTracksAlgorithm.getTask().isCanceled()
                    && runables.size() >= m_iThreads * 10) {
                executeRunnables(runables);
            }

        }
        executeRunnables(runables);
        // Sextante.addInfoToLog(""+curCalc+" / "+maxCalc);
        // rasterizeTracksAlgorithm.setProgress(curCalc, maxCalc);
    }

    private int executeRunnables(List<Runnable> runables) {
        if (m_iThreads < 2) {
            while (!runables.isEmpty()) {
                runables.remove(0).run();
            }
        } else {

            List<Thread> threads = new ArrayList<Thread>();
            for (int i = 0; i < m_iThreads; i++) {
                if (runables.isEmpty())
                    break;
                Thread t = new Thread(runables.remove(0));
                threads.add(t);
                t.start();
            }
            while (!runables.isEmpty()) {
                if (rasterizeTracksAlgorithm.getTask().isCanceled())
                    return curCalc;
                try {
                    Thread t = threads.remove(0);
                    t.join();

                    Thread t2 = new Thread(runables.remove(0));
                    t2.start();
                    threads.add(t2);

                } catch (InterruptedException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }

            }
            while (!threads.isEmpty()) {
                if (rasterizeTracksAlgorithm.getTask().isCanceled())
                    return curCalc;
                try {
                    threads.remove(0).join();
                } catch (InterruptedException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
        return curCalc;
    }

    public final static Geometry delaunayTriangulation(MultiPoint multipoint,
            MultiPoint multipoint2) {
        DelaunayTriangulationBuilder triangulationBuilder = new DelaunayTriangulationBuilder();
        Geometry mp3 = multipoint.union(multipoint2);
        triangulationBuilder.setSites(new GeometryCollection(
                new Geometry[] { mp3 }, geometryFactory));
        triangulationBuilder.setTolerance(0);
        Geometry triangles = triangulationBuilder.getTriangles(geometryFactory);
        return triangles;
    }

    public final static Geometry delaunayTriangulation(MultiPoint multipoint) {
        DelaunayTriangulationBuilder triangulationBuilder = new DelaunayTriangulationBuilder();
        triangulationBuilder.setSites(new GeometryCollection(
                new Geometry[] { multipoint }, geometryFactory));
        triangulationBuilder.setTolerance(0);
        Geometry triangles = triangulationBuilder.getTriangles(geometryFactory);
        return triangles;
    }

    /**
     * 1) computes at first the pixelsize for the given coordinates 2) creates a
     * buffer with the calculated size 3) iterates through the given triangles
     * and 3.1)
     * 
     * @param maxDist
     * @param pairTrackMapping
     * @param triangles
     */
    private void rasterizeTriangles(final double maxDist,
            Map<Coordinate, Double> pairTrackMapping, Geometry triangles,
            int[] raster) {
        // int[] raster = computeRasterbounds(pairTrackMapping.keySet());
        // Sextante.addInfoToLog("rasterizeTriangles");
        int xoffset = -raster[0];
        int yoffset = -raster[1];
        raster[0] = xoffset;
        raster[1] = yoffset;
        raster[2] = raster[2] + xoffset;
        raster[3] = raster[3] + yoffset;
        int maxX = raster[2];
        int maxY = raster[3];
        double[][] m_CalcValues = new double[maxY][maxX];
        int[][] m_CalcCounts = new int[maxY][maxX];

        double dist01, dist12, dist20;
        for (int triangle = 0; triangle < triangles.getNumGeometries(); triangle++) {
            Coordinate[] cs = triangles.getGeometryN(triangle).getCoordinates();
            if (pairTrackMapping.get(cs[0]) == null
                    || pairTrackMapping.get(cs[1]) == null
                    || pairTrackMapping.get(cs[2]) == null)
                continue;

            // Manhattan distance
            dist01 = Math.abs(cs[0].x - cs[1].x) + Math.abs(cs[0].y - cs[1].y);
            dist12 = Math.abs(cs[1].x - cs[2].x) + Math.abs(cs[1].y - cs[2].y);
            dist20 = Math.abs(cs[2].x - cs[0].x) + Math.abs(cs[2].y - cs[0].y);
            if (dist01 < maxDist && dist12 < maxDist && dist20 < maxDist) {
                barycentricInterpolation(cs, pairTrackMapping, new int[] {
                        xoffset, yoffset }, m_CalcValues, m_CalcCounts);
            }
        }
        for (int i = 0; i < 5; i++) {
            for (int y = 1; y < m_CalcCounts.length - 1; y++) {
                for (int x = 1; x < m_CalcCounts[0].length - 1; x++) {
                    if (m_CalcCounts[y][x] == 0) {
                        int neighbours = m_CalcCounts[y - 1][x - 1]
                                + m_CalcCounts[y - 1][x]
                                + m_CalcCounts[y - 1][x + 1]
                                + m_CalcCounts[y][x - 1] + m_CalcCounts[y][x]
                                + m_CalcCounts[y][x + 1]
                                + m_CalcCounts[y + 1][x - 1]
                                + m_CalcCounts[y + 1][x]
                                + m_CalcCounts[y + 1][x + 1];
                        double neighboursVal = m_CalcValues[y - 1][x - 1]
                                + m_CalcValues[y - 1][x]
                                + m_CalcValues[y - 1][x + 1]
                                + m_CalcValues[y][x - 1] + m_CalcValues[y][x]
                                + m_CalcValues[y][x + 1]
                                + m_CalcValues[y + 1][x - 1]
                                + m_CalcValues[y + 1][x]
                                + m_CalcValues[y + 1][x + 1];
                        if (neighbours > 5) {
                            m_CalcValues[y][x] += neighboursVal / neighbours;
                            m_CalcCounts[y][x] = 1;
                        }
                    }
                }
            }
        }
        // Write the Data directly to the Result, if there is no data yet
        // (surpress overwritting trackwise rastered data)
        for (int y = 0; y < m_CalcCounts.length; y++) {
            for (int x = 0; x < m_CalcCounts[0].length; x++) {
                if (m_CalcCounts[y][x] > 0) {
                    double value = m_Result.getCellValueAsDouble(x - xoffset, y
                            - yoffset);
                    if (m_Result.isNoDataValue(value)) {
                        m_Result.setCellValue(x - xoffset, y - yoffset,
                                m_CalcValues[y][x] / m_CalcCounts[y][x]);
                    }
                }
            }
        }
    }

    /**
     * Reads a binary File of Sensordata, with the structure
     * x(double),y(double),value(double),x(double),y(double),value(double),...
     * 
     * @param inputFile
     *            - the File containing the data
     * @return a mapping from Coordinates to the Sensorvalue on the given point
     */
    private final static Map<Coordinate, Double> loadTrack(File inputFile) {
        try {
            Map<Coordinate, Double> track = new HashMap<Coordinate, Double>();
            FileInputStream fis = new FileInputStream(inputFile);
            ByteBuffer buffer = ByteBuffer.wrap(Files.readAllBytes(inputFile
                    .toPath()));
            while (buffer.hasRemaining()) {
                track.put(
                        new Coordinate(buffer.getDouble(), buffer.getDouble()),
                        buffer.getDouble());
            }
            buffer.clear();
            fis.close();
            return track;
        } catch (IOException e) {
            // TODO Auto-generated catch block
            Sextante.addErrorToLog(e);
            e.printStackTrace();
        }
        return null;
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

    /**
     * computes the bounds for rasterizing of a track given as an Array of
     * coordinates
     * 
     * @param coordinates
     *            of the given track
     * @return an int[4] { minX, minY, maxX, maxY};
     */
    public int[] computeRasterbounds(Collection<Coordinate> coords) {
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

    public int[] barycentricInterpolation(Coordinate[] cs,
            Map<Coordinate, Double> map, int[] trackRasterOffset,
            double[][] calcValues, int[][] calcCounts) {
        /*
         * int maxX = calcValues[0].length; int maxY = calcValues.length;
         * BarycentricInterpolationKernel5 bik5 = new
         * BarycentricInterpolationKernel5(m_Extent.getCellSizeX(),
         * m_Extent.getCellSizeY(), m_Extent.getXMin(), m_Extent.getYMax(),
         * m_dHighCut, m_dLowCut); double[] itensities = new double[cs.length];
         * for (int i = 0; i < itensities.length; i++) {
         * itensities[i]=map.get(cs[i]); } double[] tmpCalcValues = new
         * double[maxX*maxY]; int[] tmpCalcCounts = new int[maxX*maxY];
         * rasterizeTracksAlgorithm.barycentricInterpolation4GPU_2(cs,
         * itensities, trackRasterOffset, tmpCalcValues, tmpCalcCounts, maxX,
         * maxY, bik5 ); for(int x=0; x<maxX; x++){ for(int y=0; y<maxY; y++){
         * calcValues[y][x]+=tmpCalcValues[y*maxX+x];
         * calcCounts[y][x]+=tmpCalcCounts[y*maxX+x]; } } return null;
         */

        switch (m_iInterpolationMethod) {
        case 0:
            return barycentricInterpolation2(cs, map, trackRasterOffset,
                    calcValues, calcCounts);
        case 1:
            return barycentricInterpolation3(cs, map, trackRasterOffset,
                    calcValues, calcCounts);
        case 2:
            barycentricInterpolation2(cs, map, trackRasterOffset, calcValues,
                    calcCounts);
            barycentricInterpolation2(cs, map, trackRasterOffset, calcValues,
                    calcCounts);
            barycentricInterpolation2(cs, map, trackRasterOffset, calcValues,
                    calcCounts);
            barycentricInterpolation2(cs, map, trackRasterOffset, calcValues,
                    calcCounts);
            barycentricInterpolation2(cs, map, trackRasterOffset, calcValues,
                    calcCounts);
            return barycentricInterpolation2(cs, map, trackRasterOffset,
                    calcValues, calcCounts);
        default:
            barycentricInterpolation2(cs, map, trackRasterOffset, calcValues,
                    calcCounts);
            barycentricInterpolation2(cs, map, trackRasterOffset, calcValues,
                    calcCounts);
            barycentricInterpolation2(cs, map, trackRasterOffset, calcValues,
                    calcCounts);
            barycentricInterpolation2(cs, map, trackRasterOffset, calcValues,
                    calcCounts);
            barycentricInterpolation2(cs, map, trackRasterOffset, calcValues,
                    calcCounts);
            return barycentricInterpolation2(cs, map, trackRasterOffset,
                    calcValues, calcCounts);
        }
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

    public int[] barycentricInterpolation2(Coordinate[] coords,
            Map<Coordinate, Double> map, int[] rasteroffset,
            double[][] calcValues, int[][] calcCounts) {
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
        if (scaledDetArea > m_dDetAreaCutoff) {
            Sextante.addInfoToLog(scaledDetArea + ">" + m_dDetAreaCutoff);
            return raster;
        }
        double a = Math
                .max(Math.min(map.get(coords[0]), m_dHighCut), m_dLowCut);
        double b = Math
                .max(Math.min(map.get(coords[1]), m_dHighCut), m_dLowCut);
        double c = Math
                .max(Math.min(map.get(coords[2]), m_dHighCut), m_dLowCut);
        int halfwindowssize = m_iSuperSamplingWindowSize;
        double windowStep = Math.min(m_Extent.getCellSizeX(),
                m_Extent.getCellSizeY())
                / (2 * halfwindowssize);
        for (int x = raster[0]; x < raster[2]; x++) {
            for (int y = raster[1]; y < raster[3]; y++) {
                Coordinate dscaled = m_Extent.getWorldCoordsFromGridCoords(x,
                        y, 0);
                double total = 0;
                int count = 0;
                for (int dx = -halfwindowssize; dx <= halfwindowssize; dx++) {
                    for (int dy = -halfwindowssize; dy <= halfwindowssize; dy++) {
                        double centerx = ((dscaled.x + dx * windowStep) + computedOffsets[0])
                                / computedOffsets[2];
                        double centery = ((dscaled.y + dy * windowStep) + computedOffsets[1])
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

    public int[] barycentricInterpolation3(Coordinate[] coords,
            Map<Coordinate, Double> map, int[] rasteroffset,
            double[][] calcValues, int[][] calcCounts) {
        Arrays.sort(coords);
        int[] raster = computeRasterbounds(coords);
        double detArea = (coords[0].x - coords[2].x)
                * (coords[1].y - coords[2].y) - (coords[1].x - coords[2].x)
                * (coords[0].y - coords[2].y);
        double a = Math
                .max(Math.min(map.get(coords[0]), m_dHighCut), m_dLowCut);
        double b = Math
                .max(Math.min(map.get(coords[1]), m_dHighCut), m_dLowCut);
        double c = Math
                .max(Math.min(map.get(coords[2]), m_dHighCut), m_dLowCut);
        int halfwindowssize = m_iSuperSamplingWindowSize;
        double windowStep = Math.min(m_Extent.getCellSizeX(),
                m_Extent.getCellSizeY())
                / (2 * halfwindowssize);
        for (int x = raster[0]; x < raster[2]; x++) {
            for (int y = raster[1]; y < raster[3]; y++) {
                Coordinate dscaled = m_Extent.getWorldCoordsFromGridCoords(x,
                        y, 0);
                double total = 0;
                int count = 0;
                for (int dx = -halfwindowssize; dx <= halfwindowssize; dx++) {
                    for (int dy = -halfwindowssize; dy <= halfwindowssize; dy++) {

                        double centerx = (dscaled.x + dx * windowStep);
                        double centery = (dscaled.y + dy * windowStep);
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

        Coordinate tmp = coords[0];
        coords[0] = coords[2];
        coords[2] = tmp;
        detArea = (coords[0].x - coords[2].x) * (coords[1].y - coords[2].y)
                - (coords[1].x - coords[2].x) * (coords[0].y - coords[2].y);
        a = Math.max(Math.min(map.get(coords[0]), m_dHighCut), m_dLowCut);
        b = Math.max(Math.min(map.get(coords[1]), m_dHighCut), m_dLowCut);
        c = Math.max(Math.min(map.get(coords[2]), m_dHighCut), m_dLowCut);
        halfwindowssize = m_iSuperSamplingWindowSize;
        windowStep = Math.min(m_Extent.getCellSizeX(), m_Extent.getCellSizeY())
                / (2 * halfwindowssize);
        for (int x = raster[0]; x < raster[2]; x++) {
            for (int y = raster[1]; y < raster[3]; y++) {
                Coordinate dscaled = m_Extent.getWorldCoordsFromGridCoords(x,
                        y, 0);
                double total = 0;
                int count = 0;
                for (int dx = -halfwindowssize; dx <= halfwindowssize; dx++) {
                    for (int dy = -halfwindowssize; dy <= halfwindowssize; dy++) {
                        double centerx = (dscaled.x + dx * windowStep);
                        double centery = (dscaled.y + dy * windowStep);
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

    public static void main(String[] args) {
        Rectangle r = new Rectangle(0, 0, 10, 10);
        Rectangle r2 = new Rectangle(2, 0, 2, 2);
        r.grow(5, 5);

        // r2.grow(5, 5);
        System.out.println(r.intersects(r2));
        System.out.println(r2.intersects(r));
        Sensor[] s = new Sensor[10];
        Sensor[] s2 = new Sensor[10];
        for (int i = 0; i < s.length; i++) {
            s[i] = new Sensor(20, i * 5, Math.random());
            s2[i] = new Sensor(40, i * 5, Math.random());
        }
        MultiPoint mp = geometryFactory.createMultiPoint(s);
        MultiPoint mp2 = geometryFactory.createMultiPoint(s2);
        Geometry triangles = delaunayTriangulation(mp, mp2);
        for (int i = 0; i < s.length; i++) {
            Coordinate[] cs = triangles.getGeometryN(i).getCoordinates();
            System.out.println(Arrays.toString(cs));
        }
    }
}
