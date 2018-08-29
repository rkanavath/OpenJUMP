package com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem;

import it.geomaticaeambiente.klem.BaseflowParams.BaseflowType;
import it.geomaticaeambiente.klem.Hyetograph;
import it.geomaticaeambiente.klem.HyetographGenerator.HyetographShape;
import it.geomaticaeambiente.klem.InitialAbstraction;
import it.geomaticaeambiente.klem.InitialAbstraction.AbstractionUnits;
import it.geomaticaeambiente.klem.Klem;
import it.geomaticaeambiente.klem.LsppCalculator;
import it.geomaticaeambiente.klem.LsppCalculator.LsppModel;
import it.geomaticaeambiente.klem.TimeInterval;
import it.geomaticaeambiente.klem.Watershed;

import java.awt.geom.NoninvertibleTransformException;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import javax.xml.parsers.ParserConfigurationException;

import org.openjump.core.rasterimage.RasterImageIO;
import org.openjump.core.rasterimage.TiffTags;

import com.geomaticaeambiente.klemgui.exceptions.WarningException;
import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem.KlemProperties.Amc_Type;
import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem.KlemProperties.KinematicsType;
import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem.KlemProperties.RainfallType;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.GeometryUtils;
import com.geomaticaeambiente.klemgui.utils.HydroUtils;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.geomaticaeambiente.klemgui.utils.RasterUtils;
import com.geomaticaeambiente.klemgui.utils.ShapefileUtils;
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsCalculator;
import com.geomaticaeambiente.openjump.klem.flowdir.FlowDirsStripe;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.rastertools.RasterAverager;
import com.geomaticaeambiente.openjump.klem.rastertools.RasterClipper;
import com.geomaticaeambiente.openjump.klem.routing.RoutingTimeAdvancedParameters;
import com.geomaticaeambiente.openjump.klem.routing.RoutingTimeCalculator;
import com.geomaticaeambiente.openjump.klem.routing.RoutingTimeParameters;
import com.geomaticaeambiente.openjump.klem.units.Area;
import com.geomaticaeambiente.openjump.klem.units.Length;
import com.geomaticaeambiente.openjump.klem.watersheds.WatershedExtractor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 *
 * @author AdL
 */
public class KlemUtils {

    public static void checkParams(ComponentsTreeMap componentsWithActions,
            KlemProperties klemProps, RainfallType rainfallType)
            throws ParserConfigurationException, IOException, WarningException,
            Exception {

        // check and centerButton coordinate, area and elevation
        final double xCoord = GUIUtils.getDoubleValue(componentsWithActions
                .getComponent("02", GUIUtils.OTHER, 1));
        final double yCoord = GUIUtils.getDoubleValue(componentsWithActions
                .getComponent("03", GUIUtils.OTHER, 1));
        final double area = GUIUtils.getDoubleValue(componentsWithActions
                .getComponent("04", GUIUtils.OTHER, 1));
        final double elevation = GUIUtils.getDoubleValue(componentsWithActions
                .getComponent("05", GUIUtils.OTHER, 1));

        klemProps.setOutletXCoord(xCoord);
        klemProps.setOutletYCoord(yCoord);

        if (area < 0.01 || area > 400) {

            throw new WarningException(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.BasinArea.label")
                    + ": "
                    + PluginUtils.getFourDecimalFormatToString(area)
                    + "\n"
                    + PluginUtils.getResources().getString(
                            "HydrographKlemPlugin.CheckArea.message1"));
        }

        klemProps.setCatchmentArea(area);
        klemProps.setOutletElevation(elevation);

        // check and centerButton rainfall information
        switch (rainfallType) {
        case POINT: {
            final String aS = GUIUtils.getStringValue(componentsWithActions
                    .getComponent("09", GUIUtils.OTHER, 1));
            final String nS = GUIUtils.getStringValue(componentsWithActions
                    .getComponent("10", GUIUtils.OTHER, 1));
            final String nLessHourS = GUIUtils
                    .getStringValue(componentsWithActions.getComponent("17",
                            GUIUtils.OTHER, 1));

            if (aS.equals("") || nS.equals("") || nLessHourS.equals("")) {
                throw new WarningException(
                        PluginUtils
                                .getResources()
                                .getString(
                                        "HydrographKlemPlugin.CheckLSPPParams.msgInvalidParams"));
            }

            double aD;
            double nD;
            double nLessHourD = 0.3;

            try {
                aD = Double.parseDouble(aS);
                nD = Double.parseDouble(nS);
                if (!nLessHourS.equalsIgnoreCase("auto")) {
                    nLessHourD = Double.parseDouble(nLessHourS);
                }
            } catch (final Exception ex) {
                throw new WarningException(
                        PluginUtils
                                .getResources()
                                .getString(
                                        "HydrographKlemPlugin.CheckLSPPParams.msgInvalidParams"));
            }

            if (aD < 30 || aD > 120 || nD <= 0 || nD > 1) {
                throw new WarningException(
                        PluginUtils
                                .getResources()
                                .getString(
                                        "HydrographKlemPlugin.CheckLSPPParams.msgInvalidParams"));
            }

            klemProps.setAParam(aD);
            klemProps.setNParam(nD);

            if (nLessHourS.equalsIgnoreCase("auto")) {
                klemProps.setnLessHourParam(klemProps.getAutoNLessHour());
                klemProps.setnLessHourParamAuto(true);
            } else {
                klemProps.setnLessHourParam(nLessHourD);
                klemProps.setnLessHourParamAuto(false);
            }

            break;
        }
        case DISTRIBUTED: {

            final LsppModel lsppModel = klemProps.getLsppModel();
            if (lsppModel == null) {
                throw new WarningException(PluginUtils.getResources()
                        .getString(
                                "HydrographKlemPlugin.DistributedModel.label"));
            } else if (lsppModel == LsppCalculator.LsppModel.GUMBEL
                    || lsppModel == LsppCalculator.LsppModel.GEV) {

                final String aFile = GUIUtils
                        .getStringValue(componentsWithActions.getComponent(
                                "12", GUIUtils.OTHER, 1));
                final String nFile = GUIUtils
                        .getStringValue(componentsWithActions.getComponent(
                                "13", GUIUtils.OTHER, 1));
                final String cvFile = GUIUtils
                        .getStringValue(componentsWithActions.getComponent(
                                "14", GUIUtils.OTHER, 1));
                GUIUtils.checkStringValue(aFile, ParamsTab.FILE_A);
                GUIUtils.checkStringValue(nFile, ParamsTab.FILE_N);
                GUIUtils.checkStringValue(cvFile, ParamsTab.FILE_CV);

                final int returnPeriod = GUIUtils
                        .getIntValue(componentsWithActions.getComponent("15",
                                GUIUtils.OTHER, 1));
                final String nLessHour = GUIUtils
                        .getStringValue(componentsWithActions.getComponent(
                                "17", GUIUtils.OTHER, 1));

                klemProps.setLsppModel(lsppModel);

                // check if files exist
                if ((!new File(aFile).exists() || !new File(nFile).exists() || !new File(
                        cvFile).exists())) {
                    throw new WarningException(
                            "HydrographKlemPlugin.CheckDistributedFile.label");
                }

                klemProps.setAFile(aFile);
                klemProps.setNFile(nFile);
                klemProps.setCvFile(cvFile);

                if (nLessHour.equalsIgnoreCase("auto")) {
                    klemProps.setnLessHourParam(klemProps.getAutoNLessHour());
                    klemProps.setnLessHourParamAuto(true);
                } else {
                    klemProps.setnLessHourParam(Double.parseDouble(nLessHour));
                    klemProps.setnLessHourParamAuto(false);
                }

                klemProps.setReturnPeriod(returnPeriod);

                break;
            }
        }
        // klemProps.setLsppModel(distributedType);

        case HISTORICAL: {
            final String historicalFile = GUIUtils
                    .getStringValue(componentsWithActions.getComponent("16",
                            GUIUtils.OTHER, 1));
            GUIUtils.checkStringValue(historicalFile,
                    ParamsTab.HISTORICAL_RAINFALL);

            // check if file exist
            if ((!new File(historicalFile).exists())) {
                throw new WarningException(
                        "HydrographKlemPlugin.CheckHistoricalFile.label");
            }

            klemProps.setHistoricalRainfallFile(historicalFile);
            break;
        }
        }
        klemProps.setRainfallType(rainfallType);

        // overwrite project XML
        final XMLCreatorKlem createXML = new XMLCreatorKlem(klemProps);
        createXML.createDocument();

    }

    public static void checkAdvancedParams(KinematicsType kinematicsType,
            KlemProperties klemProps, ComponentsTreeMap componentsWithActions,
            HyetographShape hyetoShape, AbstractionUnits initAbsUnit,
            Amc_Type amcType, BaseflowType baseflowType)
            throws WarningException {

        // routing time params
        if (kinematicsType == KlemProperties.KinematicsType.SIMPLE) {
            final double slopeVelocity = GUIUtils
                    .getDoubleValue(componentsWithActions.getComponent("04",
                            GUIUtils.OTHER, 1));
            final double channelVelocity = GUIUtils
                    .getDoubleValue(componentsWithActions.getComponent("05",
                            GUIUtils.OTHER, 1));
            final double threshold = GUIUtils
                    .getDoubleValue(componentsWithActions.getComponent("06",
                            GUIUtils.OTHER, 1));

            klemProps.setSlopeVelocity(slopeVelocity);
            klemProps.setChannelVelocity(channelVelocity);
            klemProps.setThresholdValue(threshold);

        } else if (kinematicsType == KlemProperties.KinematicsType.ADVANCED) {

            final double minSlopeVelocity = GUIUtils
                    .getDoubleValue(componentsWithActions.getComponent("08",
                            GUIUtils.OTHER, 1));
            final double maxSlopeVelocity = GUIUtils
                    .getDoubleValue(componentsWithActions.getComponent("09",
                            GUIUtils.OTHER, 1));
            final double costSlopeVelocity = GUIUtils
                    .getDoubleValue(componentsWithActions.getComponent("10",
                            GUIUtils.OTHER, 1));

            final double channelVelocity = GUIUtils
                    .getDoubleValue(componentsWithActions.getComponent("11",
                            GUIUtils.OTHER, 1));

            final double minThreshold = GUIUtils
                    .getDoubleValue(componentsWithActions.getComponent("13",
                            GUIUtils.OTHER, 1));
            final double maxThreshold = GUIUtils
                    .getDoubleValue(componentsWithActions.getComponent("14",
                            GUIUtils.OTHER, 1));
            final double costThreshold = GUIUtils
                    .getDoubleValue(componentsWithActions.getComponent("15",
                            GUIUtils.OTHER, 1));

            klemProps.setMinSlopeVelocity(minSlopeVelocity);
            klemProps.setMaxSlopeVelocity(maxSlopeVelocity);
            klemProps.setSlopeConstant(costSlopeVelocity);
            klemProps.setChannelVelocity(channelVelocity);
            klemProps.setMinThreshold(minThreshold);
            klemProps.setMaxThreshold(maxThreshold);
            klemProps.setThresholdConstant(costThreshold);
        }
        klemProps.setKinematicsType(kinematicsType);

        // hyeto params
        if (hyetoShape == null) {
            throw new NullPointerException(PluginUtils.getResources()
                    .getString("HydrographKlemPlugin.HyetoTypeSel.label"));
        } else {
            klemProps.setHyetoType(hyetoShape);
        }

        final double peak = GUIUtils.getDoubleValue(componentsWithActions
                .getComponent("20", GUIUtils.OTHER, 1));
        final double stepRainfall = GUIUtils
                .getDoubleValue(componentsWithActions.getComponent("21",
                        GUIUtils.OTHER, 1));
        final double rainfallRecession = GUIUtils
                .getDoubleValue(componentsWithActions.getComponent("22",
                        GUIUtils.OTHER, 1));
        final double thresholdRainRecession = GUIUtils
                .getDoubleValue(componentsWithActions.getComponent("23",
                        GUIUtils.OTHER, 1));
        final double initialAbstraction = GUIUtils
                .getDoubleValue(componentsWithActions.getComponent("24",
                        GUIUtils.OTHER, 1));

        klemProps.setHyetoPeakPosition(peak);
        klemProps.setRainfallStep(stepRainfall);
        klemProps.setRainfallRecessionValue(rainfallRecession);
        klemProps.setRainfallRecessionThreshold(thresholdRainRecession);
        klemProps.setInitialAbstractionValue(initialAbstraction);
        klemProps.setInitAbsUnit(initAbsUnit);

        if (initAbsUnit == InitialAbstraction.AbstractionUnits.FRACTION) {
            if (initialAbstraction < 0 || initialAbstraction >= 1) {
                throw new WarningException(
                        PluginUtils
                                .getResources()
                                .getString(
                                        "HydrographKlemPlugin.InvalidInitAbsFractionValueException"));
            }
        } else if (initAbsUnit == InitialAbstraction.AbstractionUnits.MILLIMETERS) {
            if (initialAbstraction < 0) {
                throw new WarningException(
                        PluginUtils
                                .getResources()
                                .getString(
                                        "HydrographKlemPlugin.InvalidInitAbsMmValueException"));
            }
        }

        // hydro params
        final double outputStep = GUIUtils.getDoubleValue(componentsWithActions
                .getComponent("29", GUIUtils.OTHER, 1));
        final double amc = GUIUtils.getDoubleValue(componentsWithActions
                .getComponent("30", GUIUtils.OTHER, 1));
        final String baseflow = GUIUtils.getStringValue(componentsWithActions
                .getComponent("32", GUIUtils.OTHER, 1));
        final double discharge = GUIUtils.getDoubleValue(componentsWithActions
                .getComponent("33", GUIUtils.OTHER, 1));
        final String arf = GUIUtils.getStringValue(componentsWithActions
                .getComponent("34", GUIUtils.OTHER, 1));
        final double frpm = GUIUtils.getDoubleValue(componentsWithActions
                .getComponent("35", GUIUtils.OTHER, 1));
        final double subSurfLoss = GUIUtils
                .getDoubleValue(componentsWithActions.getComponent("36",
                        GUIUtils.OTHER, 1));
        final double geomorphoFactor = GUIUtils
                .getDoubleValue(componentsWithActions.getComponent("37",
                        GUIUtils.OTHER, 1));
        final double thresholdGeomorphoFactor = GUIUtils
                .getDoubleValue(componentsWithActions.getComponent("38",
                        GUIUtils.OTHER, 1));

        klemProps.setHydroStepOutput(outputStep);
        klemProps.setAmcValue(amc);
        klemProps.setAmcType(amcType);
        klemProps.setBaseFlowType(baseflowType);

        if (!klemProps.isQ0Auto()) {
            klemProps.setQ0(Double.parseDouble(baseflow));
        }

        klemProps.setBaseflowRecession(discharge);

        if (klemProps.isArfAuto()) {
            klemProps.setArf(klemProps.getAutoArf());
        } else {
            klemProps.setArf(Double.parseDouble(arf));
        }

        klemProps.setPeakFraction(frpm);
        klemProps.setSubSurfaceDrainageLoss(subSurfLoss);
        klemProps.setGeomorphFactor(geomorphoFactor);
        klemProps.setGeomorphoFactorThreshold(thresholdGeomorphoFactor);
    }

    public static Klem buildKlem(KlemProperties klemProps) throws Exception {

        // Extract subcatchment data
        final DoubleBasicGrid cnGrid = RasterUtils
                .getDoubleBasicGridFromFile(klemProps.getCNFile());
        final DoubleBasicGrid demGrid = RasterUtils
                .getDoubleBasicGridFromFile(klemProps.getDemFile());

        // Flow directions
        LineString[] bluelines = null;
        if (klemProps.getBluelinesFile() != null) {
            bluelines = GeometryUtils.getLineStringsFromFeatures(ShapefileUtils
                    .loadShapefile(klemProps.getBluelinesFile()));
        }
        final FlowDirsCalculator flowDirsCalculator = new FlowDirsCalculator(
                demGrid, FlowDirsStripe.FlowDirAlgorithm.D8, bluelines, 100d);
        final FlowDirBasicGrid flowDirGrid = flowDirsCalculator.calculate();

        final WatershedExtractor watershedExtractor = new WatershedExtractor();
        final DoubleBasicGrid watershedGrid = watershedExtractor.extract(
                flowDirGrid, new Coordinate[] { klemProps.getOutletCoords() },
                true);
        RasterUtils.saveOutputRasterAsTiff(watershedGrid, new File(klemProps
                .getOutputFolder().getAbsoluteFile()
                + File.separator
                + "mask.tif"));

        // extract demMask
        final DoubleBasicGrid demGridMask = RasterClipper.clip(demGrid,
                watershedGrid);
        RasterUtils.saveOutputRasterAsTiff(demGridMask,
                klemProps.getDemMaskFile());

        // extract cnMask
        final DoubleBasicGrid cnMask = RasterClipper
                .clip(cnGrid, watershedGrid);
        final double[][] rasCurveNumber = cnMask.getData();
        RasterUtils.saveOutputRasterAsTiff(cnMask, new File(klemProps
                .getOutputFolder().getAbsoluteFile()
                + File.separator
                + "cnMask.tif"));

        // create slope mask
        calculateSlope(klemProps);

        // Create rainfall
        final TimeInterval inputStep = new TimeInterval(
                klemProps.getRainfallStep(),
                TimeInterval.TimeIntervalUnit.MINUTE);

        TimeInterval calculationStep = null;
        final RasterImageIO.CellSizeXY cellSize = RasterImageIO
                .getCellSize(klemProps.getCNFile().getAbsolutePath());
        if (cellSize.getCellSizeX() == cellSize.getCellSizeY()) {
            calculationStep = new TimeInterval(
                    cellSize.getCellSizeX() / 0.1 / 60,
                    TimeInterval.TimeIntervalUnit.MINUTE);
        }

        final TimeInterval outputStep = new TimeInterval(
                klemProps.getHydroStepOutput(),
                TimeInterval.TimeIntervalUnit.MINUTE);

        final LsppCalculator lsppCalculator = buildLsppCalculator(klemProps,
                watershedGrid);
        klemProps.setLsppCalculator(lsppCalculator);

        Klem klem = null;

        // Routing time
        // FlowDirBasicGrid flowDirGridMask = RasterClipper.clip(flowDirGrid,
        // watershedGrid);
        final double[][] routingData = calcRoutingTime(klemProps, demGridMask,
                bluelines, 100d);

        // Watershed -----------------------------------------------------------
        final Watershed watershed = new Watershed(klemProps.getCatchmentArea(),
                rasCurveNumber, routingData, cnMask.getCellSize(),
                cnMask.getNoData(), klemProps.getAmcValue(),
                klemProps.getInitialAbstraction(),
                klemProps.getBaseflowParams(),
                klemProps.getSubSurfaceDrainageLoss(),
                klemProps.getRainfallRecession(), klemProps.getArfValue(),
                klemProps.getGeomorphology());

        if (klemProps.getRainfallType() == KlemProperties.RainfallType.DISTRIBUTED
                || klemProps.getRainfallType() == KlemProperties.RainfallType.POINT) {
            klem = new Klem(watershed, klemProps.getHyetoShape(),
                    lsppCalculator, klemProps.getPeakFraction(), inputStep,
                    calculationStep, outputStep,
                    klemProps.getHyetoPeakPosition(), null);
        } else if (klemProps.getRainfallType() == KlemProperties.RainfallType.HISTORICAL) {
            klem = new Klem(readHyetograph(klemProps), watershed,
                    calculationStep, outputStep, null);
        }

        return klem;

    }

    private static double[][] calcRoutingTime(KlemProperties klemProps,
            DoubleBasicGrid demGrid, LineString[] bluelines,
            Double bluelinesWeight) throws IOException, Exception {

        RoutingTimeParameters routingTimeParameters;
        // Routing time - simple kinematics
        if (klemProps.getKinematicsType() == KlemProperties.KinematicsType.SIMPLE) {
            routingTimeParameters = new RoutingTimeParameters(
                    HydroUtils.calculateSpeed(klemProps.getSlopeVelocity()),
                    HydroUtils.calculateSpeed(klemProps.getChannelVelocity()),
                    new Area(klemProps.getThresholdValue(),
                            Length.LengthUnit.km), new Area(Double.MAX_VALUE,
                            Length.LengthUnit.km), 1);
        } else {
            // TODO: complex kinematics
            routingTimeParameters = new RoutingTimeAdvancedParameters(
                    HydroUtils.calculateSpeed(klemProps.getMinSlopeVelocity()),
                    HydroUtils.calculateSpeed(klemProps.getMaxSlopeVelocity()),
                    klemProps.getSlopeConstant(),
                    HydroUtils.calculateSpeed(klemProps.getChannelVelocity()),
                    new Area(klemProps.getMinThreshold(), Length.LengthUnit.km),
                    new Area(klemProps.getMaxThreshold(), Length.LengthUnit.km),
                    klemProps.getThresholdConstant(),
                    new Area(Double.MAX_VALUE, Length.LengthUnit.km),
                    1,
                    new Area(klemProps.getCatchmentArea(), Length.LengthUnit.km));
        }

        // Routing time --------------------------------------------------------
        final RoutingTimeCalculator routingTimeCalculator = new RoutingTimeCalculator();
        final DoubleBasicGrid routingTimeGrid = routingTimeCalculator.calcD8(
                demGrid, bluelines, bluelinesWeight, routingTimeParameters);
        final double[][] routingData = routingTimeGrid.getData();
        final double cellSize = routingTimeGrid.getCellSize();
        final double noData = routingTimeGrid.getNoData();

        RasterUtils.saveOutputRasterAsTiff(routingTimeGrid, new File(klemProps
                .getOutputFolder().getAbsolutePath()
                + File.separator
                + "routingMask.tif"));

        return routingData;

    }

    private static void calculateSlope(KlemProperties klemProps)
            throws NoninvertibleTransformException,
            TiffTags.TiffReadingException, Exception {

        // RasterImageLayer demRil =
        // RasterUtils.getRasterImageLayerFromFile(context, dem);
        // DoubleBasicGrid demGrid = RasterUtils.getDoubleBasicGrid(demRil);

        final DoubleBasicGrid demGrid = RasterUtils
                .getDoubleBasicGridFromFile(klemProps.getDemMaskFile());

        final double[][] data = demGrid.getData();

        final int nCols = demGrid.getColumnCount();
        final int nRows = demGrid.getRowCount();
        final double noData = demGrid.getNoData();
        final double cellSize = demGrid.getCellSize();

        // add border with noData values at data array
        final double[][] dataWithBorder = new double[nRows + 2][nCols + 2];

        // array fro slope values
        final double[][] slope = new double[nRows + 2][nCols + 2];

        for (int r = 0; r < dataWithBorder.length; r++) {
            for (int c = 0; c < dataWithBorder[0].length; c++) {
                dataWithBorder[r][c] = noData;
                slope[r][c] = noData;
            }
        }

        // complete dataWithBorder with data values
        for (int r = 1; r < dataWithBorder.length - 1; r++) {
            for (int c = 1; c < dataWithBorder[0].length - 1; c++) {
                dataWithBorder[r][c] = data[r - 1][c - 1];
            }
        }

        // Calculate slope using Horn's algorithm
        // Horn, B. K. P. (1981). Hill Shading and the Reflectance Map,
        // Proceedings of the IEEE, 69(1):14-47.
        int cellCount;
        int ix;
        final double[] z = new double[9]; // kernel
        double parb; // dz/dx
        double parc; // dz/dy
        // escludo bordo nodata
        for (int r = 1; r <= nRows; r++) {
            for (int c = 1; c <= nCols; c++) {
                if (dataWithBorder[r][c] != noData) {
                    cellCount = 0;
                    // kernel
                    for (int kr = -1; kr <= +1; kr++) {
                        for (int kc = -1; kc <= +1; kc++) {
                            ix = (kr + 1) * 3 + (kc + 1);
                            if (dataWithBorder[r + kr][c + kc] != noData) {
                                // differenza valore in riferimento al pixel
                                // centrale
                                z[ix] = dataWithBorder[r + kr][c + kc]
                                        - dataWithBorder[r][c];
                                cellCount++;
                            } else {
                                z[ix] = 0;
                            }
                        }
                    }
                    cellCount--;
                    parb = (z[2] + 2 * z[5] + z[8] - z[0] - 2 * z[3] - z[6])
                            / (cellCount * cellSize); // dz/dy
                    parc = (z[0] + 2 * z[1] + z[2] - z[6] - 2 * z[7] - z[8])
                            / (cellCount * cellSize); // dz/dx

                    // Slope
                    slope[r][c] = 100 * (Math.sqrt(parb * parb + parc * parc));
                }
            }
        }

        // remove noData boorder from slope
        final double[][] slope_ = new double[nRows][nCols];
        for (int r = 1; r < (slope.length - 1); r++) {
            for (int c = 1; c < (slope[0].length - 1); c++) {
                slope_[r - 1][c - 1] = slope[r][c];
            }
        }

        final DoubleBasicGrid slopeGrid = new DoubleBasicGrid(slope_, cellSize,
                noData, demGrid.getLowerLeftCoord());
        RasterUtils.saveOutputRasterAsTiff(slopeGrid, new File(klemProps
                .getOutputFolder().getAbsolutePath()
                + File.separator
                + "slopeMask.tif"));
    }

    public static LsppCalculator buildLsppCalculator(KlemProperties klemProps,
            DoubleBasicGrid watershedGrid) throws IOException, Exception {

        LsppCalculator lsppCalculator = null;
        if (klemProps.getRainfallType() == RainfallType.DISTRIBUTED) {
            final RasterAverager rasterAverage = new RasterAverager();
            // lspp mask
            // aGrid
            // RasterImageLayer aRil =
            // RasterUtils.getRasterImageLayerFromFile(context, aFile);
            final DoubleBasicGrid aGrid = RasterUtils
                    .getDoubleBasicGridFromFile(klemProps.getAFile());
            final double aAverage = rasterAverage.average(aGrid, watershedGrid);

            // nGrid
            // RasterImageLayer nRil =
            // RasterUtils.getRasterImageLayerFromFile(context, nFile);
            final DoubleBasicGrid nGrid = RasterUtils
                    .getDoubleBasicGridFromFile(klemProps.getNFile());
            final double nAverage = rasterAverage.average(nGrid, watershedGrid);

            // cvGrid
            // RasterImageLayer cvRil =
            // RasterUtils.getRasterImageLayerFromFile(context, cvFile);
            final DoubleBasicGrid cvGrid = RasterUtils
                    .getDoubleBasicGridFromFile(klemProps.getCvFile());
            final double cvAverage = rasterAverage.average(cvGrid,
                    watershedGrid);

            lsppCalculator = new LsppCalculator(aAverage, cvAverage, nAverage,
                    klemProps.getnLessHourParam(), klemProps.getLsppModel(),
                    klemProps.getReturnPeriod());

        } else if (klemProps.getRainfallType() == KlemProperties.RainfallType.POINT) {

            lsppCalculator = new LsppCalculator(klemProps.getAParam(),
                    klemProps.getNParam(), klemProps.getnLessHourParam());
        }
        return lsppCalculator;
    }

    private static Hyetograph readHyetograph(KlemProperties klemProps)
            throws FileNotFoundException, IOException, Exception {

        final BufferedReader buffReader = new BufferedReader(new FileReader(
                klemProps.getHistoricalRainfallFile()));
        final TimeInterval ciclo_in = new TimeInterval(
                Double.parseDouble(buffReader.readLine().trim().split(",")[0]),
                TimeInterval.TimeIntervalUnit.MINUTE);
        buffReader.readLine();

        // Data
        final ArrayList<Double> rainfall_al = new ArrayList<Double>();
        rainfall_al.add(0d);

        String line;
        while ((line = buffReader.readLine()) != null) {
            if (line.equals("")) {
                continue;
            }
            final String[] vals = line.trim().split(",");
            rainfall_al.add(Double.parseDouble(vals[6]));
        }

        buffReader.close();

        final double[] rainfall_a = new double[rainfall_al.size()];

        for (int p = 0; p < rainfall_a.length; p++) {
            rainfall_a[p] = rainfall_al.get(p);
        }

        return new Hyetograph(ciclo_in, rainfall_a);
    }

}
