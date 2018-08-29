package com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem;

import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem.KlemProperties.RainfallType;
import com.geomaticaeambiente.openjump.klem.units.Length;
import it.geomaticaeambiente.klem.HyetographGenerator;
import it.geomaticaeambiente.klem.InitialAbstraction;
import it.geomaticaeambiente.klem.LsppCalculator;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;
import org.apache.xml.serialize.XMLSerializer;
import org.apache.xml.serialize.OutputFormat;

/**
 *
 * @author Geomatica
 */
public class XMLCreatorKlem {

    //No generics
    List myData;
    Document dom;

    public XMLCreatorKlem(KlemProperties klemProperties) throws ParserConfigurationException, IOException {

        this.klemProperties = klemProperties;
    }

    public void createDocument() throws ParserConfigurationException, IOException {

        //get an instance of factory
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

        //get an instance of builder
        DocumentBuilder db = dbf.newDocumentBuilder();

        //create an instance of DOM
        dom = db.newDocument();

        //Create document
        createDOMTree();

        //print document
        printToFile();

    }

    /**
     * The real workhorse which creates the XML structure
     */
    private void createDOMTree() {

        //create the root element <Books>
        Element rootEle = dom.createElement(KlemProperties.PROJECT_PARAMETERS);
        dom.appendChild(rootEle);

        //Element prjFolder = dom.createElement(KlemProperties.PROJECT_FOLDER);
        //Text prjFolderText = dom.createTextNode(getString(klemProperties.getProjectFolder()));
        //prjFolder.appendChild(prjFolderText);
        //rootEle.appendChild(prjFolder);

        Element input = dom.createElement(KlemProperties.INPUT);

        Element bluelines = dom.createElement(KlemProperties.BLUELINES);
        Text bluelinesText = dom.createTextNode(Boolean.toString(klemProperties.withBluelines));
        bluelines.appendChild(bluelinesText);
        input.appendChild(bluelines);

        Element outlet = dom.createElement(KlemProperties.OUTLET);

        Element xCoord = dom.createElement(KlemProperties.XCOORD);
        Element yCoord = dom.createElement(KlemProperties.YCOORD);
        Text xCoordText = null;
        Text yCoordText = null;
        if (klemProperties.getOutletCoords() != null) {
            xCoordText = dom.createTextNode(getString(klemProperties.getOutletCoords().x));
            yCoordText = dom.createTextNode(getString(klemProperties.getOutletCoords().y));
        } else {
            xCoordText = dom.createTextNode("");
            yCoordText = dom.createTextNode("");
        }
        xCoord.appendChild(xCoordText);
        outlet.appendChild(xCoord);
        yCoord.appendChild(yCoordText);
        outlet.appendChild(yCoord);

        Element basinArea = dom.createElement(KlemProperties.AREA);
        Text basinAreaText = dom.createTextNode(getString(klemProperties.getCatchmentArea()));
        basinArea.appendChild(basinAreaText);
        outlet.appendChild(basinArea);

        Element elevation = dom.createElement(KlemProperties.ELEVATION);
        Text elevationText = dom.createTextNode(getString(klemProperties.getOutletElevation()));
        elevation.appendChild(elevationText);
        outlet.appendChild(elevation);

        input.appendChild(outlet);

        Element rainfall = dom.createElement(KlemProperties.RAINFALL);

        Element rainfallType = dom.createElement(KlemProperties.TYPE);
        Text rainfallTypeText = dom.createTextNode(getString(klemProperties.getRainfallType()));
        rainfallType.appendChild(rainfallTypeText);
        rainfall.appendChild(rainfallType);

        if (klemProperties.getRainfallType() == RainfallType.DISTRIBUTED) {
            Element model = dom.createElement(KlemProperties.MODEL);
            Text modelText = dom.createTextNode(getString(klemProperties.getLsppModel()));
            model.appendChild(modelText);
            rainfall.appendChild(model);

            Element aFile = dom.createElement(KlemProperties.A_FILE);
            Text aFileText = dom.createTextNode(getString(klemProperties.getAFile()));
            aFile.appendChild(aFileText);
            rainfall.appendChild(aFile);

            Element nFile = dom.createElement(KlemProperties.N_FILE);
            Text nFileText = dom.createTextNode(getString(klemProperties.getNFile()));
            nFile.appendChild(nFileText);
            rainfall.appendChild(nFile);

            Element cvFile = dom.createElement(KlemProperties.CV_FILE);
            Text cvFileText = dom.createTextNode(getString(klemProperties.getCvFile()));
            cvFile.appendChild(cvFileText);
            rainfall.appendChild(cvFile);

            Element returnTime = dom.createElement(KlemProperties.RETURN_TIME);
            Text returnTimeText = dom.createTextNode(getString(klemProperties.getReturnPeriod()));
            returnTime.appendChild(returnTimeText);
            rainfall.appendChild(returnTime);

            Element nLessHour = dom.createElement(KlemProperties.N_LESS_HOUR);
            Text nLessHourText = null;
            if(klemProperties.isNLessHourParamAuto()){
                nLessHourText = dom.createTextNode("Auto");
            } else {
                nLessHourText = dom.createTextNode(getString(klemProperties.getnLessHourParam()));
            }            
            nLessHour.appendChild(nLessHourText);
            rainfall.appendChild(nLessHour);

        } else if (klemProperties.getRainfallType() == RainfallType.POINT) {

            Element aParam = dom.createElement(KlemProperties.A_PARAM);
            Text aParamText = dom.createTextNode(getString(klemProperties.getAParam()));
            aParam.appendChild(aParamText);
            rainfall.appendChild(aParam);

            Element nParam = dom.createElement(KlemProperties.N_PARAM);
            Text nParamText = dom.createTextNode(getString(klemProperties.getNParam()));
            nParam.appendChild(nParamText);
            rainfall.appendChild(nParam);

            Element nLessHour = dom.createElement(KlemProperties.N_LESS_HOUR);
            Text nLessHourText = null;
            if(klemProperties.isNLessHourParamAuto()){
                nLessHourText = dom.createTextNode("Auto");
            } else {
                nLessHourText = dom.createTextNode(getString(klemProperties.getnLessHourParam()));
            }            
            nLessHour.appendChild(nLessHourText);
            rainfall.appendChild(nLessHour);

        } else if (klemProperties.getRainfallType() == RainfallType.HISTORICAL) {

            Element historical = dom.createElement(KlemProperties.HISTORICAL_FILE);
            Text historicText = dom.createTextNode(getString(klemProperties.getHistoricalRainfallFile()));
            historical.appendChild(historicText);
            rainfall.appendChild(historical);
        }

        input.appendChild(rainfall);

        Element advancedParams = dom.createElement(KlemProperties.ADVANCED_PARAMS);

        Element routingTime = dom.createElement(KlemProperties.ROUTING_TIME);

        Element kinematics = dom.createElement(KlemProperties.KINEMATICS);

        if (klemProperties.getKinematicsType() == KlemProperties.KinematicsType.SIMPLE) {
            Element kinematicsType = dom.createElement(KlemProperties.KINEMATICS_TYPE);
            Text kinematicsTypeText = dom.createTextNode(getString(klemProperties.getKinematicsType()));
            kinematicsType.appendChild(kinematicsTypeText);
            kinematics.appendChild(kinematicsType);

            Element slopeVelocity = dom.createElement(KlemProperties.SLOPE_VELOCITY);
            Text slopeVelocityText = dom.createTextNode(getString(klemProperties.getSlopeVelocity()));
            slopeVelocity.appendChild(slopeVelocityText);
            kinematics.appendChild(slopeVelocity);

            Element channelVelocity = dom.createElement(KlemProperties.CHANNEL_VELOCITY);
            Text channelVelocityText = dom.createTextNode(getString(klemProperties.getChannelVelocity()));
            channelVelocity.appendChild(channelVelocityText);
            kinematics.appendChild(channelVelocity);

            Element threshold = dom.createElement(KlemProperties.THRESHOLD);
            Text thresholdText = dom.createTextNode(getString(klemProperties.getThresholdValue()));
            threshold.appendChild(thresholdText);
            kinematics.appendChild(threshold);

        } else if (klemProperties.getKinematicsType() == KlemProperties.KinematicsType.ADVANCED) {

            Element slopeVelocity = dom.createElement(KlemProperties.SLOPE_VELOCITY);

            Element minSlopeVelocity = dom.createElement(KlemProperties.MIN_SLOPE_VELOCITY);
            Text minSlopeVelocityText = dom.createTextNode(getString(klemProperties.getMinSlopeVelocity()));
            minSlopeVelocity.appendChild(minSlopeVelocityText);
            slopeVelocity.appendChild(minSlopeVelocity);

            Element maxSlopeVelocity = dom.createElement(KlemProperties.MAX_SLOPE_VELOCITY);
            Text maxSlopeVelocityText = dom.createTextNode(getString(klemProperties.getMaxSlopeVelocity()));
            maxSlopeVelocity.appendChild(maxSlopeVelocityText);
            slopeVelocity.appendChild(maxSlopeVelocity);

            Element slopeCost = dom.createElement(KlemProperties.SLOPE_COST);
            Text slopeCostText = dom.createTextNode(getString(klemProperties.getSlopeConstant()));
            slopeCost.appendChild(slopeCostText);
            slopeVelocity.appendChild(slopeCost);

            kinematics.appendChild(slopeVelocity);

            Element channelVelocity = dom.createElement(KlemProperties.CHANNEL_VELOCITY);
            Text channelVelocityText = dom.createTextNode(getString(klemProperties.getChannelVelocity()));
            channelVelocity.appendChild(channelVelocityText);
            kinematics.appendChild(channelVelocity);

            Element threshold = dom.createElement(KlemProperties.THRESHOLD);

            Element minThreshold = dom.createElement(KlemProperties.MIN_THRESHOLD);
            Text minThresholdText = dom.createTextNode(getString(klemProperties.getMinThreshold()));
            minThreshold.appendChild(minThresholdText);
            threshold.appendChild(minThreshold);

            Element maxThreshold = dom.createElement(KlemProperties.MAX_THRESHOLD);
            Text maxThresholdText = dom.createTextNode(getString(klemProperties.getMaxThreshold()));
            maxThreshold.appendChild(maxThresholdText);
            threshold.appendChild(maxThreshold);

            Element thresholdCost = dom.createElement(KlemProperties.THRESHOLD_COST);
            Text threshlodCostText = dom.createTextNode(getString(klemProperties.getThresholdConstant()));
            thresholdCost.appendChild(threshlodCostText);
            threshold.appendChild(thresholdCost);

            kinematics.appendChild(threshold);

        }

        routingTime.appendChild(kinematics);

        advancedParams.appendChild(routingTime);

        //hyetograph
        Element hyetogtaph = dom.createElement(KlemProperties.HYETOGRAPH);

        Element hyetoType = dom.createElement(KlemProperties.HYETO_TYPE);
        Text hyetoTypeText = dom.createTextNode(getString(klemProperties.getHyetoShape()));
        hyetoType.appendChild(hyetoTypeText);
        hyetogtaph.appendChild(hyetoType);

        Element peak = dom.createElement(KlemProperties.PEAK);
        Text peakText = dom.createTextNode(getString(klemProperties.getHyetoPeakPosition()));
        peak.appendChild(peakText);
        hyetogtaph.appendChild(peak);

        Element rainfallStep = dom.createElement(KlemProperties.RAINFALL_DATA_STEP);
        Text rainfallStepText = dom.createTextNode(getString(klemProperties.getRainfallStep()));
        rainfallStep.appendChild(rainfallStepText);
        hyetogtaph.appendChild(rainfallStep);

        Element rainfallRecession = dom.createElement(KlemProperties.RAINFALL_RECESSION);
        Text rainfallRecessionText = dom.createTextNode(getString(klemProperties.getRainfallRecessionValue()));
        rainfallRecession.appendChild(rainfallRecessionText);
        hyetogtaph.appendChild(rainfallRecession);

        Element thresholdRainRecession = dom.createElement(KlemProperties.THRESH_RAINFALL_RECESSION);
        Text thresholdRainRecessionText = dom.createTextNode(getString(klemProperties.getRainfallRecessionThreshold()));
        thresholdRainRecession.appendChild(thresholdRainRecessionText);
        hyetogtaph.appendChild(thresholdRainRecession);

        Element initialAbstraction = dom.createElement(KlemProperties.INITIAL_ABSTRACTION);

        Element initAbsValue = dom.createElement(KlemProperties.INITIAL_ABSTRACTION_VALUE);
        Text initAbsValueText = dom.createTextNode(getString(klemProperties.getInitialAbstractionValue()));
        initAbsValue.appendChild(initAbsValueText);
        initialAbstraction.appendChild(initAbsValue);

        Element initAbsUnit = dom.createElement(KlemProperties.INITIAL_ABSTRACTION_UNIT);
        Text initAbsUnitText = dom.createTextNode(getString(klemProperties.getInitialAbstraction().getAbstractionUnits()));
        initAbsUnit.appendChild(initAbsUnitText);
        initialAbstraction.appendChild(initAbsUnit);

        hyetogtaph.appendChild(initialAbstraction);

        advancedParams.appendChild(hyetogtaph);

        //hydrograph
        Element hydrograph = dom.createElement(KlemProperties.HYDROGRAH);

        Element outputStep = dom.createElement(KlemProperties.OUTPUT_STEP);
        Text outputStepText = dom.createTextNode(getString(klemProperties.getHydroStepOutput()));
        outputStep.appendChild(outputStepText);
        hydrograph.appendChild(outputStep);

        Element amcValue = dom.createElement(KlemProperties.AMC_VALUE);
        Text amcValueText = dom.createTextNode(getString(klemProperties.getAmcValue()));
        amcValue.appendChild(amcValueText);
        hydrograph.appendChild(amcValue);

        Element amcType = dom.createElement(KlemProperties.AMC_TYPE);
        Text amcTypeText = dom.createTextNode(getString(klemProperties.getAmcType()));
        amcType.appendChild(amcTypeText);
        hydrograph.appendChild(amcType);

        Element baseFlowType = dom.createElement(KlemProperties.BASEFLOW_TYPE);
        Text baseflowTypeText = dom.createTextNode(getString(klemProperties.getBaseFlowType()));
        baseFlowType.appendChild(baseflowTypeText);
        hydrograph.appendChild(baseFlowType);

        Element baseFlowValue = dom.createElement(KlemProperties.BASEFLOW_VALUE);
        Text baseflowValueText = null;
        if (klemProperties.isQ0Auto()) {
            baseflowValueText = dom.createTextNode("Auto");
        } else {
            baseflowValueText = dom.createTextNode(getString(klemProperties.getQ0()));
        }
        baseFlowValue.appendChild(baseflowValueText);
        hydrograph.appendChild(baseFlowValue);

        Element discharge = dom.createElement(KlemProperties.DISCHARGE);
        Text dischargeText = dom.createTextNode(getString(klemProperties.getBasefklowRecession()));
        discharge.appendChild(dischargeText);
        hydrograph.appendChild(discharge);

        Element arf = dom.createElement(KlemProperties.ARF);
        Text arfText = null;
        if (klemProperties.isArfAuto()) {
            arfText = dom.createTextNode("Auto");
        } else {
            arfText = dom.createTextNode(getString(klemProperties.getArfValue()));
        }
        arf.appendChild(arfText);
        hydrograph.appendChild(arf);

        Element frpm = dom.createElement(KlemProperties.FRPM_TC);
        Text frpmText = dom.createTextNode(getString(klemProperties.getPeakFraction()));
        frpm.appendChild(frpmText);
        hydrograph.appendChild(frpm);

        Element subDrainLoss = dom.createElement(KlemProperties.SUB_SURF_DRAIN_LOSS);
        Text subDrainLossText = dom.createTextNode(getString(klemProperties.getSubSurfaceDrainageLoss()));
        subDrainLoss.appendChild(subDrainLossText);
        hydrograph.appendChild(subDrainLoss);

        Element geomorphFactor = dom.createElement(KlemProperties.GEOMOPRH_FACTOR);
        Text geomorphFactorText = dom.createTextNode(getString(klemProperties.getGeomorphFactor()));
        geomorphFactor.appendChild(geomorphFactorText);
        hydrograph.appendChild(geomorphFactor);

        Element threshGeomorph = dom.createElement(KlemProperties.THRESH_GEOMORPH_FACTOR);
        Text threshGeomorphText = dom.createTextNode(getString(klemProperties.getGeomorphoFactorThreshold()));
        threshGeomorph.appendChild(threshGeomorphText);
        hydrograph.appendChild(threshGeomorph);

        advancedParams.appendChild(hydrograph);

        input.appendChild(advancedParams);

        rootEle.appendChild(input);

    }

    /**
     * This method uses Xerces specific classes prints the XML document to file.
     */
    private void printToFile() throws IOException {

        File outXml = new File(klemProperties.getProjectFolder() + File.separator + klemProperties.getProjectName() + ".xml");
        //print
        OutputFormat format = new OutputFormat(dom);
        format.setIndenting(true);

        XMLSerializer serializer = new XMLSerializer(
                new FileOutputStream(outXml), format);

        serializer.serialize(dom);
    }

    
    private String getString(Object object) {

        if (object != null) {

            if (object instanceof String) {
                return object.toString();
            } else if (object instanceof Double) {
                Double value = (Double) object;
                return Double.toString(value);
            } else if (object instanceof File) {
                File file = (File) object;
                return file.getAbsolutePath();
            } else if (object instanceof Integer) {
                Integer value = (Integer) object;
                return Integer.toString(value);
            } else if (object instanceof RainfallType) {
                RainfallType type = (RainfallType) object;
                return type.toString();
            } else if (object instanceof LsppCalculator.LsppModel) {
                LsppCalculator.LsppModel type = (LsppCalculator.LsppModel) object;
                return type.toString();
            } else if (object instanceof KlemProperties.KinematicsType) {
                KlemProperties.KinematicsType type = (KlemProperties.KinematicsType) object;
                return type.toString();
            } else if (object instanceof HyetographGenerator.HyetographShape) {
                HyetographGenerator.HyetographShape type = (HyetographGenerator.HyetographShape) object;
                return type.toString();
            } else if (object instanceof InitialAbstraction.AbstractionUnits) {
                InitialAbstraction.AbstractionUnits type = (InitialAbstraction.AbstractionUnits) object;
                return type.toString();
            } else if (object instanceof KlemProperties.Amc_Type) {
                KlemProperties.Amc_Type type = (KlemProperties.Amc_Type) object;
                return type.toString();
            }
        }

        return "";
    }

    KlemProperties klemProperties;

}
