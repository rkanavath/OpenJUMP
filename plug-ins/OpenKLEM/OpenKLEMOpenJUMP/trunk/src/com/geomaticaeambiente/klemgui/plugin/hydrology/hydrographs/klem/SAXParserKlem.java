package com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem;

import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem.KlemProperties.RainfallType;
import it.geomaticaeambiente.klem.BaseflowParams;
import it.geomaticaeambiente.klem.HyetographGenerator;
import it.geomaticaeambiente.klem.InitialAbstraction;
import it.geomaticaeambiente.klem.LsppCalculator;
import java.io.File;
import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import org.xml.sax.helpers.DefaultHandler;

public class SAXParserKlem extends DefaultHandler {

    private String tempVal;
    private KlemProperties projProp;

    public SAXParserKlem() {
    }

    public void parseDocument(File klemXML) {

        //get a factory
        SAXParserFactory spf = SAXParserFactory.newInstance();
        try {

            projProp = new KlemProperties();
            projProp.setProjectFolder(klemXML.getParentFile());
            
            //get a new instance of parser
            SAXParser sp = spf.newSAXParser();

            //parse the file and also register this class for call backs
            sp.parse(klemXML, this);

        } catch (SAXException se) {
            se.printStackTrace(System.out);
        } catch (ParserConfigurationException pce) {
            pce.printStackTrace(System.out);
        } catch (IOException ie) {
            ie.printStackTrace(System.out);
        }
    }

    //Event Handlers
    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
        //reset
        tempVal = "";
        if (qName.equalsIgnoreCase(KlemProperties.PROJECT_PARAMETERS)) {
            //create a new instance of employee
            
//			tempEmp.setType(attributes.getValue("type"));
        }
    }

    @Override
    public void characters(char[] ch, int start, int length) throws SAXException {
        tempVal = new String(ch, start, length);
    }

    @Override
    public void endElement(String uri, String localName, String qName) throws SAXException {

        if (qName.equalsIgnoreCase(KlemProperties.PROJECT_FOLDER)) {
            projProp.setProjectFolder(new File(tempVal));
        } else if (qName.equalsIgnoreCase(KlemProperties.BLUELINES)) {
            if(tempVal != null && tempVal.equals("true")) {
                projProp.setWithBluelines(true);
            } else {
                projProp.setWithBluelines(false);
            }
        } else if (qName.equals(KlemProperties.XCOORD)) {
            projProp.setOutletXCoord(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.YCOORD)) {
            projProp.setOutletYCoord(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.AREA)) {
            projProp.setCatchmentArea(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.ELEVATION)) {
            projProp.setOutletElevation(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.TYPE)) {
            if (tempVal.equals(RainfallType.DISTRIBUTED.toString())) {
                projProp.setRainfallType(RainfallType.DISTRIBUTED);
            } else if (tempVal.equals(RainfallType.POINT.toString())) {
                projProp.setRainfallType(RainfallType.POINT);
            } else if (tempVal.equals(RainfallType.HISTORICAL.toString())) {
                projProp.setRainfallType(RainfallType.HISTORICAL);
            }
        } else if (qName.equals(KlemProperties.MODEL)) {
            if (tempVal.equals(LsppCalculator.LsppModel.GUMBEL.toString())) {
                projProp.setLsppModel(LsppCalculator.LsppModel.GUMBEL);
            } else if (tempVal.equals(LsppCalculator.LsppModel.GEV.toString())) {
                projProp.setLsppModel(LsppCalculator.LsppModel.GEV);
            }
        } else if (qName.equals(KlemProperties.A_FILE)) {
            projProp.setAFile(tempVal);
        } else if (qName.equals(KlemProperties.N_FILE)) {
            projProp.setNFile(tempVal);
        } else if (qName.equals(KlemProperties.CV_FILE)) {
            projProp.setCvFile(tempVal);
        } else if (qName.equals(KlemProperties.RETURN_TIME)) {
            projProp.setReturnPeriod(Integer.parseInt(tempVal));
        } else if (qName.equals(KlemProperties.A_PARAM)) {
            projProp.setAParam(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.N_PARAM)) {
            projProp.setNParam(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.N_LESS_HOUR)) {
            if (tempVal.equalsIgnoreCase("auto")) {
                projProp.setnLessHourParam(projProp.getAutoNLessHour());
                projProp.setnLessHourParamAuto(true);
            } else {
                projProp.setnLessHourParam(Double.parseDouble(tempVal));
                projProp.setnLessHourParamAuto(true);
            }
        } else if (qName.equals(KlemProperties.HISTORICAL_FILE)) {
            projProp.setHistoricalRainfallFile(tempVal);
        } else if (qName.equals(KlemProperties.KINEMATICS_TYPE)) {
            if (tempVal.equals(KlemProperties.KinematicsType.SIMPLE.toString())) {
                projProp.setKinematicsType(KlemProperties.KinematicsType.SIMPLE);
            } else if (tempVal.equals(KlemProperties.KinematicsType.ADVANCED.toString())) {
                projProp.setKinematicsType(KlemProperties.KinematicsType.ADVANCED);
            }
        } else if (qName.equals(KlemProperties.SLOPE_VELOCITY)) {
            projProp.setSlopeVelocity(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.CHANNEL_VELOCITY)) {
            projProp.setChannelVelocity(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.THRESHOLD)) {
            projProp.setThresholdValue(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.MIN_SLOPE_VELOCITY)) {
            projProp.setMinSlopeVelocity(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.MAX_SLOPE_VELOCITY)) {
            projProp.setMaxSlopeVelocity(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.SLOPE_COST)) {
            projProp.setSlopeConstant(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.MIN_THRESHOLD)) {
            projProp.setMinThreshold(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.MAX_THRESHOLD)) {
            projProp.setMaxThreshold(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.THRESHOLD_COST)) {
            projProp.setThresholdConstant(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.HYETO_TYPE)) {
            if (tempVal.equals(HyetographGenerator.HyetographShape.ALTERNATINGBLOCK.toString())) {
                projProp.setHyetoType(HyetographGenerator.HyetographShape.ALTERNATINGBLOCK);
            } else if (tempVal.equals(HyetographGenerator.HyetographShape.CONSTANT.toString())) {
                projProp.setHyetoType(HyetographGenerator.HyetographShape.CONSTANT);
            } else if (tempVal.equals(HyetographGenerator.HyetographShape.INSTANT.toString())) {
                projProp.setHyetoType(HyetographGenerator.HyetographShape.INSTANT);
            } else if (tempVal.equals(HyetographGenerator.HyetographShape.TRIANGULAR.toString())) {
                projProp.setHyetoType(HyetographGenerator.HyetographShape.TRIANGULAR);
            } else if (tempVal.equals(HyetographGenerator.HyetographShape.WALLINGFORD.toString())) {
                projProp.setHyetoType(HyetographGenerator.HyetographShape.WALLINGFORD);
            } else if (tempVal.equals(HyetographGenerator.HyetographShape.INCREASING.toString())) {
                projProp.setHyetoType(HyetographGenerator.HyetographShape.INCREASING);
            }
        } else if (qName.equals(KlemProperties.PEAK)) {
            projProp.setHyetoPeakPosition(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.RAINFALL_DATA_STEP)) {
            projProp.setRainfallStep(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.RAINFALL_RECESSION)) {
            projProp.setRainfallRecessionValue(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.THRESH_RAINFALL_RECESSION)) {
            projProp.setRainfallRecessionThreshold(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.INITIAL_ABSTRACTION_VALUE)) {
            projProp.setInitialAbstractionValue(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.INITIAL_ABSTRACTION_UNIT)) {
            if (tempVal.equals(InitialAbstraction.AbstractionUnits.FRACTION.toString())) {
                projProp.setInitAbsUnit(InitialAbstraction.AbstractionUnits.FRACTION);
            } else if (tempVal.equals(InitialAbstraction.AbstractionUnits.MILLIMETERS.toString())) {
                projProp.setInitAbsUnit(InitialAbstraction.AbstractionUnits.MILLIMETERS);
            }
        } else if (qName.equals(KlemProperties.OUTPUT_STEP)) {
            projProp.setHydroStepOutput(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.AMC_VALUE)) {
            projProp.setAmcValue(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.AMC_TYPE)) {
            if (tempVal.equals(KlemProperties.Amc_Type.COSTANT.toString())) {
                projProp.setAmcType(KlemProperties.Amc_Type.COSTANT);
            } else if (tempVal.equals(KlemProperties.Amc_Type.VARIABLE.toString())) {
                projProp.setAmcType(KlemProperties.Amc_Type.VARIABLE);
            }
        } else if (qName.equals(KlemProperties.BASEFLOW_TYPE)) {
            if (tempVal.equals(BaseflowParams.BaseflowType.LUMPED.toString())) {
                projProp.setBaseFlowType(BaseflowParams.BaseflowType.LUMPED);
            } else if (tempVal.equals(BaseflowParams.BaseflowType.DISTRIBUTED.toString())) {
                projProp.setBaseFlowType(BaseflowParams.BaseflowType.DISTRIBUTED);
            }
        } else if (qName.equals(KlemProperties.BASEFLOW_VALUE)) {
            if (tempVal.equalsIgnoreCase("auto")) {
                projProp.setQ0(projProp.getAutoQ0());
                projProp.setQ0Auto(true);
            } else {
                projProp.setQ0(Double.parseDouble(tempVal));
                projProp.setQ0Auto(false);
            }
        } else if (qName.equals(KlemProperties.DISCHARGE)) {
            projProp.setBaseflowRecession(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.ARF)) {
            if (tempVal.equalsIgnoreCase("auto")) {
                projProp.setArf(projProp.getAutoArf());
                projProp.setArfAuto(true);
            } else {
                projProp.setArf(Double.parseDouble(tempVal));
                projProp.setArfAuto(false);
            }
        } else if (qName.equals(KlemProperties.FRPM_TC)) {
            projProp.setPeakFraction(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.SUB_SURF_DRAIN_LOSS)) {
            projProp.setSubSurfaceDrainageLoss(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.GEOMOPRH_FACTOR)) {
            projProp.setGeomorphFactor(Double.parseDouble(tempVal));
        } else if (qName.equals(KlemProperties.THRESH_GEOMORPH_FACTOR)) {
            projProp.setGeomorphoFactorThreshold(Double.parseDouble(tempVal));
        }

    }

    public KlemProperties getKlemProjectPropertiesTemp() {
        return projProp;
    }

}
