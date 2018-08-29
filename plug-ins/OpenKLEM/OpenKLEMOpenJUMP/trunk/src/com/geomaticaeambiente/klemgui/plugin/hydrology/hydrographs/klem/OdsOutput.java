package com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.klem;

import it.geomaticaeambiente.klem.DesignRain;
import it.geomaticaeambiente.klem.Klem;
import it.geomaticaeambiente.klem.SimulationOutput;
import it.geomaticaeambiente.klem.SimulationOutput.SimulationDischarge;
import it.geomaticaeambiente.klem.TimeInterval;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import org.odftoolkit.simple.SpreadsheetDocument;
import org.odftoolkit.simple.table.Table;



public class OdsOutput {

    public static void createOds(Klem klem, KlemProperties klemProps,
            File odsModel, File odsOut) throws FileNotFoundException, IOException, Exception  {

        
        SpreadsheetDocument spreadSheet = SpreadsheetDocument.loadDocument(odsModel);
        Table sheet = spreadSheet.getSheetByIndex(0);
        
        // Hyetograph and hydrograph
        int row = 1;
        boolean directWasOn = false;
        
        SimulationOutput simOut = klem.getSimulationOutput();
        SimulationDischarge simDischarge = simOut.getSimulationDischarge();
        int nSteps = simDischarge.getTflo_out().length;
        int directOffIndex = 0;
        for(int o=0; o<nSteps; o++) {
            
            double hour = o * simOut.getSimulationDischarge().getTimeInterval().getInterval(TimeInterval.TimeIntervalUnit.HOUR);
                    
            sheet.getCellByPosition(TIME_COL, row).setDoubleValue(hour);
            sheet.getCellByPosition(QTOT_COL, row).setDoubleValue(simDischarge.getTflo_out()[o]);
           
            double qDir = simDischarge.getDflo_out()[o];
            sheet.getCellByPosition(QDIR_COL, row).setDoubleValue(qDir);
            
            if(qDir > 0) {
                directWasOn = true;
            }
            
            if(qDir == 0 && directWasOn) {
                directOffIndex = row;
                directWasOn = false;
            }
            
            if(simDischarge.getBflo_out()[o] == 0 && directOffIndex > 0) {
                break;
            }
            
            sheet.getCellByPosition(QBAS_COL, row).setDoubleValue(simDischarge.getBflo_out()[o]);
            
            sheet.getCellByPosition(PTOT_COL, row).setDoubleValue(simOut.getSimulationRainfall().getTotalRain()[o]);
            sheet.getCellByPosition(PEFF_COL, row).setDoubleValue(simOut.getSimulationRainfall().getEffectiveRain()[o]);
            
            row++;
            //sheet.setRowCount(sheet.getRowCount() + 1);
        }

        // Discharge data
        sheet.getCellByPosition(OVERALL_DIRECT_COL, RAINFALL_ROW)
                .setDoubleValue(simOut.getSimulationRainfall().getCumulativeEffectiveRain()); //direct rainfall
        sheet.getCellByPosition(OVERALL_BASE_COL, RAINFALL_ROW)
                .setDoubleValue(simOut.getSimulationRainfall().getCumulativeStoreRain()); //base rainfall
        sheet.getCellByPosition(OVERALL_TOTAL_COL, RAINFALL_ROW)
                .setDoubleValue(simOut.getSimulationRainfall().getCumulativeTotalRain()); //total rainfall       
        
        sheet.getCellByPosition(OVERALL_DIRECT_COL, INIT_DISCHARGE_ROW)
                .setDoubleValue(simOut.getSimulationDischarge().getDflo_out()[0]); //portata iniziale diretta
        sheet.getCellByPosition(OVERALL_BASE_COL, INIT_DISCHARGE_ROW)
                .setDoubleValue(simOut.getSimulationDischarge().getBflo_out()[0]); //portata iniziale di base
        sheet.getCellByPosition(OVERALL_TOTAL_COL, INIT_DISCHARGE_ROW)
                .setDoubleValue(simOut.getSimulationDischarge().getTflo_out()[0]); //portata iniziale totale 
        
        sheet.getCellByPosition(OVERALL_DIRECT_COL, MAX_DISCHARGE_ROW)
                .setDoubleValue(simOut.getSimulationDischarge().getDflo_max()); //portata massima diretta
        sheet.getCellByPosition(OVERALL_BASE_COL, MAX_DISCHARGE_ROW)
                .setDoubleValue(simOut.getSimulationDischarge().getBflo_max()); //portata massima di base
        sheet.getCellByPosition(OVERALL_TOTAL_COL, MAX_DISCHARGE_ROW)
                .setDoubleValue(simOut.getSimulationDischarge().getTflo_max()); //portata massima totale 
        
        sheet.getCellByPosition(OVERALL_DIRECT_COL, AVG_DISCHARGE_ROW
        ).setDoubleValue(simOut.getSimulationDischarge().getDflo_med()); //portata media diretta
        sheet.getCellByPosition(OVERALL_BASE_COL, AVG_DISCHARGE_ROW)
                .setDoubleValue(simOut.getSimulationDischarge().getBflo_med()); //portata media di base
        sheet.getCellByPosition(OVERALL_TOTAL_COL, AVG_DISCHARGE_ROW)
                .setDoubleValue(simOut.getSimulationDischarge().getTflo_med()); //portata media totale 
        
        sheet.getCellByPosition(OVERALL_DIRECT_COL, PEAK_TIME_ROW)
                .setDoubleValue(simOut.getSimulationDischarge().getDirectFlowPeakTime().getInterval(TimeInterval.TimeIntervalUnit.HOUR)); //tempo picco diretta
        sheet.getCellByPosition(OVERALL_BASE_COL, PEAK_TIME_ROW)
                .setDoubleValue(simOut.getSimulationDischarge().getBaseFlowPeakTime().getInterval(TimeInterval.TimeIntervalUnit.HOUR)); //tempo picco di base
        sheet.getCellByPosition(OVERALL_TOTAL_COL, PEAK_TIME_ROW)
                .setDoubleValue(simOut.getSimulationDischarge().getTotalFlowPeakTime().getInterval(TimeInterval.TimeIntervalUnit.HOUR)); //tempo picco totale 
                
        
        // Simulation parameters
        int rowIndex = 11;
        sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(simOut.getSimulationDischarge().getBflo_out()[1]); //baseflow
        sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getBaseflowParams().getRecession()); //costante esaurimento defl base
        sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getAmcValue()); //AMC
        sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(simOut.getSimulationDischarge().getAppearingCN()); // cn apparente
        sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getInitialAbstractionValue()); //initial abstraction
        sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getCatchmentArea()); //basin area
        sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(simOut.getSimulationDischarge().getContributingAreaPercentage()); //area contribuente
        sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getChannelVelocity()); //channel velocity
        
        if(klemProps.getKinematicsType() == KlemProperties.KinematicsType.ADVANCED) {
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getMinSlopeVelocity()); // min slope velocity
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getMaxSlopeVelocity()); // max slope velocity
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getSlopeConstant()); // slope vel. K
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getMinThreshold()); //min threshold
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getMaxThreshold()); //soglia massima versante canale
        } else {
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getSlopeVelocity()); // slope velocity
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getSlopeVelocity()); // slope velocity
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getSlopeConstant()); // slope vel. K
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getThresholdValue()); //min threshold
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getThresholdValue()); //soglia massima versante canale
        }
        
        sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getThresholdConstant()); // threshold K const.
        
        if(klemProps.getRainfallType() == KlemProperties.RainfallType.POINT ||
                klemProps.getRainfallType() == KlemProperties.RainfallType.DISTRIBUTED ){
        
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klem.getCriticalDuration().getInterval(TimeInterval.TimeIntervalUnit.HOUR)); //durata precipitazione critica
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getLsppCalculator().getParamsAN().getParamA()); //a value
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getLsppCalculator().getParamsAN().getParamN()); //n
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getLsppCalculator().getParamsAN().getParamNLess1Hour()); //n<1h
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(DesignRain.calcFattAtten(klemProps.getCatchmentArea(), klem.getCriticalDuration())); //ARF
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setStringValue(klemProps.getHyetoShape().toString()); //hyeto shape
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getPeakFraction()); //riduzione pioggia
            if (klemProps.getLsppModel() != null) {
                sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setStringValue(klemProps.getLsppModel().toString()); //LSPP Model
            } else {
                sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setStringValue("-"); //LSPP Model
            }
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getGeomorphFactor()); //geomorph factor
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getGeomorphoFactorThreshold()); //threshold geomorph factor
            sheet.getCellByPosition(PARAMETERS_COL, rowIndex++).setDoubleValue(klemProps.getHyetoPeakPosition()); //avanzamento ietogramma
            
        } else {
            
            clearCells(sheet, PARAMETERS_COL, PARAMETERS_NAME_COL, rowIndex, rowIndex + 11);
            
        }
        
        spreadSheet.save(odsOut);        
        
    }
    
    private static void clearCells(Table sheet, int fromCol, int toCol, int fromRow, int toRow) {
        
        for(int r=fromRow; r<=toRow; r++) {
            for(int c=fromCol; c<=toCol; c++) {
                sheet.getCellByPosition(c, r).removeContent();
                //cell.clearValue();
            }
        }
        
    }
    
    public static final int TIME_COL = 0;
    public static final int QTOT_COL = 1;
    public static final int QDIR_COL = 2;
    public static final int QBAS_COL = 3;
    public static final int PTOT_COL = 4;
    public static final int PEFF_COL = 5;

    public static final int OVERALL_DIRECT_COL = 15;
    public static final int OVERALL_BASE_COL = 16;
    public static final int OVERALL_TOTAL_COL = 17;
    
    public static final int RAINFALL_ROW = 1;
    public static final int INIT_DISCHARGE_ROW = 2;
    public static final int MAX_DISCHARGE_ROW = 3;
    public static final int AVG_DISCHARGE_ROW = 4;
    public static final int PEAK_TIME_ROW = 5;
    
    public static final int PARAMETERS_NAME_COL = 14;
    public static final int PARAMETERS_COL = 15;
    
}

