package com.geomaticaeambiente.klemgui.utils;

import com.geomaticaeambiente.klemgui.plugin.hydrology.hydrographs.HydroOutputPanel;
import com.geomaticaeambiente.klemgui.ui.GUIUtils;
import com.geomaticaeambiente.klemgui.ui.InitialDialog;
import com.geomaticaeambiente.klemgui.ui.LayerablesList;
import static com.geomaticaeambiente.klemgui.utils.HydroUtils.HydroType.SCS;
import com.geomaticaeambiente.openjump.klem.hydrology.Hydrology.EffectiveRainfall;
import com.geomaticaeambiente.openjump.klem.hydrology.UnitHydrograph;
import com.geomaticaeambiente.openjump.klem.units.Length;
import com.geomaticaeambiente.openjump.klem.units.Speed;
import com.geomaticaeambiente.openjump.klem.units.Time;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import it.geomaticaeambiente.klem.Hyetograph;
import it.geomaticaeambiente.klem.TimeInterval;
import javax.swing.JTabbedPane;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableModel;

/**
 *
 * @author Geomatica
 */
public class HydroUtils {
    
     public static DefaultTableModel setTableModel(UnitHydrograph unitHydrograph,
             TimeInterval.TimeIntervalUnit xUnit, String yName, boolean print){
          
        double[][] data1 = fromHydrographToData(unitHydrograph, xUnit, print);
         
        Double [][] data = new Double[data1[0].length][data1.length];
        int row = 0;        
        for(int c=0; c<data1[0].length; c++){
            int col = 0;
            for (double[] data11 : data1) {
                data[row][col] = PluginUtils.getThreeDecimalFormat(data11[c]);
                col++;
            }
            row++;
        }
        
        String time = "";
        if(xUnit == TimeInterval.TimeIntervalUnit.HOUR) {
            time = PluginUtils.getResources().getString("HyetographPlugin.Graph_TimeHour.label");
        } else if (xUnit == TimeInterval.TimeIntervalUnit.MINUTE){
            time = PluginUtils.getResources().getString("HyetographPlugin.Graph_TimeMin.label"); //TODO: modify label
            
        }
        
        DefaultTableModel dtm = new DefaultTableModel(data, new String[]{
            time,
            PluginUtils.getResources().getString("HydroUtils.Discharge.label")});
        
        return dtm;
    }
    
    
    private static double [][] fromHydrographToData(UnitHydrograph unitHydrograph, TimeInterval.TimeIntervalUnit xUnit, boolean printable){
        
        double[][] data = new double[2][unitHydrograph.getUnitHydrograph()[0].length];
        for(int s=0; s<unitHydrograph.getUnitHydrograph()[0].length; s++) {
            data[0][s] = unitHydrograph.getUnitHydrograph()[0][s];
            data[1][s] = unitHydrograph.getUnitHydrograph()[1][s];
        }
        
        if(printable == false){
            // Units are minutes: convert
            if(xUnit == TimeInterval.TimeIntervalUnit.MINUTE) {
    //            timeStep /= 60;
                for(int d=0; d<data[0].length; d++) {
                    data[0][d] *= 60;
                }
            }
        }
        
        return data;
        
    }
    
    public static String[] setHydrographHeaderString(HydroType hydroType){
        
        String type = getHydroTypeAsString(hydroType);
        String[] headerRows = new String[]{
                PluginUtils.getResources().getString("KlemGUI.HydrographTypeLabel.label") + type,
                PluginUtils.getResources().getString("HydroUtils.TimeH.label") + "," + PluginUtils.getResources().getString("HydroUtils.Discharge.label")};
        
        return headerRows;
    }
    
    
    private static String getHydroTypeAsString(HydroType type){
        
        switch(type){
            case SCS:{
                return PluginUtils.getResources().getString("HydroUtils.SCS.label");
            }
            case TRIANGULAR : {
                return PluginUtils.getResources().getString("HydroUtils.Triangular.label");
            }
            case NASH : {
                return PluginUtils.getResources().getString("HydroUtils.Nash.label");
            }
            case GEOMORPHOLOGICAL: {
                return PluginUtils.getResources().getString("HydroUtils.Geomorphological.label");
            }
            case KINEMATIC:{
                return PluginUtils.getResources().getString("HydroUtils.Kinematic.label");
            }
        }
        return null;
    }
    
    public static InitialData getOuputHydrographComponents(Hyetograph hyetograph, EffectiveRainfall effectiveRainfall, 
            UnitHydrograph unitHydrograph, TimeInterval.TimeIntervalUnit xUnit, HydroType type){
        
        InitialData initialData = new InitialData();
            
        //output data
        PersonalChart personalChart = new PersonalChartHydrograph(hyetograph,effectiveRainfall, unitHydrograph, xUnit);
        initialData.setParam_ChartPanel(personalChart, GUIUtils.OUTPUT);

        PersonalTable personalTable = new PersonalTable(
                HydroUtils.setTableModel(
                        unitHydrograph, xUnit,
                        PluginUtils.getResources().getString("HyetographPlugin.Graph_Rainfall.label"),
                        false),
                new Header(HydroUtils.setHydrographHeaderString(type)),
                false, true, false, false, false, null, new String[]{"txt", "csv"}, false);

        //set values for save file
        personalTable.setPrintableDefaultTableModel(HydroUtils.setTableModel(unitHydrograph, xUnit, PluginUtils.getResources().getString("HyetographPlugin.Graph_Rainfall.label"), true));

        initialData.setParam_PersonalTable(personalTable, new FileNameExtensionFilter("Hydrograph", new String[]{"txt","csv"}), GUIUtils.OUTPUT); //table

        return initialData;
        
    }
    
    public static void addOuputPanel(PlugInContext context, InitialDialog initialdialog, Hyetograph hyetograph, EffectiveRainfall effectiveRainfall, 
            UnitHydrograph unitHydrograph, TimeInterval.TimeIntervalUnit xUnit, HydroType type, LayerablesList layerablesList) throws Exception{
        
        JTabbedPane mainTabelPane = initialdialog.getTabbedPane();
        HydroOutputPanel outputPanel = new HydroOutputPanel(context, initialdialog, hyetograph, effectiveRainfall, unitHydrograph, xUnit, type, layerablesList);
        mainTabelPane.setComponentAt(1, outputPanel.getTabPluginComponents());
        mainTabelPane.setEnabledAt(1, true);
        mainTabelPane.setSelectedIndex(1);
        
    }
    
    public static boolean checkRangeParameter(double value, double minValue, double maxValue){     
        return !(value < minValue || value > maxValue);
    }
    
    public static void checkHydroValue(String field, double value, double minValue, double maxValue){
        
        if(!checkRangeParameter(value, minValue, maxValue)) throw new IllegalArgumentException(
            PluginUtils.getResources().getString("HydroUtils.ThresholdValue_TheValue.message")
                        .concat(field)
                        .concat(PluginUtils.getResources().getString("HydroUtils.ThresholdValue_MustBe.message"))
                        .concat(Double.toString(0d))
                        .concat(PluginUtils.getResources().getString("HydroUtils.ThresholdValue_and.message"))
                        .concat(Double.toString(maxValue))
        );
  
    }
    
    
    public static Speed calculateSpeed(double velocity){
        
        double lenght = velocity;
        double _time = 1;
        
        Length length = new Length(lenght, Length.LengthUnit.m);
        Time time = new Time(_time, Time.TimeIntervalUnit.s);
        
        return new Speed(length, time);
    }
   
    public enum HydroType {SCS, TRIANGULAR, NASH, GEOMORPHOLOGICAL, KINEMATIC, HORTON_TRIANGULAR};
    
}
