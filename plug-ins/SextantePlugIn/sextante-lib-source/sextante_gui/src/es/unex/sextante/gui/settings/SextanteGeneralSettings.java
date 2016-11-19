package es.unex.sextante.gui.settings;

import java.util.HashMap;

import es.unex.sextante.core.Sextante;

public class SextanteGeneralSettings
         extends
            Setting {

   public static final String RESULTS_FOLDER 				= "ResultsFolder";
   public static final String MODIFY_NAMES          		= "ModifyNames";
   public static final String USE_INTERNAL_NAMES    		= "UseInternalNames";
   public static final String DEFAULT_NO_DATA_VALUE 		= "NoDataValue";
   public static final String SHOW_MOST_RECENT      		= "ShowMostRecent";
   public static final String MONITOR_CLOSE_BY_DEFAULT 		= "MonitorCloseByDefault";
   public static final String MONITOR_DETAILS_BY_DEFAULT 	= "MonitorDetailsByDefault";


   @Override
   public String getName() {
      return Sextante.getText("General");
   }


   @Override
   public void createPanel() {

      panel = new SextanteGeneralSettingsPanel();

   }


   @Override
   public HashMap<String, String> getInitValues() {

      final HashMap<String, String> map = new HashMap<String, String>();
      map.put(RESULTS_FOLDER, System.getProperty("user.home"));
      map.put(MODIFY_NAMES, Boolean.FALSE.toString());
      map.put(USE_INTERNAL_NAMES, Boolean.FALSE.toString());
      map.put(USE_INTERNAL_NAMES, Boolean.FALSE.toString());
      map.put(SHOW_MOST_RECENT, Boolean.TRUE.toString());
      map.put(DEFAULT_NO_DATA_VALUE, ""+Sextante.PRESET_NO_DATA);
      map.put(MONITOR_CLOSE_BY_DEFAULT, Boolean.TRUE.toString());
      map.put(MONITOR_DETAILS_BY_DEFAULT, Boolean.FALSE.toString());

      return map;

   }


}
