package es.unex.sextante.gui.settings;

import java.io.File;
import java.util.HashMap;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;


public class SextanteRSettings
         extends
            Setting {
   
   public static final String R_ACTIVATE       = "RActivate";
   public static final String R_PORTABLE       = "RPortable";
   public static final String R_FOLDER         = "RFolder";
   public static final String R_SCRIPTS_FOLDER = "RScriptsFolder";


   @Override
   public void createPanel() {

      panel = new SextanteRSettingsPanel();

   }


   @Override
   public String getName() {

      return "R";

   }


   @Override
   public HashMap<String, String> getInitValues() {

      final HashMap<String, String> map = new HashMap<String, String>();
      map.put(R_ACTIVATE, Boolean.FALSE.toString());
      map.put(R_PORTABLE, Boolean.TRUE.toString());
      map.put(R_FOLDER, SextanteGUI.getSextantePath() + File.separator + Sextante.PORTABLE_R_FOLDER);            
      map.put(R_FOLDER, SextanteGUI.getSextantePath() + File.separator + Sextante.PORTABLE_R_SCRIPTS_FOLDER);
      return map;

   }

}
