package es.unex.sextante.gui.settings;

import java.io.File;
import java.util.HashMap;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;

public class SextanteModelerSettings
extends
Setting {

	public static final String MODELS_PORTABLE	= "ModelsPortable";
	public static final String MODELS_FOLDER 	= "ModelsFolder";


	@Override
	public void createPanel() {

		panel = new SextanteModelerSettingsPanel();

	}


	@Override
	public String getName() {

		return Sextante.getText("Models");

	}


	@Override
	public HashMap<String, String> getInitValues() {

		final HashMap<String, String> map = new HashMap<String, String>();
		map.put(MODELS_PORTABLE, Boolean.TRUE.toString());
		map.put(MODELS_FOLDER, SextanteGUI.getSextantePath() + File.separator + Sextante.PORTABLE_MODELS_FOLDER);

		return map;

	}

}
