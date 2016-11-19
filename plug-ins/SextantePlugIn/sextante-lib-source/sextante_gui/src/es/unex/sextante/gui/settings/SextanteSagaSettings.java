

package es.unex.sextante.gui.settings;

import java.io.File;
import java.util.HashMap;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;


public class SextanteSagaSettings
extends
Setting {

	public static final String SAGA_ACTIVATE      = "SagaActivate";
	public static final String SAGA_PORTABLE      = "SagaPortable";
	public static final String SAGA_FOLDER        = "SagaFolder";
	public static final String SAGA_MODS_FOLDER   = "SagaModsFolder";


	@Override
	public void createPanel() {

		panel = new SextanteSagaSettingsPanel();

	}


	@Override
	public String getName() {

		return "SAGA";

	}


	@Override
	public HashMap<String, String> getInitValues() {

		final HashMap<String, String> map = new HashMap<String, String>();
		map.put(SAGA_ACTIVATE, Boolean.FALSE.toString());
		map.put(SAGA_PORTABLE, Boolean.TRUE.toString());		
		map.put(SAGA_FOLDER, SextanteGUI.getSextantePath() + File.separator + Sextante.PORTABLE_SAGA_FOLDER);
		map.put(SAGA_MODS_FOLDER, SextanteGUI.getSextantePath() + File.separator + Sextante.PORTABLE_SAGA_MODS_FOLDER);
		return map;

	}

}
