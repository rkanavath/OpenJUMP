

package es.unex.sextante.gui.settings;

import java.io.File;
import java.util.HashMap;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;


public class SextanteGrassSettings
extends
Setting {

	public static final String GRASS_ACTIVATE        = "GrassActivate";
	public static final String GRASS_PORTABLE        = "GrassPortable";
	public static final String GRASS_FOLDER          = "GrassBinariesFolder";
	public static final String GRASS_WIN_SHELL       = "GrassWinShell";
	public static final String GRASS_LAT_LON_MODE    = "GrassLatLonMode";
	public static final String GRASS_3D_V_MODE       = "Grass3DVMode";
	public static final String GRASS_NO_VECT_BBOX    = "GrassNoVectBBox";
	public static final String GRASS_IN_POLYLINES    = "GrassInPolylines";
	public static final String GRASS_CLEAN_POLYGONS  = "GrassCleanPolygons";
	public static final String GRASS_USE_SEXTANTE_NULL  = "GrassUseSextanteNull";	
	public static final String GRASS_COMPATIBILITY_MODE  = "GrassCompatibilityMode";


	@Override
	public HashMap<String, String> getInitValues() {

		final HashMap<String, String> map = new HashMap<String, String>();
		map.put(GRASS_ACTIVATE, Boolean.FALSE.toString());
		map.put(GRASS_PORTABLE, Boolean.TRUE.toString());
		map.put(GRASS_FOLDER, SextanteGUI.getSextantePath() + File.separator + Sextante.PORTABLE_GRASS_FOLDER);
		map.put(GRASS_WIN_SHELL, SextanteGUI.getSextantePath() + File.separator + Sextante.PORTABLE_MSYS_FILE);
		map.put(GRASS_LAT_LON_MODE, Boolean.FALSE.toString());
		map.put(GRASS_3D_V_MODE, Boolean.FALSE.toString());
		map.put(GRASS_NO_VECT_BBOX, Boolean.FALSE.toString());
		map.put(GRASS_IN_POLYLINES, Boolean.FALSE.toString());
		map.put(GRASS_CLEAN_POLYGONS, Boolean.FALSE.toString());
		map.put(GRASS_USE_SEXTANTE_NULL, Boolean.FALSE.toString());
		map.put(GRASS_COMPATIBILITY_MODE, Boolean.FALSE.toString());

		return map;

	}


	@Override
	public void createPanel() {

		panel = new SextanteGrassSettingsPanel();

	}


	@Override
	public String getName() {

		return "GRASS";

	}

}
