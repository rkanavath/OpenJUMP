package es.unex.sextante.gui.grass;


import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JOptionPane;

import org.kxml2.io.KXmlParser;

import es.unex.sextante.additionalInfo.AdditionalInfo;
import es.unex.sextante.additionalInfo.AdditionalInfoBand;
import es.unex.sextante.additionalInfo.AdditionalInfoBoolean;
import es.unex.sextante.additionalInfo.AdditionalInfoFilepath;
import es.unex.sextante.additionalInfo.AdditionalInfoImageLayer;
import es.unex.sextante.additionalInfo.AdditionalInfoMultipleInput;
import es.unex.sextante.additionalInfo.AdditionalInfoNumericalValue;
import es.unex.sextante.additionalInfo.AdditionalInfoRasterLayer;
import es.unex.sextante.additionalInfo.AdditionalInfoSelection;
import es.unex.sextante.additionalInfo.AdditionalInfoString;
import es.unex.sextante.additionalInfo.AdditionalInfoVectorLayer;
import es.unex.sextante.core.GeoAlgorithm;
import es.unex.sextante.core.OutputObjectsSet;
import es.unex.sextante.core.ParametersSet;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.dataObjects.IDataObject;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.dataObjects.IVectorLayer;
import es.unex.sextante.dataObjects.vectorFilters.*;
import es.unex.sextante.exceptions.GeoAlgorithmExecutionException;
import es.unex.sextante.exceptions.RepeatedParameterNameException;
import es.unex.sextante.exceptions.WrongParameterTypeException;
import es.unex.sextante.gui.algorithm.iterative.SingleFeatureVectorLayer;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.settings.SextanteGrassSettings;
import es.unex.sextante.outputs.*;
import es.unex.sextante.parameters.Parameter;
import es.unex.sextante.parameters.ParameterBand;
import es.unex.sextante.parameters.ParameterBoolean;
import es.unex.sextante.parameters.ParameterDataObject;
import es.unex.sextante.parameters.ParameterFilepath;
import es.unex.sextante.parameters.ParameterImageLayer;
import es.unex.sextante.parameters.ParameterMultipleInput;
import es.unex.sextante.parameters.ParameterNumericalValue;
import es.unex.sextante.parameters.ParameterRasterLayer;
import es.unex.sextante.parameters.ParameterSelection;
import es.unex.sextante.parameters.ParameterString;
import es.unex.sextante.parameters.ParameterVectorLayer;


/**
 * A geoalgorithm that wraps a grass algorithm
 * 
 * @author volaya
 * 
 */
public class GrassAlgorithm
extends
GeoAlgorithm {

	private static String         encoding                              = "ISO-8859-1";

	private static final String   PARAMETER                             = "parameter";
	private static final String   DESCRIPTION                           = "description";
	private static final String   LABEL                                 = "label";
	private static final String   TASK                                  = "task";
	private static final String   NAME                                  = "name";
	private static final String   TYPE                                  = "type";
	private static final String   REQUIRED                              = "required";
	private static final String   MULTIPLE                              = "multiple";
	private static final String   VALUES                                = "values";
	private static final String   GISPROMPT                             = "gisprompt";
	private static final String   AGE                                   = "age";
	private static final String   ELEMENT                               = "element";
	private static final String   DEFAULT                               = "default";
	private static final String   KEYDESC                               = "keydesc";
	private static final String   FLAG                                  = "flag";
	private static final String   BOOLEAN                               = "boolean";
	private static final String   PROMPT                                = "prompt";

	private String                m_sDescriptionFile;

	private String                m_sParameterName;
	private String                m_sParameterType;
	private String                m_sParameterRequired;
	private String                m_sParameterMultiple;
	private ArrayList<String>     m_Values;
	private String                m_sGisPromptAge;
	private String                m_sGisPromptElement;
	private String                m_sGisPromptPrompt;
	private String                m_sDescription;
	private String                m_sTooltip                            = null;
	private String                m_sDefaultValue;
	private Boolean               m_bKeydesc;

	private String                m_sLastTag;
	
	private String                m_sTemp = null;

	//These arrays map external data files to GRASS map names
	private ArrayList<String>     m_FilesIn;
	private ArrayList<String>     m_MapsIn;

	private boolean               m_bIsExecutedFromModeller;

	//Extensions of vector file formats for which OGR provides read support
	private static final String[] ext_ogr_in = 
		{ "shp", "mif", "tab", "e00", "ntf", "000", "ddf",
		"dgn", "bna", "gml", "kml", "gpx", "geojson", "gmt", "dat", "gxt" };

	//Extensions of vector file formats for which OGR provides write support
	private static final String[] ext_ogr_out = 
		{ "shp", "mif", "tab", "dgn", "bna", "gml", "kml", "gpx", "geojson", "gmt", "gxt" };

	//Required for Modeler: multi-geom output is not allowed, because the next algorithm
	//in the modeling flow has to be able to rely on one specific geometry type being
	//present in the data input stream. Therefore, the user must restrict the output
	//to a specific type.
	public static final String    PARAMETER_RESTRICT_VECTOR_OUTPUT_TYPE = "PARAMETER_RESTRICT_VECTOR_OUTPUT_TYPE";

	//For process communication: This is the name of the process that
	//must be killed:
	private String m_sPName = null;


	/**
	 * Initializes the geoalgorithm from a grass description file, generated using the --interface-description modifier
	 * 
	 * @param sDescriptionFile
	 *                the description file
	 * @throws UnwrappableGrassProcessException
	 */
	public void initialize(final String sDescriptionFile) throws UnwrappableGrassProcessException {

		m_sDescriptionFile = sDescriptionFile;

		m_Parameters = new ParametersSet();
		m_OutputObjects = new OutputObjectsSet();

		defineCharacteristicsFromGrassXML();

	}


	/*
	 * Updates the progress bar of the ProgressMonitor object for this algorithm.
	 */
	public void updateProgress(final int i, final int n)
	{
		setProgress(i, n);
	}


	/**
	 * Registers a new "mapping", that is a relation of the form: external file <-> GRASS map. This is important for substituting
	 * the INPUT map name values in the GRASS module to run. Note: Take care to always associate a GRASS map with a full path
	 * specification, NOT just the actual filename! Otherwise, if the user adds multiple input files which have the same name but
	 * are in different directories, then there will be ambiguity and that means trouble!
	 * 
	 * @param sFileName
	 *                Name of external geodata file
	 * 
	 * @param sMapName
	 *                Name of GRASS map under which the imported file was stored
	 * 
	 */
	private void registerInMapping(final String sFileName, final String sMapName)
	{
		if ((sFileName != null) && (sMapName != null)) {
			m_FilesIn.add(sFileName);
			m_MapsIn.add(sMapName);
		}
	}


	/**
	 * Returns the name of the GRASS map that corresponds to an imported external data file.
	 * Returns null on error.
	 * 
	 * @param sFileName
	 *                Name of external geodata file
	 * 
	 */
	private String getInFile(final String sFileName)
	{
		//DEBUG
		Sextante.addInfoToLog("*** DEBUG: LOOKUP MAPPING: " + sFileName);
		for (int i = 0; i < m_FilesIn.size(); i++) {
			//DEBUG
			Sextante.addInfoToLog("*** CURRENT: " + m_FilesIn.get(i));
			if (m_FilesIn.get(i).equals(sFileName)) {
				//DEBUG
				Sextante.addInfoToLog("*** FOUND MAP : " + m_MapsIn.get(i));
				return m_MapsIn.get(i);
			}
		}
		return null;
	}


	/**
	 * Returns the name of the data file that corresponds to a GRASS map. 
	 * Returns null on error.
	 * 
	 * @param sMapName
	 *                Name of GRASS map
	 * 
	 */
	private String getInMap(final String sMapName) {
		for (int i = 0; i < m_MapsIn.size(); i++) {
			if (m_MapsIn.get(i).equals(sMapName)) {
				return m_FilesIn.get(i);
			}
		}
		return null;
	}


	private void defineCharacteristicsFromGrassXML() throws UnwrappableGrassProcessException {

		//output extent is needed to set the GRASS region
		setUserCanDefineAnalysisExtent(true);

		final File file = new File(m_sDescriptionFile);
		final KXmlParser parser = new KXmlParser();
		String modName = null;

		try {
			parser.setInput(new FileInputStream(file), encoding);

			int tag = parser.nextTag();
			boolean bOut = false;

			m_bKeydesc = false;//Indicates whether this is a complex numerical range or n-tuple
			if (parser.getEventType() != KXmlParser.END_DOCUMENT) {
				while ((tag != KXmlParser.END_DOCUMENT) && !bOut) {
					switch (tag) {
					case KXmlParser.START_TAG:
						m_sLastTag = parser.getName();
						if (parser.getName().compareTo(TASK) == 0) {
							final String sName = parser.getAttributeValue("", NAME);
							setName(sName);
							if (sName.startsWith("r")) {
								setGroup("Raster (r.*)");
							}
							else if (sName.startsWith("v")) {
								setGroup("Vector (v.*)");
							}
							else if (sName.startsWith("i")) {
								setGroup("Imagery (i.*)");
							}
							modName = new String(sName);
						}
						else if (parser.getName().compareTo(PARAMETER) == 0) {
							m_sParameterName = parser.getAttributeValue("", NAME);
							m_sParameterType = parser.getAttributeValue("", TYPE);
							m_sParameterRequired = parser.getAttributeValue("", REQUIRED);
							m_sParameterMultiple = parser.getAttributeValue("", MULTIPLE);
							m_sGisPromptAge = "";
							m_sGisPromptElement = "";
							m_sGisPromptPrompt = "";
							m_sDefaultValue = "";
							m_Values = null;
							m_sTooltip = null;
						}
						else if (parser.getName().compareTo(FLAG) == 0) {
							m_sParameterName = parser.getAttributeValue("", NAME);
							m_sParameterType = BOOLEAN;
							m_sTooltip = null;
						}
						else if (parser.getName().compareTo(GISPROMPT) == 0) {
							m_sGisPromptAge = parser.getAttributeValue("", AGE);
							m_sGisPromptElement = parser.getAttributeValue("", ELEMENT);
							m_sGisPromptPrompt = parser.getAttributeValue("", PROMPT);
						}
						else if (parser.getName().compareTo(VALUES) == 0) {
							m_Values = new ArrayList<String>();
						}
						break;
					case KXmlParser.END_TAG:
						// Arrived at end of current element (tag)
						if (parser.getName().compareTo(TASK) == 0) {
							// End of module description: no further processing
							bOut = true;
						}
						if (parser.getName().compareTo(PARAMETER) == 0) {
							// End of parameter (option) tag: create parameter!
							createParameter();
						}
						if (parser.getName().compareTo(FLAG) == 0) {
							// End of flag tag: create flag!
							createParameter();
						}
						else if ((parser.getName().compareTo(DESCRIPTION) == 0) || (parser.getName().compareTo(NAME) == 0)
								|| (parser.getName().compareTo(DEFAULT) == 0) || (parser.getName().compareTo(KEYDESC) == 0)
								|| (parser.getName().compareTo(LABEL) == 0)) {
							// End of some other tag: do nothing.
							m_sLastTag = "";
						}
						break;
					case KXmlParser.TEXT:
						if (m_sLastTag.equals(LABEL)) {
							if (m_sTooltip == null) {
								m_sTooltip = parser.getText().trim();
							}
						}
						//if (m_sLastTag.equals(DESCRIPTION) && bOut == false) {
						if (m_sLastTag.equals(DESCRIPTION)) {
							m_sDescription = parser.getText().trim();
							if (m_sTooltip == null) {
								m_sTooltip = m_sDescription;
							}							
						}
						else if (m_sLastTag.equals(NAME)) {
							m_Values.add(parser.getText().trim());
						}
						else if (m_sLastTag.equals(DEFAULT)) {
							m_sDefaultValue = parser.getText().trim();
						}
						else if (m_sLastTag.equals(KEYDESC)) {
							m_bKeydesc = true;
						}
						break;
					}
					if (!bOut) {
						tag = parser.next();
					}
				}
			}

			//now we add a fixed parameter that is only used in the modeler, to restrict the geometry type in multi-geometry algorithms
			if (this.m_OutputObjects.getVectorLayersCount() != 0) {
				final String[] sOptions = new String[] { Sextante.getText("Points"), Sextante.getText("Lines"),
						Sextante.getText("Polygons") };
				m_Parameters.addSelection(PARAMETER_RESTRICT_VECTOR_OUTPUT_TYPE, Sextante.getText("Restrict_geometry_output"),
						sOptions);
			}

		}
		catch (final Exception e) {
			//Log any unwrappable modules as warnings
			if (modName != null) {
				GrassAlgorithmProvider.addWarning(Sextante.getText("grass_err_wrap_module") + 
						" '" + modName + "'. " + Sextante.getText("grass_err_wrap_module_na"));				
			}
			throw new UnwrappableGrassProcessException();
		}

	}


	/**
	 * Creates a parameter (option) for a GRASS algorithm.
	 * Since some GRASS modules make "creative" use of parameter types or cannot
	 * be used through the SEXTANTE GUI in unmodified for other reasons, this
	 * method contains a lot of additional code that modifies the parameters
	 * of selected GRASS modules to adapt them for use from within the SEXTANTE GUI.
	 * 
	 * @throws UnwrappableGrassProcessException
	 */
	private void createParameter() throws UnwrappableGrassProcessException {

		//TODO: We could actually make at least "quiet" and "verbose" available, since
		//we have a monitor window that displays the GRASS module output to stdout/stderr.
		if (m_sParameterName.equals("overwrite") || m_sParameterName.equals("quiet") || m_sParameterName.equals("verbose")) {
			return;
		}

		Parameter param = null;
		Output out = null;
		if (m_sParameterType.compareTo(BOOLEAN) == 0) {
			param = new ParameterBoolean();
			final AdditionalInfoBoolean aib = new AdditionalInfoBoolean(false);
			param.setParameterAdditionalInfo(aib);
		}
		else if (m_sParameterType.compareTo("string") == 0) { // "string" type is used for a lot of option types in GRASS! 
			//All types of parameters that are of the broad type "string",
			//But not of a specific element type.
			if ((m_sGisPromptElement == null) || m_sGisPromptElement.equals("")) {
				if ((m_Values != null) && (m_Values.size() != 0)) {//selection
					if (m_sParameterMultiple.equals("no")) {//single selection
						param = new ParameterSelection();
						final AdditionalInfoSelection ais = new AdditionalInfoSelection(m_Values.toArray(new String[0]));
						param.setParameterAdditionalInfo(ais);
					}
					else {//multiple selection
						//TODO: needs a more elegant input widget
						param = new ParameterString();
						final AdditionalInfoString ais = new AdditionalInfoString();
						ais.setDefaultString(m_sDefaultValue);
						param.setParameterAdditionalInfo(ais);
					}
				}
				else {//any other old string
					if (getName().equals("v.in.ogr")) {
						final AdditionalInfoFilepath aif = new AdditionalInfoFilepath(false, true, false, ext_ogr_in);
						param = new ParameterFilepath();
						param.setParameterAdditionalInfo(aif);
					}
					else if (getName().equals("v.out.ogr")) {
						final AdditionalInfoFilepath aif = new AdditionalInfoFilepath(false, true, false, ext_ogr_out);
						param = new ParameterFilepath();
						param.setParameterAdditionalInfo(aif);
					}
					else {//All other GRASS modules: create simple string parameter
						param = new ParameterString();
						final AdditionalInfoString ais = new AdditionalInfoString();
						ais.setDefaultString(m_sDefaultValue);
						param.setParameterAdditionalInfo(ais);
					}
				}
			}
			else {//specific string type parameters, such as raster or vector maps
				if (m_sParameterMultiple.equals("yes")) {//multiple input maps
					if (m_sGisPromptElement.equals("cell")) {
						param = new ParameterMultipleInput();
						AdditionalInfoMultipleInput aimi = null;
						aimi = new AdditionalInfoMultipleInput(AdditionalInfoMultipleInput.DATA_TYPE_RASTER,
								m_sParameterRequired.equals("yes"));
						param.setParameterAdditionalInfo(aimi);
					}
					else if (m_sGisPromptElement.equals("vector")) {
						param = new ParameterMultipleInput();
						AdditionalInfoMultipleInput aimi = null;
						aimi = new AdditionalInfoMultipleInput(AdditionalInfoMultipleInput.DATA_TYPE_VECTOR_ANY,
								m_sParameterRequired.equals("yes"));
						param.setParameterAdditionalInfo(aimi);
					}
					else if (m_sGisPromptElement.equals("dbcolumn")) {
						//TODO: Needs a more elegant input widget.
						//TODO: Question is: To which input layer do we bind the field selector? How do we know for sure?
						param = new ParameterString();
						final AdditionalInfoString ais = new AdditionalInfoString();
						ais.setDefaultString(m_sDefaultValue);
						ais.setIsField(true);
						param.setParameterAdditionalInfo(ais);
					}
					else {
						throw new UnwrappableGrassProcessException();
					}
				}
				else {
					//single input map (raster or vector)
					if (m_sGisPromptAge.startsWith("old")) {
						if (m_sGisPromptElement.equals("cell")) {
							param = new ParameterRasterLayer();
							final AdditionalInfoRasterLayer airl = 
									new AdditionalInfoRasterLayer(m_sParameterRequired.equals("yes"));
							param.setParameterAdditionalInfo(airl);
						}
						else if (m_sGisPromptElement.equals("vector")) {
							param = new ParameterVectorLayer();
							final AdditionalInfoVectorLayer aivl = new AdditionalInfoVectorLayer(
									AdditionalInfoVectorLayer.SHAPE_TYPE_ANY, m_sParameterRequired.equals("yes"));
							param.setParameterAdditionalInfo(aivl);
						}
						//imagery groups = multi-band rasters for our purposes
						else if (m_sGisPromptElement.equals("group") && !(m_sParameterName.equals("subgroup"))) {
							param = new ParameterImageLayer();
							final AdditionalInfoImageLayer aiil = 
									new AdditionalInfoImageLayer(m_sParameterRequired.equals("yes"));
							param.setParameterAdditionalInfo(aiil);
						}
						//imagery subgroup: we do not support this type of parameter
						else if (m_sGisPromptElement.equals("subgroup")) {
							param = null;
							return;
						}
						//a table field
						else if (m_sGisPromptElement.equals("dbcolumn")) {
							//TODO: needs a more elegant input widget
							//TODO: Question is: To which input layer do we bind the field selector? How do we know for sure?
							param = new ParameterString();
							final AdditionalInfoString ais = new AdditionalInfoString();
							ais.setDefaultString(m_sDefaultValue);
							ais.setIsField(true);
							param.setParameterAdditionalInfo(ais);
						}
						else if (m_sGisPromptElement.equals("file")) {
							param = new ParameterFilepath();
							if (m_sGisPromptPrompt.startsWith("dsn")) {//DSN parameter for v.in.ogr/v.out.ogr
								final AdditionalInfoFilepath aif = new AdditionalInfoFilepath(true, true, false, null);
								param.setParameterAdditionalInfo(aif);
							}
							else if (getName().equals("v.in.dxf")) {
								final String ext[] = { "dxf" };
								final AdditionalInfoFilepath aif = new AdditionalInfoFilepath(false, true, false, ext);
								param = new ParameterFilepath();
								param.setParameterAdditionalInfo(aif);
							}
							else { //all other input files
								final AdditionalInfoFilepath aif = new AdditionalInfoFilepath(false, true, false, null);
								param.setParameterAdditionalInfo(aif);
							}
						}
						else {//Any other GRASS elements: just treat them as strings
							param = new ParameterString();
							final AdditionalInfoString ais = new AdditionalInfoString();
							ais.setDefaultString(m_sDefaultValue);
							param.setParameterAdditionalInfo(ais);
						}
					}
					else {//output
						if (m_sGisPromptElement.equals("cell") || m_sGisPromptElement.equals("tiff")) {
							out = new OutputRasterLayer();
						}
						else if (m_sGisPromptElement.equals("vector")) {
							out = new OutputVectorLayer();
						}
						else if (m_sGisPromptElement.equals("file")) {
							param = new ParameterFilepath();
							final AdditionalInfoFilepath aif = new AdditionalInfoFilepath(false, false, false, null);
							param.setParameterAdditionalInfo(aif);
						}
					}
				}
			}
			if (getName().startsWith("i."))  {
				//Imagery modules: almost all of these require some
				//extra care.
				if ( m_sParameterName.equals("subgroup")) {
					//hide "subgroup" parameter
					param = null;
					return;
				}
				if ( m_sParameterName.equals("signature")										
						|| m_sParameterName.equals("insig") 
						) {
					//input signature file parameters
					final AdditionalInfoFilepath aif = new AdditionalInfoFilepath(false, true, false, null);				
					param = new ParameterFilepath();
					param.setParameterAdditionalInfo(aif);
				}
				if ( m_sParameterName.equals("outsig")
						|| m_sParameterName.equals("sigfile")
						|| m_sParameterName.equals("signaturefile")
						) {
					//output signature file parameters
					final AdditionalInfoFilepath aif = new AdditionalInfoFilepath(false, false, false, null);				
					param = new ParameterFilepath();
					param.setParameterAdditionalInfo(aif);
				}
				if (getName().equals("i.cca") ) {
					if ( m_sParameterName.equals("group") ) {
						//This module supports two to eight input bands.
						//Replace "group" with a multi-band selector.						
						final AdditionalInfoMultipleInput ai = new AdditionalInfoMultipleInput
								(AdditionalInfoMultipleInput.DATA_TYPE_BAND, true);
						param = new ParameterMultipleInput();
						param.setParameterAdditionalInfo(ai);
					}
				}
				if (getName().equals("i.cluster") ) {
					if ( m_sParameterName.equals("seed") 
							|| m_sParameterName.equals("reportfile")) {
						final AdditionalInfoFilepath aif = new AdditionalInfoFilepath(false, true, false, null);
						param = new ParameterFilepath();
						param.setParameterAdditionalInfo(aif);
					}
				}
				if (getName().equals("i.fusion.brovey") ) {
					if ( m_sParameterName.equals("ms1") ) {
						//We add one parameter to choose the raster layer that contains
						//bands ms1 - ms3.
						final AdditionalInfoRasterLayer air = new AdditionalInfoRasterLayer(true);
						final Parameter base = new ParameterRasterLayer();
						base.setParameterName("image");
						base.setParameterDescription("image");
						base.setParameterTooltip("Image that contains low-resolution bands ms1, ms2 and ms3");
						base.setParameterAdditionalInfo(air);
						try {
							m_Parameters.addParameter(base);
						}
						catch (final RepeatedParameterNameException e) {
							throw new UnwrappableGrassProcessException();
						}
					}
					if ( m_sParameterName.equals("ms1")
							|| m_sParameterName.equals("ms2")
							|| m_sParameterName.equals("ms3")
							) {
						//These are three multi-spectral bands in the base raster layer.
						final AdditionalInfoBand aib= new AdditionalInfoBand("image");
						param = new ParameterBand();
						param.setParameterAdditionalInfo(aib);
					}
				}
			}
			if (getName().equals("i.landsat.rgb") ) {
				if ( m_sParameterName.equals("red") ) {
					//We add one parameter to choose the raster layer that contains
					//bands red, green, blue.
					final AdditionalInfoRasterLayer air = new AdditionalInfoRasterLayer(true);
					final Parameter base = new ParameterRasterLayer();
					base.setParameterName("image");
					base.setParameterDescription("image");
					base.setParameterTooltip("Landsat image that contains bands red, green and blue");
					base.setParameterAdditionalInfo(air);
					try {
						m_Parameters.addParameter(base);
					}
					catch (final RepeatedParameterNameException e) {
						throw new UnwrappableGrassProcessException();
					}
				}
				if ( m_sParameterName.equals("red")
						|| m_sParameterName.equals("green")
						|| m_sParameterName.equals("blue")
						) {
					//These are three R/G/B bands in the base raster layer.
					final AdditionalInfoBand aib= new AdditionalInfoBand("image");
					param = new ParameterBand();
					param.setParameterAdditionalInfo(aib);
				}
			}
			if (getName().equals("i.cca") ) {
				if ( m_sParameterName.equals("group") ) {
					//This module supports two to eight input bands.
					//Replace "group" with a multi-band selector.						
					final AdditionalInfoMultipleInput ai = new AdditionalInfoMultipleInput
							(AdditionalInfoMultipleInput.DATA_TYPE_BAND, true);
					param = new ParameterMultipleInput();
					param.setParameterAdditionalInfo(ai);
				}
			}
			if (getName().equals("i.maxlik") ) {
				if ( m_sParameterName.equals("sigfile") ) {
					//This is an input file path
					final AdditionalInfoFilepath aif = new AdditionalInfoFilepath(false, true, false, null);				
					param = new ParameterFilepath();
					param.setParameterAdditionalInfo(aif);
				}
			}
			if (getName().equals("i.oif") || getName().equals("i.tasscap")) {
				if ( m_sParameterName.startsWith("image") ) {
					// We are interested in Landsat bands 1-5 and 7.
					// Instead of having the user specify all six bands, we take one
					// Landsat image as input and extracts the required bands later.
					if ( m_sParameterName.equals("image1") ) {
						final AdditionalInfoImageLayer aii = new AdditionalInfoImageLayer(true);
						param = new ParameterRasterLayer();
						param.setParameterAdditionalInfo(aii);
					} else {
						param = null;
						return;
					}
				}
			}
			if (getName().equals("i.pca") ) {
				if ( m_sParameterName.equals("output_prefix") ) {
					// This module creates n raster maps with a user-defined
					// output prefix. We replace the respective parameter
					// with an additional raster output object, which will
					// have n bands.
					param = null;
					Output imageOut = null;
					imageOut = new OutputRasterLayer();
					imageOut.setName("pca_output");
					imageOut.setDescription("components");
					m_OutputObjects.add(imageOut);
					return;
				}
			}
			if (getName().equals("i.smap") ) {
				if ( m_sParameterName.equals("signaturefile") ) {
					//This is an input file path
					final AdditionalInfoFilepath aif = new AdditionalInfoFilepath(false, true, false, null);				
					param = new ParameterFilepath();
					param.setParameterAdditionalInfo(aif);
				}
			}
			if (getName().equals("i.topo.corr") ) {
				if ( m_sParameterName.equals("input") ) {
					//Replace "input" with a multi-band selector.						
					final AdditionalInfoMultipleInput ai = new AdditionalInfoMultipleInput
							(AdditionalInfoMultipleInput.DATA_TYPE_BAND, true);
					param = new ParameterMultipleInput();
					param.setParameterAdditionalInfo(ai);
				}
			}
			// Misc. other modules that require extra care
			if (getName().equals("r.xtent.cost") || getName().equals("r.xtent.cost.bat") ) {
				if ( m_sParameterName.equals("output") ) {
					final AdditionalInfoFilepath aif = new AdditionalInfoFilepath(true, true, false, null);
					param = new ParameterFilepath();
					param.setParameterAdditionalInfo(aif);
				}
			}
			if (getName().equals("r.xtent") ) {
				if ( m_sParameterName.equals("costs_input") ) {
					final AdditionalInfoFilepath aif = new AdditionalInfoFilepath(true, true, false, null);
					param = new ParameterFilepath();
					param.setParameterAdditionalInfo(aif);
				}
			}
		}
		else if (m_sParameterType.compareTo("float") == 0) {
			if (m_sParameterMultiple.equals("yes") || (m_bKeydesc == true)) {
				//handle multiple numbers, ranges and tuples as strings
				param = new ParameterString();
				final AdditionalInfoString ais = new AdditionalInfoString();
				ais.setDefaultString(m_sDefaultValue);
				param.setParameterAdditionalInfo(ais);
			}
			else {
				param = new ParameterNumericalValue();
				param.setParameterName(m_sParameterName);
				param.setParameterDescription(m_sDescription);
				double dDefaultValue;
				try {
					dDefaultValue = Double.parseDouble(m_sDefaultValue.trim());
				}
				catch (final NumberFormatException e) {
					dDefaultValue = 0;
				}
				final AdditionalInfoNumericalValue ainv = new AdditionalInfoNumericalValue(
						AdditionalInfoNumericalValue.NUMERICAL_VALUE_DOUBLE, dDefaultValue, Double.NEGATIVE_INFINITY,
						Double.MAX_VALUE);
				param.setParameterAdditionalInfo(ainv);
			}
		}
		else if (m_sParameterType.compareTo("integer") == 0) {
			if (m_sParameterMultiple.equals("yes") || (m_bKeydesc == true)) {
				//handle multiple numbers, ranges and tuples as a string
				param = new ParameterString();
				final AdditionalInfoString ais = new AdditionalInfoString();
				ais.setDefaultString(m_sDefaultValue);
				param.setParameterAdditionalInfo(ais);
			}
			else {
				int iDefaultValue;
				try {
					iDefaultValue = Integer.parseInt(m_sDefaultValue.trim());
				}
				catch (final NumberFormatException e) {
					iDefaultValue = 0;
				}
				//Skip layer=1 options
				if (!(m_sGisPromptElement.equals("layer") && (iDefaultValue == 1))) {
					param = new ParameterNumericalValue();
					param.setParameterName(m_sParameterName);
					param.setParameterDescription(m_sDescription);
					final AdditionalInfoNumericalValue ainv = new AdditionalInfoNumericalValue(
							AdditionalInfoNumericalValue.NUMERICAL_VALUE_INTEGER, iDefaultValue, 
							Integer.MIN_VALUE, Integer.MAX_VALUE);
					param.setParameterAdditionalInfo(ainv);
				}
			}
		}

		if (param != null) {
			if (param instanceof ParameterBoolean) {
				//Flags (boolean): prefix a "-" to the name to avoid collision
				//with option= parameters of the same name (e.g. v.in.ascii has "z=" and "-z"!
				param.setParameterName("-" + m_sParameterName);
				param.setParameterDescription("(-" + m_sParameterName + ") " + m_sDescription);
			}
			else {
				param.setParameterName(m_sParameterName);
				param.setParameterDescription(m_sParameterName);
			}
			param.setParameterTooltip(m_sTooltip);
			m_sTooltip = null;
			try {
				m_Parameters.addParameter(param);
			}
			catch (final RepeatedParameterNameException e) {
				throw new UnwrappableGrassProcessException();
			}			

			//modify some options that we are unable to support in their original form
			if (getName().equals("r.colors")) {
				if (param.getParameterName().equals("map")) {
					final AdditionalInfoRasterLayer airl = new AdditionalInfoRasterLayer(true);
					param.setParameterAdditionalInfo(airl);
				}
			}
			//drop some options that we are unable to support in their
			//TODO: use a helper function: dropParam ( sModuleName, sParamName[] );
			try {
				if (getName().equals("r.colors")) {
					if (param.getParameterName().equals("raster")) {
						m_Parameters.removeParameter(param);
					}
					if (param.getParameterName().equals("-r")) {
						m_Parameters.removeParameter(param);
					}
					if (param.getParameterName().equals("-w")) {
						m_Parameters.removeParameter(param);
					}
					if (param.getParameterName().equals("-i")) {
						m_Parameters.removeParameter(param);
					}
				}
				if (getName().equals("r.mapcalculator")) {
					if (param.getParameterName().equals("help")) {
						m_Parameters.removeParameter(param);
					}
					if (param.getParameterName().equals("-e")) {
						m_Parameters.removeParameter(param);
					}
					if (param.getParameterName().equals("-o")) {
						m_Parameters.removeParameter(param);
					}
				}
				if (getName().equals("r.null")) {
					if (param.getParameterName().equals("-c") || param.getParameterName().equals("-n")
							|| param.getParameterName().equals("-r")) {
						m_Parameters.removeParameter(param);
					}
				}
				if (getName().equals("r.xtent.cost")) {
					if (param.getParameterName().equals("mode")) {
						m_Parameters.removeParameter(param);
					}	
				}
				if (getName().equals("r.xtent")) {
					if (param.getParameterName().equals("costs_source")) {
						m_Parameters.removeParameter(param);
					}	
				}
				if (getName().equals("v.what.vect")) {
					if (param.getParameterName().equals("qlayer")) {
						m_Parameters.removeParameter(param);
					}
				}
			}
			catch (final Exception e) {
				throw new UnwrappableGrassProcessException();
			}

		}


		if (out != null) {
			//Set output GRASS map to a name that will have no chance to overwrite an existing map.
			//Add this stage, we combine GRASS option name and value into one string.
			//Later, we will have to split those two tokens again into separate strings.

			//volaya: No need to do all that...
			out.setName(m_sParameterName /*+ "=" + GrassUtils.getTempMapName(m_sParameterName + "_")*/);
			out.setDescription(m_sDescription /*this.getName() + ": " + m_sParameterName*/);
			m_OutputObjects.add(out);
		}
	}


	/*
	 * Process parameter values to guard against common problems.
	 * Some GRASS algorithms cannot be wrapped into a sane UI automatically.
	 * We will adjust those manually.
	 * 
	 * NOTE: At this point we cannot remove parameters from the UI anymore;
	 * they are only removed from the GRASS CLI string!
	 * 
	 * NOTE2: At this stage, there is NO access to input or output data objects,
	 * since the GRASS Map <-> file name mappings have not yet been established!
	 * Use preprocessBeforeExec() instead! 
	 * 
	 * TODO: There is too much redundant code here: write some helper functions to cut
	 * TODO: down on code complexity.
	 */
	private void preprocessParams() {
		try {			
			//r.in.gdal: correct bogus default "0" for "band")
			if (getName().equals("r.in.gdal")) {
				final Parameter band = m_Parameters.getParameter("band");
				if (band.getParameterValueAsInt() == 0) {
					m_Parameters.removeParameter("band");
				}
			}
			//r.in.srtm: drop file extension from input filename)
			if (getName().equals("r.in.srtm")) {
				final Parameter input = m_Parameters.getParameter("input");
				String filename = new String(input.getParameterValueAsString().trim());
				if (filename.endsWith(".hgt")) {
					filename = filename.substring(0, filename.lastIndexOf(".hgt"));
				}
				if (filename.endsWith(".hgt.zip")) {
					filename = filename.substring(0, filename.lastIndexOf(".hgt.zip"));
				}
			}
			//r.out.gdal: make sure to wrap "metaopt=" and "createopt=" option values in quotation marks
			if (getName().equals("r.out.gdal")) {
				final Parameter metaopt = m_Parameters.getParameter("metaopt");
				if ((metaopt.getParameterValueAsString() != null) && (metaopt.getParameterValueAsString().trim().length() > 1)) {
					if (!metaopt.getParameterValueAsString().trim().startsWith("\"")) {
						metaopt.setParameterValue("\"" + metaopt.getParameterValueAsString().trim());
					}
					if (!metaopt.getParameterValueAsString().trim().endsWith("\"")) {
						metaopt.setParameterValue(metaopt.getParameterValueAsString().trim() + "\"");
					}
				}
				final Parameter createopt = m_Parameters.getParameter("createopt");
				if ((createopt.getParameterValueAsString() != null) && (createopt.getParameterValueAsString().trim().length() > 1)) {
					if (!createopt.getParameterValueAsString().trim().startsWith("\"")) {
						createopt.setParameterValue("\"" + createopt.getParameterValueAsString().trim());
					}
					if (!createopt.getParameterValueAsString().trim().endsWith("\"")) {
						createopt.setParameterValue(createopt.getParameterValueAsString().trim() + "\"");
					}
				}
			}
			//v.in.ogr: handle DSN and LAYER name awkwardness
			if (getName().equals("v.in.ogr")) {
				final Parameter dsn = m_Parameters.getParameter("dsn");
				if ((dsn.getParameterValueAsString() == null) || (dsn.getParameterValueAsString().trim().length() < 1)) {
					final Parameter layer = m_Parameters.getParameter("layer");
					dsn.setParameterValue(layer.getParameterValueAsString());
					m_Parameters.removeParameter("layer");
				}
			}
			//v.out.ogr: handle DSN and LAYER name awkwardness
			if (getName().equals("v.out.ogr")) {
				final Parameter dsn = m_Parameters.getParameter("dsn");
				if ((dsn.getParameterValueAsString() == null) || (dsn.getParameterValueAsString().trim().length() < 1)) {
					final Parameter layer = m_Parameters.getParameter("olayer");
					final Parameter format = m_Parameters.getParameter("format");
					final String filename = new String(layer.getParameterValueAsString());
					String ext_new = "";
					//Make sure to enclose dsco= option value in quotation marks
					final Parameter dsco = m_Parameters.getParameter("dsco");
					if ((dsco.getParameterValueAsString() != null) && (dsco.getParameterValueAsString().trim().length() > 1)) {

						if (!dsco.getParameterValueAsString().trim().startsWith("\"")) {
							dsco.setParameterValue("\"" + dsco.getParameterValueAsString().trim());
						}
						if (!dsco.getParameterValueAsString().trim().endsWith("\"")) {
							dsco.setParameterValue(dsco.getParameterValueAsString().trim() + "\"");
						}
					}
					//Make sure to enclose lco= option value in quotation marks, too.
					final Parameter lco = m_Parameters.getParameter("lco");
					if ((lco.getParameterValueAsString() != null) && (lco.getParameterValueAsString().trim().length() > 1)) {
						if (!lco.getParameterValueAsString().trim().startsWith("\"")) {
							lco.setParameterValue("\"" + lco.getParameterValueAsString().trim());
						}
						if (!lco.getParameterValueAsString().trim().endsWith("\"")) {
							lco.setParameterValue(lco.getParameterValueAsString().trim() + "\"");
						}
					}
					if (format.getParameterValueAsString().equals("ESRI_Shapefile")) {
						ext_new = new String(".shp");
					}
					if (format.getParameterValueAsString().equals("MapInfo_File")) {
						ext_new = new String(".tab");
						//MapInfo files: switch to ASCII output extension
						if ((dsco.getParameterValueAsString() != null) && (dsco.getParameterValueAsString().trim().length() > 1)) {
							if (dsco.getParameterValueAsString().trim().contains("")) {
								ext_new = new String(".mif");
							}
						}
					}
					if (format.getParameterValueAsString().equals("DGN")) {
						ext_new = new String(".dgn");
					}
					if (format.getParameterValueAsString().equals("BNA")) {
						ext_new = new String(".bna");
					}
					if (format.getParameterValueAsString().equals("CSV")) {
						ext_new = new String(".csv");
					}
					if (format.getParameterValueAsString().equals("GML")) {
						ext_new = new String(".gml");
					}
					if (format.getParameterValueAsString().equals("GPX")) {
						ext_new = new String(".gpx");
					}
					if (format.getParameterValueAsString().equals("KML")) {
						ext_new = new String(".kml");
					}
					if (format.getParameterValueAsString().equals("GeoJSON")) {
						ext_new = new String(".geojson");
					}
					if (format.getParameterValueAsString().equals("GMT")) {
						ext_new = new String(".gmt");
					}
					if (format.getParameterValueAsString().equals("Geoconcept")) {
						ext_new = new String(".gxt");
					}
					if (!filename.toLowerCase().endsWith(ext_new)) {
						layer.setParameterValue(filename + ext_new);
					}
					dsn.setParameterValue(layer.getParameterValueAsString());
					m_Parameters.removeParameter("olayer");
				}
			}
			//v.to.3d: circumvent SEXTANTE parsing problem ("height" must be set)
			if (getName().equals("v.to.3d")) {
				final Parameter column = m_Parameters.getParameter("column");
				if ((column.getParameterValueAsString() != null) && (column.getParameterValueAsString().trim().length() > 0)) {
					m_Parameters.removeParameter("height");
				}
			}
		}
		catch (final Exception e) {
			GrassAlgorithmProvider.addError(Sextante.getText("grass_err_parse_parms")+" '" + this.getName() + 
					"' (" + e.getClass().getName() + ").");
			Sextante.getLogger().addError(e);
		}

		//Drop all parameters that have no value (except flags)
		final ArrayList remove = new ArrayList();
		for (int i = 0; i < m_Parameters.getNumberOfParameters(); i++) {

			final Parameter param = m_Parameters.getParameter(i);

			if (!(param instanceof ParameterBoolean)) {
				try {
					if ((param instanceof ParameterMultipleInput)) {
						final ArrayList sValues = param.getParameterValueAsArrayList();
						if (sValues.size() < 1) {
							remove.add(param.getParameterName());
						}
					}
					else {
						final String value = new String(param.getParameterValueAsString());
						if ((value == null) || (value.trim().length() < 1)) {
							GrassAlgorithmProvider.addWarning(Sextante.getText("grass_remove_empty_param") + 
									" " + param.getParameterName() + ".");							
							remove.add(param.getParameterName());
						}
					}
				}
				catch (final Exception e) {}
			}
		}
		for (int i = 0; i < remove.size(); i++) {
			try {
				m_Parameters.removeParameter((String) remove.get(i));
			}
			catch (final Exception e) {}
		}

		//quote all parameter strings that have whitespace
		for (int i = 0; i < m_Parameters.getNumberOfParameters(); i++) {
			final Parameter param = m_Parameters.getParameter(i);
			try {
				final String value = new String(param.getParameterValueAsString());
				if (value.contains(" ")) {
					param.setParameterValue((String) "\"" + value.trim() + "\"");
				}
			}
			catch (final Exception e) {}
		}

	}


	/*
	 * Further processing of GRASS module options, after data was imported
	 * but before the actual GRASS module is run.
	 */
	private void preprocessBeforeExec() {
		try {
			//i.cluster, i.gensig and i.gensigset
			if ( getName().equals("i.cluster") || getName().equals("i.gensig") || getName().equals("i.gensigset") ) {
				//1. Add "subgroup" option with same value as "group".
				ParameterImageLayer lgroup = (ParameterImageLayer) m_Parameters.getParameter("group");
				final String gname = ((FileOutputChannel) lgroup.getParameterValueAsImageLayer().
						getOutputChannel()).getFilename();
				final ParameterString psubgroup = new ParameterString();
				psubgroup.setParameterName("subgroup");
				psubgroup.setParameterValue((String)getInFile(gname));
				m_Parameters.addParameter((ParameterString)psubgroup);					
				//2. Extract only file name from "sigfile" option, as this will be saved in the subgroup folder
				Parameter sigfile = null;
				if ( getName().equals("i.cluster") ) {
					sigfile = m_Parameters.getParameter("sigfile");
				}
				if ( getName().equals("i.gensig") || getName().equals("i.gensigset") ) {
					sigfile = m_Parameters.getParameter("signaturefile");
				}
				final File f = new File (sigfile.getParameterValueAsString());
				// Store original path so that we can retrieve it later:
				m_sTemp = new String(sigfile.getParameterValueAsString());
				// Now set output file name for GRASS
				sigfile.setParameterValue((String)f.getName());		
				//Many i.* modules require specification of an imagery subgroup
				//in addition to a group. We create a subfolder "subgroup" in the
				//"group" folder created by r.in.gdal/r.in.external and then
				//we create a subgroup in there with the same names and band
				//references as the parent group.
				final File group = new File(GrassUtils.getGrassMapsetFolder() + File.separator + "group" + File.separator
						+ getInFile(gname));
				if ( group.exists() && group.isDirectory() && group.canWrite() ) {
					final File subgroup = new File (group.getAbsolutePath() + File.separator + "subgroup" + File.separator 
						+ getInFile(gname) );
					subgroup.mkdirs();
					if ( subgroup.exists() && subgroup.isDirectory() && subgroup.canWrite() ) {												
						final File ref = new File(group.getAbsolutePath() + File.separator + "REF");
						if ( ref.exists() && ref.canRead() ) {
							try {
								final File dst = new File ( subgroup.getAbsolutePath() + File.separator + "REF"); 
								Files.copy(ref.toPath(), dst.toPath());
							} catch (IOException e) {
								Sextante.addErrorToLog("GRASS interface error: Failed to copy imagery band refs to subgroup.");
								Sextante.getLogger().addError(e);
							}
						} else {
							Sextante.addErrorToLog("GRASS interface error: Failed to create imagery reference file.");
						}
					} else {
						Sextante.addErrorToLog("GRASS interface error: Failed to create imagery subgroup.");
					}
				}
			}
			//i.maxlik and i.smap
			if ( getName().equals("i.maxlik") || getName().equals("i.smap") ) {
				//1. Add "subgroup" option with same value as "group".
				ParameterImageLayer lgroup = (ParameterImageLayer) m_Parameters.getParameter("group");
				final String gname = ((FileOutputChannel) lgroup.getParameterValueAsImageLayer().
						getOutputChannel()).getFilename();
				final ParameterString psubgroup = new ParameterString();
				psubgroup.setParameterName("subgroup");
				psubgroup.setParameterValue((String)getInFile(gname));
				m_Parameters.addParameter((ParameterString)psubgroup);					
				//2. Extract only file name from "sigfile" option, as this will be saved in the subgroup folder
				Parameter sigfile = null;
				if ( getName().equals("i.maxlik") ) {
					sigfile = m_Parameters.getParameter("sigfile");
				}
				if ( getName().equals("i.smap") ) {
					sigfile = m_Parameters.getParameter("signaturefile");
				}
				final File f = new File (sigfile.getParameterValueAsString());
				// Store original path so that we can retrieve it later:
				m_sTemp = new String(sigfile.getParameterValueAsString());
				// Now set input sigfile name for GRASS
				sigfile.setParameterValue((String)f.getName());		
				//Many i.* modules require specification of an imagery subgroup
				//in addition to a group. We create a subfolder "subgroup" in the
				//"group" folder created by r.in.gdal/r.in.external and then
				//we create a subgroup in there with the same names and band
				//references as the parent group.
				final File group = new File(GrassUtils.getGrassMapsetFolder() + File.separator + "group" + File.separator
						+ getInFile(gname));
				if ( group.exists() && group.isDirectory() && group.canWrite() ) {
					final File subgroup = new File (group.getAbsolutePath() + File.separator + "subgroup" + File.separator 
						+ getInFile(gname) );
					subgroup.mkdirs();
					if ( subgroup.exists() && subgroup.isDirectory() && subgroup.canWrite() ) {
						final File ref = new File(group.getAbsolutePath() + File.separator + "REF");
						if ( ref.exists() && ref.canRead() ) {
							try {								
								final File dst = new File ( subgroup.getAbsolutePath() + File.separator + "REF"); 
								Files.copy(ref.toPath(), dst.toPath());
							} catch (IOException e) {
								Sextante.addErrorToLog("GRASS interface error: Failed to copy imagery band refs to subgroup.");
								Sextante.getLogger().addError(e);
							}
						} else {
							Sextante.addErrorToLog("GRASS interface error: Failed to create imagery reference file.");
						}
						//Create output folder for signature file in subgroup folder
						final File sigfolder = new File (group.getAbsolutePath() + File.separator + "subgroup" + File.separator 
								+ getInFile(gname) + File.separator + "sig");
							sigfolder.mkdirs();
							if ( sigfolder.exists() && sigfolder.isDirectory() && sigfolder.canWrite() ) {
								// 3. Copy user-supplied signature file into subgroup folder
								try {								
									final File dst = new File ( sigfolder.getAbsolutePath() + File.separator + f.getName()); 
									Files.copy(f.toPath(), dst.toPath());
								} catch (IOException e) {
									Sextante.addErrorToLog("GRASS interface error: Failed to copy signature file into subgroup.");
									Sextante.getLogger().addError(e);
								}
							} else {
								Sextante.addErrorToLog("GRASS interface error: Failed to create signature folder in subgroup.");
							}
					} else {
						Sextante.addErrorToLog("GRASS interface error: Failed to create imagery subgroup.");
					}
				}				
			}						
			//r.mapcalc: subsitute mapnames in formula
			if (getName().equals("r.mapcalculator")) {
				final Parameter formula = m_Parameters.getParameter("formula");
				if ((formula.getParameterValueAsString() != null) && (formula.getParameterValueAsString().trim().length() > 0)) {
					String expression = formula.getParameterValueAsString().trim();
					//Now we substitute all *map parameters.
					//We'll be tolerant: user can say "amap", but also "A" etc.
					//We don't know which maps have been specified, so we try/catch all of them.
					final String long_names[] = { "amap", "bmap", "cmap", "dmap", "emap", "fmap" };
					final String short_names[] = { "A", "B", "C", "D", "E", "F" };
					for (int i = 0; i < long_names.length; i++) {
						try {
							final Parameter map = m_Parameters.getParameter(long_names[i]);
							final String filename = ((FileOutputChannel) map.getParameterValueAsRasterLayer().getOutputChannel()).getFilename();
							final String subst = getInFile(filename);
							if ((subst != null) && (subst.trim().length() > 0)) {
								expression = expression.replace(short_names[i], subst);
								expression = expression.replace(long_names[i], subst);
							}
						}
						catch (final Exception map_e) {}//These are all harmless
					}
					formula.setParameterValue("\"" + expression + "\"");
				}
			}


		}
		catch (final Exception e) {
			JOptionPane.showMessageDialog(null, Sextante.getText("grass_warning_preprocess"),
					Sextante.getText("grass_warning_title"), JOptionPane.WARNING_MESSAGE);
			Sextante.getLogger().addError(e);
		}
	}


	/*
	 * post-process parameter values to guard against common problems.
	 * The code here fixes any issues that must be taken care of before
	 * data can be exported.
	 */
	private void postProcessBeforeExport() {
		try {
			//i.cluster, i.gensig and i.gensigset: need to export signature file to user-chosen location
			if ( getName().equals("i.cluster") || getName().equals("i.gensig") || getName().equals("i.gensigset") ) {
				ParameterImageLayer lgroup = (ParameterImageLayer) m_Parameters.getParameter("group");
				final String gname = ((FileOutputChannel) lgroup.getParameterValueAsImageLayer().
						getOutputChannel()).getFilename();				
				// Locate output signature file folder
				final File sigfolder = new File(GrassUtils.getGrassMapsetFolder() + File.separator + "group" + File.separator
						+ getInFile(gname) + File.separator + "subgroup" + File.separator + getInFile(gname) +
						File.separator + "sig" );
				boolean ok = false;
				// Locate signature file
				if ( sigfolder.exists() && sigfolder.isDirectory() && sigfolder.canRead() ) {
					Parameter param = null;
					if ( getName().equals("i.cluster") ) {
						param = m_Parameters.getParameter("sigfile");
					}
					if ( getName().equals("i.gensig") || getName().equals("i.gensigset") ) {
						param = m_Parameters.getParameter("signaturefile");
					}
					final File sigfile = new File (sigfolder.getAbsolutePath() + File.separator + 
							param.getParameterValueAsString());
					// Export sigfile to user-specified file path					
					if ( sigfile.exists() && sigfile.isFile() && sigfile.canRead() ) {
						final File dst = new File (m_sTemp);
						Files.copy(sigfile.toPath(), dst.toPath(),StandardCopyOption.REPLACE_EXISTING);
						ok = true;
					}						
				}				
				if ( ok == false ) {
					throw ( new GeoAlgorithmExecutionException("GRASS interface: Unable to export signature file.") );
				}
			}			
			//v.distance: need to export values uploaded into existing attribute table field(s)
			if (getName().equals("v.distance")) {
				final Parameter column = m_Parameters.getParameter("column");
				if ((column.getParameterValueAsString() != null) && (column.getParameterValueAsString().trim().length() > 0)) {
					final Parameter from = m_Parameters.getParameter("from");
					final String mapname = new String(
							getInFile(((FileOutputChannel) from.getParameterValueAsVectorLayer().getOutputChannel()).getFilename()));
					final Parameter upload = m_Parameters.getParameter("upload");
					final Output tmpOut = new OutputVectorLayer();
					//tmpOut.setDescription("v.distance (+" + upload.getParameterValueAsString().trim() + ")");
					tmpOut.setDescription(upload.getParameterValueAsString().trim());
					tmpOut.setName(mapname);
					m_OutputObjects.add(tmpOut);
					tmpOut.setOutputChannel(this.getOutputChannel(tmpOut.getName()));
				}
			}
			//v.edit: directly manipulates input map
			if (getName().equals("v.edit")) {
				final Parameter map = m_Parameters.getParameter("map");
				final String filename = ((FileOutputChannel) map.getParameterValueAsVectorLayer().getOutputChannel()).getFilename();
				final String mapname = new String(getInFile(filename));
				final Output tmpOut = new OutputVectorLayer();
				//tmpOut.setDescription("v.edit (edited)");
				tmpOut.setDescription("edited");
				tmpOut.setName(mapname);
				m_OutputObjects.add(tmpOut);
				tmpOut.setOutputChannel(this.getOutputChannel(tmpOut.getName()));
			}
			//v.what.vect: directly manipulates input map
			if (getName().equals("v.what.vect")) {
				final Parameter vector = m_Parameters.getParameter("vector");
				final String filename = ((FileOutputChannel) vector.getParameterValueAsVectorLayer().getOutputChannel()).getFilename();
				final String mapname = new String(getInFile(filename));
				final Output tmpOut = new OutputVectorLayer();
				//tmpOut.setDescription("v.what.vect (query)");
				tmpOut.setDescription("query");
				tmpOut.setName(mapname);
				m_OutputObjects.add(tmpOut);
				tmpOut.setOutputChannel(this.getOutputChannel(tmpOut.getName()));
			}
			//r.null: directly manipulates input map
			if (getName().equals("r.null")) {
				final Parameter map = m_Parameters.getParameter("map");
				final String filename = ((FileOutputChannel) map.getParameterValueAsRasterLayer().getOutputChannel()).getFilename();
				final String mapname = new String(getInFile(filename));
				final Output tmpOut = new OutputRasterLayer();
				//tmpOut.setDescription("r.null (null)");
				tmpOut.setDescription("null");
				tmpOut.setName(mapname);
				m_OutputObjects.add(tmpOut);
				tmpOut.setOutputChannel(this.getOutputChannel(tmpOut.getName()));
			}
			//r.colors: directly manipulates input map
			if (getName().equals("r.colors")) {
				final Parameter map = m_Parameters.getParameter("map");
				final String filename = ((FileOutputChannel) map.getParameterValueAsRasterLayer().getOutputChannel()).getFilename();
				final String mapname = new String(getInFile(filename));
				final Output tmpOut = new OutputRasterLayer();
				//tmpOut.setDescription("r.colors (colors)");
				tmpOut.setDescription("(colors)");
				tmpOut.setName(mapname);
				m_OutputObjects.add(tmpOut);
				tmpOut.setOutputChannel(this.getOutputChannel(tmpOut.getName()));
			}
			//r.colors.stddev: directly manipulates input map
			if (getName().equals("r.colors.stddev")) {
				final Parameter input = m_Parameters.getParameter("input");
				final String filename = ((FileOutputChannel) input.getParameterValueAsRasterLayer().getOutputChannel()).getFilename();
				final String mapname = new String(getInFile(filename));
				final Output tmpOut = new OutputRasterLayer();
				//tmpOut.setDescription("r.colors.stddev (colors)");
				tmpOut.setDescription("colors");
				tmpOut.setName(mapname);
				m_OutputObjects.add(tmpOut);
				tmpOut.setOutputChannel(this.getOutputChannel(tmpOut.getName()));
			}


		}
		catch (final Exception e) {
			JOptionPane.showMessageDialog(null, Sextante.getText("grass_warning_postprocess"),
					Sextante.getText("grass_warning_title"), JOptionPane.WARNING_MESSAGE);
			Sextante.getLogger().addError(e);
		}

	}


	/*
	 * Normally, GRASS always works on ALL features in a vector map, regardless
	 * of the extent of the computational region. But this is not how SEXTANTE
	 * works by default.
	 * Therefore, we take the extents of the computational region and pass them
	 * as a spatial= option to v.in.ogr.
	 * This filter can be toggled of by the user in the SEXTANTE GRASS settings.
	 */
	private String applyBBoxFilter ( final IVectorLayer layer ) 
	{
		String result = new String ("");

		final boolean noVectBBox = new Boolean(SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_NO_VECT_BBOX)).booleanValue();
		if (noVectBBox) {
			return result;
		}	   

		result += " spatial=";

		result += m_AnalysisExtent.getXMin() +",";
		result += m_AnalysisExtent.getYMin() +",";
		result += m_AnalysisExtent.getXMax() +",";
		result += m_AnalysisExtent.getYMax();

		return result;
	}


	/* Try to duplicate any selection filters.
	 * This will translate the IDs of selected features (if the host GIS supports this)
	 * into ranges of the form "1-3","8-14", etc.
	 * We will then pass these to v.extract to get only the selected features from
	 * the imported map. The string returned by this method contains a number of GRASS
	 * command that must be run AFTER v.in.ogr.
	 */
	private String applySelectionFilter ( final IVectorLayer layer, final String mapname, List<Integer> keys )
	{
		String result = new String ("");
		final List<IVectorLayerFilter> filterList = layer.getFilters(); 
		String line = new String ();
		int numFilters = filterList.size();

		for ( int iFilter = 0; iFilter < numFilters; iFilter ++ ) {
			IVectorLayerFilter Filter = filterList.get(iFilter);
			if ( Filter instanceof SelectionFilter ) {
				List<String> rangesList = new ArrayList<String>();
				int start=-2;
				int end=-2;
				boolean range_open = false;
				for ( int iFeature = 0; iFeature < ((SelectionFilter)Filter).getLast(); iFeature ++ ) {
					boolean selected;
					selected = filterList.get(iFilter).accept(null, iFeature);
					if ( selected ) {
						if ( iFeature > (end+1) ) {
							if ( end != - 2 ) {
								/* create new range */
								if ( start == end ) {
									line = (start+1) + "";
								} else {
									line = (start+1) + "-" + (end+1);
								}
								for ( int i = start+1; i <= end+1; i ++ ) {
									keys.add(i);
								}
								rangesList.add(line);
								range_open = false;
								start = iFeature;
							} else {
								start = iFeature;
							}
							end = iFeature;
							range_open = true;
						} else {
							end = iFeature;
						}
					}
				}
				if ( range_open == true ) {
					/* create new range */
					if ( start == end ) {
						line = (start+1) + "";
					} else {
						line = (start+1) + "-" + (end+1);
					}
					for ( int i = start+1; i <= end+1; i ++ ) {
						keys.add(i);
					}				   
					rangesList.add(line);
				}
				/*
				 * Build the v.extract command line needed to remove all features except
				 * the selected ones
				 */
				if ( rangesList.size() > 0 ) {
					result = "v.extract --quiet --overwrite input=" + mapname + " output=" + mapname + "2" + " list=";
					for ( int iRange = 0; iRange < rangesList.size(); iRange ++ ) {
						if ( iRange < (rangesList.size()-1) ) {
							result += rangesList.get(iRange) + ",";
						}
						else {
							result += rangesList.get(iRange);
						}					   
					}
					result += "\n";				   
				}
			}
		}
		/* if a selection was extracted: swap extraction with original data */
		if ( result.length() > 1 ) {		   
			result += "g.remove vect=" + mapname + " --quiet\n";
			result += "g.rename vect=" + mapname + "2," + mapname +" --quiet\n";
		}

		return result;
	}


	@Override
	public void defineCharacteristics() {}


	/**
	 * PROCESS THE GRASS ALGORITHM 
	 * This method imports geodata from external sources in the GRASS mapset,
	 * runs the GRASS algorithm (module) on it and then exports the results back 
	 * as new SEXTANTE layers.
	 */
	@Override
	public boolean processAlgorithm() throws GeoAlgorithmExecutionException {

		final boolean bIsInPolylines = new Boolean(SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_IN_POLYLINES)).booleanValue();

		StringBuffer sCommand = new StringBuffer();

		boolean gotKilled = false;
		GrassUtils.setInterruptible(true);

		//Preprocess some tricky parameters
		preprocessParams();

		//Send starting message to log viewer
		GrassAlgorithmProvider.addInfo("   ");
		final String modName = new String("| " + Sextante.getText("grass_start") + " " + this.getName() + " |");
		final StringBuffer decoration = new StringBuffer("+");
		for (int i = 0; i < (modName.length() - 2); i++) {
			decoration.append("-");
		}
		decoration.append("+");
		GrassAlgorithmProvider.addNote(decoration.toString(), this);
		GrassAlgorithmProvider.addNote(modName, this);
		GrassAlgorithmProvider.addNote(decoration.toString(), this);
		GrassAlgorithmProvider.addNote("   ", this);

		/* Create a temporary GRASS mapset */
		try {
			GrassUtils.createTempMapset();
		}
		catch (final IOException e) {
			JOptionPane.showMessageDialog(null, Sextante.getText("GRASS_error_create_temp_mapset") + "\n" + e.getMessage(), "",
					JOptionPane.ERROR_MESSAGE);
		}

		//Initialize "files<->GRASS maps" relations arrays
		m_FilesIn = new ArrayList<String>();
		m_MapsIn = new ArrayList<String>();

		//Get GISDBASE, LOCATION and MAPSET from String parameter
		final String mapset = new String(GrassUtils.getGrassMapsetFolder().substring(GrassUtils.getGrassMapsetFolder().lastIndexOf(File.separator) + 1,
				GrassUtils.getGrassMapsetFolder().length()));
		final String path = new String(GrassUtils.getGrassMapsetFolder().substring(0, GrassUtils.getGrassMapsetFolder().lastIndexOf(File.separator)));
		final String location = new String(path.substring(path.lastIndexOf(File.separator) + 1, path.length()));
		final String gisdbase = new String(path.substring(0, path.lastIndexOf(File.separator)));      

		final boolean bCompatMode = new Boolean(SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_COMPATIBILITY_MODE)).booleanValue();

		//Default DB driver is SQLite, but if it is not supported, then DBF will
		//be the fallback choice.
		if ( bCompatMode == false ) {
			sCommand.append("db.connect driver=sqlite database=\""+gisdbase+File.separator+location+File.separator+mapset+File.separator+"sqlite.db\"\n");
		}      
		try {
			GrassUtils.runGRASS(sCommand, "Connecting with SQLite DB:", this);
		}
		catch (final Exception e) {
			//SQlite driver not working? Fall back to DBF.
			GrassAlgorithmProvider.addWarning(Sextante.getText("grass_warn_fallback_sqlite_dbf"));
			sCommand = new StringBuffer();
			sCommand.append("db.connect driver=dbf\n");
			try {
				GrassUtils.runGRASS(sCommand, Sextante.getText("grass_warn_fallback_sqlite_dbf"), this);
			}
			catch (final Exception e2) {
				//That's it: we're dead!
				gotKilled = true;
			}
		}

		sCommand = new StringBuffer();

		//Set GRASS region		
		if (getUserCanDefineAnalysisExtent()) {
			//Take some precautions so we always have a valid region
			boolean warnXY = false;
			boolean warnZ = false;
			final double defaultCellsXY = 100.0;
			final double defaultCellsZ = 10.0;
			double w = m_AnalysisExtent.getXMin();
			double e = m_AnalysisExtent.getXMax();
			//E-W extent and resolution
			if ( m_AnalysisExtent.getXMax() - m_AnalysisExtent.getXMin() <= 0.0 ) {
				w = 0.0;
				e = 1.0;
				warnXY = true;
			}
			double ewres = m_AnalysisExtent.getCellSizeX();
			if ( ewres <= 0.0 ) {
				ewres = (e-w)/defaultCellsXY;
				warnXY = true;
			}
			//N-S extent and resolution
			double s = m_AnalysisExtent.getYMin();
			double n = m_AnalysisExtent.getYMax();
			if ( m_AnalysisExtent.getYMax() - m_AnalysisExtent.getYMin() <= 0.0 ) {
				s = 0.0;
				n = 1.0;
				warnXY = true;
			}
			double nsres = m_AnalysisExtent.getCellSizeY();
			if ( nsres <= 0.0 ) {
				nsres = (n-s)/defaultCellsXY;
				warnXY = true;
			}
			//T-B extent and resolution
			double b = m_AnalysisExtent.getZMin();
			double t = m_AnalysisExtent.getZMax();
			if ( m_AnalysisExtent.getZMax() - m_AnalysisExtent.getZMin() <= 0.0 ) {
				b = 0.0;
				t = 1.0;
				warnZ = true;
			}			
			double tbres = m_AnalysisExtent.getCellSizeZ();
			if ( tbres <= 0.0 ) {
				tbres = (t-b)/defaultCellsZ;
				warnZ = true;
			}
			if ( warnXY ) {
				Sextante.getLogger().addWarning
				("GRASS X/Y region adjusted due to wrong or missing extent and/or resolution values.");
			}
			if ( warnZ ) {
				Sextante.getLogger().addWarning
				("GRASS Z region adjusted due to wrong or missing extent and/or resolution values.");
			}
			//At this point, we should always have a valid region.
			sCommand.append("g.region");
			sCommand.append(" w=" + w);
			sCommand.append(" e=" + e);
			sCommand.append(" s=" + s);
			sCommand.append(" n=" + n);
			sCommand.append(" b=" + b);
			sCommand.append(" t=" + t);							
			sCommand.append(" ewres=" + ewres);
			sCommand.append(" nsres=" + nsres);
			sCommand.append(" tbres=" + tbres + "\n");
		}

		//
		//PREPROCESSING: SET GRASS REGION
		//
		try {
			GrassUtils.runGRASS(sCommand, "Setting GRASS region:", this);
		}
		catch (final Exception e) {
			gotKilled = true;
		}

		sCommand = new StringBuffer();

		//Commands for importing external layers into GRASS mapset
		String sFilename;
		File file;

		//Import raster layers: single layer input options
		ArrayList layers = m_Parameters.getParametersOfType(ParameterRasterLayer.class);
		for (int i = 0; i < layers.size(); i++) {
			final IRasterLayer layer = ((ParameterRasterLayer) layers.get(i)).getParameterValueAsRasterLayer();
			if (layer != null) {
				final IOutputChannel channel = layer.getOutputChannel();
				if (channel instanceof FileOutputChannel) {
					sFilename = ((FileOutputChannel) channel).getFilename();
				}
				else {
					throw new GeoAlgorithmExecutionException(
							Sextante.getText("Input_layers_are_not_compatible_with_GRASS_usage_\nMust_be_file-based_layers"));
				}

				//Create a safe GRASS map name for the imported layer
				final String sGrassName = GrassUtils.getTempMapName(sFilename);

				//Check if we need to forward the SEXTANTE "no data" setting to GRASS
				final boolean bSEXTANTENull = new Boolean(SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_USE_SEXTANTE_NULL)).booleanValue();

				//Import or attach raster layer via GDAL
				if ( bCompatMode == true || bSEXTANTENull == true ) {
					// We have to import using r.in.gdal if:
					// (a) we are running in high compatibility mode
					// or
					// (b) we need to forward the SEXTANTE "no data" setting
					sCommand.append("r.in.gdal");
				} else { 
					sCommand.append("r.external");
				}
				sCommand.append(" input=\"" + sFilename + "\"");
				sCommand.append(" band=1");
				sCommand.append(" output=" + sGrassName);
				sCommand.append(" --overwrite -o\n");

				//Add NULL value mask if SEXTANTE null value is forwarded           
				if (bSEXTANTENull == true ) { 
					sCommand.append("r.null map=" + sGrassName);
					sCommand.append(" setnull=\"" + Double.toString(SextanteGUI.getOutputFactory().getDefaultNoDataValue()) + "\"");
					sCommand.append("\n");
				}

				//Map relation between imported file and new GRASS map.
				//We will need this later to substitute the GRASS input options.
				registerInMapping(sFilename, sGrassName);
			}
		}

		//Import image layers: these are multi-band raster layers that need to
		//be imported as groups/subgroups.
		ArrayList images = m_Parameters.getParametersOfType(ParameterImageLayer.class);
		for (int i = 0; i < images.size(); i++) {
			final IRasterLayer layer = ((ParameterImageLayer) images.get(i)).getParameterValueAsImageLayer();
			if (layer != null) {
				final IOutputChannel channel = layer.getOutputChannel();
				if (channel instanceof FileOutputChannel) {
					sFilename = ((FileOutputChannel) channel).getFilename();
				}
				else {
					throw new GeoAlgorithmExecutionException(
							Sextante.getText("Input_layers_are_not_compatible_with_GRASS_usage_\nMust_be_file-based_layers"));
				}

				//Create a safe GRASS map name for the imported image group
				final String sGrassName = GrassUtils.getTempMapName(sFilename);

				//Check if we need to forward the SEXTANTE "no data" setting to GRASS
				final boolean bSEXTANTENull = 
						new Boolean(SextanteGUI.getSettingParameterValue
								(SextanteGrassSettings.GRASS_USE_SEXTANTE_NULL)).booleanValue();

				//Import or attach raster layer as image group via GDAL.
				//An image group will automatically be created by r.in.gdal/r.external and
				//all referenced bands will be added to it.
				if ( bCompatMode == true || bSEXTANTENull == true ) {
					// We have to import using r.in.gdal if:
					// (a) we are running in high compatibility mode
					// or
					// (b) we need to forward the SEXTANTE "no data" setting
					sCommand.append("r.in.gdal -k"); // -k adds integer count to raster map name (instead of RGB name)
				} else { 
					sCommand.append("r.external"); // Does not require/know -k
				}
				sCommand.append(" input=\"" + sFilename + "\"");
				sCommand.append(" output=" + sGrassName);
				sCommand.append(" --overwrite -o\n");

				//Add NULL value mask if SEXTANTE null value is forwarded
				//TODO: need to do this for all bands!? Should we maybe just ignore this for images?
				//There is text file "REF" in the "group" folder for this image that lists all
				//imported bands.
				/*
				if (bSEXTANTENull == true ) { 
					sCommand.append("r.null map=" + sGrassName);
					sCommand.append(" setnull=\"" + Double.toString(SextanteGUI.getOutputFactory().getDefaultNoDataValue()) + "\"");
					sCommand.append("\n");
				}
				*/

				//Map relation between imported file and new GRASS map.
				//We will need this later to substitute the GRASS input options.
				registerInMapping(sFilename, sGrassName);
				
				//DEBUG
				Sextante.getLogger().addInfo("*** REGISTER IN MAPPING: " + sFilename + ":" + sGrassName);											
			}
		}

		//Import raster layers: multiple layer input options (e.g. r.patch)
		ArrayList multiLayers = m_Parameters.getParametersOfType(ParameterMultipleInput.class);
		for (int i = 0; i < multiLayers.size(); i++) {
			final AdditionalInfo ai = ((ParameterMultipleInput) multiLayers.get(i)).getParameterAdditionalInfo();
			if (((AdditionalInfoMultipleInput) ai).getDataType() == AdditionalInfoMultipleInput.DATA_TYPE_RASTER) {
				final ArrayList layerList = ((ParameterMultipleInput) multiLayers.get(i)).getParameterValueAsArrayList();
				for (int j = 0; j < layerList.size(); j++) {
					//Run the input procedure as many times as we have input maps for this multiple maps option...
					final IRasterLayer layer = ((IRasterLayer) layerList.get(j));
					if (layer != null) {
						final IOutputChannel channel = layer.getOutputChannel();
						if (channel instanceof FileOutputChannel) {
							sFilename = ((FileOutputChannel) channel).getFilename();
						}
						else {
							throw new GeoAlgorithmExecutionException(
									Sextante.getText("Input_layers_are_not_compatible_with_GRASS_usage_\nMust_be_file-based_layers"));
						}

						//Create a safe GRASS map name for the imported layer
						final String sGrassName = GrassUtils.getTempMapName(sFilename);

						//Check if we need to forward the SEXTANTE "no data" setting to GRASS
						final boolean bSEXTANTENull = new Boolean(SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_USE_SEXTANTE_NULL)).booleanValue();

						//Import or attach raster layer via GDAL
						if ( bCompatMode == true || bSEXTANTENull == true ) {
							// We have to import using r.in.gdal if:
							// (a) we are running in high compatibility mode
							// or
							// (b) we need to forward the SEXTANTE "no data" setting
							sCommand.append("r.in.gdal");
						} else { 
							sCommand.append("r.external");
						}
						sCommand.append(" input=\"" + sFilename + "\"");
						sCommand.append(" band=1");
						sCommand.append(" output=" + sGrassName);
						sCommand.append(" --overwrite -o\n");

						//Add NULL value mask if SEXTANTE null value is forwarded                  
						if (bSEXTANTENull == true ) { 
							sCommand.append("r.null map=" + sGrassName);
							sCommand.append(" setnull=\"" + Double.toString(SextanteGUI.getOutputFactory().getDefaultNoDataValue()) + "\"");
							sCommand.append("\n");
						}

						//Map relation between imported file and new GRASS map.
						//We will need this later to substitute the GRASS input options.
						//registerInMapping( file.getName(), sGrassName);
						registerInMapping(sFilename, sGrassName);
					}
				}
			}
		}

		//Attaching vector layers as external OGR datasets is problematic, we will
		//import them as a new GRASS map instead. This consumes extra time and diskspace,
		//but it is much more robust.

		//Import vector layers: single layer input options
		layers = m_Parameters.getParametersOfType(ParameterVectorLayer.class);

		boolean bRunIteratively = false;
		String sIterMap = null;
		int iIteration = -1;
		//"keys" is were we keep the original cat values in strictly increasing order
		List<Integer> keys = new ArrayList<Integer>();

		for (int i = 0; i < layers.size(); i++) {
			IVectorLayer layer = ((ParameterVectorLayer) layers.get(i)).getParameterValueAsVectorLayer();
			if (layer != null) {
				IOutputChannel channel = layer.getOutputChannel();

				sFilename="";

				if (channel instanceof FileOutputChannel) {
					sFilename = ((FileOutputChannel) channel).getFilename();
				}            
				else {
					if (channel == null) {
						//This can happen if we are running in iterative mode.
						if ( layer instanceof SingleFeatureVectorLayer ) {
							iIteration = ((SingleFeatureVectorLayer)layer).getID();
							layer = ((SingleFeatureVectorLayer)layer).getOriginalLayer();
							channel = layer.getOutputChannel();
							sFilename = ((FileOutputChannel) channel).getFilename();
							bRunIteratively = true;
						} else {
							throw new GeoAlgorithmExecutionException(Sextante.getText("output_channel_null"));
						}
					} else {
						//TODO: implement support for PostGIS here!
						throw new GeoAlgorithmExecutionException(
								Sextante.getText("Input_layers_are_not_compatible_with_GRASS_usage_\nMust_be_file-based_layers"));
					}
				}

				file = new File(sFilename);
				final String sName = file.getName().substring(0, file.getName().lastIndexOf('.'));

				//Create a safe GRASS map name for the imported layer
				final String sGrassName = GrassUtils.getTempMapName(sFilename);
				if ( bRunIteratively ) {
					sIterMap = new String (sGrassName);
				}
				//Check for selection filter
				String sSelectionFilter = new String (applySelectionFilter(layer,sGrassName,keys));
				//build v.in.ogr command
				sCommand.append("v.in.ogr");
				String sParent = file.getParent();
				if (sParent.endsWith(File.separator)) {
					sParent = sParent.substring(0, sParent.length() - 1);
				}
				if (bIsInPolylines) {
					sCommand.append(" type=line");
				}
				sCommand.append(" min_area=-1");//Make sure to import every polygon
				sCommand.append(" dsn=\"" + sParent + "\"");
				sCommand.append(" layer=" + sName);            
				sCommand.append(" output=" + sGrassName);
				String sBoxFilter = applyBBoxFilter(layer);
				//Apply a bounding box filter (bbox set to current region),
				//only if we are not running in iterative mode,
				//and the user has not set GRASS to ignore the region for vector input,
				//and there is no active selection filter
				if ( ( sBoxFilter.length() > 1 ) && (bRunIteratively == false ) && ( sSelectionFilter.length() < 1 ) ) {
					sCommand.append ( sBoxFilter );
				}
				sCommand.append(" --overwrite -o");
				final boolean bCleanPolygons = new Boolean(SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_CLEAN_POLYGONS)).booleanValue();
				if (!bCleanPolygons) {
					sCommand.append(" -c");
				}
				final boolean bIs3DVMode = new Boolean(SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_3D_V_MODE)).booleanValue();
				if (bIs3DVMode) {
					sCommand.append(" -z");
				}
				sCommand.append("\n");

				//If there is a selection filter: apply it now!
				if ( sSelectionFilter.length() > 1 ) {
					sCommand.append ( sSelectionFilter );
				}

				//If running in iterative mode, we extract only the one feature needed
				//for the present iteration and work on the resulting 1-feature map.
				if ( bRunIteratively ) {
					sCommand.append("v.extract --quiet --overwrite");
					sCommand.append(" input=" + sIterMap);
					sCommand.append(" output=" + sIterMap + "2");
					if ( sSelectionFilter.length() > 1 ) {
						//If we have a selection, then we need to retrieve the original keys
						sCommand.append(" list=" + keys.get(iIteration-1));
					} else {
						sCommand.append(" list=" + iIteration);
					}
					sCommand.append("\n");
					sCommand.append("g.remove vect=" + sIterMap);
					sCommand.append(" --quiet");
					sCommand.append("\n");
					sCommand.append("g.rename vect=" + sIterMap + "2");
					sCommand.append("," + sIterMap);
					sCommand.append(" --quiet");
					sCommand.append("\n");
				}

				//Map relation between imported file and new GRASS map.
				//We will need this later to substitute the GRASS input options.
				registerInMapping(sFilename, sGrassName);            
			}
		}

		//Import vector layers: multiple layer input options (e.g. v.patch)
		multiLayers = m_Parameters.getParametersOfType(ParameterMultipleInput.class);
		for (int i = 0; i < multiLayers.size(); i++) {
			final AdditionalInfo ai = ((ParameterMultipleInput) multiLayers.get(i)).getParameterAdditionalInfo();
			if (((AdditionalInfoMultipleInput) ai).getDataType() == AdditionalInfoMultipleInput.DATA_TYPE_VECTOR_ANY) {
				final ArrayList layerList = ((ParameterMultipleInput) multiLayers.get(i)).getParameterValueAsArrayList();
				for (int j = 0; j < layerList.size(); j++) {
					//Run the input procedure as many times as we have input maps for this multiple maps option...
					final IVectorLayer layer = ((IVectorLayer) layerList.get(j));
					if (layer != null) {
						final IOutputChannel channel = layer.getOutputChannel();
						if (channel instanceof FileOutputChannel) {
							sFilename = ((FileOutputChannel) channel).getFilename();
						}
						else {
							throw new GeoAlgorithmExecutionException(
									Sextante.getText("Input_layers_are_not_compatible_with_GRASS_usage_\nMust_be_file-based_layers"));
						}
						file = new File(sFilename);
						final String sName = file.getName().substring(0, file.getName().lastIndexOf('.'));

						//Create a safe GRASS map name for the imported layer
						final String sGrassName = GrassUtils.getTempMapName(sFilename);
						//Check for selection filter
						String sSelectionFilter = new String (applySelectionFilter(layer,sGrassName,keys));
						//build v.in.ogr command
						sCommand.append("v.in.ogr");
						String sParent = file.getParent();
						if (sParent.endsWith(File.separator)) {
							sParent = sParent.substring(0, sParent.length() - 1);
						}
						if (bIsInPolylines) {
							sCommand.append(" type=line");
						}
						sCommand.append(" min_area=-1");//Make sure to import every polygon
						sCommand.append(" dsn=\"" + sParent + "\"");
						sCommand.append(" layer=" + sName);
						sCommand.append(" output=" + sGrassName);
						String sBoxFilter = applyBBoxFilter(layer);
						//Apply a bounding box filter (bbox set to current region),
						//only if the user has not set GRASS to ignore the region for vector input,
						//and there is no active selection filter
						if ( ( sBoxFilter.length() > 1 ) && ( sSelectionFilter.length() < 1 ) ) {
							sCommand.append ( sBoxFilter );
						}                  
						sCommand.append(" --overwrite -o");
						final boolean bCleanPolygons = new Boolean(SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_CLEAN_POLYGONS)).booleanValue();
						if (!bCleanPolygons) {
							sCommand.append(" -c");
						}            
						final boolean bIs3DVMode = new Boolean(SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_3D_V_MODE)).booleanValue();
						if (bIs3DVMode) {
							sCommand.append(" -z");
						}
						sCommand.append("\n");                  

						//If there is a selection filter: apply it now!
						if ( sSelectionFilter.length() > 1 ) {
							sCommand.append ( sSelectionFilter );
						}

						//Map relation between imported file and new GRASS map.
						//We will need this later to substitute the GRASS input options.
						registerInMapping(sFilename, sGrassName);
					}
				}
			}
		}


		//
		//PROCESSING STEP ONE: IMPORT DATA INTO GRASS MAPSET
		//
		if (m_MapsIn.size() > 0) {
			GrassAlgorithmProvider.addNote("   ", this);
			GrassAlgorithmProvider.addNote(Sextante.getText("grass_step_import"), this);
			GrassAlgorithmProvider.addNote("   ", this);
			setProgressText(Sextante.getText("grass_progress_importing")+"...");
			if (!gotKilled && !GrassUtils.isProcessCanceled()) {
				try {            	
					GrassUtils.runGRASS(sCommand, Sextante.getText("grass_progress_importing")+":", this);
				}
				catch (final Exception e) {
					gotKilled = true;
				}
			}
		}

		sCommand = new StringBuffer();

		//Check whether import was successful and abort if it was not.
		//This is easy, we just need to check if the new raster map's data exists on disk.
		//In the case of a raster map, a file with the temp name must exist in the "cellhd" directory.
		//In the case of a multi-band raster map (image), a file with the temp name must exist in the "group" directory.
		//In the case of a vector map, it's a directory with the temp name.
		//Step through all entries in the file <-> GRASS map relations array and see if they exists in either form (good enough).
		for (int i = 0; i < m_MapsIn.size(); i++) {
			final File rasterfile = new File(GrassUtils.getGrassMapsetFolder() + File.separator + "cellhd" + File.separator
					+ m_MapsIn.get(i));
			final File imagefile = new File(GrassUtils.getGrassMapsetFolder() + File.separator + "group" + File.separator
					+ m_MapsIn.get(i));
			final File vectordir = new File(GrassUtils.getGrassMapsetFolder() + File.separator + "vector" + File.separator
					+ m_MapsIn.get(i));
			if (!rasterfile.exists() && !imagefile.exists() && !vectordir.exists()) {
				JOptionPane.showMessageDialog(null, Sextante.getText("grass_error_import_failed"),
						Sextante.getText("grass_error_title"), JOptionPane.ERROR_MESSAGE);
				throw new GrassExecutionException();
			}
		}

		//resolve temporary output files
		for (int i = 0; i < m_OutputObjects.getOutputDataObjectsCount(); i++) {
			final Output out = m_OutputObjects.getOutput(i);
			out.setOutputChannel(this.getOutputChannel(out.getName()));
		}

		//Process some more parameters: after data was imported, but before it gets
		//processed.
		preprocessBeforeExec();


		//Command for executing the algorithm
		sCommand.append(this.getName());
		for (int i = 0; i < m_Parameters.getNumberOfParameters(); i++) {
			final Parameter param = m_Parameters.getParameter(i);

			if (param.getParameterName().equals(PARAMETER_RESTRICT_VECTOR_OUTPUT_TYPE)) {
				continue;
			}
			if (param instanceof ParameterBoolean) {
				//GRASS processing flags
				if (param.getParameterValueAsBoolean()) {
					sCommand.append(" " + param.getParameterName());//flags already contain the "-" as part of their names!
				}
			}
			else {
				if (param instanceof ParameterDataObject) {
					//GRASS input map(s)
					final IDataObject dataObject = (IDataObject) ((ParameterDataObject) param).getParameterValueAsObject();
					if (dataObject != null) {
						//Substitute option value with the name of the GRASS map that relates
						//to this input file.
						sCommand.append(" ");
						sCommand.append(param.getParameterName());
						sCommand.append("=");
						if ( bRunIteratively ) {
							file = new File (sIterMap);
							sCommand.append(sIterMap);
						} else {
							file = new File(((FileOutputChannel) dataObject.getOutputChannel()).getFilename());
							sCommand.append(getInFile(((FileOutputChannel) dataObject.getOutputChannel()).getFilename()));
						}
					}
				}
				else {
					if (param instanceof ParameterMultipleInput) {
						final ArrayList sValues = param.getParameterValueAsArrayList();
						sCommand.append(" ");
						sCommand.append(param.getParameterName());
						sCommand.append("=");
						//Check if it's a list of input maps or just strings
						final AdditionalInfo ai = ((ParameterMultipleInput) param).getParameterAdditionalInfo();
						if ((((AdditionalInfoMultipleInput) ai).getDataType() == AdditionalInfoMultipleInput.DATA_TYPE_RASTER)
								|| (((AdditionalInfoMultipleInput) ai).getDataType() == AdditionalInfoMultipleInput.DATA_TYPE_VECTOR_ANY)) {
							//Got input Maps: Substitute file names for GRASS temp map names
							for (int j = 0; j < sValues.size(); j++) {
								if (((AdditionalInfoMultipleInput) ai).getDataType() == AdditionalInfoMultipleInput.DATA_TYPE_RASTER) {
									final IRasterLayer layer = ((IRasterLayer) sValues.get(j));
									sFilename = new String(((FileOutputChannel) layer.getOutputChannel()).getFilename());
								}
								else {
									final IVectorLayer layer = ((IVectorLayer) sValues.get(j));
									sFilename = new String(((FileOutputChannel) layer.getOutputChannel()).getFilename());
								}
								sCommand.append(getInFile(sFilename));
								if (j < (sValues.size() - 1)) {
									sCommand.append(",");
								}
							}
						}
						else {
							//Got something else: Just append option values
							for (int k = 0; k < sValues.size(); k++) {
								sCommand.append(sValues.get(k));
								if (k < (sValues.size() - 1)) {
									sCommand.append(",");
								}
							}
						}
					}
					/*
               else { 
               if (param instanceof ParameterNumericalValue) {        	 
                   //Numerical values can be passed as strings
                   final String sValue = param.getParameterValueAsString();
                   if ((sValue != null) && !sValue.trim().equals("")) {
                      sCommand.append(" ");
                      sCommand.append(param.getParameterName());
                      sCommand.append("=");
                      sCommand.append(sValue);
                   }
               } 
					 */        

					else {
						//Any other GRASS option: should be just a simple string
						try {
							final String sValue = param.getParameterValueAsString();
							if ((sValue != null) && !sValue.trim().equals("")) {
								sCommand.append(" ");
								sCommand.append(param.getParameterName());
								sCommand.append("=");
								sCommand.append(sValue);
							}
						} catch ( Exception e ) {
							/* Don't have a parameter value? That's a big problem: abort! */
							Sextante.addErrorToLog(e);
							final String msg = new String 
									( 	"\n" + Sextante.getText("Warn_grass_param_mismatch") + "\n" +
											Sextante.getText("Warn_grass_param_mismatch_param_name") +
											" \"" + param.getParameterName() + "\".\n");
							throw new GeoAlgorithmExecutionException(msg);
						}
					}
				}
			}         
		}

		for (int i = 0; i < m_OutputObjects.getOutputDataObjectsCount(); i++) {
			//All GRASS options that create output
			final Output out = m_OutputObjects.getOutput(i);
			final IOutputChannel channel = out.getOutputChannel();
			if (!(channel instanceof NullOutputChannel)) {
				sCommand.append(" ");
				sCommand.append(out.getName() + "=" + out.getName());
			}
			//Modify name of output option (drop option name) for further processing
			//final String tmpString = out.getName();
			//out.setName(tmpString.substring(tmpString.indexOf('=') + 1));
		}

		sCommand.append(" --overwrite\n");


		//
		//PROCESSING STEP TWO: Run GRASS command
		//
		GrassAlgorithmProvider.addNote("   ", this);
		GrassAlgorithmProvider.addNote(Sextante.getText("grass_step_execute"), this);
		GrassAlgorithmProvider.addNote("   ", this);
		String cmd = sCommand.toString();
		for (int i = 0; i < m_MapsIn.size(); i++) {
			cmd = cmd.replace(m_MapsIn.get(i), "<" + m_FilesIn.get(i) + ">");
		}
		//Use a regular expression for those internal map names that cannot be fully resolved
		cmd = cmd.replaceAll("([_][a-z0-9]{8}[_][a-z0-9]{4}[_][a-z0-9]{4}[_][a-z0-9]{4}[_][a-z0-9]{12})", "[TMP]");
		cmd = cmd.substring(0, cmd.length() - 1);
		GrassAlgorithmProvider.addInfo(cmd, this);
		GrassAlgorithmProvider.addInfo("   ", this);
		setProgress(0, 100);
		setProgressText(Sextante.getText("grass_progress_running")+"...");
		if (!gotKilled && !GrassUtils.isProcessCanceled()) {
			try {
				GrassUtils.runGRASS(sCommand, Sextante.getText("grass_progress_running")+":", this);

			}
			catch (final Exception e) {
				gotKilled = true;
			}
		}

		sCommand = new StringBuffer();

		//Run any post-processing code that may be necessary,
		//after running the GRASS command but before exporting the data
		if (!gotKilled && !GrassUtils.isProcessCanceled()) {
			postProcessBeforeExport();
		}

		//Commands for exporting results to non-grass format (shapefile)
		//Alas: ESRI shapefiles only support one geometry type per file.
		//So: split all GRASS multi-geom maps into single-geom maps.
		//Create new output objects for the new maps.
		//Delete old multi-geom map.
		if (!gotKilled && !GrassUtils.isProcessCanceled()) {
			final int numRuns = m_OutputObjects.getOutputDataObjectsCount();
			for (int i = 0; i < numRuns; i++) {
				final Output out = m_OutputObjects.getOutput(i);
				final IOutputChannel channel = out.getOutputChannel();
				if (channel instanceof NullOutputChannel) {
					continue;
				}
				out.setDescription(out.getName());
				if (out instanceof OutputVectorLayer) {
					/* it's a vector layer: check geometry types */
					if (GrassUtils.isMultiGeom(out.getName()) == true) {
						//We have a multiple geometry map: split it up!
						final StringBuffer sSplitCommand = new StringBuffer();
						/* separate out points and kernels */
						if (GrassVInfoUtils.getNumPolygons(out.getName()) > 0) {
							final String sTempMap = new String(GrassUtils.getTempMapName(out.getName()));
							sSplitCommand.append("v.extract type=area input=");
							sSplitCommand.append(out.getName());
							sSplitCommand.append(" output=");
							sSplitCommand.append(sTempMap);
							sSplitCommand.append("\n");
							//Create new output object
							final Output tmpOut = new OutputVectorLayer();
							tmpOut.setDescription(out.getDescription() + " (polygons)");
							tmpOut.setName(sTempMap);
							m_OutputObjects.add(tmpOut);
							tmpOut.setOutputChannel(this.getOutputChannel(tmpOut.getName()));
						}
						if (GrassVInfoUtils.getNumFaces(out.getName()) > 0) {
							final String sTempMap = new String(GrassUtils.getTempMapName(out.getName()));
							sSplitCommand.append("v.extract type=face input=");
							sSplitCommand.append(out.getName());
							sSplitCommand.append(" output=");
							sSplitCommand.append(sTempMap);
							sSplitCommand.append("\n");
							//Create new output object
							final Output tmpOut = new OutputVectorLayer();
							tmpOut.setDescription(out.getDescription() + " (faces)");
							tmpOut.setName(sTempMap);
							m_OutputObjects.add(tmpOut);
							tmpOut.setOutputChannel(this.getOutputChannel(tmpOut.getName()));
						}
						if (GrassVInfoUtils.getNumLines(out.getName()) > 0) {
							final String sTempMap = new String(GrassUtils.getTempMapName(out.getName()));
							sSplitCommand.append("v.extract type=line input=");
							sSplitCommand.append(out.getName());
							sSplitCommand.append(" output=");
							sSplitCommand.append(sTempMap);
							sSplitCommand.append("\n");
							//Create new output object
							final Output tmpOut = new OutputVectorLayer();
							tmpOut.setDescription(out.getDescription() + " (lines)");
							tmpOut.setName(sTempMap);
							m_OutputObjects.add(tmpOut);
							tmpOut.setOutputChannel(this.getOutputChannel(tmpOut.getName()));
						}
						if (GrassVInfoUtils.getNumPoints(out.getName()) > 0) {
							final String sTempMap = new String(GrassUtils.getTempMapName(out.getName()));
							sSplitCommand.append("v.extract type=point input=");
							sSplitCommand.append(out.getName());
							sSplitCommand.append(" output=");
							sSplitCommand.append(sTempMap);
							sSplitCommand.append("\n");
							//Create new output object
							final Output tmpOut = new OutputVectorLayer();
							tmpOut.setDescription(out.getDescription() + " (points)");
							tmpOut.setName(sTempMap);
							m_OutputObjects.add(tmpOut);
							tmpOut.setOutputChannel(this.getOutputChannel(tmpOut.getName()));
						}
						if (GrassVInfoUtils.getNumKernels(out.getName()) > 0) {
							//TODO: kernel support is not implemented in GRASS 6.4's v.extract...
							//TODO: ...for now, we will just convert all kernels to points and...
							//TODO: ...output them to a separate map.
							final String sTempMap = new String(GrassUtils.getTempMapName(out.getName()));
							sSplitCommand.append("v.type type=kernel,point input=");
							sSplitCommand.append(out.getName());
							sSplitCommand.append(" output=");
							sSplitCommand.append(sTempMap);
							sSplitCommand.append("\n");
							//Create new output object
							final Output tmpOut = new OutputVectorLayer();
							tmpOut.setDescription(out.getDescription() + " (kernels)");
							tmpOut.setName(sTempMap);
							m_OutputObjects.add(tmpOut);
							tmpOut.setOutputChannel(this.getOutputChannel(tmpOut.getName()));
						}

						//We can now delete the original Output object
						m_OutputObjects.remove(out);

						//
						//INTERMEDIATE STEP: Split GRASS map into single geom type maps
						//
						if (!gotKilled && !GrassUtils.isProcessCanceled()) {
							try {
								GrassUtils.runGRASS(sSplitCommand, Sextante.getText("grass_output_split_geoms"), this);
							}
							catch (final Exception e) {
								gotKilled = true;
							}
						}
					}
				}
			}

			//END ( if !gotKilled && Canceled )
		}

		if (!gotKilled && !GrassUtils.isProcessCanceled()) {
			for (int i = 0; i < m_OutputObjects.getOutputDataObjectsCount(); i++) {
				final Output out = m_OutputObjects.getOutput(i);
				final IOutputChannel channel = out.getOutputChannel();
				if (!(channel instanceof FileOutputChannel)) {
					continue;
				}
				final FileOutputChannel foc = (FileOutputChannel) channel;
				if (out instanceof OutputVectorLayer) {
					//Now we dump all output vector layers into shapefiles.
					//The type= option of v.out.ogr determines which geometry types will be written
					//into the output shapefile. First, we call v.info -t <GRASS map> to find out which
					//geometries the GRASS map contains. Then we need to call v.out.ogr as often as it
					//takes to write out all the geometry types.

					//Check if the map geometries have 3D coordinates
					final boolean is_3d = GrassVInfoUtils.isMap3D(out.getName());

					//These will have to be set depending on the data in the GRASS map (see below)
					String optType = null;
					String optLCO = null;

					int num_points = 0;
					int num_kernels = 0;
					int num_lines = 0;
					int num_polygons = 0;
					int num_faces = 0;
					//Query all output vector maps again. At this point, we can
					//assume that they are all single geom type.
					if (m_bIsExecutedFromModeller) {
						final int iType = m_Parameters.getParameter(PARAMETER_RESTRICT_VECTOR_OUTPUT_TYPE).getParameterValueAsInt();
						if (iType == OutputVectorLayer.SHAPE_TYPE_POINT) {
							num_points = 1;
						}
						else if (iType == OutputVectorLayer.SHAPE_TYPE_LINE) {
							num_lines = 1;
						}
						else if (iType == OutputVectorLayer.SHAPE_TYPE_POLYGON) {
							num_polygons = 1;
						}
					}
					else {
						num_points = GrassVInfoUtils.getNumPoints(out.getName());
						num_kernels = GrassVInfoUtils.getNumKernels(out.getName());
						num_lines = GrassVInfoUtils.getNumLines(out.getName());
						num_polygons = GrassVInfoUtils.getNumPolygons(out.getName());
						num_faces = GrassVInfoUtils.getNumFaces(out.getName());
					}

					if (num_points > 0) {
						optType = new String("point");
						if (is_3d) {
							optLCO = new String("\"SHPT=POINTZ\"");
						}
						else {
							optLCO = new String("\"SHPT=POINT\"");
						}
					}
					if (num_kernels > 0) {
						optType = new String("kernel");
						optLCO = new String("\"SHPT=POINTZ\"");
					}
					if (num_lines > 0) {
						optType = new String("line");
						if (is_3d) {
							optLCO = new String("\"SHPT=ARCZ\"");
						}
						else {
							optLCO = new String("\"SHPT=ARC\"");
						}
					}
					if (num_polygons > 0) {
						optType = new String("area");
						if (is_3d) {
							optLCO = new String("\"SHPT=POLYGONZ\"");
						}
						else {
							optLCO = new String("\"SHPT=POLYGON\"");
						}
					}
					if (num_faces > 0) {
						optType = new String("face");
						optLCO = new String("\"SHPT=POLYGONZ\"");
					}

					//GRASS 6.4.3 has support for "-s" to skip "cat" field export!

					if ( bCompatMode == false ) {
						sCommand.append("v.out.ogr --o -s -c input=");
					} else {
						sCommand.append("v.out.ogr --o -c input=");
					}
					sCommand.append(out.getName());
					file = new File(foc.getFilename());
					sCommand.append(" dsn=\"" + file.getParent() + "\"");
					final String sName = file.getName().substring(0, file.getName().lastIndexOf('.'));
					sCommand.append(" format=ESRI_Shapefile");
					sCommand.append(" olayer=" + sName);
					if (optType != null) {
						sCommand.append(" type=" + optType);
					}
					if (optLCO != null) {
						sCommand.append(" lco=" + optLCO);
					}
					sCommand.append("\n");
				}
				else if (out instanceof OutputRasterLayer) {
					//Raster layer output: adjust region to layer before exporting
					final boolean bSEXTANTENull = new Boolean(SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_USE_SEXTANTE_NULL)).booleanValue();
					sCommand.append("g.region rast=" + out.getName() + "\n");
					sCommand.append("r.out.gdal --o -f -c");
					if ( bSEXTANTENull == true ) {
						sCommand.append(" nodata=\"" + Double.toString(SextanteGUI.getOutputFactory().getDefaultNoDataValue()) + "\"");
					}
					sCommand.append(" createopt=\"TFW=YES,COMPRESS=LZW\"");
					sCommand.append(" input=");
					sCommand.append(out.getName());
					sCommand.append(" output=\"" + foc.getFilename() + "\"");
					sCommand.append("\n");
					/* Write GRASS color table to a temporary, very simple ASCII file.
					 * The receiving GIS can later pick it up from there and make use of it,
					 * if it wants to.
					 */
					GrassUtils.writeColorTable(out);
				}
			}

			//END ( if !gotKilled && Canceled )
		}


		//
		//PROCESSING STEP THREE: Export data from GRASS mapset
		//
		if (m_OutputObjects.getOutputDataObjectsCount() > 0) {
			GrassAlgorithmProvider.addNote("   ", this);
			GrassAlgorithmProvider.addNote(Sextante.getText("grass_step_export"), this);
			GrassAlgorithmProvider.addNote("   ", this);
			setProgressText(Sextante.getText("grass_progress_exporting")+"...");
			if (!gotKilled && !GrassUtils.isProcessCanceled()) {
				try {
					GrassUtils.runGRASS(sCommand, Sextante.getText("grass_progress_exporting")+":", this);
				}
				catch (final Exception e) {
					gotKilled = true;
				}
			}
		}


		/* Delete temporary mapset */
		GrassUtils.deleteTempMapset();

		setProgressText(Sextante.getText("grass_progress_done"));

		GrassAlgorithmProvider.publishMessage(Sextante.getText("grass_message_log"));

		return !m_Task.isCanceled();

	}


	@Override
	public GeoAlgorithm getNewInstance() throws InstantiationException, IllegalAccessException {	   

		final GrassAlgorithm alg = this.getClass().newInstance();
		alg.setOutputObjects(m_OutputObjects.getNewInstance());
		alg.setName(this.getName());
		alg.setParameters(m_Parameters.getNewInstance());      
		alg.setIsDeterminatedProcess(true);
		alg.setUserCanDefineAnalysisExtent(getUserCanDefineAnalysisExtent());
		alg.setDescriptionFile(m_sDescriptionFile);
		alg.m_bIsExecutedFromModeller = false;

		return alg;

	}


	/**
	 * Sets the xml file with the description of the algorithm
	 * 
	 * @param descriptionFile
	 *                the xml file with the description of the algorithm
	 */
	private void setDescriptionFile(final String descriptionFile) {

		m_sDescriptionFile = descriptionFile;

	}


	/**
	 * Returns a list of strings with internally used GRASS map names.
	 * 
	 * @return List of strings with internally used GRASS map names.
	 */
	public ArrayList<String> getMapNames() {
		return m_MapsIn;
	}


	/**
	 * Returns a list of strings with file names corresponding to internally used GRASS map names.
	 * 
	 * @return List of strings with file names corresponding to internally used GRASS map names.
	 */
	public ArrayList<String> getFileNames() {
		return m_FilesIn;
	}


	@Override
	public String getCommandLineName() {

		return "grass:" + this.getName();

	}


	@Override
	public boolean isSuitableForModelling() {

		return !GrassModelerBlackList.isInBlackList(this);


	}


	@Override
	public boolean preprocessForModeller(final Object obj) {

		m_bIsExecutedFromModeller = true;

		return true;

	}

}
