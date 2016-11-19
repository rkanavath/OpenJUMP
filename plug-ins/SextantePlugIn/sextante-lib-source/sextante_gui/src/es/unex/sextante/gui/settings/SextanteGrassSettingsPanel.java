package es.unex.sextante.gui.settings;

import info.clearthought.layout.TableLayout;
import info.clearthought.layout.TableLayoutConstants;

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.HashMap;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;

import es.unex.sextante.core.GeoAlgorithm;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.algorithm.FileSelectionPanel;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.exceptions.WrongGrassFolderException;
import es.unex.sextante.gui.exceptions.WrongGrassMapsetFolderException;
import es.unex.sextante.gui.exceptions.WrongGrassWinShellException;
import es.unex.sextante.gui.grass.GrassAlgorithmProvider;

public class SextanteGrassSettingsPanel extends SettingPanel {

	// Officially supported GRASS version range
	public static final int MAJOR_MIN = 6;
	public static final int MAJOR_MAX = 6;
	public static final int MINOR_MIN = 4;
	public static final int MINOR_MAX = 4;

	private JCheckBox jActivateCheckBox;
	private JCheckBox jPortableCheckBox;
	private JLabel jLabelGrassFolder;
	private FileSelectionPanel jGrassFolder;
	private JLabel jLabelWinShell;
	private FileSelectionPanel jWinShellFile;
	private JLabel jLabelUpdate;
	private JButton jButtonUpdate;
	private JCheckBox jCheckBoxLatLon;
	private JCheckBox jCheckBox3DV;
	private JCheckBox jCheckBoxNoVectBBox;
	private JCheckBox jCheckBoxInPolylines;
	private JCheckBox jCheckBoxCleanPolygons;
	private JCheckBox jCheckBoxSextanteNull;
	private JCheckBox jCheckBoxCompatibility;

	@Override
	protected void initGUI() {

		final TableLayout thisLayout = new TableLayout(new double[][] {
				{ SextanteConfigurationDialog.SPACER_SMALL,
						TableLayoutConstants.MINIMUM, TableLayoutConstants.FILL,
						SextanteConfigurationDialog.SPACER_SMALL },
				{
						SextanteConfigurationDialog.SPACER_SMALL,
						TableLayoutConstants.MINIMUM, // row 1
						TableLayoutConstants.MINIMUM, // row 2
						TableLayoutConstants.MINIMUM, // row 3
						TableLayoutConstants.MINIMUM, // row 4
						TableLayoutConstants.MINIMUM, // row 5
						TableLayoutConstants.MINIMUM, // row 6
						TableLayoutConstants.MINIMUM, // row 7
						TableLayoutConstants.MINIMUM, // row 8
						TableLayoutConstants.MINIMUM, // row 9
						TableLayoutConstants.MINIMUM, // row 10
						TableLayoutConstants.MINIMUM, // row 11
						TableLayoutConstants.MINIMUM, // row 12
						TableLayoutConstants.MINIMUM, // row 13
						TableLayoutConstants.MINIMUM, // row 14
						TableLayoutConstants.MINIMUM, // row 15
						TableLayoutConstants.FILL,
						TableLayoutConstants.MINIMUM, // row 17
						SextanteConfigurationDialog.SPACER_SMALL } });
		thisLayout.setHGap(5);
		thisLayout.setVGap(5);
		this.setLayout(thisLayout);
		{
			jActivateCheckBox = new JCheckBox(Sextante.getText("ActivateProvider") + " GRASS GIS");
			final String sActivate = SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_ACTIVATE);
			final boolean bActivate = Boolean.parseBoolean(sActivate);
			jActivateCheckBox.setSelected(bActivate);
			this.add(jActivateCheckBox, "1, 1");

			this.add(new JSeparator(SwingConstants.HORIZONTAL), "1, 2, 2, 2");
			/* ----------------------------------------------------------- */

			jPortableCheckBox = new JCheckBox(Sextante.getText("Portable"));
			final String sActivatePortable = SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_PORTABLE);
			final boolean bActivatePortable = Boolean.parseBoolean(sActivatePortable);
			jPortableCheckBox.setEnabled(bActivate);
			jPortableCheckBox.setSelected(bActivatePortable);
			this.add(jPortableCheckBox, "1, 3");

			jLabelGrassFolder = new JLabel();
			jLabelGrassFolder.setEnabled(bActivate);
			if (bActivatePortable == true) {
				jLabelGrassFolder.setEnabled(false);
			}
			this.add(jLabelGrassFolder, "1, 4");
			jLabelGrassFolder.setText(Sextante.getText("GRASS_folder"));
			jGrassFolder = new FileSelectionPanel(true, true, (String[]) null, Sextante.getText("selector_choose_folder"));
			jGrassFolder.getTextField().setEnabled(bActivate);
			jGrassFolder.getButton().setEnabled(bActivate);
			if (bActivatePortable == true) {
				jGrassFolder.getTextField().setEnabled(false);
				jGrassFolder.getButton().setEnabled(false);
			}
			final String sFolder = SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_FOLDER);
			jGrassFolder.setFilepath(sFolder);
			this.add(jGrassFolder, "2, 4");
			
			/* the file selector for sh.exe will only be active if we are running on Windows */
			jLabelWinShell = new JLabel();
			jLabelWinShell.setEnabled(bActivate && Sextante.isWindows());
			if (bActivatePortable == true) {
				jLabelWinShell.setEnabled(false);
			}
			this.add(jLabelWinShell, "1, 5");
			jLabelWinShell.setText("Windows " + Sextante.getText("grass_windows_shell"));
			jWinShellFile = new FileSelectionPanel(true, true, new String[] {"sh.exe", null}, Sextante.getText("selector_choose_file"));
			jWinShellFile.getTextField().setEnabled(bActivate && Sextante.isWindows());
			jWinShellFile.getButton().setEnabled(bActivate && Sextante.isWindows());
			if (bActivatePortable == true) {
				jWinShellFile.getTextField().setEnabled(false);
				jWinShellFile.getButton().setEnabled(false);
			}
			final String sScriptsFolder = SextanteGUI.getSettingParameterValue(SextanteGrassSettings.GRASS_WIN_SHELL);
			jWinShellFile.setFilepath(sScriptsFolder);
			this.add(jWinShellFile, "2, 5");

			this.add(new JSeparator(SwingConstants.HORIZONTAL), "1, 6, 2, 6");
			/* ----------------------------------------------------------- */
			
			jLabelUpdate = new JLabel();
			jLabelUpdate.setText( Sextante.getText("update_library") );
			jLabelUpdate.setEnabled(bActivate);
			this.add(jLabelUpdate, "1, 7");      
			jButtonUpdate = new JButton(Sextante.getText("load_GRASS_modules"));
			jButtonUpdate.setEnabled(bActivate);
			this.add(jButtonUpdate, "2, 7");			

			this.add(new JSeparator(SwingConstants.HORIZONTAL), "1, 8, 2, 8");
			/* ----------------------------------------------------------- */
			
			jCheckBoxLatLon = new JCheckBox(Sextante.getText ("GRASS_lat_lon_mode"));
			jCheckBoxLatLon.setSelected(new Boolean(SextanteGUI.getSettingParameterValue
					(SextanteGrassSettings.GRASS_LAT_LON_MODE)).booleanValue());
			this.add(jCheckBoxLatLon, "1, 9, 2, 9");

			jCheckBox3DV = new JCheckBox();
			jCheckBox3DV.setText(Sextante.getText("grass_input_3d"));
			jCheckBox3DV.setSelected(new Boolean(SextanteGUI.getSettingParameterValue
					(SextanteGrassSettings.GRASS_3D_V_MODE)).booleanValue());
			this.add(jCheckBox3DV, "1, 10, 2, 10");			
			
			jCheckBoxNoVectBBox = new JCheckBox(Sextante.getText("grass_no_vect_bbox"));
			jCheckBoxNoVectBBox.setSelected(new Boolean(SextanteGUI.getSettingParameterValue
					(SextanteGrassSettings.GRASS_NO_VECT_BBOX)).booleanValue());
			this.add(jCheckBoxNoVectBBox, "1, 11, 2, 11");

			jCheckBoxInPolylines = new JCheckBox();
			jCheckBoxInPolylines.setText(Sextante.getText("grass_import_polylines"));
			jCheckBoxInPolylines.setSelected(new Boolean(SextanteGUI.getSettingParameterValue
					(SextanteGrassSettings.GRASS_IN_POLYLINES)).booleanValue());
			this.add(jCheckBoxInPolylines, "1, 12, 2, 12");
			
			jCheckBoxCleanPolygons = new JCheckBox();
			jCheckBoxCleanPolygons.setText(Sextante.getText("grass_clean_polygons"));
			jCheckBoxCleanPolygons.setSelected(new Boolean(SextanteGUI.getSettingParameterValue
					(SextanteGrassSettings.GRASS_CLEAN_POLYGONS)).booleanValue());
			this.add(jCheckBoxCleanPolygons, "1, 13, 2, 13");
			
			jCheckBoxSextanteNull = new JCheckBox();
			jCheckBoxSextanteNull.setText(Sextante.getText("grass_use_sextante_null"));
			jCheckBoxSextanteNull.setSelected(new Boolean(SextanteGUI.getSettingParameterValue
					(SextanteGrassSettings.GRASS_USE_SEXTANTE_NULL)).booleanValue());
			this.add(jCheckBoxSextanteNull, "1, 14, 2, 14");			
			
			jCheckBoxCompatibility = new JCheckBox();
			jCheckBoxCompatibility.setText(Sextante.getText("grass_compatibility_mode"));
			jCheckBoxCompatibility.setSelected(new Boolean(SextanteGUI.getSettingParameterValue
					(SextanteGrassSettings.GRASS_COMPATIBILITY_MODE)).booleanValue());
			this.add(jCheckBoxCompatibility, "1, 15, 2, 15");			

		}

		/* add provider logo and URL */
		final URL res = getClass().getClassLoader().getResource("images/grass.smlogo.gif");
		if (res != null) {
			final ImageIcon logo = new ImageIcon(res);
			JLabel logoLabel = new JLabel(logo);
			logoLabel.setIconTextGap(4);
			logoLabel.setVerticalTextPosition(SwingConstants.BOTTOM);
			logoLabel.setText("<html><i><a href=http://grass.osgeo.org/>http://grass.osgeo.org/</a></i></html>");
			this.add(logoLabel,"1, 17, 2, 17");
		}
		
		/**********************************/
		/** Action listeners for widgets **/
		/**********************************/
		{
			jActivateCheckBox.addActionListener(new ActionListener() {
				public void actionPerformed(final ActionEvent arg0) {
					setCursor(new Cursor(Cursor.WAIT_CURSOR));
					SextanteGUI.setSettingParameterValue(
							SextanteGrassSettings.GRASS_ACTIVATE, new Boolean(
									jActivateCheckBox.isSelected()).toString());
					SextanteGUI.updateAlgorithmProvider(GrassAlgorithmProvider.class);
					/* toggle remaining widgets on or off */
					boolean active = jActivateCheckBox.isSelected();
					jPortableCheckBox.setEnabled(active);
					jLabelGrassFolder.setEnabled(active);
					jGrassFolder.getTextField().setEnabled(active);
					jGrassFolder.getButton().setEnabled(active);					
					if ( Sextante.isWindows()) {
						jLabelWinShell.setEnabled(active);
						jWinShellFile.getTextField().setEnabled(active);
						jWinShellFile.getButton().setEnabled(active);
					}
					jLabelUpdate.setEnabled(active);
					jButtonUpdate.setEnabled(active);
					jCheckBoxLatLon.setEnabled(active);
					jCheckBox3DV.setEnabled(active);
					jCheckBoxNoVectBBox.setEnabled(active);
					jCheckBoxInPolylines.setEnabled(active);
					jCheckBoxCleanPolygons.setEnabled(active);
					jCheckBoxSextanteNull.setEnabled(active);
					jCheckBoxCompatibility.setEnabled(active);
					jActivateCheckBox.getParent().repaint();
					active = jPortableCheckBox.isSelected();
					if ( active == true ) {
						jLabelGrassFolder.setEnabled(false);
						jGrassFolder.getTextField().setEnabled(false);
						jGrassFolder.getButton().setEnabled(false);
						if ( Sextante.isWindows()) {
							jLabelWinShell.setEnabled(false);
							jWinShellFile.getTextField().setEnabled(false);
							jWinShellFile.getButton().setEnabled(false);
						}
					}					
					setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				}
			});
		}

	      jPortableCheckBox.addActionListener(new ActionListener() {
	          public void actionPerformed(final ActionEvent arg0) {        	 
	             setCursor(new Cursor(Cursor.WAIT_CURSOR));
	             SextanteGUI.setSettingParameterValue(SextanteGrassSettings.GRASS_PORTABLE,
	                      new Boolean(jPortableCheckBox.isSelected()).toString());             
	             //Set portable GRASS bin dir
	             SextanteGUI.checkDir ( Sextante.PORTABLE_GRASS_FOLDER, true, "GRASS GIS" );
	             String sPath = new String (SextanteGUI.getSextantePath() + File.separator + Sextante.PORTABLE_GRASS_FOLDER);             
	             SextanteGUI.setSettingParameterValue(SextanteGrassSettings.GRASS_FOLDER, sPath);
	             jGrassFolder.setFilepath(sPath);
	             if ( Sextante.isWindows()) {
	            	 //Set portable sh.exe file
	            	 SextanteGUI.checkFile ( Sextante.PORTABLE_MSYS_FILE, false, "GRASS GIS shell scripts" );
	            	 sPath = SextanteGUI.getSextantePath() + File.separator + Sextante.PORTABLE_MSYS_FILE;
	            	 SextanteGUI.setSettingParameterValue(SextanteGrassSettings.GRASS_WIN_SHELL, sPath);
	            	 jWinShellFile.setFilepath(sPath);
	             }
	             //Activate/deactivate the remaining widgets on this page
	             final boolean active = jPortableCheckBox.isSelected();
	             if ( active == true ) {
					jLabelGrassFolder.setEnabled(false);
					jGrassFolder.getTextField().setEnabled(false);
					jGrassFolder.getButton().setEnabled(false);
					if ( Sextante.isWindows()) {
						jLabelWinShell.setEnabled(false);
						jWinShellFile.getTextField().setEnabled(false);
						jWinShellFile.getButton().setEnabled(false);
					}
	             } else {
					jLabelGrassFolder.setEnabled(true);
					jGrassFolder.getTextField().setEnabled(true);
					jGrassFolder.getButton().setEnabled(true);					
					if ( Sextante.isWindows()) {
						jLabelWinShell.setEnabled(true);
						jWinShellFile.getTextField().setEnabled(true);
						jWinShellFile.getButton().setEnabled(true);
					}
				}
	            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	          }
	       });
	      
	      /* trigger update of GRASS modules registry */
	      jButtonUpdate.addActionListener(new ActionListener() {
	    	  public void actionPerformed(final ActionEvent evt) {
	    		  jButtonSetupGRASSActionPerformed(evt);
	    	  }
	      });	      
	}

	
	@Override
	public HashMap<String, String> getValues() {

		final HashMap<String, String> map = new HashMap<String, String>();

		map.put(SextanteGrassSettings.GRASS_ACTIVATE, new Boolean(jActivateCheckBox.isSelected()).toString());
		map.put(SextanteGrassSettings.GRASS_PORTABLE, new Boolean(jPortableCheckBox.isSelected()).toString());
		final String GrassPath = jWinShellFile.getFilepath();
		if (GrassPath != null) {
			map.put(SextanteGrassSettings.GRASS_FOLDER, jGrassFolder.getFilepath());
		}
		if (Sextante.isWindows()) {
			final String shellPath = jWinShellFile.getFilepath();
			if (shellPath != null) {
				map.put(SextanteGrassSettings.GRASS_WIN_SHELL, shellPath);
			}
		}
		map.put(SextanteGrassSettings.GRASS_LAT_LON_MODE, new Boolean(jCheckBoxLatLon.isSelected()).toString());
		map.put(SextanteGrassSettings.GRASS_3D_V_MODE, new Boolean(jCheckBox3DV.isSelected()).toString());
		map.put(SextanteGrassSettings.GRASS_NO_VECT_BBOX, new Boolean(jCheckBoxNoVectBBox.isSelected()).toString());
		map.put(SextanteGrassSettings.GRASS_IN_POLYLINES, new Boolean(jCheckBoxInPolylines.isSelected()).toString());
		map.put(SextanteGrassSettings.GRASS_CLEAN_POLYGONS, new Boolean(jCheckBoxCleanPolygons.isSelected()).toString());
		map.put(SextanteGrassSettings.GRASS_USE_SEXTANTE_NULL, new Boolean(jCheckBoxSextanteNull.isSelected()).toString());
		map.put(SextanteGrassSettings.GRASS_COMPATIBILITY_MODE, new Boolean(jCheckBoxCompatibility.isSelected()).toString());
		return map;
	}

	// Attempt to setup GRASS
	private void jButtonSetupGRASSActionPerformed(final ActionEvent evt) {

		// we set these values here in advance, since they are needed to perform
		// grass initialization. Have to look for a workaround to avoid this...
		final HashMap<String, String> map = getValues();
		SextanteGUI.setSettings(map);

		GrassAlgorithmProvider.deleteDescriptionFiles();
		GrassAlgorithmProvider.deleteAlgorithms();

		boolean failed = false;
		// 1: GRASS installation folder
		try {
			this.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			checkGrassFolder();
			this.setCursor(Cursor.getDefaultCursor());
		} catch (final WrongGrassFolderException e) {
			this.setCursor(Cursor.getDefaultCursor());
			JOptionPane.showMessageDialog(null, Sextante
					.getText("grass_error_binaries_folder"), Sextante
					.getText("grass_error_title"), JOptionPane.ERROR_MESSAGE);
			jGrassFolder.setFilepath("");
			failed = true;
		} finally {
			this.setCursor(Cursor.getDefaultCursor());
		}
		// 2: On Windows: sh.exe
		if (Sextante.isWindows() && (failed == false)) {
			try {
				this.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				if (checkGrassWinShell(jWinShellFile.getFilepath()) > 0) {
					this.setCursor(Cursor.getDefaultCursor());
					JOptionPane.showMessageDialog(null, Sextante
							.getText("grass_warning_missing_cmd"), Sextante
							.getText("grass_warning_title"),
							JOptionPane.WARNING_MESSAGE);
				}
				this.setCursor(Cursor.getDefaultCursor());
			} catch (final WrongGrassWinShellException e) {
				this.setCursor(Cursor.getDefaultCursor());
				JOptionPane.showMessageDialog(null, Sextante
						.getText("grass_error_win_shell_binary")
						+ "\n" + Sextante.getText("grass_shell_url"), Sextante
						.getText("grass_error_title"),
						JOptionPane.ERROR_MESSAGE);
				failed = true;
				GrassAlgorithmProvider.deleteAlgorithms();
				SextanteGUI.getGUIFactory().updateToolbox();
				jWinShellFile.setFilepath("");
			} finally {
				this.setCursor(Cursor.getDefaultCursor());
			}
		}

		// 3: GRASS version
		if (!isSupported() && (failed == false)) {
			this.setCursor(Cursor.getDefaultCursor());
			JOptionPane.showMessageDialog(null, Sextante
					.getText("grass_warning_version"), Sextante
					.getText("grass_warning_title"),
					JOptionPane.WARNING_MESSAGE);
		}

		// Setup GRASS!!!
		int num_algs = 0;
		try {
			this.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			num_algs = GrassAlgorithmProvider
					.createAlgorithmsDescriptionFiles();
			this.setCursor(Cursor.getDefaultCursor());
		} catch (final Exception e) {
			this.setCursor(Cursor.getDefaultCursor());
			JOptionPane.showMessageDialog(null, Sextante
					.getText("grass_error_setup"), "",
					JOptionPane.ERROR_MESSAGE);
		} finally {
			this.setCursor(Cursor.getDefaultCursor());
		}
		this.setCursor(Cursor.getDefaultCursor());
		if (num_algs > 0) {
			// Success
			SextanteGUI.updateAlgorithmProvider(GrassAlgorithmProvider.class);
			final HashMap<String, GeoAlgorithm> algs = Sextante.getAlgorithms()
					.get("GRASS");
			int iNumAlgs = 0;
			if (algs != null) {
				iNumAlgs = algs.size();
			}
			JOptionPane.showMessageDialog(null, Sextante
					.getText("grass_info_setup_success")
					+ " " + iNumAlgs + ". ", Sextante
					.getText("grass_info_title"),
					JOptionPane.INFORMATION_MESSAGE);
		}
	}

	private void checkGrassFolder() throws WrongGrassFolderException {

		final String grassFolder = jGrassFolder.getFilepath();
		// Minimal set of GRASS modules we need
		final String[] check_modules = { "g.region", "g.remove", "r.in.gdal",
				"r.info", "r.null", "r.out.gdal", "v.in.ogr", "v.info",
				"v.out.ogr", null };

		// Check if this is actually a valid shell and throw an error, if it's
		// not.
		if (grassFolder == null) {
			throw new WrongGrassFolderException();
		}

		if (grassFolder.length() < 2) {
			throw new WrongGrassFolderException();
		}

		if (grassFolder.trim().equals("")) {
			throw new WrongGrassFolderException();
		}

		File check = new File(grassFolder);
		if (!check.exists()) {
			throw new WrongGrassFolderException();
		}

		check = new File(grassFolder + File.separator + "etc" + File.separator
				+ "VERSIONNUMBER");
		if (!check.exists()) {
			throw new WrongGrassFolderException();
		}

		// Check for minimal set of GRASS modules
		int i = 0;
		while (check_modules[i] != null) {
			if (Sextante.isUnix() || Sextante.isMacOSX()) {
				check = new File(grassFolder + File.separator + "bin"
						+ File.separator + check_modules[i]);
			} else {
				check = new File(grassFolder + File.separator + "bin"
						+ File.separator + check_modules[i] + ".exe");
			}
			if (!check.exists()) {
				throw new WrongGrassFolderException();
			}
			i++;
		}

	}

	private int checkGrassWinShell(final String grassWinShell)
			throws WrongGrassWinShellException {

		int num_missing = 0;

		// Minimal set of command line tools we need to run GRASS scripts
		final String[] check_commands = { "which", "gawk.exe", "cut.exe",
				"grep.exe", "basename.exe", "sed.exe", "install.exe",
				"curl.exe", "bc.exe", "wc.exe", "paste.exe", "head.exe",
				"tail.exe", "cat.exe", "expr.exe", "xargs.exe", "ls.exe",
				"sort.exe", "cs2cs.exe", "gdalwarp.exe", "unzip.exe" };

		// r.in.aster needs gdalwarp
		// r.in.srtm needs unzip
		// bc.exe needs readline5.dll
		// v.in.garmin needs gpstrans and gardump
		// v.in/out.gpsbabel need gpsbabel
		// some scripts need curl
		// some scripts need cs2cs

		// Check if this is actually a valid shell and throw an error, if it's
		// not.
		if (grassWinShell == null) {
			throw new WrongGrassWinShellException();
		}

		if (grassWinShell.length() < 2) {
			throw new WrongGrassWinShellException();
		}

		if (grassWinShell.trim().equals("")) {
			throw new WrongGrassWinShellException();
		}

		final File check = new File(grassWinShell);
		if (!check.exists()) {
			throw new WrongGrassWinShellException();
		}

		// Look for required (recommended) binaries in "bin" folders of MSYS
		// and GRASS installation.
		// Note: if they are missing here, they could still be in some
		// user-defined
		// folder in %PATH%, so we only issue a warning and log all "missing"
		// binary names.
		String shToolsPath = grassWinShell;
		shToolsPath = shToolsPath.substring(0, shToolsPath
				.lastIndexOf(File.separator));
		String GrassPath = jGrassFolder.getFilepath();
		GrassPath = GrassPath + File.separator + "bin";
		for (final String element : check_commands) {
			boolean missing = false;
			final File check_sh = new File(shToolsPath + File.separator
					+ element);
			final File check_grass = new File(GrassPath + File.separator
					+ element);
			if (!check_sh.exists() && !check_grass.exists()) {
				missing = true;
			}
			if (missing == true) {
				Sextante
						.addWarningToLog("GRASS interface: External command "
								+ element
								+ " not found in either '"
								+ shToolsPath + "' or '" + GrassPath + "'");
				num_missing++;
			}
		}

		return (num_missing);
	}

	/*
	 * Returns the major version number of the GRASS version we are running.
	 * Returns "-1" if anything goes wrong.
	 */
	private static int getGrassMajorVersion() {

		final String sFolder = SextanteGUI
				.getSettingParameterValue(SextanteGrassSettings.GRASS_FOLDER);
		InputStreamReader isr = null;
		BufferedReader br = null;
		try {
			final File file = new File(sFolder + File.separator + "etc"
					+ File.separator + "VERSIONNUMBER");
			isr = new InputStreamReader(new FileInputStream(file));
			br = new BufferedReader(isr);
			final String sLine = br.readLine();
			final String[] sNumbers = sLine.split("\\.");
			final String sMajor = sNumbers[0];
			return (Integer.parseInt(sMajor));
		} catch (final Exception e) {
			return (-1);
		} finally {
			try {
				br.close();
				isr.close();
			} catch (final Exception e) {
				return (-1);
			}
		}
	}

	/*
	 * Returns the minor version number of the GRASS version we are running.
	 * Returns "-1" if anything goes wrong.
	 */
	private static int getGrassMinorVersion() {

		final String sFolder = SextanteGUI
				.getSettingParameterValue(SextanteGrassSettings.GRASS_FOLDER);
		InputStreamReader isr = null;
		BufferedReader br = null;
		try {
			final File file = new File(sFolder + File.separator + "etc"
					+ File.separator + "VERSIONNUMBER");
			isr = new InputStreamReader(new FileInputStream(file));
			br = new BufferedReader(isr);
			final String sLine = br.readLine();
			final String[] sNumbers = sLine.split("\\.");
			final String sMinor = sNumbers[1];
			return (Integer.parseInt(sMinor));
		} catch (final Exception e) {
			return (-1);
		} finally {
			try {
				br.close();
				isr.close();
			} catch (final Exception e) {
				return (-1);
			}
		}
	}

	/*
	 * Returns true only if the detected GRASS version is not too old or too
	 * young to be supported by this interface.
	 */
	public static boolean isSupported() {
		if ((getGrassMajorVersion() < MAJOR_MIN)
				|| (getGrassMajorVersion() > MAJOR_MAX)) {
			return (false);
		}
		if ((getGrassMinorVersion() < MINOR_MIN)
				|| (getGrassMinorVersion() > MINOR_MAX)) {
			return (false);
		}
		return (true);
	}

}
