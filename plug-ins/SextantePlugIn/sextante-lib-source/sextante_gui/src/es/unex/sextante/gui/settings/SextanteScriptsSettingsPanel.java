package es.unex.sextante.gui.settings;

import info.clearthought.layout.TableLayout;
import info.clearthought.layout.TableLayoutConstants;

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.HashMap;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.algorithm.FileSelectionPanel;
import es.unex.sextante.gui.cmd.ScriptAlgorithmProvider;
import es.unex.sextante.gui.core.SextanteGUI;

public class SextanteScriptsSettingsPanel
extends
SettingPanel {

	private JCheckBox          jPortableCheckBox;
	private JLabel             jLabelScripts;
	private FileSelectionPanel jScriptsFolder;
	private JLabel             jLabelUpdate;
	private JButton            jButtonUpdate;


	@Override
	protected void initGUI() {
		final TableLayout thisLayout = new TableLayout(new double[][] {
				{ 	SextanteConfigurationDialog.SPACER_SMALL, 
					TableLayoutConstants.MINIMUM,
					TableLayoutConstants.FILL,
					SextanteConfigurationDialog.SPACER_SMALL },
					{ 	SextanteConfigurationDialog.SPACER_SMALL,
						TableLayoutConstants.MINIMUM, //row 1 
						TableLayoutConstants.MINIMUM, //row 2
						TableLayoutConstants.MINIMUM, //row 3
						TableLayoutConstants.MINIMUM, //row 4
						TableLayoutConstants.FILL } });
		thisLayout.setHGap(5);
		thisLayout.setVGap(5);
		this.setLayout(thisLayout);

		jPortableCheckBox = new JCheckBox(Sextante.getText("Portable"));
		final String sActivatePortable = SextanteGUI.getSettingParameterValue(SextanteScriptsSettings.SCRIPTS_PORTABLE);
		final boolean bActivatePortable = Boolean.parseBoolean(sActivatePortable);
		jPortableCheckBox.setSelected(bActivatePortable);
		this.add(jPortableCheckBox, "1, 1");		

		jLabelScripts = new JLabel();
		jLabelScripts.setEnabled(true);
		if (bActivatePortable == true) {
			jLabelScripts.setEnabled(false);
		}		
		jLabelScripts.setText(Sextante.getText("Scripts_folder"));
		this.add(jLabelScripts, "1, 2");
		jScriptsFolder = new FileSelectionPanel(true, true, (String[]) null, Sextante.getText("selector_choose_folder"));
		jScriptsFolder.getTextField().setEnabled(true);
		jScriptsFolder.getButton().setEnabled(true);
		if (bActivatePortable == true) {
			jScriptsFolder.getTextField().setEnabled(false);
			jScriptsFolder.getButton().setEnabled(false);
		}
		final String sFolder = SextanteGUI.getSettingParameterValue(SextanteScriptsSettings.SCRIPTS_FOLDER);
		jScriptsFolder.setFilepath(sFolder);
		this.add(jScriptsFolder, "2, 2");

		this.add(new JSeparator(SwingConstants.HORIZONTAL), "1, 3, 2, 3");
		/* ----------------------------------------------------------- */		
		
		jLabelUpdate = new JLabel();
		jLabelUpdate.setText( Sextante.getText("update_library") );
		jLabelUpdate.setEnabled(true);
		this.add(jLabelUpdate, "1, 4");
		jButtonUpdate = new JButton(Sextante.getText("load_scripts"));
		this.add(jButtonUpdate, "2, 4");
		
		/**********************************/
		/** Action listeners for widgets **/
		/**********************************/		
		
		jPortableCheckBox.addActionListener(new ActionListener() {
			public void actionPerformed(final ActionEvent arg0) {        	 
				setCursor(new Cursor(Cursor.WAIT_CURSOR));
				SextanteGUI.setSettingParameterValue(SextanteScriptsSettings.SCRIPTS_PORTABLE,
						new Boolean(jPortableCheckBox.isSelected()).toString());             
				//Set portable user scripts folder
				SextanteGUI.checkDir ( Sextante.PORTABLE_SCRIPTS_FOLDER, true, "SEXTANTE user scripts" );
				String sPath = new String (SextanteGUI.getSextantePath() + File.separator + 
						Sextante.PORTABLE_SCRIPTS_FOLDER);
				SextanteGUI.setSettingParameterValue(SextanteScriptsSettings.SCRIPTS_FOLDER, sPath);
				jScriptsFolder.setFilepath(sPath);
				//Activate/deactivate the remaining widgets on this page
				final boolean active = jPortableCheckBox.isSelected();
				if ( active == true ) {
					jLabelScripts.setEnabled(false);
					jScriptsFolder.getTextField().setEnabled(false);
					jScriptsFolder.getButton().setEnabled(false);

				} else {
					jLabelScripts.setEnabled(true);
					jScriptsFolder.getTextField().setEnabled(true);
					jScriptsFolder.getButton().setEnabled(true);					

				}
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			}
		});		
		
		jButtonUpdate.addActionListener(new ActionListener() {
			public void actionPerformed(final ActionEvent arg0) {
				SextanteGUI.setSettingParameterValue(SextanteScriptsSettings.SCRIPTS_FOLDER, jScriptsFolder.getFilepath());
				SextanteGUI.updateAlgorithmProvider(ScriptAlgorithmProvider.class);
				final int iNumAlgs = Sextante.getAlgorithms().get(new ScriptAlgorithmProvider().getName()).size();
				JOptionPane.showMessageDialog(null, Sextante.getText("ScriptsLoaded") + " " + iNumAlgs + ". ",
						Sextante.getText("Scripts"), JOptionPane.INFORMATION_MESSAGE);
			}
		});		
	}


	@Override
	public HashMap<String, String> getValues() {

		final HashMap<String, String> map = new HashMap<String, String>();
		map.put(SextanteScriptsSettings.SCRIPTS_PORTABLE, new Boolean(jPortableCheckBox.isSelected()).toString());
		final String path = jScriptsFolder.getFilepath();
		if (path != null) {
			map.put(SextanteScriptsSettings.SCRIPTS_FOLDER, path);
		}
		return map;

	}

}
