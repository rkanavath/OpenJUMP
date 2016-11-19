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
import es.unex.sextante.gui.modeler.ModelerAlgorithmProvider;

public class SextanteModelerSettingsPanel
extends
SettingPanel {


	private JCheckBox          jPortableCheckBox;
	private JLabel             jLabelModels;
	private FileSelectionPanel jModelsFolder;	
	private JButton            jButtonUpdate;
	private JLabel             jLabelUpdate;


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
		final String sActivatePortable = SextanteGUI.getSettingParameterValue(SextanteModelerSettings.MODELS_PORTABLE);
		final boolean bActivatePortable = Boolean.parseBoolean(sActivatePortable);
		jPortableCheckBox.setSelected(bActivatePortable);
		this.add(jPortableCheckBox, "1, 1");		

		jLabelModels = new JLabel();
		jLabelModels.setEnabled(true);
		if (bActivatePortable == true) {
			jLabelModels.setEnabled(false);
		}		
		jLabelModels.setText(Sextante.getText("Models_folder"));
		this.add(jLabelModels, "1, 2");
		jModelsFolder = new FileSelectionPanel(true, true, (String[]) null, Sextante.getText("selector_choose_folder"));
		jModelsFolder.getTextField().setEnabled(true);
		jModelsFolder.getButton().setEnabled(true);
		if (bActivatePortable == true) {
			jModelsFolder.getTextField().setEnabled(false);
			jModelsFolder.getButton().setEnabled(false);
		}
		final String sFolder = SextanteGUI.getSettingParameterValue(SextanteModelerSettings.MODELS_FOLDER);
		jModelsFolder.setFilepath(sFolder);
		this.add(jModelsFolder, "2, 2");

		this.add(new JSeparator(SwingConstants.HORIZONTAL), "1, 3, 2, 3");
		/* ----------------------------------------------------------- */		
		
		jLabelUpdate = new JLabel();
		jLabelUpdate.setText( Sextante.getText("update_library") );
		jLabelUpdate.setEnabled(true);
		this.add(jLabelUpdate, "1, 4");
		jButtonUpdate = new JButton(Sextante.getText("load_models"));
		this.add(jButtonUpdate, "2, 4");	
		
		/**********************************/
		/** Action listeners for widgets **/
		/**********************************/		
		
		jPortableCheckBox.addActionListener(new ActionListener() {
			public void actionPerformed(final ActionEvent arg0) {        	 
				setCursor(new Cursor(Cursor.WAIT_CURSOR));
				SextanteGUI.setSettingParameterValue(SextanteModelerSettings.MODELS_PORTABLE,
						new Boolean(jPortableCheckBox.isSelected()).toString());             
				//Set portable user models folder
				SextanteGUI.checkDir ( Sextante.PORTABLE_MODELS_FOLDER, true, "SEXTANTE user models" );
				String sPath = new String (SextanteGUI.getSextantePath() + File.separator + 
						Sextante.PORTABLE_MODELS_FOLDER);
				SextanteGUI.setSettingParameterValue(SextanteModelerSettings.MODELS_FOLDER, sPath);
				jModelsFolder.setFilepath(sPath);
				//Activate/deactivate the remaining widgets on this page
				final boolean active = jPortableCheckBox.isSelected();
				if ( active == true ) {
					jLabelModels.setEnabled(false);
					jModelsFolder.getTextField().setEnabled(false);
					jModelsFolder.getButton().setEnabled(false);

				} else {
					jLabelModels.setEnabled(true);
					jModelsFolder.getTextField().setEnabled(true);
					jModelsFolder.getButton().setEnabled(true);					

				}
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			}
		});		
		
		jButtonUpdate.addActionListener(new ActionListener() {
			public void actionPerformed(final ActionEvent arg0) {
				SextanteGUI.setSettingParameterValue(SextanteModelerSettings.MODELS_FOLDER, jModelsFolder.getFilepath());
				SextanteGUI.updateAlgorithmProvider(ModelerAlgorithmProvider.class);
				final int iNumAlgs = Sextante.getAlgorithms().get(new ModelerAlgorithmProvider().getName()).size();
				JOptionPane.showMessageDialog(null, Sextante.getText("ModelsLoaded") + " " + iNumAlgs + ". ",
						Sextante.getText("Models"), JOptionPane.INFORMATION_MESSAGE);
			}
		});
	}


	@Override
	public HashMap<String, String> getValues() {

		final HashMap<String, String> map = new HashMap<String, String>();
		final String path = jModelsFolder.getFilepath();
		map.put(SextanteModelerSettings.MODELS_PORTABLE, new Boolean(jPortableCheckBox.isSelected()).toString());
		if (path != null) {
			map.put(SextanteModelerSettings.MODELS_FOLDER, path);
		}
		return map;

	}

}
