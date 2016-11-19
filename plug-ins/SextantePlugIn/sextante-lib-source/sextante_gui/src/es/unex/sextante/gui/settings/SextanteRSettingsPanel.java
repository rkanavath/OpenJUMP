package es.unex.sextante.gui.settings;

import info.clearthought.layout.TableLayout;
import info.clearthought.layout.TableLayoutConstants;

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.net.URL;
import java.util.HashMap;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.algorithm.FileSelectionPanel;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.r.RAlgorithmProvider;


public class SextanteRSettingsPanel
         extends
            SettingPanel {

   private FileSelectionPanel jRFolder;
   private JButton            jButton;
   private JLabel             jLabelFolder;
   private JLabel             jLabelLoadRScripts;
   private JCheckBox          jActivateCheckBox;
   private JCheckBox          jPortableCheckBox;
   private JLabel             jLabelScriptsFolder;
   private FileSelectionPanel jRScriptsFolder;


   @Override
   protected void initGUI() {
	   
      final TableLayout thisLayout = new TableLayout(new double[][] {
               { SextanteConfigurationDialog.SPACER_SMALL,
            	   TableLayoutConstants.MINIMUM,
            	   TableLayoutConstants.FILL,
            	   SextanteConfigurationDialog.SPACER_SMALL },
               { SextanteConfigurationDialog.SPACER_SMALL,
            		   TableLayoutConstants.MINIMUM, // row 1
            		   TableLayoutConstants.MINIMUM, // row 2
            		   TableLayoutConstants.MINIMUM, // row 3
            		   TableLayoutConstants.MINIMUM, // row 4
            		   TableLayoutConstants.MINIMUM, // row 5
            		   TableLayoutConstants.MINIMUM, // row 6
            		   TableLayoutConstants.MINIMUM, // row 7
            		   TableLayoutConstants.FILL,
					   TableLayoutConstants.MINIMUM,
					   SextanteConfigurationDialog.SPACER_SMALL } });
      thisLayout.setHGap(5);
      thisLayout.setVGap(5);
      this.setLayout(thisLayout);
      
      jActivateCheckBox = new JCheckBox(Sextante.getText("ActivateProvider") + " \"R\"" );
      final String sActivate = SextanteGUI.getSettingParameterValue(SextanteRSettings.R_ACTIVATE);
      final boolean bActivate = Boolean.parseBoolean(sActivate);
      jActivateCheckBox.setSelected(bActivate);
      this.add(jActivateCheckBox, "1,1");      
      
      this.add(new JSeparator(SwingConstants.HORIZONTAL), "1, 2, 2, 2");
      /* -----------------------------------------------------------*/      
      
      jPortableCheckBox = new JCheckBox(Sextante.getText("Portable"));
      final String sActivatePortable = SextanteGUI.getSettingParameterValue(SextanteRSettings.R_PORTABLE);
      final boolean bActivatePortable = Boolean.parseBoolean(sActivatePortable);
      jPortableCheckBox.setEnabled(bActivate);
      jPortableCheckBox.setSelected(bActivatePortable);      
      this.add(jPortableCheckBox, "1,3");
      
      jLabelFolder = new JLabel();
      jLabelFolder.setEnabled(bActivate);
      if ( bActivatePortable == true ) {
    	  jLabelFolder.setEnabled(false);
      }
      this.add(jLabelFolder, "1, 4");
      jLabelFolder.setText(Sextante.getText("R_folder"));
      jRFolder = new FileSelectionPanel(true, true, (String[]) null, Sextante.getText("selector_choose_folder"));
      jRFolder.getTextField().setEnabled(bActivate);
      jRFolder.getButton().setEnabled(bActivate);
      if ( bActivatePortable == true ) {
    	  jRFolder.getTextField().setEnabled(false);
          jRFolder.getButton().setEnabled(false);    	  
      }
      final String sFolder = SextanteGUI.getSettingParameterValue(SextanteRSettings.R_FOLDER);
      jRFolder.setFilepath(sFolder);
      this.add(jRFolder, "2, 4");
      
      jLabelScriptsFolder = new JLabel();
      jLabelScriptsFolder.setEnabled(bActivate);
      if ( bActivatePortable == true ) {
    	  jLabelScriptsFolder.setEnabled(false);
      }
      this.add(jLabelScriptsFolder, "1, 5");
      jLabelScriptsFolder.setText(Sextante.getText("R_Scripts_folder"));
      jRScriptsFolder = new FileSelectionPanel(true, true, (String[]) null, Sextante.getText("selector_choose_folder"));
      jRScriptsFolder.getTextField().setEnabled(bActivate);
      jRScriptsFolder.getButton().setEnabled(bActivate);
      if ( bActivatePortable == true ) {
    	  jRScriptsFolder.getTextField().setEnabled(false);
          jRScriptsFolder.getButton().setEnabled(false);
      }
      final String sScriptsFolder = SextanteGUI.getSettingParameterValue(SextanteRSettings.R_SCRIPTS_FOLDER);
      jRScriptsFolder.setFilepath(sScriptsFolder);
      this.add(jRScriptsFolder, "2, 5");
      
      this.add(new JSeparator(SwingConstants.HORIZONTAL), "1, 6, 2, 6");
      /* -----------------------------------------------------------*/            
      
      jLabelLoadRScripts = new JLabel();
      jLabelLoadRScripts.setText( Sextante.getText("update_library") );
      jLabelLoadRScripts.setEnabled(bActivate);
      this.add(jLabelLoadRScripts, "1, 7");      
      jButton = new JButton(Sextante.getText("load_R_scripts"));
      jButton.setEnabled(bActivate);
      this.add(jButton, "2, 7");

      /* add provider logo and URL */
      final URL res = getClass().getClassLoader().getResource("images/Rlogo.gif");
      if (res != null) {
    	  final ImageIcon logo = new ImageIcon(res);
    	  JLabel logoLabel = new JLabel(logo);
    	  logoLabel.setIconTextGap(4);
    	  logoLabel.setVerticalTextPosition(SwingConstants.BOTTOM);
    	  logoLabel.setText("<html><i><a href=http://www.r-project.org/>http://www.r-project.org/</a></i></html>");
    	  this.add(logoLabel,"1, 9, 2, 9");
      }      

      
      /* Action listeners for widgets */
            
      jActivateCheckBox.addActionListener(new ActionListener() {
          public void actionPerformed(final ActionEvent arg0) {        	 
             setCursor(new Cursor(Cursor.WAIT_CURSOR));
             SextanteGUI.setSettingParameterValue(SextanteRSettings.R_ACTIVATE,
                      new Boolean(jActivateCheckBox.isSelected()).toString());               
             SextanteGUI.updateAlgorithmProvider(RAlgorithmProvider.class);             
             //Activate/deactivate the remaining widgets on this page
             boolean active = jActivateCheckBox.isSelected();
             jPortableCheckBox.setEnabled(active);
        	 jLabelFolder.setEnabled(active);
        	 jRFolder.getTextField().setEnabled(active);
             jRFolder.getButton().setEnabled(active);
             jLabelScriptsFolder.setEnabled(active);
             jRScriptsFolder.getTextField().setEnabled(active);
             jRScriptsFolder.getButton().setEnabled(active);
             jLabelLoadRScripts.setEnabled(active);
             jButton.setEnabled(active);
             jActivateCheckBox.getParent().repaint();
             active = jPortableCheckBox.isSelected();
             if ( active == true ) {
            	 jLabelFolder.setEnabled(false);
            	 jRFolder.getTextField().setEnabled(false);
                 jRFolder.getButton().setEnabled(false);
                 jLabelScriptsFolder.setEnabled(false);
                 jRScriptsFolder.getTextField().setEnabled(false);
                 jRScriptsFolder.getButton().setEnabled(false);
             }           
             setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
          }
       });
 
      jPortableCheckBox.addActionListener(new ActionListener() {
          public void actionPerformed(final ActionEvent arg0) {        	 
             setCursor(new Cursor(Cursor.WAIT_CURSOR));
             SextanteGUI.setSettingParameterValue(SextanteRSettings.R_PORTABLE,
                      new Boolean(jPortableCheckBox.isSelected()).toString());             
             //Set portable R bin dir
             SextanteGUI.checkDir ( Sextante.PORTABLE_R_FOLDER, true, "R" );
             String sPath = new String (SextanteGUI.getSextantePath() + File.separator + Sextante.PORTABLE_R_FOLDER);             
             SextanteGUI.setSettingParameterValue(SextanteRSettings.R_FOLDER, sPath);
             jRFolder.setFilepath(sPath);
             //Set portable scripts dir
             SextanteGUI.checkDir ( Sextante.PORTABLE_R_SCRIPTS_FOLDER, false, "R user scripts" );
             sPath = SextanteGUI.getSextantePath() + File.separator + Sextante.PORTABLE_R_SCRIPTS_FOLDER;
             SextanteGUI.setSettingParameterValue(SextanteRSettings.R_SCRIPTS_FOLDER, sPath);
             jRScriptsFolder.setFilepath(sPath);
             //Activate/deactivate the remaining widgets on this page
             final boolean active = jPortableCheckBox.isSelected();
             if ( active == true ) {
            	 jLabelFolder.setEnabled(false);
            	 jRFolder.getTextField().setEnabled(false);
                 jRFolder.getButton().setEnabled(false);
                 jLabelScriptsFolder.setEnabled(false);
                 jRScriptsFolder.getTextField().setEnabled(false);
                 jRScriptsFolder.getButton().setEnabled(false);
             } else {
            	 jLabelFolder.setEnabled(true);
            	 jRFolder.getTextField().setEnabled(true);
                 jRFolder.getButton().setEnabled(true);
                 jLabelScriptsFolder.setEnabled(true);
                 jRScriptsFolder.getTextField().setEnabled(true);
                 jRScriptsFolder.getButton().setEnabled(true);            	 
             }
             setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
          }
       });

      /* this one refreshes the registry of R scripts */
      jButton.addActionListener(new ActionListener() {
          public void actionPerformed(final ActionEvent arg0) {
             SextanteGUI.setSettingParameterValue(SextanteRSettings.R_SCRIPTS_FOLDER, jRScriptsFolder.getFilepath());
             SextanteGUI.updateAlgorithmProvider(RAlgorithmProvider.class);
             final int iNumAlgs = Sextante.getAlgorithms().get(new RAlgorithmProvider().getName()).size();
             JOptionPane.showMessageDialog(null, Sextante.getText("RScriptsLoaded") + " " + iNumAlgs + ". ",
                      Sextante.getText("Scripts"), JOptionPane.INFORMATION_MESSAGE);
          }
       });
      
   }


   @Override
   public HashMap<String, String> getValues() {

      final HashMap<String, String> map = new HashMap<String, String>();
      String path = jRFolder.getFilepath();
      if (path != null) {
         map.put(SextanteRSettings.R_FOLDER, path);
      }
      path = jRScriptsFolder.getFilepath();
      if (path != null) {
         map.put(SextanteRSettings.R_SCRIPTS_FOLDER, path);
      }
      map.put(SextanteRSettings.R_ACTIVATE, new Boolean(jActivateCheckBox.isSelected()).toString());
      return map;

   }

}
