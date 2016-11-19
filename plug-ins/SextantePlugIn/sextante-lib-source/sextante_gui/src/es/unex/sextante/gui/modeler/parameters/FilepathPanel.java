package es.unex.sextante.gui.modeler.parameters;

import info.clearthought.layout.TableLayout;
import info.clearthought.layout.TableLayoutConstants;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

import es.unex.sextante.additionalInfo.AdditionalInfoFilepath;
import es.unex.sextante.additionalInfo.AdditionalInfoVectorLayer;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.exceptions.NullParameterAdditionalInfoException;
import es.unex.sextante.gui.modeler.ModelerPanel;
import es.unex.sextante.parameters.Parameter;
import es.unex.sextante.parameters.ParameterFilepath;

public class FilepathPanel
         extends
            ParameterPanel {

   private JTextField jTextFieldExtension;
   private JLabel     jLabelExtension;
   private JComboBox  jComboBoxType;
   private JLabel     jLabelType;
   private JCheckBox  jCheckBoxVoxel;


   public FilepathPanel(final JDialog parent,
                      final ModelerPanel panel) {

      super(parent, panel);

   }


   public FilepathPanel(final ModelerPanel panel) {

      super(panel);

   }


   @Override
   protected void initGUI() {

	   super.initGUI();

	   super.setTitle(Sextante.getText("modeler_add_par_filepath"));

	   super.setPreferredSize(new java.awt.Dimension(400, 242));

	   try {
		   {
			   final TableLayout thisLayout = new TableLayout
			   (new double[][] { { TableLayoutConstants.MINIMUM, 5.0, TableLayoutConstants.FILL },
					   { 	TableLayoutConstants.MINIMUM,
				   			1.0,
				   			TableLayoutConstants.MINIMUM,
				   			1.0,
				   			TableLayoutConstants.MINIMUM } });
			   thisLayout.setHGap(5);
			   thisLayout.setVGap(5);

			   jPanelMiddle.setLayout(thisLayout);

			   jLabelType = new JLabel();
			   jLabelType.setText(Sextante.getText("filepath_type"));
			   jPanelMiddle.add(jLabelType, "0, 0");			   
			   final ComboBoxModel jComboBoxTypeModel = new DefaultComboBoxModel(new String[] {
					   Sextante.getText("filepath_file_open"),
					   Sextante.getText("filepath_file_save"),
					   Sextante.getText("filepath_folder") });
			   jComboBoxType = new JComboBox();
			   jComboBoxType.setModel(jComboBoxTypeModel);
			   jPanelMiddle.add(jComboBoxType, "2, 0");

			   jLabelExtension = new JLabel();
			   jPanelMiddle.add(jLabelExtension, "0, 2");
			   jLabelExtension.setText(Sextante.getText("file_extension"));
			   jTextFieldExtension = new JTextField();
			   jPanelMiddle.add(jTextFieldExtension, "2, 2");

			   jCheckBoxVoxel = new JCheckBox();
			   jCheckBoxVoxel.setSelected(false);
			   jCheckBoxVoxel.setText(Sextante.getText("filepath_is_voxel_data"));
			   jPanelMiddle.add(jCheckBoxVoxel, "0, 4");
		   }
	   }
	   catch (final Exception e) {
		   Sextante.addErrorToLog(e);
	   }

   }


   @Override
   public String getParameterDescription() {

      return Sextante.getText("Filepath");

   }


   @Override
   protected boolean prepareParameter() {


      final String sDescription = jTextFieldDescription.getText();
      String[] sDefault;
      
      if (sDescription.length() != 0) {
         final AdditionalInfoFilepath ai = new AdditionalInfoFilepath();
         sDefault = new String[1];
         sDefault[0]=jTextFieldExtension.getText();
         if ( sDefault[0] != null && sDefault[0].length() > 0 ) {
        	 ai.setExtensions(sDefault);
         }
         ai.setIsFolder(false);
         ai.setIsOpenDialog(false);
         switch (jComboBoxType.getSelectedIndex()) {
			case 0:
		         ai.setIsOpenDialog(true);
				break;
			case 2:
				ai.setIsFolder(true);
				break;
		 }
         ai.setIsVoxelData(jCheckBoxVoxel.isSelected());
         m_Parameter = new ParameterFilepath();
         m_Parameter.setParameterDescription(sDescription);
         m_Parameter.setParameterAdditionalInfo(ai);
         
         m_Parameter.setColorR(m_Color.getRed());        
         m_Parameter.setColorG(m_Color.getGreen());        
         m_Parameter.setColorB(m_Color.getBlue());        
         m_Parameter.setColorAlpha(m_Color.getAlpha());         
         
         return true;
      }
      else {
         JOptionPane.showMessageDialog(null, Sextante.getText("Invalid_description"), Sextante.getText("Warning"),
                  JOptionPane.WARNING_MESSAGE);
         return false;
      }


   }


   @Override
   public void setParameter(final Parameter param) {

      super.setParameter(param);

      try {
         final AdditionalInfoFilepath ai = (AdditionalInfoFilepath) param.getParameterAdditionalInfo();
         if ( ai.getExtensions() != null )
        	 jTextFieldExtension.setText(ai.getExtensions()[0]);
         jComboBoxType.setSelectedIndex(1);
         if ( ai.isOpenDialog() == true )
        	 jComboBoxType.setSelectedIndex(0);
         if ( ai.isFolder() == true )
        	 jComboBoxType.setSelectedIndex(2);
         if ( ai.getIsVoxelData() == true )
        	 jCheckBoxVoxel.setSelected(true);
      }
      catch (final NullParameterAdditionalInfoException e) {
         e.printStackTrace();
      }

   }


   @Override
   public boolean parameterCanBeAdded() {

      return true;

   }

}
