package es.unex.sextante.gui.modeler.parameters;

import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JOptionPane;

import es.unex.sextante.additionalInfo.AdditionalInfoImageLayer;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.exceptions.NullParameterAdditionalInfoException;
import es.unex.sextante.gui.modeler.ModelerPanel;
import es.unex.sextante.parameters.Parameter;
import es.unex.sextante.parameters.ParameterImageLayer;

public class ImageLayerPanel
         extends
            ParameterPanel {

   private JCheckBox jCheckBoxMandatory;


   public ImageLayerPanel(final JDialog parent,
                           final ModelerPanel panel) {

      super(parent, panel);

   }


   public ImageLayerPanel(final ModelerPanel panel) {

      super(panel);

   }


   @Override
   protected void initGUI() {

      super.initGUI();

      super.setTitle(Sextante.getText("modeler_add_par_image"));
      
      super.setPreferredSize(new java.awt.Dimension(400, 182));      
      
      try {
         jCheckBoxMandatory = new JCheckBox();
         jCheckBoxMandatory.setSelected(true);
         jPanelMiddle.add(jCheckBoxMandatory);
         jCheckBoxMandatory.setText(Sextante.getText("Mandatory"));
      }
      catch (final Exception e) {
         Sextante.addErrorToLog(e);
      }

   }


   @Override
   protected boolean prepareParameter() {


      final String sDescription = jTextFieldDescription.getText();

      if (sDescription.length() != 0) {
         final AdditionalInfoImageLayer addInfo = new AdditionalInfoImageLayer(jCheckBoxMandatory.isSelected());
         m_Parameter = new ParameterImageLayer();
         m_Parameter.setParameterAdditionalInfo(addInfo);
         m_Parameter.setParameterDescription(jTextFieldDescription.getText());
         
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
         final AdditionalInfoImageLayer ai = (AdditionalInfoImageLayer) param.getParameterAdditionalInfo();
         jCheckBoxMandatory.setSelected(ai.getIsMandatory());
      }
      catch (final NullParameterAdditionalInfoException e) {
         e.printStackTrace();
      }

   }


   @Override
   public String getParameterDescription() {

      return Sextante.getText("Image_layer");

   }


   @Override
   public boolean parameterCanBeAdded() {

      return true;

   }

}
