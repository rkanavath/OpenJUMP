package es.unex.sextante.gui.modeler;

import info.clearthought.layout.TableLayout;
import info.clearthought.layout.TableLayoutConstants;

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import es.unex.sextante.core.Sextante;

public class OutputLayerSettingsPanel
         extends
            JPanel {

   private JCheckBox  jCheckBoxAdd;
   private JTextField jTextFieldName;


   public OutputLayerSettingsPanel() {

      super();
      initGUI();

   }


   private void initGUI() {
      try {
    	  	this.setPreferredSize(new java.awt.Dimension(271, 48));
            {
               jCheckBoxAdd = new JCheckBox(Sextante.getText("Keep_as_final_result"));
               jCheckBoxAdd.addItemListener(new ItemListener() {
                  public void itemStateChanged(final ItemEvent e) {
                     jTextFieldName.setEnabled(jCheckBoxAdd.isSelected());                     
                  }
               });               
            }
            {
               jTextFieldName = new JTextField();
               jTextFieldName.setMaximumSize(new java.awt.Dimension(340, 18));
               jTextFieldName.setEnabled(false);               
            }
            final TableLayout thisLayout = new TableLayout(new double[][] {
                    { TableLayoutConstants.MINIMUM, TableLayoutConstants.FILL},
                    { TableLayoutConstants.MINIMUM } });
            thisLayout.setHGap(5);
            thisLayout.setVGap(5);
            this.setLayout(thisLayout);
            this.add(jCheckBoxAdd, "0, 0");
            this.add(jTextFieldName, "1, 0");            
      }
      catch (final Exception e) {
         Sextante.addErrorToLog(e);
      }
   }

   
   public JCheckBox getCheckBox () {
	   return ( this.jCheckBoxAdd );
   }
   
   
  public JTextField getTextField () {
	  return ( this.jTextFieldName );
  }

   public void setKeepAsFinalResult(final boolean bKeep) {

      jCheckBoxAdd.setSelected(bKeep);

   }


   public boolean getKeepAsFinalResult() {

      return this.jCheckBoxAdd.isSelected();

   }


   @Override
   public void setName(final String sName) {

      jTextFieldName.setText(sName);

   }


   @Override
   public String getName() {

      return jTextFieldName.getText();

   }


}
