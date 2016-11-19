package es.unex.sextante.gui.modeler.parameters;

import info.clearthought.layout.TableLayout;
import info.clearthought.layout.TableLayoutConstants;

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import es.unex.sextante.core.ParametersSet;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.modeler.ColorComboBox;
import es.unex.sextante.gui.modeler.ModelerPanel;
import es.unex.sextante.parameters.Parameter;

public abstract class ParameterPanel
         extends
            JDialog {

   protected JPanel       	jPanelName;
   protected ColorComboBox	jComboColor;
   protected JButton      	jButtonOk;
   protected JButton      	jButtonCancel;
   protected JLabel       	jLabelDescription;
   protected JPanel       	jPanelMiddle;
   protected JPanel       	jPanelButtons;
   protected JTextField   	jTextFieldDescription;
   protected Parameter    	m_Parameter;
   protected ModelerPanel 	m_ModelerPanel;
   protected Color			m_Color = null;


   public abstract String getParameterDescription();


   protected abstract boolean prepareParameter();


   public abstract boolean parameterCanBeAdded();


   public ParameterPanel(final JDialog dialog,
                         final ModelerPanel modelerPanel) {

      super(dialog, "", true);
      this.setLocationRelativeTo(null);

      m_ModelerPanel = modelerPanel;

      initGUI();

   }


   public ParameterPanel(final ModelerPanel modelerPanel) {

      super(SextanteGUI.getMainFrame(), "", true);
      this.setLocationRelativeTo(null);

      m_ModelerPanel = modelerPanel;
      
      /* default color for new parameters is white */
      if ( m_Color == null )
    	  m_Color = new Color( 	255, 255, 255, 255 );

      initGUI();

   }


   protected void initGUI() {

	  /* the preferred size for this dialog is set by a more
	     specific class (e.g. NumericalValuePanel) after running
	     this (super) class' constructor.
	   */
      //this.setSize(new java.awt.Dimension(390, 300));
      //this.setPreferredSize(new java.awt.Dimension(390, 300));
	  this.setResizable(true);
      {
         final TableLayout thisLayout = new TableLayout(new double[][] { { 3.0, TableLayoutConstants.FILL, 3.0 },
                  { 3.0, TableLayoutConstants.MINIMUM, TableLayoutConstants.FILL, TableLayoutConstants.MINIMUM, 3.0, 30.0, 3.0 } });
         thisLayout.setHGap(5);
         thisLayout.setVGap(5);
         this.setLayout(thisLayout);
         {
            jPanelName = new JPanel();
            final TableLayout jPanelNameLayout = new TableLayout
            	(new double[][] { { TableLayoutConstants.MINIMUM, 5.0, TableLayoutConstants.FILL },
                     { 20.0, 3.0, TableLayoutConstants.MINIMUM, 3.0 } });
            jPanelNameLayout.setHGap(5);
            jPanelNameLayout.setVGap(5);
            jPanelName.setLayout(jPanelNameLayout);
            this.add(jPanelName, "1, 1");
            {
                jLabelDescription = new JLabel();
                jPanelName.add(jLabelDescription, "0, 0");
                jLabelDescription.setText(Sextante.getText("Description"));
             }            
            {
               jTextFieldDescription = new JTextField();
               jTextFieldDescription.setText(getDefaultName());
               jPanelName.add(jTextFieldDescription, "2, 0");
            }
            jPanelName.add(new JSeparator(SwingConstants.HORIZONTAL), "0, 2, 2, 2");
         }
         {
            jPanelButtons = new JPanel();
            final TableLayout jPanelButtonsLayout = new TableLayout(new double[][] { { 5.0, 90.0, TableLayoutConstants.FILL, 90.0, 5.0, 90.0 },
            			{ 30.0 } });
            //final FlowLayout jPanelButtonsLayout = new FlowLayout();
            //jPanelButtonsLayout.setAlignment(FlowLayout.RIGHT);
            jPanelButtons.setLayout(jPanelButtonsLayout);
            this.add(new JSeparator(SwingConstants.HORIZONTAL), "1, 3");
            this.add(jPanelButtons, "1, 5");
    		{
    			jComboColor = new ColorComboBox();
    			if ( m_Parameter != null ) {
    				m_Color = new Color( 	m_Parameter.getColorR(),
    										m_Parameter.getColorG(),
    										m_Parameter.getColorB(),
    										m_Parameter.getColorAlpha());
    			}
    			jComboColor.getComboBox().setBackground(m_Color);
    			jPanelButtons.add(jComboColor.getComboBox(), "1, 0");
    			jComboColor.getComboBox().addActionListener(new ActionListener() {
                    public void actionPerformed(final ActionEvent evt) {
                    	m_Color = (Color) jComboColor.getComboBox().getSelectedItem();
                    	jComboColor.getComboBox().setBackground(m_Color);  	
                    }
                 });			
    		}            
            {
               jButtonCancel = new JButton();
               jButtonCancel.setText(Sextante.getText("Cancel"));
               jPanelButtons.add(jButtonCancel, "3, 0" );
               jButtonCancel.addActionListener(new ActionListener() {
                  public void actionPerformed(final ActionEvent evt) {
                     m_Parameter = null;
                     cancel();
                  }
               });
            }
            {
                jButtonOk = new JButton();                
                jButtonOk.setText(Sextante.getText("OK"));
                jPanelButtons.add(jButtonOk, "5, 0");
                jButtonOk.addActionListener(new ActionListener() {
                   public void actionPerformed(final ActionEvent evt) {
                      if (prepareParameter()) {
                         cancel();
                      }
                   }
                });
             }            
         }
         {
            jPanelMiddle = new JPanel();
            FlowLayout layout = (FlowLayout) jPanelMiddle.getLayout();
            layout.setVgap(1);
            this.add(jPanelMiddle, "1, 2");
         }
      }

   }


   protected void cancel() {

      dispose();
      setVisible(false);

   }


   /**
    * Returns the parameter created using this panel
    *
    * @return the parameter created using this panel
    */
   public Parameter getParameter() {

      return m_Parameter;

   }

   
   /**
    * Returns the color currently selected using this panel
    *
    * @return the selected color
    */
   public Color getColor() {

      return m_Color;

   }  

   
   /**
    * Sets the color currently selected in this panel
    *
    * @param
    * 			Color to set
    */
   public void setColor(Color c) {

      m_Color = c;

   }   

   
   public ColorComboBox getColorComboBox () {
	   
	   return (jComboColor);
   }
   
   
   @Override
   public String toString() {

      return getParameterDescription();

   }


   private String getDefaultName() {

      boolean bNameFound;
      String sName;
      int i = 1;
      final ParametersSet ps = m_ModelerPanel.getAlgorithm().getParameters();
      final int iCount = ps.getNumberOfParameters();
      Parameter param;

      do {
         bNameFound = true;
         sName = getParameterDescription() + " " + Integer.toString(i);
         for (int j = 0; j < iCount; j++) {
            param = ps.getParameter(j);
            if (param.getParameterDescription().equals(sName)) {
               bNameFound = false;
               break;
            }
         }
         i++;
      }
      while (!bNameFound);

      return sName;

   }


   /**
    * Fills the fields in the panel with default values
    */
   public void updateOptions() {

      jTextFieldDescription.setText(getDefaultName());

   };


   /**
    * Fills the fields in the panel with the characteristics of an already created parameter
    *
    * @param param
    *                a parameter
    */
   public void setParameter(final Parameter param) {

      jTextFieldDescription.setText(param.getParameterDescription());

   }


}
