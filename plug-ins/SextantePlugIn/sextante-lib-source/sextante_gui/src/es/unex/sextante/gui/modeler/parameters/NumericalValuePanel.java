package es.unex.sextante.gui.modeler.parameters;

import info.clearthought.layout.TableLayout;
import info.clearthought.layout.TableLayoutConstants;

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

import es.unex.sextante.additionalInfo.AdditionalInfoNumericalValue;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.exceptions.NullParameterAdditionalInfoException;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.modeler.ModelerPanel;
import es.unex.sextante.gui.r.RAlgorithmProvider;
import es.unex.sextante.gui.settings.SextanteRSettings;
import es.unex.sextante.parameters.Parameter;
import es.unex.sextante.parameters.ParameterNumericalValue;

public class NumericalValuePanel
extends
ParameterPanel {

	private static int	typeFloat = 0;
	private static int	typeInteger = 1;
	
	private JCheckBox  jCheckBoxMin;
	private JCheckBox  jCheckBoxMax;
	private JLabel     jLabelType;
	private JTextField jTextFieldMin;
	private JTextField jTextFieldMax;
	private JTextField jTextFieldDefault;
	private JComboBox  jComboBoxType;
	private JLabel     jLabelDefault;


	public NumericalValuePanel(final JDialog parent,
			final ModelerPanel panel) {

		super(parent, panel);

	}


	public NumericalValuePanel(final ModelerPanel panel) {

		super(panel);

	}


	@Override
	protected void initGUI() {

		super.initGUI();

		super.setTitle(Sextante.getText("modeler_add_par_numerical"));

		super.setPreferredSize(new java.awt.Dimension(400, 284));

		try {
			{
				final TableLayout thisLayout = new TableLayout(new double[][] {
						{ 	TableLayoutConstants.MINIMUM, 
							5.0,
							TableLayoutConstants.FILL },
							{ 		TableLayoutConstants.MINIMUM,
								1.0,
								TableLayoutConstants.MINIMUM,
								1.0,
								TableLayoutConstants.MINIMUM,
								1.0,
								TableLayoutConstants.MINIMUM } });
				thisLayout.setHGap(5);
				thisLayout.setVGap(5);
				jPanelMiddle.setLayout(thisLayout);
				{
					jCheckBoxMin = new JCheckBox();					
					jCheckBoxMin.setText(Sextante.getText("Min_value"));
					jPanelMiddle.add(jCheckBoxMin, "0, 0");
					jCheckBoxMin.addActionListener(new ActionListener() {
						public void actionPerformed(final ActionEvent arg0) {
							if  (jCheckBoxMin.isSelected() == true) {
								jTextFieldMin.setEnabled(true);
								jTextFieldMin.setEditable(true);
							} else {
								jTextFieldMin.setEnabled(false);
								jTextFieldMin.setEditable(false);
							}
						}
					});
				}
				{
					jCheckBoxMax = new JCheckBox();					
					jCheckBoxMax.setText(Sextante.getText("Max_value"));
					jPanelMiddle.add(jCheckBoxMax, "0, 2");
					jCheckBoxMax.addActionListener(new ActionListener() {
						public void actionPerformed(final ActionEvent arg0) {
							if  (jCheckBoxMax.isSelected() == true) {
								jTextFieldMax.setEnabled(true);
								jTextFieldMax.setEditable(true);
							} else {
								jTextFieldMax.setEnabled(false);
								jTextFieldMax.setEditable(false);
							}
						}
					});               
				}
				{
					jTextFieldMin = new JTextField();
					if ( jCheckBoxMin.isSelected() == false ) {
						jTextFieldMin.setEnabled(false);
						jTextFieldMin.setEditable(false);
					} else {
						jTextFieldMin.setEnabled(true);
						jTextFieldMin.setEditable(true);
					}
					jPanelMiddle.add(jTextFieldMin, "2, 0");
					jTextFieldMin.addFocusListener(new FocusAdapter() {
						@Override
						public void focusLost(final FocusEvent e) {
							checkTextFieldContent((JTextField) e.getSource());
						}
					});
				}
				{
					jTextFieldMax = new JTextField();
					if ( jCheckBoxMax.isSelected() == false ) {
						jTextFieldMax.setEnabled(false);
						jTextFieldMax.setEditable(false);
					} else {
						jTextFieldMax.setEnabled(true);
						jTextFieldMax.setEditable(true);
					}
					jPanelMiddle.add(jTextFieldMax, "2, 2");
					jTextFieldMax.addFocusListener(new FocusAdapter() {
						@Override
						public void focusLost(final FocusEvent e) {
							checkTextFieldContent((JTextField) e.getSource());
						}
					});
				}
				{
					jLabelDefault = new JLabel();
					jPanelMiddle.add(jLabelDefault, "0, 4");
					jLabelDefault.setText(Sextante.getText("Default_value"));

					jTextFieldDefault = new JTextField();
					jPanelMiddle.add(jTextFieldDefault, "2, 4");
					jTextFieldDefault.addFocusListener(new FocusAdapter() {
						@Override
						public void focusLost(final FocusEvent e) {
							checkTextFieldContent((JTextField) e.getSource());
						}
					});
				}
				{
					jLabelType = new JLabel();
					jPanelMiddle.add(jLabelType, "0, 6");
					jLabelType.setText(Sextante.getText("Value_type"));
				}
				{
					final ComboBoxModel jComboBoxTypeModel = 
						new DefaultComboBoxModel(new String[] { Sextante.getText("Float"),
							Sextante.getText("Integer") });
					jComboBoxType = new JComboBox();
					jPanelMiddle.add(jComboBoxType, "2, 6");
					jComboBoxType.setModel(jComboBoxTypeModel);
				}
			}
		}
		catch (final Exception e) {
			Sextante.addErrorToLog(e);
		}

	}

	
	/* Parses a text field containing a number and returns
	   the result as a truncated double value */
	private double truncNumeric ( String number ) throws Exception {
		Integer Value;
		try {
			Value = Integer.parseInt(number);
		} catch (final Exception e) {
			throw e;
		}
		return ( (double) Value.intValue() );		
	}
	

	@Override
	protected boolean prepareParameter() {

		double dMax, dMin;
		double dAbsoluteMax, dAbsoluteMin;
		double dDefault;
		int iType;

		try {
			if (jComboBoxType.getSelectedIndex() == typeInteger ) {
				iType = AdditionalInfoNumericalValue.NUMERICAL_VALUE_INTEGER;
				dAbsoluteMin = Integer.MIN_VALUE;
				dAbsoluteMax = Integer.MAX_VALUE;
			}
			else {
				iType = AdditionalInfoNumericalValue.NUMERICAL_VALUE_DOUBLE;
				dAbsoluteMin = Double.NEGATIVE_INFINITY;
				dAbsoluteMax = Double.MAX_VALUE;
			}
			if ( iType == AdditionalInfoNumericalValue.NUMERICAL_VALUE_INTEGER ) {
				/* truncate to integer before saving */
				dDefault = truncNumeric (jTextFieldDefault.getText());
			} else {
				dDefault = Double.parseDouble(jTextFieldDefault.getText());
			}			
			if (jCheckBoxMin.isSelected()) {
				if ( iType == AdditionalInfoNumericalValue.NUMERICAL_VALUE_INTEGER ) {
					/* truncate to integer before saving */
					dMin = truncNumeric (jTextFieldMin.getText());
				} else {				
					dMin = Double.parseDouble(jTextFieldMin.getText());
				}
			}
			else {
				dMin = dAbsoluteMin;
			}
			if (jCheckBoxMax.isSelected()) {
				if ( iType == AdditionalInfoNumericalValue.NUMERICAL_VALUE_INTEGER ) {
					/* truncate to integer before saving */
					dMax = truncNumeric (jTextFieldMax.getText());
				} else {								
					dMax = Double.parseDouble(jTextFieldMax.getText());
				}
			}
			else {
				dMax = dAbsoluteMax;
			}
			if ( dDefault < dMin || dDefault > dMax || dMin > dMax ) {
				JOptionPane.showMessageDialog(null, Sextante.getText("Invalid_parameters"), Sextante.getText("Warning"),
						JOptionPane.WARNING_MESSAGE);
				return false;
			}
		}
		catch (final Exception e) {
			JOptionPane.showMessageDialog(null, Sextante.getText("Invalid_parameters"), Sextante.getText("Warning"),
					JOptionPane.WARNING_MESSAGE);
			return false;
		}

		final String sDescription = jTextFieldDescription.getText();

		if (sDescription.length() != 0) {
			final AdditionalInfoNumericalValue addInfo = new AdditionalInfoNumericalValue(iType, dDefault, Math.min(dMin, dMax), Math.max(
					dMin, dMax));
			m_Parameter = new ParameterNumericalValue();
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

		boolean isInt = false;
		
		try {
			final AdditionalInfoNumericalValue ai = (AdditionalInfoNumericalValue) param.getParameterAdditionalInfo();
			if (ai.getType() == AdditionalInfoNumericalValue.NUMERICAL_VALUE_INTEGER) {
				jComboBoxType.setSelectedIndex(typeInteger);
				isInt = true;
			}
			else {
				jComboBoxType.setSelectedIndex(typeFloat);
			}			
			if (ai.getMaxValue() != Double.MAX_VALUE && ai.getMaxValue() != Integer.MAX_VALUE) {
				jCheckBoxMax.setSelected(true);
				jTextFieldMax.setEnabled(true);
				jTextFieldMax.setEditable(true);
				if ( isInt == false ) {
					jTextFieldMax.setText(Double.toString(ai.getMaxValue()));
				} else {
					jTextFieldMax.setText(Integer.toString((int)ai.getMaxValue()));
				}
			}
			if (ai.getMinValue() != Double.NEGATIVE_INFINITY && ai.getMinValue() != Integer.MIN_VALUE) {
				jCheckBoxMin.setSelected(true);
				jTextFieldMin.setEnabled(true);
				jTextFieldMin.setEditable(true);
				if ( isInt == false ) {
					jTextFieldMin.setText(Double.toString(ai.getMinValue()));
				} else {
					jTextFieldMin.setText(Integer.toString((int)ai.getMinValue()));
				}
			}
			if ( isInt == false ) {
				jTextFieldDefault.setText(Double.toString(ai.getDefaultValue()));
			} else {
				jTextFieldDefault.setText(Integer.toString((int)ai.getDefaultValue()));
			}
		}
		catch (final NullParameterAdditionalInfoException e) {
			e.printStackTrace();
		}

	}


	@Override
	public String getParameterDescription() {

		return Sextante.getText("Numerical_value");

	}


	private void checkTextFieldContent(final JTextField textField) {

		final String content = textField.getText();
		if (content.length() != 0) {
			try {
				Double.parseDouble(content);
				return;
			}
			catch (final NumberFormatException nfe) {
				getToolkit().beep();
				textField.requestFocus();
			}
		}


	}


	@Override
	public boolean parameterCanBeAdded() {

		return true;

	}

}
