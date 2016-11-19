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

import es.unex.sextante.additionalInfo.AdditionalInfoMultipleInput;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.exceptions.NullParameterAdditionalInfoException;
import es.unex.sextante.gui.modeler.ModelerPanel;
import es.unex.sextante.parameters.Parameter;
import es.unex.sextante.parameters.ParameterMultipleInput;

public class MultipleInputPanel
extends
ParameterPanel {

	private JLabel    jLabelType;
	private JComboBox jComboBoxType;
	private JCheckBox jCheckBoxMandatory;


	public MultipleInputPanel(final JDialog parent,
			final ModelerPanel panel) {

		super(parent, panel);

	}


	public MultipleInputPanel(final ModelerPanel panel) {

		super(panel);

	}


	@Override
	protected void initGUI() {

		super.initGUI();

		super.setTitle(Sextante.getText("modeler_add_par_multiple"));

		super.setPreferredSize(new java.awt.Dimension(400, 212));

		try {
			{
				final TableLayout thisLayout = new TableLayout
					(new double[][] { { TableLayoutConstants.MINIMUM, 5.0, TableLayoutConstants.FILL },
						{  	TableLayoutConstants.MINIMUM,
							1.0,
							TableLayoutConstants.MINIMUM,
							1.0 } });
				thisLayout.setHGap(5);
				thisLayout.setVGap(5);
				jPanelMiddle.setLayout(thisLayout);
				{
					jLabelType = new JLabel();
					jPanelMiddle.add(jLabelType, "0, 0");
					jLabelType.setText(Sextante.getText("Input_type"));
				}
				{
					final ComboBoxModel jComboBoxTypeModel = new DefaultComboBoxModel(new String[] { 							
							Sextante.getText("Vector_any_type"),							
							Sextante.getText("Vectorial__points"),
							Sextante.getText("Vectorial__lines"),
							Sextante.getText("Vectorial__polygons"),
							Sextante.getText("Raster"),
							Sextante.getText("Raster_band"),
							Sextante.getText("Table") });
					jComboBoxType = new JComboBox();
					jPanelMiddle.add(jComboBoxType, "2, 0");
					jComboBoxType.setModel(jComboBoxTypeModel);
				}
				{
					jCheckBoxMandatory = new JCheckBox();
					jCheckBoxMandatory.setSelected(true);
					jPanelMiddle.add(jCheckBoxMandatory, "0,2,2,2");
					jCheckBoxMandatory.setText(Sextante.getText("Mandatory"));
				}
			}
		}
		catch (final Exception e) {
			Sextante.addErrorToLog(e);
		}

	}


	@Override
	protected boolean prepareParameter() {

		int iType = AdditionalInfoMultipleInput.DATA_TYPE_VECTOR_ANY;

		switch (jComboBoxType.getSelectedIndex()) {
		case 0:
			iType = AdditionalInfoMultipleInput.DATA_TYPE_VECTOR_ANY;
			break;
		case 1:
			iType = AdditionalInfoMultipleInput.DATA_TYPE_VECTOR_POINT;
			break;
		case 2:
			iType = AdditionalInfoMultipleInput.DATA_TYPE_VECTOR_LINE;
			break;			
		case 3:
			iType = AdditionalInfoMultipleInput.DATA_TYPE_VECTOR_POLYGON;
			break;
		case 4:
			iType = AdditionalInfoMultipleInput.DATA_TYPE_RASTER;
			break;
		case 5:
			iType = AdditionalInfoMultipleInput.DATA_TYPE_BAND;
			break;
		case 6:
			iType = AdditionalInfoMultipleInput.DATA_TYPE_RASTER_3D;
			break;			
		case 7:
			iType = AdditionalInfoMultipleInput.DATA_TYPE_TABLE;
			break;
		}

		final String sDescription = jTextFieldDescription.getText();
		final boolean bMandatory = jCheckBoxMandatory.isSelected();

		if (sDescription.length() != 0) {
			final AdditionalInfoMultipleInput addInfo = new AdditionalInfoMultipleInput(iType, bMandatory);
			m_Parameter = new ParameterMultipleInput();
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
			final AdditionalInfoMultipleInput ai = (AdditionalInfoMultipleInput) param.getParameterAdditionalInfo();
			jCheckBoxMandatory.setSelected(ai.getIsMandatory());
			jComboBoxType.setSelectedIndex(0);
			switch (ai.getDataType()) {
			case AdditionalInfoMultipleInput.DATA_TYPE_VECTOR_ANY:
				jComboBoxType.setSelectedIndex(0);
				break;
			case AdditionalInfoMultipleInput.DATA_TYPE_VECTOR_POINT:
				jComboBoxType.setSelectedIndex(1);
				break;
			case AdditionalInfoMultipleInput.DATA_TYPE_VECTOR_LINE:
				jComboBoxType.setSelectedIndex(2);
				break;
			case AdditionalInfoMultipleInput.DATA_TYPE_VECTOR_POLYGON:
				jComboBoxType.setSelectedIndex(3);
				break;				
			case AdditionalInfoMultipleInput.DATA_TYPE_RASTER:
				jComboBoxType.setSelectedIndex(4);
				break;
			case AdditionalInfoMultipleInput.DATA_TYPE_BAND:
				jComboBoxType.setSelectedIndex(5);
				break;
			case AdditionalInfoMultipleInput.DATA_TYPE_RASTER_3D:
				jComboBoxType.setSelectedIndex(6);
				break;				
			case AdditionalInfoMultipleInput.DATA_TYPE_TABLE:
				jComboBoxType.setSelectedIndex(7);
				break;
			}

		}
		catch (final NullParameterAdditionalInfoException e) {
			e.printStackTrace();
		}

	}


	@Override
	public String getParameterDescription() {

		return Sextante.getText("Multiple_input");

	}


	@Override
	public boolean parameterCanBeAdded() {

		return true;

	}

}
