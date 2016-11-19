

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

import es.unex.sextante.additionalInfo.AdditionalInfoVectorLayer;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.exceptions.NullParameterAdditionalInfoException;
import es.unex.sextante.gui.modeler.ModelerPanel;
import es.unex.sextante.parameters.Parameter;
import es.unex.sextante.parameters.ParameterVectorLayer;


public class VectorLayerPanel
extends
ParameterPanel {

	private JCheckBox jCheckBoxMandatory;
	private JLabel    jLabelType;
	private JComboBox jComboBoxType;


	public VectorLayerPanel(final JDialog parent,
			final ModelerPanel panel) {

		super(parent, panel);

	}


	public VectorLayerPanel(final ModelerPanel panel) {

		super(panel);

	}


	@Override
	protected void initGUI() {

		super.initGUI();

		super.setTitle(Sextante.getText("modeler_add_par_vector"));

		super.setPreferredSize(new java.awt.Dimension(400, 212));//180

		try {

			final TableLayout thisLayout = new TableLayout(new double[][] {
					{ TableLayoutConstants.MINIMUM, 5.0, TableLayoutConstants.FILL },
					{ 	TableLayoutConstants.MINIMUM, 
						1.0,
						TableLayoutConstants.MINIMUM } });
			thisLayout.setHGap(5);
			thisLayout.setVGap(5);
			
			jPanelMiddle.setLayout(thisLayout);

			jLabelType = new JLabel();
			jPanelMiddle.add(jLabelType, "0, 0");
			jLabelType.setText(Sextante.getText("vector_type"));
			final ComboBoxModel jComboBoxTypeModel = new DefaultComboBoxModel(new String[] {
					Sextante.getText("Any"),
					Sextante.getText("Points"),
					Sextante.getText("Lines"),
					Sextante.getText("Polygons") });
			jComboBoxType = new JComboBox();
			jPanelMiddle.add(jComboBoxType, "2, 0");
			jComboBoxType.setModel(jComboBoxTypeModel);
			
			jCheckBoxMandatory = new JCheckBox();
			jCheckBoxMandatory.setSelected(true);
			jCheckBoxMandatory.setText(Sextante.getText("Mandatory"));			
			jPanelMiddle.add(jCheckBoxMandatory, "0, 2");			
		}
		catch (final Exception e) {
			Sextante.addErrorToLog(e);
		}

	}


	@Override
	protected boolean prepareParameter() {

		int iType = AdditionalInfoVectorLayer.SHAPE_TYPE_ANY;
		final String sDescription = jTextFieldDescription.getText();

		if (sDescription.length() != 0) {
			switch (jComboBoxType.getSelectedIndex()) {
			case 0:
				iType = AdditionalInfoVectorLayer.SHAPE_TYPE_ANY;
				break;
			case 1:
				iType = AdditionalInfoVectorLayer.SHAPE_TYPE_POINT;
				break;
			case 2:
				iType = AdditionalInfoVectorLayer.SHAPE_TYPE_LINE;
				break;
			case 3:
				iType = AdditionalInfoVectorLayer.SHAPE_TYPE_POLYGON;
				break;
			}

			final AdditionalInfoVectorLayer addInfo = new AdditionalInfoVectorLayer(iType, jCheckBoxMandatory.isSelected());
			m_Parameter = new ParameterVectorLayer();
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
			final AdditionalInfoVectorLayer ai = (AdditionalInfoVectorLayer) param.getParameterAdditionalInfo();
			jCheckBoxMandatory.setSelected(ai.getIsMandatory());
			jComboBoxType.setSelectedIndex(0);
			switch (ai.getShapeType()) {
			case AdditionalInfoVectorLayer.SHAPE_TYPE_ANY:
				jComboBoxType.setSelectedIndex(0);
				break;
			case AdditionalInfoVectorLayer.SHAPE_TYPE_POINT:
				jComboBoxType.setSelectedIndex(1);
				break;
			case AdditionalInfoVectorLayer.SHAPE_TYPE_LINE:
				jComboBoxType.setSelectedIndex(2);
				break;
			case AdditionalInfoVectorLayer.SHAPE_TYPE_POLYGON:
				jComboBoxType.setSelectedIndex(3);
				break;
			}
		}
		catch (final NullParameterAdditionalInfoException e) {
			e.printStackTrace();
		}
		
	}


	@Override
	public String getParameterDescription() {

		return Sextante.getText("Vector_layer");

	}


	@Override
	public boolean parameterCanBeAdded() {

		return true;

	}

}
