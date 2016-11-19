package es.unex.sextante.gui.modeler.parameters;

import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.modeler.ModelerPanel;
import es.unex.sextante.parameters.ParameterPoint;

public class PointPanel
extends
ParameterPanel {

	private JLabel jLabel;	

	public PointPanel(final JDialog parent,
			final ModelerPanel panel) {

		super(parent, panel);

	}


	public PointPanel(final ModelerPanel panel) {

		super(panel);

	}

	@Override
	protected void initGUI() {

		super.initGUI();

		super.setTitle(Sextante.getText("modeler_add_par_point"));

		super.setPreferredSize(new java.awt.Dimension(400, 182));

		try {
			jLabel = new JLabel();
			jPanelMiddle.add(jLabel);
			jLabel.setText(Sextante.getText("nothing_to_set"));
		}
		catch (final Exception e) {
			Sextante.addErrorToLog(e);
		}

	}   


	@Override
	public String getParameterDescription() {

		return Sextante.getText("Coordinate");

	}


	@Override
	protected boolean prepareParameter() {


		final String sDescription = jTextFieldDescription.getText();

		if (sDescription.length() != 0) {
			m_Parameter = new ParameterPoint();
			m_Parameter.setParameterDescription(sDescription);
			
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
	public boolean parameterCanBeAdded() {

		return true;

	}

}
