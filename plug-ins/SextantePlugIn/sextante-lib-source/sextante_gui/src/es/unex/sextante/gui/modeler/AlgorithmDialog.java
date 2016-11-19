package es.unex.sextante.gui.modeler;

import info.clearthought.layout.TableLayout;
import info.clearthought.layout.TableLayoutConstants;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import es.unex.sextante.core.GeoAlgorithm;
import es.unex.sextante.core.ObjectAndDescription;
import es.unex.sextante.core.OutputObjectsSet;
import es.unex.sextante.core.ParametersSet;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.IGUIFactory;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.modeler.ColorComboBox;
import es.unex.sextante.modeler.elements.IModelElement;
import es.unex.sextante.modeler.elements.ModelElement3DRasterLayer;
import es.unex.sextante.modeler.elements.ModelElementNumericalValue;
import es.unex.sextante.modeler.elements.ModelElementRasterLayer;
import es.unex.sextante.modeler.elements.ModelElementTable;
import es.unex.sextante.modeler.elements.ModelElementVectorLayer;
import es.unex.sextante.outputs.Output;
import es.unex.sextante.outputs.Output3DRasterLayer;
import es.unex.sextante.outputs.OutputNumericalValue;
import es.unex.sextante.outputs.OutputRasterLayer;
import es.unex.sextante.outputs.OutputTable;
import es.unex.sextante.outputs.OutputVectorLayer;
import es.unex.sextante.parameters.Parameter;

public class AlgorithmDialog
extends
JDialog {

	private final ModelAlgorithm                 m_ModelAlgorithm;
	private final GeoAlgorithm                   m_Algorithm;
	private final String                         m_sAlgorithmName;
	private final String                         m_sAlgorithmDescription;
	private final HashMap                        m_DataObjects;
	
	private JTextField							 jTextFieldDescription;
	private JLabel								 jLabelDescription;
	private JPanel								 jPanelName;
	private JPanel                               jPanelButtons;
	private ColorComboBox                        jComboColor;
	private JButton                              jButtonInfo;	
	private JButton                              jButtonCancel;
	private JButton                              jButtonOK;

	protected GeoAlgorithmModelerParametersPanel jPanelParametersMain = null;
	private int                                  m_iDialogReturn;


	public AlgorithmDialog(final GeoAlgorithm algorithm,
			final String sName,
			final String sDescription,
			final ModelAlgorithm modelAlgorithm,
			final GeoAlgorithmModelerParametersPanel panel,
			final HashMap dataObjects,
			final JDialog parent) {

		super(parent, sDescription, true);
		//setLocationRelativeTo(null);

		m_Algorithm = algorithm;
		m_ModelAlgorithm = modelAlgorithm;
		m_DataObjects = dataObjects;
		m_sAlgorithmName = sName;
		m_sAlgorithmDescription = sDescription;

		jPanelParametersMain = panel;
		jPanelParametersMain.init(this);

		initGUI();
		setLocationRelativeTo(null);

	}


	public AlgorithmDialog(final GeoAlgorithm algorithm,
			final String sName,
			final String sDescription,
			final ModelAlgorithm modelAlgorithm,
			final GeoAlgorithmModelerParametersPanel panel,
			final HashMap dataObjects) {

		super(SextanteGUI.getMainFrame(), "", true);
		setLocationRelativeTo(null);

		m_Algorithm = algorithm;
		m_ModelAlgorithm = modelAlgorithm;
		m_DataObjects = dataObjects;
		m_sAlgorithmName = sName;
		m_sAlgorithmDescription = sDescription;

		jPanelParametersMain = panel;
		jPanelParametersMain.init(this);

		initGUI();
		setLocationRelativeTo(null);

	}


	private void initGUI() {

		this.setTitle(Sextante.getText("modeler_add_algorithm") + " (" + m_Algorithm.getName()  + ")" );
		this.setPreferredSize(new java.awt.Dimension(700, 450));//422
		this.setResizable(false);
		
		final TableLayout thisLayout = new TableLayout(new double[][] {
				{ TableLayoutConstants.FILL },
				{ 3.0, TableLayoutConstants.MINIMUM, 1.0, TableLayoutConstants.FILL, 1.0, 40.0, 4.0 } });
		thisLayout.setHGap(5);
		thisLayout.setVGap(5);		
		this.getContentPane().setLayout(thisLayout);
		this.getContentPane().add(getJPanelParameters(), "0, 3"); // algorithm parameters
		
		/* description */
		jPanelName = new JPanel();
        final TableLayout jPanelNameLayout = new TableLayout
        	(new double[][] { { 5.0, TableLayoutConstants.MINIMUM, 5.0, TableLayoutConstants.FILL, 5.0 },
                 { 20.0, 3.0, TableLayoutConstants.MINIMUM } });
        jPanelNameLayout.setHGap(5);
        jPanelNameLayout.setVGap(5);
        jPanelName.setLayout(jPanelNameLayout);		
		{
			jLabelDescription = new JLabel();
			jLabelDescription.setText(Sextante.getText("Description"));
			jPanelName.add(jLabelDescription, "1, 0");
		}            
		{
			jTextFieldDescription = new JTextField();
			jTextFieldDescription.setText(getDefaultName());
			jPanelName.add(jTextFieldDescription, "3, 0");
		}
		jPanelName.add(new JSeparator(SwingConstants.HORIZONTAL), "1, 2, 3, 2");
		this.getContentPane().add(jPanelName, "0, 1"); // editable description
		
		/* row of buttons on panel bottom area */
		final TableLayout panelLayout = new TableLayout(new double[][] {
				{ 	5.0,
					TableLayoutConstants.MINIMUM, // col 1: Help
					5.0,
					90.0, // 3: Colour
					TableLayoutConstants.FILL,
					90.0, // 5: Cancel
					5.0,
					90.0, // 7: OK
					5.0 },
				{ 1.0, 30.0, 5.0 } });
		panelLayout.setHGap(5);
		panelLayout.setVGap(5);
				
		jPanelButtons = new JPanel();
		jPanelButtons.setLayout(panelLayout);
		{
			jButtonInfo = new JButton();
			jPanelButtons.add(jButtonInfo, "1, 1");
            jButtonInfo.setIcon(new ImageIcon(getClass().getClassLoader().getResource("images/info.gif")));
            jButtonInfo.addActionListener(new ActionListener() {
               public void actionPerformed(final ActionEvent evt) {
            	   SextanteGUI.getGUIFactory().showHelpDialog(m_Algorithm);
               }
            });
		}
		{
			jComboColor = new ColorComboBox();
			final Color curColor = new Color( 	m_Algorithm.getColorR(),
												m_Algorithm.getColorG(),
												m_Algorithm.getColorB(),
												m_Algorithm.getColorAlpha());
			jComboColor.getComboBox().setBackground(curColor);
			jPanelButtons.add(jComboColor.getComboBox(), "3, 1");
			jComboColor.getComboBox().addActionListener(new ActionListener() {
                public void actionPerformed(final ActionEvent evt) {
                	final Color curColor = (Color) jComboColor.getComboBox().getSelectedItem();
                	jComboColor.getComboBox().setBackground(curColor);
                	m_Algorithm.setColorR(curColor.getRed());
                	m_Algorithm.setColorG(curColor.getGreen());
                	m_Algorithm.setColorB(curColor.getBlue());
                	m_Algorithm.setColorAlpha(curColor.getAlpha());                	
                }
             });			
		}		
		{
			jButtonCancel = new JButton();
			jPanelButtons.add(jButtonCancel, "5, 1");
			jButtonCancel.setText(Sextante.getText("Cancel"));
			jButtonCancel.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(final java.awt.event.ActionEvent e) {
					m_iDialogReturn = IGUIFactory.CANCEL;
					cancel();
				}
			});
		}		
		{
			jButtonOK = new JButton();
			jPanelButtons.add(jButtonOK, "7, 1");
			jButtonOK.setText(Sextante.getText("OK"));
			jButtonOK.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(final java.awt.event.ActionEvent e) {
					if (addAlgorithm()) {
						m_iDialogReturn = IGUIFactory.OK;
						cancel();
					}
				}
			});
		}
		this.getContentPane().add(jPanelButtons,"0, 5");				
	}


	private String getDefaultName() {

		if ( m_Algorithm.getDescription() != null && m_Algorithm.getDescription().length() > 1 ) {
			return (m_Algorithm.getDescription());
		}
	
		return m_Algorithm.getName();

	}	
	
	
	protected boolean addAlgorithm() {

		String sKey;
		String sAssignment;
		String sDescription;
		final HashMap map = new HashMap();
		OutputObjectsSet ooSet;
		if (assignParameters(map)) {
			m_ModelAlgorithm.addAlgorithm(m_Algorithm, m_sAlgorithmName);
			if ( jTextFieldDescription.getText() != null && jTextFieldDescription.getText().length() > 0 ) {
				sDescription = jTextFieldDescription.getText();
				m_Algorithm.setDescription(jTextFieldDescription.getText());
			} else {
				m_Algorithm.setDescription(m_Algorithm.getName());
			}
			final Set set = map.keySet();
			final Iterator iter = set.iterator();
			while (iter.hasNext()) {
				sKey = (String) iter.next();
				sAssignment = (String) map.get(sKey);
				//System.out.println ("*** ADD ASSIGNMENT: KEY=" + sKey +" ASSIGN=" +sAssignment + ".\n");
				m_ModelAlgorithm.addInputAsignment(sKey, sAssignment, m_sAlgorithmName);
			}
			ooSet = m_Algorithm.getOutputObjects();
			for (int i = 0; i < ooSet.getOutputObjectsCount(); i++) {
				final Output out = ooSet.getOutput(i);
				if ((out instanceof OutputRasterLayer) || (out instanceof OutputVectorLayer) || (out instanceof OutputTable)
						|| (out instanceof Output3DRasterLayer) || (out instanceof OutputNumericalValue)) {
					sKey = out.getName();
					sDescription = out.getDescription();
					sKey += m_sAlgorithmName;
					sDescription = "\"" + sDescription + "\" " + Sextante.getText("from") + " " + m_sAlgorithmDescription;
					m_DataObjects.put(sKey, new ObjectAndDescription(sDescription, getOutputAsModelElement(out)));
				}

			}
			
			return true;
		}
		else {
			JOptionPane.showMessageDialog(null, Sextante.getText("Invalid_parameters"), Sextante.getText("Warning"),
					JOptionPane.WARNING_MESSAGE);
			return false;
		}

	}


	private IModelElement getOutputAsModelElement(final Output out) {

		IModelElement element = null;

		if (out instanceof OutputRasterLayer) {
			element = new ModelElementRasterLayer();
			final int iBands = ((OutputRasterLayer) out).getNumberOfBands();
			((ModelElementRasterLayer) element).setNumberOfBands(iBands);
		}
		else if (out instanceof OutputVectorLayer) {
			element = new ModelElementVectorLayer();
			final int iShapeType = ((OutputVectorLayer) out).getShapeType();
			((ModelElementVectorLayer) element).setShapeType(iShapeType);
		}
		else if (out instanceof OutputTable) {
			element = new ModelElementTable();
		}
		else if (out instanceof Output3DRasterLayer) {
			element = new ModelElement3DRasterLayer();
		}
		else if (out instanceof OutputNumericalValue) {
			element = new ModelElementNumericalValue();
		}

		return element;

	}


	protected boolean assignParameters(final HashMap map) {

		return getJPanelParameters().assignParameters(map);

	}


	public void cancel() {

		dispose();
		setVisible(false);

	}


	private GeoAlgorithmModelerParametersPanel getJPanelParameters() {

		return jPanelParametersMain;

	}


	public int getDialogReturn() {

		return m_iDialogReturn;

	}


	public GeoAlgorithm getAlgorithm() {

		return m_Algorithm;

	}


	public ModelAlgorithm getModelAlgorithm() {

		return m_ModelAlgorithm;

	}


	public HashMap getDataObjects() {

		return m_DataObjects;

	}


	public String getAlgorithmName() {

		return m_sAlgorithmName;

	}


	public String getAlgorithmDescription() {

		return m_sAlgorithmDescription;

	}


}
