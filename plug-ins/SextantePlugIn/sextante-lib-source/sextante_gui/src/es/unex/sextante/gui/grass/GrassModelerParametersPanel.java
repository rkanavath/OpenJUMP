package es.unex.sextante.gui.grass;

import info.clearthought.layout.TableLayoutConstants;

import java.util.HashMap;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JTextField;

import es.unex.sextante.additionalInfo.AdditionalInfoNumericalValue;
import es.unex.sextante.core.ObjectAndDescription;
import es.unex.sextante.core.OutputObjectsSet;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.algorithm.ParameterContainer;
import es.unex.sextante.gui.modeler.DefaultModelerParametersPanel;
import es.unex.sextante.gui.modeler.OutputLayerSettingsPanel;
import es.unex.sextante.gui.modeler.OutputParameterContainer;
import es.unex.sextante.modeler.elements.ModelElementNumericalValue;
import es.unex.sextante.modeler.elements.ModelElementPoint;
import es.unex.sextante.modeler.elements.ModelElementString;
import es.unex.sextante.outputs.Output;
import es.unex.sextante.parameters.Parameter;
import es.unex.sextante.parameters.ParameterString;

public class GrassModelerParametersPanel
extends
DefaultModelerParametersPanel {

	private JComboBox jComboRestrictGeometryType;


	@Override
	protected void addOutputObjects(final JPanel pane) {

		super.addOutputObjects(pane);		

		final String[] options = new String[] { Sextante.getText("Points"), Sextante.getText("Lines"), Sextante.getText("Polygons") };
		jComboRestrictGeometryType = new JComboBox(options);
		//addTitleLabel(pane, Sextante.getText("grass_restrict_geometry_ouput"), m_iCurrentRow, false);
		//pane.add(jComboRestrictGeometryType, getStringTableCoords(2, m_iCurrentRow));
		m_iCurrentRow++;

	}


	private boolean makeNumericalValueAssignment(final HashMap map,
			final ParameterContainer parameterContainer) {

		double dValue;
		ObjectAndDescription oad;
		Parameter parameter = null;
		String sKey = null, sInnerKey;
		Double value;
		String sAssignment;

		final JComboBox comboBox = (JComboBox) parameterContainer.getContainer();
		final JTextField textField = (JTextField) comboBox.getEditor().getEditorComponent();
		final String sValue = textField.getText();

		try {
			parameter = m_Algorithm.getParameters().getParameter(parameterContainer.getName());
			final AdditionalInfoNumericalValue ai = (AdditionalInfoNumericalValue) parameter.getParameterAdditionalInfo();
			final double dMin = ai.getMinValue();
			final double dMax = ai.getMaxValue();
			sKey = parameter.getParameterName();
			final Object ob = comboBox.getSelectedItem();
			if (ob instanceof ObjectAndDescription) {
				oad = (ObjectAndDescription) ob;
				sAssignment = (String) oad.getObject();
				map.put(sKey, sAssignment);
				return true;
			}
			else {
				try {
					dValue = Double.parseDouble(sValue);
					if ((dValue < dMin) || (dValue > dMax)) {
						return false;
					}
					sInnerKey = getInnerParameterKey();
					map.put(sKey, sInnerKey);
					value = new Double(dValue);
					m_DataObjects.put(sInnerKey, new ObjectAndDescription("Numerical Value", value));
					return true;
				}
				catch (final NumberFormatException e) {
					return false;
				}
			}
		}
		catch (final Exception e) {
			Sextante.addErrorToLog(e);
			return false;
		}

	}   
	
	
	@Override
	public boolean assignParameters(final HashMap map) {	   


		boolean bAssigningOK = true;
		int i;
		ParameterContainer parameterContainer;
		
		/* Many GRASS modules have non-mandatory Numerical and String options.
		 * Therefore, we cannot use a strict settings check for GRASS algorithms. */
		for (i = 0; i < m_InputParameterContainer.size(); i++) {
			parameterContainer = (ParameterContainer) m_InputParameterContainer.get(i);
			if (parameterContainer.getType().equals("Table") || parameterContainer.getType().equals("Vector Layer")
					|| parameterContainer.getType().equals("Raster Layer") || parameterContainer.getType().equals("Image Layer") ) {
				makeDataObjectAssignment(map, parameterContainer);
			}
			else if (parameterContainer.getType().equals("Multiple Input")) {
				makeMultipleInputAssignment(map, parameterContainer);
			}
			else if (parameterContainer.getType().equals("Numerical Value")) {
				makeNumericalValueAssignment(map, parameterContainer);
				bAssigningOK = true;
			}
			else if (parameterContainer.getType().equals("String")) {
				makeStringAssignment(map, parameterContainer);
				bAssigningOK = true;
			}			
			else if (parameterContainer.getType().equals("Boolean")) {
				makeBooleanAssignment(map, parameterContainer);
				bAssigningOK = true;
			}
			else if (parameterContainer.getType().equals("Fixed Table")) {
				bAssigningOK = makeFixedTableAssignment(map, parameterContainer);
			}
			else if (parameterContainer.getType().equals("Filepath")) {
				bAssigningOK = makeFilepathAssignment(map, parameterContainer);
			}
			else if (parameterContainer.getType().equals("Band")) {
				bAssigningOK = makeRasterBandAssignment(map, parameterContainer);
			}
			else if (parameterContainer.getType().equals("Table Field")) {
				makeTableFieldAssignment(map, parameterContainer);
				bAssigningOK = true;
			}
			else if (parameterContainer.getType().equals("Selection")) {
				bAssigningOK = makeSelectionAssignment(map, parameterContainer);
			}

			if (!bAssigningOK) {
				return false;
			}
		}

		if ( jComboRestrictGeometryType != null ) {
			m_GlobalAlgorithm.setGeometryTypeRestriction(jComboRestrictGeometryType.getSelectedIndex());
		}

		/* set output objects */
		final OutputObjectsSet oosetGlobal = this.m_GlobalAlgorithm.getOutputObjects();
		final OutputObjectsSet ooset = this.m_Algorithm.getOutputObjects();

		for (i = 0; i < m_OutputParameterDefinitionContainer.size(); i++) {
			final OutputParameterContainer opc = (OutputParameterContainer) m_OutputParameterDefinitionContainer.get(i);
			final OutputLayerSettingsPanel olsp = (OutputLayerSettingsPanel) opc.getContainer();
			final String sName = opc.getName() + this.m_sAlgorithmName;
			if (olsp.getKeepAsFinalResult()) {
				try {
					final Output out = ooset.getOutput(opc.getName());
					final Output outToAdd = out.getClass().newInstance();
					outToAdd.setName(sName);
					outToAdd.setDescription(olsp.getName());
					oosetGlobal.add(outToAdd);
				}
				catch (final Exception e) {
				}
			}
			else {
				oosetGlobal.remove(sName);
			}
		}	   

		return (true);
	}


	@Override
	protected double[][] getTableLayoutMatrix() {

		int i;
		int iRows = 0;

		final double iSizeColumns[] = { 10, TableLayoutConstants.FILL, 360, 10 };

		iRows += m_Algorithm.getNumberOfParameters();

		if (m_Algorithm.requiresRasterLayers() || m_Algorithm.requiresMultipleRasterLayers()
				|| m_Algorithm.requiresMultipleRasterBands()) {
			iRows++;
		}
		if (m_Algorithm.requiresVectorLayers() || m_Algorithm.requiresMultipleVectorLayers()) {
			iRows++;
		}
		if (m_Algorithm.requiresTables() || m_Algorithm.requiresMultipleTables()) {
			iRows++;
		}
		if (m_Algorithm.requires3DRasterLayers()) {
			iRows++;
		}
		if (m_Algorithm.requiresNonDataObjects()) {
			iRows++;
		}

		final OutputObjectsSet ooSet = m_Algorithm.getOutputObjects();
		int iOutput = ooSet.getOutputObjectsCount();

		/*
		final int iVectorLayers = ooSet.getVectorLayersCount();
		if (iVectorLayers != 0) {
			iRows += 2;
			iOutput += 2;
		}
		*/

		if (iOutput != 0) {
			iRows += (iOutput + 1);
		}
		iRows++;

		final double iSizeRows[] = new double[iRows];
		for (i = 0; i < iRows - iOutput; i++) {
			iSizeRows[i] = CELL_HEIGHT;
		}
		for (i = iRows - iOutput; i < iRows; i++) {
			iSizeRows[i] = CELL_HEIGHT;
		}
		iSizeRows[iRows-1] = CELL_HEIGHT/2;

		final double iSize[][] = new double[2][];
		iSize[0] = iSizeColumns;
		iSize[1] = iSizeRows;

		return iSize;

	}


}
