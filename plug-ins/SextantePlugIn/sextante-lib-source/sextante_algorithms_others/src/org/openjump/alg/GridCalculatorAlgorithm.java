package org.openjump.alg;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.nfunk.jep.JEP;

import es.unex.sextante.core.AnalysisExtent;
import es.unex.sextante.core.GeoAlgorithm;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.exceptions.GeoAlgorithmExecutionException;
import es.unex.sextante.exceptions.RepeatedParameterNameException;
import es.unex.sextante.gui.modeler.ModelAlgorithm;
import es.unex.sextante.modeler.elements.ModelElementNumericalValue;
import es.unex.sextante.parameters.RasterLayerAndBand;

public class GridCalculatorAlgorithm extends GeoAlgorithm {
    public static final String LAYERS = "LAYERS";
    public static final String FORMULA = "FORMULA";
    public static final String RESULT = "RESULT";

    public boolean processAlgorithm() throws GeoAlgorithmExecutionException {
        List<String> names = new ArrayList();
        ArrayList layers = this.m_Parameters
                .getParameterValueAsArrayList("LAYERS");
        String sFormula = this.m_Parameters
                .getParameterValueAsString("FORMULA").toLowerCase();
        List<RasterLayerAndBand> bands = FormulaParser.getBandsFromFormula(
                sFormula, layers);
        if (bands == null) {
            throw new GeoAlgorithmExecutionException(
                    Sextante.getText("Syntax_error"));
        }
        IRasterLayer m_Result = getNewRasterLayer("RESULT",
                Sextante.getText("Result"), 5);

        IRasterLayer[] window = new IRasterLayer[bands.size()];
        String[] sVariable = new String[bands.size()];
        int[] iBands = new int[bands.size()];
        AnalysisExtent extent = m_Result.getWindowGridExtent();
        int iNX = m_Result.getWindowGridExtent().getNX();
        int iNY = m_Result.getWindowGridExtent().getNY();

        JEP jep = new JEP();
        jep.addStandardConstants();
        jep.addStandardFunctions();
        for (int i = 0; i < bands.size(); i++) {
            RasterLayerAndBand layerAndBand = (RasterLayerAndBand) bands.get(i);
            IRasterLayer layer = layerAndBand.getRasterLayer();
            iBands[i] = layerAndBand.getBand();
            String layerName = layer.getName();
            String normalizedName = FormulaParser.getNormalizedName(layerName);
            sFormula = FormulaParser.replaceVariables(sFormula, layerName,
                    normalizedName);
            names.add(normalizedName);
            window[i] = layer;
            window[i].setWindowExtent(extent);
            if ((layer.getDataType() == 4) || (layer.getDataType() == 5)) {
                window[i].setInterpolationMethod(4);
            } else {
                window[i].setInterpolationMethod(0);
            }
            sVariable[i] = (layer.getName() + " Band " + Integer
                    .toString(iBands[i] + 1));
            sVariable[i] = sVariable[i].toLowerCase();
            sVariable[i] = sVariable[i].replaceAll(" ", "");
            sVariable[i] = sVariable[i].replaceAll("\\[", "_");
            sVariable[i] = sVariable[i].replaceAll("\\]", "_");
            sVariable[i] = FormulaParser.replaceDots(sVariable[i]);
            sVariable[i] = FormulaParser.getNormalizedName(sVariable[i]);

            jep.addVariable(sVariable[i], 0.0D);
        }
        sFormula = FormulaParser.prepareFormula(sFormula, names);
        sFormula = sFormula.toLowerCase().replaceAll(" ", "");
        sFormula = sFormula.replaceAll("\\[", "_");
        sFormula = sFormula.replaceAll("\\]", "_");
        sFormula = FormulaParser.replaceDots(sFormula);
        jep.parseExpression(sFormula);
        if (!jep.hasError()) {
            for (int y = 0; (y < iNY) && (setProgress(y, iNY)); y++) {
                for (int x = 0; x < iNX; x++) {
                    int i = 0;
                    for (i = 0; i < bands.size(); i++) {
                        double dValue = window[i].getCellValueAsDouble(x, y,
                                iBands[i]);
                        if (!window[i].isNoDataValue(dValue)) {
                            jep.addVariable(sVariable[i], dValue);
                        } else {
                            m_Result.setNoData(x, y);
                            break;
                        }
                    }
                    if (i == bands.size()) {
                        double dValue = jep.getValue();
                        if (!Double.isNaN(dValue)) {
                            m_Result.setCellValue(x, y, dValue);
                        } else {
                            m_Result.setNoData(x, y);
                        }
                    }
                }
            }
        }
        throw new GeoAlgorithmExecutionException(jep.getErrorInfo());
    }

    public void defineCharacteristics() {
        setName(Sextante.getText("Raster_calculator"));
        setGroup(Sextante.getText("Basic_tools_for_raster_layers"));
        setUserCanDefineAnalysisExtent(true);
        try {
            this.m_Parameters.addMultipleInput("LAYERS",
                    Sextante.getText("Layers"), 1, true);
            this.m_Parameters.addString("FORMULA", Sextante.getText("Formula"));
            addOutputRasterLayer("RESULT", Sextante.getText("Result"));
        } catch (RepeatedParameterNameException e) {
            Sextante.addErrorToLog(e);
        }
    }

    public boolean preprocessForModeller(Object obj)
            throws GeoAlgorithmExecutionException {
        ModelAlgorithm model = (ModelAlgorithm) obj;
        try {
            String sFormula = this.m_Parameters.getParameterValueAsString(
                    "FORMULA").toLowerCase();
            String sKey = model.getInputAsignment("LAYERS", this);
            HashMap inputs = model.getInputs();
            ArrayList array = (ArrayList) inputs.get(sKey);
            ArrayList<String> variables = new ArrayList();
            for (int i = 0; i < array.size(); i++) {
                sKey = (String) array.get(i);
                IRasterLayer layer = (IRasterLayer) inputs.get(sKey);
                String sVariableName = layer.getName();
                if (variables.contains(sVariableName)) {
                    char c = (char) (97 + i);
                    sVariableName = Character.toString(c) + "_" + sVariableName;
                    layer.setName(sVariableName);
                }
                variables.add(sVariableName);
                sFormula = sFormula.replace(sKey.toLowerCase(), sVariableName);
            }
            Set set = inputs.keySet();
            Iterator iter = set.iterator();
            while (iter.hasNext()) {
                Object key = iter.next();
                Object input = inputs.get(key);
                if (((input instanceof Double))
                        && (sFormula.contains(((String) key).toLowerCase()))) {
                    sFormula = sFormula.replace(((String) key).toLowerCase(),
                            input.toString());
                }
                if (((input instanceof ModelElementNumericalValue))
                        && (sFormula.contains(((String) key).toLowerCase()))) {
                    return false;
                }
            }
            this.m_Parameters.getParameter("FORMULA").setParameterValue(
                    sFormula);
        } catch (Exception e) {
            throw new GeoAlgorithmExecutionException(
                    Sextante.getText("Syntax_error"));
        }
        return true;
    }
}
