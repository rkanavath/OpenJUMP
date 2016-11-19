package org.openjump.alg;

import java.util.ArrayList;
import java.util.List;

import org.nfunk.jep.JEP;

import es.unex.sextante.core.ObjectAndDescription;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.parameters.RasterLayerAndBand;

public class FormulaParser {
    private static final int MAX_BANDS = 256;

    public static String prepareFormula(String sFormula, List<String> names) {
        StringBuffer sb = new StringBuffer(sFormula);
        int iIndex = 0;
        while ((iIndex = sb.indexOf("|", iIndex)) != -1) {
            if ((sb.indexOf("|", iIndex - 1) == -1)
                    && (sb.indexOf("|", iIndex + 1) == -1)) {
                sb.delete(iIndex, iIndex + 1);
                sb.insert(iIndex, " Band ");
                iIndex = 0;
            } else {
                iIndex++;
            }
        }
        iIndex = 0;
        for (int i = 0; i < names.size(); i++) {
            String sName = ((String) names.get(i)).toLowerCase();
            while ((iIndex = sb.indexOf(sName, iIndex)) != -1) {
                String sSubstring = sb.substring(iIndex, Math.min(iIndex
                        + sName.length() + " band ".length(), sb.length()));
                if (!sSubstring.toLowerCase().equals(sName + " band ")) {
                    sb.delete(iIndex, iIndex + sName.length());
                    sb.insert(iIndex, sName + " Band 1");
                    iIndex = 0;
                } else {
                    iIndex++;
                }
            }
        }
        return sb.toString();
    }

    public static String getNormalizedName(String name) {
        String pattern = "^[\\d].*";
        if ((name != null) && (name.matches(pattern))) {
            name = "prefix_" + name;
        }
        return name;
    }

    public static String replaceVariables(String formula, String layerName,
            String normalizedName) {
        if (!normalizedName.equals(layerName)) {
            return formula.replaceAll(layerName, normalizedName);
        }
        return formula;
    }

    public static List<RasterLayerAndBand> getBandsFromFormula(String sFormula,
            List<IRasterLayer> layers) {
        List<RasterLayerAndBand> array = new ArrayList();
        List<String> names = new ArrayList();
        JEP jep = new JEP();
        jep.addStandardConstants();
        jep.addStandardFunctions();
        for (int i = 0; i < layers.size(); i++) {
            IRasterLayer layer = (IRasterLayer) layers.get(i);
            String layerName = layer.getName();
            String normalizedLayerName = getNormalizedName(layerName);
            sFormula = replaceVariables(sFormula, layerName,
                    normalizedLayerName);
            names.add(normalizedLayerName);
        }
        sFormula = prepareFormula(sFormula.toLowerCase(), names);
        sFormula = sFormula.toLowerCase().replaceAll(" ", "");
        sFormula = sFormula.replaceAll("\\[", "_");
        sFormula = sFormula.replaceAll("\\]", "_");
        sFormula = replaceDots(sFormula);
        int i = 0;
        for (i = 0; i < layers.size(); i++) {
            IRasterLayer layer = (IRasterLayer) layers.get(i);
            for (int j = 0; j < layer.getBandsCount(); j++) {
                String sLayerName = layer.getName() + " Band "
                        + Integer.toString(j + 1);
                sLayerName = sLayerName.toLowerCase();
                sLayerName = sLayerName.replaceAll(" ", "");
                sLayerName = sLayerName.replaceAll("\\[", "_");
                sLayerName = sLayerName.replaceAll("\\]", "_");
                sLayerName = replaceDots(sLayerName);
                sLayerName = getNormalizedName(sLayerName);
                if (sFormula.lastIndexOf(sLayerName) != -1) {
                    array.add(new RasterLayerAndBand(layer, j));
                    jep.addVariable(sLayerName, 0.0D);
                }
            }
        }
        jep.parseExpression(sFormula);
        if (jep.hasError()) {
            Sextante.addErrorToLog(jep.getErrorInfo());
            return null;
        }
        if (array.size() == 0) {
            return null;
        }
        return array;
    }

    public static List<String> getBandsFromFormulaForModeler(String sFormula,
            ObjectAndDescription[] layers, ObjectAndDescription[] numerical) {
        List<String> array = new ArrayList();
        List<String> names = new ArrayList();
        JEP jep = new JEP();
        jep.addStandardConstants();
        jep.addStandardFunctions();
        for (int i = 0; i < layers.length; i++) {
            ObjectAndDescription oad = layers[i];
            String layerName = (String) oad.getObject();
            String normalizedLayerName = getNormalizedName(layerName);
            sFormula = replaceVariables(sFormula, layerName,
                    normalizedLayerName);
            names.add(layerName);
        }
        int i = 0;
        for (i = 0; i < numerical.length; i++) {
            jep.addVariable(((String) numerical[i].getObject()).toLowerCase(),
                    0.0D);
        }
        sFormula = prepareFormula(sFormula.toLowerCase(), names);
        sFormula = sFormula.toLowerCase().replaceAll(" ", "");
        sFormula = sFormula.replaceAll("\\[", "_");
        sFormula = sFormula.replaceAll("\\]", "_");
        sFormula = sFormula.replaceAll("\\:", "_");
        sFormula = sFormula.replaceAll("\"", "_");
        sFormula = replaceDots(sFormula);
        for (i = 0; i < names.size(); i++) {
            String sName = (String) names.get(i);
            for (int j = 0; j < 256; j++) {
                String sLayerName = sName + " Band " + Integer.toString(j + 1);
                sLayerName = sLayerName.toLowerCase();
                sLayerName = sLayerName.replaceAll(" ", "");
                sLayerName = sLayerName.replaceAll("\\[", "_");
                sLayerName = sLayerName.replaceAll("\\]", "_");
                sLayerName = sLayerName.replaceAll("\\:", "_");
                sLayerName = sLayerName.replaceAll("\"", "_");
                sLayerName = replaceDots(sLayerName);
                sLayerName = getNormalizedName(sLayerName);
                if (sFormula.lastIndexOf(sLayerName) != -1) {
                    array.add((String) layers[i].getObject());
                    jep.addVariable(sLayerName, 0.0D);
                }
            }
        }
        jep.parseExpression(sFormula);
        if (jep.hasError()) {
            return null;
        }
        if (array.size() == 0) {
            return null;
        }
        return array;
    }

    public static String replaceDots(String s) {
        StringBuffer sb = new StringBuffer(s);
        for (int i = 0; i < sb.length() - 1; i++) {
            char c = sb.charAt(i);
            char c2 = sb.charAt(i + 1);
            if ((c == '.') && (!Character.isDigit(c2))) {
                sb = sb.deleteCharAt(i);
            }
        }
        return sb.toString();
    }
}
