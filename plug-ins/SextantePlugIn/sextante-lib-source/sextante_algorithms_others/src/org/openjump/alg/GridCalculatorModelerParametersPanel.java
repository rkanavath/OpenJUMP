package org.openjump.alg;

import info.clearthought.layout.TableLayout;

import java.awt.Dimension;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import es.unex.sextante.core.ObjectAndDescription;
import es.unex.sextante.core.OutputObjectsSet;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.exceptions.WrongOutputIDException;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.modeler.GeoAlgorithmModelerParametersPanel;
import es.unex.sextante.gui.modeler.OutputLayerSettingsPanel;
import es.unex.sextante.modeler.elements.ModelElementNumericalValue;
import es.unex.sextante.modeler.elements.ModelElementRasterLayer;
import es.unex.sextante.outputs.Output;
import es.unex.sextante.parameters.Parameter;

public class GridCalculatorModelerParametersPanel extends
        GeoAlgorithmModelerParametersPanel {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private static final int MAX_BANDS = 3;
    private JTextArea jTextExpression;
    private JScrollPane jScrollPane;
    private JTree jTree;
    private JScrollPane jScrollPane1;
    private CalculatorKeysPanel m_KeysPanel;
    private HashMap m_Constants;
    private OutputLayerSettingsPanel m_OutputLayerSettingsPanel;

    protected void initGUI() {
        setPreferredSize(new Dimension(570, 350));
        TableLayout thisLayout = new TableLayout(
                new double[][] {
                        { 10.0D, -1.0D, -1.0D, -1.0D, -1.0D, 10.0D },
                        { 10.0D, -1.0D, -1.0D, -1.0D, 50.0D, 5.0D, 50.0D, 5.0D,
                                10.0D } });

        thisLayout.setHGap(10);
        thisLayout.setVGap(10);
        setLayout(thisLayout);
        setSize(new Dimension(350, 350));

        this.jScrollPane = new JScrollPane();
        add(this.jScrollPane, "1, 4, 4, 4");
        this.jScrollPane.setHorizontalScrollBarPolicy(31);

        this.jTextExpression = new JTextArea();
        this.jScrollPane.setViewportView(this.jTextExpression);
        this.jTextExpression.setPreferredSize(new Dimension(0, 0));
        this.jTextExpression.setBorder(BorderFactory.createBevelBorder(1));
        try {
            Parameter param = this.m_Algorithm.getParameters().getParameter(
                    "FORMULA");
            ObjectAndDescription oad = (ObjectAndDescription) getParameterValue(param);
            if (oad != null) {
                String sFormula = (String) oad.getObject();
                this.jTextExpression.setText(sFormula);
            }
        } catch (Exception e) {
            Sextante.addErrorToLog(e);
        }
        this.m_KeysPanel = new CalculatorKeysPanel(this.jTextExpression);
        add(this.m_KeysPanel, "3, 1, 4, 3");

        this.jScrollPane1 = new JScrollPane();
        add(this.jScrollPane1, "1, 1, 2, 3");

        this.jTree = new JTree();
        this.jScrollPane1.setViewportView(this.jTree);
        this.jScrollPane1.setHorizontalScrollBarPolicy(31);
        this.jTree.setBorder(BorderFactory.createBevelBorder(1));
        MouseListener ml = new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                int iRow = GridCalculatorModelerParametersPanel.this.jTree
                        .getRowForLocation(e.getX(), e.getY());
                TreePath path = GridCalculatorModelerParametersPanel.this.jTree
                        .getPathForLocation(e.getX(), e.getY());
                if ((iRow != -1) && (e.getClickCount() == 2)) {
                    GridCalculatorModelerParametersPanel.this
                            .insertTextFromTree(path);
                }
            }
        };
        this.jTree.addMouseListener(ml);
        try {
            populateTree();
        } catch (Exception e) {
            e.printStackTrace();
        }
        OutputObjectsSet oosetGlobal = this.m_GlobalAlgorithm
                .getOutputObjects();
        this.m_OutputLayerSettingsPanel = new OutputLayerSettingsPanel();
        String sDescription = Sextante.getText("Result");
        String sKey = "RESULT";
        if (!oosetGlobal.containsKey("RESULT" + this.m_sAlgorithmName)) {
            sDescription = "\"" + sDescription + "\" "
                    + Sextante.getText("from") + " "
                    + this.m_sAlgorithmDescription;
            this.m_OutputLayerSettingsPanel.setKeepAsFinalResult(false);
        } else {
            try {
                Output out = oosetGlobal.getOutput("RESULT"
                        + this.m_sAlgorithmName);
                sDescription = out.getDescription();
                this.m_OutputLayerSettingsPanel.setKeepAsFinalResult(true);
            } catch (WrongOutputIDException e) {
            }
        }
        this.m_OutputLayerSettingsPanel.setName(sDescription);

        add(this.m_OutputLayerSettingsPanel, "2, 6, 4, 6");
        add(new JLabel(Sextante.getText("Result")), "1,6,2,6");
    }

    private void populateTree() {
        this.jTree.setModel(null);

        ObjectAndDescription[] layers = getElementsOfClass(
                ModelElementRasterLayer.class, false);
        DefaultMutableTreeNode main = new DefaultMutableTreeNode(
                Sextante.getText("ELEMENTS"));
        DefaultMutableTreeNode node = new DefaultMutableTreeNode(
                Sextante.getText("Layers"));
        for (int i = 0; i < layers.length; i++) {
            ObjectAndDescription oad = (ObjectAndDescription) this.m_DataObjects
                    .get(layers[i].getObject());
            ModelElementRasterLayer merl = (ModelElementRasterLayer) oad
                    .getObject();
            int iBands = merl.getNumberOfBands();
            if (iBands == -1) {
                iBands = 3;
            }
            for (int j = 0; j < iBands; j++) {
                String sName = layers[i].getDescription() + " Band "
                        + Integer.toString(j + 1);
                String sVariableName = layers[i].getObject() + " Band "
                        + Integer.toString(j + 1);
                DefaultMutableTreeNode child;

                if ((iBands == 3) && (j != 0)) {
                    child = new DefaultMutableTreeNode(
                            new ObjectAndDescription(sName + "["
                                    + Sextante.getText("unchecked") + "]",
                                    sVariableName));
                } else {
                    child = new DefaultMutableTreeNode(
                            new ObjectAndDescription(sName, sVariableName));
                }
                node.add(child);
            }
        }
        main.add(node);

        String[] sFunctions = { "sin", "cos", "tan", "asin", "acos", "atan",
                "atan2", "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
                "ln", "log", "exp", "abs", "rand", "mod", "sqrt", "if" };

        node = new DefaultMutableTreeNode(Sextante.getText("Functions"));
        int i = 0;
        for (i = 0; i < sFunctions.length; i++) {
            DefaultMutableTreeNode child = new DefaultMutableTreeNode(" "
                    + sFunctions[i] + "() ");
            node.add(child);
        }
        main.add(node);

        String[] sOperators = { "+", "-", "*", "/", "%", "!", "^", "&&", "||",
                "<", ">", "<=", ">=", "==", "!=" };
        node = new DefaultMutableTreeNode(Sextante.getText("Operators"));
        for (i = 0; i < sOperators.length; i++) {
            String s = " " + sOperators[i] + " ";
            DefaultMutableTreeNode child = new DefaultMutableTreeNode(
                    new ObjectAndDescription(s, s));
            node.add(child);
        }
        main.add(node);

        ObjectAndDescription[] values = getElementsOfClass(
                ModelElementNumericalValue.class, false);
        node = new DefaultMutableTreeNode(Sextante.getText("Numerical_values"));
        for (i = 0; i < values.length; i++) {
            String sName = values[i].getDescription();
            String sVariableName = values[i].getObject().toString();
            DefaultMutableTreeNode child = new DefaultMutableTreeNode(
                    new ObjectAndDescription(sName, sVariableName));
            node.add(child);
        }
        main.add(node);

        node = new DefaultMutableTreeNode(Sextante.getText("Constants"));
        this.m_Constants = new HashMap();
        this.m_Constants.put("e", " " + Double.toString(2.718281828459045D)
                + " ");
        this.m_Constants.put("Pi", " " + Double.toString(3.141592653589793D)
                + " ");
        this.m_Constants.put(
                "NODATA",
                " "
                        + Double.toString(SextanteGUI.getOutputFactory()
                                .getDefaultNoDataValue()) + " ");
        Set set = this.m_Constants.keySet();
        Iterator iter = set.iterator();
        while (iter.hasNext()) {
            String s = (String) iter.next();
            DefaultMutableTreeNode child = new DefaultMutableTreeNode(
                    new ObjectAndDescription(s, s));
            node.add(child);
        }
        main.add(node);

        this.jTree.setModel(new DefaultTreeModel(main));
        DefaultTreeCellRenderer renderer = (DefaultTreeCellRenderer) this.jTree
                .getCellRenderer();
        renderer.setOpenIcon(null);
        renderer.setClosedIcon(null);
        renderer.setLeafIcon(null);
    }

    private void insertTextFromTree(TreePath path) {
        TreePath parent = path.getParentPath();
        if ((parent != null)
                && (!parent.toString().equals(
                        "[" + Sextante.getText("ELEMENTS") + "]"))) {
            String sParentName = parent.toString();
            DefaultMutableTreeNode node = (DefaultMutableTreeNode) path
                    .getLastPathComponent();
            String s = (String) ((ObjectAndDescription) node.getUserObject())
                    .getObject();
            if (sParentName.equals("[" + Sextante.getText("ELEMENTS") + ", "
                    + Sextante.getText("Constants]"))) {
                if (this.m_Constants.containsKey(s)) {
                    s = (String) this.m_Constants.get(s);
                } else {
                    s = "";
                }
            }
            this.jTextExpression.insert(s,
                    this.jTextExpression.getCaretPosition());
            if (sParentName.equals("[" + Sextante.getText("ELEMENTS") + ", "
                    + Sextante.getText("Functions]"))) {
                this.jTextExpression.setCaretPosition(this.jTextExpression
                        .getCaretPosition() - 2);
            }
        }
    }

    public boolean assignParameters(HashMap map) {
        ObjectAndDescription[] layers = getElementsOfClass(
                ModelElementRasterLayer.class, false);
        ObjectAndDescription[] numerical = getElementsOfClass(
                ModelElementNumericalValue.class, false);
        ArrayList layersList = new ArrayList();

        String sFormula = this.jTextExpression.getText();
        List<String> array = FormulaParser.getBandsFromFormulaForModeler(
                sFormula, layers, numerical);
        if (array == null) {
            return false;
        }
        layersList.clear();
        for (int i = 0; i < array.size(); i++) {
            String sKey = (String) array.get(i);
            if (!layersList.contains(sKey)) {
                layersList.add(sKey);
            }
        }
        ObjectAndDescription[] values = getElementsOfClass(
                ModelElementNumericalValue.class, false);
        for (int i = 0; i < values.length; i++) {
            if (sFormula.contains(values[i].getObject().toString())) {
                map.put("DUMMY" + getInnerParameterKey(), values[i].getObject()
                        .toString());
            }
        }
        String sArrayKey = getInnerParameterKey();
        map.put("LAYERS", sArrayKey);
        this.m_DataObjects.put(sArrayKey, new ObjectAndDescription(
                "Multiple Input", layersList));

        String sFormulaKey = getInnerParameterKey();
        map.put("FORMULA", sFormulaKey);
        this.m_DataObjects.put(sFormulaKey, new ObjectAndDescription("String",
                this.jTextExpression.getText()));

        OutputObjectsSet oosetGlobal = this.m_GlobalAlgorithm
                .getOutputObjects();
        OutputObjectsSet ooset = this.m_Algorithm.getOutputObjects();

        String sName = "RESULT" + this.m_sAlgorithmName;
        if (this.m_OutputLayerSettingsPanel.getKeepAsFinalResult()) {
            try {
                Output out = ooset.getOutput("RESULT");
                Output outToAdd = (Output) out.getClass().newInstance();
                outToAdd.setName(sName);
                outToAdd.setDescription(this.m_OutputLayerSettingsPanel
                        .getName());
                oosetGlobal.add(outToAdd);
            } catch (Exception e) {
            }
        } else {
            oosetGlobal.remove(sName);
        }
        return true;
    }

}
