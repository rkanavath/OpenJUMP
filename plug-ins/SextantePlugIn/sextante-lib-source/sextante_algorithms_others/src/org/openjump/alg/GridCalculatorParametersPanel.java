package org.openjump.alg;

import info.clearthought.layout.TableLayout;

import java.awt.Dimension;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import es.unex.sextante.core.GeoAlgorithm;
import es.unex.sextante.core.OutputObjectsSet;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.exceptions.WrongInputException;
import es.unex.sextante.gui.algorithm.GeoAlgorithmParametersPanel;
import es.unex.sextante.gui.algorithm.OutputChannelSelectionPanel;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.outputs.Output;
import es.unex.sextante.parameters.RasterLayerAndBand;

public class GridCalculatorParametersPanel extends GeoAlgorithmParametersPanel {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private JTextArea jTextExpression;
    private JScrollPane jScrollPane;
    private JTree jTree;
    private JScrollPane jScrollPane1;
    private CalculatorKeysPanel m_KeysPanel;
    private TreeMap m_Constants;
    private GeoAlgorithm m_Algorithm;
    private OutputChannelSelectionPanel outputChannelSelectionPanel;

    public void init(GeoAlgorithm algorithm) {
        this.m_Algorithm = algorithm;

        initGUI();
    }

    private void initGUI() {
        setPreferredSize(new Dimension(570, 350));
        TableLayout thisLayout = new TableLayout(new double[][] {
                { 10.0D, -1.0D, -1.0D, -1.0D, -1.0D, 10.0D },
                { 10.0D, -1.0D, -1.0D, -1.0D, 50.0D, 5.0D, 20.0D, 5.0D, 20.0D,
                        10.0D } });

        thisLayout.setHGap(10);
        thisLayout.setVGap(10);
        setLayout(thisLayout);
        setSize(new Dimension(350, 350));

        this.jTextExpression = new JTextArea();
        this.jTextExpression.setLineWrap(true);
        this.jScrollPane = new JScrollPane(this.jTextExpression);
        add(this.jScrollPane, "1, 4, 4, 4");
        this.jScrollPane.setHorizontalScrollBarPolicy(31);
        this.jScrollPane.setVerticalScrollBarPolicy(22);
        this.jTextExpression.setBorder(BorderFactory.createBevelBorder(1));

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
                int iRow = GridCalculatorParametersPanel.this.jTree
                        .getRowForLocation(e.getX(), e.getY());
                TreePath path = GridCalculatorParametersPanel.this.jTree
                        .getPathForLocation(e.getX(), e.getY());
                if ((iRow != -1) && (e.getClickCount() == 2)) {
                    GridCalculatorParametersPanel.this.insertTextFromTree(path);
                }
            }
        };
        this.jTree.addMouseListener(ml);

        populateTree();
        try {
            OutputObjectsSet ooSet = this.m_Algorithm.getOutputObjects();
            Output out = ooSet.getOutput("RESULT");
            this.outputChannelSelectionPanel = new OutputChannelSelectionPanel(
                    out, this.m_Algorithm.getParameters());
            add(this.outputChannelSelectionPanel, "3,6,4,6");
            add(new JLabel(Sextante.getText("Result")), "1,6,2,6");
        } catch (Exception e) {
            Sextante.addErrorToLog(e);
        }
    }

    private void populateTree() {
        this.jTree.setModel(null);

        IRasterLayer[] layers = SextanteGUI.getInputFactory().getRasterLayers();
        DefaultMutableTreeNode main = new DefaultMutableTreeNode(
                Sextante.getText("ELEMENTS"));
        DefaultMutableTreeNode node = new DefaultMutableTreeNode(
                Sextante.getText("Layers"));
        for (int i = 0; i < layers.length; i++) {
            for (int j = 0; j < layers[i].getBandsCount(); j++) {
                String sName = layers[i].getName() + " Band "
                        + Integer.toString(j + 1);
                DefaultMutableTreeNode child = new DefaultMutableTreeNode(sName);
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
            DefaultMutableTreeNode child = new DefaultMutableTreeNode(" "
                    + sOperators[i] + " ");
            node.add(child);
        }
        main.add(node);

        node = new DefaultMutableTreeNode(Sextante.getText("Constants"));
        this.m_Constants = new TreeMap();
        this.m_Constants.put("e", " " + Double.toString(2.718281828459045D)
                + " ");
        this.m_Constants.put("Pi", " " + Double.toString(3.141592653589793D)
                + " ");
        this.m_Constants.put(
                "NODATA",
                " "
                        + Double.toString(SextanteGUI.getOutputFactory()
                                .getDefaultNoDataValue()) + " ");
        for (i = 0; i < layers.length; i++) {
            String sName = Sextante.getText("Cell_size_[")
                    + layers[i].getName() + "]";
            double dCellsize = layers[i].getLayerCellSizeX();
            this.m_Constants.put(sName, " " + Double.toString(dCellsize) + " ");
        }
        Set set = this.m_Constants.keySet();
        Iterator iter = set.iterator();
        while (iter.hasNext()) {
            DefaultMutableTreeNode child = new DefaultMutableTreeNode(
                    iter.next());
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
            String s = path.getLastPathComponent().toString();
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

    public void assignParameters() throws WrongInputException {
        IRasterLayer[] layers = SextanteGUI.getInputFactory().getRasterLayers();
        ArrayList layersList = new ArrayList();
        for (IRasterLayer element : layers) {
            layersList.add(element);
        }
        List<RasterLayerAndBand> array = FormulaParser.getBandsFromFormula(
                this.jTextExpression.getText(), layersList);
        if (array == null) {
            throw new WrongInputException();
        }
        layersList.clear();
        for (int i = 0; i < array.size(); i++) {
            RasterLayerAndBand rlab = (RasterLayerAndBand) array.get(i);
            IRasterLayer layer = rlab.getRasterLayer();
            if (!layersList.contains(layer)) {
                layersList.add(layer);
            }
        }
        try {
            this.m_Algorithm.getParameters().getParameter("LAYERS")
                    .setParameterValue(layersList);
            this.m_Algorithm.getParameters().getParameter("FORMULA")
                    .setParameterValue(this.jTextExpression.getText());
            OutputObjectsSet ooSet = this.m_Algorithm.getOutputObjects();
            Output out = ooSet.getOutput("RESULT");
            out.setOutputChannel(this.outputChannelSelectionPanel
                    .getOutputChannel());
        } catch (Exception e) {
            Sextante.addErrorToLog(e);
            throw new WrongInputException();
        }
    }

    public void setOutputValue(String outputName, String value) {
        this.outputChannelSelectionPanel.setText(value);
    }

    public void setParameterValue(String parameterName, String value) {
        if (parameterName.equals("FORMULA")) {
            this.jTextExpression.setText(value);
        }
    }
}
