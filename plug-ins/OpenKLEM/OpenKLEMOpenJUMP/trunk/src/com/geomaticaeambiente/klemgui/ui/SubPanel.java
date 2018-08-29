package com.geomaticaeambiente.klemgui.ui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.HashMap;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import com.geomaticaeambiente.klemgui.utils.ComponentEntry;

/**
 * Class to create the panel for input or output components.
 * 
 * @author Geomatica
 */
public class SubPanel extends JPanel {

    /**
     * Constructor that memorize the components that will build the panel.
     * 
     * @param listComp
     *            List of ComponentEntry[] that will form the panel.
     * @param title
     *            Panel title
     */
    public SubPanel(List<ComponentEntry[]> listComp, String title) {

        this.listComp = listComp;
        this.title = title;

        // outputParameters = new TreeMap();
        initComponents();
    }

    /**
     * Set the components of the panel.
     */
    private void initComponents() {

        setLayout(new GridBagLayout());
        // Check components width to extract the biggest one.
        final HashMap<Integer, Integer> maxWidth = checkComponentSize(listComp,
                Integer.MAX_VALUE); // GUIUtils.SUB_PANEL_WIDTH);

        int countGridRows = 0;

        for (int n = 0; n < listComp.size(); n++) {
            final ComponentEntry[] ce = listComp.get(n);
            // for (ComponentEntry[] ce : listComp) {
            int countComp = 0;
            int columnGrid = 0;
            int width_ = 0;

            for (int m = 0; m < ce.length; m++) {

                final GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
                gridBagConstraints.gridx = columnGrid;
                gridBagConstraints.gridy = countGridRows;
                gridBagConstraints.weightx = 1;
                gridBagConstraints.weighty = 1;
                gridBagConstraints.insets = new Insets(5, 5, 5, 5); // space
                                                                    // belove a
                                                                    // component
                gridBagConstraints.anchor = GridBagConstraints.NORTHWEST;
                gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;

                // set the width of 2nd component based on the nuber of elements
                // on the row.
                if (ce[m].getWidth() == 0) {
                    if (m == 0 && ce.length == 1) {
                        width_ = GUIUtils.SUB_PANEL_WIDTH - DIFF;
                        gridBagConstraints.gridwidth = maxWidth.size();

                    } else if (m == 1 && ce.length == 2) {
                        if (ce[m].getComponent() instanceof JTextField
                                || ce[m].getComponent() instanceof JComboBox
                                || ce[m].getComponent() instanceof JLabel) {
                            width_ = GUIUtils.SUB_PANEL_WIDTH - DIFF
                                    - maxWidth.get(0);
                            gridBagConstraints.gridwidth = maxWidth.size() - 1;
                        }
                    } else if (m == 1 && ce.length == 3) {
                        if (ce[m].getComponent() instanceof JTextField
                                || ce[m].getComponent() instanceof JComboBox
                                || ce[m].getComponent() instanceof JLabel) {
                            width_ = GUIUtils.SUB_PANEL_WIDTH - DIFF
                                    - maxWidth.get(0) - maxWidth.get(2);
                            gridBagConstraints.gridwidth = maxWidth.size() - 2;
                            if (maxWidth.size() - 2 > 1) {
                                columnGrid = columnGrid
                                        + (maxWidth.size() - 2 - 1);
                            }
                        }
                    } else if (m == 1 && ce.length == 4) {
                        if (ce[m].getComponent() instanceof JTextField
                                || ce[m].getComponent() instanceof JComboBox
                                || ce[m].getComponent() instanceof JLabel) {
                            width_ = GUIUtils.SUB_PANEL_WIDTH - DIFF
                                    - maxWidth.get(0) - maxWidth.get(2)
                                    - maxWidth.get(3);
                            gridBagConstraints.gridwidth = maxWidth.size() - 3;
                        }
                    } else {
                        width_ = maxWidth.get(m);
                    }
                } else {
                    width_ = ce[m].getWidth();
                }

                // ce[m].getComponent().setMinimumSize(new Dimension(width_,
                // ce[m].getComponent().getPreferredSize().height));
                ce[m].getComponent().setPreferredSize(
                        new Dimension(width_, ce[m].getComponent()
                                .getPreferredSize().height));
                // rowHeight = calculateHeight(rowHeight, ce[m].getComponent());

                add(ce[m].getComponent(), gridBagConstraints);
                columnGrid++;

                // outputParameters.put((PluginUtils.producerKeyMap(title,
                // n)+countComp), ce[m].getComponent());
                countComp++;
            }

            countGridRows++;
        }

        setBorder(BorderFactory.createTitledBorder(title));
        // setPreferredSize(new Dimension(GUIUtils.SUB_PANEL_WIDTH, height));
        // setMinimumSize(new Dimension(GUIUtils.SUB_PANEL_WIDTH, height));
        // setMinimumSize(new Dimension(Integer.MAX_VALUE, height));
    }

    /**
     * Method to check the width of components.
     * 
     * @param componentsEntry
     *            List of components.
     * @param panelWidth
     *            With of the panel that contains the components
     * @return An HasMap which size derives from the max number of elements on
     *         the row of the panel.
     */
    private HashMap checkComponentSize(List<ComponentEntry[]> componentsEntry,
            int panelWidth) {

        // Map to memorize the width of component for each element of the line.
        // The key map is the element position on the line.
        final HashMap<Integer, Integer> positionWidthMap = new HashMap<Integer, Integer>();// mappa
                                                                                           // che
                                                                                           // ha
                                                                                           // come
                                                                                           // chiave
                                                                                           // la
                                                                                           // posizione
                                                                                           // dell'elemento
                                                                                           // nella
                                                                                           // riga
        positionWidthMap.put(0, -Integer.MAX_VALUE);

        for (int l = 0; l < componentsEntry.size(); l++) {
            final ComponentEntry[] compEntries = componentsEntry.get(l);

            int width = 0;

            // check the type of component and the position on the row.
            for (int comp = 0; comp < compEntries.length; comp++) { // controllo
                                                                    // il tipo
                                                                    // di
                                                                    // componente
                                                                    // e la
                                                                    // posizione
                                                                    // nella
                                                                    // riga
                final JComponent component = compEntries[comp].getComponent();
                if (component instanceof PersonalTableComponents
                        || component instanceof PersonalRasterCombPanel) {
                    continue;
                }

                width = component.getPreferredSize().width;
                int max = 0;
                if (positionWidthMap.containsKey(comp)) {
                    max = Math.max(width, positionWidthMap.get(comp));
                } else {
                    max = width;
                }
                positionWidthMap.put(comp, max);
            }
        }

        // Check that the width of elements on the row is lower than the panel
        // one,
        // otherwise it reduces the width of second element.
        int sum = 0;
        int widthPosition1 = 0;
        for (int n = 0; n < positionWidthMap.size(); n++) {
            if (n == 1) {
                widthPosition1 = positionWidthMap.get(n);
            }

            final int widthValue = positionWidthMap.get(n);
            sum = sum + widthValue;
        }

        if (sum > panelWidth) {
            final int newWidthPosition1 = panelWidth
                    - (sum - positionWidthMap.get(0) - DIFF); // 50 per dare un
                                                              // po' di distacco
                                                              // dai margini
            positionWidthMap.put(1, newWidthPosition1);
        }
        return positionWidthMap;

    }

    /**
     * Calculate the height of component
     * 
     * @param hSum
     *            sum of height of previous components
     * @param component
     * @return Sum of previous components plus component height and top and
     *         bottom insets.
     */
    private int calculateHeight(int hSum, JComponent component) {

        if (component instanceof PersonalRasterCombPanel) {
            return Math.max(hSum, component.getPreferredSize().height
                    + component.getInsets().bottom + component.getInsets().top
                    + 15);
        }
        if (component instanceof JLabel) {
            return Math.max(hSum,
                    20 + component.getInsets().bottom
                            + component.getInsets().top + 15);
        }

        return Math.max(hSum,
                component.getPreferredSize().height
                        + component.getInsets().bottom
                        + component.getInsets().top);
    }

    // public TreeMap getOutputParameters() {
    // return outputParameters;
    // }

    private final List<ComponentEntry[]> listComp;
    private final String title;
    // private TreeMap outputParameters;
    private final int DIFF = 30;
}
