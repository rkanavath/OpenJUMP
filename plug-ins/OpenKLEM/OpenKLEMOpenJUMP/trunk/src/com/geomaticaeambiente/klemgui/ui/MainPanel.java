package com.geomaticaeambiente.klemgui.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.openjump.core.rasterimage.RasterImageLayer;

import com.geomaticaeambiente.klemgui.ui.CustomComboBox.RasterComboBox;
import com.geomaticaeambiente.klemgui.utils.ComponentEntry;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.Layerable;

/**
 * Class to implements the main panel that will contain input, output panels.
 * This class will be added to InitialPanel.
 * 
 * @author Geomatica
 */
public abstract class MainPanel extends JPanel implements LayersChangedListener {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /**
     * Constructor that memorizes the ComponentsTreeMap with all input and
     * output parameters.
     * 
     * @param initialDialog
     * @param ptm
     *            ComponentsTreeMap with all input and output parameters for the
     *            plugin.
     * @param leftButtonVisible
     *            if true the leftButton button is visible on the main panel.
     * @param centerButtonVisible
     *            if true the centerButton button is visible on the main panel.
     * @param rightButtonVisible
     * @param rightButtonText
     * @param layerablesList
     */
    public MainPanel(InitialDialog initialDialog, ComponentsTreeMap ptm,
            boolean leftButtonVisible, boolean centerButtonVisible,
            boolean rightButtonVisible, String rightButtonText,
            LayerablesList layerablesList) {

        this.ptm = ptm;
        this.leftButtonVisible = leftButtonVisible;
        this.centerButtonVisible = centerButtonVisible;
        this.rightButtonVisible = rightButtonVisible;
        this.rightButtonText = rightButtonText;
        this.initialDialog = initialDialog;
        this.layerablesList = layerablesList;

        initComponents();

    }

    public MainPanel(InitialDialog initialDialog, ComponentsTreeMap ptm,
            boolean leftButtonVisible, boolean centerButtonVisible,
            boolean rightButtonVisible, String rightButtonText,
            boolean isExtraSubPanelPresent, ExtraSubPanelPosition position,
            LayerablesList layerablesList) {

        this.ptm = ptm;
        this.leftButtonVisible = leftButtonVisible;
        this.centerButtonVisible = centerButtonVisible;
        this.rightButtonVisible = rightButtonVisible;
        this.rightButtonText = rightButtonText;
        this.initialDialog = initialDialog;
        this.position = position;
        this.isExtraSubPanelPresent = isExtraSubPanelPresent;
        this.layerablesList = layerablesList;

        initComponents();

    }

    /**
     * Set initial components of main panel.
     */
    private void initComponents() {

        final GridBagLayout gbl1 = new GridBagLayout();

        setLayout(gbl1);

        // setBackground(Color.red);
        setBorder(BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));

        // label with tool name
        label = new JLabel("");
        label.setFont(new java.awt.Font("Tahoma", 2, 11));
        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = componentRow;
        gbc.insets = new Insets(1, 1, 10, 1);
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.FIRST_LINE_START;

        add(label, gbc);
        componentRow++;

        final List<SubPanel> subPaneList = setSubPanelOrder();
        subPaneList.removeAll(Collections.singleton(null));
        for (final SubPanel subPaneList1 : subPaneList) {
            if (subPaneList1 != null) {
                add(subPaneList1, getPanelGridBagConstraints());
                componentRow++;
            }
        }

        initMandatoryButton();
        initOptionalButton();
        validate();

        layerablesList.addListener(this);

    }

    /**
     * Create sub panels that contains input, output components.
     * 
     * @param components
     *            Components for input or output parameters.
     * @param title
     *            Title for the sub panel.
     * @return
     */
    public SubPanel getSubPanel(List<ComponentEntry[]> components, String title) {

        final SubPanel tempSubPanel = new SubPanel(components, title);

        return tempSubPanel;

    }

    /**
     * Set the Execute button
     */
    private void initMandatoryButton() {

        // JButton rightButton = new
        // JButton(PluginUtils.getResources().getString("MainPanel.ExecuteButton.text"));
        rightButton = new JButton(rightButtonText);

        final GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = componentRow;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new Insets(0, 0, 0, 8);

        if (rightButtonVisible) {
            rightButton.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    rightButton();
                }
            });
            add(rightButton, gridBagConstraints);
        }

    }

    /**
     * Set optional button as Load and Save.
     */
    private void initOptionalButton() {

        leftButton = new JButton(leftButtonText);

        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = componentRow;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new Insets(1, 5, 1, 1);

        if (leftButtonVisible) {
            leftButton.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {

                    leftButton();
                }
            });
            add(leftButton, gridBagConstraints);
        }

        centerButton = new JButton(centerButtonText);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = componentRow;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.CENTER;

        if (centerButtonVisible) {
            centerButton.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {

                    centerButton();
                }
            });
            add(centerButton, gridBagConstraints);
        }

    }

    private GridBagConstraints getPanelGridBagConstraints() {
        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = componentRow;
        gbc.insets = new Insets(1, 1, 1, 1);
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        return gbc;
    }

    private List setSubPanelOrder() {

        final List subPanelList = new ArrayList<SubPanel>();
        if (!ptm.getInputComponents().isEmpty()) {
            subPanelList.add(0, getSubPanel(ptm.getInputComponents(), "Input"));

        } else {
            subPanelList.add(0, null);
        }
        if (!ptm.getOtherComponents().isEmpty()) {
            subPanelList.add(1, getSubPanel(ptm.getOtherComponents(), "Other"));
        } else {
            subPanelList.add(1, null);
        }
        if (!ptm.getOuputComponents().isEmpty()) {
            subPanelList
                    .add(2, getSubPanel(ptm.getOuputComponents(), "Output"));

        } else {
            subPanelList.add(2, null);
        }

        if (isExtraSubPanelPresent == true) {
            if (!ptm.getExtraComponents().isEmpty()) {

                if (position != null) {
                    subPanelList.add(findPosition(),
                            getSubPanel(ptm.getExtraComponents(), ""));
                }
            }
        }

        return subPanelList;
    }

    private int findPosition() {

        switch (position) {
        case INITIAL:
            return 0;
        case AFTER_INPUT:
            return 1;
        case AFTER_OTHER:
            return 2;
        case AFTER_OUTPUT:
            return 3;
        }
        return 0;
    }

    // set rightButton action
    public abstract void rightButton();

    // public abstract void rightButton();

    // set leftButton action
    public abstract void leftButton();

    // set centerButton action
    public abstract void centerButton();

    public InitialDialog getInitialDialog() {
        return initialDialog;
    }

    public void setEnableExecute(boolean enable) {
        rightButton.setEnabled(enable);
    }

    public void setRightButtonText(String rightButtonText) {
        this.rightButtonText = rightButtonText;
        rightButton.setText(rightButtonText);
    }

    public void setCenterButtonText(String centerButtonText) {
        this.centerButtonText = centerButtonText;

        centerButton.setText(centerButtonText);

    }

    public JButton getCenterButton() {
        return centerButton;
    }

    public JButton getRightButton() {
        return rightButton;
    }

    public JButton getLeftButton() {
        return leftButton;
    }

    public void setLeftButtonText(String leftButtonText) {
        this.leftButtonText = leftButtonText;
        leftButton.setText(leftButtonText);
    }

    public void setToolLabel(String toolName) {
        label.setText(toolName);
    }

    @Override
    public void layerablesChanged(Layerable[] layerables) {

        final List<ComponentEntry[]> components_l = ptm.getInputComponents();

        for (final ComponentEntry[] componentEntries : components_l) {

            for (final ComponentEntry componentEntry : componentEntries) {
                if (componentEntry.getComponent() instanceof RasterComboBox) {

                    final JComboBox comboBox = (JComboBox) componentEntry
                            .getComponent();
                    comboBox.removeAllItems();

                    comboBox.addItem("");

                    for (final Layerable layerable : layerables) {
                        if (layerable instanceof RasterImageLayer) {
                            comboBox.addItem(layerable);
                        }
                    }
                    componentEntry.setComponent(comboBox);
                } else if (componentEntry.getComponent() instanceof CustomComboBox.LayerComboBox) {

                    final JComboBox comboBox = (JComboBox) componentEntry
                            .getComponent();
                    comboBox.removeAllItems();

                    comboBox.addItem("");

                    for (final Layerable layerable : layerables) {
                        if (layerable instanceof Layer) {
                            comboBox.addItem(layerable);
                        }
                    }
                    componentEntry.setComponent(comboBox);
                }
            }

        }
    }

    private final ComponentsTreeMap ptm;
    private final boolean leftButtonVisible;
    private final boolean centerButtonVisible;
    private int componentRow;
    private final InitialDialog initialDialog;
    private final int countOptionalButto = 0;
    private final boolean rightButtonVisible;
    // private JPanel subPanel;
    private String rightButtonText = PluginUtils.getResources().getString(
            "MainPanel.ExecuteButton.text");
    private String centerButtonText = PluginUtils.getResources().getString(
            "MainPanel.SaveButton.text");
    private final String centerButtonTooltip = PluginUtils.getResources()
            .getString("MainPanel.SaveButton.tooltip");
    private String leftButtonText = PluginUtils.getResources().getString(
            "MainPanel.LoadButton.text");
    private JButton rightButton;
    private JButton leftButton;
    private JButton centerButton;
    private boolean isExtraSubPanelPresent = false;
    private JLabel label;

    public enum ExtraSubPanelPosition {
        INITIAL, AFTER_INPUT, AFTER_OTHER, AFTER_OUTPUT
    };

    private ExtraSubPanelPosition position = ExtraSubPanelPosition.AFTER_OUTPUT;

    final LayerablesList layerablesList;

}
