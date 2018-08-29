package com.geomaticaeambiente.klemgui.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableModel;

import org.openjump.core.rasterimage.RasterImageLayer;

import com.geomaticaeambiente.klemgui.exceptions.WarningException;
import com.geomaticaeambiente.klemgui.utils.CommonHydrographData;
import com.geomaticaeambiente.klemgui.utils.ComponentsTreeMap;
import com.geomaticaeambiente.klemgui.utils.PluginUtils;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 * Class that contains methods useful for the GUI of the plugins.
 * 
 * @author Geomatica
 */

public class GUIUtils {

    /**
     * Method to add rows to table.
     * 
     * @param dtm
     * @param table
     */
    protected static void addRows(DefaultTableModel dtm, JTable table) {
        final int selRows[] = table.getSelectedRows();
        if (selRows.length > 0) {
            dtm.insertRow(selRows[0], new Object[] { null, null, null });
        } else {
            dtm.addRow(new Object[] { null, null, null });
            // select the last row
            final int lastRow = table.getRowCount() - 1;
            table.getSelectionModel().setSelectionInterval(lastRow, lastRow);
            table.scrollRectToVisible(table.getCellRect(lastRow, 1, true));
        }
    }

    /**
     * Method to remove row from a table.
     * 
     * @param dtm
     * @param table
     */
    protected static void removeRow(DefaultTableModel dtm, JTable table) {

        final int selRows[] = table.getSelectedRows();
        for (int i = selRows.length - 1; i >= 0; --i) {
            dtm.removeRow(selRows[i]);
        }
    }

    public static JComponent getComponent(Object object) {

        if (object instanceof RasterImageLayer) {

            final RasterImageLayer ril = (RasterImageLayer) object;
            final JTextField field = new JTextField(ril.getName());

            return field;

        } else if (object instanceof Double || object instanceof Float
                || object instanceof Integer || object instanceof String) {
            Object value = null;

            if (object instanceof Double) {
                value = object;
            } else if (object instanceof Float) {
                value = object;
            } else if (object instanceof Integer) {
                value = object;
            } else if (object instanceof String) {
                value = object;
            }

            final JTextField field = new JTextField(value.toString());
            return field;

        } else if (object instanceof File) {
            final File file = (File) object;

            final JTextField field = new JTextField(file.getAbsolutePath());
            return field;

        } else if (object instanceof Layer) {

            final Layer layer = (Layer) object;
            final JTextField field = new JTextField(layer.getName());
            return field;

        } else if (object instanceof RasterImageLayer[]) {
            final RasterImageLayer[] objects = (RasterImageLayer[]) object;
            final CustomComboBox customCBox = new CustomComboBox();

            final CustomComboBox.RasterComboBox comboBox2 = customCBox.new RasterComboBox(
                    objects);
            comboBox2.setBorder(javax.swing.BorderFactory.createEtchedBorder());

            return comboBox2;

        } else if (object instanceof Layer[]) {
            final Layer[] objects = (Layer[]) object;
            final CustomComboBox customCBox = new CustomComboBox();

            final CustomComboBox.LayerComboBox comboBox2 = customCBox.new LayerComboBox(
                    objects);
            comboBox2.setBorder(javax.swing.BorderFactory.createEtchedBorder());

            return comboBox2;
        } else if (object instanceof Object[]) {
            final Object[] objects = (Object[]) object;
            final JComboBox comboBox = new JComboBox();
            comboBox.setBorder(javax.swing.BorderFactory.createEtchedBorder());
            comboBox.addItem("");
            for (final Object object2 : objects) {
                comboBox.addItem(object2);
            }

            return comboBox;
        }

        return null;
    }

    /**
     * Method to get the String from a component.
     * 
     * @param component
     * @return For JTextField return the text in textField, for JComboBox return
     *         the selected item as String, for label return the label text.
     */
    public static String getStringValue(JComponent component)
            throws NumberFormatException {

        if (component instanceof JTextField) {
            final JTextField textField = (JTextField) component;
            return textField.getText();
        } else if (component instanceof JLabel) {
            final JLabel label = (JLabel) component;
            return label.getText();
        } else if (component instanceof JComboBox) {
            final JComboBox comboBox = (JComboBox) component;
            return comboBox.getSelectedItem().toString();
        } else if (component instanceof PersonalTableComponents) {
            final PersonalTableComponents personalTable = (PersonalTableComponents) component;
            final JTable table = personalTable.getTabel();
            if (table.isEditing()) {
                table.getCellEditor().stopCellEditing();
            }
            // table.getCellEditor().stopCellEditing();
            // Write table values ad xxxxxx_xxxxxx;
            // underscore to separate values in the same row;
            // semicolon to separate values in differemt rows;
            String values = "";

            for (int r = 0; r < table.getRowCount(); r++) {
                for (int c = 0; c < table.getColumnCount(); c++) {
                    if (table.getValueAt(r, c) == null) {
                        throw new NullPointerException(
                                PluginUtils
                                        .getResources()
                                        .getString(
                                                "GUIUtils.CheckTableValues.message")
                                        .concat(Integer.toString(r + 2))
                                        .concat(PluginUtils
                                                .getResources()
                                                .getString(
                                                        "GUIUtils.CheckTableValuesColumn.message"))
                                        .concat(Integer.toString(c + 1)));
                    }

                    values = values.concat(table.getValueAt(r, c).toString());

                    // if c is a even number add _
                    // if(c%2 == 0)
                    values = values.concat("_");
                }
                values = values.concat(";");
            }
            // System.out.println(values);
            return values;
        }

        return null;
    }

    /**
     * Method to extract the information about raster selected (raster which are
     * present in the expression value) in PersonalRasterCombPanel.
     * 
     * @param component
     *            PersonalRasterCombPanel
     * @return an array of boolean values, true the raster in posistion n appear
     *         in the expression
     */

    public static boolean[] getSelectedRasterFromRasterCombo(
            JComponent component) {

        if (component instanceof PersonalRasterCombPanel) {
            final PersonalRasterCombPanel panel = (PersonalRasterCombPanel) component;
            return panel.getSelRaster();
        }
        return null;
    }

    /**
     * Method to extract the expression from PersonalRasterCombPanel.
     * 
     * @param component
     *            PersonalRasterCombPanel
     * @return a String with the combination of raster
     */
    public static String getExprerssionFromRasterCombo(JComponent component) {

        if (component instanceof PersonalRasterCombPanel) {
            final PersonalRasterCombPanel panel = (PersonalRasterCombPanel) component;
            return panel.getExpression();
        }
        return null;
    }

    public static boolean componentIsSelected(JComponent component)
            throws Exception {

        final JToggleButton toggleButton = (JToggleButton) component;
        return toggleButton.isSelected();

    }

    // public static File loadFile(JComponent component, String extension,
    // String description){
    //
    // //Load file
    // JFileChooser tableChooser = new JFileChooser();
    // tableChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    // tableChooser.setDialogType(JFileChooser.OPEN_DIALOG);
    // FileFilter fileFilter = GUIUtils.setFileFilter(extension, description);
    // tableChooser.setFileFilter((javax.swing.filechooser.FileFilter)
    // fileFilter);
    // int ret = tableChooser.showOpenDialog(component);
    // if (ret == JFileChooser.APPROVE_OPTION) {
    // return tableChooser.getSelectedFile();
    // }
    //
    // return null;
    // }

    public static double getDoubleValue(JComponent component)
            throws NumberFormatException {

        final String value = getStringValue(component);

        return Double.parseDouble(value);

    }

    public static int getIntValue(JComponent component)
            throws NumberFormatException {

        final String value = getStringValue(component);

        return Integer.parseInt(value);

    }

    public static Boolean getBooleanValue(JComponent component) {

        if (component instanceof JCheckBox) {

            final JCheckBox checkBox = (JCheckBox) component;
            return checkBox.isSelected();

        }

        return null;

    }

    public static CommonHydrographData getCommonHydroDataFromPanel(
            CommonHydroPanel hydroPanel) throws Exception {

        return hydroPanel.getPersonalCommonHydrographData();
    }

    // public static File saveFile(JComponent component, String extension,
    // String description){
    //
    // //Save file
    // JFileChooser tableChooser = new JFileChooser();
    // tableChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    // tableChooser.setDialogType(JFileChooser.SAVE_DIALOG);
    // FileFilter fileFilter = setFileFilter(extension, description);
    // tableChooser.setFileFilter(fileFilter);
    // int ret = tableChooser.showSaveDialog(component);
    // if (ret == JFileChooser.APPROVE_OPTION) {
    // return tableChooser.getSelectedFile();
    // }
    // return null;
    //
    // }

    // private static FileFilter setFileFilter(final String extension, final
    // String description){
    //
    // FileFilter fileFilter = new FileFilter() {
    //
    // @Override
    // public String getDescription() {
    // return description;
    // }
    //
    // @Override
    // public boolean accept(File f) {
    //
    // if(f.toString().toLowerCase().endsWith(extension.toLowerCase())){
    // return true;
    // }
    //
    // return false;
    // }
    // };
    // return fileFilter;
    // }
    //
    public static int getSelectedButton(JComponent component) {

        if (component instanceof RadioButtonsPanel) {
            final RadioButtonsPanel panel = (RadioButtonsPanel) component;
            return panel.getSelectedIndex();
        }
        return -1;
    }

    public static int getSelectedJRadioButton(JRadioButton... radioButton) {

        for (int n = 0; n < radioButton.length; n++) {
            if (radioButton[n].isSelected()) {
                return n;
            }
        }
        return -1;
    }

    public static void checkStringValue(String stringValue, String field)
            throws Exception {
        if (stringValue == null || stringValue.isEmpty()) {
            throw new WarningException(PluginUtils.getResources()
                    .getString("Check.CheckStringValue.message").concat(field));
        }
    }

    // check if the String can be a file
    public static void checkFileValue(String stringValue, String field)
            throws Exception {

        final File file = new File(stringValue);
        if (file.getParent() == null) {
            throw new WarningException(PluginUtils.getResources()
                    .getString("Check.CheckStringValue.message").concat(field));
        }

    }

    public static void checkIntegerValue(String stringValue, String field)
            throws NumberFormatException {

        final int intValue = Integer.parseInt(stringValue);

    }

    public static void checkDoubleValue(String stringValue, String field)
            throws NumberFormatException {

        final double doubleValue = Double.parseDouble(stringValue);

    }

    public static void checkDoublePosValue(String stringValue, String field)
            throws NumberFormatException {

        final double doubleValue = Double.parseDouble(stringValue);
        if (doubleValue < 0) {
            throw new NumberFormatException();
        }

    }

    public static ActionListener setSaveFileAction(final JTextField field,
            final FileNameExtensionFilter fileFilter) {

        final ActionListener actionListener = new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {

                final File outputFiles[] = PluginUtils.openJChooserDialog(
                        field, JFileChooser.FILES_ONLY,
                        JFileChooser.SAVE_DIALOG, fileFilter, null, false);

                if (outputFiles != null && outputFiles.length > 0) {
                    field.setText(outputFiles[0].getAbsolutePath());
                }
            }
        };
        return actionListener;
    }

    public static ActionListener setSaveRasterTif(JTextField field) {

        final FileNameExtensionFilter fileFilter = new FileNameExtensionFilter(
                "Tiff raster (*.tif)", new String[] { "tif" });
        return setSaveFileAction(field, fileFilter);
    }

    public static ActionListener setSaveFile(JTextField field,
            String description, String extension) {

        FileNameExtensionFilter fileFilter = null;
        if (extension != null) {
            fileFilter = new FileNameExtensionFilter(description + " (*."
                    + extension + ")", new String[] { extension });
        }

        return setSaveFileAction(field, fileFilter);
    }

    public static void deleteFolder(File file) {

        final String[] list = file.list();
        if (list != null) {
            for (final String element : list) {
                final File currentFile = new File(file, element);
                if (currentFile.isDirectory()) {
                    deleteFolder(currentFile);
                } else {
                    currentFile.delete();
                }
            }
        } else {
            file.delete();
        }

    }

    public static void setVisibleComponents(boolean value,
            JComponent[]... components) {

        for (final JComponent[] components_ : components) {
            for (final JComponent element : components_) {
                element.setVisible(value);
            }
        }
    }

    public static void setEnableComponents(boolean value,
            JComponent[]... components) {

        for (final JComponent[] components_ : components) {
            for (final JComponent element : components_) {
                element.setEnabled(value);
            }
        }
    }

    public static JComponent[] getJComponentFromRowRange(
            ComponentsTreeMap personalTreeMap, int startRow, int endRow,
            String type) {

        final List components_al = new ArrayList<JComponent[]>();

        for (int n = startRow; n <= endRow; n++) {

            String index = Integer.toString(n);
            if (index.length() == 1) {
                index = "0" + Integer.toString(n);
            } else {
                index = Integer.toString(n);
            }

            components_al.add(personalTreeMap.getLineComponents(index, type));
        }

        final List allComponents_al = new ArrayList<JComponent[]>();
        for (int m = 0; m < components_al.size(); m++) {
            final int length = ((JComponent[]) components_al.get(m)).length;

            for (int c = 0; c < length; c++) {
                allComponents_al.add(((JComponent[]) components_al.get(m))[c]);
            }
        }

        final JComponent[] components_ar = (JComponent[]) allComponents_al
                .toArray(new JComponent[allComponents_al.size()]);

        return components_ar;
    }

    public static void setExclamationMark(JLabel... labels) {

        for (final JLabel label : labels) {
            label.setIcon(new ImageIcon(
                    GUIUtils.class
                            .getResource("/com/geomaticaeambiente/klemgui/images/ExclamationMark.png")));
            label.setToolTipText(PluginUtils.getResources().getString(
                    "HydrographKlemPlugin.QuestionMark.Tooltip"));
            label.setVisible(false);
        }
    }

    public static void setErrorExclamationMark(String string, JLabel... labels) {

        for (final JLabel label : labels) {
            label.setIcon(new ImageIcon(
                    GUIUtils.class
                            .getResource("/com/geomaticaeambiente/klemgui/images/ExclamationMark.png")));
            label.setToolTipText(string);
            label.setVisible(false);
        }
    }

    /**
     * Method to set visible or the label (es: exclamation mark) when the text
     * of textfield is different from the value of check.
     * 
     * @param loadProject
     * @param textField
     *            text to check
     * @param check
     *            value that the texfield should have
     * @param label
     *            label to display
     */

    public static void setJTextAction(boolean loadProject,
            final JTextField textField, final Double check, final JLabel label) {

        if (loadProject == true) {
            textField.getDocument().addDocumentListener(new DocumentListener() {

                @Override
                public void insertUpdate(DocumentEvent e) {
                    // if the project has a value check the value
                    if (check != null) {

                        try {
                            final double val = Double.parseDouble(textField
                                    .getText());
                            if (val != check) {
                                label.setVisible(true);
                            } else {
                                label.setVisible(false);
                            }
                        } catch (final Exception ex) {
                            label.setVisible(true);
                        }

                        // else each alteration will be marked
                    } else {
                        label.setVisible(true);
                    }
                }

                @Override
                public void removeUpdate(DocumentEvent e) {
                }

                @Override
                public void changedUpdate(DocumentEvent e) {
                }
            });
        }

    }

    public static final String INPUT = "Input";
    public static final String OUTPUT = "Output";
    public static final String OTHER = "Other";
    public static final String EXTRA = "Extra";

    public static final int SUB_PANEL_WIDTH = 450; // max width of sub panel

    // protected static JComboBox createJComboBoxPopulated(Object[] object){
    //
    // JComboBox comboBox = new JComboBox();
    // comboBox.setModel(setComboBoxModel(object));
    //
    // return comboBox;
    // }
    //
    // protected static ComboBoxModel setComboBoxModel(Object[] objects){
    //
    // if(objects instanceof String[]){//vedere se accetta anche float o
    // double...
    //
    // return new DefaultComboBoxModel(objects);
    //
    // } else {
    //
    // String[] elements = new String[objects.length];
    // for(int n=0; n<objects.length; n++){
    //
    // if(objects instanceof Double[] || objects instanceof Float[] ||
    // objects instanceof Integer[]){
    // elements[n] = objects[n].toString();
    //
    // } else if (objects instanceof RasterImageLayer[]){
    // RasterImageLayer ril = (RasterImageLayer) objects[n];
    // elements[n] = ril.getImageFileName();
    // } else if (objects instanceof Layer[]){
    // Layer layer = (Layer) objects[n];
    // elements[n] = layer.getName();
    // }
    //
    // }
    // return new DefaultComboBoxModel(elements);
    // }
    // }

    public static String setGUILabel(String label) {
        return label + ": ";
    }

    public static String setLeftSpace(String label) {
        return "    " + label;
    }

    public static String getOutputVectorLabel() {
        return GUIUtils.setGUILabel(VECTOR_OUT_LABEL);
    }

    public static String getOutputRasterLabel() {
        return GUIUtils.setGUILabel(RASTER_OUT_LABEL);
    }

    public static String getOutputRasterString() {
        return RASTER_OUT_LABEL;
    }

    public static void listLayerAttribute(PlugInContext context,
            JComboBox jComboBox_attribute, JComboBox jComboBox_layer) {

        if (jComboBox_layer.getSelectedItem() != null
                && !jComboBox_layer.getSelectedItem().toString().equals("")) {

            final Layer layer = context
                    .getWorkbenchContext()
                    .getLayerManager()
                    .getLayer(
                            ((Layer) jComboBox_layer.getSelectedItem())
                                    .getName());

            jComboBox_attribute.removeAllItems();
            for (int l = 0; l < layer.getFeatureCollectionWrapper()
                    .getFeatureSchema().getAttributeCount(); l++) {
                if (layer.getFeatureCollectionWrapper().getFeatureSchema()
                        .getAttributeType(l).equals(AttributeType.INTEGER)
                        || layer.getFeatureCollectionWrapper()
                                .getFeatureSchema().getAttributeType(l)
                                .equals(AttributeType.LONG)
                        || layer.getFeatureCollectionWrapper()
                                .getFeatureSchema().getAttributeType(l)
                                .equals(AttributeType.FLOAT)
                        || layer.getFeatureCollectionWrapper()
                                .getFeatureSchema().getAttributeType(l)
                                .equals(AttributeType.DOUBLE)) {
                    jComboBox_attribute.addItem(makeObj(layer
                            .getFeatureCollectionWrapper().getFeatureSchema()
                            .getAttributeName(l)));
                }
            }
        }

    }

    private static Object makeObj(final String item) {
        return new Object() {
            @Override
            public String toString() {
                return item;
            }
        };
    }

    private static final String RASTER_OUT_LABEL = PluginUtils.getResources()
            .getString("KlemGUI.OutputRaster.label");
    private static final String VECTOR_OUT_LABEL = PluginUtils.getResources()
            .getString("KlemGUI.OutputVector.label");

}
