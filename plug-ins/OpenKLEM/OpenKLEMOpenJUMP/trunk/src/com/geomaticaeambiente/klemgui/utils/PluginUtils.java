package com.geomaticaeambiente.klemgui.utils;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.io.File;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.openjump.core.rasterimage.RasterImageLayer;

import com.geomaticaeambiente.klemgui.ui.CustomComboBox;
import com.geomaticaeambiente.openjump.klem.rastertools.ReclassTuple;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 * The class contains a variety of methods useful for plugins. For example
 * implements a method for get the rasterImageLayer from a PlugIncontext (OJ).
 * 
 * @author Geomatica
 */
public class PluginUtils {

    /**
     * From Layerable array the method extracts the RasterImageLayer.
     * 
     * @param layerables
     * @return
     */

    public static RasterImageLayer[] getRasterImageLayers(Layerable[] layerables) {

        final List layerList = new ArrayList<Layer>();
        for (final Layerable layerable : layerables) {
            if (layerable instanceof RasterImageLayer) {
                layerList.add(layerable);
            }
        }

        return (RasterImageLayer[]) layerList
                .toArray(new RasterImageLayer[layerList.size()]);
    }

    public static Layerable[] getLayerables(PlugInContext context) {

        final Collection collection = context.getWorkbenchContext()
                .getLayerManager().getLayerables(Layerable.class);
        final Iterator iter = collection.iterator();
        final List<Layerable> layerablesList = new ArrayList<Layerable>();
        while (iter.hasNext()) {
            layerablesList.add((Layerable) iter.next());
        }
        return layerablesList.toArray(new Layerable[layerablesList.size()]);

    }

    public static RasterImageLayer getRasterImageLayerSelected(
            CustomComboBox.RasterComboBox comboBox) {

        return (RasterImageLayer) comboBox.getSelectedItem();
    }

    public static Layer[] getLayers(Layerable[] layerables) {

        final List layerList = new ArrayList<Layer>();
        for (final Layerable layerable : layerables) {
            if (layerable instanceof Layer) {
                layerList.add(layerable);
            }
        }

        return (Layer[]) layerList.toArray(new Layer[layerList.size()]);
    }

    public static Layer getLayerSelected(CustomComboBox.LayerComboBox comboBox) {

        if (comboBox.getSelectedItem().toString().equals("")) {
            return null;
        }

        return (Layer) comboBox.getSelectedItem();

    }

    public static ReclassTuple[] getReclassPairFromString(String values)
            throws NullPointerException {

        final String[] ar_values = values.split(";");

        final String[][] d_ar_values = new String[ar_values.length][];
        for (int n = 0; n < ar_values.length; n++) {
            d_ar_values[n] = ar_values[n].split("_");
        }

        final ReclassTuple[] reclassPair = new ReclassTuple[d_ar_values.length];
        for (int n = 0; n < d_ar_values.length; n++) {
            for (final String d_ar_value : d_ar_values[n]) {

                final double minVal = Double.parseDouble(d_ar_values[n][0]);
                final double maxVal = Double.parseDouble(d_ar_values[n][1]);
                final double newVal = Double.parseDouble(d_ar_values[n][2]);

                reclassPair[n] = new ReclassTuple(minVal, maxVal, newVal);
            }
        }

        return reclassPair;

    }

    private Object makeObj(final String item) {
        return new Object() {
            @Override
            public String toString() {
                return item;
            }
        };
    }

    /**
     * Method to produce the key for the TreeMap as String. To allow that
     * element in the Map will be sort correctly the number of element comes
     * first the word. If the value has only one digit before is added the 0
     * number.
     * 
     * @param word
     * @param value
     * @return
     */
    public static String producerKeyMap(String word, int value) {
        String id;

        final int lengthValue = Integer.toString(value).length();

        if (lengthValue == 1) {
            id = "0" + Integer.toString(value) + word;
        } else {
            id = Integer.toString(value) + word;
        }

        return id;
    }

    public static void setWorkspacePath(File path) {
        workspace = path;
    }

    public static File getWorkspacePath() {
        return workspace;
    }

    public static ImageIcon getFolderIcon() {
        return new ImageIcon(
                PluginUtils.class
                        .getResource("/com/geomaticaeambiente/klemgui/images/folder-horizontal-open_16.png"));
    }

    public static File[] openJChooserDialog(JComponent component, int typeObg,
            int typeDialog, FileNameExtensionFilter ff, File selectedFile,
            boolean multipleSelection) {

        final JFileChooser fileChooser = new JFileChooser();
        fileChooser.setFileSelectionMode(typeObg);
        fileChooser.setDialogType(typeDialog);
        fileChooser.setSelectedFile(selectedFile);
        fileChooser.setMultiSelectionEnabled(multipleSelection);
        // check if workspace is set
        if (lastVisitedFile != null) {
            fileChooser.setCurrentDirectory(lastVisitedFile);
        } else if (PluginUtils.getWorkspacePath() != null) {
            fileChooser.setCurrentDirectory(PluginUtils.getWorkspacePath());
        }

        if (ff != null) {
            fileChooser.setFileFilter(ff);
        }
        int ret = 0;

        if (typeDialog == JFileChooser.OPEN_DIALOG) {
            ret = fileChooser.showOpenDialog(component);
        } else if (typeDialog == JFileChooser.SAVE_DIALOG) {
            ret = fileChooser.showSaveDialog(component);
        }

        if (ret == JFileChooser.APPROVE_OPTION) {
            try {
                if (typeDialog == JFileChooser.SAVE_DIALOG) {
                    final File file = getSelectedFileWithExtension(fileChooser);
                    lastVisitedFile = file;
                    return new File[] { file };
                } else {
                    File[] files;
                    if (multipleSelection) {
                        files = fileChooser.getSelectedFiles();
                    } else {
                        files = new File[] { fileChooser.getSelectedFile() };
                    }
                    lastVisitedFile = files[0];

                    return files;
                }

            } catch (final Exception e) {
                return null;
            }

        }
        return null;

    }

    public static File[] openJChooserDialog2(JComponent component, int typeObg,
            int typeDialog, FileNameExtensionFilter ff, File selectedFile,
            boolean multipleSelection) {

        final JFileChooser fileChooser = new JFileChooser();
        fileChooser.setFileSelectionMode(typeObg);
        fileChooser.setDialogType(typeDialog);
        fileChooser.setSelectedFile(selectedFile);
        fileChooser.setMultiSelectionEnabled(multipleSelection);
        // check if workspace is set
        if (lastVisitedFile != null) {
            fileChooser.setCurrentDirectory(lastVisitedFile);
        } else if (PluginUtils.getWorkspacePath() != null) {
            fileChooser.setCurrentDirectory(PluginUtils.getWorkspacePath());
        }

        if (ff != null) {
            fileChooser.setFileFilter(ff);
        }
        int ret = 0;

        if (typeDialog == JFileChooser.OPEN_DIALOG) {
            ret = fileChooser.showOpenDialog(component);
        } else if (typeDialog == JFileChooser.SAVE_DIALOG) {
            ret = fileChooser.showSaveDialog(component);
        }
        if (ret == JFileChooser.CANCEL_OPTION
                || ret == JFileChooser.ERROR_OPTION) {

            return null;
        }

        if (typeDialog == JFileChooser.SAVE_DIALOG) {
            final File file = getSelectedFileWithExtension(fileChooser);
            lastVisitedFile = file;
            return new File[] { file };
        }
        File[] files;
        if (multipleSelection) {
            files = fileChooser.getSelectedFiles();
        } else {
            files = new File[] { fileChooser.getSelectedFile() };
        }
        lastVisitedFile = files[0];

        return files;
    }

    public static JCheckBox loadRastersCheckBox = null;

    public static File[] openJChooserDialog3(JComponent component, int typeObg,
            int typeDialog, FileNameExtensionFilter ff, File selectedFile,
            boolean multipleSelection) {

        final JFileChooser fileChooser = new JFileChooser();
        fileChooser.setFileSelectionMode(typeObg);
        fileChooser.setDialogType(typeDialog);
        fileChooser.setSelectedFile(selectedFile);
        fileChooser.setMultiSelectionEnabled(multipleSelection);
        /*******/
        final Box box = new Box(BoxLayout.Y_AXIS);
        final JPanel jPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        loadRastersCheckBox = new javax.swing.JCheckBox();
        loadRastersCheckBox.setText(PluginUtils.getResources().getString(
                "HydrographKlemPlugin.LoadFiles.text"));
        jPanel.add(loadRastersCheckBox);
        box.add(jPanel);
        box.add(Box.createRigidArea(new Dimension(5, 180)));
        fileChooser.setAccessory(box);
        /********/
        // check if workspace is set
        if (lastVisitedFile != null) {
            fileChooser.setCurrentDirectory(lastVisitedFile);
        } else if (PluginUtils.getWorkspacePath() != null) {
            fileChooser.setCurrentDirectory(PluginUtils.getWorkspacePath());
        }

        if (ff != null) {
            fileChooser.setFileFilter(ff);
        }
        int ret = 0;

        if (typeDialog == JFileChooser.OPEN_DIALOG) {
            ret = fileChooser.showOpenDialog(component);
        } else if (typeDialog == JFileChooser.SAVE_DIALOG) {
            ret = fileChooser.showSaveDialog(component);
        }

        if (ret == JFileChooser.APPROVE_OPTION) {
            try {
                if (typeDialog == JFileChooser.SAVE_DIALOG) {
                    final File file = getSelectedFileWithExtension(fileChooser);
                    lastVisitedFile = file;
                    return new File[] { file };
                } else {
                    File[] files;
                    if (multipleSelection) {
                        files = fileChooser.getSelectedFiles();
                    } else {
                        files = new File[] { fileChooser.getSelectedFile() };
                    }
                    lastVisitedFile = files[0];

                    return files;
                }

            } catch (final Exception e) {
                return null;
            }

        }
        return null;

    }

    /**
     * Returns the selected file from a JFileChooser, including the extension
     * from the file filter. Code by:
     * http://stackoverflow.com/questions/16846078
     * 
     * @param c
     */
    private static File getSelectedFileWithExtension(JFileChooser c) {

        File file = c.getSelectedFile();
        if (c.getFileFilter() instanceof FileNameExtensionFilter) {
            final String[] exts = ((FileNameExtensionFilter) c.getFileFilter())
                    .getExtensions();
            final String nameLower = file.getName().toLowerCase();
            for (final String ext : exts) { // check if it already has a valid
                                            // extension
                if (nameLower.endsWith('.' + ext.toLowerCase())) {
                    return file; // if yes, return as-is
                }
            }
            // if not, append the first extension from the selected filter
            file = new File(file.toString() + '.' + exts[0]);
        }
        return file;

    }

    private static DecimalFormatSymbols getDecimalFormatSymbols() {
        return new DecimalFormatSymbols(Locale.ENGLISH);
    }

    public static double getOneDecimalFormat(double value) {
        final DecimalFormat oneDecimaFormat = new DecimalFormat("0.0",
                getDecimalFormatSymbols());
        return Double.parseDouble(oneDecimaFormat.format(value));

    }

    public static double getTwoDecimalFormat(double value) {
        final DecimalFormat oneDecimaFormat = new DecimalFormat("0.00",
                getDecimalFormatSymbols());
        return Double.parseDouble(oneDecimaFormat.format(value));
    }

    public static double getThreeDecimalFormat(double value) {
        final DecimalFormat oneDecimaFormat = new DecimalFormat("0.000",
                getDecimalFormatSymbols());
        return Double.parseDouble(oneDecimaFormat.format(value));
    }

    public static double getFourDecimalFormat(double value) {
        final DecimalFormat oneDecimaFormat = new DecimalFormat("0.0000",
                getDecimalFormatSymbols());
        return Double.parseDouble(oneDecimaFormat.format(value));
    }

    public static String getOneDecimalFormatToString(double value) {
        final DecimalFormat oneDecimaFormat = new DecimalFormat("0.0",
                getDecimalFormatSymbols());
        return oneDecimaFormat.format(value);

    }

    public static String getTwoDecimalFormatToString(double value) {
        final DecimalFormat oneDecimaFormat = new DecimalFormat("0.00",
                getDecimalFormatSymbols());
        return oneDecimaFormat.format(value);
    }

    public static String getThreeDecimalFormatToString(double value) {
        final DecimalFormat oneDecimaFormat = new DecimalFormat("0.000",
                getDecimalFormatSymbols());
        return oneDecimaFormat.format(value);
    }

    public static String getFourDecimalFormatToString(double value) {
        final DecimalFormat oneDecimaFormat = new DecimalFormat("0.0000",
                getDecimalFormatSymbols());
        return oneDecimaFormat.format(value);
    }

    public static String getSixDecimalFormatToString(double value) {
        final DecimalFormat sixDecimaFormat = new DecimalFormat("0.000000",
                getDecimalFormatSymbols());
        return sixDecimaFormat.format(value);
    }

    public static ResourceBundle getResources() {
        return java.util.ResourceBundle
                .getBundle("com/geomaticaeambiente/klemgui/resources/Bundle");
    }

    private static File workspace;
    private static File lastVisitedFile;
    public static String plugInName = getResources().getString("OpenKLEM.name");
    public static String version = getResources().getString("OpenKlem.version");
    public static String versionNumber = getResources().getString(
            "OpenKlem.version-number");

}