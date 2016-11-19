package es.unex.sextante.gui.settings;

import info.clearthought.layout.TableLayout;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.algorithm.FileSelectionPanel;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.exceptions.WrongSettingValuesException;
import es.unex.sextante.gui.toolbox.AlgorithmGroupConfiguration;
import es.unex.sextante.gui.toolbox.AlgorithmGroupsOrganizer;

public class SextanteGeneralSettingsPanel extends SettingPanel {

    private JCheckBox jCheckBoxChangeNames;
    private JLabel jLabelResults;
    private FileSelectionPanel jFolderResults;
    private JLabel jLabelNoDataValue;
    private JLabel jLabelToolboxSettings;
    private JLabel jLabelConfigPath;
    private JTextField jTextFieldNoData;
    private JCheckBox jCheckBoxUseInternalNames;
    private JButton jButtonConfigureGroups;
    private JCheckBox jCheckBoxShowMostRecent;
    private JCheckBox jCheckBoxMonitorCloseByDefault;
    private JCheckBox jCheckBoxMonitorDetailsByDefault;

    @Override
    protected void initGUI() {

        final TableLayout thisLayout = new TableLayout(new double[][] {
                { SextanteConfigurationDialog.SPACER_SMALL,
                        TableLayout.MINIMUM, TableLayout.FILL,
                        SextanteConfigurationDialog.SPACER_SMALL },
                { SextanteConfigurationDialog.SPACER_SMALL, // row 0 (spacer)
                        TableLayout.MINIMUM, // row 1
                        TableLayout.MINIMUM, // row 2
                        TableLayout.MINIMUM, // row 3
                        TableLayout.MINIMUM, // row 4
                        TableLayout.MINIMUM, // row 5
                        TableLayout.MINIMUM, // row 6
                        TableLayout.MINIMUM, // row 7
                        TableLayout.MINIMUM, // row 8
                        TableLayout.MINIMUM, // row 9
                        TableLayout.MINIMUM, // row 10
                        TableLayout.MINIMUM, // row 11
                        TableLayout.FILL, // row 12
                        SextanteConfigurationDialog.SPACER_SMALL } }); // row 13
        thisLayout.setHGap(5);
        thisLayout.setVGap(5);
        this.setLayout(thisLayout);
        {
            final boolean bUseInternalNames = new Boolean(
                    SextanteGUI
                            .getSettingParameterValue(SextanteGeneralSettings.USE_INTERNAL_NAMES))
                    .booleanValue();
            final boolean bModiFyResultsNames = new Boolean(
                    SextanteGUI
                            .getSettingParameterValue(SextanteGeneralSettings.MODIFY_NAMES))
                    .booleanValue();
            final boolean bShowMostRecent = new Boolean(
                    SextanteGUI
                            .getSettingParameterValue(SextanteGeneralSettings.SHOW_MOST_RECENT))
                    .booleanValue();
            final boolean bMonitorCloseByDefault = new Boolean(
                    SextanteGUI
                            .getSettingParameterValue(SextanteGeneralSettings.MONITOR_CLOSE_BY_DEFAULT))
                    .booleanValue();
            final boolean bMonitorDetailsByDefault = new Boolean(
                    SextanteGUI
                            .getSettingParameterValue(SextanteGeneralSettings.MONITOR_DETAILS_BY_DEFAULT))
                    .booleanValue();

            jLabelResults = new JLabel();
            this.add(jLabelResults, "1, 1");
            jLabelResults.setText(Sextante.getText("Output_folder"));
            jFolderResults = new FileSelectionPanel(true, true,
                    (String[]) null, Sextante.getText("selector_choose_folder"));
            this.add(jFolderResults, "2,1");
            jFolderResults.setFilepath(SextanteGUI.getOutputFolder());

            jCheckBoxChangeNames = new JCheckBox();
            jCheckBoxChangeNames.setText(Sextante
                    .getText("Modify_output_names"));
            jCheckBoxChangeNames.setSelected(bModiFyResultsNames);
            this.add(jCheckBoxChangeNames, "1, 2, 2, 2");

            jCheckBoxUseInternalNames = new JCheckBox();
            jCheckBoxUseInternalNames.setText(Sextante
                    .getText("Use_internal_names_for_outputs"));
            jCheckBoxUseInternalNames.setSelected(bUseInternalNames);
            this.add(jCheckBoxUseInternalNames, "1, 3, 2, 3");

            this.add(new JSeparator(SwingConstants.HORIZONTAL), "1, 4, 2, 4");
            /* ----------------------------------------------------------- */

            jLabelNoDataValue = new JLabel();
            jLabelNoDataValue
                    .setText(Sextante.getText("Default_no_data_value"));
            this.add(jLabelNoDataValue, "1, 5");
            jTextFieldNoData = new JTextField();
            final String sNoDataValue = Double.toString(SextanteGUI
                    .getOutputFactory().getDefaultNoDataValue());
            jTextFieldNoData.setText(sNoDataValue);
            this.add(jTextFieldNoData, "2, 5");

            this.add(new JSeparator(SwingConstants.HORIZONTAL), "1, 6, 2, 6");
            /* ----------------------------------------------------------- */

            jButtonConfigureGroups = new JButton(
                    Sextante.getText("ConfigureAlgGroups") + "...");
            jButtonConfigureGroups.addActionListener(new ActionListener() {
                public void actionPerformed(final ActionEvent arg0) {
                    configureGroups();
                }
            });
            this.add(jButtonConfigureGroups, "2, 7, 2, 7");
            jLabelToolboxSettings = new JLabel();
            jLabelToolboxSettings.setText(Sextante.getText("SEXTANTE_toolbox"));
            this.add(jLabelToolboxSettings, "1, 7");

            jCheckBoxShowMostRecent = new JCheckBox(
                    Sextante.getText("ShowMostRecent"));
            jCheckBoxShowMostRecent.setSelected(bShowMostRecent);
            this.add(jCheckBoxShowMostRecent, "1, 8, 2, 8");

            this.add(new JSeparator(SwingConstants.HORIZONTAL), "1, 9, 2, 9");
            /* ----------------------------------------------------------- */

            jCheckBoxMonitorCloseByDefault = new JCheckBox(
                    Sextante.getText("MonitorCloseByDefault"));
            jCheckBoxMonitorCloseByDefault.setSelected(bMonitorCloseByDefault);
            this.add(jCheckBoxMonitorCloseByDefault, "1, 10, 2, 10");

            jCheckBoxMonitorDetailsByDefault = new JCheckBox(
                    Sextante.getText("MonitorDetailsByDefault"));
            jCheckBoxMonitorDetailsByDefault
                    .setSelected(bMonitorDetailsByDefault);
            this.add(jCheckBoxMonitorDetailsByDefault, "1, 11, 2, 11");

            jLabelConfigPath = new JLabel();
            jLabelConfigPath.setText("<html><i>"
                    + Sextante.getText("Config_path_label") + " "
                    + Sextante.getUserFolder() + "</i></html>");

            this.add(jLabelConfigPath, "1, 12");
        }

    }

    protected void configureGroups() {

        final AlgorithmGroupsConfigurationDialog dialog = new AlgorithmGroupsConfigurationDialog();
        dialog.setVisible(true);

        final HashMap<String, AlgorithmGroupConfiguration> map = dialog
                .getGrouppingsMap();
        if (map != null) {
            AlgorithmGroupsOrganizer.setConfiguration(map);
            AlgorithmGroupsOrganizer.saveSettings();
        }

    }

    @Override
    public HashMap<String, String> getValues()
            throws WrongSettingValuesException {

        final HashMap<String, String> map = new HashMap<String, String>();
        String path = jFolderResults.getFilepath();
        if (path != null) {
            map.put(SextanteGeneralSettings.RESULTS_FOLDER, path);
        }
        map.put(SextanteGeneralSettings.MODIFY_NAMES, new Boolean(
                jCheckBoxChangeNames.isSelected()).toString());
        map.put(SextanteGeneralSettings.USE_INTERNAL_NAMES, new Boolean(
                jCheckBoxUseInternalNames.isSelected()).toString());
        map.put(SextanteGeneralSettings.SHOW_MOST_RECENT, new Boolean(
                jCheckBoxShowMostRecent.isSelected()).toString());
        map.put(SextanteGeneralSettings.MONITOR_CLOSE_BY_DEFAULT, new Boolean(
                jCheckBoxMonitorCloseByDefault.isSelected()).toString());
        map.put(SextanteGeneralSettings.MONITOR_DETAILS_BY_DEFAULT,
                new Boolean(jCheckBoxMonitorDetailsByDefault.isSelected())
                        .toString());
        try {
            final double dValue = Double
                    .parseDouble(jTextFieldNoData.getText());
            SextanteGUI.getOutputFactory().setDefaultNoDataValue(dValue);
        } catch (final Exception e) {
            throw new WrongSettingValuesException();
        }
        map.put(SextanteGeneralSettings.DEFAULT_NO_DATA_VALUE,
                jTextFieldNoData.getText());

        return map;

    }

}
