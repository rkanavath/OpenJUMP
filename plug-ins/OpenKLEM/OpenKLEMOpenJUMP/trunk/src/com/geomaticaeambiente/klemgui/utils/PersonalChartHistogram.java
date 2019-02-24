package com.geomaticaeambiente.klemgui.utils;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Paint;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.CategoryAxis;
import org.jfree.chart.axis.CategoryLabelPositions;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.category.BarRenderer;
import org.jfree.chart.renderer.category.StandardBarPainter;
import org.jfree.data.category.DefaultCategoryDataset;

import com.geomaticaeambiente.openjump.klem.cn.ValuesRange;
import com.geomaticaeambiente.openjump.klem.rastertools.HistogramCalculator.Histogram;
import com.vividsolutions.jump.util.StringUtil;
import com.vividsolutions.jump.workbench.ui.ErrorDialog;

/**
 *
 * @author deluca
 */
public class PersonalChartHistogram {

    public JPanel buildHistogramPanel(final JComponent parent,
            Histogram histogram) {

        // Build graph ---------------------------------------------------------
        final DefaultCategoryDataset dataset = new DefaultCategoryDataset();

        final LinkedHashMap<ValuesRange, Double> values_hm = histogram
                .getAreaValues();
        final String series = "Area";

        // Dataset and data array for csv output
        final String[][] data = new String[values_hm.size() + 1][2];
        data[0][0] = "X";
        data[0][1] = "Y";
        int row = 1;

        double firstMaxVal = ((ValuesRange) values_hm.keySet().toArray()[0])
                .getMaxValue();

        String zeros = "0";

        if (firstMaxVal > 0 && firstMaxVal < 1) {

            zeros = zeros.concat(".");
            while (firstMaxVal < 1) {
                firstMaxVal *= 10;
                zeros = zeros.concat("0");
            }

        }

        for (final Map.Entry<ValuesRange, Double> entry : values_hm.entrySet()) {

            final double minVal = entry.getKey().getMinValue();
            final double maxVal = entry.getKey().getMaxValue();

            final DecimalFormat decForm = new DecimalFormat(zeros, decForSymb);

            final String minValS = decForm.format(minVal);
            final String maxValS = decForm.format(maxVal);

            final double val = entry.getValue() / 1E6;
            String label;

            data[row][0] = Double.toString(val);
            if (!histogram.isUniqueVals()) {
                label = minValS + "-" + maxValS;
            } else {
                label = minValS;
            }

            dataset.addValue(val, series, label);
            data[row][1] = label;

            row++;
        }

        final JFreeChart chart = ChartFactory.createBarChart(null, null, "km2",
                dataset, PlotOrientation.VERTICAL, false, true, false);

        // Rendering
        final CategoryPlot plot = chart.getCategoryPlot();
        final Paint backgroundPaint = new Color(128, 128, 128);
        plot.setBackgroundPaint(backgroundPaint);

        final BarRenderer barRenderer = (BarRenderer) plot.getRenderer();
        barRenderer.setBarPainter(new StandardBarPainter());
        barRenderer.setShadowVisible(false);

        barRenderer.setDrawBarOutline(false);

        final Paint bluePaint = new Color(64, 64, 255);
        barRenderer.setSeriesPaint(0, bluePaint);

        final Paint outlinePaint = new Color(0, 0, 0);
        barRenderer.setSeriesOutlinePaint(0, outlinePaint);
        barRenderer.setSeriesOutlineStroke(0, new BasicStroke(1));

        final CategoryAxis catAxis = plot.getDomainAxis();
        catAxis.setCategoryLabelPositions(CategoryLabelPositions
                .createUpRotationLabelPositions(Math.toRadians(45)));

        plot.setRenderer(barRenderer);

        // Chart panel
        final ChartPanel chartPanel = new ChartPanel(chart);

        // Stats panel
        final JPanel bottomPanel = new JPanel(new GridBagLayout());

        // Min
        final GridBagConstraints min_const_l = new GridBagConstraints();
        min_const_l.gridx = 0;
        min_const_l.gridy = 0;
        min_const_l.weightx = 0.1;
        final JLabel min_l = new JLabel(PluginUtils.getResources().getString(
                "ChartHistogram.labels.min"));
        bottomPanel.add(min_l, min_const_l);

        final GridBagConstraints min_const_s = new GridBagConstraints();
        min_const_s.gridx = 1;
        min_const_s.gridy = 0;
        min_const_s.weightx = 0.1;
        final JLabel min_s = new JLabel(createLabel(histogram.getMin(),
                decForm00));
        bottomPanel.add(min_s, min_const_s);

        // Max
        final GridBagConstraints max_const_l = new GridBagConstraints();
        max_const_l.gridx = 0;
        max_const_l.gridy = 1;
        max_const_l.weightx = 0.1;
        final JLabel max_l = new JLabel(PluginUtils.getResources().getString(
                "ChartHistogram.labels.max"));
        bottomPanel.add(max_l, max_const_l);

        final GridBagConstraints max_const_s = new GridBagConstraints();
        max_const_s.gridx = 1;
        max_const_s.gridy = 1;
        max_const_s.weightx = 0.1;
        final JLabel max_s = new JLabel(createLabel(histogram.getMax(),
                decForm00));
        bottomPanel.add(max_s, max_const_s);

        // Mean
        final GridBagConstraints mean_const_l = new GridBagConstraints();
        mean_const_l.gridx = 0;
        mean_const_l.gridy = 2;
        mean_const_l.weightx = 0.1;
        final JLabel mean_l = new JLabel(PluginUtils.getResources().getString(
                "ChartHistogram.labels.mean"));
        bottomPanel.add(mean_l, mean_const_l);

        final GridBagConstraints mean_const_s = new GridBagConstraints();
        mean_const_s.gridx = 1;
        mean_const_s.gridy = 2;
        mean_const_s.weightx = 0.1;
        final JLabel mean_s = new JLabel(createLabel(histogram.getMean(),
                decForm00));
        bottomPanel.add(mean_s, mean_const_s);

        // StDev
        final GridBagConstraints stDev_const_l = new GridBagConstraints();
        stDev_const_l.gridx = 2;
        stDev_const_l.gridy = 0;
        stDev_const_l.weightx = 0.1;
        final JLabel stDev_l = new JLabel(PluginUtils.getResources().getString(
                "ChartHistogram.labels.stdDev"));
        bottomPanel.add(stDev_l, stDev_const_l);

        final GridBagConstraints stDev_const_s = new GridBagConstraints();
        stDev_const_s.gridx = 3;
        stDev_const_s.gridy = 0;
        stDev_const_s.weightx = 0.1;
        final JLabel stDev_s = new JLabel(createLabel(histogram.getStdDev(),
                decForm00));
        bottomPanel.add(stDev_s, stDev_const_s);

        // Sum
        final GridBagConstraints sum_const_l = new GridBagConstraints();
        sum_const_l.gridx = 2;
        sum_const_l.gridy = 1;
        sum_const_l.weightx = 0.1;
        final JLabel sum_l = new JLabel(PluginUtils.getResources().getString(
                "ChartHistogram.labels.sum"));
        bottomPanel.add(sum_l, sum_const_l);

        final GridBagConstraints sum_const_s = new GridBagConstraints();
        sum_const_s.gridx = 3;
        sum_const_s.gridy = 1;
        sum_const_s.weightx = 0.1;
        final JLabel sum_s = new JLabel(createLabel(histogram.getSum(),
                decForm00));
        bottomPanel.add(sum_s, sum_const_s);

        // Area
        final GridBagConstraints area_const_l = new GridBagConstraints();
        area_const_l.gridx = 2;
        area_const_l.gridy = 2;
        area_const_l.weightx = 0.1;
        final JLabel area_l = new JLabel(PluginUtils.getResources().getString(
                "ChartHistogram.labels.area"));
        bottomPanel.add(area_l, area_const_l);

        final GridBagConstraints area_const_s = new GridBagConstraints();
        area_const_s.gridx = 3;
        area_const_s.gridy = 2;
        area_const_s.weightx = 0.1;
        final JLabel area_s = new JLabel(createLabel(histogram.getArea(),
                decForm00));
        bottomPanel.add(area_s, area_const_s);

        // Mode
        final GridBagConstraints mode_const_l = new GridBagConstraints();
        mode_const_l.gridx = 4;
        mode_const_l.gridy = 0;
        mode_const_l.weightx = 0.1;
        final JLabel mode_l = new JLabel(PluginUtils.getResources().getString(
                "ChartHistogram.labels.mode"));
        bottomPanel.add(mode_l, mode_const_l);

        final GridBagConstraints mode_const_s = new GridBagConstraints();
        mode_const_s.gridx = 5;
        mode_const_s.gridy = 0;
        mode_const_s.weightx = 0.1;
        final JLabel mode_s = new JLabel(createLabel(histogram.getMode(),
                decForm0));
        bottomPanel.add(mode_s, mode_const_s);

        final GridBagConstraints chartConst = new GridBagConstraints();
        chartConst.gridx = 0;
        chartConst.gridy = 0;
        chartConst.weightx = 0.1;
        chartConst.weighty = 0.1;
        chartConst.fill = GridBagConstraints.BOTH;

        final GridBagConstraints bottomPanelConst = new GridBagConstraints();
        bottomPanelConst.gridx = 0;
        bottomPanelConst.gridy = 1;
        bottomPanelConst.fill = java.awt.GridBagConstraints.HORIZONTAL;

        // Export panel
        final JPanel exportPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints exportPanelConst = new GridBagConstraints();
        exportPanelConst.gridx = 0;
        exportPanelConst.gridy = 2;

        final JButton csvButton = new JButton(PluginUtils.getResources()
                .getString("ChartHistogram.csvButton.text"));
        csvButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {

                final FileNameExtensionFilter fileFilter = new FileNameExtensionFilter(
                        PluginUtils.getResources().getString(
                                "ChartHistogram.csvSaveDialog.description"),
                        new String[] { "csv", "txt" });
                final File[] textFile = PluginUtils.openJChooserDialog(parent,
                        JFileChooser.FILES_ONLY, JFileChooser.SAVE_DIALOG,
                        fileFilter, RasterUtils.lastVisitedFolder, false);

                if (textFile == null || textFile[0] == null) {
                    return;
                }

                final File csvFile = textFile[0];

                try {

                    TextUtils.exportAsTextFile(data, csvFile, ",");

                    JOptionPane.showMessageDialog(
                            parent,
                            PluginUtils.getResources().getString(
                                    "SetWorkspacePlugin.Done.message"),
                            PluginUtils.plugInName,
                            JOptionPane.INFORMATION_MESSAGE);

                } catch (final Exception ex) {
                    ErrorDialog.show(parent, PluginUtils.plugInName,
                            ex.toString(), StringUtil.stackTrace(ex));
                }

            }
        });

        exportPanel.add(csvButton);

        // Main panel
        final JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.add(chartPanel, chartConst);
        mainPanel.add(bottomPanel, bottomPanelConst);
        mainPanel.add(exportPanel, exportPanelConst);

        return mainPanel;
    }

    private String createLabel(Double val, DecimalFormat decForm) {

        if (val != null) {
            return decForm.format(val);
        } else {
            return "-";
        }

    }

    private final DecimalFormatSymbols decForSymb = new DecimalFormatSymbols(
            Locale.ENGLISH);
    private final DecimalFormat decForm0 = new DecimalFormat("0", decForSymb);
    private final DecimalFormat decForm00 = new DecimalFormat("0.00",
            decForSymb);

}
