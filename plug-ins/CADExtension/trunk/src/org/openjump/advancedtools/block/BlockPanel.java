package org.openjump.advancedtools.block;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Shape;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.TitledBorder;

import org.openjump.advancedtools.language.I18NPlug;
import org.saig.core.gui.swing.sldeditor.util.FormUtils;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.workbench.JUMPWorkbench;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.plugin.EnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.WorkbenchToolBar;

public class BlockPanel extends JPanel {

    static WorkbenchContext context = JUMPWorkbench.getInstance().getFrame()
            .getContext();
    private static final long serialVersionUID = 1L;
    public static final String blockFolder = "VertexImages";
    public static final String NAME = "Insert symbol as block";
    @SuppressWarnings("rawtypes")
    public static JComboBox chooseBox;

    public static DefaultComboBoxModel<String> model = new DefaultComboBoxModel<String>();

    public static JPanel block_panel = new JPanel(new GridLayout(0, 1));
    public static JPanel option_panel = new JPanel(new GridLayout(0, 1));
    public static JPanel preview_panel = new JPanel(new GridLayout(0, 1));
    public static JPanel mainPanel = new JPanel(new GridLayout(0, 1));
    public static JLabel block_panel_jlabel;
    public static JLabel option_panel_jlabel1;
    public static JLabel option_panel_jlabel2;

    // public static JPanel previewPanel = new JPanel();
    public static JButton jButton_Help = new JButton();
    private static TitledBorder titledBorder;
    @SuppressWarnings("rawtypes")
    public static List blocks;
    public static HashMap<Object, Shape> shapes;

    public static String circle = I18N
            .get("deejump.ui.style.RenderingStylePanel.circle");

    public static String triangle = I18N
            .get("deejump.ui.style.RenderingStylePanel.triangle");
    public static String square = I18N
            .get("deejump.ui.style.RenderingStylePanel.square");
    public static String hexagon = "hexagon";
    public static String cross = I18N
            .get("deejump.ui.style.RenderingStylePanel.cross");
    public static String star = I18N
            .get("deejump.ui.style.RenderingStylePanel.star");
    public static SpinnerModel rotationModel = new SpinnerNumberModel(0, // initial
                                                                         // value
            0, // min
            359, // max
            1);
    public static SpinnerModel dimensionModel = new SpinnerNumberModel(100, // initial
                                                                            // value
            10, // min
            400, // max
            10);
    public static JSpinner rotationSpinner = new JSpinner();

    public static JSpinner dimensionSpinner = new JSpinner();

    private static WorkbenchToolBar toolBar = new WorkbenchToolBar(null) {
        /**
         * 
         */
        private static final long serialVersionUID = 1L;

        @Override
        public JButton addPlugIn(Icon icon, PlugIn plugIn,
                EnableCheck enableCheck, WorkbenchContext workbenchContext) {
            return super.addPlugIn(icon, plugIn, enableCheck, workbenchContext);
        }

    };

    @SuppressWarnings({})
    public JPanel panel_blocks(PlugInContext context) {

        block_panel = new JPanel(new GridBagLayout());
        block_panel_jlabel = new JLabel(
                I18NPlug.getI18N("org.openjump.core.ui.plugins.block.dialog.select-block"));
        block_panel_jlabel
                .setToolTipText(I18NPlug
                        .getI18N("org.openjump.core.ui.plugins.block.dialog.dialog.select-block-message"));

        rotationSpinner = new JSpinner(rotationModel);
        option_panel_jlabel1 = new JLabel(
                I18NPlug.getI18N("org.openjump.core.ui.plugins.block.dialog-rotation"));
        option_panel_jlabel1
                .setToolTipText(I18NPlug
                        .getI18N("org.openjump.core.ui.plugins.annotation.dialog.font-rotation-message"));

        dimensionSpinner = new JSpinner(dimensionModel);
        option_panel_jlabel2 = new JLabel(
                I18NPlug.getI18N("org.openjump.core.ui.plugins.block.dialog-dimension"));
        option_panel_jlabel2.setToolTipText(gedtDescription());

        FormUtils.addRowInGBL(block_panel, 1, 0, null, option_panel_jlabel2,
                dimensionSpinner, option_panel_jlabel1, rotationSpinner);

        mainPanel = new JPanel(new GridBagLayout());
        titledBorder = new TitledBorder(BorderFactory.createEtchedBorder(
                Color.white, new Color(148, 145, 140)),
                I18NPlug.getI18N("org.openjump.core.ui.plugins.block"));
        mainPanel.setBorder(titledBorder);
        initToolBar(context.getWorkbenchContext());
        FormUtils.addRowInGBL(mainPanel, 1, 0, toolBar);
        FormUtils.addRowInGBL(mainPanel, 2, 0, block_panel);

        return mainPanel;
    }

    public static List<String> list;

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public JPanel chooseBlockPanel(PlugInContext context) {

        chooseBox = new JComboBox(new Vector(listOfBlockFiles(context)));

        Dimension d = new Dimension(125, chooseBox.getPreferredSize().height);
        chooseBox.setPreferredSize(d);

        BlockCellRenderer renderer = new BlockCellRenderer();
        chooseBox.setRenderer(renderer);
        list = listOfBlockFiles(context);
        HashMap<Object, Icon> icons = ListOfBlocksIcons();
        renderer.setTooltips(list);
        renderer.setIcons(icons);

        model = (DefaultComboBoxModel) chooseBox.getModel();
        JPanel test_block_panel = new JPanel(new GridBagLayout());

        test_block_panel
                .setToolTipText(I18NPlug
                        .getI18N("org.openjump.core.ui.plugins.block.dialog.dialog.select-block-message"));
        test_block_panel.add(chooseBox);

        return test_block_panel;
    }

    private void initToolBar(WorkbenchContext context) {
        DrawBlockPlugIn draw = new DrawBlockPlugIn();
        SaveBlockPlugIn save = new SaveBlockPlugIn();
        // ShowHelpPlugIn help = new ShowHelpPlugIn();
        DrawOrientedBlockPlugIn drag = new DrawOrientedBlockPlugIn();

        toolBar.addPlugIn(draw.getIcon(), draw, null, context);
        toolBar.addPlugIn(drag.getIcon(), drag, null, context);
        toolBar.addPlugIn(save.getIcon(), save, null, context);
        toolBar.addSeparator();
        toolBar.add(chooseBlockPanel(context.createPlugInContext()),
                BorderLayout.EAST);

        // ShowBlockPlugIn showBlockPlugIn = new ShowBlockPlugIn();
        // toolBar.addPlugIn(save.getIcon(), showBlockPlugIn, null, context);
        // toolBar.addPlugIn(help.getIcon(), help, null, context);
    }

    @SuppressWarnings("unchecked")
    public static List<String> listOfBlockFiles(PlugInContext context) {
        File pluginDir = context.getWorkbenchContext().getWorkbench()
                .getPlugInManager().getPlugInDirectory();
        String wd = pluginDir.getAbsolutePath();
        blocks = new ArrayList<String>();
        blocks.add(square);
        blocks.add(circle);
        blocks.add(triangle);
        blocks.add(cross);
        blocks.add(star);
        try {
            String block_folder = wd + File.separator + blockFolder;
            File directory = new File(block_folder);
            // get all the files from a directory
            File[] fList = directory.listFiles();
            String fileName = null;
            for (File file : fList) {
                if (file.isFile()) {
                    String ext = null;
                    String s = file.getName();
                    int i = s.lastIndexOf('.');
                    if (i > 0 && i < s.length() - 1) {
                        ext = s.substring(i + 1).toUpperCase();
                    }
                    if (ext.equals("WKT"))
                        fileName = file.getName();
                    int pos = fileName.lastIndexOf(".");
                    if (pos > 0) {
                        fileName = fileName.substring(0, pos);
                        fileName.substring(pos, fileName.length());
                    }
                    blocks.add(fileName);
                }
            }

        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return blocks;
    }

    private static HashMap<Object, Icon> icons = new HashMap<Object, Icon>();

    public static HashMap<Object, Icon> ListOfBlocksIcons() {
        File pluginDir = context.getWorkbench().getPlugInManager()
                .getPlugInDirectory();
        String wd = pluginDir.getAbsolutePath();
        icons = new HashMap<Object, Icon>();

        try {
            Geometry geom1 = BlockUtils.square();
            Shape shape1 = context.getLayerViewPanel().getJava2DConverter()
                    .toShape(geom1);
            icons.put("shapes", BlockUtils.createIcon(shape1));

            Geometry geom2 = BlockUtils.circle();
            Shape shape2 = context.getLayerViewPanel().getJava2DConverter()
                    .toShape(geom2);
            icons.put("shapes", BlockUtils.createIcon(shape2));

            Geometry geom3 = BlockUtils.triangle();
            Shape shape3 = context.getLayerViewPanel().getJava2DConverter()
                    .toShape(geom3);
            icons.put("shapes", BlockUtils.createIcon(shape3));

            Geometry geom4 = BlockUtils.cross();
            Shape shape4 = context.getLayerViewPanel().getJava2DConverter()
                    .toShape(geom4);
            icons.put("shapes", BlockUtils.createIcon(shape4));

            Geometry geom5 = BlockUtils.star();
            Shape shape5 = context.getLayerViewPanel().getJava2DConverter()
                    .toShape(geom5);
            icons.put("shapes", BlockUtils.createIcon(shape5));
            String block_folder = wd + File.separator + blockFolder;
            File directory = new File(block_folder);
            // get all the files from a directory
            File[] fList = directory.listFiles();
            Geometry geom = null;
            for (File file : fList) {

                if (file.isFile()) {
                    String ext = null;
                    String s = file.getName();
                    int i = s.lastIndexOf('.');
                    if (i > 0 && i < s.length() - 1) {
                        ext = s.substring(i + 1).toUpperCase();
                    }
                    if (ext.equals("WKT")) {

                        String blockName = (String) BlockPanel.chooseBox
                                .getSelectedItem() + ".wkt";
                        /*
                         * geom2 = BlockUtils.readGeomFromWKT(
                         * context.getWorkbenchContext(), blockName,
                         * BlockPanel.blockFolder);
                         */

                        geom = BlockUtils.readGeomFromWKT(blockName,
                                BlockPanel.blockFolder);

                    }
                    Shape selectedShape = context.getLayerViewPanel()
                            .getJava2DConverter().toShape(geom);
                    icons.put("shapes", BlockUtils.createIcon(selectedShape));
                }

            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        return icons;
    }

    public String gedtDescription() {

        String tooltip = "";
        tooltip = "<HTML><BODY>";
        tooltip += "<DIV style=\"width: 200px; text-justification: justify;\">";
        tooltip += "<b>"
                + I18NPlug
                        .getI18N("org.openjump.core.ui.plugins.block.define-dimension")
                + "</b>" + "<br>";
        tooltip += I18NPlug
                .getI18N("org.openjump.core.ui.plugins.block.define-dimension-description")
                + "<br>";
        tooltip += "</DIV></BODY></HTML>";
        return tooltip;
    }

    public Icon makeIcon(Shape s) {

        BufferedImage image = new BufferedImage(20, 20,
                BufferedImage.TYPE_BYTE_BINARY);
        Graphics2D gr = image.createGraphics();
        // move the shape in the region of the image
        gr.setColor(Color.red);
        gr.setBackground(Color.white);
        gr.draw(s);
        gr.dispose();
        ImageIcon icon = new ImageIcon(image);
        return icon;
    }

}
