package org.openjump.core.ui.plugin.colorchooser;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JColorChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import language.I18NPlug;

import org.openjump.core.ui.plugin.colorchooser.gui.ColorMenu;
import org.openjump.core.ui.plugin.colorchooser.gui.ComboButton;
import org.openjump.core.ui.plugin.colorchooser.utils.ColorUtils;

import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollectionWrapper;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.FeatureEventType;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.LayerManager;
import com.vividsolutions.jump.workbench.model.UndoableCommand;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheck;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.LayerViewPanel;
import com.vividsolutions.jump.workbench.ui.images.IconLoader;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.task.TaskMonitorManager;

public class FeatureColorChooserPlugIn extends AbstractPlugIn {

    private static int buttonWidth = 25;
    private PlugInContext context;
    public ComboButton colorChooserButton;
    public static ComboButton colorSetbutton;
    private JPopupMenu colorPickerPopup = new JPopupMenu();

    public static JMenuItem mi;
    public static final String COLOR = "COLOR";
    public static final String R_G_B = BasicStyle.RGB_ATTRIBUTE_NAME;
    private TaskMonitorManager taskMonitorManager;
    private int customIndex = 1;

    @Override
    public void initialize(final PlugInContext context) throws Exception {
        this.context = context;
        colorSetbutton = new ComboButton(1) {
            private static final long serialVersionUID = 1L;

            @Override
            public void setBounds(int x, int y, int width, int height) {
                super.setBounds(x, y,
                        FeatureColorChooserPlugIn.this.buttonWidth, height);
            }
        };
        this.colorChooserButton = new ComboButton(0) {
            private static final long serialVersionUID = 1L;

            @Override
            public void setBounds(int x, int y, int width, int height) {
                super.setBounds(colorSetbutton.getX() + buttonWidth, y,
                        buttonWidth, height);
            }
        };
        colorSetbutton.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                FeatureColorChooserPlugIn.this.setFeatureColor(colorSetbutton
                        .getColor());
            }

            @Override
            public void mousePressed(MouseEvent e) {
            }

            @Override
            public void mouseReleased(MouseEvent e) {
            }

            @Override
            public void mouseExited(MouseEvent e) {
            }

            @Override
            public void mouseEntered(MouseEvent e) {
            }
        });

        colorChooserButton.addMouseListener(new MouseListener() {
            @Override
            public void mousePressed(MouseEvent e) {
                int x = colorSetbutton.getLocation().x;
                int y = colorSetbutton.getLocation().y
                        + colorSetbutton.getHeight();
                FeatureColorChooserPlugIn.this.colorPickerPopup.show(
                        colorSetbutton.getParent(), x, y);
            }

            @Override
            public void mouseClicked(MouseEvent e) {
            }

            @Override
            public void mouseReleased(MouseEvent e) {
            }

            @Override
            public void mouseExited(MouseEvent e) {
            }

            @Override
            public void mouseEntered(MouseEvent e) {
            }
        });

        final JPopupMenu popup = new JPopupMenu();
        popup.setLayout(new GridLayout(0, 1));
        mi = new JMenuItem(I18NPlug.getI18N("use-layer-style-color"),
                new ColorIcon(null));

        final JMenu recent = new JMenu(I18NPlug.getI18N("recent-color") + "...");

        mi.addActionListener(new ColorPickerActionListener(null));
        popup.add(mi);
        final ColorMenu cm = new ColorMenu(I18NPlug.getI18N("choose-color"));
        cm.setIcon(getColorIcon());

        cm.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Color color = cm.getColor();
                if (color != null) {
                    colorSetbutton.setColor(color);
                    setFeatureColor(color);
                    cm.addActionListener(new ColorPickerActionListener(color));

                    FeatureColorChooserPlugIn.this.colorSetbutton
                            .setColor(color);
                    FeatureColorChooserPlugIn.this.setFeatureColor(color);
                    String hex = ColorUtils.colorRGBToHex(color);
                    String acad = ColorUtils.getColorFromRegistry(hex);
                    String msg = "Index color: " + acad;

                    String text = "Hex: " + hex + "   RGB: " + color.getRed()
                            + "," + color.getGreen() + "," + color.getBlue();
                    JMenuItem mis = new JMenuItem(text,
                            new FeatureColorChooserPlugIn.ColorIcon(color));
                    mis.setToolTipText(msg);
                    mis.addActionListener(new FeatureColorChooserPlugIn.ColorPickerActionListener(
                            color));
                    recent.add(mis);
                    FeatureColorChooserPlugIn.this.colorPickerPopup.insert(
                            recent,
                            FeatureColorChooserPlugIn.this.customIndex++);
                    popup.revalidate();
                    popup.repaint();
                }
            }
        });

        popup.add(cm);

        mi = new JMenuItem(I18NPlug.getI18N("other-color"), getColorIcon_2());
        mi.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent paramAnonymousActionEvent) {
                new JColorChooser();
                Color color = JColorChooser.showDialog(context
                        .getWorkbenchContext().getWorkbench().getFrame(),
                        I18NPlug.getI18N("choose-color"), new Color(0, 0, 0));
                if (color != null) {
                    colorSetbutton.setColor(color);
                    setFeatureColor(color);
                    FeatureColorChooserPlugIn.this.colorSetbutton
                            .setColor(color);
                    FeatureColorChooserPlugIn.this.setFeatureColor(color);
                    String hex = ColorUtils.colorRGBToHex(color);
                    String acad = ColorUtils.getColorFromRegistry(hex);

                    String msg = "Index color: " + acad;

                    String text = "Hex: " + hex + "   RGB: " + color.getRed()
                            + "," + color.getGreen() + "," + color.getBlue();
                    JMenuItem mis = new JMenuItem(text,
                            new FeatureColorChooserPlugIn.ColorIcon(color));
                    mis.setToolTipText(msg);
                    mis.addActionListener(new FeatureColorChooserPlugIn.ColorPickerActionListener(
                            color));
                    recent.add(mis);
                    FeatureColorChooserPlugIn.this.colorPickerPopup.insert(
                            recent,
                            FeatureColorChooserPlugIn.this.customIndex++);
                    popup.revalidate();
                    popup.repaint();
                }
            }
        });
        popup.add(mi);

        // popup.addSeparator();
        mi = new JMenuItem(I18NPlug.getI18N("picker-color"), getPickColorIcon());
        PickColorPlugIn pick = new PickColorPlugIn();
        mi.setToolTipText(I18NPlug.getI18N("msg2"));
        final ActionListener listener = AbstractPlugIn.toActionListener(pick,
                context.getWorkbenchContext(), taskMonitorManager);
        mi.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                listener.actionPerformed(e);
            }
        });
        popup.add(mi);

        //
        popup.add(recent);

        this.colorPickerPopup = popup;
        colorSetbutton.setToolTipText(I18NPlug.getI18N("set-color-Tool"));
        colorChooserButton.setToolTipText(I18NPlug.getI18N("pick-color-tools"));
        context.getWorkbenchContext().getWorkbench().getFrame().getToolBar()
                .addSeparator();
        context.getWorkbenchContext().getWorkbench().getFrame().getToolBar()
                .add(colorSetbutton);
        context.getWorkbenchContext().getWorkbench().getFrame().getToolBar()
                .add(colorChooserButton);
        context.getWorkbenchContext().getWorkbench().getFrame().getToolBar()
                .addSeparator();

    }

    public Icon getColorIcon() {
        ImageIcon icon = new ImageIcon(getClass().getResource(
                "color-swatch.png"));
        return GUIUtil.toSmallIcon(icon);
    }

    public Icon getColorIcon_2() {
        ImageIcon icon = IconLoader.icon("color_wheel.png");
        return GUIUtil.toSmallIcon(icon);
    }

    public Icon getPickColorIcon() {
        ImageIcon icon2 = new ImageIcon(getClass().getResource("pipette.png"));
        return GUIUtil.toSmallIcon(icon2);
    }

    private void setFeatureColor(Color color) {
        LayerViewPanel layerViewPanel = this.context.getWorkbenchContext()
                .getLayerViewPanel();
        if (layerViewPanel == null)
            return;
        Collection<Layer> layers = layerViewPanel.getSelectionManager()
                .getLayersWithSelectedItems();

        for (Iterator<Layer> i = layers.iterator(); i.hasNext();) {
            Layer layer = i.next();
            if (layer.isEditable())
                continue;
            layerViewPanel.getContext().warnUser(
                    I18NPlug.getI18N("selected-items-layers-must-be-editable")
                            + " (" + layer.getName() + ")");
            return;
        }

        for (Iterator<Layer> i = layers.iterator(); i.hasNext();) {
            Layer layer = i.next();
            layer.setFeatureCollectionModified(true);
            FeatureCollectionWrapper fcw = layer.getFeatureCollectionWrapper();
            FeatureSchema schema = fcw.getFeatureSchema();

            if (!schema.hasAttribute(R_G_B)) {
                schema.addAttribute(R_G_B, AttributeType.STRING);

                for (Iterator<Feature> j = fcw.iterator(); j.hasNext();) {
                    Feature feature = j.next();
                    Object[] attributes = new Object[schema.getAttributeCount()];

                    for (int k = 0; k < attributes.length - 1; k++) {
                        attributes[k] = feature.getAttribute(k);
                    }
                    feature.setAttributes(attributes);
                }
            }
            if (!schema.hasAttribute(COLOR)) {
                schema.addAttribute(COLOR, AttributeType.STRING); // .INTEGER);
                for (Iterator<Feature> j = fcw.iterator(); j.hasNext();) {
                    Feature feature = j.next();
                    Object[] attributes = new Object[schema.getAttributeCount()];

                    for (int k = 0; k < attributes.length - 1; k++) {
                        attributes[k] = feature.getAttribute(k);
                    }
                    feature.setAttributes(attributes);
                }
            }
        }
        Collection<Feature> features = layerViewPanel.getSelectionManager()
                .getFeaturesWithSelectedItems();
        setRGB(layers, features, ColorUtils.colorRGBToHex(color));
    }

    public static FeatureSchema createFeatureSchema() {
        return new FeatureSchema() {

            private static final long serialVersionUID = 1L;
        };
    }

    protected void setRGB(final Collection<Layer> layers,
            final Collection<Feature> features, String RGB) {
        if (layers.isEmpty())
            return;
        final String newRGB = RGB;
        final ArrayList<Object> RGBs = new ArrayList<Object>();
        final ArrayList<Object> Colors = new ArrayList<Object>();

        for (Iterator<Feature> f = features.iterator(); f.hasNext();) {
            Feature feature = f.next();
            RGBs.add(feature.getAttribute(R_G_B));
            Colors.add(feature.getAttribute(COLOR));
        }

        final LayerManager layerManager = layers.iterator().next()
                .getLayerManager();
        layerManager.getUndoableEditReceiver().startReceiving();

        try {
            UndoableCommand command = new UndoableCommand("Edit R_G_B") {
                @Override
                public void execute() {
                    for (Iterator<Feature> f = features.iterator(); f.hasNext();) {
                        Feature feature = f.next();
                        feature.setAttribute(R_G_B, newRGB);
                        feature.setAttribute(COLOR,
                                ColorUtils.getColorFromRegistry(newRGB));
                    }

                    for (Iterator<Layer> i = layers.iterator(); i.hasNext();) {
                        Layer layer = i.next();
                        layer.fireAppearanceChanged();
                        layerManager.fireFeaturesChanged(features,
                                FeatureEventType.ATTRIBUTES_MODIFIED, layer);
                    }

                }

                @Override
                public void unexecute() {
                    int i = 0;
                    for (Iterator<Feature> f = features.iterator(); f.hasNext();) {
                        Feature feature = f.next();
                        Object ob = RGBs.get(i++);
                        feature.setAttribute(R_G_B, ob);
                        String oldRGB = ob.toString();
                        feature.setAttribute(COLOR,
                                ColorUtils.getColorFromRegistry(oldRGB));
                    }

                    for (Iterator<Layer> j = layers.iterator(); j.hasNext();) {
                        Layer layer = j.next();
                        layer.fireAppearanceChanged();
                        layerManager.fireFeaturesChanged(features,
                                FeatureEventType.ATTRIBUTES_MODIFIED, layer);
                    }
                }
            };
            command.execute();
            layerManager.getUndoableEditReceiver().receive(
                    command.toUndoableEdit());
        } finally {
            layerManager.getUndoableEditReceiver().stopReceiving();
        }
    }

    /*
     * public static FeatureSchema getSchema(FeatureSchema featureSchema) {
     * featureSchema.addAttribute(R_G_B, AttributeType.STRING);
     * featureSchema.addAttribute(COLOR, AttributeType.INTEGER); return
     * featureSchema; }
     */

    @Override
    public boolean execute(PlugInContext context) throws Exception {
        return true;
    }

    public static EnableCheck createEnableCheck(
            WorkbenchContext workbenchContext, boolean b) {
        EnableCheckFactory checkFactory = new EnableCheckFactory(
                workbenchContext);

        return new MultiEnableCheck().add(
                checkFactory.createWindowWithLayerViewPanelMustBeActiveCheck())
                .add(checkFactory.createAtLeastNLayersMustBeEditableCheck(1));
    }

    private class ColorIcon implements Icon {
        private Color color = null;

        public ColorIcon(Color color) {
            this.color = color;
        }

        @Override
        public int getIconHeight() {
            return 10;
        }

        @Override
        public int getIconWidth() {
            return 10;
        }

        @Override
        public void paintIcon(Component comp, Graphics g, int x, int y) {
            Color oldColor = g.getColor();

            int j = 0;
            int size = Math.max(getIconHeight(), 2);
            g.translate(x, y);

            if (this.color == null)
                g.setColor(new Color(0, 0, 0));
            else {
                g.setColor(this.color);
            }
            j = 0;

            if (this.color == null) {
                g.drawLine(0, 8, 5, 8);
                g.drawLine(5, 8, 5, 7);
                g.drawLine(1, 8, 1, 1);
                g.drawLine(0, 1, 2, 1);
            } else {
                for (int i = size - 1; i >= 0; i--) {
                    g.drawLine(0, j, 7, j);
                    j++;
                }
            }
            g.translate(-x, -y);
            g.setColor(oldColor);
        }
    }

    public class ColorPickerActionListener implements ActionListener {
        Color color = null;

        ColorPickerActionListener(Color color) {
            this.color = color;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            colorSetbutton.setColor(this.color);
            String hex = ColorUtils.colorRGBToHex(this.color);
            String acad = ColorUtils.getColorFromRegistry(hex);
            colorSetbutton.setToolTipText("Index color: " + acad + "  Hex:"
                    + hex + "   RGB: " + this.color.getRed() + ","
                    + this.color.getGreen() + "," + this.color.getBlue());
            setFeatureColor(this.color);
        }
    }

}