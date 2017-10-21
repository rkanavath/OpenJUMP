package org.openjump.core.ui.plugin.colorchooser.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Hashtable;

import javax.swing.BorderFactory;
import javax.swing.JMenu;
import javax.swing.JPanel;
import javax.swing.MenuSelectionManager;
import javax.swing.border.BevelBorder;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;

import org.openjump.core.ui.plugin.colorchooser.utils.ColorUtils;

public class ColorMenu extends JMenu {
    /*
     * Modified from: Swing, Second Edition 2nd Edition by James Gosling
     * (Foreword), Matthew Robinson (Author), Pavel Vorobiev (Author), Pavel A
     * Vorobiev PhD (Author), David Anderson (Author), David Karr (Author)
     * https://www.grandt.com/sbe/files/swing2e/Chapter12.zip
     * https://www.grandt.com/sbe/files/uts2/Chapter12html/Chapter12.htm
     */
    private static final long serialVersionUID = 1L;
    protected Border m_unselectedBorder;
    protected Border m_selectedBorder;
    protected Border m_activeBorder;

    protected Hashtable<Color, ColorPane> m_panes;
    protected ColorPane m_selected;

    public ColorMenu(String name) {
        super(name);
        m_unselectedBorder = new CompoundBorder(new MatteBorder(1, 1, 1, 1,
                getBackground()), new BevelBorder(BevelBorder.LOWERED,
                Color.white, Color.gray));
        m_selectedBorder = new CompoundBorder(new MatteBorder(2, 2, 2, 2,
                Color.red), new MatteBorder(1, 1, 1, 1, getBackground()));
        m_activeBorder = new CompoundBorder(new MatteBorder(2, 2, 2, 2,
                Color.blue), new MatteBorder(1, 1, 1, 1, getBackground()));

        m_panes = new Hashtable<Color, ColorPane>();
        ColorPane pn = null;

        /*
         * First the darkest colors palette.
         */
        JPanel p = new JPanel();
        p.setBorder(new EmptyBorder(1, 1, 1, 1));
        p.setLayout(new GridLayout(5, 24));
        for (int r = 18; r < 250; r += 10) {
            String hex = ColorUtils.getColorIndexRegistry(String.valueOf(r));
            Color c = Color.decode("0x" + hex);
            pn = new ColorPane(c);
            p.add(pn);
            m_panes.put(c, pn);
        }
        for (int r = 16; r < 250; r += 10) {
            String hex = ColorUtils.getColorIndexRegistry(String.valueOf(r));
            Color c = Color.decode("0x" + hex);
            pn = new ColorPane(c);
            p.add(pn);
            m_panes.put(c, pn);
        }
        for (int r = 14; r < 250; r += 10) {
            String hex = ColorUtils.getColorIndexRegistry(String.valueOf(r));
            Color c = Color.decode("0x" + hex);
            pn = new ColorPane(c);
            p.add(pn);
            m_panes.put(c, pn);
        }
        for (int r = 12; r < 250; r += 10) {
            String hex = ColorUtils.getColorIndexRegistry(String.valueOf(r));
            Color c = Color.decode("0x" + hex);
            pn = new ColorPane(c);
            p.add(pn);
            m_panes.put(c, pn);
        }
        for (int r = 10; r < 250; r += 10) {
            String hex = ColorUtils.getColorIndexRegistry(String.valueOf(r));
            Color c = Color.decode("0x" + hex);
            pn = new ColorPane(c);
            p.add(pn);
            m_panes.put(c, pn);
        }
        /*
         * Then the brighter colors palette
         */

        JPanel pb = new JPanel();
        pb.setBorder(new EmptyBorder(1, 1, 1, 1));
        pb.setLayout(new GridLayout(5, 24));
        for (int r = 11; r < 250; r += 10) {
            String hex = ColorUtils.getColorIndexRegistry(String.valueOf(r));
            Color c = Color.decode("0x" + hex);
            pn = new ColorPane(c);
            pb.add(pn);
            m_panes.put(c, pn);
        }
        for (int r = 13; r < 250; r += 10) {
            String hex = ColorUtils.getColorIndexRegistry(String.valueOf(r));
            Color c = Color.decode("0x" + hex);
            pn = new ColorPane(c);
            pb.add(pn);
            m_panes.put(c, pn);
        }
        for (int r = 15; r < 250; r += 10) {
            String hex = ColorUtils.getColorIndexRegistry(String.valueOf(r));
            Color c = Color.decode("0x" + hex);
            pn = new ColorPane(c);
            pb.add(pn);
            m_panes.put(c, pn);
        }
        for (int r = 17; r < 250; r += 10) {
            String hex = ColorUtils.getColorIndexRegistry(String.valueOf(r));
            Color c = Color.decode("0x" + hex);
            pn = new ColorPane(c);
            pb.add(pn);
            m_panes.put(c, pn);
        }
        for (int r = 19; r < 250; r += 10) {
            String hex = ColorUtils.getColorIndexRegistry(String.valueOf(r));
            Color c = Color.decode("0x" + hex);
            pn = new ColorPane(c);
            pb.add(pn);
            m_panes.put(c, pn);
        }

        JPanel pmain = new JPanel();
        pmain.setBorder(BorderFactory.createTitledBorder("Full color palette"));
        pmain.add(p);
        pmain.add(pb);

        /*
         * Than the main 11 colors
         */
        JPanel p2 = new JPanel(new FlowLayout(FlowLayout.LEFT));
        p2.setBorder(new EmptyBorder(1, 1, 1, 1));
        p2.setAlignmentX(Component.RIGHT_ALIGNMENT);
        // p2.setLayout(new GridLayout(1, 10));
        for (int r = 0; r < 10; r++) {
            String hex = ColorUtils.getColorIndexRegistry(String.valueOf(r));
            Color c = Color.decode("0x" + hex);
            pn = new ColorPane(c);
            p2.add(pn);
            m_panes.put(c, pn);
        }

        /*
         * The last line with the greys
         */
        JPanel p3 = new JPanel(new FlowLayout(FlowLayout.LEFT));
        p3.setBorder(new EmptyBorder(1, 1, 1, 1));
        p3.setAlignmentX(Component.RIGHT_ALIGNMENT);
        // p2.setLayout(new GridLayout(1, 10));
        for (int r = 250; r < 256; r++) {
            String hex = ColorUtils.getColorIndexRegistry(String.valueOf(r));
            Color c = Color.decode("0x" + hex);
            pn = new ColorPane(c);
            p3.add(pn);
            m_panes.put(c, pn);
        }

        add(p);
        add(pb);
        add(p2);
        add(p3);
    }

    public void setColor(Color c) {
        Object obj = m_panes.get(c);
        if (obj == null)
            return;
        if (m_selected != null)
            m_selected.setSelected(false);
        m_selected = (ColorPane) obj;
        m_selected.setSelected(true);
    }

    public Color getColor() {
        if (m_selected == null)
            return null;
        return m_selected.getColor();
    }

    public void doSelection() {
        fireActionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED,
                getActionCommand()));
    }

    class ColorPane extends JPanel implements MouseListener {
        protected Color m_c;
        protected boolean m_selected;

        public ColorPane(Color c) {
            m_c = c;
            setBackground(c);
            setBorder(m_unselectedBorder);
            String hex = ColorUtils.colorRGBToHex(c);
            String acad = ColorUtils.getColorIndexRegistry(hex);
            String msg = "Index color: " + acad + "   Hex: " + hex + "   RGB: "
                    + c.getRed() + "," + c.getGreen() + "," + c.getBlue();
            setToolTipText(msg);
            addMouseListener(this);
        }

        public Color getColor() {
            return m_c;
        }

        @Override
        public Dimension getPreferredSize() {
            return new Dimension(20, 20);
        }

        @Override
        public Dimension getMaximumSize() {
            return getPreferredSize();
        }

        @Override
        public Dimension getMinimumSize() {
            return getPreferredSize();
        }

        public void setSelected(boolean selected) {
            m_selected = selected;
            if (m_selected)
                setBorder(m_selectedBorder);
            else
                setBorder(m_unselectedBorder);
        }

        public boolean isSelected() {
            return m_selected;
        }

        @Override
        public void mousePressed(MouseEvent e) {
        }

        @Override
        public void mouseClicked(MouseEvent e) {
        }

        @Override
        public void mouseReleased(MouseEvent e) {
            setColor(m_c);
            MenuSelectionManager.defaultManager().clearSelectedPath();
            doSelection();
        }

        @Override
        public void mouseEntered(MouseEvent e) {
            setBorder(m_activeBorder);
        }

        @Override
        public void mouseExited(MouseEvent e) {
            setBorder(m_selected ? m_selectedBorder : m_unselectedBorder);
        }
    }
}