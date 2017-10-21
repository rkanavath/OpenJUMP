package org.openjump.core.ui.plugin.colorchooser.gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;

import javax.swing.JButton;
import javax.swing.UIManager;
import javax.swing.border.Border;

public class ComboButton extends JButton {
    /**
	 * 
	 */
    private static final long serialVersionUID = 1L;
    static final int TRIANGLE = 0;
    static final int RECTANGLE = 1;
    private Color color = null;
    private int symbol = 0;
    protected Border m_unselectedBorder;

    public ComboButton(int symbol) {
        if ((symbol >= 0) && (symbol <= 1))
            this.symbol = symbol;
        setRequestFocusEnabled(false);

    }

    public void setColor(Color color) {
        this.color = color;
        repaint();
    }

    public Color getColor() {
        return this.color;
    }

    @Override
    public void paint(Graphics g) {
        int w = getSize().width;
        int h = getSize().height;
        Color origColor = g.getColor();
        boolean isPressed = getModel().isPressed();
        boolean isEnabled = isEnabled();

        if ((h < 5) || (w < 5)) {
            g.setColor(origColor);
            return;
        }

        if (isPressed) {
            g.translate(1, 1);
        }

        super.paint(g);

        int size = Math.min((h - 4) / 3, (w - 4) / 3);
        size = Math.max(size, 2);
        if (this.symbol == 0)
            paintTriangle(g, (w - size) / 2, (h - size) / 2, size, isEnabled);
        else {
            paintRectangle(g, (w - size) / 2, (h - size) / 2, size, isEnabled);
        }

        if (isPressed) {
            g.translate(-1, -1);
        }

        g.setColor(origColor);
    }

    @Override
    public Dimension getPreferredSize() {
        return new Dimension(25, 25);
    }

    @Override
    public Dimension getMinimumSize() {
        return getPreferredSize();
    }

    @Override
    public Dimension getMaximumSize() {
        return getPreferredSize();
    }

    @Override
    public boolean isFocusTraversable() {
        return false;
    }

    private void paintTriangle(Graphics g, int x, int y, int size,
            boolean isEnabled) {
        Color oldColor = g.getColor();

        int j = 0;
        size = Math.max(size, 2);
        int mid = size / 2;
        g.translate(x, y);

        if (isEnabled)
            g.setColor(UIManager.getColor("controlDkShadow"));
        else {
            g.setColor(UIManager.getColor("controlShadow"));
        }
        if (!isEnabled) {
            g.translate(1, 1);
            g.setColor(UIManager.getColor("controlLtHighlight"));

            for (int i = size - 1; i >= 0; i--) {
                g.drawLine(mid - i, j, mid + i, j);
                j++;
            }

            g.translate(-1, -1);
            g.setColor(UIManager.getColor("controlShadow"));
        }

        j = 0;

        for (int i = size - 1; i >= 0; i--) {
            g.drawLine(mid - i, j, mid + i, j);
            j++;
        }

        g.translate(-x, -y);
        g.setColor(oldColor);
    }

    private void paintRectangle(Graphics g, int x, int y, int size,
            boolean isEnabled) {
        Color oldColor = g.getColor();

        int j = 0;
        size = Math.max(size, 2);
        g.translate(x, y);

        if (isEnabled) {
            if (this.color == null)
                g.setColor(new Color(0, 0, 0));
            else
                g.setColor(this.color);
        } else
            g.setColor(UIManager.getColor("controlShadow"));

        if (!isEnabled) {
            g.translate(1, 1);
            g.setColor(UIManager.getColor("controlLtHighlight"));

            if (this.color == null) {
                g.drawLine(0, 7, 5, 7);
                g.drawLine(5, 7, 5, 6);
                g.drawLine(1, 7, 1, 0);
                g.drawLine(0, 0, 2, 0);
            } else {
                for (int i = size - 1; i >= 0; i--) {
                    g.drawLine(0, j, 7, j);
                    j++;
                }
            }

            g.translate(-1, -1);
            g.setColor(UIManager.getColor("controlShadow"));
        }

        j = 0;

        if (this.color == null) {
            g.drawLine(0, 7, 5, 7);
            g.drawLine(5, 7, 5, 6);
            g.drawLine(1, 7, 1, 0);
            g.drawLine(0, 0, 2, 0);
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