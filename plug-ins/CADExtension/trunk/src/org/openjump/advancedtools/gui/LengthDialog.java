package org.openjump.advancedtools.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.NumberFormat;
import java.util.Locale;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.openjump.advancedtools.language.I18NPlug;

public class LengthDialog extends JDialog implements ActionListener {
    private static final long serialVersionUID = 1L;
    private double longitud;
    private JButton aceptar = new JButton(
            I18NPlug.getI18N("org.openjump.core.ui.plugins.Dialog.Accept"));
    private JLabel label = new JLabel(
            I18NPlug.getI18N("org.openjump.core.ui.plugins.Dialog.Length"));
    private JButton cancelar = new JButton(
            I18NPlug.getI18N("org.openjump.core.ui.plugins.Dialog.Cancel"));

    private JFormattedTextField numberTextFiel;
    public boolean cancelado = true;

    public LengthDialog(JFrame parent, double length) {
        super(parent, I18NPlug
                .getI18N("org.openjump.core.ui.plugins.Dialog.Length"), true);
        this.setIconImage(org.openjump.advancedtools.icon.IconLoader
                .image("cadTools.png"));
        this.longitud = length;
        JPanel p1 = new JPanel();
        this.numberTextFiel = getUSFormatedNumberTextField(125);
        this.numberTextFiel.setValue(Double.valueOf(length));
        this.numberTextFiel.addActionListener(this);
        p1.add(this.label);
        p1.add(this.numberTextFiel);
        JPanel p2 = new JPanel();
        this.aceptar.addActionListener(this);
        this.cancelar.addActionListener(this);
        p2.add(this.aceptar);
        p2.add(this.cancelar);
        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(p1, "Center");
        getContentPane().add(p2, "South");
        pack();
        setLocationRelativeTo(parent);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if ((e.getSource().equals(this.aceptar))
                || (e.getSource().equals(this.numberTextFiel))) {
            this.cancelado = false;
            this.longitud = ((Number) this.numberTextFiel.getValue())
                    .doubleValue();
        }
        dispose();
    }

    public void actionPerformed1(ActionEvent e) {
        if ((e.getSource().equals(this.aceptar))
                || (e.getSource().equals(this.numberTextFiel))) {
            this.cancelado = true;
        }

        dispose();
    }

    public double getLength() {
        return this.longitud;
    }

    public void setLabel(String label) {
        this.label.setText(label);
    }

    @Override
    public void setTitle(String title) {
        super.setTitle(title);
    }

    public static JFormattedTextField getUSFormatedNumberTextField(int lenght) {
        NumberFormat format = NumberFormat.getInstance(Locale.US);
        format.setGroupingUsed(false);
        JFormattedTextField numberTextFiel = new JFormattedTextField(format);
        numberTextFiel.setPreferredSize(new Dimension(lenght, numberTextFiel
                .getPreferredSize().height));
        return numberTextFiel;
    }
}

/*
 * Location:
 * C:\Users\Beppe\Desktop\GIS\OpenJUMP-20150713-r4536-CORE\lib\ext\GA_CAD
 * .jar!\org\saig\jump\widgets\tools\editing\LengthDialog.class Java compiler
 * version: 5 (49.0) JD-Core Version: 0.7.1
 */