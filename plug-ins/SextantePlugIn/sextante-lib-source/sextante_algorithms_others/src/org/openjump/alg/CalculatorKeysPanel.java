package org.openjump.alg;

import info.clearthought.layout.TableLayout;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextArea;

public class CalculatorKeysPanel extends JPanel {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private JTextArea m_TextExpression = null;
    private JButton jButtonMinus;
    private JButton jButtonDivide;
    private JButton jButton2;
    private JButton jButtonDot;
    private JButton jButtonBrackets;
    private JButton jButton0;
    private JButton jButton9;
    private JButton jButton8;
    private JButton jButton7;
    private JButton jButton6;
    private JButton jButton5;
    private JButton jButton4;
    private JButton jButton3;
    private JButton jButton1;
    private JButton jButtonMultiply;
    private JButton jButtonPlus;

    public CalculatorKeysPanel(JTextArea textExpression) {
        this.m_TextExpression = textExpression;

        initialize();
    }

    private void initialize() {
        ActionListener listener = new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                CalculatorKeysPanel.this.addText(evt.getSource());
            }
        };
        ActionListener listenerBrackets = new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                CalculatorKeysPanel.this.m_TextExpression.insert(" ()",
                        CalculatorKeysPanel.this.m_TextExpression
                                .getCaretPosition());
                CalculatorKeysPanel.this.m_TextExpression
                        .setCaretPosition(CalculatorKeysPanel.this.m_TextExpression
                                .getCaretPosition() - 1);
            }
        };
        TableLayout thisLayout = new TableLayout(new double[][] {
                { -1.0D, 10.0D, -1.0D, -1.0D, -1.0D },
                { -1.0D, -1.0D, -1.0D, -1.0D } });

        thisLayout.setHGap(5);
        thisLayout.setVGap(5);
        setLayout(thisLayout);
        setPreferredSize(new Dimension(284, 239));

        this.jButtonPlus = new JButton();
        add(this.jButtonPlus, "0, 0");
        this.jButtonPlus.setText("+");
        this.jButtonPlus.addActionListener(listener);

        this.jButtonMinus = new JButton();
        add(this.jButtonMinus, "0, 1");
        this.jButtonMinus.setText("-");
        this.jButtonMinus.addActionListener(listener);

        this.jButtonMultiply = new JButton();
        add(this.jButtonMultiply, "0, 2");
        this.jButtonMultiply.setText("*");
        this.jButtonMultiply.addActionListener(listener);

        this.jButtonDivide = new JButton();
        add(this.jButtonDivide, "0, 3");
        this.jButtonDivide.setText("/");
        this.jButtonDivide.addActionListener(listener);

        this.jButton1 = new JButton();
        add(this.jButton1, "2, 2");
        this.jButton1.setText("1");
        this.jButton1.addActionListener(listener);

        this.jButton2 = new JButton();
        add(this.jButton2, "3, 2");
        this.jButton2.setText("2");
        this.jButton2.addActionListener(listener);

        this.jButton3 = new JButton();
        add(this.jButton3, "4, 2");
        this.jButton3.setText("3");
        this.jButton3.addActionListener(listener);

        this.jButton4 = new JButton();
        add(this.jButton4, "2, 1");
        this.jButton4.setText("4");
        this.jButton4.addActionListener(listener);

        this.jButton5 = new JButton();
        add(this.jButton5, "3, 1");
        this.jButton5.setText("5");
        this.jButton5.addActionListener(listener);

        this.jButton6 = new JButton();
        add(this.jButton6, "4, 1");
        this.jButton6.setText("6");
        this.jButton6.addActionListener(listener);

        this.jButton7 = new JButton();
        add(this.jButton7, "2, 0");
        this.jButton7.setText("7");
        this.jButton7.addActionListener(listener);

        this.jButton8 = new JButton();
        add(this.jButton8, "3, 0");
        this.jButton8.setText("8");
        this.jButton8.addActionListener(listener);

        this.jButton9 = new JButton();
        add(this.jButton9, "4, 0");
        this.jButton9.setText("9");
        this.jButton9.addActionListener(listener);

        this.jButton0 = new JButton();
        add(this.jButton0, "2, 3");
        this.jButton0.setText("0");
        this.jButton0.addActionListener(listener);

        this.jButtonDot = new JButton();
        add(this.jButtonDot, "4, 3");
        this.jButtonDot.setText(".");
        this.jButtonDot.addActionListener(listener);

        this.jButtonBrackets = new JButton();
        add(this.jButtonBrackets, "3, 3");
        this.jButtonBrackets.setText("( )");
        this.jButtonBrackets.addActionListener(listenerBrackets);
    }

    private void addText(Object source) {
        if ((source instanceof JButton)) {
            String s = ((JButton) source).getText();
            try {
                final int i = Integer.parseInt(s);
            } catch (NumberFormatException e) {

                s = " " + s + " ";
            }
            this.m_TextExpression.insert(s,
                    this.m_TextExpression.getCaretPosition());
        }
    }
}
