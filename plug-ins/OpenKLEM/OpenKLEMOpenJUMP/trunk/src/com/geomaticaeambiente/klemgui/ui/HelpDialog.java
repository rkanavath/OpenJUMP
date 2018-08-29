package com.geomaticaeambiente.klemgui.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.StyledDocument;

import com.geomaticaeambiente.klemgui.utils.PluginUtils;

/**
 *
 * @author Geomatica
 */
public class HelpDialog extends JDialog {

    public HelpDialog(JDialog parent, boolean modal)
            throws BadLocationException {
        super(parent, modal);
    }

    public void initComponents(StyledDocument styledDocument)
            throws BadLocationException {

        setTitle(PluginUtils.getResources().getString(
                "PersonalRasterCombPanel.jToggleButton_Help.text"));

        GridBagConstraints gridBagConstraints = new GridBagConstraints();
        final JPanel jPanel_Help = new JPanel();
        jPanel_Help.setLayout(new GridBagLayout());

        final JScrollPane jScrollPane_Help = new JScrollPane();
        final JTextPane jTextPane_Help = new JTextPane();
        jTextPane_Help.setEditable(false);
        jTextPane_Help.setStyledDocument(styledDocument);

        jScrollPane_Help.setViewportView(jTextPane_Help);

        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        gridBagConstraints.anchor = GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new Insets(2, 2, 2, 2);
        jPanel_Help.add(jScrollPane_Help, gridBagConstraints);

        gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new Insets(11, 10, 11, 10);
        this.add(jPanel_Help);

        // jTextArea1.setText("Trallalero trallal'");

        setType(Type.UTILITY);
        setUndecorated(false);
        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        pack();
    }

}
