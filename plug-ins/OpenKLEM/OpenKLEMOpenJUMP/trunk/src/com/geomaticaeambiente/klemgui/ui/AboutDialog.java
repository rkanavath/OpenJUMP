package com.geomaticaeambiente.klemgui.ui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;

import com.geomaticaeambiente.klemgui.utils.PluginUtils;

class AboutDialog extends JDialog {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public AboutDialog() {

        setTitle("About"); // bundle.getString("InitialDialog.jButton_About.text"));
                           // //NOI18N
        setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
        setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);

        getContentPane().setBackground(Color.WHITE);

        add(Box.createRigidArea(new Dimension(0, 10)));

        final JLabel titleLabel = new JLabel(PluginUtils.plugInName);
        titleLabel.setFont(new Font("monospaced", Font.BOLD, 15));
        titleLabel.setAlignmentX(0.5f);

        // final DateFormat date = new SimpleDateFormat("yyyyMMdd");
        final JLabel titleLabel2 = new JLabel(PluginUtils.version + ": "

        + PluginUtils.versionNumber);
        titleLabel2.setFont(new Font("monospaced", Font.BOLD, 13));
        titleLabel2.setAlignmentX(0.5f);

        final JTextArea textArea = new JTextArea(java.util.ResourceBundle
                .getBundle("com/geomaticaeambiente/klemgui/resources/Bundle")
                .getString("AboutDialog.TextArea.text"));
        textArea.setLineWrap(true);
        textArea.setWrapStyleWord(true);
        textArea.setAlignmentX(0.5f);
        textArea.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
        final Font font = new Font("monospaced", Font.PLAIN, 12); // NOI18N
        textArea.setFont(font);
        textArea.setEditable(false);

        final JPanel readmeP = (JPanel) getContentPane();

        readmeP.add(titleLabel, BorderLayout.NORTH);
        readmeP.add(titleLabel2, BorderLayout.CENTER);
        readmeP.add(textArea, BorderLayout.SOUTH);

        add(Box.createRigidArea(new Dimension(0, 10)));

        final JButton close = new JButton(java.util.ResourceBundle.getBundle(
                "com/geomaticaeambiente/klemgui/resources/Bundle").getString(
                "InitialDialog.jButton_Close.text"));
        close.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent event) {
                dispose();
            }
        });

        close.setAlignmentX(0.5f);
        add(close);
        setModalityType(ModalityType.APPLICATION_MODAL);
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        setSize(600, 250);
        setResizable(false);
        // pack();
    }

    ResourceBundle bundle = java.util.ResourceBundle
            .getBundle("com/geomaticaeambiente/klemgui/resources/Bundle"); // NOI18N

}