package es.unex.sextante.gui.core;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;

import es.unex.sextante.core.Sextante;

/**
 * A simple dialog to show a java component
 * 
 * @author volaya
 * 
 */
public class GenericInfoDialog extends JDialog {

    /**
     * constructor
     * 
     * @param component
     *            the component to show in the dialog
     * @param sTitle
     *            the title of the dialog
     * @param parent
     *            the parent frame
     */
    public GenericInfoDialog(final Component component, final String sTitle,
            final Frame parent) {

        super(parent, sTitle, true);

        initGUI(component, this);

        pack();
        setLocationRelativeTo(null);

    }

    protected void initGUI(final Component component, final JDialog dlg) {
        setPreferredSize(component.getPreferredSize());
        setSize(component.getPreferredSize());
        final BorderLayout thisLayout = new BorderLayout();
        final JPanel pane = new JPanel();
        pane.setLayout(thisLayout);
        setContentPane(pane);
        pane.add(component, BorderLayout.CENTER);
        // Add "Dismiss" button
        final JButton dismiss = new JButton(
                Sextante.getText("sextante_dlg_info_dismiss"));
        dismiss.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent arg0) {
                dlg.dispose();
            }
        });
        pane.add(dismiss, BorderLayout.PAGE_END);
    }

}
