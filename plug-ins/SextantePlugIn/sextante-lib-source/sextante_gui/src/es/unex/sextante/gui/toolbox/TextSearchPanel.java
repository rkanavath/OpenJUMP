package es.unex.sextante.gui.toolbox;

import info.clearthought.layout.TableLayout;
import info.clearthought.layout.TableLayoutConstants;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTextField;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;

/**
 * A panel with a text box to enter search strings, which filters the list of
 * algorithms in the toolbox
 * 
 * @author volaya
 * 
 */
public class TextSearchPanel extends JPanel {

    private final ToolboxPanel m_Panel;
    private JButton jButtonSearch;
    private JTextField jTextField;
    private JButton jButtonHelp;
    private JButton jButtonSettings;
    private final JDialog m_Parent;

    /**
     * Constructor
     * 
     * @param panel
     *            The toolbox panel
     * @param parent
     *            the parent dialog
     */
    public TextSearchPanel(final ToolboxPanel panel, final JDialog parent) {

        m_Panel = panel;
        m_Parent = parent;

        initGUI();

    }

    private void initGUI() {

        try {
            {
                final TableLayout thisLayout = new TableLayout(new double[][] {
                        { 7.0, TableLayoutConstants.MINIMUM,
                                TableLayoutConstants.FILL, 10.0,
                                TableLayoutConstants.MINIMUM,
                                TableLayoutConstants.MINIMUM, 7.0 },
                        { TableLayoutConstants.FILL,
                                TableLayoutConstants.MINIMUM,
                                TableLayoutConstants.FILL } });
                thisLayout.setHGap(5);
                thisLayout.setVGap(5);
                this.setLayout(thisLayout);
                {
                    jButtonHelp = new JButton();
                    this.add(jButtonHelp, "4, 1");
                    jButtonHelp.setIcon(new ImageIcon(getClass()
                            .getClassLoader().getResource("images/info.gif")));
                    jButtonHelp
                            .setPreferredSize(new java.awt.Dimension(20, 20));
                    jButtonHelp.addActionListener(new ActionListener() {
                        public void actionPerformed(final ActionEvent evt) {
                            showHelp();
                        }
                    });
                    jButtonSettings = new JButton();
                    this.add(jButtonSettings, "5, 1");
                    jButtonSettings.setIcon(new ImageIcon(getClass()
                            .getClassLoader().getResource(
                                    "images/wrench-screwdriver.png")));
                    jButtonSettings.setPreferredSize(new java.awt.Dimension(20,
                            20));
                    jButtonSettings.addActionListener(new ActionListener() {
                        public void actionPerformed(final ActionEvent evt) {
                            showSettings(evt);
                        }
                    });
                }
                {
                    jTextField = new JTextField();
                    this.add(jTextField, "2, 1");
                    jTextField.addKeyListener(new KeyAdapter() {
                        @Override
                        public void keyTyped(final KeyEvent event) {
                            processKeyPresssed(event);
                        }
                    });
                }
                {
                    jButtonSearch = new JButton();
                    this.add(jButtonSearch, "1, 1");
                    jButtonSearch.setText(Sextante.getText("Search"));
                    jButtonSearch.setPreferredSize(new java.awt.Dimension(66,
                            20));
                    jButtonSearch.addActionListener(new ActionListener() {
                        public void actionPerformed(final ActionEvent evt) {
                            final String sString = jTextField.getText().trim()
                                    .toLowerCase();
                            searchString(sString, true);
                        }
                    });
                }
            }
        } catch (final Exception e) {
            Sextante.addErrorToLog(e);
        }
    }

    protected void processKeyPresssed(final KeyEvent event) {

        String sString;
        switch (event.getKeyChar()) {
        case KeyEvent.VK_ENTER:
            sString = jTextField.getText().trim().toLowerCase();
            searchString(sString, true);
            break;
        default:
            sString = jTextField.getText() + event.getKeyChar();
            sString = sString.trim().toLowerCase();
            searchString(sString, false);
            break;
        }

    }

    protected void showSettings(final ActionEvent evt) {

        SextanteGUI.getGUIFactory().showSettingsDialog(m_Panel, m_Parent);

    }

    protected void showHelp() {

        SextanteGUI.getGUIFactory().showHelpDialog("toolbox");

    }

    protected void searchString(final String sString,
            final boolean bSearchInHelpFiles) {

        if (sString.equals("")) {
            m_Panel.fillTreesWithAllAlgorithms();
        } else {
            m_Panel.fillTreesWithSelectedAlgorithms(sString, bSearchInHelpFiles);
        }

    }

}
