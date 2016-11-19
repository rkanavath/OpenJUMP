package es.unex.sextante.gui.toolbox;

import java.awt.Frame;
import java.net.URL;

import javax.swing.ImageIcon;
import javax.swing.JDialog;

import es.unex.sextante.core.Sextante;

/**
 * A dialog to show a toolbox
 * 
 * @author volaya
 * 
 */
public class ToolboxDialog extends JDialog implements IToolboxDialog {

    /**
	 * 
	 */
    private static final long serialVersionUID = -6608836827062468343L;
    private ToolboxPanel m_Panel;

    /**
     * Constructor
     * 
     * @param parent
     *            the parent frame
     */
    public ToolboxDialog(final Frame parent) {

        super(parent, "SEXTANTE", true);

        this.setResizable(false);

        initialize();

        // this.setLocationRelativeTo(null);
    }

    public void initialize() {

        ImageIcon img;
        final URL res = getClass().getClassLoader().getResource(
                "images/sextante_toolbox.gif");
        if (res != null) {
            img = new ImageIcon(res);
        } else {
            img = null;
        }

        m_Panel = new ToolboxPanel(this, null);
        this.setContentPane(m_Panel);

        m_Panel.fillTreesWithAllAlgorithms();

    }

    /**
     * Returns the toolbox panel contained in this dialog
     * 
     * @return the toolbox panel contained in this dialog
     */
    public ToolboxPanel getToolboxPanel() {

        return m_Panel;

    }

    public void setAlgorithmsCount(final int iCount) {

        setTitle(Sextante.getText("Processing") + " - "
                + Sextante.getText("Toolbox") + " (" + Integer.toString(iCount)
                + Sextante.getText(" Tools") + ")");

    }

    public JDialog getDialog() {

        return this;

    }

}
