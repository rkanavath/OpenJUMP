package es.unex.sextante.gui.algorithm;

import java.awt.BorderLayout;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JPanel;

import es.unex.sextante.gui.core.SextanteGUI;

public class FileOutputChannelSelectionPanel extends JPanel
{
	private static final long serialVersionUID = 1L;

	private final ActionListener	m_Listener;
	private final String       		m_sDescription;
	private final String[]    		m_sExtensions;
	private JFileChooser       		m_Chooser;

	private String					m_LastPath = null;


	public FileOutputChannelSelectionPanel(final String sDescription,
			final String[] sExtensions,
			final ActionListener listener,
			final String initialDir ) {

		m_sDescription = sDescription;
		m_sExtensions = sExtensions;
		m_Listener = listener;
		m_LastPath = initialDir;
		initGUI();

	}


	private void initGUI() {

		this.setLayout(new BorderLayout());
		m_Chooser = new JFileChooser();
		m_Chooser.setDialogType(JFileChooser.SAVE_DIALOG);
		m_Chooser.setFileFilter(new GenericFileFilter(m_sExtensions, m_sDescription));
		if ( m_LastPath != null ) {
			m_Chooser.setCurrentDirectory(new File (m_LastPath));
		} else {
			m_Chooser.setCurrentDirectory(new File (SextanteGUI.getOutputFolder()));
		}
		m_Chooser.addActionListener(m_Listener);
		this.add(m_Chooser);

	}


	public String getSelectedFile() {
		return m_Chooser.getSelectedFile().getAbsolutePath();
	}


}
