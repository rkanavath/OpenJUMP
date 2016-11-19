package es.unex.sextante.gui.algorithm;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JDialog;
import javax.swing.JTabbedPane;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.outputs.Output;
import es.unex.sextante.outputs.Output3DRasterLayer;
import es.unex.sextante.outputs.OutputRasterLayer;
import es.unex.sextante.outputs.OutputVectorLayer;

public class OutputChannelSelectionDialog extends JDialog implements ActionListener
{
	private static final long serialVersionUID = 1L;
	
	public static final int                     TYPE_RASTER   = 0;
	public static final int                     TYPE_VECTOR   = 1;
	public static final int                     TYPE_TABLE    = 2;
	public static final int                     TYPE_RASTER3D = 3;

	private static int							DLG_WIDTH = 800;
	private static int							DLG_HEIGHT = 600;
	private static String						LAST_DIR = null;
	
	
	private String                              m_sOutputChannel;
	private GeneralOptionsChannelSelectionPanel m_GeneralOptionsChannelSelectionPanel;
	private FileOutputChannelSelectionPanel     m_FileOutputChannelSelectionPanel;
	//TODO: Add support for database output
	//private DatabaseOutputChannelSelectionPanel m_DatabaseOutputChannelSelectionPanel;
	private final Output                        m_Output;


	public OutputChannelSelectionDialog(final Output out) {

		super(SextanteGUI.getMainFrame(), true);

		m_Output = out;

		initGUI();

		this.pack();
		setLocationRelativeTo(null);

	}


	private void initGUI() {

		this.setResizable(true);
		this.setPreferredSize(new Dimension(DLG_WIDTH, DLG_HEIGHT));
		this.getContentPane().setLayout(new BorderLayout());

		final JTabbedPane tabbedPane = new JTabbedPane();

		m_GeneralOptionsChannelSelectionPanel = new GeneralOptionsChannelSelectionPanel(m_Output, this);

		String[] sExt;
		String sDesc;
		int iType;
		if (m_Output instanceof OutputRasterLayer) {
			sExt = SextanteGUI.getOutputFactory().getRasterLayerOutputExtensions();
			sDesc = Sextante.getText(Sextante.getText("Raster_layers"));
			iType = TYPE_RASTER;
		}
		else if (m_Output instanceof Output3DRasterLayer) {
			sExt = SextanteGUI.getOutputFactory().get3DRasterLayerOutputExtensions();
			sDesc = Sextante.getText(Sextante.getText("3D_Raster_layers"));
			iType = TYPE_RASTER3D;
		}
		else if (m_Output instanceof OutputVectorLayer) {
			sExt = SextanteGUI.getOutputFactory().getVectorLayerOutputExtensions();
			sDesc = Sextante.getText(Sextante.getText("Vector_layer"));
			iType = TYPE_VECTOR;
		}
		else {
			sExt = SextanteGUI.getOutputFactory().getTableOutputExtensions();
			sDesc = Sextante.getText(Sextante.getText("Tables"));
			iType = TYPE_TABLE;
		}
		m_FileOutputChannelSelectionPanel = new FileOutputChannelSelectionPanel(sDesc, sExt, this, LAST_DIR);
		tabbedPane.addTab(Sextante.getText("File"), m_FileOutputChannelSelectionPanel);

		tabbedPane.addTab(Sextante.getText("General"), m_GeneralOptionsChannelSelectionPanel);      

		if (iType == TYPE_VECTOR) {
			//TODO: Add support for database output
			//m_DatabaseOutputChannelSelectionPanel = new DatabaseOutputChannelSelectionPanel(sDesc, this);
			//tabbedPane.addTab(Sextante.getText("Database"), m_DatabaseOutputChannelSelectionPanel);
		}

		this.getContentPane().add(tabbedPane);

	}


	public String getOutputChannelString() {

		return m_sOutputChannel;

	}


	public void actionPerformed(final ActionEvent action) {

		/* Save dialog state */
		OutputChannelSelectionDialog.DLG_WIDTH = this.getWidth();
		OutputChannelSelectionDialog.DLG_HEIGHT = this.getHeight();		
		
		if (action.getActionCommand().equals("CancelSelection")) {
			m_sOutputChannel = null;			
			this.setVisible(false);
			this.dispose();
		}
		else if (action.getActionCommand().equals("ApproveSelection")) {
			m_sOutputChannel = m_FileOutputChannelSelectionPanel.getSelectedFile();
			if ( m_sOutputChannel != null ) {
				// Store this folder as default folder for file browser instance.
				LAST_DIR = new File ( m_sOutputChannel ).getAbsolutePath();
			}
			// Add default file extension in case none was given.
			if (m_Output instanceof OutputRasterLayer) {
				m_sOutputChannel = SextanteGUI.checkAddExtension(m_sOutputChannel, ".tif");
			}
			if (m_Output instanceof OutputVectorLayer) {
				m_sOutputChannel = SextanteGUI.checkAddExtension(m_sOutputChannel, ".shp");
			}			
			this.setVisible(false);
			this.dispose();
		}
		//FIXME: The "overwrite" option does not seem to get used.
		else if (action.getActionCommand().equals(GeneralOptionsChannelSelectionPanel.OVERWRITE)) {
			m_sOutputChannel = Sextante.getText("[Overwrite]");
			this.setVisible(false);
			this.dispose();
		}
		else if (action.getActionCommand().equals(GeneralOptionsChannelSelectionPanel.SAVE_TO_TEMP_FILE)) {
			m_sOutputChannel = Sextante.getText("[Save_to_temporary_file]");
			this.setVisible(false);
			this.dispose();
		}
		else if (action.getActionCommand().equals(GeneralOptionsChannelSelectionPanel.DO_NOT_CREATE_OUTPUT)) {
			m_sOutputChannel = Sextante.getText("[Do_not_create_output]");
			this.setVisible(false);
			this.dispose();
		}

	}
}
