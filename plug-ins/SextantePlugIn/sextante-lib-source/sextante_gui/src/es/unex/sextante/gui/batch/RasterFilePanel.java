package es.unex.sextante.gui.batch;

import java.awt.Dimension;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JTable;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.algorithm.GenericFileFilter;
import es.unex.sextante.gui.core.SextanteGUI;

public class RasterFilePanel extends TextFieldAndButton
{
	private static final long serialVersionUID = 1L;

	private final JTable 		m_Table;

	private static int			FILE_BROWSER_WIDTH = 800;
	private static int			FILE_BROWSER_HEIGHT = 600;


	public RasterFilePanel(final JTable table) {

		m_Table = table;


	}


	@Override
	protected void btnActionPerformed() {

		final JFileChooser fc = new JFileChooser();
		
		//If there is already a file selection: start file browser in same dir.
		if ( textField.getText() != null ) {
			String path = null;			
			if ( Sextante.isWindows() ) {
				path = textField.getText().replaceAll("/","\\\\");
			} else {
				path = textField.getText();
			}
			fc.setCurrentDirectory(new File (path));
		}
		
		fc.setPreferredSize(new Dimension (FILE_BROWSER_WIDTH, FILE_BROWSER_HEIGHT));

		fc.setFileFilter(new GenericFileFilter(SextanteGUI.getInputFactory().getRasterLayerInputExtensions(),
				Sextante.getText("Capas_raster")));
		fc.setMultiSelectionEnabled(true);
		final int returnVal = fc.showOpenDialog(this.getParent().getParent());
		
		FILE_BROWSER_WIDTH = fc.getWidth();
		FILE_BROWSER_HEIGHT = fc.getHeight();

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			textField.setText(fc.getSelectedFile().getAbsolutePath());
			m_Table.setValueAt(textField.getText(), m_Table.getSelectedRow(), m_Table.getSelectedColumn());
		}


	}

}
