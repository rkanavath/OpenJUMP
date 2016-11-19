package es.unex.sextante.gui.settings;

import info.clearthought.layout.TableLayout;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.TableModel;
import javax.swing.table.TableColumn;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.toolbox.AlgorithmGroupConfiguration;
import es.unex.sextante.gui.toolbox.AlgorithmGroupsOrganizer;

public class AlgorithmGroupsConfigurationDialog
extends
JDialog {


	private HashMap<String, AlgorithmGroupConfiguration> m_Map;
	private JButton                                      jButtonCancel;
	private JButton                                      jButtonRestore;
	private JTable                                       jTable;
	private JScrollPane                                  jScrollPane;
	private JButton                                      jButtonOK;
	private boolean                                      m_bIsDefaultSettings;


	public AlgorithmGroupsConfigurationDialog() {

		super(SextanteGUI.getMainFrame(), true);
		initGUI();

	}


	private void initGUI() {

		m_Map = AlgorithmGroupsOrganizer.getGrouppingMap();

		final TableLayout thisLayout = new TableLayout(new double[][] {
				{ 3.0, 
					TableLayout.MINIMUM, // 1: Restore Default
					TableLayout.FILL, // 2: Filler
					90.0, // 3: Cancel
					6.0, // 4: Spacer
					90.0, // 5: Ok
					3.0 },
				{ 3.0, TableLayout.FILL, 3.0, 30.0, 3.0 } });
		thisLayout.setHGap(5);
		thisLayout.setVGap(5);
		getContentPane().setLayout(thisLayout);
		
		{
			jButtonRestore = new JButton();
			getContentPane().add(jButtonRestore, "1, 3");
			jButtonRestore.setText(Sextante.getText("RestoreDefault"));
			jButtonRestore.addActionListener(new ActionListener() {
				public void actionPerformed(final ActionEvent evt) {
					jButtonRestoreActionPerformed(evt);
				}
			});
		}		
		{
			jButtonCancel = new JButton();
			getContentPane().add(jButtonCancel, "3, 3");
			jButtonCancel.setText(Sextante.getText("Cancel"));
			jButtonCancel.addActionListener(new ActionListener() {
				public void actionPerformed(final ActionEvent evt) {
					jButtonCancelActionPerformed(evt);
				}
			});
		}
		{
			jButtonOK = new JButton();
			getContentPane().add(jButtonOK, "5, 3");
			jButtonOK.setText(Sextante.getText("OK"));
			jButtonOK.addActionListener(new ActionListener() {
				public void actionPerformed(final ActionEvent evt) {
					jButtonOKActionPerformed(evt);
				}
			});
		}		
		{
			jScrollPane = new JScrollPane();
			getContentPane().add(jScrollPane, "1, 1, 5, 1");
			TableModel jTableModel;
			if (m_Map.size() == 0) {
				jTableModel = new AlgorithmGroupsConfigurationTableModel(this);
				m_bIsDefaultSettings = true;
			}
			else {
				jTableModel = new AlgorithmGroupsConfigurationTableModel(m_Map, this);
			}
			jTable = new JTable();
			jTable.getTableHeader().setReorderingAllowed(false);
			jScrollPane.setViewportView(jTable);
			jTable.setModel(jTableModel);
			TableColumn column = null;
			/* Set column widths. */
			for (int i = 0; i < 5; i++) {
			    column = jTable.getColumnModel().getColumn(i);
			    column.setPreferredWidth(100);
			    if ( i == 2 || i == 4 ) {
			        column.setPreferredWidth(50);
			    }
			    if ( i == 1 ) {
			    	column.setPreferredWidth(150);
			    }
			}			

		}
		{
			this.setTitle(Sextante.getText("Tools")+ " - " + Sextante.getText("ConfigureAlgGroups"));
			this.setSize(800, 500);			
			//this.pack();
			this.setLocationRelativeTo(null);
		}


	}


	public HashMap<String, AlgorithmGroupConfiguration> getGrouppingsMap() {

		return m_Map;

	}


	private void jButtonOKActionPerformed(final ActionEvent evt) {

		m_Map.clear();
		if (!m_bIsDefaultSettings) {
			for (int i = 0; i < jTable.getRowCount(); i++) {
				final AlgorithmGroupConfiguration conf = new AlgorithmGroupConfiguration();
				conf.setGroup(jTable.getValueAt(i, 2).toString());
				conf.setSubgroup(jTable.getValueAt(i, 3).toString());
				conf.setShow(((Boolean) jTable.getValueAt(i, 4)).booleanValue());
				m_Map.put(jTable.getValueAt(i, 0).toString(), conf);
			}
		}
		this.dispose();
		this.setVisible(false);

	}


	private void jButtonCancelActionPerformed(final ActionEvent evt) {

		m_Map = null;
		this.dispose();
		this.setVisible(false);


	}


	private void jButtonRestoreActionPerformed(final ActionEvent evt) {

		m_Map.clear();
		final TableModel jTableModel = new AlgorithmGroupsConfigurationTableModel(this);
		jTable.setModel(jTableModel);
		m_bIsDefaultSettings = true;

	}


	public void hasBeenModified() {

		m_bIsDefaultSettings = false;

	}


}
