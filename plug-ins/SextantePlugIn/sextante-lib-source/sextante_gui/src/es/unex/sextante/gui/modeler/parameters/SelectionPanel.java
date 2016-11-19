package es.unex.sextante.gui.modeler.parameters;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.util.Arrays;

import info.clearthought.layout.TableLayout;
import info.clearthought.layout.TableLayoutConstants;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import es.unex.sextante.additionalInfo.AdditionalInfoSelection;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.exceptions.NullParameterAdditionalInfoException;
import es.unex.sextante.gui.modeler.ModelerPanel;
import es.unex.sextante.gui.modeler.SelectionAndChoices;
import es.unex.sextante.parameters.Parameter;
import es.unex.sextante.parameters.ParameterSelection;

public class SelectionPanel
extends
ParameterPanel {

	private SelectionTreePanel tree;
	private String searchArgument;
	private TreePath selectedPath = null;


	public SelectionPanel(final JDialog parent,
			final ModelerPanel panel) {

		super(parent, panel);

	}


	public SelectionPanel(final ModelerPanel panel) {

		super(panel);

	}


	@Override
	public String getParameterDescription() {

		return Sextante.getText("option_setting");

	}


	@Override
	protected void initGUI() {

		super.initGUI();

		super.setTitle(Sextante.getText("modeler_add_par_selection"));

		super.setPreferredSize(new java.awt.Dimension(400, 540));      

		super.setResizable(true);

		final TableLayout thisLayout = new TableLayout(new double[][] {
				{ 	TableLayoutConstants.FILL },	
				{ TableLayoutConstants.FILL} });
		thisLayout.setHGap(5);
		thisLayout.setVGap(5);
		jPanelMiddle.setLayout(thisLayout);		

		try {
			tree = new SelectionTreePanel();
			super.jButtonOk.setEnabled(false);
			final JButton jButtonOk = super.jButtonOk;
			final JPanel jPanel = super.m_ModelerPanel;
			tree.getTree().addTreeSelectionListener(new TreeSelectionListener() {				
				public void valueChanged(TreeSelectionEvent e) {
					DefaultMutableTreeNode node = (DefaultMutableTreeNode)
					tree.getTree().getLastSelectedPathComponent();
					if (node == null) {
						jButtonOk.setEnabled(false);
					} else {					
						Object nodeInfo = node.getUserObject();
						if (node.isLeaf()) {
							jButtonOk.setEnabled(true);
						} else {
							jButtonOk.setEnabled(false);
						}
					}
					jPanel.repaint();
				}
			});

			jPanelMiddle.add(tree, "0, 0");
		}
		catch (final Exception e) {
			Sextante.addErrorToLog(e);
		}

	}


	@Override
	protected boolean prepareParameter() {


		final String sDescription = jTextFieldDescription.getText();		

		if (sDescription.length() != 0) {
			m_Parameter = new ParameterSelection();
			final SelectionAndChoices sac = tree.getSelectedList();
			if (sac != null) {
				final AdditionalInfoSelection ai = new AdditionalInfoSelection(sac.getChoices());
				String sSelPath = tree.getTree().getSelectionPath().toString();				
				ai.setSelectionPath(sSelPath);
				m_Parameter.setParameterAdditionalInfo(ai);
				if (sDescription.trim().equals("")) {
					m_Parameter.setParameterDescription(sac.getDescription());
				}
				else {
					m_Parameter.setParameterDescription(sDescription);
				}

				m_Parameter.setColorR(m_Color.getRed());        
				m_Parameter.setColorG(m_Color.getGreen());        
				m_Parameter.setColorB(m_Color.getBlue());        
				m_Parameter.setColorAlpha(m_Color.getAlpha());				

				return true;
			}
			else {
				return false;
			}
		}
		else {
			JOptionPane.showMessageDialog(null, Sextante.getText("Invalid_description"), Sextante.getText("Warning"),
					JOptionPane.WARNING_MESSAGE);
			return false;
		}


	}

	
	private void walk(final DefaultTreeModel model, final Object o) {
		int cc;
		
		if ( searchArgument == null )
			return;
		
		cc = model.getChildCount(o);
		for (int i = 0; i < cc; i++) {
			DefaultMutableTreeNode child = (DefaultMutableTreeNode) model.getChild(o, i);
			if (model.isLeaf(child)) {
				String path = Arrays.toString(child.getPath());				
				if (searchArgument.equals(path.substring(1, path.length() - 1))) {
					selectedPath = new TreePath(model.getPathToRoot(child));
					searchArgument = null;
					break;
				}
			} else {
				walk(model, child);
			}
		}
	}

	
	@Override
	public void setParameter(final Parameter param) {

		super.setParameter(param);

		try {
			if ( param != null ) {
				final AdditionalInfoSelection ai = (AdditionalInfoSelection) param.getParameterAdditionalInfo();
				if ( ai != null && ai.getSelectionPath() != null ) {
					if ( ai.getSelectionPath().length() > 0 ) {
						searchArgument = ai.getSelectionPath();
						searchArgument = searchArgument.replace("[", "");
						searchArgument = searchArgument.replace("]", "");
						DefaultTreeModel model = (DefaultTreeModel) tree.getTree().getModel();
						selectedPath = null;
						walk(model, model.getRoot());
						if ( selectedPath != null ) {
							super.jButtonOk.setEnabled(true);
							tree.getTree().expandPath(selectedPath);
							tree.getTree().makeVisible(selectedPath);
							tree.getTree().setSelectionPath(selectedPath);
							tree.getTree().scrollPathToVisible(selectedPath.getParentPath());							
						}
					}
				}
			}
		}
		catch (final NullParameterAdditionalInfoException e) {}		
	}


	@Override
	public boolean parameterCanBeAdded() {

		return true;

	}

}
