package es.unex.sextante.gui.modeler;


import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;

import es.unex.sextante.gui.core.SextanteGUI;




public class ColorComboBox
{
	
	
	private static final Color colors[] = { 	
			  SextanteGUI.COLOR_WHITE,
			  SextanteGUI.COLOR_GREY,
			  SextanteGUI.COLOR_BLACK,
			  SextanteGUI.COLOR_YELLOW,
			  SextanteGUI.COLOR_ORANGE,
			  SextanteGUI.COLOR_RED,
			  SextanteGUI.COLOR_GREEN_LIGHT,
			  SextanteGUI.COLOR_GREEN_DARK,
			  SextanteGUI.COLOR_BLUE_LIGHT,
			  SextanteGUI.COLOR_BLUE_DARK,
			  SextanteGUI.COLOR_PINK,			  
			  SextanteGUI.COLOR_PURPLE,
			  SextanteGUI.COLOR_BROWN_LIGHT,
			  SextanteGUI.COLOR_BROWN_DARK
	};
	
	private JComboBox comboBox;
	private ColorCellRenderer renderer;
	

	public ColorComboBox() {

		comboBox = new JComboBox(colors);
		comboBox.setMaximumRowCount(colors.length);
		comboBox.setEditable(false);
		renderer =  new ColorCellRenderer();
		comboBox.setRenderer(renderer);
	}	
	
	
	public JComboBox getComboBox() {
		return ( comboBox );
	}
	
	
	public ColorCellRenderer getRenderer() {
		return ( renderer );
	}	
	
	
	static class ColorCellRenderer implements ListCellRenderer {
		protected DefaultListCellRenderer defaultRenderer = new DefaultListCellRenderer();

		private final static Dimension preferredSize = new Dimension(0, 20);

		public Component getListCellRendererComponent(JList list, Object value,
				int index, boolean isSelected, boolean cellHasFocus) {
			JLabel renderer = (JLabel) defaultRenderer
			.getListCellRendererComponent(list, value, index,
					isSelected, cellHasFocus);
			if (value instanceof Color) {
				renderer.setBackground((Color) value);
			}
			renderer.setText("");
			renderer.setPreferredSize(preferredSize);
			return renderer;
		}
	}

}
