/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI 
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * JUMP is Copyright (C) 2003 Vivid Solutions
 *
 * This program implements extensions to JUMP and is
 * Copyright (C) 2010 Jorge Almaraz.
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * 
 * For more information, contact:
 *
 * Jukka Rahkonen
 * jukka.rahkonen@latuviitta.fi
 * 
 */
package org.jam.openjump.spatialiteplugin;

import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import jedit.JEditTextArea;
import jedit.TextAreaDefaults;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.sql.SQLException;
import java.util.ArrayList;

public class SpatialiteDialog extends JDialog  implements ActionListener {

    static String imgurl="/org/jam/openjump/spatialiteplugin/images/";

	private static final long serialVersionUID = 1L;
	private SpatialiteDb currentDb = null;
	private PlugInContext context = null;
	JToolBar toolbar;
	JButton bload;
	JButton  bsql;
	JButton bimplay;
	
	JSplitPane splitv;
	JSplitPane splith;
	JEditTextArea tsql;
	JTable tabla;
	JScrollPane scroll;
	
	JTree tree;
	DefaultMutableTreeNode rootnode;
	JScrollPane treeview;
	Tnode root =null;

	
	public SpatialiteDialog(PlugInContext ctx) {
		
		super(ctx.getWorkbenchFrame() ,"Spatialite Plugin",false);
		context=ctx;
		initComponents();
	}

	public SpatialiteDialog() {
		
		super();
		context=null;
		initComponents();
	}
	
	@SuppressWarnings("serial")
	private void initComponents() {
		currentDb = new SpatialiteDb();
        System.out.println("SpatialiteDialog.initComponents: " + currentDb);
		setSize(320, 240);
		this.setLayout(new BorderLayout());
		this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		
		toolbar=new JToolBar();
		toolbar.setFloatable(false);
		this.add(toolbar,BorderLayout.NORTH);
		toolbar.addSeparator();
		
		ImageIcon ico=new ImageIcon(getClass().getResource(imgurl+"database.png"));
		bload =new JButton(ico);
		bload.setToolTipText("Open Spatialite DB");
		bload.addActionListener(this);
		toolbar.add(bload);
		
		ico=new ImageIcon(getClass().getResource(imgurl+"runsql.png"));
		bsql =new JButton(ico);
		bsql.setToolTipText("Execute SQL query");
		bsql.setEnabled(false);
		bsql.addActionListener(this);
		toolbar.addSeparator();
		toolbar.add(bsql);
		
		ico=new ImageIcon(getClass().getResource(imgurl+"addlayer.png"));
		bimplay=new JButton(ico);
		bimplay.setToolTipText("Add layer");
		bimplay.setEnabled(false);
		bimplay.addActionListener(this);
		toolbar.addSeparator();
		toolbar.add(bimplay);
		
		tree=new JTree();
		tree.setRootVisible(true);
		tree.setToggleClickCount(3);
		tree.setEditable(false);
		
		treeview= new JScrollPane(tree);
		treeview.setPreferredSize(new Dimension(200,200));
		this.add(treeview,BorderLayout.WEST);
		tree.addMouseListener(new MouseAdapter() {
		     public void mousePressed(MouseEvent e) {
		    	tree_click( e); 
		     }	 
		});
		
		splitv=new JSplitPane();
		splitv.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		this.add(splitv,BorderLayout.CENTER);
		
		splith=new JSplitPane();
		splith.setOrientation(JSplitPane.VERTICAL_SPLIT);
		//this.add(split,BorderLayout.CENTER);
		splitv.setLeftComponent(treeview);
		splitv.setRightComponent(splith);
		
		TextAreaDefaults df =TextAreaDefaults.getDefaults();
		df.cols=60;
		df.rows=6;
		df.eolMarkers=false;
		df.paintInvalid=false;
		df.lineHighlight=false;
		tsql=new JEditTextArea(df);
		splith.setLeftComponent(tsql);
		
		tabla = new JTable()
		{
			public boolean isCellEditable(int row, int column)
			{
				return false;
			}
		};
		
		tabla.setAutoCreateRowSorter(true);
		tabla.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		scroll= new JScrollPane(tabla);
		scroll.setPreferredSize(new Dimension(500,300));
		splith.setRightComponent(scroll);
		this.pack();
		updateNodes();
	}
	
	@SuppressWarnings("unchecked")
	public void execsql(){
		Object[][] dat;
		ArrayList cols = new ArrayList();
		try {
			dat = currentDb.loadData(tsql.getText(),cols);
		} catch (SQLException e) {
			JOptionPane.showMessageDialog(null, e.getMessage());
			e.printStackTrace();
			return;
		}
		tabla.setModel(new DefaultTableModel(dat,cols.toArray()));
		TableColumn column = null;
		for (int i = 0; i < tabla.getColumnModel().getColumnCount(); i++) {
			column = tabla.getColumnModel().getColumn(i);
			column.setPreferredWidth(65);
		}
	}


	private void loadDb(){
        System.out.println("SpatialiteDialog.loadDb()");
		bsql.setEnabled(false);
		bimplay.setEnabled(false);
		this.setTitle("");
		FileDialog 	fd = new FileDialog(this,"Load DB",FileDialog.LOAD);
		fd.setVisible(true);
		String filename = fd.getFile();
		if (filename == null)	return;
		else{	
			filename = fd.getDirectory() + filename;
			if (currentDb.openDataBase(filename)){
				bsql.setEnabled(true);
				bimplay.setEnabled(true);
				this.setTitle(filename);
				updateNodes();
                System.out.println("update nodes");
				if (currentDb.getSpatialMetaData() == 2) {
					JOptionPane.showMessageDialog(null, "<html><h3>FDO-OGR detected.</h3><br>Activating FDO-OGR auto-wrapping</html>");
                }
			}	
		}	
	}
	
	@Override
	public void actionPerformed(ActionEvent ev)  {
		if (ev.getSource()==bsql) execsql();
		else if (ev.getSource()==bload) loadDb();
		else if (ev.getSource()==bimplay){ImportLayer.ImportSql(context,currentDb ,tsql.getText());}
		else if (ev.getSource()==tree) JOptionPane.showMessageDialog(this, "Vale");
		
	}
	public void updateNodes(){
        System.out.println("SpatialiteDialog.updateNodes()");
		if (root==null){
			root=Tnode.loadFromXml(); 
		}
		Tnode tablesNode =(Tnode) root.getChildAt(0);
		tablesNode.removeChilds();
		if (currentDb.isClosed()==false) currentDb.getTablesNames(tablesNode);
		tree.setRowHeight(0);
		tree.setModel(new DefaultTreeModel(root));
		tree.setCellRenderer(new RendererTree());
		tree.setToolTipText("");
		tsql.setTokenMarker(new SqliteTokenMarker(root) );
	}
	
	public void showmessage(String msg){
		JOptionPane.showMessageDialog(this,msg);
	}

	public void tree_click(MouseEvent e){ 
    int selRow = tree.getRowForLocation(e.getX(), e.getY());
    TreePath selPath = tree.getPathForLocation(e.getX(), e.getY());
    if(selRow != -1) {
        if(e.getClickCount() == 1) {//pass
        }
        else if(e.getClickCount() == 2) {
          Tnode nd =	(Tnode)selPath.getLastPathComponent();
          if (nd.cat>0){
        	  StringSelection strSel = new StringSelection( nd.code+" " );
        	  Clipboard cb = Toolkit.getDefaultToolkit().getSystemClipboard();
        	  cb.setContents(strSel, null);
        	  tsql.paste();
          }
        }
    }
}

	
	
}



@SuppressWarnings("serial")
class RendererTree extends DefaultTreeCellRenderer{
	static String htmlcab="<html><head><style type=\"text/css\">i {color:navy;}</style></head><body>%s</body></html>";
	ImageIcon iiFolder;
	ImageIcon iiGeoTable;
	ImageIcon iiGeoTablefdo;
	ImageIcon iiGeoTablevir;
	ImageIcon iiTable;
	ImageIcon iiFsql;
	ImageIcon iiFun;
	public RendererTree(){
		String imgurl="/org/jam/openjump/spatialiteplugin/images/";
		iiFolder=new ImageIcon(getClass().getResource(imgurl+"folder.png"));
		iiGeoTable=new ImageIcon(getClass().getResource(imgurl+"geotable.png"));
		iiGeoTablefdo=new ImageIcon(getClass().getResource(imgurl+"geotablefdo.png"));
		iiGeoTablevir=new ImageIcon(getClass().getResource(imgurl+"geotablevir.png"));
		iiTable=new ImageIcon(getClass().getResource(imgurl+"table.png"));
		iiFsql=new ImageIcon(getClass().getResource(imgurl+"fsql.png"));
		iiFun=new ImageIcon(getClass().getResource(imgurl+"fun.png"));
	}
	
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean hasFocus)
    {               
        super.getTreeCellRendererComponent(tree,value,selected,expanded,leaf,row,hasFocus);
        Tnode node=(Tnode)value;
        this.setToolTipText(String.format(htmlcab,  node.des));
        this.setText(node.caption);

      //0:GROUP;1:EXPR;2:FUN;3:geotable;4:table
        switch (node.cat) {
			case Tnode.CAT_GROUP: this.setIcon(iiFolder); break;
			case Tnode.CAT_EXPR: this.setIcon(iiFsql); break;
			case Tnode.CAT_FUN: this.setIcon(iiFun); break;
			case Tnode.CAT_GEOTABLE: this.setIcon(iiGeoTable);  break;
			case Tnode.CAT_GEOTABLE_VSHAPE: this.setIcon(iiGeoTablevir);  break;
			case Tnode.CAT_GEOTABLE_VFDO: this.setIcon(iiGeoTablevir);  break;
			case Tnode.CAT_GEOTABLE_FDO: this.setIcon(iiGeoTablefdo);  break;
			case Tnode.CAT_TABLE: this.setIcon(iiTable); break;
			default:break;	
		}

        return this;
    }

}

