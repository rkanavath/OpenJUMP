package org.openjump.sigle.plugin.tutorial;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.io.IOException;
import java.net.URL;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;

import com.vividsolutions.jump.workbench.ui.images.IconLoader;

public class HelpDialog extends JPanel implements TreeSelectionListener {
    /**
     * Class adapted from HelpOJPlugIn.class from SIGLE OpenJump Viatoris
     * 2015-02-22. Giuseppe Aruta. version 01
     */
    private static final long serialVersionUID = 1L;
    private JEditorPane htmlPane;
    private JTree tree;
    private URL helpURL;
    private static boolean DEBUG = false;
    private static boolean playWithLineStyle = false;
    private static String lineStyle = "Horizontal";
    ImageIcon ICON = IconLoader.icon("disk.png");

    public HelpDialog() {
        super(new GridLayout(1, 0));

        DefaultMutableTreeNode top = new DefaultMutableTreeNode("Sextante");
        createNodes(top);
        this.tree = new JTree(top);
        this.tree.getSelectionModel().setSelectionMode(1);
        this.tree.addTreeSelectionListener(this);
        if (playWithLineStyle) {
            System.out.println("line style = " + lineStyle);
            this.tree.putClientProperty("JTree.lineStyle", lineStyle);
        }
        JScrollPane treeView = new JScrollPane(this.tree);
        this.htmlPane = new JEditorPane();
        this.htmlPane.setEditable(false);
        // initHelp();
        JScrollPane htmlView = new JScrollPane(this.htmlPane);

        JSplitPane splitPaneHTML = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                treeView, htmlView);
        Dimension minimumSize = new Dimension(100, 50);
        htmlView.setMinimumSize(minimumSize);
        treeView.setMinimumSize(minimumSize);
        splitPaneHTML.setDividerLocation(100);
        splitPaneHTML.setPreferredSize(new Dimension(500, 300));

        add(splitPaneHTML, BorderLayout.CENTER);

        // add(southPanel, BorderLayout.PAGE_END);
    }

    public void valueChanged(TreeSelectionEvent e) {
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.tree
                .getLastSelectedPathComponent();
        if (node == null) {
            return;
        }
        Object nodeInfo = node.getUserObject();
        if (node.isLeaf()) {
            BookInfo book = (BookInfo) nodeInfo;
            displayURL(book.bookURL);
            if (DEBUG) {
                System.out.print(book.bookURL + ":  \n    ");
            }
        } else {
            displayURL(this.helpURL);
        }
        if (DEBUG) {
            System.out.println(nodeInfo.toString());
        }
    }

    private class BookInfo {
        public String bookName;
        public URL bookURL;
        public Icon bookicon;

        public BookInfo(String book, String filename) {
            this.bookName = book;
            this.bookURL = HelpDialog.class.getResource(filename);
            if (this.bookURL == null) {
                System.err.println("Couldn't find file: " + filename);
            }
        }

        public String toString() {
            return this.bookName;
        }

    }

    private void initHelp() {
        String s = "welcome.html";
        this.helpURL = ClassLoader.getSystemResource(s);
        if (this.helpURL == null) {
            System.err.println("Couldn't open help file: " + s);
        } else if (DEBUG) {
            System.out.println("Help URL is " + this.helpURL);
        }
        displayURL(this.helpURL);
    }

    private void displayURL(URL url) {
        try {
            if (url != null) {
                this.htmlPane.setPage(url);
            } else {
                this.htmlPane.setText("File Not Found");
                if (DEBUG) {
                    System.out.println("Attempted to display a null URL.");
                }
            }
        } catch (IOException e) {
            System.err.println("Attempted to read a bad URL: " + url);
        }
    }

    private void createNodes(DefaultMutableTreeNode top) {

        DefaultMutableTreeNode intro = new DefaultMutableTreeNode(new BookInfo(
                "Introduction", "/sextante_help/en/general/intro.html"));

        top.add(new DefaultMutableTreeNode(new BookInfo("Introduction",
                "/sextante_help/en/general/intro.html")));
        top.add(new DefaultMutableTreeNode(new BookInfo("Toolbox",
                "/sextante_help/en/general/toolbox.html")));
        top.add(new DefaultMutableTreeNode(new BookInfo("History",
                "/sextante_help/en/general/history.html")));
        top.add(new DefaultMutableTreeNode(new BookInfo("Modeler",
                "/sextante_help/en/general/modeler.html")));
        top.add(new DefaultMutableTreeNode(new BookInfo("Command line",
                "/sextante_help/en/general/cmd.html")));
        top.add(new DefaultMutableTreeNode(new BookInfo("Batch",
                "/sextante_help/en/general/batch.html")));
        top.add(new DefaultMutableTreeNode(new BookInfo(
                "Configure algorithm providers",
                "/sextante_help/en/general/providers.html")));
        top.add(new DefaultMutableTreeNode(new BookInfo("List of algotithms",
                "/sextante_help/en/general/sextante_algo.html")));

        /*
         * category = new DefaultMutableTreeNode("Sextante Manual");
         * top.add(category);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo("Introduction",
         * "intro.html")); category.add(book);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo("Toolbox",
         * "toolbox.html")); category.add(book);
         * 
         * category2 = new DefaultMutableTreeNode("List of Algorithms");
         * top.add(category2);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo("Sextante Algorithms",
         * "sextante_algo.html")); category2.add(book);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo("2. Les menus",
         * "/sextante/help/en/general/sextante.html")); category.add(book);
         * 
         * book = new DefaultMutableTreeNode( new
         * BookInfo("3. Les barres d'outils",
         * "/helpfile/fr/tutorial/3_barres.html")); category.add(book);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo("4. Le clic-droit",
         * "/helpfile/fr/tutorial/4_clicdroit.html")); category.add(book);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo(
         * "5. Les données géographiques",
         * "/helpfile/fr/tutorial/5_donneesgeo.html")); category.add(book);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo(
         * "6. La gestion des données géographiques",
         * "/helpfile/fr/tutorial/6_gestiondonnees.html")); category.add(book);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo("7. Les styles",
         * "/helpfile/fr/tutorial/7_style.html")); category.add(book);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo(
         * "8. Les données attributaires",
         * "/helpfile/fr/tutorial/8_attributs.html")); category.add(book);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo("9. Analyse",
         * "/helpfile/fr/tutorial/9_analyse.html")); category.add(book);
         * 
         * category = new DefaultMutableTreeNode("Exercices");
         * top.add(category);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo("Exercice 1",
         * "/helpfile/fr/tutorial/exercice.html")); category.add(book);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo("Exercice 2",
         * "/helpfile/fr/tutorial/exercice.html")); category.add(book);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo("Exercice 3",
         * "/helpfile/fr/tutorial/exercice.html")); category.add(book);
         * 
         * category = new DefaultMutableTreeNode("BeanShell Editor");
         * top.add(category);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo("BeanShell Editor",
         * "/helpfile/fr/tutorial/BshEditor4jump-fr.html")); category.add(book);
         * 
         * book = new DefaultMutableTreeNode(new BookInfo("Spim",
         * "/helpfile/fr/tutorial/jump-spim-fr.html")); category.add(book);
         */
    }

    public static void createAndShowGUI() {

        // JFrame.setDefaultLookAndFeelDecorated(true);

        JFrame frame = new JFrame("Help Sextante");

        HelpDialog newContentPane = new HelpDialog();
        newContentPane.setOpaque(true);
        frame.setContentPane(newContentPane);

        frame.pack();
        frame.setVisible(true);
        frame.setAlwaysOnTop(true);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
            }
        });
    }

    public void setAlwaysOnTop(boolean b) {
    }

}
