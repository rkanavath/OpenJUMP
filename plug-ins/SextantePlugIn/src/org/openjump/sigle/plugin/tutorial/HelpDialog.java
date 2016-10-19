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

import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.images.IconLoader;

import es.unex.sextante.core.Sextante;

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

        DefaultMutableTreeNode basic_concept = new DefaultMutableTreeNode(
                Sextante.getText("Basic_concepts"));

        DefaultMutableTreeNode algorithms = new DefaultMutableTreeNode(
                Sextante.getText("Algorithms"));
        DefaultMutableTreeNode child;
        DefaultMutableTreeNode node;
        basic_concept.add(new DefaultMutableTreeNode(new BookInfo(
                "About Sextante", "/sextante_help/en/general/about.htm")));
        basic_concept.add(new DefaultMutableTreeNode(new BookInfo(
                "Introduction", "/sextante_help/en/general/intro.html")));
        basic_concept.add(new DefaultMutableTreeNode(new BookInfo(Sextante
                .getText("SEXTANTE_toolbox"),
                "/sextante_help/en/general/toolbox.html")));
        basic_concept
                .add(new DefaultMutableTreeNode(new BookInfo(Sextante
                        .getText("History"),
                        "/sextante_help/en/general/history.html")));
        basic_concept.add(new DefaultMutableTreeNode(new BookInfo(Sextante
                .getText("Models"), "/sextante_help/en/general/modeler.html")));
        basic_concept
                .add(new DefaultMutableTreeNode(new BookInfo(Sextante
                        .getText("Command_line"),
                        "/sextante_help/en/general/cmd.html")));
        basic_concept.add(new DefaultMutableTreeNode(new BookInfo(Sextante
                .getText("Batch_processing"),
                "/sextante_help/en/general/batch.html")));
        basic_concept.add(new DefaultMutableTreeNode(new BookInfo(Sextante
                .getText("ConfiguringProviders"),
                "/sextante_help/en/general/providers.html")));
        algorithms.add(new DefaultMutableTreeNode(new BookInfo(
                "List of algotithms",
                "/sextante_help/en/general/sextante_algo.html")));

        top.add(basic_concept);
        top.add(algorithms);

    }

    public static void createAndShowGUI(PlugInContext context) {

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
