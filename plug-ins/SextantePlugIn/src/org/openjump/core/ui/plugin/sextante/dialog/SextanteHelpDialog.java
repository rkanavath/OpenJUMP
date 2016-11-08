package org.openjump.core.ui.plugin.sextante.dialog;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.text.Collator;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JInternalFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTree;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import org.openjump.core.ui.swing.DetachableInternalFrame;

import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.util.FileUtil;
import com.vividsolutions.jump.workbench.JUMPWorkbench;
import com.vividsolutions.jump.workbench.Logger;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;

import es.unex.sextante.core.GeoAlgorithm;
import es.unex.sextante.core.ObjectAndDescription;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.IAlgorithmProvider;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.help.AlgorithmTreeCellRenderer;
import es.unex.sextante.gui.help.HelpIO;
import es.unex.sextante.openjump.extensions.SextanteToolboxPlugin;
import es.unex.sextante.openjump.language.I18NPlug;

public class SextanteHelpDialog extends JPanel implements TreeSelectionListener {
    /**
     * Class adapted from HelpOJPlugIn.class from SIGLE OpenJump Viatoris
     */
    private static final long serialVersionUID = 1L;
    private JEditorPane htmlPane;
    private TreePath m_Path;
    private JTree tree;
    private URL helpURL;
    private static boolean DEBUG = false;
    private static String help = I18NPlug
            .getI18N("es.unex.sextante.kosmo.extensions.SextanteHelpPlugin.help");

    public SextanteHelpDialog() {
        // super(new GridLayout(1, 0));

        DefaultMutableTreeNode top = new DefaultMutableTreeNode(
                Sextante.getText("Help"));
        createNodes(top);
        this.tree = new JTree(top);
        this.tree.getSelectionModel().setSelectionMode(1);
        this.tree.addTreeSelectionListener(this);
        tree.setCellRenderer(new AlgorithmTreeCellRenderer());
        MouseAdapter local1 = new MouseAdapter() {
            public void mousePressed(MouseEvent paramAnonymousMouseEvent) {
                m_Path = tree.getPathForLocation(
                        paramAnonymousMouseEvent.getX(),
                        paramAnonymousMouseEvent.getY());
                showHelp(m_Path);
                tree.setSelectionPath(m_Path);

            }
        };
        tree.addMouseListener(local1);
        tree.addTreeSelectionListener(new TreeSelectionListener() {
            public void valueChanged(
                    TreeSelectionEvent paramAnonymousTreeSelectionEvent) {
                m_Path = paramAnonymousTreeSelectionEvent.getPath();
                if (m_Path != null) {
                    showHelp(m_Path);
                }
                DefaultMutableTreeNode localDefaultMutableTreeNode = (DefaultMutableTreeNode) m_Path
                        .getLastPathComponent();
                Object localObject = localDefaultMutableTreeNode
                        .getUserObject();
                if ((localObject instanceof GeoAlgorithm)) {
                }
            }
        });

        final BorderLayout thisLayout = new BorderLayout();
        this.setLayout(thisLayout);
        this.setPreferredSize(new java.awt.Dimension(800, 500));
        this.setSize(new java.awt.Dimension(800, 500));
        JScrollPane treeViewPane = new JScrollPane(this.tree,
                ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        treeViewPane.setPreferredSize(new Dimension(300, 450));
        treeViewPane.setMinimumSize(new Dimension(300, 450));
        this.htmlPane = new JEditorPane();
        this.htmlPane.setEditable(false);
        this.htmlPane.getDocument().putProperty("IgnoreCharsetDirective",
                Boolean.TRUE);
        JScrollPane htmlViewPane = new JScrollPane(this.htmlPane,
                ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        htmlViewPane.setPreferredSize(new Dimension(300, 450));
        htmlViewPane.setMinimumSize(new Dimension(300, 450));
        JSplitPane splitPaneHTML = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                treeViewPane, htmlViewPane);
        JPanel buttonpanel = new JPanel();
        JButton print = new JButton("Print");
        // JButton print = new JButton("Save");
        print.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(ActionEvent e) {
                printButton_actionPerformed(e);
            }
        });
        buttonpanel.add(print, new GridBagConstraints(3, 1, 1, 1, 0.0, 0.0,
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0,
                        0, 0, 0), 0, 0));

        add(splitPaneHTML, BorderLayout.CENTER);
        // add(buttonpanel, BorderLayout.SOUTH);

    }

    protected void showHelp(final TreePath path) {

        if (path != null) {
            try {
                DefaultMutableTreeNode node = (DefaultMutableTreeNode) path
                        .getLastPathComponent();
                Object ob = node.getUserObject();
                htmlPane.setContentType("text/html");
                if (ob instanceof GeoAlgorithm) {
                    // Removed as it doens't work
                    // Used modified code from SextanteGUI.class below
                    // Object help = SextanteGUI
                    // .getAlgorithmHelp((GeoAlgorithm) ob);
                    Object help = getAlgorithmHelp((GeoAlgorithm) ob);
                    if (help instanceof String) {
                        htmlPane.setText((String) help);
                    } else if (help instanceof URL) {
                        htmlPane.setPage((URL) help);
                    }
                } else if (ob instanceof ObjectAndDescription) {
                    ObjectAndDescription oad = (ObjectAndDescription) ob;
                    String sHtmlFile = (String) oad.getObject();
                    try {
                        final URL url = new URL("file:///" + sHtmlFile);
                        htmlPane.setPage(url);
                    } catch (final Exception e) {
                        // will show a blank page
                    }
                }
                htmlPane.setCaretPosition(0);
            } catch (final Exception e) {
                // Sextante.addErrorToLog(e);
            }

        }

    }

    // /////////////////////////////////////////////////////////////////////////////

    // The following code comes form SextanteGUI and has been modified in order
    // to work with OpenJUMP
    private final static ArrayList<IAlgorithmProvider> m_AlgorithmProviders = new ArrayList<IAlgorithmProvider>();

    public static Object getAlgorithmHelp(final GeoAlgorithm alg) {
        final String sName = Sextante.getAlgorithmProviderName(alg);
        for (int i = 0; i < m_AlgorithmProviders.size(); i++) {
            if (m_AlgorithmProviders.get(i).getName().equals(sName)) {
                return m_AlgorithmProviders.get(i).getAlgorithmHelp(alg);
            }
        }
        String sFilename;
        String sPath;
        if (sName.equals("SEXTANTE")) {
            sFilename = alg.getCommandLineName() + ".xml";
            // Get help folder path
            sPath = getHelpPath(alg, false);
            // /Method to convert .xml to .html
            return HelpIO.getHelpAsHTMLCode(alg, sPath + File.separator
                    + sFilename);
        } else {
            return ""; // TODO:create default help page
        }
    }

    // /Modified from SExtanteGUI
    // / Get Help folder path
    // / Forcing locale
    public static String getHelpPath(final GeoAlgorithm alg,
            final boolean bForceLocale) {
        String sPackage = alg.getClass().getPackage().toString();
        sPackage = sPackage.substring(8);
        final String help_Path = System.getProperty("user.dir")
                .concat(File.separator).concat("lib").concat(File.separator)
                .concat("ext").concat(File.separator).concat("sextante_help")
                .concat(File.separator);
        String sPath = help_Path + File.separator
                + Locale.getDefault().getLanguage() + File.separator + sPackage;

        final File dir = new File(sPath);
        if (!dir.exists() && !bForceLocale) {
            sPath = help_Path + File.separator + Locale.ENGLISH.getLanguage()
                    + File.separator + sPackage;
        }
        return sPath;

    }

    // ////////////////////////////////////////////////////////////////
    protected void printButton_actionPerformed(ActionEvent e) {
        try {
            htmlPane.setContentType("text/html");
            boolean done = htmlPane.print();
            if (done) {
                JUMPWorkbench.getInstance().getFrame()
                        .setStatusMessage("Printing is done");

            } else {
                JUMPWorkbench.getInstance().getFrame()
                        .warnUser("Error while printing");
            }
        } catch (Exception pex) {
            Logger.error(pex);
            JUMPWorkbench
                    .getInstance()
                    .getFrame()
                    .warnUser(
                            I18N.get("org.openjump.core.ui.plugin.mousemenu.SaveDatasetsPlugIn.Error-See-Output-Window"));
            JUMPWorkbench.getInstance().getFrame().getOutputFrame()
                    .createNewDocument();
            JUMPWorkbench
                    .getInstance()
                    .getFrame()
                    .getOutputFrame()
                    .addText(
                            "Print Help Dialog Exception:"
                                    + new Object[] { e.toString() });
            pex.printStackTrace();
        }

    }

    protected void saveButton_actionPerformed(ActionEvent e) {
        JFileChooser chooser;
        File archivo = null;
        chooser = GUIUtil.createJFileChooserWithOverwritePrompting();
        chooser.setMultiSelectionEnabled(false);
        chooser.setFileFilter(GUIUtil.createFileFilter(
                I18N.get("org.openjump.core.ui.plugin.file.open.SelectFileLoaderPanel.file-type"), new String[] { "htm" })); //$NON-NLS-1$//$NON-NLS-2$
        int returned = chooser.showSaveDialog(JUMPWorkbench.getInstance()
                .getFrame());

        if (returned == JFileChooser.APPROVE_OPTION) {
            String path = chooser.getSelectedFile().getAbsolutePath();
            chooser.getSelectedFile().delete();
            archivo = new File(path);
            archivo = FileUtil.addExtensionIfNone(archivo, "htm");//$NON-NLS-1$

            try {
                ByteArrayOutputStream baos = new ByteArrayOutputStream();
                htmlPane.setContentType("text/html");
                htmlPane.getDocument();
                HTMLEditorKit hk = new HTMLEditorKit();
                hk.write(baos, htmlPane.getDocument(), 0, htmlPane
                        .getDocument().getLength());

                FileOutputStream fos = new FileOutputStream(new File(
                        archivo.getAbsolutePath()));
                baos.writeTo(fos);
            } catch (Exception e1) {
                Logger.error(e1);
                JUMPWorkbench
                        .getInstance()
                        .getFrame()
                        .warnUser(
                                I18N.get("org.openjump.core.ui.plugin.mousemenu.SaveDatasetsPlugIn.Error-See-Output-Window"));
                JUMPWorkbench.getInstance().getFrame().getOutputFrame()
                        .createNewDocument();
                JUMPWorkbench
                        .getInstance()
                        .getFrame()
                        .getOutputFrame()
                        .addText(
                                "SaveImageToRasterPlugIn Exception:"
                                        + new Object[] { e.toString() });
            }
        }
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

    public class BookInfo {
        public String bookName;
        public URL bookURL;

        public BookInfo(String book, String filename) {
            this.bookName = book;
            this.bookURL = SextanteHelpDialog.class.getResource(filename);
            if (this.bookURL == null) {
                System.err.println("Couldn't find file: " + filename);
            }
        }

        public String toString() {
            return this.bookName;
        }

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

    String toolbox = I18NPlug
            .getI18N("es.unex.sextante.kosmo.extensions.SextanteToolboxPlugin.Sextante-toolbox");
    String results = I18NPlug
            .getI18N("es.unex.sextante.kosmo.extensions.SextanteResultsPlugin.Results");
    String history = I18NPlug
            .getI18N("es.unex.sextante.kosmo.extensions.SextanteHistoryPlugin.History");
    String modeler = I18NPlug
            .getI18N("es.unex.sextante.kosmo.extensions.SextanteModelerPlugin.Modeler");
    String command_line = I18NPlug
            .getI18N("es.unex.sextante.kosmo.extensions.SextanteCommandLinePlugin.Command-line");
    String data_explorer = I18NPlug
            .getI18N("es.unex.sextante.kosmo.extensions.SextanteDataExplorerPlugin.dataexplorer");
    String coordinates = I18NPlug
            .getI18N("es.unex.sextante.kosmo.extensions.SextantePickCooridnates.pick-coordinates");

    private void createNodes(DefaultMutableTreeNode top) {

        DefaultMutableTreeNode basic_concept = new DefaultMutableTreeNode(
                Sextante.getText("Basic_concepts"));

        DefaultMutableTreeNode tools = new DefaultMutableTreeNode(
                Sextante.getText("Tools"));
        DefaultMutableTreeNode additional_information = new DefaultMutableTreeNode(
                Sextante.getText("Additional_information"));
        DefaultMutableTreeNode Sextante_Serial = new DefaultMutableTreeNode(
                "Sextante Serial number:" + Sextante.getVersionNumber());
        SextanteToolboxPlugin sx = new SextanteToolboxPlugin();

        DefaultMutableTreeNode algorithms = new DefaultMutableTreeNode(
                Sextante.getText("Algorithms"));

        basic_concept.add(new DefaultMutableTreeNode(new BookInfo(
                "About Sextante", "/sextante_help/en/general/about.htm")));
        basic_concept.add(new DefaultMutableTreeNode(new BookInfo(
                "Introduction", "/sextante_help/en/general/intro.html")));
        basic_concept.add(new DefaultMutableTreeNode(new BookInfo(
                "OpenJUMP and Sextante data",
                "/sextante_help/en/general/openjump.html")));
        // Sextante_Toolbox
        tools.add(new DefaultMutableTreeNode(new BookInfo(toolbox,
                "/sextante_help/en/general/toolbox.html")));
        // AA Results
        tools.add(new DefaultMutableTreeNode(new BookInfo(results,
                "/sextante_help/en/general/results.html")));

        // Sextante History
        tools.add(new DefaultMutableTreeNode(new BookInfo(history,
                "/sextante_help/en/general/history.html")));

        // Sextante Modeler
        tools.add(new DefaultMutableTreeNode(new BookInfo(modeler,
                "/sextante_help/en/general/modeler.html")));

        // Sextante Command Line
        tools.add(new DefaultMutableTreeNode(new BookInfo(command_line,
                "/sextante_help/en/general/cmd.html")));
        // AA Explorer
        tools.add(new DefaultMutableTreeNode(new BookInfo(data_explorer,
                "/sextante_help/en/general/explorer.html")));
        // AA Pick coordinates
        tools.add(new DefaultMutableTreeNode(new BookInfo(coordinates,
                "/sextante_help/en/general/coordinates.html")));

        additional_information.add(new DefaultMutableTreeNode(new BookInfo(
                Sextante.getText("Batch_processing"),
                "/sextante_help/en/general/batch.html")));
        additional_information.add(new DefaultMutableTreeNode(new BookInfo(
                Sextante.getText("ConfiguringProviders"),
                "/sextante_help/en/general/providers.html")));

        algorithms.add(new DefaultMutableTreeNode(new BookInfo(
                "List of algotithms",
                "/sextante_help/en/general/sextante_algo.html")));

        top.add(basic_concept);
        top.add(tools);
        top.add(additional_information);
        top.add(algorithms);
        top.add(Sextante_Serial);

        DefaultMutableTreeNode node;
        DefaultMutableTreeNode child;
        setCursor(new Cursor(Cursor.WAIT_CURSOR));

        final HashMap<String, HashMap<String, GeoAlgorithm>> algs = Sextante
                .getAlgorithms();
        final Set<String> groupKeys = algs.keySet();
        final Iterator<String> groupIter = groupKeys.iterator();
        while (groupIter.hasNext()) {
            final HashMap<String, DefaultMutableTreeNode> baseGroups = new HashMap<String, DefaultMutableTreeNode>();
            final String groupKey = groupIter.next();
            final DefaultMutableTreeNode toolsNode = new DefaultMutableTreeNode(
                    groupKey);
            algorithms.add(toolsNode);
            final HashMap<String, GeoAlgorithm> groupAlgs = algs.get(groupKey);
            final Set keys = groupAlgs.keySet();
            final Iterator iter = keys.iterator();
            while (iter.hasNext()) {
                final GeoAlgorithm alg = groupAlgs.get(iter.next());
                child = new DefaultMutableTreeNode(alg);
                node = baseGroups.get(alg.getGroup());
                if (node == null) {
                    node = new DefaultMutableTreeNode(alg.getGroup());
                    baseGroups.put(alg.getGroup(), node);
                    addNodeInSortedOrder(toolsNode, node);
                }
                addNodeInSortedOrder(node, child);
            }

        }
        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

        top.add(algorithms);

    }

    private void addNodeInSortedOrder(DefaultMutableTreeNode parent,
            DefaultMutableTreeNode child) {
        int n = parent.getChildCount();
        if (n == 0) {
            parent.add(child);
            return;
        }
        Collator collator = Collator.getInstance();
        collator.setStrength(0);
        DefaultMutableTreeNode node = null;
        for (int i = 0; i < n; i++) {
            node = (DefaultMutableTreeNode) parent.getChildAt(i);
            try {
                if (collator.compare(node.toString(), child.toString()) > 0) {
                    parent.insert(child, i);
                    return;
                }
            } catch (Exception localException) {
            }
        }
        parent.add(child);
    }

    public static void createAndShowGUI(PlugInContext context) {

        for (JInternalFrame iFrame : context.getWorkbenchFrame()
                .getInternalFrames()) {
            if (iFrame instanceof SextanteHelpFrame) {
                if (!((SextanteHelpFrame) iFrame).isClosed()) {
                    iFrame.toFront();
                    return;
                }
            }
        }
        final SextanteHelpFrame frame = new SextanteHelpFrame(context);
        frame.setSize(1100, 600);
        frame.setTitle(help);
        context.getWorkbenchFrame().addInternalFrame(frame, false, true);
    }

    public static class SextanteHelpFrame extends DetachableInternalFrame {
        /**
         * 
         */
        private static final long serialVersionUID = 1L;

        public SextanteHelpFrame(final PlugInContext context) {
            context.getLayerManager();
            SextanteHelpDialog newContentPane = new SextanteHelpDialog();

            add(newContentPane, BorderLayout.CENTER);
            setResizable(true);
            setClosable(true);
            setIconifiable(true);
            setName(help);
            setMaximizable(true);
            pack();
            setVisible(true);

        }

    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
            }
        });
    }

    public static Icon getIcon() {

        return new ImageIcon(SextanteGUI.class.getClassLoader().getResource(
                "images/sextante.gif"));

    }

}
