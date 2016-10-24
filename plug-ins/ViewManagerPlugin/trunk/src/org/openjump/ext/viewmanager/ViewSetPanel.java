package org.openjump.ext.viewmanager;

import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * A JPanel containing all the elements to manage a viewSet
 * <ul>
 *     <li>add a new view to the viewSet</li>
 *     <li>rename a view</li>
 *     <li>remove a view from a viewSet</li>
 *     <li>apply the view to current viewPanel</li>
 *     <li>replace a view</li>
 *     <li>move a viewup or down in the view list</li>
 * </ul>
 */
public class ViewSetPanel extends JPanel {

    private I18N I18N_ = I18N.getInstance("view_manager");

    private ViewSet viewSet;
    private PlugInContext context;

    public ViewSetPanel(final PlugInContext context, final ViewSet viewSet) {
        super(new GridBagLayout());
        this.context = context;
        if (viewSet != null) {
            init(context, viewSet);
            viewSet.addListener(new ViewSet.Listener() {
                public void actionPerformed(ViewSet viewSet, int mod, View view) {
                    removeAll();
                    init(context, viewSet);
                    Window window = SwingUtilities.getWindowAncestor(ViewSetPanel.this);
                    if (window != null) window.pack();
                }
            });
        }
    }

    void reset(final PlugInContext context, final ViewSet viewSet) {
        init(context, viewSet);
        viewSet.addListener(new ViewSet.Listener(){
            public void actionPerformed(ViewSet viewSet, int mod, View view) {
                removeAll();
                init(context, viewSet);
                Window window = SwingUtilities.getWindowAncestor(ViewSetPanel.this);
                if (window != null) window.pack();
            }
        });
    }


    private void init(PlugInContext context, ViewSet viewSet) {
        this.viewSet = viewSet;
        removeAll();
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.weightx = 1.0;
        constraints.insets = new Insets(2,2,2,2);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        if (viewSet != null) {
            constraints.gridy = viewSet.views.size();
            for (View view : viewSet.views) {
                constraints.gridy--;
                add(new ViewPanel(context, view), constraints);
            }
        }
    }

    private class ViewPanel extends JPanel implements ActionListener {

        ImageIcon deleteIcon  = new ImageIcon(ViewSetPanel.class.getClassLoader().getResource("/images/delete.png"));
        ImageIcon replaceIcon = new ImageIcon(ViewSetPanel.class.getClassLoader().getResource("/images/replace.png"));
        ImageIcon moveIcon    = new ImageIcon(ViewSetPanel.class.getClassLoader().getResource("/images/move-vertical.png"));
        ImageIcon topIcon     = new ImageIcon(ViewSetPanel.class.getClassLoader().getResource("/images/arrow-top.png"));
        ImageIcon upIcon      = new ImageIcon(ViewSetPanel.class.getClassLoader().getResource("/images/arrow-up.png"));
        ImageIcon downIcon    = new ImageIcon(ViewSetPanel.class.getClassLoader().getResource("/images/arrow-down.png"));
        ImageIcon bottomIcon  = new ImageIcon(ViewSetPanel.class.getClassLoader().getResource("/images/arrow-bottom.png"));

        private JMenuItem replaceByCurrentViewItem = new JMenuItem(I18N_.getText("view_manager","ViewSetPanel.replace-by-current-view"));
        private JMenuItem replaceBySelectedLayerItem = new JMenuItem(I18N_.getText("view_manager","ViewSetPanel.replace-by-selected-layers"));

        private JMenuItem topMenuItem = new JMenuItem(I18N_.getText("view_manager","ViewSetPanel.move-to-top"), topIcon);
        private JMenuItem upMenuItem = new JMenuItem(I18N_.getText("view_manager","ViewSetPanel.move-up"), upIcon);
        private JMenuItem downMenuItem = new JMenuItem(I18N_.getText("view_manager","ViewSetPanel.move-down"), downIcon);
        private JMenuItem bottomMenuItem = new JMenuItem(I18N_.getText("view_manager","ViewSetPanel.move-to-bottom"), bottomIcon);

        private JPopupMenu replacePopupMenu = new JPopupMenu();
        private JPopupMenu movePopupMenu = new JPopupMenu();



        final private PlugInContext context;
        final private View view;
        final private JTextField viewTextField = new JTextField(24);
        final private JButton applyButton      = new JButton(I18N_.getText("view_manager","ViewSetPanel.apply"));
        final private JButton deleteButton     = new JButton(deleteIcon);
        final private JButton replaceButton    = new JButton(replaceIcon);
        final private JButton moveButton       = new JButton(moveIcon);



        public ViewPanel(PlugInContext context, View view) {
            super(new GridBagLayout());
            this.context = context;
            this.view = view;
            init();

            applyButton.addActionListener(this);
            applyButton.setActionCommand("apply");
            applyButton.setPreferredSize(new Dimension(applyButton.getPreferredSize().width, 22));

            deleteButton.addActionListener(this);
            deleteButton.setToolTipText(I18N_.getText("view_manager","ViewSetPanel.delete"));
            deleteButton.setActionCommand("delete");
            deleteButton.setPreferredSize(new Dimension(22,22));

            replacePopupMenu.add(replaceByCurrentViewItem);
            replaceByCurrentViewItem.addActionListener(this);
            replaceByCurrentViewItem.setActionCommand("replaceByCurrentView");

            replacePopupMenu.add(replaceBySelectedLayerItem);
            replaceBySelectedLayerItem.addActionListener(this);
            replaceBySelectedLayerItem.setActionCommand("replaceBySelectedLayers");

            movePopupMenu.add(topMenuItem);
            topMenuItem.addActionListener(this);
            topMenuItem.setActionCommand("moveToTop");

            movePopupMenu.add(upMenuItem);
            upMenuItem.addActionListener(this);
            upMenuItem.setActionCommand("moveUp");

            movePopupMenu.add(downMenuItem);
            downMenuItem.addActionListener(this);
            downMenuItem.setActionCommand("moveDown");

            movePopupMenu.add(bottomMenuItem);
            bottomMenuItem.addActionListener(this);
            bottomMenuItem.setActionCommand("moveToBottom");

            viewTextField.addActionListener(this);
            viewTextField.setActionCommand("changeName");


            replaceButton.setToolTipText(I18N_.getText("view_manager","ViewSetPanel.replace"));
            replaceButton.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    replacePopupMenu.show(e.getComponent(), e.getX(), e.getY());
                }
            });
            replaceButton.setPreferredSize(new Dimension(22,22));


            moveButton.setToolTipText(I18N_.getText("view_manager","ViewSetPanel.move"));
            moveButton.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    movePopupMenu.show(e.getComponent(), e.getX(), e.getY());
                }
            });
            moveButton.setPreferredSize(new Dimension(22,22));


            viewTextField.getDocument().addDocumentListener(new DocumentListener() {
                @Override
                public void insertUpdate(DocumentEvent e) {
                    changeName();
                }

                @Override
                public void removeUpdate(DocumentEvent e) {
                    changeName();
                }

                @Override
                public void changedUpdate(DocumentEvent e) {
                    changeName();
                }
            });

        }

        public void init() {
            GridBagConstraints constraints = new GridBagConstraints();
            constraints.gridx = 0;
            constraints.gridy = 0;
            constraints.weightx = 1.0;
            constraints.insets = new Insets(0,1,0,1);
            constraints.fill = GridBagConstraints.HORIZONTAL;
            viewTextField.setText(view.name);
            add(viewTextField, constraints);

            constraints.weightx = 0.0;
            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 1;
            add(applyButton, constraints);
            constraints.gridx = 2;
            add(deleteButton, constraints);
            constraints.gridx = 3;
            add(replaceButton, constraints);
            constraints.gridx = 4;
            add(moveButton, constraints);
        }

        @Override
        public void actionPerformed(ActionEvent ae) {
            String action = ae.getActionCommand();
            if (action.equals("changeName")) {
                changeName();
            } else if (action.equals("apply")) {
                apply();
            } else if (action.equals("delete")) {
                delete();
            } else if (action.equals("moveToTop")) {
                moveToTop();
            } else if (action.equals("moveUp")) {
                moveUp();
            } else if (action.equals("moveDown")) {
                moveDown();
            } else if (action.equals("moveToBottom")) {
                moveToBottom();
            } else if (action.equals("replaceByCurrentView")) {
                replaceByCurrentView();
            } else if (action.equals("replaceBySelectedLayers")) {
                replaceBySelectedLayers();
            }
        }

        private void changeName() {
            view.name = viewTextField.getText();
        }

        private void delete() {
            viewSet.removeView(view);
        }

        private void moveToTop() {
            viewSet.moveViewToTop(view);
        }

        private void moveUp() {
            viewSet.moveViewUp(view);
        }

        private void moveDown() {
            viewSet.moveViewDown(view);
        }

        private void moveToBottom() {
            viewSet.moveViewToBottom(view);
        }

        private void replaceByCurrentView() {
            View newView = new View(context, false);
            newView.name = view.name;
            viewSet.replaceView(view, newView);
        }

        private void replaceBySelectedLayers() {
            View newView = new View(context, true);
            newView.name = view.name;
            viewSet.replaceView(view, newView);
        }

        private void apply() {
            for (StyledLayer styledLayer : view.styledLayers) {
                styledLayer.applyStyles(context.getLayerManager());
            }
        }

    }


}
