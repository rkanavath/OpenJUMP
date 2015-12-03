package org.openjump.ext.viewmanager;

import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import org.apache.log4j.Logger;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Created by UMichael on 14/06/2015.
 */
public class ViewSetPanel extends JPanel {

    I18N I18N_ = I18N.getInstance("view_manager");

    ViewSet viewSet;

    public ViewSetPanel(final PlugInContext context, final ViewSet viewSet) {
        super(new GridBagLayout());
        setBorder(BorderFactory.createLineBorder(Color.black));
        initToolBar();
        if (viewSet != null) {
            init(context, viewSet);
            viewSet.addListener(new ViewSet.Listener() {
                public void actionPerformed(ViewSet viewSet, int mod, View view) {
                    removeAll();
                    initToolBar();
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
                initToolBar();
                init(context, viewSet);
                Window window = SwingUtilities.getWindowAncestor(ViewSetPanel.this);
                if (window != null) window.pack();
            }
        });
    }

    private void initToolBar() {

    }

    private void init(PlugInContext context, ViewSet viewSet) {
        this.viewSet = viewSet;
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.anchor = GridBagConstraints.CENTER;
        constraints.insets = new Insets(2,2,2,2);
        if (viewSet != null) {
            constraints.gridy = viewSet.views.size();
            for (View view : viewSet.views) {
                constraints.gridx = 0;
                constraints.gridy--;
                add(new ViewPanel(context, view), constraints);
            }
        }
    }

    class ViewPanel extends JPanel implements ActionListener {

        private JMenuItem deleteMenuItem = new JMenuItem(I18N_.getText("view_manager","ViewSetPanel.delete"));
        private JMenuItem topMenuItem = new JMenuItem(I18N_.getText("view_manager","ViewSetPanel.move-to-top"));
        private JMenuItem upMenuItem = new JMenuItem(I18N_.getText("view_manager","ViewSetPanel.move-up"));
        private JMenuItem downMenuItem = new JMenuItem(I18N_.getText("view_manager","ViewSetPanel.move-down"));
        private JMenuItem bottomMenuItem = new JMenuItem(I18N_.getText("view_manager","ViewSetPanel.move-to-bottom"));
        private JPopupMenu popupMenu = new JPopupMenu();

        final private PlugInContext context;
        final private View view;
        final private JTextField viewTextField = new JTextField(24);
        final private JButton apply = new JButton(I18N_.getText("view_manager","ViewSetPanel.apply"));

        public ViewPanel(PlugInContext context, View view) {
            super(new GridBagLayout());
            this.context = context;
            this.view = view;
            init();

            popupMenu.add(deleteMenuItem);
            deleteMenuItem.addActionListener(this);
            deleteMenuItem.setActionCommand("delete");

            popupMenu.add(topMenuItem);
            topMenuItem.addActionListener(this);
            topMenuItem.setActionCommand("moveToTop");

            popupMenu.add(upMenuItem);
            upMenuItem.addActionListener(this);
            upMenuItem.setActionCommand("moveUp");

            popupMenu.add(downMenuItem);
            downMenuItem.addActionListener(this);
            downMenuItem.setActionCommand("moveDown");

            popupMenu.add(bottomMenuItem);
            bottomMenuItem.addActionListener(this);
            bottomMenuItem.setActionCommand("moveToBottom");

            viewTextField.addActionListener(this);
            viewTextField.setActionCommand("changeName");
            apply.addActionListener(this);
            apply.setActionCommand("apply");
            apply.setComponentPopupMenu(popupMenu);
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
            constraints.insets = new Insets(0,2,0,2);
            constraints.anchor = GridBagConstraints.WEST;
            viewTextField.setText(view.name);
            add(viewTextField, constraints);
            constraints.gridx = 1;
            add(apply, constraints);
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

        private void apply() {
            for (StyledLayer styledLayer : view.styledLayers) {
                styledLayer.applyStyles(context.getLayerManager());
            }
        }

    }


}
