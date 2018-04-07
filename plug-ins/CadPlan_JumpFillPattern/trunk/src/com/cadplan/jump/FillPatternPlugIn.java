/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * Copyright (C) 2006 Cadplan
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
 */

package com.cadplan.jump;

import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTextArea;

import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.workbench.JUMPWorkbench;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.plugin.AboutPlugIn;

/**
 * User: geoff Date: 28/04/2007 Time: 09:40:22 Copyright 2007 Geoffrey G Roy.
 */
public class FillPatternPlugIn extends AbstractPlugIn {
    private final String version = "0.3 April 2018";

    public final static String NAME = "Fill Patterns";

    @Override
    public void initialize(PlugInContext context) throws Exception {

        context.getFeatureInstaller().addMainMenuPlugin(this,
                new String[] { MenuNames.HELP }, NAME, false, null, null);
        final LoadFillPatterns loader = new LoadFillPatterns(context);

    }

    @Override
    public boolean execute(PlugInContext context) throws Exception {
        final String newline = "\n";

        final String text = newline
                + "JumpFillPattern allows users to create and use their own fill patterns. "
                + newline
                + "The plugin is loaded on launching OpenJump. New patterns appear in the OpenJump Change Styles dialog"
                + newline
                + newline
                + "JumpFillPattern allows to use two types of patterns:"
                + newline
                + newline
                + "Vector Patterns"
                + newline
                + "Vector-based patterns that are specified using the Well Known Text (WKT) specification."
                + newline
                + "These patterns are included in text files with a .wkt file name extension."
                + newline
                + "Each file contains just one pattern, with format: <Line width>:<Extent size of the tile>:<WKT Specification>"
                + newline
                + "Example: 1:12:LINESTRING(5 5, 5 -5, -5 -5, 5 5 "
                + newline
                + newline
                + "Image Patterns"
                + newline
                + "Images that form the basis of the pattern. Image patterns are created from GIF, JPEG, PNG or SVG image files"
                + newline
                + "GIF/JPG/PNG: raw image size defines the size of the tile that will be tessellated over the polygon to be filled."
                + newline
                + "SVG: default tile size is 32x32 pixels. Another tile size can be specified by modifing the file name. "
                + newline
                + "Example: 'name_x64.svg' will draw a tile of 64x64 pixels"
                + newline
                + newline
                + "More info on JumpFillPattern plugin:"
                + newline
                + "sourceforge.net/p/jump-pilot/code/HEAD/tree/plug-ins/CadPlan_JumpFillPattern/doc/JumpFillPatternUserGuide.pdf";

        final JDialog jDialog = new JDialog(JUMPWorkbench.getInstance()
                .getFrame(), false);
        final String fontName = JUMPWorkbench.getInstance().getFrame()
                .getFont().getName();
        jDialog.setIconImage(AboutPlugIn.ICON.getImage());

        final JPanel jPanel_Help = new JPanel(new BorderLayout());
        jPanel_Help.setBorder(BorderFactory.createEtchedBorder());

        final JTextArea jTextArea_Help1 = new JTextArea();
        jTextArea_Help1.setFont(new Font(fontName, 1, 18));
        jTextArea_Help1
                .setText("JumpFillPattern Plugin (c)2007 Geoffrey G. Roy");

        final JTextArea jTextArea_Help2 = new JTextArea();
        jTextArea_Help2.setFont(new Font(fontName, 0, 16));
        jTextArea_Help2.setText("http://www.cadplan.com.au -  Version: "
                + version);

        final JTextArea jTextArea_Help3 = new JTextArea();
        jTextArea_Help3.setFont(new Font("Arial", 0, 14));
        jTextArea_Help3.setText(text);
        final JPanel buttonPanel = new JPanel();
        final JButton okButton = new JButton(I18N.get("ui.OKCancelPanel.ok"));

        okButton.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                // jDialog.setVisible(false);
                jDialog.dispose();

            }
        });

        buttonPanel.add(okButton, null);

        jPanel_Help.add(jTextArea_Help1, BorderLayout.NORTH);
        jPanel_Help.add(jTextArea_Help2, BorderLayout.CENTER);
        jPanel_Help.add(jTextArea_Help3, BorderLayout.SOUTH);
        jDialog.add(jPanel_Help, BorderLayout.PAGE_START);
        jDialog.add(buttonPanel, BorderLayout.PAGE_END);

        jDialog.setTitle(NAME);
        jDialog.pack();
        jDialog.setResizable(false);
        jDialog.setLocationRelativeTo(JUMPWorkbench.getInstance().getFrame());
        jDialog.setVisible(true);

        return true;
    }

}
