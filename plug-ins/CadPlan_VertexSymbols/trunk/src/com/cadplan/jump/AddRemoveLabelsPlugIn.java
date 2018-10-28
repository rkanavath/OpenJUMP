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

import java.util.Collection;

import javax.swing.Icon;

import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.MenuNames;

/**
 * User: geoff Date: 28/04/2007 Time: 09:40:22 Copyright 2007 Geoffrey G Roy.
 */
public class AddRemoveLabelsPlugIn extends AbstractPlugIn {
    private I18NPlug iPlug;

    @Override
    public void initialize(PlugInContext context) throws Exception {

        final EnableCheckFactory check = new EnableCheckFactory(
                context.getWorkbenchContext());
        final MultiEnableCheck mcheck = new MultiEnableCheck();
        mcheck.add(check.createAtLeastNLayersMustExistCheck(1));
        mcheck.add(check.createAtLeastNLayersMustBeEditableCheck(1));
        final String menuName = MenuNames.PLUGINS;
        // final String menuItem = iPlug.get("VertexSymbols.MenuItem");
        context.getFeatureInstaller().addMainMenuPlugin(this,
                new String[] { menuName, MenuNames.STYLE }, getName(), false,
                getIcon(), mcheck);

    }

    public Icon getIcon() {
        return null;
    }

    @Override
    public String getName() {
        iPlug = new I18NPlug("VertexSymbols", "language.VertexSymbolsPlugin");
        return iPlug.get("AddRemoveLabelsPlugIn.MenuItem");
    }

    @Override
    public boolean execute(PlugInContext context) throws Exception {
        @SuppressWarnings("unchecked")
        final Collection<Layer> vlayers = context.getLayerNamePanel()
                .selectedNodes(Layer.class);
        for (final Layer layer : vlayers) {

            if (layer.getVertexStyle().isEnabled()
                    && !layer.getLabelStyle().isEnabled()) {
                final ExternalSymbolsType vertexStyle = (ExternalSymbolsType) layer
                        .getVertexStyle();

                if (vertexStyle.getTextEnabled()) {
                    vertexStyle.setTextEnabled(false);

                } else {
                    vertexStyle.setTextEnabled(true);

                }

            }
            layer.fireAppearanceChanged();
        }
        return true;
    }
}
