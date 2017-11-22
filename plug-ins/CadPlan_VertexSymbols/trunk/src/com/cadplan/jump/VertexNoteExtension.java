package com.cadplan.jump;

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 * User: geoff
 * Date: 1/08/2007
 * Time: 08:23:56
 * Copyright 2005 Geoffrey G Roy.
 */
public class VertexNoteExtension extends Extension
{
    public void configure(PlugInContext context) throws Exception
    {
        new VertexNotePlugin().initialize(context);
    }
}
