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


/**
 * User: geoff
 * Date: 29/04/2007
 * Time: 08:29:31
 * Copyright 2007 Geoffrey G Roy.
 */
public class ChartParams
{
    public static String version = "0.99";
    public static final int PIE_EAST = 0;
    public static final int PIE_NORTH = 1;
    public static final int BAR = 2;
    public static final int COLUMN = 3;
    public static final int LABELS = 4;
    public static boolean clockwise = true;
   
    public static String [] typeNames;

    public static String layerName  = null;
    public static ChartAttribute [] attributes;
    public static int [] attributeOrder;
    public static boolean [] includeAttribute;
    public static int chartType = PIE_EAST;
    public static boolean includeLegend = true;
    public static boolean uniformSize = true;
    public static boolean autoScale = true;
    public static double scaleValue = 20.0;
    public static double barWidth = 5;
    public static boolean cancelled = false;
    public static boolean reloadDialog = false;
    public static boolean ordered = false;
    public static boolean showScale = false;
    public static boolean linearScale = true;
    public static double maxValue = 1.0;
    
    public static int originX = 0;
    public static int originY = 0;
    public static boolean showLabels = false;
    public static boolean localScale = false;
    
    public static boolean showUnderLabels = false;
    public static int underLabelIndex = 0;
    public static String underLabelName = "Name";

    public ChartParams()
    {

    }
    public static void setNames(I18NPlug iPlug)
    {
       typeNames = new String [] {iPlug.get("JumpChart.Dialog.PieEast"), iPlug.get("JumpChart.Dialog.PieNorth"),
            iPlug.get("JumpChart.Dialog.Bar"), iPlug.get("JumpChart.Dialog.Column"), iPlug.get("JumpChart.Dialog.Labels")};
    }
    
    public static double scaleInterval(double val)
    {
    	boolean debug = false;
        double interval = 0.2;
        double order = 0;
        int k = 1;
        double s = val*2.0/4.0;

        order = Math.floor(Math.log10(s));
        double sb = Math.floor(s/Math.pow(10.0, order));
        interval =  sb*Math.pow(10.0, order)/2.0;
        if(debug) System.out.println("Scaling: val="+val+"  order="+order+"  k="+k+"  sb="+sb);
        
        if(debug) System.out.println("interval="+interval);
        return interval;
    }
}
