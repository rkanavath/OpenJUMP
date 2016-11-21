/*
 * The JCS Conflation Suite (JCS) is a library of Java classes that
 * can be used to build automated or semi-automated conflation solutions.
 *
 * Copyright (C) 2003 Vivid Solutions
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
 * Vivid Solutions
 * Suite #1A
 * 2328 Government Street
 * Victoria BC  V8T 5G5
 * Canada
 *
 * (250)385-6040
 * www.vividsolutions.com
 */

package com.vividsolutions.jcs.plugin.clean;

import com.vividsolutions.jcs.conflate.coverage.MicroSegmentRemover;

import com.vividsolutions.jts.geom.Geometry;

import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureDatasetFactory;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.*;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;
import com.vividsolutions.jump.workbench.ui.MenuNames;

import fr.michaelm.jump.plugin.topology.I18NPlug;

import java.util.ArrayList;
import java.util.Collection;

/**
 * Plugin to remove smallest segments of a geometry.
 * @deprecated replaced and improved in
 * com.vividsolutions.jump.workbench.ui.plugin.analysis.RemoveSmallSegments
 * added to the core library in November 2016
 */
@Deprecated
public class RemoveMicroSegmentsPlugIn extends ThreadedBasePlugIn {

    private static String LAYER;
    private static String TOLERANCE;
    private static String WORK_ON_COPY;
    private static String USE_FENCE;

    private Layer layer;
    private double tolerance = 1.0;
    private boolean workOnCopy = true;
    private boolean useFence = false;
    private Geometry fence = null;

    public RemoveMicroSegmentsPlugIn() { }

    /**
     * Returns a very brief description of this task.
     * @return the name of this task
     */
    public String getName() {
        return I18NPlug.getI18N("qa.RemoveMicroSegmentsPlugIn.remove-micro-segments");
    }

    public void initialize(PlugInContext context) throws Exception {
        String topology = I18NPlug.getI18N("Topology");
        context.getFeatureInstaller().addMainMenuPlugin(
            this, new String[]{MenuNames.PLUGINS, topology},
            I18NPlug.getI18N("qa.RemoveMicroSegmentsPlugIn.remove-micro-segments") + "...",
            false, null, new MultiEnableCheck()
                .add(context.getCheckFactory().createTaskWindowMustBeActiveCheck())
                .add(context.getCheckFactory().createAtLeastNLayersMustExistCheck(1)));
        LAYER = I18NPlug.getI18N("Layer");
        TOLERANCE = I18NPlug.getI18N("dist-tolerance");
        WORK_ON_COPY = I18NPlug.getI18N("work-on-copy");
        USE_FENCE = I18NPlug.getI18N("use-fence");
    }

    public boolean execute(PlugInContext context) throws Exception {
        MultiInputDialog dialog = new MultiInputDialog(
            context.getWorkbenchFrame(),
            I18NPlug.getI18N("qa.RemoveMicroSegmentsPlugIn.remove-micro-segments"),
            true);

        dialog.setSideBarDescription(
          I18NPlug.getI18N("qa.RemoveMicroSegmentsPlugIn.description")
        );
        
        String fieldName = LAYER;
        dialog.addLayerComboBox(fieldName, context.getCandidateLayer(0), null, context.getLayerManager());
        
        dialog.addDoubleField(TOLERANCE, tolerance, 8, I18NPlug.getI18N("dist-tolerance"));
        dialog.addCheckBox(WORK_ON_COPY, workOnCopy, I18NPlug.getI18N("work-on-copy"));
        dialog.addCheckBox(USE_FENCE, useFence, I18NPlug.getI18N("use-fence"));
        
        GUIUtil.centreOnWindow(dialog);
        dialog.setVisible(true);
        if (dialog.wasOKPressed()) { 
            layer = dialog.getLayer(LAYER);
            tolerance = dialog.getDouble(TOLERANCE);
            workOnCopy = dialog.getBoolean(WORK_ON_COPY);
            useFence = dialog.getBoolean(USE_FENCE);
            return true;
        }
        else return false;
    }

    public void run(TaskMonitor monitor, PlugInContext context) throws Exception {
        monitor.allowCancellationRequests();
        monitor.report(I18NPlug.getI18N("qa.RemoveMicroSegmentsPlugIn.remove-micro-segments"));
        if (useFence) {
            if (context.getLayerViewPanel().getFence() == null) {
              context.getWorkbenchFrame().warnUser(
                  I18NPlug.getI18N("no-fence-defined"));
              return;
            }
            fence = context.getLayerViewPanel().getFence();
        }
        FeatureCollection newDataset = null;
        Collection removedPoints = new ArrayList();
        if (workOnCopy) {
            newDataset = new FeatureDataset(layer.getFeatureCollectionWrapper().getFeatureSchema());
        }
        int count = 0;
        for (Object o : layer.getFeatureCollectionWrapper().getFeatures()) {
            Feature f = (Feature)o;
            MicroSegmentRemover msr = new MicroSegmentRemover(f.getGeometry(), fence);
            msr.removeMicroSegments(tolerance);
            if (++count%10==0) {
                monitor.report(count, layer.getFeatureCollectionWrapper().size(), I18NPlug.getI18N("features"));
            }
            if (workOnCopy) {
                Feature nf = f.clone(true);
                nf.setGeometry(msr.getResultingGeometry());
                newDataset.add(nf);
            }
            else if (msr.hasGeometryChanged()) {
                f.setGeometry(msr.getResultingGeometry());
            }
            removedPoints.addAll(msr.getRemovedPoints());
        }
        monitor.report(layer.getFeatureCollectionWrapper().size(), layer.getFeatureCollectionWrapper().size(), I18NPlug.getI18N("features"));
        context.getLayerManager()
               .addLayer(I18NPlug.getI18N("model.StandardCategoryNames.qa"),
                         layer.getName() + " " + I18NPlug.getI18N("qa.RemoveMicroSegmentsPlugIn.removed-coordinates"),
                         FeatureDatasetFactory.createFromGeometry(removedPoints));
        if (workOnCopy) {
            context.getLayerManager()
                   .addLayer(I18NPlug.getI18N("model.StandardCategoryNames.qa"),
                         layer.getName() + " " + I18NPlug.getI18N("qa.RemoveMicroSegmentsPlugIn.without-micro-segments"),
                         newDataset);
        }
    }

}
