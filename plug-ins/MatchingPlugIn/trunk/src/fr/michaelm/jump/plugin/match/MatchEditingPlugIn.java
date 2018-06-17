/*
 * (C) 2017 Michaël Michaud
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
 * m.michael.michaud@orange.fr
 */

package fr.michaelm.jump.plugin.match;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.LayerManager;
import com.vividsolutions.jump.workbench.plugin.EnableCheck;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;
import com.vividsolutions.jump.workbench.ui.WorkbenchFrame;
import com.vividsolutions.jump.workbench.ui.task.TaskMonitorManager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.Iterator;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

/**
 * PlugIn to find features from a layer matching features of another layer. 
 * @author Michaël Michaud
 */
public class MatchEditingPlugIn extends AbstractPlugIn implements ActionListener {
    
    private final String MATCH_EDITING = I18NPlug.getI18N("Match-editing");
    private final String MATCHING      = I18NPlug.getI18N("Matching");
    private final String LINK_LAYER    = I18NPlug.getI18N("Links");
    private final String SOURCE_LAYER  = I18NPlug.getI18N("Source-layer");
    private final String TARGET_LAYER  = I18NPlug.getI18N("Target-layer");
    
    private WorkbenchContext workbenchContext = null;
    private Layer linkLayer   = null;
    private Layer sourceLayer = null;
    private Layer targetLayer = null;
    
   
    
    public MatchEditingPlugIn() {
    }
    
    public String getName() {
        return MATCH_EDITING;
    }

    public void initialize(PlugInContext context) throws Exception {
        
        context.getFeatureInstaller().addMainMenuPlugin(
          this, new String[]{MenuNames.PLUGINS, MATCHING},
          MATCH_EDITING + "...",
          false, null, getEnableCheck(context));
        workbenchContext = context.getWorkbenchContext();
    }

    /**
     * Execute method initialize the plugin interface and get all the
     * parameters from the user.
     */
    public boolean execute(PlugInContext context) throws Exception {

        ////////////////////////////////////////////////////////////////////////
        // UI : CREATE MULTITAB INPUT DIALOG
        ////////////////////////////////////////////////////////////////////////
                
        final MultiInputDialog dialog = new MultiInputDialog(
            context.getWorkbenchFrame(), MATCH_EDITING, false);
        
        linkLayer = context.getLayerNamePanel().getSelectedLayers()[0];
        FeatureCollection fc = linkLayer.getFeatureCollectionWrapper();
        JTextField linkTextField = dialog.addTextField(LINK_LAYER, linkLayer.getName(), 24, null, null);
        linkTextField.setEditable(false);
        
        sourceLayer = findTargetLayer(linkLayer, "SOURCE", context.getLayerManager());
        if (sourceLayer != null) {
            JTextField sourceTextField = dialog.addTextField(SOURCE_LAYER, sourceLayer.getName(), 24, null, null);
            sourceTextField.setEditable(false);
        } else {
            JOptionPane.showMessageDialog(context.getWorkbenchFrame(), 
                I18NPlug.getI18N("Missing-source-layer"),
                I18NPlug.getI18N("Missing-layer"), JOptionPane.ERROR_MESSAGE);
            return false;
        }
        
        
        targetLayer = findTargetLayer(linkLayer, "TARGET", context.getLayerManager());
        if (targetLayer != null) {
            JTextField targetTextField = dialog.addTextField(TARGET_LAYER, targetLayer.getName(), 24, null, null);
            targetTextField.setEditable(false);
        } else {
            JOptionPane.showMessageDialog(context.getWorkbenchFrame(), 
                I18NPlug.getI18N("Missing-target-layer"), 
                I18NPlug.getI18N("Missing-layer"), JOptionPane.ERROR_MESSAGE);
            return false;
        }
        
        
        JButton createLinksButton = dialog.addButton(I18NPlug.getI18N("Create-links-label"), I18NPlug.getI18N("Create-links"), "");
        createLinksButton.addActionListener(this);
        createLinksButton.setActionCommand("link");
        
        JButton updateMatchesButton = dialog.addButton(I18NPlug.getI18N("Update-matches"), I18NPlug.getI18N("Update-matches"), "");
        updateMatchesButton.addActionListener(this);
        updateMatchesButton.setActionCommand("update");
        
        dialog.setSideBarDescription(I18NPlug.getI18N("Match-editing-description"));
        
        for (Object o : workbenchContext.getLayerManager().getLayers()) {
            Layer layer = (Layer)o;
            if (layer != linkLayer && layer != sourceLayer && layer != targetLayer) {
                layer.setSelectable(false);
                layer.setEditable(false);
                layer.setVisible(false);
            }
        }
        linkLayer.setVisible(true);
        linkLayer.setEditable(true);
        linkLayer.setSelectable(true);
        
        sourceLayer.setVisible(true);
        sourceLayer.setSelectable(true);
        sourceLayer.setEditable(false);
        
        targetLayer.setVisible(true);        
        targetLayer.setSelectable(true);
        targetLayer.setEditable(false);
        
        GUIUtil.centreOnWindow(dialog);
        dialog.setVisible(true);
 
        return true;
        
    }
    

    
    
    // Return the layer containing features identified by attribute column
    // return null if layer is not found
    private Layer findTargetLayer(Layer links, String attribute, LayerManager layerManager) {
        int index = links.getFeatureCollectionWrapper().getFeatureSchema().getAttributeIndex(attribute);
        if (index < 0) return null;
        for (Iterator it = links.getFeatureCollectionWrapper().iterator() ; it.hasNext() ;) {
            Feature f = (Feature)it.next();
            int fid = f.getInteger(index);
            for (Object o : layerManager.getLayers()) {
                if (o == links) continue;
                Layer lyr = (Layer)o;
                for (Iterator it2 = lyr.getFeatureCollectionWrapper().iterator() ; it2.hasNext() ;) {
                    Feature f2 = (Feature)it2.next();
                    if (fid == f2.getID()) {
                        return lyr;
                    }
                }
            }
        }
        return null;
    }
    
    private EnableCheck getEnableCheck(final PlugInContext context) {
        return new MultiEnableCheck()
          .add(context.getCheckFactory().createTaskWindowMustBeActiveCheck())
          .add(context.getCheckFactory().createExactlyNLayersMustBeSelectedCheck(1))
          .add(context.getCheckFactory().createAtLeastNLayersMustExistCheck(3))
          .add(context.getCheckFactory().createSelectedLayersMustBeEditableCheck())
          .add(new EnableCheck(){
              public String check(JComponent component) {
                  Layer lyr = context.getWorkbenchContext().getLayerableNamePanel().getSelectedLayers()[0];
                  FeatureSchema schema = lyr.getFeatureCollectionWrapper().getFeatureSchema();
                  return schema.hasAttribute("SOURCE") &&
                         schema.hasAttribute("TARGET") &&
                         schema.hasAttribute("SCORE") ? 
                      null : I18NPlug.getI18N("Invalid-link-layer");
              }
          });
    }
    
    public void actionPerformed(ActionEvent e) {
        if ("link".equals(e.getActionCommand())) {
                Collection selectedSource = workbenchContext
                        .getLayerViewPanel()
                        .getSelectionManager()
                        .getFeaturesWithSelectedItems(sourceLayer);
                Collection selectedTarget = workbenchContext
                        .getLayerViewPanel()
                        .getSelectionManager()
                        .getFeaturesWithSelectedItems(targetLayer);
            for (Object o1 : selectedSource) {
                Feature source = (Feature)o1;
                for (Object o2 : selectedTarget) {
                    Feature target = (Feature)o2;
                    BasicFeature bf = new BasicFeature(linkLayer.getFeatureCollectionWrapper().getFeatureSchema());
                    Coordinate cSource = source.getGeometry().getInteriorPoint().getCoordinate();
                    Coordinate cTarget = target.getGeometry().getInteriorPoint().getCoordinate();
                    if (cSource.equals(cTarget)) {
                        bf.setGeometry(new GeometryFactory().createPoint(cSource));
                    }
                    else {
                        bf.setGeometry(new GeometryFactory().createLineString(
                            new Coordinate[]{cSource, cTarget}));
                    }
                    bf.setAttribute("SOURCE", source.getID());
                    bf.setAttribute("TARGET", target.getID());
                    bf.setAttribute("SCORE", 1.0);
                    linkLayer.getFeatureCollectionWrapper().add(bf);
                }
            }
        }
        else if ("update".equals(e.getActionCommand())) {
            try {
                MatchingUpdatePlugIn mupi = new MatchingUpdatePlugIn();
                mupi.setLinkLayerName(linkLayer.getName());
                mupi.setSourceLayerName(sourceLayer.getName());
                mupi.setTargetLayerName(targetLayer.getName());
                PlugInContext context = workbenchContext.createPlugInContext();
                if (mupi.execute(context)) {
                    new TaskMonitorManager().execute(mupi, context); 
                }
            } catch(Exception ex) {
                WorkbenchFrame.toMessage(ex);
            }
        }
    }

}
