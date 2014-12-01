package org.openjump.core.ui.plugin.layer;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateFilter;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.coordsys.CoordinateSystem;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.UndoableCommand;
import com.vividsolutions.jump.workbench.plugin.*;
import com.vividsolutions.jump.workbench.ui.HTMLFrame;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;
import com.vividsolutions.jump.workbench.ui.plugin.FeatureInstaller;
import org.cts.CRSFactory;
import org.cts.Identifier;
import org.cts.crs.CRSException;
import org.cts.crs.CoordinateReferenceSystem;
import org.cts.crs.GeodeticCRS;
import org.cts.datum.GeodeticDatum;
import org.cts.op.*;
import org.cts.registry.*;
import org.openjump.core.ccordsys.srid.SRIDStyle;
import org.openjump.swing.SuggestTreeComboBox;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.NoninvertibleTransformException;
import java.io.IOException;
import java.util.*;

/**
 * PlugIn to transform coordinates with Coordinate Transformation Suite (CTS)
 */
public class CTSPlugIn extends ThreadedBasePlugIn {

    I18N I18N_ = I18N.getInstance("cts_plugin");

    private final String REGISTRY       = I18N_.getText("cts_plugin","CTSPlugIn.registry");
    private final String SOURCE         = "source";
    private final String SOURCE_LABEL   = I18N_.getText("cts_plugin","CTSPlugIn.srcCRS");
    private final String TARGET         = "target";
    private final String TARGET_LABEL   = I18N_.getText("cts_plugin","CTSPlugIn.tgtCRS");
    private final String OP_NOT_FOUND   = I18N_.getText("cts_plugin","CTSPlugIn.op-not-found");
    private final String HETEROGEN_SRC  = I18N_.getText("cts_plugin","CTSPlugIn.heterogeneous-sources");
    private final String TRANSFORM      = I18N_.getText("cts_plugin","CTSPlugIn.transform");
    private final String REPLACE        = I18N_.getText("cts_plugin","CTSPlugIn.replace");
    private final String SOURCE_DATUM   = I18N_.getText("cts_plugin","CTSPlugIn.srcDatum");
    private final String TARGET_DATUM   = I18N_.getText("cts_plugin","CTSPlugIn.tgtDatum");
    private final String SOURCE_SPHEROID    = I18N_.getText("cts_plugin","CTSPlugIn.srcSpheroid");
    private final String TARGET_SPHEROID    = I18N_.getText("cts_plugin","CTSPlugIn.tgtSpheroid");
    private final String SOURCE_TOWGS84     = I18N_.getText("cts_plugin","CTSPlugIn.srcToWgs84");
    private final String TARGET_TOWGS84     = I18N_.getText("cts_plugin","CTSPlugIn.tgtToWgs84");
    private final String TRANSFORMED_LAYERS = I18N_.getText("cts_plugin","CTSPlugIn.transformed-layers");

    private static final String EPSG = "EPSG";
    private static final String IGNF = "IGNF";

    String registry = EPSG;
    String srcCode = "4326";
    String tgtCode = "4326";
    final Map<String,String> codes = new LinkedHashMap<String, String>(64);

    public void initialize(PlugInContext context) throws Exception {

        WorkbenchContext workbenchContext = context.getWorkbenchContext();
        FeatureInstaller featureInstaller = new FeatureInstaller(workbenchContext);

        JPopupMenu layerNamePopupMenu = context
                .getWorkbenchContext()
                .getWorkbench()
                .getFrame()
                .getLayerNamePopupMenu();

        featureInstaller.addPopupMenuPlugin(layerNamePopupMenu, this,
                new String[0],
                getName(),
                false,
                null, //getIcon(),
                createEnableCheck(workbenchContext));
    }

    public String getName() {
        return I18N_.getText("cts_plugin","CTSPlugIn");
    }

    public boolean execute(final PlugInContext context) throws Exception {

        MultiInputDialog dialog = new MultiInputDialog(context.getWorkbenchFrame(), "CoordinateTransformation", true);
        CoordinateSystem coordSystem = null;
        if (context.getSelectedLayers().length > 0 &&
                null != (coordSystem = context.getSelectedLayer(0).getFeatureCollectionWrapper().getFeatureSchema().getCoordinateSystem())) {
            srcCode = coordSystem == CoordinateSystem.UNSPECIFIED ?
                    "0" : Integer.toString(coordSystem.getEPSGCode());
        }

        final JComboBox registry_cb = dialog.addComboBox(REGISTRY, registry, Arrays.asList("EPSG", "IGNF"),"");

        codes.clear();
        codes.putAll(getAvailableCRS(context, (String) registry_cb.getSelectedItem()));

        final SuggestTreeComboBox srcCodesCB = new SuggestTreeComboBox(codes.keySet().toArray(new String[codes.size()]), 8);
        srcCodesCB.setSelectedItem(srcCode);
        srcCodesCB.setPrototypeDisplayValue("abcdefghijklmnpqrstuvwxyz/0123456789");
        dialog.addRow(SOURCE, new JLabel(SOURCE_LABEL), srcCodesCB, new EnableCheck[0], "");

        final SuggestTreeComboBox tgtCodesCB = new SuggestTreeComboBox(codes.keySet().toArray(new String[codes.size()]), 8);
        tgtCodesCB.setSelectedItem(tgtCode);
        tgtCodesCB.setPrototypeDisplayValue("abcdefghijklmnpqrstuvwxyz/0123456789");
        dialog.addRow(TARGET, new JLabel(TARGET_LABEL), tgtCodesCB, new EnableCheck[0], "");

        registry_cb.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    codes.clear();
                    codes.putAll(getAvailableCRS(context, (String) registry_cb.getSelectedItem()));
                    srcCodesCB.changeModel(codes.keySet().toArray(new String[codes.size()]));
                    tgtCodesCB.changeModel(codes.keySet().toArray(new String[codes.size()]));
                    srcCodesCB.setSelectedItem(codes.keySet().iterator().next());
                    tgtCodesCB.setSelectedItem(codes.keySet().iterator().next());
                } catch(RegistryException t) {
                    t.printStackTrace();
                } catch(CRSException t) {
                    t.printStackTrace();
                } catch(IOException t) {
                    t.printStackTrace();
                }

            }
        });

        dialog.setVisible(true);
        if (dialog.wasOKPressed()) {
            registry = dialog.getText(REGISTRY);
            srcCode = codes.get(srcCodesCB.getSelectedItem());
            tgtCode = codes.get(tgtCodesCB.getSelectedItem());
            return true;
        }
        return false;
    }

    public void run(TaskMonitor monitor, PlugInContext context)
            throws RegistryException, CRSException, CoordinateOperationException {
        reportNothingToUndoYet(context);
        if (!tgtCode.equals(srcCode)) {
            CRSFactory crsFactory = new CRSFactory();
            RegistryManager registryManager = crsFactory.getRegistryManager();
            if (registry.equals("EPSG")) {
                registryManager.addRegistry(new EPSGRegistry());
            } else if (registry.equals("IGNF")) {
                registryManager.addRegistry(new IGNFRegistry());
            }
            CoordinateReferenceSystem srcCRS = registryManager.getRegistry(registry)
                    .getCoordinateReferenceSystem(new Identifier(registry, srcCode, null));
            CoordinateReferenceSystem tgtCRS = registryManager.getRegistry(registry)
                    .getCoordinateReferenceSystem(new Identifier(registry, tgtCode, null));

            commitChanges(monitor, context, srcCRS, tgtCRS);
            HTMLFrame html = context.getOutputFrame();
            html.createNewDocument();
            html.setTitle(getName());
            html.append("<h2>" + TRANSFORMED_LAYERS + "</h2>");
            html.append(Arrays.toString(context.getSelectedLayers()));
            html.append("<h2>" + SOURCE_LABEL + "</h2>");
            html.addField(SOURCE_LABEL, srcCRS.toString());
            html.addField(SOURCE_DATUM, srcCRS.getDatum().toString());
            html.addField(SOURCE_TOWGS84, srcCRS.getDatum().getToWGS84().toString());
            html.addField(SOURCE_SPHEROID, ((GeodeticDatum)srcCRS.getDatum()).getEllipsoid().toString());
            html.append("<h2>" + TARGET_LABEL + "</h2>");
            html.addField(TARGET_LABEL, tgtCRS.toString());
            html.addField(TARGET_DATUM, tgtCRS.getDatum().toString());
            html.addField(TARGET_TOWGS84, tgtCRS.getDatum().getToWGS84().toString());
            html.addField(TARGET_SPHEROID, ((GeodeticDatum)srcCRS.getDatum()).getEllipsoid().toString());
            html.append("<h2>" + getName() + "</h2>");
            html.addField("", getOperation(srcCRS, tgtCRS).toString().replaceAll("\n","<br>"));
        }
    }

    // Commit reprojection as an undoable transaction
    private void commitChanges(final TaskMonitor monitor,
                               final PlugInContext context,
                               final CoordinateReferenceSystem srcCRS,
                               final CoordinateReferenceSystem tgtCRS) throws CoordinateOperationException {

        // Short-circuits for cases where transformation cannot be done
        CoordinateOperation coordinateOperation = getOperation(srcCRS, tgtCRS);
        if (coordinateOperation == null) {
            context.getWorkbenchFrame().warnUser(OP_NOT_FOUND);
            return;
        }
        Layer[] layers = context.getLayerNamePanel().getSelectedLayers();
        if (layers.length == 0) {
            // Should never reach here if the plugin has been called from UI
            return;
        }

        // Prepare parameters and data structures for transaction
        final CoordinateFilter filter = getCoordinateFilter(coordinateOperation);
        boolean epsg = tgtCRS.getAuthorityName().equalsIgnoreCase(EPSG);
        int epsgCode = epsg ? Integer.parseInt(tgtCRS.getAuthorityKey()) : 0;
        CoordinateSystemWrapper newCoordinateSystem = new CoordinateSystemWrapper(tgtCRS);
        final Map<String,ArrayList<Geometry>> srcGeometryMap = new HashMap<String,ArrayList<Geometry>>();
        final Map<String,ArrayList<Geometry>> tgtGeometryMap = new HashMap<String,ArrayList<Geometry>>();
        final Map<String,CoordinateSystem> oldCoordinateSystems = new HashMap<String,CoordinateSystem>();
        final Map<String,SRIDStyle> oldSridStyles = new HashMap<String,SRIDStyle>();
        final Map<String,CoordinateSystem> newCoordinateSystems = new HashMap<String,CoordinateSystem>();

        // Start transaction
        context.getLayerManager().getUndoableEditReceiver().reportNothingToUndoYet();
        for (Layer layer : context.getSelectedLayers()) {
            oldCoordinateSystems.put(layer.getName(), layer.getFeatureCollectionWrapper().getFeatureSchema().getCoordinateSystem());
            ArrayList<Geometry> srcGeometries = new ArrayList<Geometry>();
            ArrayList<Geometry> tgtGeometries = new ArrayList<Geometry>();
            int count = 0;
            monitor.report(TRANSFORM + " " + layer.getName());
            for (Object object : layer.getFeatureCollectionWrapper().getFeatures()) {
                Geometry srcGeom = ((Feature)object).getGeometry();
                srcGeometries.add(srcGeom);
                Geometry tgtGeom = (Geometry)srcGeom.clone();
                tgtGeom.apply(filter);
                tgtGeom.setSRID(epsgCode);
                tgtGeom.geometryChanged();
                tgtGeometries.add(tgtGeom);
                if (++count % 100 == 0) monitor.report(count, layer.getFeatureCollectionWrapper().getFeatures().size(), "");
            }
            srcGeometryMap.put(layer.getName(), srcGeometries);
            tgtGeometryMap.put(layer.getName(), tgtGeometries);
            oldSridStyles.put(layer.getName(), (SRIDStyle)layer.getStyle(SRIDStyle.class));
            oldCoordinateSystems.put(layer.getName(), layer.getFeatureCollectionWrapper().getFeatureSchema().getCoordinateSystem());
            newCoordinateSystems.put(layer.getName(), newCoordinateSystem);
        }
        UndoableCommand cmd = new UndoableCommand(getName()) {
            public void execute() {
                boolean isFiringEvents = context.getLayerManager().isFiringEvents();
                context.getLayerManager().setFiringEvents(false);
                for (Layer layer : context.getSelectedLayers()) {
                    monitor.report(REPLACE + " " + layer.getName());
                    List<Feature> features = layer.getFeatureCollectionWrapper().getFeatures();
                    ArrayList<Geometry> geometries = tgtGeometryMap.get(layer.getName());
                    CoordinateSystem cs = newCoordinateSystems.get(layer.getName());
                    for (int i = 0 ; i < features.size() ; i++) {
                        Feature feature = features.get(i);
                        feature.setGeometry(geometries.get(i));
                    }
                    Layer.tryToInvalidateEnvelope(layer);
                    layer.removeStyle(layer.getStyle(SRIDStyle.class));
                    SRIDStyle sridStyle = new SRIDStyle();
                    sridStyle.setSRID(cs.getEPSGCode());
                    layer.addStyle(sridStyle);
                    layer.getFeatureCollectionWrapper().getFeatureSchema().setCoordinateSystem(cs);
                    layer.setFeatureCollectionModified(true);
                }
                context.getLayerManager().setFiringEvents(isFiringEvents);
                try {
                    context.getLayerViewPanel().getViewport().zoomToFullExtent();
                } catch(NoninvertibleTransformException e) {
                    e.printStackTrace();
                }
            }

            public void unexecute() {
                boolean isFiringEvents = context.getLayerManager().isFiringEvents();
                context.getLayerManager().setFiringEvents(false);
                for (Layer layer : context.getSelectedLayers()) {
                    List<Feature> features = layer.getFeatureCollectionWrapper().getFeatures();
                    ArrayList<Geometry> geometries = srcGeometryMap.get(layer.getName());
                    CoordinateSystem cs = oldCoordinateSystems.get(layer.getName());
                    for (int i = 0 ; i < features.size() ; i++) {
                        Feature feature = features.get(i);
                        feature.setGeometry(geometries.get(i));
                    }
                    Layer.tryToInvalidateEnvelope(layer);
                    layer.removeStyle(layer.getStyle(SRIDStyle.class));
                    if (oldSridStyles.get(layer.getName()) != null) layer.addStyle(oldSridStyles.get(layer.getName()));
                    layer.getFeatureCollectionWrapper().getFeatureSchema().setCoordinateSystem(cs);
                    layer.setFeatureCollectionModified(true);
                }
                context.getLayerManager().setFiringEvents(isFiringEvents);
                try {
                    context.getLayerViewPanel().getViewport().zoomToFullExtent();
                } catch(NoninvertibleTransformException e) {
                    e.printStackTrace();
                }
            }
        };
        boolean exceptionOccurred = true;
        try {
            cmd.execute();
            exceptionOccurred = false;
        }
        finally {
            if (exceptionOccurred) {
                context.getLayerManager().getUndoableEditReceiver()
                        .getUndoManager().discardAllEdits();
            }
        }
        context.getLayerManager().getUndoableEditReceiver().receive(cmd.toUndoableEdit());
    }

    private CoordinateOperation getOperation(final CoordinateReferenceSystem srcCRS,
                                             final CoordinateReferenceSystem tgtCRS)
            throws CoordinateOperationException{
        Set<CoordinateOperation> ops = CoordinateOperationFactory
                .createCoordinateOperations((GeodeticCRS) srcCRS, (GeodeticCRS) tgtCRS);
        CoordinateOperation op = ops.size() == 0 ? null : CoordinateOperationFactory.getMostPrecise(ops);
        return op;
    }

    private CoordinateFilter getCoordinateFilter(final CoordinateOperation op) {
        return new CoordinateFilter() {
            @Override
            public void filter(Coordinate coordinate) {
                try {
                    double[] xyz = op.transform(new double[]{coordinate.x, coordinate.y, coordinate.z});
                    coordinate.setOrdinate(0, xyz[0]);
                    coordinate.setOrdinate(1, xyz[1]);
                    if (xyz.length > 2) coordinate.setOrdinate(2, xyz[2]);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };
    }

    private Map<String,String> getAvailableCRS(PlugInContext context, String registry)
            throws IOException, RegistryException, CRSException {
        return RegistryReader.read(registry);
    }

    private EnableCheck createEnableCheck(final WorkbenchContext context) {
        return new MultiEnableCheck()
                .add(EnableCheckFactory.getInstance().createTaskWindowMustBeActiveCheck())
                .add(EnableCheckFactory.getInstance().createAtLeastNLayersMustBeSelectedCheck(1))
                .add(new EnableCheck() {
                    @Override
                    public String check(JComponent component) {
                        Layer[] layers = context.getLayerNamePanel().getSelectedLayers();
                        if (layers.length > 0) {
                            if (!layers[0].isEditable()) return "Selected layers must be editable";
                            CoordinateSystem cs = layers[0].getFeatureCollectionWrapper().getFeatureSchema().getCoordinateSystem();
                            for (int i = 1 ; i < layers.length ; i++) {
                                if (!layers[i].isEditable()) return "Selected layers must be editable";
                                CoordinateSystem csi = layers[i].getFeatureCollectionWrapper().getFeatureSchema().getCoordinateSystem();
                                if (cs == csi) continue;
                                if (cs == null && csi != null) return HETEROGEN_SRC;
                                if (cs != null && csi == null) return HETEROGEN_SRC;
                                if (cs == CoordinateSystem.UNSPECIFIED && csi != CoordinateSystem.UNSPECIFIED) return HETEROGEN_SRC;
                                if (cs != CoordinateSystem.UNSPECIFIED && csi == CoordinateSystem.UNSPECIFIED) return HETEROGEN_SRC;
                                if (cs != null && csi!= null && cs.getEPSGCode() != csi.getEPSGCode()) return HETEROGEN_SRC;
                            }
                            return null;
                        }
                        // should never reach here
                        return layers.length > 0 ? null : "At least 1 layer must be selected";
                    }
                });
    }

}
