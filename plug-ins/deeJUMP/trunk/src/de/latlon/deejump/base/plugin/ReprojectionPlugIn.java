//$HeadURL$
/*----------------    FILE HEADER  ------------------------------------------
 This file is part of deegree.
 Copyright (C) 2001-2008 by:
 Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/deegree/
 lat/lon GmbH
 http://www.lat-lon.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.
 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 Lesser General Public License for more details.
 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 Contact:

 Andreas Poth
 lat/lon GmbH
 Aennchenstr. 19
 53177 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Prof. Dr. Klaus Greve
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: greve@giub.uni-bonn.de
 ---------------------------------------------------------------------------*/

package de.latlon.deejump.base.plugin;

import static com.vividsolutions.jump.workbench.model.Layer.tryToInvalidateEnvelope;
import static com.vividsolutions.jump.workbench.ui.MenuNames.LAYER;
import static de.latlon.deejump.base.i18n.I18N.get;
import static java.awt.GridBagConstraints.CENTER;
import static java.awt.GridBagConstraints.WEST;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.deegree.crs.coordinatesystems.CoordinateSystem;
import org.deegree.model.crs.CRSFactory;
import org.deegree.model.crs.GeoTransformer;
import org.deegree.model.crs.UnknownCRSException;
import org.deegree.model.spatialschema.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.ThreadedBasePlugIn;

/**
 * <code>ReprojectionPlugIn</code>
 * 
 * @author <a href="mailto:schmitz@lat-lon.de">Andreas Schmitz</a>
 * @author last edited by: $Author$
 * 
 * @version $Revision$, $Date$
 */
public class ReprojectionPlugIn extends ThreadedBasePlugIn {

    CoordinateSystem source, target;

    @Override
    public void initialize( PlugInContext context ) {
        EnableCheckFactory enableCheckFactory = new EnableCheckFactory( context.getWorkbenchContext() );

        MultiEnableCheck enableCheck = new MultiEnableCheck();
        enableCheck.add( enableCheckFactory.createWindowWithLayerManagerMustBeActiveCheck() );
        enableCheck.add( enableCheckFactory.createAtLeastNLayerablesMustBeSelectedCheck( 1, Layerable.class ) );

        context.getFeatureInstaller().addMainMenuItem( this, new String[] { LAYER },
                                                       get( "ReprojectionPlugIn.name" ) + "{pos:17}", false, null,
                                                       enableCheck );
    }

    @Override
    public boolean execute( PlugInContext context ) {
        final JDialog dlg = new JDialog() {
            private static final long serialVersionUID = 5452020145083027223L;

            {
                Container p = getContentPane();
                p.setLayout( new GridBagLayout() );
                GridBagConstraints gb = new GridBagConstraints();
                gb.insets = new Insets( 2, 2, 2, 2 );
                gb.anchor = WEST;

                gb.gridx = 0;
                gb.gridy = 0;
                p.add( new JLabel( get( "ReprojectionPlugIn.sourcesrs" ) ), gb );
                ++gb.gridx;
                final JTextField sourceField = new JTextField( "EPSG:4326", 20 );
                p.add( sourceField, gb );
                ++gb.gridy;
                gb.gridx = 0;
                gb.gridwidth = 2;
                final JLabel sourceLabel = new JLabel( "(dummy)" );
                p.add( sourceLabel, gb );

                gb.gridwidth = 1;
                ++gb.gridy;
                gb.gridx = 0;
                p.add( new JLabel( get( "ReprojectionPlugIn.targetsrs" ) ), gb );
                ++gb.gridx;
                final JTextField targetField = new JTextField( "EPSG:31467", 20 );
                p.add( targetField, gb );
                ++gb.gridy;
                gb.gridx = 0;
                gb.gridwidth = 2;
                final JLabel targetLabel = new JLabel( "(dummy)" );
                p.add( targetLabel, gb );

                ++gb.gridy;
                gb.gridx = 0;
                gb.gridwidth = 1;
                gb.anchor = CENTER;
                final JButton ok = new JButton( get( "General.ok" ) );
                p.add( ok, gb );
                ++gb.gridx;
                final JButton cancel = new JButton( get( "General.cancel" ) );
                p.add( cancel, gb );

                ActionListener listener = new ActionListener() {
                    public void actionPerformed( ActionEvent e ) {
                        if ( e.getSource() == cancel ) {
                            source = null;
                            target = null;
                        }
                        setVisible( false );
                        dispose();
                    }
                };
                DocumentListener dl = new DocumentListener() {
                    public void changedUpdate( DocumentEvent e ) {
                        try {
                            if ( e.getDocument() == sourceField.getDocument() ) {
                                source = CRSFactory.create( sourceField.getText() ).getCRS();
                                sourceLabel.setText( source.getName() );
                            } else {
                                target = CRSFactory.create( targetField.getText() ).getCRS();
                                targetLabel.setText( target.getName() );
                            }
                        } catch ( UnknownCRSException ex ) {
                            if ( e.getDocument() == sourceField.getDocument() ) {
                                source = null;
                                sourceLabel.setText( get( "ReprojectionPlugIn.invalidsrs" ) );
                            } else {
                                target = null;
                                targetLabel.setText( get( "ReprojectionPlugIn.invalidsrs" ) );
                            }
                        }
                        ok.setEnabled( source != null && target != null );
                    }

                    public void insertUpdate( DocumentEvent e ) {
                        changedUpdate( e );
                    }

                    public void removeUpdate( DocumentEvent e ) {
                        changedUpdate( e );
                    }
                };
                getRootPane().setDefaultButton( ok );
                sourceField.getDocument().addDocumentListener( dl );
                targetField.getDocument().addDocumentListener( dl );
                ok.addActionListener( listener );
                cancel.addActionListener( listener );
                sourceField.setText( "EPSG:4326" );
                targetField.setText( "EPSG:31466" );
            }
        };

        dlg.pack();
        dlg.setModal( true );
        dlg.setResizable( false );
        dlg.setVisible( true );

        return source != null && target != null;
    }

    public void run( TaskMonitor monitor, PlugInContext context )
                            throws Exception {
        GeoTransformer trans = new GeoTransformer( target );

        for ( Layer l : context.getSelectedLayers() ) {
            for ( Object o : l.getFeatureCollectionWrapper().getFeatures() ) {
                Feature f = (Feature) o;
                Geometry g = f.getGeometry();
                org.deegree.model.spatialschema.Geometry deeg = JTSAdapter.wrap( g );
                g = JTSAdapter.export( trans.transform( deeg, source ) );
                f.setGeometry( g );
            }
            l.fireAppearanceChanged();
            tryToInvalidateEnvelope( l );
        }
    }

}
