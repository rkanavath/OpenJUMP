/* 
 * Kosmo - Sistema Abierto de Informaci�n Geogr�fica
 * Kosmo - Open Geographical Information System
 *
 * http://www.saig.es
 * (C) 2008, SAIG S.L.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation;
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *
 * For more information, contact:
 * 
 * Sistemas Abiertos de Informaci�n Geogr�fica, S.L.
 * Avnda. Rep�blica Argentina, 28
 * Edificio Domocenter Planta 2� Oficina 7
 * C.P.: 41930 - Bormujos (Sevilla)
 * Espa�a / Spain
 *
 * Tel�fono / Phone Number
 * +34 954 788876
 * 
 * Correo electr�nico / Email
 * info@saig.es
 *
 */
package org.openjump.advancedtools.tools.cogo;

import java.awt.Cursor;
import java.awt.Graphics2D;

import javax.swing.*;
import javax.swing.event.InternalFrameAdapter;
import javax.swing.event.InternalFrameEvent;

import org.apache.log4j.Logger;
import org.openjump.advancedtools.icon.IconLoader;
import org.openjump.advancedtools.language.I18NPlug;
import org.openjump.core.ui.swing.DetachableInternalFrame;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jump.workbench.JUMPWorkbench;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.LayerNamePanelProxy;
import com.vividsolutions.jump.workbench.ui.LayerViewPanel;
import com.vividsolutions.jump.workbench.ui.cursortool.CursorTool;
import com.vividsolutions.jump.workbench.ui.cursortool.SelectFeaturesTool;
import com.vividsolutions.jump.workbench.ui.cursortool.editing.FeatureDrawingUtil;

/**
 * Herramienta para dibujar una linea, con opcion de a�adir vetices pinchando en
 * ventana, o mediante ventana de comandos
 * <p>
 * </p>
 * 
 * @author Eduardo Montero Ruiz
 * @since Kosmo 1.0.0
 */
public class DrawGeometryCommandsTool extends DrawGeometryTool {// DrawLineStringTool
                                                                // {
    /** Cursor of the tool */

    public static final Cursor CURSOR = createCursor(IconLoader.icon(
            "commands_cursor.png").getImage());

    /** Nombre asociado a la herramienta */
    public final static String NAME = I18NPlug
            .getI18N("org.openjump.core.ui.tools.DrawLineStringCommandsTool.draw-with-commands");

    /** Nombre asociado a la herramienta */
    public final static String DESCRIPTION = I18NPlug
            .getI18N("org.openjump.core.ui.tools.DrawLineStringCommandsTool.description");

    /** Log */
    private final static Logger LOGGER = Logger
            .getLogger(DrawGeometryCommandsTool.class);

    protected DrawGeometryCommandsTool(FeatureDrawingUtil featureDrawingUtil) {
        super(featureDrawingUtil);
    }


    public static CursorTool create(LayerNamePanelProxy layerNamePanelProxy) {
        FeatureDrawingUtil featureDrawingUtil = new FeatureDrawingUtil(
                layerNamePanelProxy);
        final DrawGeometryCommandsTool tool = new DrawGeometryCommandsTool(featureDrawingUtil);
        return featureDrawingUtil.prepare(tool, true);
    }

    /**
     * Devuelve el nombre asociado a la herramienta
     * 
     * @return String - Nombre asociado a la herramienta
     */
    @Override
    public String getName() {

        String tooltip = "";
        tooltip = "<HTML><BODY>";
        tooltip += "<DIV style=\"width: 300px; text-justification: justify;\">";
        tooltip += "<b>" + NAME + "</b>" + "<br>";
        tooltip += DESCRIPTION + "<br>";
        tooltip += "</DIV></BODY></HTML>";
        return tooltip;
    }

    @Override
    public Icon getIcon() {
        return IconLoader.icon("drawCommands.png");
    }

    @Override
    public Cursor getCursor() {
        return CURSOR;
    }

    /**
     * Activa la herramienta, anyadiendole un se�alizador de snap
     */
    /*
    @Override
    public void activate(LayerViewPanel layerViewPanel) {
        super.activate(layerViewPanel);
    }
    */

    @Override
    public void deactivate() {
        super.deactivate();
    }

    /**
     * Indicador de gesto terminado
     */
    @Override
    public void gestureFinished() throws Exception {
        super.gestureFinished();
    }

    /**
     * Termina la ejecuci�n de la herramienta
     */
    public void setGestureFinished() {
        try {
            super.finishGesture();
        } catch (Exception e) {
            LOGGER.error("", e); //$NON-NLS-1$
        }
    }

    /**
     * Anyade una nueva coordenada a la linea que se esta trazando
     * 
     * @param coordinate
     */
    public void addCoordinate(Coordinate coordinate) {
        try {
            Graphics2D graphics = (Graphics2D) JUMPWorkbench.getInstance()
                    .getFrame().getContext().getLayerViewPanel().getGraphics();
            this.drawShapeXOR(graphics);
            this.add(coordinate);
            this.drawShapeXOR(graphics);
        } catch (Exception e) {
            LOGGER.error("", e);
        }
    }

    public void removeCoordinate(Coordinate coordinate) {
        try {
            Graphics2D graphics = (Graphics2D) JUMPWorkbench.getInstance()
                    .getFrame().getContext().getLayerViewPanel().getGraphics();
            this.drawShapeXOR(graphics);
            this.getCoordinates().remove(coordinate);
            this.drawShapeXOR(graphics);
            // this.drawShapeXOR(graphics);
        } catch (Exception e) {
            LOGGER.error("", e);
        }
    }

}
