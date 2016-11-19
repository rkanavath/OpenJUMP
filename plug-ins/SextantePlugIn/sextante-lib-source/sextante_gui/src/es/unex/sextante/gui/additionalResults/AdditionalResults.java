package es.unex.sextante.gui.additionalResults;

import java.util.ArrayList;

import es.unex.sextante.core.ObjectAndDescription;
import es.unex.sextante.gui.core.SextanteGUI;

/**
 * This class stores results generated by SEXTANTE geoalgorithms that are
 * neither layers nor tables (i.e text results and charts). They are all stored
 * as java Component objects, so they can be shown in a dialogs. Charts do not
 * have to be modified, since they already extend the Component class. Text
 * strings should be put into some kind of panel or TextArea. This is not done
 * by this class, but should be done by the corresponding post-process task (see
 * {IPostProcessFactory})
 *
 * @author volaya
 *
 */
public class AdditionalResults {

    private static ArrayList<ObjectAndDescription> m_Components = new ArrayList<ObjectAndDescription>();

    /**
     * Shows a panel with results, only if there is at least one of them,
     */
    public static void showPanel() {

        if (m_Components == null || m_Components.size() == 0) {
            return;
        }

        SextanteGUI.getGUIFactory().showAdditionalResultsDialog(m_Components);

    }

    /**
     * Returns the list of results as a list of components
     *
     * @return the list of results
     */
    public static ArrayList<ObjectAndDescription> getComponents() {

        return m_Components;

    }

    /**
     * Adds a new result
     *
     * @param oad
     *            a result and its description, to be shown in the corresponding
     *            panel
     */
    public static void addComponent(final ObjectAndDescription oad) {

        m_Components.add(oad);

    }

    /**
     * Removes a result
     *
     * @param oad
     *            The result to remove
     */
    public static void removeComponent(final ObjectAndDescription oad) {

        m_Components.remove(oad);

    }

}
