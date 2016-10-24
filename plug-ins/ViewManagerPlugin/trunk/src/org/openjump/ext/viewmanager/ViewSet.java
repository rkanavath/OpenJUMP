package org.openjump.ext.viewmanager;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * A set of Views with some methods to move views up and down
 * and to replace a view definition.
 */
@XmlRootElement
public class ViewSet {

    public static int REMOVE  = -1;
    public static int MOVMOD  =  0;
    public static int ADD     =  1;
    public static int REPLACE =  2;

    List<Listener> listeners = new ArrayList<Listener>();

    private String name;

    @XmlElement (name="view")
    List<View> views = new ArrayList<View>();

    public ViewSet() {}

    public ViewSet(String name) {
        this.name = name;
    }

    public void addView(View view) {
        views.add(view);
        fireAddView(view);
    }

    public void removeView(View view) {
        views.remove(view);
        fireRemoveView(view);
    }

    public void moveViewToTop(View view) {
        int pos = views.indexOf(view);
        if (pos < views.size()-1) {
            views.remove(view);
            views.add(view);
            fireMoveView(view);
        }
    }

    public void moveViewUp(View view) {
        int pos = views.indexOf(view);
        if (pos < views.size()-1) {
            views.remove(pos);
            views.add(pos + 1, view);
            fireMoveView(view);
        }
    }

    public void moveViewDown(View view) {
        int pos = views.indexOf(view);
        if (pos>0) {
            views.remove(pos);
            views.add(pos-1, view);
            fireMoveView(view);
        }
    }

    public void moveViewToBottom(View view) {
        int pos = views.indexOf(view);
        if (pos>0) {
            views.remove(pos);
            views.add(0, view);
            fireMoveView(view);
        }
    }

    public void replaceView(View oldView, View newView) {
        int pos = views.indexOf(oldView);
        views.set(pos, newView);
        fireReplaceView(oldView);
    }

    public void fireAddView(View view) {
        for (Listener listener : listeners) {
            listener.actionPerformed(this, ADD, view);
        }
    }

    public void fireRemoveView(View view) {
        for (Listener listener : listeners) {
            listener.actionPerformed(this, REMOVE, view);
        }
    }

    public void fireMoveView(View view) {
        for (Listener listener : listeners) {
            listener.actionPerformed(this, MOVMOD, view);
        }
    }

    public void fireReplaceView(View oldView) {
        for (Listener listener : listeners) {
            listener.actionPerformed(this, REPLACE, oldView);
        }
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public boolean isEmpty() {
        return views.isEmpty();
    }

    public void addListener(Listener listener) {
        listeners.add(listener);
    }

    public void removeListener(Listener listener) {
        listeners.remove(listener);
    }

    public String toString() {
        return name;
    }

    public static interface Listener {
        void actionPerformed(ViewSet viewSet, int mod, View view);
    }

}
