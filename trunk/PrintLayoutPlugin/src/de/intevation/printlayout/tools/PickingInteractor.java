/*
 * PickingInteractor.java
 * ------------------
 * (c) 2007 by Intevation GmbH
 *
 * @author Sascha L. Teichmann (teichmann@intevation.de)
 * @author Ludwig Reiter       (ludwig@intevation.de)
 *
 * This program is free software under the LGPL (>=v2.1)
 * Read the file LICENSE.txt coming with the sources for details.
 */
package de.intevation.printlayout.tools;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.Component;
import java.awt.Color;

import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;

import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.GeneralPath;

import javax.swing.SwingUtilities;

import org.apache.batik.swing.gvt.InteractorAdapter;
import org.apache.batik.swing.gvt.Overlay;

import org.apache.batik.dom.AbstractElement;

import org.apache.batik.dom.svg.SVGGraphicsElement;

import de.intevation.printlayout.MatrixTools;
import de.intevation.printlayout.DocumentManager;

import org.w3c.dom.svg.SVGSVGElement;
import org.w3c.dom.svg.SVGMatrix;
import org.w3c.dom.svg.SVGException;
import org.w3c.dom.svg.SVGRect;
import org.w3c.dom.svg.SVGDocument;

import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.EventObject;

public class PickingInteractor
extends      InteractorAdapter
implements   Overlay, Tool
{
	public static final String IDENTIFIER = "picking-tool";

	public static class PickingEvent
	extends             EventObject
	{
		public PickingEvent(Object source) {
			super(source);
		}
	} // class PickingEvent

	public interface PickingListener {

		void selectionChanged(PickingEvent evt);

	} // interface PickingListener


	protected static class OnScreenBox
	{
		String id;
		Shape  shape;

		OnScreenBox() {
		}

		OnScreenBox(String id) {
			this.id = id;
		}

		boolean inside(int x, int y) {
			return shape != null && shape.contains(x, y);
		}

		public boolean equals(Object other) {
			return id.equals(((OnScreenBox)other).id);
		}

		public String toString() {
			return id;
		}
	} // class OnScreenBox

	protected boolean inUse;
	protected boolean finished;
	
	protected DocumentManager documentManager;

	protected ArrayList       selected;

	protected ArrayList       listeners;

	// used for dragging selected items
	protected int startX = Integer.MIN_VALUE;
	protected int startY = Integer.MIN_VALUE;


	public PickingInteractor() {
	}

	public PickingInteractor(DocumentManager documentManager) {
		setDocumentManager(documentManager);
	}
	
	public void setDocumentManager(DocumentManager documentManager) {
		this.documentManager = documentManager;
	}

	public DocumentManager getDocumentManager() {
		return documentManager;
	}

	public void addPickingListener(PickingListener listener) {
		if (listener != null) {
			if (listeners == null) {
				listeners = new ArrayList(5);
				listeners.add(listener);
			}
			else if (!listeners.contains(listeners))
				listeners.add(listener);
		}
	}

	public void removePickingListener(PickingListener listener) {
		if (listener  != null
		&&  listeners != null
		&&  listeners.remove(listener) 
		&&  listeners.isEmpty());
			listeners = null;
	}

	public String getToolIdentifier() {
		return IDENTIFIER;
	}

	public void setInUse(boolean inUse) {
		this.inUse = inUse;
	}

	public boolean getInUse() {
		return inUse;
	}

	public boolean startInteraction(InputEvent ie) {
		if (!inUse) return false;

		boolean start = ie instanceof MouseEvent;

		if (start)
			finished = false;

		return start;
	}
	
	public boolean endInteraction() {
		return finished;
	}

	protected static final boolean isShiftDown(int modifiers) {
		return (modifiers & InputEvent.SHIFT_MASK) != 0;
	}

	protected static final boolean isCtrlDown(int modifiers) {
		return (modifiers & InputEvent.CTRL_MASK) != 0;
	}
	
	public void mouseClicked(MouseEvent me) {

		finished = true;

		int modifiers = me.getModifiers();

		if ((modifiers & InputEvent.BUTTON1_MASK) != InputEvent.BUTTON1_MASK)
			return;

		int x = me.getX();
		int y = me.getY();

		ArrayList result = query(x, y);

		int N = result.size();

		boolean changed = false;

		if (isCtrlDown(modifiers)) { // add to selected
			if (N > 0) {
				Object last = result.get(N-1);
				if (selected == null) { // empty list
					selected = new ArrayList();
					selected.add(last);
					changed = true;
				}
				else {
					if (!selected.contains(last)) {
						selected.add(last);
						changed = true;
					}
				}
			}
		}
		else if (isShiftDown(modifiers)) { // remove from selection
			if (N > 0 && selected != null) {
				Object last = result.get(N-1);

				if (selected.remove(last)) {
					changed = true;
					if (selected.isEmpty())
						selected = null;
				}
			}
		}
		else { // select only one. if missed target deselect all
			if (N > 0) {
				if (selected != null) selected.clear();
				else                  selected = new ArrayList();
				selected.add(result.get(N-1));
			}
			else
				selected = null;

			changed = true;
		}

		((Component)me.getSource()).repaint();

		if (changed)
			fireSelectionChanged();
	}

	protected void fireSelectionChanged() {
		if (listeners != null) {
			PickingEvent evt = new PickingEvent(this);
			for (int i = listeners.size()-1; i >= 0; --i)
				((PickingListener)listeners.get(i)).selectionChanged(evt);
		}
	}

	/** spatial query all for all objects around (x, y) in screen coordinate */
	protected ArrayList query(int x, int y) {
		return query(x, y, true);
	}

	protected ArrayList query(int x, int y, boolean directlyInSheet) {

		ArrayList ordered = new ArrayList();

		SVGDocument document = documentManager.getSVGDocument();

		SVGSVGElement sheet =
			(SVGSVGElement)document.getElementById(DocumentManager.DOCUMENT_SHEET);
		
		AffineTransform xform;
		try {
			xform	= MatrixTools.toJavaTransform(
				sheet.getScreenCTM().inverse());
		}
		catch (SVGException se) {
			se.printStackTrace();
			return ordered;
		}

		Point2D screenPoint   = new Point2D.Double(x, y);
		Point2D documentPoint = new Point2D.Double();

		xform.transform(screenPoint, documentPoint);

		SVGRect query = sheet.createSVGRect();
		query.setX((float)documentPoint.getX() - 0.25f);
		query.setY((float)documentPoint.getY() - 0.25f);

		query.setWidth(0.5f); // half mm
		query.setHeight(0.5f);

		NodeList result = sheet.getIntersectionList(query, null);

		int N = result.getLength();

		for (int i = 0; i < N; ++i) {
			AbstractElement obj = (AbstractElement)result.item(i);
			String last = null;
			AbstractElement lastElement = null;
			do {
				AbstractElement parent = (AbstractElement)obj.getParentNode();
				if (parent == null)
					break;
				String id = parent.getAttributeNS(null, "id");
				if (id != null && id.startsWith(DocumentManager.OBJECT_ID)) {
					last = id;
					lastElement = parent;
				}
				obj = parent;
			}
			while (obj != null && obj != sheet);

			if (last != null) { 
				if (directlyInSheet 
				&& (lastElement == null || lastElement.getParentNode() != sheet))
					continue;

				OnScreenBox box = new OnScreenBox(last);

				if (!ordered.contains(box))
					ordered.add(box);
			}
		}

		return ordered;
	}
  
	public void paint(Graphics g) {
		if (!inUse || selected == null)
			return;

		Graphics2D g2d = (Graphics2D)g;

		g2d.setPaint(Color.red);

		SVGDocument document = documentManager.getSVGDocument();

		for (int i = 0; i < selected.size();) {

			OnScreenBox box = (OnScreenBox)selected.get(i);

			SVGGraphicsElement element =
				(SVGGraphicsElement)document.getElementById(box.id);

			if (element == null) { // no available any longer
				selected.remove(i);
				continue;
			}

			++i;

			SVGRect bbox = element.getBBox();
			SVGMatrix matrix = element.getScreenCTM();

			AffineTransform xform = MatrixTools.toJavaTransform(matrix);

			Rectangle2D rect = new Rectangle2D.Float(
				(float)bbox.getX(), 
				(float)bbox.getY(),
				(float)bbox.getWidth(),
				(float)bbox.getHeight());

			GeneralPath path = new GeneralPath(rect);

			// cache and draw
			g2d.draw(box.shape = path.createTransformedShape(xform));
		}
		
		// some one has removed the objects from DOM
		// inform listeners that selection is no longer valid
		if (selected.isEmpty()) {
			selected = null;
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					fireSelectionChanged();
				}
			});
		}
	}

	public void mouseDragged(MouseEvent e) {
		if (startX != Integer.MIN_VALUE) {
			int x = e.getX();
			int y = e.getY();

			boolean found = false;

			for (int i = numSelections()-1; i >= 0; --i)
				if (((OnScreenBox)selected.get(i)).inside(x, y)) {
					found = true;
					break;
				}

			int dx = x - startX;
			int dy = y - startY;
			startX = x;
			startY = y;

			if (found)
				documentManager.translateIDs(
					getSelectedIDs(), new Point2D.Double(dx, dy));
		}
		else
			finished = true;
	}

	public void mouseEntered(MouseEvent e) {
		finished = true;
	}

	public void mouseExited(MouseEvent e) {
		startX = Integer.MIN_VALUE;
		startY = Integer.MIN_VALUE;
		finished = true;
	}

	public void mouseMoved(MouseEvent e) {
		finished = true;
	}

	public void mousePressed(MouseEvent e) {
		startX = e.getX();
		startY = e.getY();
		finished = true;
	}

	public void mouseReleased(MouseEvent e) {
		mouseExited(e);
		//finished = true;
	}

	public int numSelections() {
		return selected == null ? 0 : selected.size();
	}

	public boolean hasSelection() {
		return selected != null && !selected.isEmpty();
	}

	public String [] getSelectedIDs() {
		if (!hasSelection())
			return null;

		String [] ids = new String[selected.size()];

		for (int i = 0; i < ids.length; ++i)
			ids[i] = ((OnScreenBox)selected.get(i)).id;

		return ids;
	}

	public void clearSelection() {
		if (selected != null) {
			selected = null;
			fireSelectionChanged();
		}
	}
}
// end of file
