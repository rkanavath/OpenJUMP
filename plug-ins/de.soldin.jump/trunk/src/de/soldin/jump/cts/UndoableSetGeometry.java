/**
 * @(#)UndoableSetGeometry.java	29.06.2004
 *
 * Copyright 2004 Edgar Soldin
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package de.soldin.jump.cts;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import javax.swing.undo.AbstractUndoableEdit;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.workbench.model.Layer;

/**
 * The <code>UndoableSetGeometry</code> is a implementation of a 
 * {@link java.util.Collection}, as well as a {@link 
 * javax.swing.undo.AbstractUndoableEdit}. The purpose is to have 
 * an undoable swing component for modifying geometries.
 * <p>
 * With these capabilities joined it can act as a container for multiple
 * <code>UndoableSetGeometry</code> objects, which can be executed in 
 * a batch and as a single action.
 * </p><p>
 * Attention: This class differs from {@link de.soldin.jump.UndoableSetGeometry}
 * as it specializes in applying <code>CSSettings</code> on the appropriate layer. So the more 
 * generic one is {@link de.soldin.jump.UndoableSetGeometry}.
 * <p>
 */
public class UndoableSetGeometry 
	extends AbstractUndoableEdit
	implements Collection
	{
	private Collection transformations = new Vector();
	private String name_prefix = ""; 
	private String name = "";
	
	private CSSetting css = null;
	private HashMap proposed_geoms = new HashMap();
	private HashMap original_geoms = new HashMap();	
	
	public void redo() {
		execute();
		super.redo();
	}

	public void undo() {
		unexecute();
		super.undo();
	}
	
	public String getPresentationName(){
		return getName();
	}
	
	public String getUndoPresentationName(){
		return getName();
	}
	
	public String getRedoPresentationName(){
		return getName();
	}

	public UndoableSetGeometry(String name) {
		this(null, name, null);
	}

	public UndoableSetGeometry(String prefix, String name) {
		this(prefix, name, null);
	}

	public UndoableSetGeometry(String prefix, String name, CSSetting css) {
		this.name_prefix = prefix;
		this.name = name;
		this.css = css;
	}

	public void execute() {
		//System.out.print("UT:execute() "+this+" ");
		if (css!=null){
			//System.out.print("css ");				
			List features = css.layer.getFeatureCollectionWrapper().getFeatures();
			ArrayList modifiedFeatures = new ArrayList();
			ArrayList modifiedFeaturesOldClones = new ArrayList();
			
			for (Iterator iter = proposed_geoms.keySet().iterator(); iter.hasNext();) {
				Feature feature = (Feature) iter.next();
				Geometry new_geom = (Geometry)proposed_geoms.get(feature);
				Geometry old_geom = feature.getGeometry();
				
				original_geoms.put(feature,old_geom);

				modifiedFeatures.add(feature);
				modifiedFeaturesOldClones.add(feature.clone());
				feature.setGeometry(new_geom);
			}
			
			Layer.tryToInvalidateEnvelope(css.layer);
			// fire the appropriate event, so everybody gets notified
			if (!modifiedFeatures.isEmpty()) {
					css.layer.getLayerManager().fireGeometryModified(
							modifiedFeatures,
							css.layer,
							modifiedFeaturesOldClones);
			}

		}else{
			//System.out.print("batch ");

			for (Iterator iter = this.iterator(); iter.hasNext();) {
				UndoableSetGeometry transformation = (UndoableSetGeometry) iter.next();
				transformation.execute();
			}

		}

	}

	public void unexecute() {
		//System.out.print("UT:unexecute() "+this+" ");
		if (css!=null && original_geoms.size()>0){
			//System.out.print("css ");		
			List features = css.layer.getFeatureCollectionWrapper().getFeatures();
			ArrayList modifiedFeatures = new ArrayList();
			ArrayList modifiedFeaturesOldClones = new ArrayList();
			
			for (Iterator iter = original_geoms.keySet().iterator(); iter.hasNext();) {
				Feature feature = (Feature) iter.next();
				Geometry new_geom = (Geometry)original_geoms.get(feature);

				modifiedFeatures.add(feature);
				modifiedFeaturesOldClones.add(feature.clone());
				feature.setGeometry(new_geom);
				
				//original_geoms.remove(feature);				
			}
			original_geoms.clear();
						
			Layer.tryToInvalidateEnvelope(css.layer);
			// fire the appropriate event, so everybody gets notified
			if (!modifiedFeatures.isEmpty()) {
				css.layer.getLayerManager().fireGeometryModified(
					modifiedFeatures,
					css.layer,
					modifiedFeaturesOldClones);
			}

			css.rollback();
			
		}else{
			//System.out.print("batch ");	
			for (Iterator iter = this.iterator(); iter.hasNext();) {
				UndoableSetGeometry transformation = (UndoableSetGeometry) iter.next();
				transformation.unexecute();
			}
		}
		//System.out.println();

	}
	
	
	
	public String getName() {
		String out = "";
		for (Iterator iter = transformations.iterator(); iter.hasNext();) {
			out += (out.length()>0?", ":"")+((UndoableSetGeometry)iter.next()).getName();
		}
		return (css==null?name_prefix:"")+name+out;
	}
	
	public String getName_prefix() {
		return name_prefix;
	}

	public void setName_prefix(String string) {
		name_prefix = string;
	}
	
	public void setGeom(Feature feature, Geometry geom){
		proposed_geoms.put(feature,geom);
	}
	
	public Geometry getGeom(Feature in_feature){
		List features = css.layer.getFeatureCollectionWrapper().getFeatures();
		Feature feature = (Feature)features.get(features.indexOf(in_feature));
		return (Geometry)feature.getGeometry().clone();
	}
/*	
	public UndoableCommand createCommand() {
			UndoableCommand command = new UndoableCommand(getName()) {
					public void execute() {
							execute();
					}

					public void unexecute() {
							unexecute();
					}
			};
			return command;
	}
*/	
	public boolean add(UndoableSetGeometry t){
		return transformations.add(t);
	}

	public int size() {
		return transformations.size();
	}

	public void clear() {
		transformations.clear();
	}

	public boolean isEmpty() {
		return transformations.isEmpty();
	}

	public Object[] toArray() {
		return transformations.toArray();
	}

	public boolean add(Object o) {
		return transformations.add(o);
	}

	public boolean contains(Object o) {
		return transformations.contains(o);
	}

	public boolean remove(Object o) {
		return transformations.remove(o);
	}

	public boolean addAll(Collection c) {
		return transformations.addAll(c);
	}

	public boolean containsAll(Collection c) {
		return transformations.containsAll(c);
	}

	public boolean removeAll(Collection c) {
		return transformations.removeAll(c);
	}

	public boolean retainAll(Collection c) {
		return transformations.retainAll(c);
	}


	public Iterator iterator() {
		return transformations.iterator();
	}

	public Object[] toArray(Object[] a) {
		return transformations.toArray(a);
	}

}
