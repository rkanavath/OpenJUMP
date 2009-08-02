package org.openjump.index.rstartree;

import java.util.List;

import com.vividsolutions.jts.geom.Envelope;

public interface RStarTreeNode {

	/**
	 * Returns a representation of space that encloses this node, preferably
	 * not much bigger than this nodes's boundary yet fast to test for intersection
	 * with the envelopes of other nodes. 
	 * @return an Envelope 
	 */
	Envelope getEnvelope();

	boolean hasChildren();
	
	boolean hasOnlyLeafChildren(); 
	 
	List<RStarTreeNode> getChildren();

	boolean isFull();
	
	int getLevel();
	
	RStarTreeNode getParent();

	// update level for all newNode and all its children
	// update newNode's parent entry
	void addChild(RStarTreeNode newNode);

	void minimizeEnvelope();


	boolean removeChild(RStarTreeNode tmpNode);

	int numberOfChildren();

	void setParent(RStarTreeNode node);

	void setLevel(int i);

	int getCapacity();



}
