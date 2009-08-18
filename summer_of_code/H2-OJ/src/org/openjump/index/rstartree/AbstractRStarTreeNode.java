package org.openjump.index.rstartree;

import java.util.ArrayList;
import java.util.List;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.util.Assert;

public abstract class AbstractRStarTreeNode implements RStarTreeNode {
	protected ArrayList<RStarTreeNode> childNodes;
	protected Envelope envelope = null;
	protected int nodeCapacity;
	protected int level;
	protected RStarTreeNode parent;

	/**
	 * Constructs an AbstractNode with the given maximum number of child nodes
	 * 
	 * @param nodeCapacity is how many child nodes this node can have
	 */
	public AbstractRStarTreeNode(RStarTreeNode parent, int nodeCapacity, int level) {
		//Assert.isTrue(nodeCapacity >= 4, "Node capacity must be greater than or equal to 4");
		this.nodeCapacity = nodeCapacity;
		this.level = level;
		this.parent = parent;
		//if (nodeCapacity > 0)
			this.childNodes = new ArrayList<RStarTreeNode>(this.nodeCapacity);
		this.envelope = null;
	}

	/**
	 * Returns either child {@link AbstractNodes}, or if this is a leaf node, real data (wrapped
	 * in {@link ItemBoundables}).
	 */
	public List<RStarTreeNode> getChildren() {
		return childNodes;
	}

	/**
	 * Returns a representation of space that encloses this Node,
	 * preferably not much bigger than this Node's boundary yet fast to
	 * test for intersection with the bounds of other Nodes. 
	 *
	 * @return an Envelope 
	 */
	protected Envelope computeBounds() {
		Envelope returnEnvelope = new Envelope();
		for (RStarTreeNode child : this.childNodes) {
			returnEnvelope.expandToInclude(child.getEnvelope());
		}
		return returnEnvelope;
	}


	/**
	 * 
	 */
	public Envelope getEnvelope() {
		if (envelope == null) {
			envelope = computeBounds();
		}
		return envelope;
	}


	
	/**
	 * 
	 */
	public void setLevel(int treeLevel) {
		Assert.isTrue(level >= 0, "level must be a positive number");
		this.level = treeLevel;
		// Recursively alter the levels of this node's children
		if (childNodes != null)
		for (RStarTreeNode child : this.childNodes) {
			child.setLevel(this.level+1);
		}
	}

	
	/**
	 * 
	 */
	public void setParent(RStarTreeNode node) {
		this.parent = node;
	}

	/**
	 * 
	 */
	public boolean hasChildren() {
		return (!childNodes.isEmpty());
	}
	
	/**
	 * 
	 */
	public int getCapacity() {
		return nodeCapacity;
	}
 
	
	/**
	 * 
	 */
	public boolean isFull() {
		return (childNodes.size() == nodeCapacity);
	}
	
	/**
	 * 
	 */
	public int getLevel() {
		return level;
	}
	
	/**
	 * 
	 */
	public RStarTreeNode getParent() {
		return parent;
	}

	// update level for all newNode and all its children
	// update newNode's parent entry
	/**
	 * 
	 */
	public void addChild(RStarTreeNode child) {
		if (child != null) {
			child.setParent(this);
			child.setLevel(level + 1);
			childNodes.add(child);
			if (envelope == null)
				envelope = child.getEnvelope();
			else
				envelope.expandToInclude(child.getEnvelope());
		}
	}

	/**
	 * 
	 */
	public void minimizeEnvelope() {
		envelope = computeBounds();
	}


	/**
	 * 
	 */
	public boolean removeChild(RStarTreeNode tmpNode) {
		if (tmpNode == null) 
			return false;
		else {
			boolean ret = childNodes.remove(tmpNode);
			this.envelope = computeBounds();
			return ret;
		}
	}

	/**
	 * 
	 */
	public int numberOfChildren() {
		return childNodes.size();
	}

	
	/**
	 * 
	 */
	public boolean hasOnlyLeafChildren() {
		for (RStarTreeNode child : childNodes) {
			if (child instanceof RStarTreeLeafNode)
				continue;
			else
				return false;
		}
		return true;
	}
	
	/**
	 * 
	 */
	public String toString() {
		String returnString = "=====================================\n";
		returnString.concat("Level = "+level+"\nChildren:\n");
		for (RStarTreeNode child : this.childNodes) {
			returnString.concat(child.toString()+"\n");
		}
		return returnString;
	}
	
}
