package org.openjump.index.rstartree;

import java.util.ArrayList;
import java.util.List;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.util.Assert;

public abstract class AbstractRStarTreeNode implements RStarTreeNode {
	private ArrayList<RStarTreeNode> childNodes;
	private Envelope envelope = null;
	private int nodeCapacity;

	/**
	 * Constructs an AbstractNode with the given maximum number of child nodes
	 * 
	 * @param nodeCapacity is how many child nodes this node can have
	 */
	public AbstractRStarTreeNode(int nodeCapacity) {
		Assert.isTrue(nodeCapacity >= 4, "Node capacity must be greater than or equal to 4");
		this.nodeCapacity = nodeCapacity;
		this.childNodes = new ArrayList<RStarTreeNode>(this.nodeCapacity);
	}

	/**
	 * Returns either child {@link AbstractNodes}, or if this is a leaf node, real data (wrapped
	 * in {@link ItemBoundables}).
	 */
	public List<RStarTreeNode> getChildNodes() {
		return childNodes;
	}

	/**
	 * Returns a representation of space that encloses this Node,
	 * preferably not much bigger than this Node's boundary yet fast to
	 * test for intersection with the bounds of other Nodes. 
	 *
	 * @return an Envelope 
	 */
	protected abstract Envelope computeBounds();

	// TODO make sure this method works with dynamic updates
	public Envelope getEnvelope() {
		if (envelope == null) {
			envelope = computeBounds();
		}
		return envelope;
	}

	/**
	 * 
	 */
	// TODO this is going to need to be fixed for R* dynamic updates
	public void addChildNode(RStarTreeNode childNode) {
		//Assert.isTrue(bounds == null);
		childNodes.add(childNode);
	}
	
	/*
	public boolean hasOnlyLeafChildren() {
		for (RStarTreeNode b : childNodes) {
			if (b instanceof RStarTreeLeafNode)
				continue;
			else
				return false;
		}
		return true;
	}
	*/
	
}
