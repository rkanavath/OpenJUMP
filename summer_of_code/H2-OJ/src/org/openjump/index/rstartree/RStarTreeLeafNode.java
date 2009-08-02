package org.openjump.index.rstartree;

import com.vividsolutions.jts.util.Assert;


public class RStarTreeLeafNode extends AbstractRStarTreeNode implements
		RStarTreeNode {

	public RStarTreeLeafNode(RStarTreeNode parent, int nodeCapacity, int level) {
		super(parent, nodeCapacity, level);
	}

	public void deleteItem(Object item) {
		for (RStarTreeNode child : getChildren()) {
			if (child instanceof RStarTreeItemNode) {
				if (((RStarTreeItemNode)child).getItem() == item)
					removeChild(child);
			}
			else
				Assert.shouldNeverReachHere("RStarLeafNode has a child other than an RStarTreeItemNode");
		}
		
	}

	public boolean containsItem(Object item) {
		// TODO Auto-generated method stub
		return false;
	}
}
