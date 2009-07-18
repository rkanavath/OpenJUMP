package org.openjump.index.rstartree;

import java.util.List;

import com.vividsolutions.jts.geom.Envelope;

public class RStarTreeItemNode extends AbstractRStarTreeNode implements
		RStarTreeNode {

	public RStarTreeItemNode(Object item, Envelope itemEnv) {
		// TODO Auto-generated constructor stub
	}

	@Override
	protected Envelope computeBounds() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void addChild(RStarTreeNode newNode) {
		// TODO Auto-generated method stub

	}

	@Override
	public List<RStarTreeNode> getChildren() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getLevel() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public RStarTreeNode getParent() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean hasChildren() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean hasOnlyLeafChildren() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isFull() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void minimizeEnvelope() {
		// TODO Auto-generated method stub

	}

}
