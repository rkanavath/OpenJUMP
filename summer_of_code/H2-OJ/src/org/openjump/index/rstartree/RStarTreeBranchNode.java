package org.openjump.index.rstartree;

import com.vividsolutions.jts.geom.Envelope;

public class RStarTreeBranchNode extends AbstractRStarTreeNode implements
		RStarTreeNode {

	public RStarTreeBranchNode(int nodeCapacity) {
		super(nodeCapacity);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected Envelope computeBounds() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Envelope getEnvelope() {
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

	public int propagateLevels(int root_level) {
		// TODO Auto-generated method stub
		
	}

}
