package org.openjump.index.rstartree;

import java.util.List;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.util.Assert;

public class RStarTreeItemNode extends AbstractRStarTreeNode implements
		RStarTreeNode {
	
	private Object item;
	//private Envelope itemEnvelope;

	public RStarTreeItemNode(RStarTreeNode parent, int level, Object item, Envelope itemEnv) {
		super(parent, 0, level);
		this.item = item;
		this.envelope = itemEnv;
	}
	
	public Object getItem() {
		return item;
	}
	
	//public Envelope getEnvelope() { return this.itemEnvelope; }

	public boolean hasChildren() { return false; }
	
	public boolean hasOnlyLeafChildren() { return false; }
	 
	public List<RStarTreeNode> getChildren() { return null; }

	public boolean isFull() { return true; }
		

	public void addChild(RStarTreeNode newNode) { Assert.shouldNeverReachHere(); }

	public void minimizeEnvelope() { return; }


	public boolean removeChild(RStarTreeNode tmpNode) {
		Assert.shouldNeverReachHere();
		return false;
	}

	public int numberOfChildren() { return 0; }
	
	public String toString() {
		return "(Item Node)\n+Level = "+this.level+ "\nItem = "+item+"\nEnvelope = "+envelope;
	}

}
