package org.openjump.index.rstartree;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.TreeMap;

import com.vividsolutions.jts.util.Assert;

public class SplitDistributions extends ArrayList<Distribution> {

	private double marginSum = -1.0;

	/**
	 * 
	 */
	private static final long serialVersionUID = -8563015200877453986L;

	/**
	 * 
	 * @param sortedMinX
	 * @param minObjectsPerNode
	 * @param maxObjectsPerNode
	 */
	public SplitDistributions(TreeMap<Double, RStarTreeNode> sortedNodes, int minChildren, int maxChildren) {
		super(maxChildren - (2 * minChildren) + 2);
		//Assert.isTrue(super.size() > 1, "maxChildren must be greater than 2*minChildren+2");
		
		ArrayList<RStarTreeNode> tmpFirstGroup = new ArrayList<RStarTreeNode>(maxChildren - minChildren);
		
		for (int i=0; i < minChildren; i++) 
			tmpFirstGroup.add(sortedNodes.pollFirstEntry().getValue());
		
		RStarTreeNode[] firstNodes = new RStarTreeNode[0];
		RStarTreeNode[] lastNodes = new RStarTreeNode[0];
		
		do {
			super.add(new Distribution(tmpFirstGroup.toArray(firstNodes), sortedNodes.values().toArray(lastNodes)));
			tmpFirstGroup.add(sortedNodes.pollFirstEntry().getValue());
		}
		while (sortedNodes.size() >= minChildren);
	}

	/**
	 * 
	 * @return
	 */
	public double getMarginSum() {
		if (this.marginSum < 0.0) {
			double tmpSum = 0.0;
			for (Distribution child : this) {
				//System.err.println(child.toString());
				tmpSum += child.getMargin();
			}
			this.marginSum = tmpSum;
		}
		return this.marginSum;
	}
	

	/**
	 * Choose the distribution with the minimum overlap value. Resolve ties by choosing the distribution with the
	 * minimum area value.
	 * 
	 * @return
	 */
	public Distribution minOverlapDistribution() {
		Distribution returnDistribution = null;
		Distribution tmpDistribution = null;
		for (Iterator<Distribution> i= super.iterator(); i.hasNext(); ) {
			//System.err.println("Current Distribution: "+i);
			if (returnDistribution == null) 
				returnDistribution = i.next();
			else {
				tmpDistribution = i.next();
				if ((tmpDistribution.getOverlap() < returnDistribution.getOverlap()) ||
				    (tmpDistribution.getOverlap() == returnDistribution.getOverlap() && 
				     tmpDistribution.getArea() < returnDistribution.getArea())) 
					returnDistribution = tmpDistribution;
			}
		}

		return returnDistribution;
	}

	
}
