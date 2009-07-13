/*
 * Implementation of the R*Tree spatial data structure.
 * This spatial index was initially designed for JTS, the Java Topology Suite.
 * 
 * 
 * References used in the design of this data structure:
 * 
 * Norbert Beckmann, Hans-Peter Kriegel, Ralf Schneider, Bernhard Seeger: 
 * The R*-Tree: An Efficient and Robust Access Method for Points and Rectangles. 
 * SIGMOD Conference 1990: 322-331
 * 
 * Antonin Guttman: 
 * R-Trees: A Dynamic Index Structure for Spatial Searching, 
 * Proc. 1984 ACM SIGMOD International Conference on Management of Data, pp. 47-57. 
 * ISBN 0-89791-128-8
 * 
 * Hanan Samet:
 * Foundations of Multidimensional and Metric Data Structures, pp. 270-294.
 * Morgan Kaufmann, 2006. ISBN 0-12-396-446-9
 */

package org.openjump.index.rstartree;

import java.util.List;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.ItemVisitor;
import com.vividsolutions.jts.index.SpatialIndex;
import com.vividsolutions.jts.util.Assert;

public class RStarTree implements SpatialIndex {

	private static final int DEFAULT_MAX_OBJECTS_PER_NODE = 10;
	private static final int DEFAULT_MIN_OBJECTS_PER_NODE = 5;
	private static final int DEFAULT_FORCED_REINSERTION_RATE = 30;
	
	private int maxObjectsPerNode, minObjectsPerNode, forcedReinsertionRate;
	
	protected RStarTreeNode root;

	
	/**
	 * Creates a new tree with the given parameters.
	 * 
	 * @param maxObjectsPerNode is the node capacity of the tree
	 * @param minObjectsPerNode is how empty nodes are allowed to get before the tree is rebalanced
	 */
	public RStarTree(int maxObjectsPerNode, int minObjectsPerNode, int forcedReinsertionRate) {
	    Assert.isTrue(maxObjectsPerNode >= 4, "Node capacity must be greater than or equal to 4");
	    this.maxObjectsPerNode = maxObjectsPerNode;
	    
	    Assert.isTrue(minObjectsPerNode >= 2, "Minimum number of objects per node must be greater than or equal to 2");
	    Assert.isTrue(minObjectsPerNode <= this.maxObjectsPerNode/2, 
	    		"Minimum number of objects per node must be less than or equal to half of the node capacity");
	    this.maxObjectsPerNode = maxObjectsPerNode;
	    
	    Assert.isTrue(forcedReinsertionRate >= 0 || forcedReinsertionRate <= 100, 
	    		"Forced Reinsertion rate must be between 0 and 100");
	    this.forcedReinsertionRate = forcedReinsertionRate;
	}
	/**
	 * Default Constructor
	 */
	public RStarTree() {
		this(DEFAULT_MAX_OBJECTS_PER_NODE, DEFAULT_MIN_OBJECTS_PER_NODE, DEFAULT_FORCED_REINSERTION_RATE);
	}
	

	/**
	 * Adds a spatial item with an extent specified by the given {@link Envelope} to the index
	 */
	public void insert(Envelope itemEnv, Object item) {
		RStarTreeNode subtree = ChooseSubtree(this.root, itemEnv);
	}

	/**
	 * Finds the most suitable subtree to insert an object with the given Envelope.
	 * Suitability based on which subtree requires the minimal amount of envelope growth to include
	 * the new item and results in a minimal amount of overlap with other subtrees.
	 * 
	 * @param itemEnv is the {@link Envelope} of the item that is being inserted into the tree
	 * @return the non-leaf node that would be the best fit for an item with the given Envelope
	 */
	protected RStarTreeNode ChooseSubtree(RStarTreeNode currentRoot, Envelope itemEnv) {
		
		// if currentRoot is a leaf node, return it as the best subtree
		if (!currentRoot.hasChildren())
			return currentRoot;
		
		// if the currentRoot only has leaves as children nodes, 
		// Determine the minimum overlap cost.
		// Choose the child whose envelope needs the least overlap enlargement to include the new data
		// envelope. Resolve ties by choosing the child that needs the least area enlargement of its
		// envelope.
		else if (currentRoot.hasOnlyLeafChildren()){
			return chooseMinimumOverlap(currentRoot.getChildren(), itemEnv);
		}
		
		// the children of currentRoot aren't leaves...
		// Determine the minimum area cost.
		// Choose the child whose envelope needs the least area enlargement to include the new data
		// envelope. Resolve ties by choosing the child that has the smallest envelope area.
		else {
			return chooseMinimumArea(currentRoot.getChildren(), itemEnv);
		}
		
		
	}
	
	protected RStarTreeNode chooseMinimumOverlap(List<RStarTreeNode> children, Envelope itemEnv) {
		// TODO Auto-generated method stub
		return null;
	}
	
	// the children of currentRoot aren't leaves...
	// Determine the minimum area cost.
	// Choose the child whose envelope needs the least area enlargement to include the new data
	// envelope. Resolve ties by choosing the child that has the smallest envelope area.
	protected RStarTreeNode chooseMinimumArea(List<RStarTreeNode> children, Envelope itemEnv) {
		if (children == null)
			return null;	
		RStarTreeNode currentMin = null;
		Envelope currentMinExpandedEnv = new Envelope();
		Envelope tempExpandedEnv = new Envelope();
		
		for (RStarTreeNode child : children) {
			if (currentMin == null) {
				currentMin = child;
				currentMinExpandedEnv.init(child.getEnvelope());
				currentMinExpandedEnv.expandToInclude(itemEnv);
			}
			else {
				tempExpandedEnv.init(child.getEnvelope());
				tempExpandedEnv.expandToInclude(itemEnv);
				if (tempExpandedEnv.getArea() < currentMinExpandedEnv.getArea()) {
					currentMin = child;
					currentMinExpandedEnv.init(tempExpandedEnv);
				}
				else if (tempExpandedEnv.getArea() == currentMinExpandedEnv.getArea()) {

				}
			}
		}
		return currentMin;
	}
	

	
	/**
	 * Queries the index for all items whose extents intersect the given search {@link Envelope}
	 * Note that some kinds of indexes may also return objects which do not in fact
	 * intersect the query envelope.
	 *
	 * @param searchEnv the envelope to query for
	 * @return a list of the items found by the query
	 */
	public List query(Envelope searchEnv) {
		return null;
	}

	/**
	 * Queries the index for all items whose extents intersect the given search {@link Envelope},
	 * and applies an {@link ItemVisitor} to them.
	 * Note that some kinds of indexes may also return objects which do not in fact
	 * intersect the query envelope.
	 *
	 * @param searchEnv the envelope to query for
	 * @param visitor a visitor object to apply to the items found
	 */
	public void query(Envelope searchEnv, ItemVisitor visitor) {

	}

	/**
	 * Removes a single item from the tree.
	 *
	 * @param itemEnv the Envelope of the item to remove
	 * @param item the item to remove
	 * @return <code>true</code> if the item was found
	 */
	public boolean remove(Envelope itemEnv, Object item) {
		return false;
	}


}
