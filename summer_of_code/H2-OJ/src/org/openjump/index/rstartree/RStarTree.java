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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.ItemVisitor;
import com.vividsolutions.jts.index.SpatialIndex;
import com.vividsolutions.jts.util.Assert;

public class RStarTree implements SpatialIndex {

	private static final int DEFAULT_MAX_OBJECTS_PER_NODE = 256;
	private static final int DEFAULT_MIN_OBJECTS_PER_NODE = 64;
	private static final int DEFAULT_FORCED_REINSERTION_NUMBER = 38;
	private static final int LARGE_NODE_SUBSET = 32; // performance variable for chooseMinimumOverlap
	private static final int ROOT_LEVEL = 0;
	
	private int maxObjectsPerNode, minObjectsPerNode, forcedReinsertionNumber;
	private int leafLevel;
	
	protected RStarTreeNode root;

	
	/**
	 * Creates a new tree with the given parameters.
	 * 
	 * @param maxObjectsPerNode is the node capacity of the tree
	 * @param minObjectsPerNode is how empty nodes are allowed to get before the tree is re-balanced
	 * @param forcedReinsertionNumber is how many children will be reinserted into the tree due to overflows during insert
	 */
	public RStarTree(int maxObjectsPerNode, int minObjectsPerNode, int forcedReinsertionNumber) {

	    Assert.isTrue(maxObjectsPerNode >= 4, "Node capacity must be greater than or equal to 4");
	    this.maxObjectsPerNode = maxObjectsPerNode;
	    
	    Assert.isTrue(minObjectsPerNode >= 2, "Minimum number of objects per node must be greater than or equal to 2");
	    Assert.isTrue(minObjectsPerNode <= this.maxObjectsPerNode/2, 
	    		"Minimum number of objects per node must be less than or equal to half of the node capacity");
	    this.minObjectsPerNode = minObjectsPerNode;
	    
	    Assert.isTrue(forcedReinsertionNumber >= 0 || forcedReinsertionNumber <= this.maxObjectsPerNode, 
	    		"Forced Reinsertion rate must be between 0 and "+this.maxObjectsPerNode);
	    this.forcedReinsertionNumber = forcedReinsertionNumber;
	    
	    this.root = new RStarTreeLeafNode(null, this.maxObjectsPerNode, ROOT_LEVEL);
	    this.leafLevel = ROOT_LEVEL;
	}
	/**
	 * Default Constructor
	 */
	public RStarTree() {
		this(DEFAULT_MAX_OBJECTS_PER_NODE, DEFAULT_MIN_OBJECTS_PER_NODE, DEFAULT_FORCED_REINSERTION_NUMBER);
	}
	

	/**
	 * Adds a spatial item with an extent specified by the given {@link Envelope} to the index
	 */
	public void insert(Envelope itemEnv, Object item) {
		boolean[] levelsVisited = new boolean[this.leafLevel+1];
		Arrays.fill(levelsVisited, false);
		insertNode(new RStarTreeItemNode(null, this.leafLevel, item, itemEnv), this.leafLevel, levelsVisited);
	}

	/**
	 * 
	 * @param node
	 */
	protected void insertNode (RStarTreeNode node, int levelMask, boolean[] levelsVisited) {
		
		RStarTreeNode subtree = chooseSubtree(this.root, node, levelMask);
		
		subtree.addChild(node);

		// if subtree is full, we must either split or reinsert
		if (subtree.isFull()) {
			RStarTreeNode insertionPathRoot = overflowEqualize(subtree, levelsVisited);
			
			// adjust all envelopes in the insertion path such that they are the minimum bounding boxes required to enclose all children		
			for (RStarTreeNode inode = subtree; inode != insertionPathRoot; inode = inode.getParent()) 
				inode.minimizeEnvelope();
			insertionPathRoot.minimizeEnvelope();
			if (insertionPathRoot.getParent() != null)
				insertionPathRoot.getParent().minimizeEnvelope();
		}	
	}
	
	/**
	 * 
	 * @param subtree
	 * @param levelsVisited
	 * @return
	 */
	protected RStarTreeNode overflowEqualize(RStarTreeNode subtree, boolean[] levelsVisited) {

		// if subtree isn't the root && this is the first call of overflowEqualize in the given level
		// then reinsert
		if (subtree != this.root && levelsVisited[subtree.getLevel()] == false) {
			levelsVisited[subtree.getLevel()] = true;
			reInsert(subtree, this.forcedReinsertionNumber, levelsVisited);
			return subtree;
		}
		// otherwise split
		else {
			RStarTreeNode newNodeFromSplit = split(subtree);
			
			// if overflowTreatment caused a split of the root, create a new root
			if (subtree.getParent() == null) {
				RStarTreeBranchNode newRoot = new RStarTreeBranchNode(null, this.maxObjectsPerNode, ROOT_LEVEL);
				newRoot.addChild(this.root);
				newRoot.addChild(subtree);
				this.root = newRoot;
				this.leafLevel++;
				return this.root;
			}
			
			// if split wasn't of the root ...
			else {
				// ... add new node to parent ...
				subtree.getParent().addChild(newNodeFromSplit);
				
				// ... if parent is full, propagate overflowEqualize up the tree
				if (subtree.getParent().isFull())  {
					return overflowEqualize(subtree.getParent(), levelsVisited);
				}
				else {
					return subtree.getParent();
				}
			}
		}
	}
	
	
	/**
	 * Split subtree. The items in subtree are distributed into two groups, one group remains within subtree,
	 * the other group is placed in a new node that is returned to the caller.
	 * 
	 * @param subtree
	 * @return
	 */
	protected RStarTreeNode split(RStarTreeNode subtree) {		
		RStarTreeNode newNode = null;
		if (subtree instanceof RStarTreeBranchNode)
			newNode = new RStarTreeBranchNode(subtree.getParent(), subtree.getCapacity(), subtree.getLevel());
		else if (subtree instanceof RStarTreeLeafNode)
			newNode = new RStarTreeLeafNode(subtree.getParent(), subtree.getCapacity(), subtree.getLevel());
		else
			Assert.shouldNeverReachHere("RStarTree::split -- can only split branch or leaf nodes");
		
		// determine the axis that is perpendicular to which the split is performed
		TreeMap<Double, RStarTreeNode> sortedMinX = new TreeMap<Double, RStarTreeNode>();
		TreeMap<Double, RStarTreeNode> sortedMaxX = new TreeMap<Double, RStarTreeNode>();
		TreeMap<Double, RStarTreeNode> sortedMinY = new TreeMap<Double, RStarTreeNode>();
		TreeMap<Double, RStarTreeNode> sortedMaxY = new TreeMap<Double, RStarTreeNode>();
		for (RStarTreeNode child : subtree.getChildren()) {
			sortedMinX.put(child.getEnvelope().getMinX(), child);
			sortedMaxX.put(child.getEnvelope().getMaxX(), child);
			sortedMinY.put(child.getEnvelope().getMinY(), child);
			sortedMaxY.put(child.getEnvelope().getMaxY(), child);
		}
		SplitDistributions minXdistributions = new SplitDistributions(sortedMinX, minObjectsPerNode, maxObjectsPerNode);
		SplitDistributions minYdistributions = new SplitDistributions(sortedMinY, minObjectsPerNode, maxObjectsPerNode);
		SplitDistributions maxXdistributions = new SplitDistributions(sortedMaxX, minObjectsPerNode, maxObjectsPerNode);
		SplitDistributions maxYdistributions = new SplitDistributions(sortedMaxY, minObjectsPerNode, maxObjectsPerNode);
		
		
		SplitDistributions bestXdistributions = (minXdistributions.getMarginSum() < maxXdistributions.getMarginSum()) ? 
				                                 minXdistributions : maxXdistributions;
		SplitDistributions bestYdistributions = (minYdistributions.getMarginSum() < maxYdistributions.getMarginSum()) ? 
                                                 minYdistributions : maxYdistributions;

		Distribution bestDistribution = (bestXdistributions.getMarginSum() < bestYdistributions.getMarginSum()) ? 
				                          bestXdistributions.minOverlapDistribution() : 
				                          bestYdistributions.minOverlapDistribution();
		
				                          

		// distribute the entries
		//Assert.isTrue(returnSplit != null);
		int i =0;
		for (RStarTreeNode node : bestDistribution.firstGroup()) {

			newNode.addChild(node);
			subtree.removeChild(node);
		}
		return newNode;
	}
	

	/**
	 * 
	 * @param subtree
	 * @param reinsertionNumber
	 */
	private void reInsert(RStarTreeNode subtree, int reinsertionNumber, boolean[] levelsVisited) {

		ArrayList<RStarTreeNode> removedNodes = new ArrayList<RStarTreeNode>(reinsertionNumber);
		// for all children in subtree, compute the distance between the center 
		// of the child's envelope and the center of subtree's envelope
		// sort in increasing order the centroid distances
		TreeMap<Double, RStarTreeNode> nodesSortedByCentroid = new TreeMap<Double, RStarTreeNode>();
		Coordinate subtreeCenter = subtree.getEnvelope().centre();
		List<RStarTreeNode> children = subtree.getChildren();
		for (RStarTreeNode child : children) {
			if (child != null && child.getEnvelope() != null)
				nodesSortedByCentroid.put(subtreeCenter.distance(child.getEnvelope().centre()), child);
		}
		
		// remove the first 'reinsertionNumber' of sorted children from subtree 
		RStarTreeNode tmpNode;
		for (int i = 0; i < reinsertionNumber; i++) {
			tmpNode = nodesSortedByCentroid.pollFirstEntry().getValue();
			removedNodes.add(tmpNode);
			subtree.removeChild(tmpNode);
		}
		
		// adjust subTree's envelope to accommodate the current set of children
		subtree.minimizeEnvelope();
		
		// reinsert all the removed children into the tree
		for (RStarTreeNode child : removedNodes) {
			insertNode(child, subtree.getLevel()+1, levelsVisited);
		}
	}
	
	
	/**
	 * Finds the most suitable subtree to insert an object with the given Envelope.
	 * Suitability based on which subtree requires the minimal amount of envelope growth to include
	 * the new item and results in a minimal amount of overlap with other subtrees.
	 * 
	 * @param itemEnv is the {@link Envelope} of the item that is being inserted into the tree
	 * @return the non-leaf node that would be the best fit for an item with the given Envelope
	 */
	protected RStarTreeNode chooseSubtree(RStarTreeNode currentRoot, RStarTreeNode newNode, int levelMask) {

		// if currentRoot is a leaf node, return it as the best subtree
		if (currentRoot instanceof RStarTreeLeafNode)
			return currentRoot;
		
		// if currentRoot is at the level described by levelMask, it is the best subtree
		if (currentRoot.getLevel() == levelMask)
			return currentRoot;
		
		// if the currentRoot only has leaves as children nodes, 
		// Determine the minimum overlap cost.
		// Choose the child whose envelope needs the least overlap enlargement to include the new data
		// envelope. Resolve ties by choosing the child that needs the least area enlargement of its
		// envelope.
		else if (currentRoot.hasOnlyLeafChildren()){
			return chooseSubtree(chooseMinimumOverlap(currentRoot.getChildren(), newNode), newNode, levelMask);
		}
		
		// the children of currentRoot aren't leaves...
		// Determine the minimum area cost.
		// Choose the child whose envelope needs the least area enlargement to include the new data
		// envelope. Resolve ties by choosing the child that has the smallest envelope area.
		else {
			return chooseSubtree(chooseMinimumArea(currentRoot.getChildren(), newNode), newNode, levelMask);
		}		
	}
	

	
	/**
	 * Determine the minimum overlap cost.
	 * Choose the child whose envelope needs the least overlap enlargement to include the new data
	 * envelope. Resolve ties by choosing the child that needs the least area enlargement of its
	 * envelope.
	 */
	protected RStarTreeNode chooseMinimumOverlap(List<RStarTreeNode> children, RStarTreeNode node) {

		if (children == null)
			return null;	
		RStarTreeNode currentMin = null;
		double currentMinOverlapCost = -1.0;
		double tempOverlapCost = 0.0;
		Envelope tempExpandedEnvelope = new Envelope();
		Envelope currentMinExpandedEnvelope = new Envelope();
		List<RStarTreeNode> childSubset;
		
		// performance hack for very large nodes
		// sort the envelopes in children in increasing order of their area enlargement needed to include the 
		// new envelope then select the LARGE_NODE_SUBSET number of smallest children
		if (children.size() > LARGE_NODE_SUBSET) {
			childSubset = new ArrayList<RStarTreeNode>(LARGE_NODE_SUBSET);
			TreeMap<Double, RStarTreeNode> childrenSortedByAreaEnlargement = new TreeMap<Double, RStarTreeNode>();
			// sort children by the amount of area enlargement
			for (RStarTreeNode child : children) {
				tempExpandedEnvelope.init(child.getEnvelope());
				tempExpandedEnvelope.expandToInclude(node.getEnvelope());
				childrenSortedByAreaEnlargement.put(new Double(tempExpandedEnvelope.getArea() - child.getEnvelope().getArea()), 
													child);
			}
			// add the first LARGE_NODE_SUBSET number of children to childSubset
			for (Map.Entry<Double, RStarTreeNode> child = childrenSortedByAreaEnlargement.pollFirstEntry(); 
				 child != null && childSubset.size() <= LARGE_NODE_SUBSET; 
				 child = childrenSortedByAreaEnlargement.pollFirstEntry()) {
				childSubset.add(child.getValue());
			}
		}
		else {
			childSubset = children;
		}

		// find the child that has the least overlap cost once expanded to include the new envelope
		for (RStarTreeNode child : childSubset) {
			// expand child's envelope to encompass itemEnv
			tempExpandedEnvelope.init(child.getEnvelope());
			tempExpandedEnvelope.expandToInclude(node.getEnvelope());			
			
			// sum up the intersection between this child's enlarged envelope and all the other children's envelopes
			for (RStarTreeNode otherChild : children) {
				if (otherChild != child)
					tempOverlapCost += child.getEnvelope().intersection(otherChild.getEnvelope()).getArea();
			}
			
			// if this is the first time through, or the current child has less overlap cost than the currentMin
			if (currentMin == null || tempOverlapCost < currentMinOverlapCost) {
				currentMin = child;
				currentMinOverlapCost = tempOverlapCost;
				currentMinExpandedEnvelope = tempExpandedEnvelope;
				continue;
			}
			// tie breaker
			else if (tempOverlapCost == currentMinOverlapCost) {
				if (tempExpandedEnvelope.getArea() - child.getEnvelope().getArea() < 
						currentMinExpandedEnvelope.getArea() - currentMin.getEnvelope().getArea()) {
					currentMin = child;
					currentMinOverlapCost = tempOverlapCost;
					currentMinExpandedEnvelope = tempExpandedEnvelope;
					continue;
				}
			}
			// current min is less than the current child
		}

		return currentMin;
	}
	
	


	/**
	 * the children of currentRoot aren't leaves...
	 * Determine the minimum area cost.
	 * Choose the child whose envelope needs the least area enlargement to include the new data
	 * envelope. Resolve ties by choosing the child that has the smallest envelope area.
	 */
	protected RStarTreeNode chooseMinimumArea(List<RStarTreeNode> children, RStarTreeNode node) {

		if (children == null)
			return null;	
		RStarTreeNode currentMin = null;
		Envelope currentMinExpandedEnv = new Envelope();
		Envelope tempExpandedEnv = new Envelope();
		double currentMinAreaIncrease = -1.0, tempAreaIncrease = -1.0;
		
		for (RStarTreeNode child : children) {
			// first time through the for loop
			if (currentMin == null) {
				currentMin = child;
				currentMinExpandedEnv.init(child.getEnvelope());
				currentMinExpandedEnv.expandToInclude(node.getEnvelope());
				currentMinAreaIncrease = currentMinExpandedEnv.getArea() - child.getEnvelope().getArea();
				continue;
			}
			// compare the current child with the previously found minimum
			else {
				tempExpandedEnv.init(child.getEnvelope());
				tempExpandedEnv.expandToInclude(node.getEnvelope());
				tempAreaIncrease = tempExpandedEnv.getArea() - child.getEnvelope().getArea();
				// current child of the for loop is smaller than our previous minimum
				if (tempAreaIncrease < currentMinAreaIncrease) {
					currentMin = child;
					currentMinExpandedEnv.init(tempExpandedEnv);
					currentMinAreaIncrease = tempAreaIncrease;
					continue;
				}
				// break ties by looking at total area increase
				else if (tempAreaIncrease == currentMinAreaIncrease) {
					if (child.getEnvelope().getArea() < currentMin.getEnvelope().getArea()) {
						currentMin = child;
						currentMinExpandedEnv.init(tempExpandedEnv);
						currentMinAreaIncrease = tempAreaIncrease;
						continue;
					} 
					// currentMin has a smaller area than the challenger
				}
				// currentMin's area increase is smaller than the challenger 
			}
		} // end for loop
		
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
	public List<Object> query(Envelope searchEnv) {
		
		// if the query envelope doesn't intersect with the root envelope, there are no matches
		if (!searchEnv.intersects(root.getEnvelope()))
			return null;
		
		ArrayList<Object> matches = new ArrayList<Object>();
		
		query(searchEnv, root, matches);
		return matches;
	}
	
	protected void query (Envelope searchEnv, RStarTreeNode node, List matches) {
		for (RStarTreeNode child : node.getChildren()) {
			// if current child doesn't intersect the search envelope, continue to the next child
			if (!searchEnv.intersects(child.getEnvelope()))
				continue;
			
			// if the current child contains just an item, add that item to matches and continue
			if (child instanceof RStarTreeItemNode) {
				matches.add(((RStarTreeItemNode)child).getItem());
				continue;
			}
			
			// current child is a branch node or leaf node, thus recursion down to the item is needed
			else {
				query (searchEnv, child, matches);
			}
		}
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
		
		if (searchEnv.intersects(root.getEnvelope()))
			query(searchEnv, root, visitor);
	}
	
	protected void query (Envelope searchEnv, RStarTreeNode node, ItemVisitor visitor) {
		for (RStarTreeNode child : node.getChildren()) {
			// if current child doesn't intersect the search envelope, continue to the next child
			if (!searchEnv.intersects(child.getEnvelope()))
				continue;
			
			// if the current child contains just an item, visit that item 
			if (child instanceof RStarTreeItemNode) {
		        visitor.visitItem(((RStarTreeItemNode)child).getItem());
				continue;
			}
			
			// current child is a branch node or leaf node, thus recursion down to the item is needed
			else {
				query (searchEnv, child, visitor);
			}
		}
	}
	

	
	/**
	 * Removes a single item from the tree.
	 *
	 * @param itemEnv the Envelope of the item to remove
	 * @param item the item to remove
	 * @return <code>true</code> if the item was found
	 */
	public boolean remove(Envelope itemEnv, Object item) {
		if (itemEnv.intersects(root.getEnvelope()))
			return false;
		
		// get the location of the item within the tree
		RStarTreeLeafNode leafNode = findLeafNode(root, itemEnv, item);
		
		// item not found within the tree
		if (leafNode == null)
			return false;
		
		// item found, delete item..
		else {
			leafNode.deleteItem(item);
			boolean[] levelsVisited = new boolean[this.leafLevel+1];
			Arrays.fill(levelsVisited, false);
			condenseTree(leafNode, levelsVisited);
			return true;
		}
	}
	
	
	/**
	 * 
	 * @param currentNode
	 * @param itemEnv
	 * @param item
	 * @return
	 */
	protected RStarTreeLeafNode findLeafNode(RStarTreeNode currentNode, Envelope itemEnv, Object item) {

		if (currentNode instanceof RStarTreeLeafNode) {
			if (((RStarTreeLeafNode)currentNode).containsItem(item))
				return (RStarTreeLeafNode)currentNode;
			else
				return null;
		}
		
		for (RStarTreeNode child : currentNode.getChildren()) {
			if (child.getEnvelope().intersects(itemEnv)) {
				RStarTreeLeafNode tmpNode = findLeafNode(child, itemEnv, item);
				if (tmpNode != null) {
					return tmpNode;
				}
			}
		}

		return null;
	}
	
	/**
	 * 
	 * @param node
	 */
	protected void condenseTree(RStarTreeNode node, boolean[] levelsVisited) {

		// if node is root, minimize envelope and stop recursion
		if (node.equals(root)) {
			node.minimizeEnvelope();
		}
		// node has above the minimum number of children, minimize envelope then
		// recurse up the tree with the assumption that all the parents will hit 
		// this condition or the above
		else if (node.numberOfChildren() >= this.minObjectsPerNode) {
			node.minimizeEnvelope();
			condenseTree(node.getParent(), levelsVisited);
		}
		// node has less than minimum number of children
		// delete node and re-insert its children into the 
		// tree and recurse up the tree with condenseTree.
		// that recursion could result in another deletion-reinsertion or 
		else {
			List<RStarTreeNode> children = node.getChildren();
			RStarTreeNode parent = node.getParent();
			parent.removeChild(node);
			for (RStarTreeNode child : children) {
				this.insertNode(child, child.getLevel(), levelsVisited);
			}
			condenseTree(parent, levelsVisited);
		}		
	}


}
