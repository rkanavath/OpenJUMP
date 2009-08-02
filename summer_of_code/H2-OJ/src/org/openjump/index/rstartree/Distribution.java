package org.openjump.index.rstartree;


import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.util.Assert;

public class Distribution {
	
	private RStarTreeNode[] group1, group2;
	private double margin = -1, overlap = -1, area = -1;
	private Envelope group1Env, group2Env;

	public Distribution(RStarTreeNode[] array, RStarTreeNode[] array2) {
		Assert.isTrue(array != null, "Distribution::array 1 is null");
		Assert.isTrue(array2 != null, "Distribution::array 2 is null");
		group1 = array;
		group2 = array2;
		group1Env = new Envelope();
		group2Env = new Envelope();
		for (int i=0; i<group1.length; i++) 
			group1Env.expandToInclude(group1[i].getEnvelope());
		for (int i=0; i<group2.length; i++) 
			group2Env.expandToInclude(group2[i].getEnvelope());
	}

	public double getMargin() {
		if (margin < 0)
			margin = calculateMargin();
		return margin;
	}

	private double calculateMargin() {
		double returnMargin = 0.0;
		returnMargin += group1Env.getHeight() * 2;
		returnMargin += group1Env.getWidth() * 2;
		returnMargin += group2Env.getHeight() * 2;
		returnMargin += group2Env.getWidth() * 2;
		return returnMargin;
	}

	public double getOverlap() {
		if (overlap < 0)
			overlap = calculateOverlap();
		return overlap;
	}

	private double calculateOverlap() {
		Envelope tmpEnv = new Envelope(group1Env);
		tmpEnv.intersection(group2Env);
		return tmpEnv.getArea();
	}

	public double getArea() {
		if (area < 0)
			area = calculateArea();
		return area;
	}

	private double calculateArea() {
		return group1Env.getArea() + group2Env.getArea();
	}

	public RStarTreeNode[] firstGroup() {
		return group1;
	}

	public RStarTreeNode[] secondGroup() {
		return group2;
	}
	
	public String toString() {
		return "Group1 Envelope = "+group1Env.toString()+"\tGroup2 Envelope = "+group2Env.toString();
	}

}
