package org.openjump.index.rstartree.test;

import java.util.List;
import java.util.Random;

import org.openjump.index.rstartree.RStarTree;
import org.openjump.index.rstartree.RStarTreeItemNode;
import org.openjump.index.rstartree.RStarTreeNode;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jump.feature.Feature;

public class RStarTreeTest {

	private static final int maxObjects = 100;
	private static final int maxObjectsPerNode = 16;
	private static final int minObjectsPerNode = 8;
	private static final int forcedReinsertionNum = 4;
	private static final int maxX = 10000;
	private static final int maxY = 10000;


	/**
	 * @param args
	 */
	public static void main(String[] args) {
		RStarTree rstarindex = new RStarTree(maxObjectsPerNode, minObjectsPerNode, forcedReinsertionNum);
		STRtree strindex = new STRtree(maxObjectsPerNode);

		// timers
		long rstartimer;
		long strtimer;

		Envelope[] randomEnvs = new Envelope[maxObjects];
		Random rand = new Random();
		for (int i = 0; i < randomEnvs.length; i++) {
			randomEnvs[i] = new Envelope(rand.nextInt(maxX), rand.nextInt(maxX), rand.nextInt(maxY), rand.nextInt(maxY));
		}

		// load and build strindex
		strtimer = System.currentTimeMillis();
		for (int i=0; i<randomEnvs.length; i++) {
			strindex.insert(randomEnvs[i], "#"+i);
		}
		strindex.build();
		strtimer = System.currentTimeMillis() - strtimer;

		// load rstarindex
		rstartimer = System.currentTimeMillis();
		for (int i=0; i<randomEnvs.length; i++) {
			rstarindex.insert(randomEnvs[i], "Node #"+i);
		}
		rstartimer = System.currentTimeMillis() - rstartimer;

		// report load times
		System.out.println("STRtree load time = "+strtimer+"\tR*tree load time = "+rstartimer);

		// test the query speed 
		// STRtree
		System.out.println("Testing query speed of STRtree");
		strtimer = System.currentTimeMillis();
		for (Envelope env : randomEnvs) {
			strindex.query(env);
		}
		strtimer = System.currentTimeMillis() - strtimer;

		// R*Tree
		System.out.println("Testing query speed of R*Tree");
		rstartimer = System.currentTimeMillis();
		for (Envelope env : randomEnvs) {
			rstarindex.query(env);
		}
		rstartimer = System.currentTimeMillis() - rstartimer;

		// report query times
		System.out.println("R*Tree query time: "+rstartimer+"\tSTRtree query time:"+strtimer);

		//test query accuracy of R*Tree
		System.out.println("Testing query accuracy of R*Tree");
		List strmatches, rstarmatches;
		for (Envelope env : randomEnvs) {
			strmatches = strindex.query(env);
			rstarmatches = rstarindex.query(env);
			if (rstarmatches == null)
				System.out.println("R*Tree is null");
			else if (strmatches == null)
				System.out.println("STRtree is null");
			else {
				for (Object match : rstarmatches) {
					if (match instanceof RStarTreeItemNode) {
						RStarTreeItemNode rMatch = (RStarTreeItemNode)match;
						if (!strmatches.contains(rMatch.getItem())) {
							System.out.println("R*Tree returns an object not in STRtree for the same query");
						}
					}
				}

				for (Object match : strmatches) {
					if (match instanceof RStarTreeItemNode) {
						RStarTreeItemNode strMatch = (RStarTreeItemNode)match;
						if (!strmatches.contains(strMatch.getItem())) {
							System.out.println("STRtree returns an object not in R*Tree for the same query");
						}
					}
				}

			}

		}

	}
}
