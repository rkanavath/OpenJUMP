/***********************************************
 * created on 		25.08.2005
 * last modified: 	07.09.2005 consider two lines forming a ring
 * 
 * author:			sstein
 * 
 * description:
 * 	displaces lines from lines (and ?) to ensure cartographic legibility, 
 *  that means ensures visual separability of lines. <p>
 *  The algorithm:<p>
 *  - splits long lines with a huge number of vertices to avoid big matrices.<p>
 *  - allows only a maximum number of lines to proceed to avoid a to big network matrix.<p>
 *  - might be useful for generalization of contour lines in 
 *  combination with simplification and smoothing algorithms. <p>
 *  Ideally, to avoid line crossings: <p>
 *   1. simplify and displace at the same time <p>
 * 	 2. smooth and displace at the same time <p> 
 *  The displacement algorithm is based on the "Snakes technique" <p>
 * 	Literature: <p>
 *  See Steiniger and Meier (2004): Snakes a technique for line displacement and smoothing.
 *  See also Burgardt and Meier (1997), Burghardt (2001) or Bader (2001) (e.g. use http://schoolar.google.com)
 * ================= 
 * TODO: implement external displacement forces resulting from polygons 
 * 		 => see #evaluateLineWithDataset()
 * TODO: polygon features or lines can be of different feature type = different Schema!
 * 		=> check that, or work only with geometries for nonLineFeatures!!! 
 *****************************************************/
package mapgen.algorithms.snakes;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import mapgen.agents.goals.BuildingGoals;
import mapgen.geomutilities.InterpolateLinePoints;
import mapgen.geomutilities.SecondGeodeticTask2d;
import mapgen.geomutilities.SplitLineString;
import mapgen.measures.LineLineDistance;
import mapgen.measures.PolygonLineDistance;

import org.jmat.MatlabSyntax;
import org.jmat.data.Matrix;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.index.quadtree.Quadtree;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureDatasetFactory;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.task.TaskMonitor;

/**
 * @description:
 * 	displaces lines from lines (and ?) to ensure cartographic legibility, 
 *  that means ensures visual separability of lines. <p>
 * 
 * @author sstein
 *
 */
public class LineDisplacementSnakes {

    private final String DISPID = "displaceID";
    //-- for tests
    private static String T6 = "min sepparation distance in m ?";
    double signaturRadiusTestInMeter = 0;
    //-------
    private final String newAttributString = "DisplaceLines";
    private final String newSplitLineAttributeString = "SplitIdLine";
   	private final String newSplitSegmentAttributeString = "SplitIdLineSegm";
    double signatureRadiusA = 0;
    int scale = 1;
    int iterMax = 20;
    boolean solveIter = false;
    int maxPoints = 100; //defines the maximal number of vertices for a line to process the line
    					 //other wise matrices become to big (memory and speed problem can occure)
    int minPoints = 6; //necessary to calculate snakes matrix
    double crucialDistance = 0.2; //this values is needed to evaluate Networkstate
    							  //if distance of points is below that, the points and subsequently the lines
    							  //are connected
    double maxNodeMoveDistance = 10.0; //this should be reset according to visual perceptible maxDisplacement 
    double maxObjects = 1000; //to avoid to big matrix calculated for network state evaluation
    
    FeatureSchema emptyFS = new FeatureSchema();
    FeatureCollection initialPointEnergies = null;
    FeatureCollection minDistAndSignatureBuffers = null;
    FeatureCollection networkNodesAndSplitPoints = new FeatureDataset(emptyFS);
    FeatureCollection displacedLines = null;
    //--------
    //for actual status output
    public boolean monitorExists = false;
    private TaskMonitor jumpMonitor = null;
    
    //--------------------------
    // constructors
    //-------------------------
    /**
     * use that constructor if a selection of features of different geometry types is delivered 
     */
    public LineDisplacementSnakes(Collection features, int iterations, double signatureDiameterInM){
        
        this.maxNodeMoveDistance = 10.0;
        double minDist = 0;        		
        double signatureRadius = 0.5*signatureDiameterInM;
        
    	FeatureSchema fs = null;
    	FeatureDataset fd = null;
    	ArrayList noLineFeatures = new ArrayList(); 
    	boolean lineStringsFound = false;
    	boolean found=false; int i=0; int count = 0;
    	for (Iterator iterator = features.iterator(); iterator.hasNext();) {
    	    i++;
            Feature element = (Feature) iterator.next();
			if (element.getGeometry() instanceof LineString){
			    count++;			
				lineStringsFound = true;
				if (count==1){
	      			//-- not sure to do that, since feature schemas of selected objects might be different 
	      			fs = copyFeatureSchema(element.getSchema());
	      			fd = new FeatureDataset(fs);
				}
				fd.add(element);
			}
			else{
			    noLineFeatures.add(element);
			}			      			
		}	    	
    	//-- add all no LineFeatures
    	for (Iterator iter = noLineFeatures.iterator(); iter.hasNext();) {
            Feature element = (Feature) iter.next();
            Feature f = new BasicFeature(fs);
            f.setGeometry(element.getGeometry());
            fd.add(f);
        }
    	if(lineStringsFound == true){
          	//--start
    	    this.displaceLines(fd,iterations,signatureRadius,minDist);
    	}
    	else{
    		//context.getWorkbenchFrame().warnUser("no LineStrings found!!!");
    	    System.out.println("LineDisplacementSnakes.constructor: no LineStrings found!!!");
    	}

    }

    /**
     * 
     * @param features Collection of different geometry type features
     * @param iterations
     * @param signatureDiameterMM
     * @param scale
     */
    public LineDisplacementSnakes(Collection features, int iterations, double signatureDiameterMM, int scale){
        
       	BuildingGoals goals = new BuildingGoals(scale);
      	double minDist = goals.getMinObjectSeparationReal();      	
      	double signatureRadius = 0.5*signatureDiameterMM*goals.getFactor();
      	
      	this.maxNodeMoveDistance = goals.getMinObjectSeparationReal();      	
      	     
      	System.out.println("minDist[m]: " + minDist + " -- signatureRad [m]: " + signatureRadius);
      	//--start
        
    	FeatureSchema fs = null;
    	FeatureDataset fd = null;
    	ArrayList noLineFeatures = new ArrayList(); 
    	boolean lineStringsFound = false;
    	boolean found=false; int i=0; int count = 0;
    	for (Iterator iterator = features.iterator(); iterator.hasNext();) {
    	    i++;
            Feature element = (Feature) iterator.next();
			if (element.getGeometry() instanceof LineString){
			    count++;			
				lineStringsFound = true;
				if (count==1){
	      			//-- not sure to do that, since feature schemas of selected objects might be different 
	      			fs = copyFeatureSchema(element.getSchema());
	      			fd = new FeatureDataset(fs);
				}
				fd.add(element);
			}
			else{
			    noLineFeatures.add(element);
			}			      			
		}	    	
    	//-- add all no LineFeatures
    	for (Iterator iter = noLineFeatures.iterator(); iter.hasNext();) {
            Feature element = (Feature) iter.next();
            Feature f = new BasicFeature(fs);
            f.setGeometry(element.getGeometry());
            fd.add(f);
        }
    	if(lineStringsFound == true){
          	//--start
    	    this.displaceLines(fd,iterations,signatureRadius,minDist);
    	}
    	else{
    		//context.getWorkbenchFrame().warnUser("no LineStrings found!!!");
    	    System.out.println("LineDisplacementSnakes.constructor: no LineStrings found!!!");
    	}

    }

    /**
     * 
     * @param features Collection of different geometry type features
     * @param iterations
     * @param signatureDiameterMM
     * @param scale
     * @param context Jump Workbenchcontext to send status messages 
     */
    public LineDisplacementSnakes(Collection features, int iterations, double signatureDiameterMM, 
    			int scale, TaskMonitor monitor){
        
    	this.monitorExists = true;
    	this.jumpMonitor = monitor;
    	
       	BuildingGoals goals = new BuildingGoals(scale);
      	double minDist = goals.getMinObjectSeparationReal();      	
      	double signatureRadius = 0.5*signatureDiameterMM*goals.getFactor();
      	
      	this.maxNodeMoveDistance = goals.getMinObjectSeparationReal();      	
      	     
      	System.out.println("minDist[m]: " + minDist + " -- signatureRad [m]: " + signatureRadius);
      	//--start
        
    	FeatureSchema fs = null;
    	FeatureDataset fd = null;
    	ArrayList noLineFeatures = new ArrayList(); 
    	boolean lineStringsFound = false;
    	boolean found=false; int i=0; int count = 0;
    	for (Iterator iterator = features.iterator(); iterator.hasNext();) {
    	    i++;
            Feature element = (Feature) iterator.next();
			if (element.getGeometry() instanceof LineString){
			    count++;			
				lineStringsFound = true;
				if (count==1){
	      			//-- not sure to do that, since feature schemas of selected objects might be different 
	      			fs = copyFeatureSchema(element.getSchema());
	      			fd = new FeatureDataset(fs);
				}
				fd.add(element);
			}
			else{
			    noLineFeatures.add(element);
			}			      			
		}	    	
    	//-- add all no LineFeatures
    	for (Iterator iter = noLineFeatures.iterator(); iter.hasNext();) {
            Feature element = (Feature) iter.next();
            Feature f = new BasicFeature(fs);
            f.setGeometry(element.getGeometry());
            fd.add(f);
        }
    	if(lineStringsFound == true){
          	//--start
    	    this.displaceLines(fd,iterations,signatureRadius,minDist);
    	}
    	else{
    		//context.getWorkbenchFrame().warnUser("no LineStrings found!!!");
    	    System.out.println("LineDisplacementSnakes.constructor: no LineStrings found!!!");
    	}

    }

    /**
     * 
     * @param lineFeatures set of LineString : all lines should have the same FeatureSchema  
     * @param iterations
     * @param scale
     * @param signatureDiameternMM
     */
    public LineDisplacementSnakes(FeatureCollection lineFeatures, int iterations, int scale, double signatureDiameternMM){
      	/**
      	//--- for tests ---
      	double minDist = 0;
      	double signatureRadius = this.signaturRadiusTestInMeter;
      	**/
       	BuildingGoals goals = new BuildingGoals(scale);
      	double minDist = goals.getMinObjectSeparationReal();      	
      	double signatureRadius = 0.5*signatureDiameternMM*goals.getFactor();
      	
      	this.maxNodeMoveDistance = goals.getMinObjectSeparationReal();      	
      	     
      	System.out.println("minDist[m]: " + minDist + " -- signatureRad [m]: " + signatureRadius);
      	//--start
      	this.displaceLines(lineFeatures,iterations, signatureRadius,minDist);
    }

    //--------------------------
    // main methods
    //-------------------------

    /**
     * method is called from the constructors. Necessary to<p> 
     * 	- set params<p>
     *  - split long lines<p>
     *  - extends short lines by adding (one) middle point<p>
     *  - reconnects the splitted lines
     * The method itself calls calcDisplacement# and stores the output
     *
     * @param allFeatures
     * @param iterations
     * @param signatureRadius
     * @param minDistance
     */
    private void displaceLines(FeatureCollection allFeatures, int iterations, double signatureRadius, double minDistance){
        //-- attention allFeatures can contain Features of all types : lines, polygon, ..
        // a sorting out is done in displace!
        //--snakes parameters
        double alpha = 1;
        double beta = 1; 
        this.solveIter = true;        	   
	    this.iterMax = iterations;
	    
	    Iterator iter = allFeatures.iterator();
	    Feature firstF = (Feature)iter.next();
	    FeatureSchema originalFS = firstF.getSchema();
	    
    	FeatureCollection newFeatures = this.splitLonglines(allFeatures, this.maxPoints);
    	newFeatures = this.extendTooShortLines(newFeatures, this.minPoints);
	    FeatureCollection fc = this.calcDisplacement(newFeatures, minDistance, signatureRadius, alpha, beta);      	
	    this.displacedLines = this.concatLonglines(fc,originalFS);
	    System.gc();
    }
    
	
    /**
     * method which does the netwok calculation and activates the displacment
     * @param features
     * @param minDist
     * @param signatureRadius
     * @param alpha
     * @param beta
     * @return the displaced line objects (LineStrings)
     */
	private FeatureCollection calcDisplacement(FeatureCollection features, double minDist, 
	        			double signatureRadius, double alpha, double beta){


	    System.out.println("++++++++++ init displacement ++++++++++++");
	    System.out.println("parameter do weighting 1) internal to external energy and 2) stiffness to elasticity");
	    System.out.println("parameter: alpha: " + alpha + " -- beta: " + beta);
	    /**
	     * E = Int( Eext + alpha/2*dx'' + beta/2*dx''''  )ds
	     * alpha=1, beta=1 : E = I( Eext + 0.5 dx'' + 0.5 dx'''' )ds
	     * alpha=2, beta=2 : E = I( Eext +  dx'' +  dx'''' )ds
	     */

	    System.gc(); //flush garbage collector	    
	    // --------------------------	    
		if (this.solveIter==false){
			this.iterMax=1;
		}
	    //-- get selected items
	    	    
	    int noItems = features.size();
	    if (noItems > this.maxObjects){
	    	String msg = "LineDisplacementSnakes.calcDisplacement: can not proceed more the 1000 Features: due to matrix memory problems";
	    	System.out.println(msg);	    	
	    	//context.getWorkbenchFrame().warnUser(msg);	    	
	    	return null;
	    }
	    Geometry resultgeom = null;
	    FeatureSchema fs = new FeatureSchema(); boolean attributeExists = false;
	    FeatureSchema treeFS = new FeatureSchema();
	    treeFS.addAttribute("Geometry",AttributeType.GEOMETRY);
	    treeFS.addAttribute(this.DISPID, AttributeType.INTEGER);
	    FeatureDataset resultFeatures = null;
	    ArrayList lineToDisplaceList = new ArrayList();
       	Quadtree featureQtree = new Quadtree();
       	ArrayList nonLineFeatures = new ArrayList(); ArrayList nonLineFeaturesID = new ArrayList();
       	//-------------------------
	    //put them all in a quadtree
       	//--------------------------
       	if(this.monitorExists){
       		this.jumpMonitor.report("Snakes Displacement: create Quadtree");
       	}
       	int count=0;
      	for (Iterator iter = features.iterator(); iter.hasNext();) {     		
      		count++;
      		Feature f = (Feature)iter.next();
      		//System.out.println("========== Item featureID: " + f.getID() + " ==========");
      		//-- create new FeatureType for tree .. needed to get network-state 
      		Feature treeF = new BasicFeature(treeFS);
      		treeF.setGeometry(f.getGeometry());
      		treeF.setAttribute(this.DISPID, new Integer(count-1)); //count-1 to start with 0
      		//--
            featureQtree.insert(treeF.getGeometry().getEnvelopeInternal(), treeF);       
      	}
      	//-------------------------
      	// get Networkstate Matrix
      	//-------------------------
       	if(this.monitorExists){
	     this.jumpMonitor.report("Snakes Displacement: calculate network matrix and nodes");
       	}
      	Matrix networkStateMatrix = this.evaluateLineNetworkState(featureQtree, this.crucialDistance);
	    //-------------------
	    // calc nodes and line ids from networkMatrix
	    //-------------------
      	DisplacementNetworkNodeList nodeList = this.calcNodeList(networkStateMatrix);
      	//-------------------------
      	// initialize LineDataset (lineToDisplaceList) and 
      	// calculate Energies
      	// get a List with nonLineGeometries
      	//------------------------
       	if(this.monitorExists){
       		this.jumpMonitor.report("Snakes Displacement: create LineToDisplace Features and conflict evaluation");
       	}
      	System.out.println("LineDisplacementSnakes: create LineToDisplace Features and conflict evaluation");
      	count = 0;
      	for (Iterator iter = features.iterator(); iter.hasNext();) {     		
      		count++;
      		Feature f = (Feature)iter.next();
      		if(f.getGeometry() instanceof LineString){
	      		System.out.println("========== Item featureID: " + f.getID() + " ==========");
	      		int vertices = f.getGeometry().getNumPoints();
	      		System.out.println("vertices: " + vertices);
	      		if ( vertices < maxPoints){
		      		LineToDisplace lineTD = new LineToDisplace(f,count-1); //count -1 to start with 0
		      		this.evaluateLineWithDataset(lineTD, featureQtree, minDist, 
		      		        signatureRadius, signatureRadius, networkStateMatrix, alpha, beta);
		           	//-- add LineToDisplace to LineToDisplaceList
			       	lineToDisplaceList.add(lineTD);
			       	System.out.print("Line: " + count + " -- IntegralEnergy: " + lineTD.getCurrentIntegralEnergyViaVertices());
			       	System.out.print(" -- extEnergy: " + lineTD.getExternalEnergySum());
			       	System.out.print(" -- intEnergy: " + lineTD.getInternalEnergySum());
			       	System.out.println("");
	      		}
	      		else{// matrices become to big (memory and speed problem can occure)
	      			System.out.println("Line has " + vertices + " vertices: not added to lineToDisplaceList");
	      			//context.getWorkbenchFrame().warnUser("Line has too many vertices => processing skipped!!");
	      		}
		    }
      		else{
      			//-- add to List needed later for new qtree
      			nonLineFeatures.add(f);
      			nonLineFeaturesID.add(new Integer(count-1)); //count -1 to start with 0
      			System.out.println("LineDisplacementSnakes.calcDisplacement(): feature no linestring");
      		}      		
      	}      	
      	//--------------------
      	//show lines as points and buffer
      	//--------------------
      	FeatureDataset fd = null; int idx = 0;
      	ArrayList bufferGeomLines = new ArrayList();  
      	for (Iterator iter = lineToDisplaceList.iterator(); iter.hasNext();) {
			LineToDisplace element = (LineToDisplace) iter.next();
			ArrayList featureList = element.createJumpPointFeaturesOfTempGeom();
			idx++;
			if(idx == 1){
				Feature f = (Feature)featureList.get(0);
				FeatureSchema fs2 = f.getSchema();
				fd = new FeatureDataset(fs2);
			}			
			fd.addAll(featureList);
			//--- buffer
			bufferGeomLines.add(element.getTempGeometry().buffer(minDist*0.5+signatureRadius));
		}
	    if (fd != null){
	        this.initialPointEnergies = fd;
		    //context.addLayer(StandardCategoryNames.WORKING, "initial points with energy", fd);
		    }	    
	    if (bufferGeomLines.size() > 0){
		    FeatureCollection myCollB = FeatureDatasetFactory.createFromGeometry(bufferGeomLines);		    
		    //context.addLayer(StandardCategoryNames.WORKING, "lines with mindist + signature buffers", myCollB);
		    this.minDistAndSignatureBuffers = myCollB;
	    }    
      	//--------------------
      	//get network nodes and show as points
      	//--------------------
	    //monitor.report("Snakes Displacement: get network nodes and display");
	    System.out.println("LinesDisplacementSnakes: get network nodes and display");
	    //-- set network points
	    nodeList.allocatePoints(lineToDisplaceList);
	    //-- calculate initial average point per node 
	    ArrayList networkNodes = nodeList.getAverageNodePoints();
	    if (networkNodes.size() > 0){
		    FeatureCollection myCollB = FeatureDatasetFactory.createFromGeometry(networkNodes);
		    //context.addLayer(StandardCategoryNames.WORKING, "networknodes and split points", myCollB);
		    this.networkNodesAndSplitPoints = myCollB;
	    }    	    
	    //---------------------
	    // displace all lines, then evaluate new 
	    //--------------------
	    //ArrayList pseudoDisplacedLines = new ArrayList();
	    int loops = 0;
	    boolean oneDisplacementMade = true;
	    //while(solved){
	    while((loops < this.iterMax) &&(oneDisplacementMade==true)){	    	
	    	loops++;
	       	if(this.monitorExists){
	       		this.jumpMonitor.report("Snakes Displacement: Iteration " + loops);
	       	}
	    	oneDisplacementMade = false;
	    	System.out.println("========= Iteration : " + loops + "  ==========="); 
	    	//-- iterate over all lines
	    	int linIdx = 0;
      		for (Iterator iter = lineToDisplaceList.iterator(); iter.hasNext();) {
      			linIdx++;
      			LineToDisplace element = (LineToDisplace) iter.next();
      			if(element.hasConflicts()==true){
      				oneDisplacementMade = true;
      			    //--- attention: line must have more than 6 points
      			    //    calculation of Apent before cycle would make it faster,
      				//	  but then only one line could be solved instead all after each other
      		       	SnakesDisplacementLines sd = new SnakesDisplacementLines();
      		       	sd.setParamsAndCalcMat(element.getTempGeometry().getNumPoints(),
      		       							alpha, beta,
											element.getNetworkstate());

      				//if(loops == 1){
	          			//-------------------------------
	          			// do pseudo displacement for one line to get new internal energy
	          			//-------------------------------
	    		       	LineString newLine = sd.solve((LineString)element.getTempGeometry(),
	    		       									  element.getExtEnergy(),
														  element.getDeltaIntEnergyToOrigState()); //is Matrix of zeros
	    		       	//pseudoDisplacedLines.add(newLine);
	    		       	//----------------------------
	    		       	// calculate new internal Energy and energy differences after pseudo-displacement
	    		       	// to introduce the dE_int  for the final displacement
	    		       	//---------------------------	    		       	 	    		       	
	    		       	/**
	    		       	//-- the following E_int calculation was a wrong calculation
	    		       	Matrix intEnergy = SnakesEnergyDisplacement.calcInternalEnergy(
	   							(LineString)element.getOriginalGeometry(), newLine );
	   					**/
	    		       	//System.out.println(" old intEnergy: " + element.getInternalEnergySum());
	    		       	Matrix intEnergy = SnakesEnergyDisplacement.calcInternalEnergy(
	    		       			newLine, alpha, beta);
	    		       	element.setIntEnergy(intEnergy);	    		       	
	    		       	/* //-- some outputs
	    		       	System.out.println("energies after pseudo displacement:");
	    		       	System.out.print("Line: " + linIdx + " -- IntegralEnergy: " + element.getCurrentIntegralEnergyViaVertices());
	    		       	System.out.print(" -- extEnergy: " + element.getExternalEnergySum());
	    		       	System.out.print(" -- intEnergy: " + element.getInternalEnergySum());
	    		       	System.out.print(" -- deltaIntEnergy: " + element.getDeltaIntEnergySum());
	    		       	System.out.println("");
	    		       	
	    		       	//-- create point features with internal energy after pseudo displacement
	    		       	ArrayList featureList = element.createJumpPointFeaturesOfTempGeom();
	    			    if (fd.size() > 0){
	    				    context.addLayer(StandardCategoryNames.WORKING, "testdisplacement points", fd);
	    				    }
	    				*/	    
      			//	}
    		       	//--------------------------
    		       	// do displacement, increment counter and set new temporary geometry
	    		    // check if original line is closed and close again  	
    		       	//-------------------------
    		       	/*LineString*/newLine = sd.solve((LineString)element.getTempGeometry(),
								  element.getExtEnergy(),
								  element.getDeltaIntEnergyToOrigState());
    		       	element.noOfDisplacementsAdd();
      				element.setTempGeometry(newLine);
      				//-- close LineString if original Linestring is a ring. 
      				//  The new Start and Endpoint coordinates are interpolated      				
      				if (element.isClosedLine() == true){
      					element.closeTempGeometry();
      				}      				
      			}      			
      		}//end for LineToDisplaceList
      		if (oneDisplacementMade==true){
      	      	//--------------------
      	      	//get new network nodes, show as points and set new average point positions (of tempGeometry)
      	      	//--------------------
      		    //-- set network points
      		    nodeList.allocatePoints(lineToDisplaceList);
      		    /*
      		    //-- calculate average point per node and display them
      		    networkNodes = nodeList.getAverageNodePoints();
      		    if (networkNodes.size() > 0){
      			    FeatureCollection myCollB = FeatureDatasetFactory.createFromGeometry(networkNodes);
      			    context.addLayer(StandardCategoryNames.WORKING, "networknodes " + loops, myCollB);
      		    } 
      		    */   	    
      		    //-- calculate average pos of node points
      		    nodeList.getAverageNodePoints();
      		    //-- set all points to that average position 
      		    nodeList.setNodePointsToAverage();
      		    //-- check and correct if node displacement was to 
      		    //   far from original node position
      		    nodeList.checkAndSetActualAverageNodeToMaxDist(this.maxNodeMoveDistance);
	      		//---------------------
				//  create new qtree
				//--------------------
	      		featureQtree = new Quadtree();
	          	for (int idx1=0; idx1 < nonLineFeatures.size(); idx1 ++) {     		
	          		Feature f = (Feature)nonLineFeatures.get(idx1);
	          		Integer dispFID = (Integer)nonLineFeaturesID.get(idx1);
	          		Feature treeF = new BasicFeature(treeFS);
	          		treeF.setGeometry(f.getGeometry());
	          		treeF.setAttribute(this.DISPID, dispFID);
	          		Envelope env = treeF.getGeometry().getEnvelopeInternal();
	                featureQtree.insert(env, treeF);	          		
	          	}      		          	
	          	for (Iterator iter = lineToDisplaceList.iterator(); iter.hasNext();) {  
	          		LineToDisplace element = (LineToDisplace) iter.next();
	          		//-- create new FeatureType for tree .. needed to get network-state 
	          		Feature treeF = new BasicFeature(treeFS);
	          		treeF.setGeometry(element.getTempGeometry());
	          		treeF.setAttribute(this.DISPID, new Integer(element.getFeatureID()));	          		
	                featureQtree.insert(treeF.getGeometry().getEnvelopeInternal(), treeF);
	          	}      		
	      		//----------------
				//  evaluate new
				//----------------
	        	System.out.println(">>>>> after displacement : " + loops); 
	        	int i = 0;
	          	for (Iterator iter = lineToDisplaceList.iterator(); iter.hasNext();) {
	          		i++;
	          		LineToDisplace element = (LineToDisplace) iter.next();               
		      		this.evaluateLineWithDataset(element, featureQtree, minDist, signatureRadius, 
		      		        signatureRadius, networkStateMatrix, alpha, beta);
			       	System.out.print("Line: " + i + " -- IntegralEnergy: " + element.getCurrentIntegralEnergyViaVertices());
			       	System.out.print(" -- extEnergy: " + element.getExternalEnergySum());
			       	System.out.print(" -- intEnergy: " + element.getInternalEnergySum());
			       	System.out.print(" -- deltaIntEnergy: " + element.getDeltaIntEnergySum());
			       	System.out.println("");
	          	}
      		}
      		else{//one displacement made == false 
      			System.out.println(">>>>> no conflicts found!");
      		}
		} //end while

	    //-----------------------
	    // create resultList
	    //-----------------------
	    idx=0;
      	for (Iterator iter = lineToDisplaceList.iterator(); iter.hasNext();) {
      		idx++;
			LineToDisplace element = (LineToDisplace) iter.next();
			Feature f = (Feature)element.getOriginalJumpFeature().clone();
			if(idx == 1){
				FeatureSchema fs3 = f.getSchema();
				resultFeatures = new FeatureDataset(fs3);
			}
			//-- set new geometry
			f.setGeometry(element.getTempGeometry());
			//-- add to out list
			resultFeatures.add(f);
		}
      	//-- show pseudo displaced lines
      	/*
      	FeatureCollection myCollA = FeatureDatasetFactory.createFromGeometry(pseudoDisplacedLines);
	    if (myCollA.size() > 0){
	    	context.addLayer(StandardCategoryNames.WORKING, "pseudoLinesOfFirstLoop", myCollA);
	    }
	    */
      	//-------------------------
      	// show states of a lineset
      	//-------------------------
      	/*
      	for (int i = 0; i < this.iterMax; i++) {
			ArrayList actualState= new ArrayList();
	      	for (Iterator iter = lineToDisplaceList.iterator(); iter.hasNext();) {
				LineToDisplace element = (LineToDisplace) iter.next();
				if(element.getNoOfDisplacements() > i){
					actualState.add((Geometry)element.getAllGeometryStates().get(i));
				}
	      	}
		    FeatureCollection myCollG = FeatureDatasetFactory.createFromGeometry(actualState);
		    if (myCollG.size() > 0){
		    	context.addLayer(StandardCategoryNames.WORKING, "state " +  i, myCollG);
		    }
		}   		    	       			    
      	*/
        return resultFeatures;        
	}
     
	/**
	 * evaluates internal, external, integral Energy, networkstate of 
	 * tempGeometry of LineToDisplace and stores them in the LineToDisplace fields 
	 * @param line the object to check
	 * @param geomQTree all the other objects
	 * @param minDist
	 * @param signatureRadius
	 */
	private void evaluateLineWithDataset(LineToDisplace line, Quadtree featureQTree, 
										double minDist, double signature1Radius, 
										double signature2Radius, Matrix networkStates, 
										double alpha, double beta){
		
        //------------------
  		// get Candidates
        //-----------------
        //-- rough check using the tree of original geometries
        List candidates = featureQTree.query(line.getOriginalGeometry().getEnvelopeInternal());
        //----------------
        // detailed check and conflict:
        //----------------
        //-- clear old List of conflict Matrices of LineToDisplace
        line.getDispVectorDxMatrixList().clear();
        line.getDispVectorDyMatrixList().clear();
       	//-- make new conflict list for this line with other lines
       	//List confMatListdx = new ArrayList();
       	//List confMatListdy = new ArrayList();	
       	//
        LineString candLine = null;
       	for (Iterator jter = candidates.iterator(); jter.hasNext();) {
            Feature candidateF = (Feature)jter.next();
            Geometry candidateG = candidateF.getGeometry();
	    	Integer linIdB = (Integer)candidateF.getAttribute(this.DISPID);
        	//just to check if it works
        	/*        	  
        	 if(candidate.equals(line.getTempGeometry())){
        		System.out.println("DisplaceLinesPlugIn.evaluateLineWithDataset(): now line would check against it self");
        	 }
        	*/
            //-----------------------------------------
            // check candidate if LineString or Polygon
            //-----------------------------------------
            if((candidateF.getGeometry() instanceof LineString) && 
            		//-- to exclude evaluation with it self
            		(!candidateF.getGeometry().equals(line.getTempGeometry()) )){ 
            	candLine = (LineString)candidateF.getGeometry();
            	LineLineDistance checkConflicts = new LineLineDistance(
            				(LineString)line.getTempGeometry(), candLine, 
							minDist, signature1Radius, signature2Radius);
        	    if(checkConflicts.haveConflict()){

        	    	//-- this appears if lines are to narrow or intersect
        	       	//-- get disp vectors/matrix for this two lines
        	    	checkConflicts.calcDisplacementVectors();
        	       	//-----------------
        	       	// check for first time the network state
        	    	// this needs previous processing of  #calcDisplacementVectors()
        	       	//----------------
        	       	if (line.getNoOfDisplacements() == 0){
	        	       	if(checkConflicts.hasIdentityFirstPtLineA()){
	        	       	    line.setNetworkstateStartPt(1);
	        	       	}
	        	       	if(checkConflicts.hasIdentityLastPtLineA()){
	        	       	    line.setNetworkstateEndPt(1);
	            		}
        	       	}//end check networkstate
        	       	Matrix conflictMatrixDx = checkConflicts.getDxConflictMatrix();
        	       	Matrix conflictMatrixDy = checkConflicts.getDyConflictMatrix();
        	       	/*--------
        	       	 * set only half displacement vectors since every line will be moved
        	       	 * otherwise the displaced distance between both lines will be twice as large
        	       	 * as it is intend to be
        	       	 --------*/   
        	       	conflictMatrixDx = MatlabSyntax.times(conflictMatrixDx,0.5);
        	       	conflictMatrixDy = MatlabSyntax.times(conflictMatrixDy,0.5);
        	       	/*--------
        	       	 * change conflictMatrix for first and second line point if
        	       	 * the networkstate makes it necessary - the second point is 
        	       	 * neccesary to save incoming direction and pushing away from first point
        	       	 * rows = number of vertices of line
        	       	 ---------*/
        	       	//-- attention: using the network state matrix
        	       	//				since we only want to modify the actual conflict matrix
        	       	if((networkStates.get(line.getFeatureID(),linIdB.intValue()) == 1) &&
        	       			(networkStates.get(linIdB.intValue(), line.getFeatureID()) == 1)){
        	       		//-- set conflicts with the first two points (1. edge) from 
        	       		//   other line object to zero
        	       		this.resetMatrixForNetworkStatusStartStart(conflictMatrixDx);
        	       		this.resetMatrixForNetworkStatusStartStart(conflictMatrixDy);
        	       	}
        	       	if((networkStates.get(line.getFeatureID(),linIdB.intValue()) == 1) &&
        	       			(networkStates.get(linIdB.intValue(), line.getFeatureID()) == 2)){
        	       		this.resetMatrixForNetworkStatusStartEnd(conflictMatrixDx);
        	       		this.resetMatrixForNetworkStatusStartEnd(conflictMatrixDy);
        	       	} 
        	       	if((networkStates.get(line.getFeatureID(),linIdB.intValue()) == 2)||
        	       			(networkStates.get(linIdB.intValue(), line.getFeatureID()) == 1)){
        	       		//-- set conflicts with the last two points (1. edge) from 
        	       		//   other line object to zero
        	       		this.resetMatrixForNetworkStatusEndStart(conflictMatrixDx);
        	       		this.resetMatrixForNetworkStatusEndStart(conflictMatrixDy);
        	       	}
        	       	if((networkStates.get(line.getFeatureID(),linIdB.intValue()) == 2)||
        	       			(networkStates.get(linIdB.intValue(), line.getFeatureID()) == 2)){
        	       		this.resetMatrixForNetworkStatusEndEnd(conflictMatrixDx);
        	       		this.resetMatrixForNetworkStatusEndEnd(conflictMatrixDy);
        	       	}
        	       	//-- if lines form a ring
        	       	if((networkStates.get(line.getFeatureID(),linIdB.intValue()) == 3)||
        	       			(networkStates.get(linIdB.intValue(), line.getFeatureID()) == 3)){
        	       		this.resetMatrixForNetworkStatusStartEnd(conflictMatrixDx);
        	       		this.resetMatrixForNetworkStatusStartEnd(conflictMatrixDy);
        	       		this.resetMatrixForNetworkStatusEndStart(conflictMatrixDx);
        	       		this.resetMatrixForNetworkStatusEndStart(conflictMatrixDy);
        	       	}        	       	
        	       	//--store matrices to line object 
        	       	line.getDispVectorDxMatrixList().add(conflictMatrixDx);
        	       	line.getDispVectorDyMatrixList().add(conflictMatrixDy);
        	    }
            }
            else if(candidateF.getGeometry() instanceof Polygon){
            	Polygon candPoly = (Polygon)candidateF.getGeometry();
            	PolygonLineDistance checkConflicts = new PolygonLineDistance(
            			candPoly, (LineString)line.getTempGeometry(), minDist, signature1Radius);
            	/*****
            	 * TODO: implement line conflicts with polygons
            	 * 		 question: Who has the higher order im mapping?
            	 * 		 So: don't implement for buildings?
            	 * 			 but implement for point signatures (cities, roads) and 
            	 * 			 lake polygons.      
            	 *****/
            }
            else{
            	//System.out.println("DisplaceLinesPlugIn.evaluateLineWithDataset(): selfcheck or other object: no linestring/polygon");
            }
        }// iteration over all candidates
       	
       	//-------------------------------------
       	// calc ext and internal energy of line
       	// and add to LineToDisplace object
       	//-------------------------------------
       	//-- calc external Energy for all conflict over all lines
       	Matrix extEnergy = SnakesEnergyDisplacement.calcExtEnergyOverAllLines(
       							line.getDispVectorDxMatrixList(),
								line.getDispVectorDyMatrixList(),
       							line.getTempGeometry().getNumPoints());
       	line.setExtEnergy(extEnergy);
       	//-- calc internal Energy
       		// if first run , then original line = displaced line
       	/** the following E_int calculation was a wrong calculation
       	Matrix intEnergy = SnakesEnergyDisplacement.calcInternalEnergy(
       							(LineString)line.getOriginalGeometry(), 
								(LineString)line.getTempGeometry());
		**/
       	Matrix intEnergy = SnakesEnergyDisplacement.calcInternalEnergy(
					(LineString)line.getTempGeometry(),	alpha, beta);
       	line.setIntEnergy(intEnergy);
       	//System.out.println("alpha: " + alpha + " -- beta: " + beta);
       	//System.out.println("int energy sum: " + line.getInternalEnergySum());

       	//-- calc overall (integral) energy
       	double energy = SnakesEnergyDisplacement.calcIntegralEnergy(intEnergy, extEnergy);
       	line.getIntegralEnergyList().add(new Double(energy));	
	}
	
	/**
	 * sets the conflicts of first two linevertices with first  
	 * two points of other line to zero  
	 * @param conflictMatrix for one coordinate
	 */
	private void resetMatrixForNetworkStatusStartStart(Matrix conflictMatrix){
   		int nrPointsL2 = conflictMatrix.getColumnDimension();
   		conflictMatrix.set(0,0,0); 
   		conflictMatrix.set(0,1,0); 
   		conflictMatrix.set(1,0,0); 
   		conflictMatrix.set(1,1,0);         	       		        	       		
	}

	/**
	 * sets the conflicts of first two linevertices with  
	 * last two points of other line to zero  
	 * @param conflictMatrix for one coordinate
	 */
	private void resetMatrixForNetworkStatusStartEnd(Matrix conflictMatrix){
   		int nrPointsL2 = conflictMatrix.getColumnDimension();
   		conflictMatrix.set(0,nrPointsL2-1,0); 
   		conflictMatrix.set(0,nrPointsL2-2,0); 
   		conflictMatrix.set(1,nrPointsL2-1,0); 
   		conflictMatrix.set(1,nrPointsL2-2,0);         	       		
	}

	/**
	 * sets the conflicts of last two linevertices with first  
	 * two points of other line to zero  
	 * @param conflictMatrix for in coordinate
	 */
	private void resetMatrixForNetworkStatusEndStart(Matrix conflictMatrix){
   		int nrPointsL1 = conflictMatrix.getRowDimension();
   		conflictMatrix.set(nrPointsL1-1,0,-0); 
   		conflictMatrix.set(nrPointsL1-1,1,-0); 
   		conflictMatrix.set(nrPointsL1-2,0,-0); 
   		conflictMatrix.set(nrPointsL1-2,1,-0);         	       		        	       		
	}

	/**
	 * sets the conflicts of last two linevertices with  
	 * last two points of other line to zero  
	 * @param conflictMatrix for in coordinate
	 */
	private void resetMatrixForNetworkStatusEndEnd(Matrix conflictMatrix){
   		int nrPointsL1 = conflictMatrix.getRowDimension();
   		int nrPointsL2 = conflictMatrix.getColumnDimension();
   		conflictMatrix.set(nrPointsL1-1,nrPointsL2-1,-0);         	       		
   		conflictMatrix.set(nrPointsL1-2,nrPointsL2-1,-0); 
   		conflictMatrix.set(nrPointsL1-1,nrPointsL2-2,-0); 
   		conflictMatrix.set(nrPointsL1-2,nrPointsL2-2,-0);         	       		        	       		        	       				
	}

	/**
	 * copy the input feature to a new Schema whereby the new 
	 * Feature Schema musst be an extended or shortened one 
	 * @param oldSchema
	 * @return Feature
	 */
	private Feature copyFeature(Feature feature, FeatureSchema newSchema){
		FeatureSchema oldSchema = feature.getSchema();
		Feature newF = new BasicFeature(newSchema);
		int n = 0;
		if (oldSchema.getAttributeCount() > newSchema.getAttributeCount()){
			//for schema shortening
			n = newSchema.getAttributeCount();
		}
		else{
			//for schema extension
			n = oldSchema.getAttributeCount();
		}
		for (int i = 0; i < n; i++) {			
			String aname = oldSchema.getAttributeName(i);
			Object value = feature.getAttribute(aname);			
			newF.setAttribute(aname,value);						
		}		
		return newF;
	}
	
	/**
	 * copy/clone the input featureSchema and  since it is not proper implemented in Jump 
	 * @param oldSchema
	 * @return
	 */
	private FeatureSchema copyFeatureSchema(FeatureSchema oldSchema){
		FeatureSchema fs = new FeatureSchema();
		for (int i = 0; i < oldSchema.getAttributeCount(); i++) {
			AttributeType at = oldSchema.getAttributeType(i);
			String aname = oldSchema.getAttributeName(i);
			fs.addAttribute(aname,at);
			fs.setCoordinateSystem(oldSchema.getCoordinateSystem());			
		}		
		return fs;
	}
	
	
	/**
	 * calculates a matrix containing the networkstate for one line to another. <p>
	 * possible states: <p>
	 * 		0: no connection, <p> 
	 * 		1: connected on StartPoint of actual Line<p> 
	 * 		2: connected on Endpoint of actual Line<p>
	 * 		3: both lines are connected on Start and End point<p>
	 * 		-1: Feature is not a LineString<p>
	 * Attention: matrix will not be symmetric! 
	 * @param originalFeatureQTree : the Feature Attribute "displaceID" musst be from 0 to n 
	 * @param tresholdDistance
	 * @return matrix with integer values of network state
	 */
	private Matrix evaluateLineNetworkState(Quadtree originalFeatureQTree, double thresholdDistance){
		int n=originalFeatureQTree.size();
		List allFeatures = originalFeatureQTree.queryAll(); 
		Matrix networkState = MatlabSyntax.zeros(n,n);
		//this procedure will only check points against each other ... no line-Point distances are evaluated!
		for(int i=0; i < n; i++){
			Feature f = (Feature)allFeatures.get(i);
			Integer id1 = (Integer)f.getAttribute(this.DISPID);
			if (f.getGeometry() instanceof LineString){
				LineString linA = (LineString)f.getGeometry();
				//-- query all objects close by
	        	List candidates = originalFeatureQTree.query(f.getGeometry().getEnvelopeInternal());	        	
	        	for (Iterator iter = candidates.iterator(); iter.hasNext();) {
					Feature candidate = (Feature) iter.next();
					Integer id2 = (Integer)candidate.getAttribute(this.DISPID);
					//-- evaluate
					if (candidate.getGeometry() instanceof LineString){
						boolean wasSet = false;
						LineString linB = (LineString)candidate.getGeometry();
						double distSS = SecondGeodeticTask2d.calcDistancePoints(linA.getStartPoint(),linB.getStartPoint());
						double distSE = SecondGeodeticTask2d.calcDistancePoints(linA.getStartPoint(),linB.getEndPoint());
						if ((distSS < thresholdDistance)|| (distSE < thresholdDistance)) {
							//-- start point has problems
							networkState.set(id1.intValue(),id2.intValue(),1);
							wasSet = true;
						}
						double distES = SecondGeodeticTask2d.calcDistancePoints(linA.getEndPoint(),linB.getStartPoint());
						double distEE = SecondGeodeticTask2d.calcDistancePoints(linA.getEndPoint(),linB.getEndPoint());						
						if ((distES < thresholdDistance)|| (distEE < thresholdDistance)) {
							if (wasSet==true){
								//--start and endpoint have problems
								//-- if line has checked itself
								if (linA.equals(linB)==true){
									networkState.set(id1.intValue(),id2.intValue(),5);
								}
								else{
									//--get configuration / line orientation
									if((distEE < thresholdDistance) && (distSS < thresholdDistance)){
										//--end-end and start-start are connected 
										networkState.set(id1.intValue(),id2.intValue(),4);
									}
									else if((distES < thresholdDistance) && (distSE < thresholdDistance)){
										//--start-end and end-start are connected
										networkState.set(id1.intValue(),id2.intValue(),3);
										}
									else{
										//-- this should usually not happen 
										//   but could be if a circular exists (kreisförmige strasse, schlaufe)?  
										networkState.set(id1.intValue(),id2.intValue(),99);
									}
								}
							}
							else{
								//--end point has problems 
								networkState.set(id1.intValue(),id2.intValue(),2);
							}
						}
					}
					else{//-- other object is not a line string 
						networkState.set(id1.intValue(),id2.intValue(),-1);
					}
				}// iter candidates
			}//if
			else{//-- this object is not a line string
				networkState.setRow(id1.intValue(),-1);
			}
		} //iter all items 
		return networkState;
	}
	
	/**
	 * 
	 * @param networkStateMatrix
	 * @return
	 */
	private DisplacementNetworkNodeList calcNodeList(Matrix networkStateMatrix){		
		DisplacementNetworkNodeList nodeList = new DisplacementNetworkNodeList();
		Matrix modNetMat = (Matrix)networkStateMatrix.copy();
		int n = modNetMat.getRowDimension(); //the matrix should be a square matrix		
		for(int curRow=0; curRow < n; curRow++){
			ArrayList startPtConfLineIndices = new ArrayList();
			ArrayList endPtConfLineIndices = new ArrayList();
			//-------------------------------
			//search connetcions of one line (=one row) 
			//over all other lines (=cols)
			//-------------------------------
			for(int curCol=0; curCol < n; curCol++){
				 double val = modNetMat.get(curRow,curCol);
				 if (val == 1){
				 	startPtConfLineIndices.add(new Integer(curCol));
				 }
				 if (val == 2){
				 	endPtConfLineIndices.add(new Integer(curCol));
				 }				
				 if (val == 3){
				 	startPtConfLineIndices.add(new Integer(curCol));
				 	endPtConfLineIndices.add(new Integer(curCol));				 	
				 }		
				 if (val > 3){
				 	System.out.println("LineDisplacementSnakes.calcNodeList: this networkstate is not treated!!!" +
				 			"..and recommends an implementation");
				 	System.out.println("LineDisplacementSnakes.calcNodeList: networkstate:"+ val);
				 }
			}//end loop over all other lines
			//------------------------------- 
			// if found: make a new node, add the point indizes 
			// and store to nodeList
			//-------------------------------
			if (startPtConfLineIndices.size() > 0){
				//--make new node
				DisplacementNetworkNode node = new DisplacementNetworkNode();
				//-- add point of this line (1= start position)
				node.linePosList.add(new Integer(1));
				node.lineIdForPoints.add(new Integer(curRow));
				//-- add points of other lines
				for (int i=0; i < startPtConfLineIndices.size() ;i++){
					//-- get line ID in matrix
					Integer idx = (Integer)startPtConfLineIndices.get(i);
					node.lineIdForPoints.add(idx);
					//-- look for the kind of point (start or end) of other line
					//   and save in networkNode lists
					double val = modNetMat.get(idx.intValue(),curRow);
					if (val == 3 ){
						//-- set other point is endpoint 
						node.linePosList.add(new Integer(2));
					}
					else{
						node.linePosList.add(new Integer((int)val));
					}
				}		
				nodeList.add(node);
			}
			if (endPtConfLineIndices.size() > 0){
				//--make new node
				DisplacementNetworkNode node = new DisplacementNetworkNode();
				//-- add point of this line (2= end position)
				node.linePosList.add(new Integer(2));
				node.lineIdForPoints.add(new Integer(curRow));
				//-- add points of other lines
				for (int i=0; i < endPtConfLineIndices.size() ;i++){
					Integer idx = (Integer)endPtConfLineIndices.get(i);
					node.lineIdForPoints.add(idx);
					//-- look for the kind of point (start or end) of other line
					//   and save in networkNode lists
					double val = modNetMat.get(idx.intValue(),curRow);
					if (val == 3 ){
						//-- set other point is startpoint 
						node.linePosList.add(new Integer(1));
					}
					else{
						node.linePosList.add(new Integer((int)val));
					}
				}		
				nodeList.add(node);
			}
			//-------------------------
			// delete found connections
			//-------------------------
			//--set row values to -2 that means it has been checked
			modNetMat.setRow(curRow,-2);
			//--set correspondending matrix values to -2 which have been found
			for (int i=0; i < startPtConfLineIndices.size() ;i++){
				Integer idx = (Integer)startPtConfLineIndices.get(i);
				//-- get pos value (1 or 2) of other points and delete 
				//   equal values = same point connections in that row
				double val = modNetMat.get(idx.intValue(),curRow);
				for(int j=0; j < n; j++){
					double valtemp = modNetMat.get(idx.intValue(),j);
					if(val == valtemp){
						modNetMat.set(idx.intValue(),j,-2);
					}
				}
				//--now delete also coresponding value
				modNetMat.set(idx.intValue(),curRow,-2);

			}
			for (int i=0; i < endPtConfLineIndices.size() ;i++){
				Integer idx = (Integer)endPtConfLineIndices.get(i);
				//-- get pos value (1 or 2) of other points and delete 
				//   equal values = same point connections in that row
				double val = modNetMat.get(idx.intValue(),curRow);
				for(int j=0; j < n; j++){
					double valtemp = modNetMat.get(idx.intValue(),j);
					if(val == valtemp){
						modNetMat.set(idx.intValue(),j,-2);
					}
				}
				//--now delete also coresponding value
				modNetMat.set(idx.intValue(),curRow,-2);
			}
		}//end loop over alle rows = lines
		
		return nodeList;
	}
	
	/**
	 * splits to long lines and created a new (extended) FeatureDataset with 
	 * new Attribute containing information which line has been split into others.
     * Attention: the function delivers a copy the original feature collection. The featureSchema is
     * obtained from the first feature!!!.
     *
	 * @param features
	 * @param maxVertices
	 * @return
	 */
	private FeatureCollection splitLonglines(FeatureCollection features, int maxVertices){
 
		FeatureDataset resultFeatures = null;
		FeatureSchema fs = null;
       	int count=0; int lineId = 0;
      	for (Iterator iter = features.iterator(); iter.hasNext();) {     		
      		count++;
      		Feature f = (Feature)iter.next();
      		//--            
      		if (count == 1){      			
      			//-- not sure to do that, since feature schemas of selected objects might be different 
      			fs = copyFeatureSchema(f.getSchema());
       			boolean attributeExists1 = fs.hasAttribute(this.newSplitLineAttributeString);          			
       			if (attributeExists1 == false){	      		      			
	      			fs.addAttribute(this.newSplitLineAttributeString, AttributeType.INTEGER);
       			}
       			boolean attributeExists2 = fs.hasAttribute(this.newSplitSegmentAttributeString);          			
       			if (attributeExists2 == false){	      		      			
	      			fs.addAttribute(this.newSplitSegmentAttributeString, AttributeType.INTEGER);
       			}       			
      			resultFeatures = new FeatureDataset(fs);
      		}  
      		//-- make new with new featureSchema 
      		Feature newf = this.copyFeature(f,fs);
      		//-- create add/new features
      		if (f.getGeometry()instanceof LineString){
      			LineString line = (LineString)f.getGeometry();
      			if (line.getNumPoints() > maxPoints){
      				lineId = lineId+1;
	      	    	int[] pointidx = this.getSegmentationPointsForLongLines(line, maxVertices);
	      	        LineString[] segmentList = SplitLineString.splitInSegments(line,pointidx);
	      	        for (int i = 0; i < segmentList.length; i++) {
						Feature newSegment = (Feature)newf.clone(true); //true = deep with geometry 
						newSegment.setAttribute(this.newSplitLineAttributeString,new Integer(lineId));
						newSegment.setAttribute(this.newSplitSegmentAttributeString,new Integer(i+1));
						newSegment.setGeometry((LineString)segmentList[i].clone());
						resultFeatures.add(newSegment);
					}
      			}
      			else{
          			newf.setAttribute(this.newSplitLineAttributeString,new Integer(0));
          			newf.setAttribute(this.newSplitSegmentAttributeString,new Integer(0));
          			resultFeatures.add(newf);      				
      			}
      		}//if lineString
      		else{
      			newf.setAttribute(this.newSplitLineAttributeString,new Integer(0));
      			newf.setAttribute(this.newSplitSegmentAttributeString,new Integer(0));
      			resultFeatures.add(newf);
      		}
      	}      
		return resultFeatures;
	}
	
	/**
	 * 
	 * @param features
	 * @return
	 */
	private FeatureCollection concatLonglines(FeatureCollection features, FeatureSchema originalSchema){
		
		//-- this merging demands that segments of one original line 
		//   are stored after each other in the featureCollection (so as it is done in splitting) 
		FeatureDataset resultFeatures = new FeatureDataset(originalSchema);
		FeatureSchema fs = null;
		ArrayList linesToMerge = new ArrayList();
		ArrayList lineSegments = null;
       	Integer lineID = new Integer(0); boolean found = false; 
      	for (Iterator iter = features.iterator(); iter.hasNext();) {     		      	
      		Feature f = (Feature)iter.next();           
      		//-- make new feature with new featureSchema 
      		Feature newf = this.copyFeature(f,originalSchema);
      		//-- create add/new features
      		int oldLineID = 0;
      		if (f.getGeometry()instanceof LineString){
      			LineString line = (LineString)f.getGeometry();
				oldLineID = lineID.intValue(); //store old lineId
      			lineID = (Integer)f.getAttribute(this.newSplitLineAttributeString);
      			if (lineID.intValue() > 0){
      				found = true; //necessary to add last lineSegment to linesToMerge 
      							  //at the end of the loop
      				if (lineID.intValue() == oldLineID){
      					lineSegments.add(f);
      				}
      				else{//new id number
      					if(lineSegments!= null){
      						linesToMerge.add((ArrayList)lineSegments.clone());
      						lineSegments = new ArrayList();
      						lineSegments.add(f);
      					}
      					else{//do this if we get our first line to merge
      						lineSegments = new ArrayList();
      						lineSegments.add(f);
      					}
      				}
      			}
      			else{// lineID value == 0
          			resultFeatures.add(newf);      				
      			}
      		}//if lineString
      		else{
      			resultFeatures.add(newf);
      		}
      	}
      	if (found == true){
      	    // add last segment
      		linesToMerge.add(lineSegments);
      	}
      	if (found == true){
	      	//-- create Features from segments 
			for (Iterator iter = linesToMerge.iterator(); iter.hasNext();) {
				List segmentList = (List) iter.next();
				int n= segmentList.size();
				LineString[] lsArray = new LineString[n];
				Feature newf = null;
				int j=0; boolean dontSave = false;
				for (Iterator iterator = segmentList.iterator(); iterator.hasNext();) {
					Feature lineF = (Feature) iterator.next();
					if(j==0){//Line feature attributes will be the ones of first line segment
						newf = this.copyFeature(lineF,originalSchema); 
					}
	      			Integer segmID = (Integer)lineF.getAttribute(this.newSplitSegmentAttributeString);
	      			if (j !=  (segmID.intValue()-1)){
	      				dontSave = true;
	      				System.out.println("LineDisplacementSnakes.concateLongLines(): Segments not in correct order for merge");
	      			}
	      			else{
	      				lsArray[segmID.intValue()-1]= (LineString)lineF.getGeometry();
	      			}
					j++;
				}
				if(dontSave == false){
					LineString mergedLine = SplitLineString.concatSegements(lsArray);
					newf.setGeometry(mergedLine);
					resultFeatures.add(newf);
				}
			}
      	}
        //-- concat segments and check if smoothed too much
        //this.smoothedLine = SplitLineString.concatSegements(smoothedSegments);
		return resultFeatures;
	}
	
    /**
     * calculates the segementation points for too long lines, the number of points
     * is not strikt, rather a random number of points is added or subtracted   
     * @param line
     * @return pointidx
     */
    private int[] getSegmentationPointsForLongLines(LineString line, int maxPoints){
    	//-- calculate random split index = maxpoint +/- x	: x=<5;
    	int segmentPointIdx = maxPoints + (int)Math.ceil((Math.random()-0.5)*10);
   	    int parts = 1 + (int)Math.ceil(line.getNumPoints()/maxPoints);   	    
       	int[] pointidx = new int[parts-1];
       	int delta = (int)Math.ceil(line.getNumPoints() / parts);
       	System.out.println("delta: "+ delta);
       	System.out.print("Segmentation point indices for to long lines: ");
       	for (int j = 0; j < pointidx.length; j++) {
       	    pointidx[j]=delta*(j+1);
           	System.out.print(pointidx[j] + "  ");
        }
       	System.out.println(" ");
       	return pointidx;
    }

    /**
     * this function checks if a linestring is to short with respect to the number of vertices.<p>
     * If yes it adds middle points to every line segment.<p>
     * Attention: the function delivers a copy the original feature collection. The featureSchema is
     * obtained from the first feature!!!.
     * @param fc
     * @param minNoOfVertices
     * @return
     */
    private FeatureCollection extendTooShortLines(FeatureCollection fc, int minNoOfVertices){
    	
    	FeatureDataset newFc = null;    	
		FeatureSchema fs = null;
       	int count=0; int lineId = 0;
      	for (Iterator iter = fc.iterator(); iter.hasNext();) {     		
      		count++;
      		Feature f = (Feature)iter.next();
      		//--            
      		if (count == 1){      			
      			//-- not sure to do that, since feature schemas of selected objects might be different 
      			fs = copyFeatureSchema(f.getSchema());
      			newFc = new FeatureDataset(fs);
      		} 
      		if(f.getGeometry() instanceof LineString){
      			LineString line = (LineString)f.getGeometry();
      			if(line.getNumPoints() <= minNoOfVertices){
      				LineString newLine = InterpolateLinePoints.addMiddlePoints(line);
      				f.setGeometry(newLine);
      				System.out.println("LineDisplacementSnakes.extendTooShortLines: to short " +
      						"line found with" + line.getNumPoints() + "points. Points added");
      			}
      		}			
			newFc.add(f);
      	}    	
    	return newFc;
    }
    
    /********************************* Getters and Setters ***************************/
    
    public FeatureCollection getDisplacedLines() {
        return displacedLines;
    }
    public FeatureCollection getInitialPointEnergies() {
        return initialPointEnergies;
    }
    public FeatureCollection getMinDistAndSignatureBuffers() {
        return minDistAndSignatureBuffers;
    }

    public FeatureCollection getNetworkNodesAndSplitPoints() {
        return networkNodesAndSplitPoints;
    }
}
