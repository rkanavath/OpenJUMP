package ca.ucalgary.engg.moveantools.util;

import java.util.ArrayList;

/**
 * the class contains code for calculating the Brownian Bridge and is taken
 * from the R implementation of the adehabitat package. In particular the
 * class contains the code of the file c.test which has been adapted to work 
 * in Java. The original code was distributed with adehabitat version 1.8.3
 * 
 * The adehabitat code is distributed under the GPL license (>= 2). 
 * see also URL: https://www.faunalia.it/animove/trac/
 * 
 * @author Stefan Steiniger
 *
 */
public class BrownianBridgeUtil {

	/**
	 * calculates the bounding box extend based on the variance maximum. 
	 * note, sigma1 and sigma2 are not squared. [author: sstein]
	 * @param sigma1 
	 * @param sigma2
	 * @param dTmax the maximum time difference between points
	 */
	public static double calcMaxVarDist(double sigma1, double sigma2, double dTmax){
		//--- BBox diemensions of raster
		// if I calculated correctly the bounding box/extend should be based on 
		// the total variance, which is
		// var = var(alpha,Sigma1) + var(alpha,Sigma2)
		// var_max = T* 0.5 * sigma1^2 + sigma2^2
		// e.g.: for T=1, sigma1 = 200, and sigma2 = 20
		// var_max = 1*0.5*(200*200) + (20*20) 
		double varMax = dTmax * 0.5 * (sigma1 * sigma1) + (sigma2 * sigma2);  
		// since we work with sigma^2 get the root/stdv
		// take n-times the standard dev (for 5 times we have 99.999997 % of all points)
		double extend = 10.0*Math.sqrt(varMax);
		return extend;
	}
	
	//TODO: i am not sure why the array allocation starts from 1 usually instead of 
	//      and so it is not clear what the size of the arrays should be [sstein]

	/**
	 * calculates the PDF value for the position
	 * note, can return NaN
	 * @param x1 point x coord (i.e. cell)
	 * @param y1 point y coord (i.e. cell)
	 * @param moyx (mean) x coordinate of brownian bridge center
	 * @param moyy (mean) y coordinate of brownian bridge center
	 * @param var the variance
	 * @return the PDF value function can return NaN or zero : zero indicates a 
	 *         precision problem. To avoid the problem of returning zero one can
	 *         increase the time between points, e.g. by using milliseconds
	 *         instead of seconds. For this reason instead of dT = 1 I also used
	 *         10'000 secs.  
	 */
	public static double norm2d(double x1, double y1, double moyx, double moyy,
		    double var){
	    double cste = 0;
	    double dist = ((x1 - moyx) * (x1 - moyx)) + ((y1 - moyy) * (y1 - moyy));
	    cste = (1.0 / (2.0 * Math.PI * var)); // [sstein] - not sure but should be sqrt()?
	    double expn = Math.exp( (-1.0 * dist / (2.0 * var)) );
	    //if (exp == 0){
	    	//System.out.println("BrownianBridgeUtil.norm2d: precicision limit of e^x reached for value x: " + (-1.0 * dist / (2.0 * var)));
	    	
	    	// its a negative exponent, so we would have some very small values; i.e:
			// e^(RealBigNumber) = ReallyReallyBigNumer (e.g. e^709) = 8.2*10^307
			// e^(-RBN) = ReallyReallySmallNumber (e.g. e^-709) = 1.2*10^-308
			// below: - for the calcCVL() function this probably matters
	    	//        - for the integrno() it may not matter that much as we calculate sums????
	    //}
	    cste = cste * expn;
	    return cste;
	}

	/**
	 * calculates the maximum (window size) value h(sig1, sig2)
	 * function can be used to estimate the grid size
	 * @param sig1
	 * @param sig2
	 * @param numAlpha number of alpha values that are used for integration (i.e. 25 is used in the other code)
	 * @param maxt the maximal appearing time difference between two points in the dataset
	 * @return
	 */
	public static double maxh(double sig1, double sig2, int numAlpha, double maxt){
	    int na,i;
	    double res, tmp, a, alpha[];
	    
	    // Build the vector alpha
	    alpha = new double[numAlpha+1];
	    alpha[1] = 0;
	    for (i = 2; i <= numAlpha; i++) {
	    	alpha[i] = ((double) i) / ((double)numAlpha);
	    }
	    
	    double sig12 = sig1*sig1;
	    double sig22 = sig2*sig2;
	    res = 0;
	    na = alpha.length;
	    
	    for (i = 1; i < na; i++) {
			a = alpha[i];
			tmp = (maxt * a * (1 - a) * sig12) + ( ( (a*a) + ((1-a)*(1-a)) ) * sig22);
			if (tmp > res)
			    res = tmp;
	    }
	    return (Math.sqrt(res));
	}

	/**
	 * function that estimates the maximal time difference between subsequent points.
	 * this value may be used to retrieve maxh, i.e. the to estimate the grid size
	 * @param T
	 * @return
	 */
	public static double maxdt(double[] T)
	{
	    int i,nt;
	    double res;

	    res = 0;
	    nt = T.length;

	    for (i = 2; i <= nt; i++) {
			if ((T[i]-T[i-1]) > res)
			    res = (T[i]-T[i-1]);
	    }
	    return res;
	}
	
	/*
	// keeps all the steps for which at least one relocation is 
	//   available in the box [sstein] this is proably some R specific thing?
	private int consdanslabox(double[] Xg, double[][] xy, 
			  int nl, int[] indcons, double maxvh, int controlbox){
	    
		int i,k,cons;
	    double tmp1, tmp2, a, b;
	    // On a besoin d'une boucle sur les pas
	    k=0;
	    
	    for (i = 1; i<nl; i++) {

	    	cons = 0;

	    	if (xy[i][1] > (Xg[1] - (controlbox * maxvh)) ) {
	    		if (xy[i][1] < (Xg[1] + (controlbox * maxvh)) ) {
	    			if (xy[i][2] > (Xg[2] - (controlbox * maxvh)) ) {
	    				if (xy[i][2] < (Xg[2] + (controlbox * maxvh)) ) {
	    					cons = 1;
	    				}
	    			}
	    		}
	    	}
	    	if (xy[i+1][1] > (Xg[1] - (controlbox * maxvh)) ) {
	    		if (xy[i+1][1] < (Xg[1] + (controlbox * maxvh)) ) {
	    			if (xy[i+1][2] > (Xg[2] - (controlbox * maxvh)) ) {
	    				if (xy[i+1][2] < (Xg[2] + (controlbox * maxvh)) ) {
	    					cons = 1;
	    				}
	    			}
	    		}
	    	}

	    	if (cons == 0) {
	    		a = (xy[i+1][2] - xy[i][2]) / (xy[i+1][1] - xy[i][1]);
	    		b = xy[i+1][2] - a * xy[i+1][1];
	    		tmp1 = a * (Xg[1] - (controlbox * maxvh)) + b;
	    		tmp2 = a * (Xg[1] + (controlbox * maxvh)) + b;

	    		if (tmp1 <= (Xg[2] + (controlbox * maxvh))) {
	    			if (tmp1 >= (Xg[2] - (controlbox * maxvh))) {
	    				cons = 1;
	    			}
	    		}

	    		if (tmp2 <= (Xg[2] + (controlbox * maxvh))) {
	    			if (tmp2 >= (Xg[2] - (controlbox * maxvh))) {
	    				cons = 1;
	    			}
	    		}
	    	}


	    	if (cons==1) {
	    		k++;
	    		indcons[k]=i;
	    	}

	    }

	    return k;
	}
	*/

	/**
	 * Integral of norm2d on alpha (i.e. the calculates the density),  
	 * @param XG a grid cell
	 * @param X1 point p1 of the segment
	 * @param X2 point p2 of the segment
	 * @param T time/weight - ensure that the time (i.e. T = dt/T = timeInterval/TotalTime) is positive
	 * @param sig1 - i.e. sigma1^2
	 * @param sig2 - i.e. sigma2^2
	 * @param alpha
	 * @return
	 */
	public static double integrno(double[] XG, double[] X1, double[] X2, 
		      double T, double sig1, double sig2, double[] alpha){
	    // Declaration
	    int i, na;
	    double val[], XX[], var, nx1, ny1, nx2, ny2, ny, moyx, moyy, al;
	    double result = 0;
	    XX = new double[3];
	    val = new double[alpha.length+1];
	    // Memory allocation
	    na = alpha.length-1;
	    //vecalloc(&val, na);
	    //vecalloc(&XX, 2);
	    
	    XX[1] = X2[1] - X1[1];
	    XX[2] = X2[2] - X1[2];
	    
	    
	    // loop for the computation of the value
	    //double[] varTrace = new double[na];  
	    //double[] varOneTrace = new double[na]; double[] varTwoTrace = new double[na]; 
	    double varOne, varTwo;
	    boolean precisionLimitReported = false;
	    boolean nanReported = false;
	    for (i = 1; i<= na; i++) {
			al = alpha[i];
			
			//-- Horne et al (2007) : Eq 5: 
			//   sigma^2(t) = T*alpha*(1-alpha)*sigma_m^2 
			//	            + (1-alpha)^2 * delta_a^2 + alpha^2 * delta_b^2;   
			//   here: delta_a = delta_b
			//         alpha is supposed to be: al=t/T, T = TotalTimeI? //see also the calcCVL function
			varOne = (T) * al * (1.0 - al) * (sig1); 
			varTwo = (((al * al) + ((1.0 - al) * (1.0 - al))) * (sig2));
			var = varOne + varTwo;
			//varTrace[i-1] = var; varOneTrace[i-1] = varOne; varTwoTrace[i-1] = varTwo;
			moyx = X1[1] + al * XX[1];
			moyy = X1[2] + al * XX[2];
			
			double tmp = norm2d(XG[1], XG[2], moyx, moyy, var);
			if(tmp == 0){
				//--DON'T do the approximation, values are too different and..
				//  I get Infinity results per cell for the raster BB
				//  and NaN values for the segmentBB
				//double tmpApprox = norm2dApprox(XG[1], XG[2], moyx, moyy, var);
				//tmp = tmpApprox;
				/*
				if(precisionLimitReported == false){ // to avoid unnecessary outputs
					System.out.println("BrownianBridgeUtil: norm2d precicision limit of e^x reached, i.e. e^x=0; using approximation value:" + tmpApprox);
					precisionLimitReported = true;
				}
				*/
			}
			if(Double.isNaN(tmp)){
				tmp = 0;
				/*
				if(nanReported == false){ // to avoid unnecessary outputs
					System.out.println("BrownianBridgeUtil: norm2d returned NaN");
					nanReported = true;
				}
				*/
			}
			val[i] = tmp;
	    }
	    int huhu = 1+1; huhu++; //this is just to have a stop for debugging
	    // loop for the computation of the integral
	    for (i = 2; i<= na; i++) {
			nx1 = alpha[i-1];
			ny1 = val[i-1];
			nx2 = alpha[i];
			ny2 = val[i];
			ny = ny1;
			if (ny2 <= ny1)
			    ny = ny2;
				result = result + (nx2 - nx1) * (ny + (Math.abs(ny2 - ny1) / 2.0));
	    }
	    
	    // Free memory
	    //freevec(val);
	    //freevec(XX);
	    return result;
	}


	/**
	 * Computes UD at a node of the grid
	 * @param XG a grid cell
	 * @param XY the vector of all observations/locations
	 * @param T the time for each of the locations
	 * @param sig1 - sigma1^2 - related to speed of animals
	 * @param sig2 - sigma2^2 - related to the imprecision of the relocations
	 * @param alpha - vector for the integration (sub intervals along the travel line)  
	 * @param ncons - #locations-1
	 * @param indcons - array that contains the location indices [1..#locations]
	 * @return
	 */
	public static double udbbnoeud(double[] XG, double[][] XY, double[] T, double sig1,
		       double sig2, double[] alpha, int ncons, int[] indcons){
	    /* Declaration */
	    int i, nlo;
	    double Xtmp1[], Xtmp2[], dt, poids, dttot;
	    double result = 0;
	    Xtmp1 = new double[3]; Xtmp2 = new double[3];
	    /* Memory allocation */
	    //vecalloc(&Xtmp1, 2);
	    //vecalloc(&Xtmp2, 2);
	    nlo = XY.length-1;
	    dttot = T[nlo] - T[1];
	    
	    /* for each step */
	    for (i = 1; i <= ncons; i++) {
		
			/* Computes weights and time lags */
			dt = T[indcons[i]+1] - T[indcons[i]];
			//[sstein] ensure we have a positive dime difference
			dt = Math.abs(dt);
			poids = dt / dttot;
			if(Double.isInfinite(poids)){ //division by zero
				poids = 0;
			}
			if(Double.isNaN(poids)){
				poids = 0;
			}
			/* Output of the relocation values at i, and use of the function integrno */
			Xtmp1[1] = XY[indcons[i]][1];
			Xtmp1[2] = XY[indcons[i]][2];
			Xtmp2[1] = XY[indcons[i]+1][1];
			Xtmp2[2] = XY[indcons[i]+1][2];
			//					  cell, loc1, loc2, timediff, sig1,sig2, vector of substeps  
			double tmp = integrno(XG, Xtmp1, Xtmp2, dt, sig1, sig2, alpha);
			if(Double.isNaN(tmp)){
				String s ="time to stop";
			}
			result = result + (poids * tmp); // add (weighted) cell-values over all locations
	    }
	    return result;
	}


	/**
	 * main function to calculated the BB in R/C
	 * @param xgri x grid values
	 * @param ygri y grid values
	 * @param ncolgri number of columns of the grid
	 * @param nliggri number of lines/rows of the grid
	 * @param nloc number of locations
	 * @param sig1 - sigma1^2 -	first smoothing parameter for the brownian bridge 
	 * 		method (related to the speed of the animals; it can be estimated 
	 * 		by the function liker).
	 * @param sig2 - sigma2^2 - second smoothing parameter for the brownian bridge 
	 * 		method (related to the imprecision of the relocations, supposed known).
	 * @param xlo - x of locations
	 * @param ylo - y of locations
	 * @param Tr - date/time as double [in seconds?]
	 * @param nalpha (i.e. set 25 as default) - a parameter used internally to 
	 * 		compute the integral of the Brownian bridge. The integral is computed by 
	 * 		cutting each step built by two relocations into nalpha sub-intervals. 
	 */
	public static double[][] kernelbb(double[] xgri, double[] ygri, int ncolgri,
		      int nliggri, int nloc, double sig1, double sig2, 
		      double[] xlo, double[] ylo, double[] Tr, 
		      int nalpha){
		
	    // Declaration 
	    int i, j, k, ncg, nlg, nlo, indcons[], ncons;
	    double gri[][], xg[], yg[], XY[][], alpha[], Xgr[], T[], res, vol;
	    //double maxt, maxvh;
	    
	    /* Memory Allocation */
	    ncg = ncolgri;
	    nlg = nliggri;
	    nlo = nloc;
	    
	    //taballoc(&gri,nlg, ncg);
	    //taballoc(&XY, nlo, 2);
	    //vecalloc(&xg, nlg);
	    //vecalloc(&T, nlo);
	    //vecalloc(&yg, ncg);
	    //vecalloc(&Xgr, 2);
	    //vecalloc(&alpha, *nalpha);
	    //vecintalloc(&indcons, nlo);
	    
	    // R to C    
	    XY = new double[nlo][3];
	    T = new double[nlo+1];
	    for (i=1; i<=nlo; i++) {
			XY[i][1] = xlo[i-1];
			XY[i][2] = ylo[i-1];
			T[i] = Tr[i-1];
	    }
	    
	    xg = new double[nlg+1];
	    for (i=1; i<=nlg; i++) {
	    	xg[i] = xgri[i-1];
	    }
	    
	    yg = new double[ncg+1];
	    for (i=1; i<=ncg; i++) {
	    	yg[i] = ygri[i-1];
	    }
	    
	    // Build the vector alpha
	    alpha = new double[nalpha+1];
	    alpha[1] = 0;
	    for (i = 2; i <= nalpha; i++) {
	    	alpha[i] = ((double) i) / ((double)nalpha);
	    }
	    
	    //-- Maximum dt and sigma for the normal distribution
	    //   [sstein] not sure why this is calculated but never used
	    //maxt = maxdt(T);
	    //maxvh = maxh(sig1, sig2, alpha, maxt);
	    
		
	    // Loop on the grid
	    vol = 0;
	    Xgr = new double[3];
	    indcons = new int[nlo];
	    gri = new double[nlg+1][ncg+1];
	    
	    res = xg[2] - xg[1];
	    for (i=1; i<=nlg; i++) {
			for (j=1; j<=ncg; j++) {
			    Xgr[1] = xg[i];
			    Xgr[2] = yg[j];
			    /*
			      ncons = consdanslabox(Xgr, XY, nlo, indcons, maxvh, *controlbox);
			    */
		    	ncons = nlo-1;
			    for (k = 1; k < nlo; k++){ //[sstein] its not clear to me what this loop would do?
					indcons[k]=k;
				    double tmp = udbbnoeud(Xgr, XY, T, sig1, sig2, alpha, ncons, indcons);
				    gri[i][j] = tmp;
				    vol+=tmp;
			    }
			}
	    }
	    
	    // Standardization of the volume 

	    for (i=1; i<=nlg; i++) {
			for (j=1; j<=ncg; j++) {
			    gri[i][j] =  gri[i][j] / (vol * Math.pow(res,2));
			}
	    }
	    
	    // Free memory
	    //freetab(gri);
	    //freevec(xg);
	    //freevec(yg);
	    //freevec(T);
	    //freetab(XY);
	    //freevec(Xgr);
	    //freevec(alpha);
	    //freeintvec(indcons);
	    return gri;
	}

	/* *********************************************************************
	   Maximisation of the likelihood for the Brownian bridge
	   (R adehabitat code modified)
	   *********************************************************************/
	
	/**
	 * this is the CVL function from tests.c. It is a bit similar to BBLikelihoodVar
	 * in the Mod1Minimization.bas by Horne, Garton et al (2007). However, while here we will take the min 
	 * value for all calculations later, Horne, Garton et al (2007) chose the value
	 * using the golden section optimization approach. 
	 * @param xy
	 * @param Tr date
	 * @param nloc number of loactions
	 * @param sigmas to test
	 * @param nsig number of sigma to test
	 * @param sigma2 sigma2^2
	 * @return
	 */
	public static double[] calcCVL(double[][] xy, double Tr[], 
		 int nloc, double[] sigma, int nsig, double sigma2){
	    
		int i, j, k, r;
	    double /*xy[][],*/ T[], ai, mui[], sigmai;
	    
	    int nlo = nloc;
	    int ns = nsig;
	    //double s2 = sigma2;
	    
	    /*//-- memory alloc
	    taballoc(&xy, nlo, 2);
	    vecalloc(&T, nlo);
	    vecalloc(&mui, 2);
	    */
	    //replace this by...
	    //xy = new double[nlo+1][3];
	    T = new double[nlo+1];
	    // C to R
	    //k = 0;
	    for (i=1; i <= nlo; i++) {
	    	/* dont need this anymore
			for (j = 1; j <= 2; j++) {
			    xy[i][j] = xyr[k];
			    k++;
			}
			*/
			T[i] = Tr[i-1]; //not sure if I may mess-up the indices here
	    }
	    
	    mui = new double[3];
	    double[] Lr = new double[ns]; 
	    // Indices of odd locations
    	//ArrayList<Double> objectValsPrev = null; //used for debugging
	    for (r = 1; r <= ns; r++) {
	    	/*
			if(sigma[r-1] > 0){//for debugging
				System.out.println("lets start debugging here: sigma[r-1] = " + sigma[r-1]);
			}
			*/
	    	Lr[r-1] = 0;
	    	k=1;
	    	//ArrayList<Double> objectVals = new ArrayList<Double>(); //used for debugging
	    	for (i=1; i < nlo; i++) {//loop over all points
	    		if (k == 2) {
	    			ai = Math.abs(T[i] - T[i-1])/ Math.abs(T[i+1] - T[i-1]); //[sstein] added Math.abs to ensure positive difference
	    			if(Double.isInfinite(ai)){
	    				ai = 0.0; //was a division by zero
	    			}
	    			//         since this would otherwise screw up 
	    			//         the calculation of sigma
	    			mui[1] = xy[i-1][1] + ai * (xy[i+1][1] - xy[i-1][1]);
	    			mui[2] = xy[i-1][2] + ai * (xy[i+1][2] - xy[i-1][2]);
	    			double Ttemp = Math.abs(T[i+1]-T[i-1]); // ensure positive dt
	    			//			T*alpha*(alpha-1)*sigma
	    			sigmai = (Ttemp * ai * (1-ai) * sigma[r-1]) + (Math.pow((1 - ai),2) * (sigma2)) + (Math.pow(ai,2) * (sigma2));

	    			double res = norm2d(xy[i][1], xy[i][2], mui[1], mui[2], sigmai);
	    			double logres = Math.log(res);
	    			if((res == 0) || (Double.isNaN(res)) || (Double.isNaN(logres))){
	    				//--not sure what to do yet.
	    				//  sometimes logres is NaN
	    				if(Double.isNaN(logres) || (Double.isInfinite(logres))){
	    					logres = 0.0;
	    				}
	    				//  log(0) = infinity : i.e. a really large number
	    				//  if we have a zero value, it means we have one value less
	    				//  in the list.. at somepoint the value of a certain position
	    				//  will not be zero anymore and we will obtain a jump in the 
	    				//  sum value, i.e. Lr (see the DisplayLikerFunctionForBBPlugIn)
	    				if(res == 0){
	    					//use approximation
	    					// mhm.. some of the results for the BBRastervsersion don't look so good - 
	    					// so maybe don't use the approximiation?
	    					/*
	    					double resApprox = norm2dApprox(xy[i][1], xy[i][2], mui[1], mui[2], sigmai);
	    	    			double logAppRes = Math.log(resApprox);
	    					System.out.println("BrownianBridgeUtil: norm2d precicision limit reached, i.e. e^x=0; " +
	    							"for sigma1: " + Math.sqrt(sigma[r-1]) +  "; using approximation value: " + resApprox + ", log(res):" + logAppRes);
	    					if(Double.isNaN(res)){
	    						//don't assign new value, we go with the zero value
	    					}
	    					else{
	    						res = resApprox;
	    					}
	    					if(Double.isNaN(logAppRes)){
	    						//don't assign new value, we go with the zero value
	    					}
	    					else{
	    						logres = logAppRes;
	    					}
	    					*/
	    				}
	    				//System.out.println("BrownianBridgeUtil.calcCVL: is NaN");
	    			}
	    			Lr[r-1] = Lr[r-1] + logres;
	    			//Lr[r-1] = Lr[r-1] + res;
	    			//objectVals.add(logres); //used for debugging	
	    			k=1;
	    		}
	    		k++;
	    	}
	    	Lr[r-1] = -1 * Lr[r-1] ; //added this line from Horne and Garton et al (2007) code 
	    							 // hence we will obtain a function to minimize
	    							 // instead of searching for max value
	    	//objectValsPrev = objectVals; //used for debugging
	    }

	    /*
	    // Free memory
	    freetab(xy);
	    freevec(T);
	    freevec(mui);
	    */
	    return Lr;
	}
	
	/**
	 * Raise a double to a positive integer power.
	 * Fast version of Math.pow.
	 * @param x number to be taken to a power.
	 * @param n power to take x to. 0 <= n <= Integer.MAX_VALUE
	 * Negative numbers will be treated as unsigned positives.
	 * @return x to the power n
	 * @author Patricia Shanahan pats@acm.org
	 */
	public static double power( double x , int n )
	{
		int bitMask = n;
		double evenPower = x;
		double result;
		if ( (bitMask & 1) != 0 )
		{
			result = x;
		}
		else
		{
			result = 1;
		}
		bitMask >>>= 1;
			while ( bitMask != 0 )
			{
				evenPower *= evenPower;
				if ( (bitMask & 1) != 0 )
				{
					result *= evenPower;
				}
				bitMask >>>= 1;
			} // end while
				return result;
	} // end power

	/**
	 * calculates e^x in a fast way - but only approximates values. Errors may be up to 25 percent
	 * But probably better to have a value than having no value???
	 * Martin Ankerl;<br>
	 * http://firstclassthoughts.co.uk/misc/optimized_power_method_for_java_and_c_and_cpp.html
	 * @param val
	 * @return
	 */
	public static double exp(double val) {
	    final long tmp = (long) (1512775 * val + (1072693248 - 60801));
	    return Double.longBitsToDouble(tmp << 32);
	}
	
	/**
	 * calculates the PDF value for the position uses a fast approximation of exp calculation
	 * note, can return NaN. Note - when I use this I got problems with the BB output,
	 * either delivering NaN (for SegmentBB) or Infnity (RasterBB)
	 * @param x1 point x coord (i.e. cell)
	 * @param y1 point y coord (i.e. cell)
	 * @param moyx (mean) x coordinate of brownian bridge center
	 * @param moyy (mean) y coordinate of brownian bridge center
	 * @param var the variance
	 * @return the PDF value function can return NaN or zero : zero indicates a 
	 *         precision problem. To avoid the problem of returning zero one can
	 *         increase the time between points, e.g. by using milliseconds
	 *         instead of seconds. For this reason instead of dT = 1 I also used
	 *         10'000 secs.  
	 */
	public static double norm2dApprox(double x1, double y1, double moyx, double moyy,
		    double var){
	    double cste = 0;
	    double dist = ((x1 - moyx) * (x1 - moyx)) + ((y1 - moyy) * (y1 - moyy));
	    cste = (1.0 / (2.0 * Math.PI * var)); // [sstein] - not sure but should be sqrt()?
	    double expv = exp( (-1.0 * dist / (2.0 * var)) );
	    cste = cste * expv;
	    return cste;
	}
}
