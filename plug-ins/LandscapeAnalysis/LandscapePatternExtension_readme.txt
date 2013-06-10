Landscape Pattern Extension for OpenJUMP
=======================================

Version: 10.June.2013

(improvements to the 2009 version: fixed some bugs that prevented the calculation of single metrics)

Installation instructions:
-------------------------
1. put the *.jar file into OpenJUMP's lib/ext/ folder
2. Start OpenJUMP: 
	The functions should be under the menu item: >Plugins>LE-Patterns
3. no step three :)


contained functions:
-------------------
1)  Extract Core Edge And Patch
	The Function will classify patches and extracts edges and 
    	core area of (forest-)polyogns based on buffer operations.
	The given buffer distance will define the width of the edges.

2) Extract Corridors
	The function extracts corridors, branches, shortcut areas, and patches. Corridors 
    	are narrow areas within habitat polygons. Not yet implemented is the detection of shortcut-branches
    	and corridor branches (see Vogt et al. 2007, Eco. Indicators). The option 'remove small branches' 
    	eliminates those branches with an area smaller than a square with buffer-radius side length.

3) Characterize Polygons with Single Polygon Metrics
	Provides a set of metrics to describe a single polygon.
	a) x centroid 
	b) y centroid 
	c) polygon corners
	d) number of holes in polygon
	e) area
	f) perimeter
	g) fractal dimension
	h) shape index
	i) Shumm's Shape Index (MacEachren 1985, Geogr. Annaler)
	j) concavity
	k) compactness
	l) elongation of Minimum (Area) Bounding Rectangle
	m) orientation of Minimum (Area) Bounding Rectangle
	n) average orientation of sides (Duchene et al. ICA Gen. Workshop 2003)
	o) squareness of corners
	For a description of the metrics see Steiniger et al. (2008, Transactions in GIS)
	and Burghardt and Steiniger (2005, Int. Cartogr. Conf. La Coruna)

4) Characterize Polygons with Polygon Neighborhood Metrics
	Provides a set of neihborhood metrics to describe the neighborhood of single polygon.
	a) Number of Polygons in Buffer 
	b) Convex Hull area density
	c) Buffer Hull area density
	d) R-Index
	For a description of the metrics see Steiniger et al. (2008, Transactions in GIS)

Please note, that the function "Extract Core Edge And Patch" and "Extract Corridors" are also 
part of the OpenJUMP HoRAE Toolbox, see:
http://gisciencegroup.ucalgary.ca/wiki/OpenJUMP_HoRAE

Legal notice:
------------
The plugin is distributed under the GNU-GPL.
Copyright: Stefan Steiniger (so I can re-use it in a different context :)

Questions and remarks to: sstein@geo.uzh.ch
