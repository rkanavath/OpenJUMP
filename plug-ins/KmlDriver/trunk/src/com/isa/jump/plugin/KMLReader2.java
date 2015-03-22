/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI 
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * JUMP is Copyright (C) 2003 Vivid Solutions
 *
 * This program implements extensions to JUMP and is
 * Copyright (C) 2005 Integrated Systems Analysts, Inc.
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * 
 * For more information, contact:
 *
 * Integrated Systems Analysts, Inc.
 * 630C Anchors St., Suite 101
 * Fort Walton Beach, Florida
 * USA
 *
 * (850)862-7321
 */
package com.isa.jump.plugin;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.LineNumberReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.StringTokenizer;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateFilter;
import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.CoordinateSequenceFactory;
import com.vividsolutions.jts.geom.CoordinateSequences;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.PrecisionModel;
import com.vividsolutions.jts.io.gml2.GMLConstants;
import com.vividsolutions.jts.io.gml2.GMLHandler;
import com.vividsolutions.jump.coordsys.CoordinateSystem;
import com.vividsolutions.jump.coordsys.Radius;
import com.vividsolutions.jump.coordsys.Reprojector;
import com.vividsolutions.jump.coordsys.Spheroid;
import com.vividsolutions.jump.coordsys.impl.PredefinedCoordinateSystems;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.io.DriverProperties;
import com.vividsolutions.jump.io.GMLInputTemplate;
import com.vividsolutions.jump.io.IllegalParametersException;
import com.vividsolutions.jump.io.JUMPReader;
import com.vividsolutions.jump.io.JUMPWriter;
import com.vividsolutions.jump.io.ParseException;
import com.vividsolutions.jump.io.datasource.DelegatingCompressedFileHandler;
import com.vividsolutions.jump.io.datasource.StandardReaderWriterFileDataSource;

public class KMLReader2 extends DefaultHandler implements JUMPReader {

	private static final String folderElement = "Folder";
	private static final String placemarkElement = "Placemark";
	private static final String nameElement = "Name";
	private static final String descriptionElement = "Description";
	//private static final String simpleField = "SimpleField";
    List<Exception> exceptions = new ArrayList<Exception>();
	
	//private boolean inFolder = false;
	private boolean inPlacemark = false;

	//private CoordinateSystem destination = null;
	//private CoordinateSystem source = null;
	//private double centralMeridian = 0;
	//private int zoneInt = 0;
	//private boolean zoneSouth = false;
	//private String zoneStr = "";
	
	private String parentFolders;
	private String folderName;
	private String folderDesc;
	private String placemarkName;
	private String placemarkDesc;
	private Geometry geom;
	private Feature feature;
	private StringBuffer buffer;
    //private List geoms = new ArrayList();
	
	private GMLHandler currGeomHandler;
	private String lastEltName = null;
	private GeometryFactory fact = new GeometryFactory() {
	
	    public LinearRing createLinearRing(CoordinateSequence cs) {
		if (cs.getCoordinate(0).equals(cs.getCoordinate(cs.size() - 1))) 
			return super.createLinearRing(cs);
			
			// add a new coordinate to close the ring
			CoordinateSequenceFactory csFact = getCoordinateSequenceFactory();
			CoordinateSequence csNew = csFact.create(cs.size() + 1, cs.getDimension());
			CoordinateSequences.copy(cs, 0, csNew, 0, cs.size());
			CoordinateSequences.copyCoord(csNew, 0, csNew, csNew.size() - 1);
			return super.createLinearRing(csNew);
	    }
	
	};
	

	public KMLReader2() {
		super();
		xr = new org.apache.xerces.parsers.SAXParser();
		xr.setContentHandler(this);
		xr.setErrorHandler(this);
	}
	

	private static class ClassicReaderWriterFileDataSource extends StandardReaderWriterFileDataSource 
	{
		public ClassicReaderWriterFileDataSource
		(
				JUMPReader reader,
				JUMPWriter writer,
				String[] extensions) {
			super(new DelegatingCompressedFileHandler(reader, toEndings(extensions)), writer, extensions);
			this.extensions = extensions;
		}
	}

	public static class KML extends ClassicReaderWriterFileDataSource 
	{
		public KML() 
		{
			super(new KMLReader(false), new KMLWriter(), new String[] {"kml"});
		}
	}

	/*
	private GMLInputTemplate makeTemplate() {
		String geometryElement = "***";
		String s = "";
		s += "<?xml version='1.0' encoding='UTF-8'?>";
		s += "<JCSGMLInputTemplate>";
		s += ("<CollectionElement>" + collectionElement +
		"</CollectionElement>");
		s += ("<FeatureElement>" + featureElement + "</FeatureElement>");
		s += ("<GeometryElement>" + geometryElement + "</GeometryElement>");
		s += "<ColumnDefinitions>";

		s += "<column>";
		s += "<name>name</name>";
		s += "<type>STRING</type>";
		s += "<valueelement elementname=\"name\"/>";
		s += "<valuelocation position=\"body\"/>";
		s += "</column>";
		
		s += "</ColumnDefinitions>";

		s += "</JCSGMLInputTemplate>";

		GMLInputTemplate template = new GMLInputTemplate();
		StringReader sr = new StringReader(s);

		try {
			template.load(sr);
		}catch ( Exception ex) {
			ex.printStackTrace();
		} finally {
			sr.close();
		}

		return template;
	}
	*/

	/**
	 *  Main Entry - load in a GML file
	 *
	 *@param  dp                              Description of the Parameter
	 *@return                                 Description of the Return Value
	 *@exception  IllegalParametersException  Description of the Exception
	 *@exception  Exception                   Description of the Exception
	 */
	public FeatureCollection read(DriverProperties dp) 
	                              throws IllegalParametersException, Exception {
		
  		//source = PredefinedCoordinateSystems.GEOGRAPHICS_WGS_84;
  		//destination = null;
  		
		FeatureCollection fc;
		
		String inputFname;
//		boolean isCompressed;
//		isCompressed = (dp.getProperty("CompressedFile") != null);

		inputFname = dp.getProperty("File");

		if (inputFname == null) {
			inputFname = dp.getProperty("DefaultValue");
		}

		if (inputFname == null) {
			throw new IllegalParametersException(
					"call to GMLReader.read() has DataProperties w/o a InputFile specified");
		}

		java.io.Reader r;
		//GMLInputTemplate template = makeTemplate();
		//setInputTemplate(template);

//		if (isCompressed) {
//		r = new BufferedReader(new InputStreamReader(
//		CompressedFile.openFile(inputFname,
//		dp.getProperty("CompressedFile"))));
//		} else {
		r = new BufferedReader(new FileReader(inputFname));
//		}

		fc = read(r, inputFname);
		r.close();
		Envelope env = fc.getEnvelope();
		return fc;
	}
	
	private FeatureSchema getSchema() {
	    FeatureSchema schema = new FeatureSchema();
	    schema.addAttribute("GEOMETRY", AttributeType.GEOMETRY);
	    schema.addAttribute("PlacemarkName", AttributeType.STRING);
	    schema.addAttribute("PlacemarkDesc", AttributeType.STRING);
	    schema.addAttribute("FolderName", AttributeType.STRING);
	    schema.addAttribute("FolderDesc", AttributeType.STRING);
	    schema.addAttribute("ParentFolders", AttributeType.STRING);
	    return schema;
	}


	/**
	 *  STATE   MEANING <br>
	 *  0      Init <br>
	 *  1      Waiting for Collection tag <br>
	 *  2      Waiting for Feature tag <br>
	 *  3      Getting jcs columns <br>
	 *  4      Parsing geometry (goes back to state 3) <br>
	 *  1000   Parsing Multi-geometry, recursion level =1 <br>
	 *  1001   Parsing Multi-geometry, recursion level =2 <br>
	 */
	static int STATE_INIT = 0;
	static int STATE_PARSE_GEOM_NESTED = 1000;
	static int STATE_FOUND_FEATURE_TAG = 3;
	static int STATE_PARSE_GEOM_SIMPLE = 4;
	static int STATE_WAIT_COLLECTION_TAG = 1;
	static int STATE_WAIT_FEATURE_TAG = 2;
	GMLInputTemplate GMLinput = null;
	int STATE = STATE_INIT; //list of points
	Point apoint;
	Feature currentFeature;
	int currentGeometryNumb = 1;
	FeatureCollection fc;
	FeatureSchema fcmd; // list of geometries
	Geometry finalGeometry; //list of geometrycollections - list of list of geometry
	ArrayList geometry;
	GeometryFactory geometryFactory = new GeometryFactory(); //this might get replaced if there's an SRID change
	ArrayList innerBoundaries = new ArrayList();
	Attributes lastStartTag_atts;
	String lastStartTag_name;
	String lastStartTag_qName; //accumulate values inside a tag

	// info about the last start tag encountered
	String lastStartTag_uri;
	LineString lineString;
	LinearRing linearRing; // a LR
	LinearRing outerBoundary; //list of LinearRing
	ArrayList pointList = new ArrayList(); // list of accumulated points (Coordinate)
	Polygon polygon; // polygon

	// higherlevel geomery object
	ArrayList recursivegeometry = new ArrayList();

	// low-level geometry objects
	Coordinate singleCoordinate = new Coordinate();
	String streamName; //result geometry  -
	StringBuffer tagBody;
	XMLReader xr; //see above

	/*
	int SRID =0; // srid to give the created geometries
	public boolean parseSRID = false ; //true = put SRID for srsName="EPSG:42102"
	*/
	/**
	 * true => for 'OBJECT' types, if you find more than 1 item, make a list and store all the results
	 */
	 /*
	public boolean multiItemsAsLists = false;
    */
    
	/**
	 * parse SRID information in geometry tags
	 * @param parseTheSRID true = parse
	 */
	 /*
	public void acceptSRID(boolean parseTheSRID)
	{
		parseSRID =parseTheSRID;
	}

	public void processMultiItems(boolean accept)
	{
		multiItemsAsLists=accept;
	}
	*/

	/**
	 *  Attach a GMLInputTemplate information class.
	 *
	 *@param  template  The new inputTemplate value
	 */
	 /*
	public void setInputTemplate(GMLInputTemplate template) {
		GMLinput = template;
	}
	*/

	/**
	 *  SAX handler - store and accumulate tag bodies
	 *
	 *@param  ch                Description of the Parameter
	 *@param  start             Description of the Parameter
	 *@param  length            Description of the Parameter
	 *@exception  SAXException  Description of the Exception
	 */
    public void characters(char[] ch, int start, int length) throws SAXException {
        try {
            tagBody.append(ch,start,length);
            
            String lecture = new String(ch,start,length);
		    if(buffer != null) buffer.append(lecture);       
            
        } catch (Exception e) {
            throw new SAXException(e.getMessage());
        }
    }

	/**
	 *  SAX HANDLER - move to state 0
	 */
	public void endDocument() {
		System.out.println("End document");
		//STATE = STATE_INIT;
	}

	/**
	 * SAX handler - handle state information and transitions based on ending
	 * elements Most of the work of the parser is done here.
	 * @exception  SAXException  Description of the Exception
	 */
	 /*
	public void endElement(String uri, String name, String qName) throws SAXException {
		try {
			int index;

			if (STATE == STATE_INIT) {
				tagBody = new StringBuffer();
				return; //something wrong
			}

			if (STATE > STATE_FOUND_FEATURE_TAG) {
				if (isMultiGeometryTag(qName)) {
					if (STATE == STATE_PARSE_GEOM_NESTED) {
						STATE = STATE_PARSE_GEOM_SIMPLE; //finished - no action.  geometry is correct
					} else {
						//build the geometry that was in that collection
						Geometry g = geometryFactory.buildGeometry(geometry);
						geometry = (ArrayList) recursivegeometry.get(STATE -
								STATE_PARSE_GEOM_NESTED - 1);
						geometry.add(g);
						recursivegeometry.remove(STATE -
								STATE_PARSE_GEOM_NESTED);
						g = null;
						STATE--;
					}
				}

				//these correspond to <coord><X>0.0</X><Y>0.0</Y></coord>
				if (qName.compareToIgnoreCase("X") == 0) {
					singleCoordinate.x = (new Double(tagBody.toString())).doubleValue();
				} else if (qName.compareToIgnoreCase("Y") == 0) {
					singleCoordinate.y = (new Double(tagBody.toString())).doubleValue();
				} else if (qName.compareToIgnoreCase("Z") == 0) {
					singleCoordinate.z = (new Double(tagBody.toString())).doubleValue();
				} else if (qName.compareToIgnoreCase("COORD") == 0) {
					pointList.add(new Coordinate(singleCoordinate)); //remember it
				}
				// this corresponds to <gml:coordinates>1195156.78946687,382069.533723461</gml:coordinates>
				else if (qName.compareToIgnoreCase("COORDINATES") == 0) {
					//tagBody has a wack-load of points in it - we need
					// to parse them into the pointList list.
					// assume that the x,y,z coordinate are "," separated, and the points are " " separated
					parsePoints(tagBody.toString(), geometryFactory);
				} else if (qName.compareToIgnoreCase("linearring") == 0) {
					Coordinate[] c = new Coordinate[0];

					c = (Coordinate[]) pointList.toArray(c);

					//c= (Coordinate[])l;
					linearRing = geometryFactory.createLinearRing(c);
				} else if (qName.compareToIgnoreCase("outerBoundaryIs") == 0){
					outerBoundary = linearRing;
				} else if (qName.compareToIgnoreCase("innerBoundaryIs") == 0) {
					innerBoundaries.add(linearRing);
				} else if (qName.compareToIgnoreCase("polygon") == 0){
					//LinearRing[] lrs = new LinearRing[1];
					LinearRing[] lrs = new LinearRing[0];

					lrs = (LinearRing[]) innerBoundaries.toArray(lrs);
					polygon = geometryFactory.createPolygon(outerBoundary, lrs);
					geometry.add(polygon);
				} else if (qName.compareToIgnoreCase("linestring") == 0) {
					Coordinate[] c = new Coordinate[0];

					c = (Coordinate[]) pointList.toArray(c);

					lineString = geometryFactory.createLineString(c);
					geometry.add(lineString);
				} else if (qName.compareToIgnoreCase("point") == 0) {
					apoint = geometryFactory.createPoint((Coordinate) pointList.get(
							0));
					geometry.add(apoint);
				}
			} else if (STATE == STATE_FOUND_FEATURE_TAG) {
				if (qName.compareToIgnoreCase(featureElement) == 0) {
					tagBody = new StringBuffer();
					STATE = STATE_WAIT_FEATURE_TAG;

					//create a feature and put it inside the featurecollection
					if (currentFeature.getGeometry() == null) {
						Geometry g = currentFeature.getGeometry();

						if (g != null) {
							System.out.println(g.toString());
						}

						throw new ParseException(
								"no geometry specified in feature");
					}

					fc.add(currentFeature);
					currentFeature = null;

					return;
				} else {
					//check to see if this was a tag we want to store as a column
					//DB: added 2nd check for GML like <a><b></b></a>
					//     the "b" tag is the "lastStartTag_qName" for "</b>" and "</a>" we only need to
					//     process it once.
					try {
						if (   ((index = GMLinput.match(lastStartTag_qName,lastStartTag_atts)) > -1) &&
								(lastStartTag_qName.equalsIgnoreCase(qName))
						)
							currentFeature.setAttribute(GMLinput.columnName(index),
									GMLinput.getColumnValue(index, tagBody.toString(), lastStartTag_atts));
					} catch (Exception e) {
						//dont actually do anything with the parse problem - just ignore it,
						// we cannot send it back because the function its overiding doesnt allow
						e.printStackTrace();
					}

					tagBody = new StringBuffer();
				}
			} else if (STATE == STATE_WAIT_FEATURE_TAG) {
				if (qName.compareToIgnoreCase(collectionElement) == 0) {
					STATE = STATE_INIT; //finish

					//System.out.println("DONE!");
					tagBody = new StringBuffer();

					return;
				}
			} else if (STATE == STATE_WAIT_COLLECTION_TAG) {
				tagBody = new StringBuffer();

				return; //still look for start collection tag
			}
			if (isGeometryTag(qName)) { //Assume it was input correctly
				tagBody = new StringBuffer();
				STATE = STATE_FOUND_FEATURE_TAG;

				finalGeometry = geometryFactory.buildGeometry(geometry);
				
				reprojectGeometry(finalGeometry);
				
				//System.out.println("end geom: "+finalGeometry.toString() );
				currentFeature.setGeometry(finalGeometry);
				currentGeometryNumb++;

				return;
			}

		} catch (Exception e) {
			throw new SAXException(e.getMessage());
		}
	}
	*/

	public void error(SAXParseException exception) throws SAXException {
		throw exception;
	}

	public void fatalError(SAXParseException exception)
	throws SAXException {
		throw exception;
	}



	/**
	 *  Main function to read a GML file. You should have already called
	 *  setInputTempalate().
	 *
	 *@param  r              reader to read the GML File from
	 *@param  readerName     what to call the reader for error reporting
	 *@return                Description of the Return Value
	 *@exception  Exception  Description of the Exception
	 */
	public FeatureCollection read(java.io.Reader r, String readerName)
                                                              throws Exception {
		LineNumberReader myReader = new LineNumberReader(r);

		/*
		if (GMLinput == null) {
			throw new ParseException(
					"you must set the GMLinput template first!");
		}
		*/

		FeatureSchema schema = getSchema();
		streamName = readerName;

		fcmd = GMLinput.toFeatureSchema();
		fc = new FeatureDataset(fcmd);

		try {
			xr.parse(new InputSource(myReader));
		} catch (SAXParseException e) {
			throw new ParseException(e.getMessage() + "  Last Opened Tag: " +
					lastStartTag_qName + ".  Reader reports last line read as " +
					myReader.getLineNumber(),
					streamName + " - " + e.getPublicId() + " (" + e.getSystemId() +
					") ", e.getLineNumber(), e.getColumnNumber());
		} catch (SAXException e) {
			throw new ParseException(e.getMessage() + "  Last Opened Tag: " +
					lastStartTag_qName, streamName, myReader.getLineNumber(), 0);
		}

		return fc;
	}

	////////////////////////////////////////////////////////////////////
	// Event handlers.
	////////////////////////////////////////////////////////////////////

	/**
	 *  SAX handler - move to state 1
	 */
	public void startDocument() {
		//System.out.println("Start document");
		tagBody = new StringBuffer();
		//inFolder = false;
		inPlacemark = false;
	}
	
  /**
   *  SAX handler. Handle state and state transitions based on an element
   *  starting.
   *
   *@param  uri               Description of the Parameter
   *@param  name              Description of the Parameter
   *@param  qName             Description of the Parameter
   *@param  atts              Description of the Parameter
   *@exception  SAXException  Description of the Exception
   */
    public void startElement(String uri, String name, String qName, Attributes atts) 
                                                           throws SAXException {
        
        if (name.equalsIgnoreCase(nameElement)) {
            
        }
        if (name.equalsIgnoreCase(descriptionElement)) {
            
        }
        if (name.equalsIgnoreCase(folderElement)) {

        }
        if (name.equalsIgnoreCase(placemarkElement)) {
            inPlacemark = true;
        }
        //else if (name.equalsIgnoreCase(featureElement)) {
        //    
        //}
        FeatureSchema schema = new FeatureSchema();
        schema.addAttribute("GEOMETRY", AttributeType.GEOMETRY);
	    schema.addAttribute("PlacemarkName", AttributeType.STRING);
	    schema.addAttribute("PlacemarkDesc", AttributeType.STRING);
	    schema.addAttribute("FolderName", AttributeType.STRING);
	    schema.addAttribute("FolderDesc", AttributeType.STRING);
	    schema.addAttribute("ParentFolders", AttributeType.STRING);
	    if (name.equalsIgnoreCase(GMLConstants.GML_POLYGON)) {
			currGeomHandler = new GMLHandler(fact, null);
		}
		if (currGeomHandler != null) {
			currGeomHandler.startElement(uri, name, qName, atts);
		}
		if (currGeomHandler == null) {
			lastEltName = name;
		}
	}
  
	/*
    public void characters(char[] ch, int start, int length) throws SAXException {
        if (currGeomHandler != null) {
            currGeomHandler.characters(ch, start, length);
        }
        else {
            String content = new String(ch, start, length).trim();
    	    if (content.length() > 0) {
    		    System.out.println(lastEltName + "= " + content);
    	    }
        }
	}
	*/
	
	public void ignorableWhitespace(char[] ch, int start, int length) throws SAXException {
        if (currGeomHandler != null) {
    	    currGeomHandler.ignorableWhitespace(ch, start, length);
    	}
	}
	
    /**
     *  SAX handler - handle state information and transitions based on ending
     *  elements.
     *
     *@param  uri               Description of the Parameter
     *@param  name              Description of the Parameter
     *@param  qName             Description of the Parameter
     *@exception  SAXException  Description of the Exception
     */
    public void endElement(String uri, String name, String qName) throws SAXException {
        // System.out.println("/" + name);
        if (name.equalsIgnoreCase(nameElement)) {
            /*if (inPlaceholder)*/ feature.setAttribute("PlaceholderName", buffer);
        }
        if (name.equalsIgnoreCase(folderElement)) {
            parentFolders = null;
            folderName = null;
            folderDesc = null;
            placemarkName = null;
            placemarkDesc = null;
            //feature.add
        }
        else if (name.equalsIgnoreCase(placemarkElement)) {
            inPlacemark = false;
            feature.setAttribute("ParentFolders", parentFolders);
            feature.setAttribute("FolderName", folderName);
            feature.setAttribute("FolderDesc", folderDesc);
            feature.setAttribute("PlacemarkName", placemarkName);
            feature.setAttribute("PlacemarkDesc", placemarkDesc);
            feature.setAttribute("GEOMETRY", geometry);
        }
        if (currGeomHandler != null) {
            currGeomHandler.endElement(uri, name, qName);
            if (currGeomHandler.isGeometryComplete()) {
                //geometry = currGeomHandler.getGeometry();
                //System.out.println(g);
                //geoms.add(g);
				// reset to indicate no longer parsing geometry
				currGeomHandler = null;
			}
        }
    }
	

	/**
	 *  SAX handler. Handle state and state transitions based on an element
	 *  starting
	 *@exception  SAXException  Description of the Exception
	 */
	/*
	public void startElement(String uri, String name, String qName,
			Attributes atts) throws SAXException {
		try {
			//System.out.println("Start element: " + qName);
			tagBody = new StringBuffer();
			lastStartTag_uri = uri;
			lastStartTag_name = name;
			lastStartTag_qName = qName;
			lastStartTag_atts = atts;

			if (STATE == STATE_INIT) {
				return; //something wrong
			}

			if ((STATE == STATE_WAIT_COLLECTION_TAG) &&
					(qName.compareToIgnoreCase(collectionElement) == 0)) {
				//found the collection tag
				// System.out.println("found collection");
				STATE = STATE_WAIT_FEATURE_TAG;

				return;
			}

			if ((STATE == STATE_WAIT_FEATURE_TAG) &&
					(qName.compareToIgnoreCase(featureElement) == 0)) {
				//found the feature tag
				//System.out.println("found feature");
				currentFeature = new BasicFeature(fcmd);
				STATE = STATE_PARSE_GEOM_SIMPLE; //STATE_FOUND_FEATURE_TAG;
				recursivegeometry = new ArrayList();
				geometry = new ArrayList();
				recursivegeometry.add(geometry);
				finalGeometry = null;

				SRID = 0 ;// default SRID (reset for each feature, but should be constant for a featurecollection)
				if (geometryFactory.getSRID() != SRID)
					geometryFactory = new GeometryFactory(new PrecisionModel(), SRID);

				return;
			}

			if (parseSRID && (STATE >= STATE_PARSE_GEOM_SIMPLE) && isGeometryTag(qName) )
			{
				//System.out.println("src="+atts.getValue("srsName"));
				//System.out.println("srid="+ parseSRID(atts.getValue("srsName")));

				int newSRID =  parseSRID(atts.getValue("srsName"));
				//NOTE: if parseSRID it usually means that there was an error parsing
				//      but, it could actually be specified as 'EPGS:0'.  Thats not
				//      a problem because we've already defaulted to srid 0.
				if (newSRID != 0)
				{
					SRID = newSRID;
					if (geometryFactory.getSRID() != SRID)
						geometryFactory = new GeometryFactory(new PrecisionModel(), SRID);
				}
			}


			if ((STATE >= STATE_PARSE_GEOM_SIMPLE) &&
					((qName.compareToIgnoreCase("coord") == 0) ||
							(qName.compareToIgnoreCase("gml:coord") == 0))) {
				singleCoordinate.x = Double.NaN;
				singleCoordinate.y = Double.NaN;
				singleCoordinate.z = Double.NaN;
			}

			if ((STATE >= STATE_PARSE_GEOM_SIMPLE) &&
					(!((qName.compareToIgnoreCase("X") == 0) ||
							(qName.compareToIgnoreCase("y") == 0) ||
							(qName.compareToIgnoreCase("z") == 0) ||
							(qName.compareToIgnoreCase("coord") == 0)))) {
				pointList.clear(); //clear out any accumulated points
			}

			if ((STATE >= STATE_PARSE_GEOM_SIMPLE) &&
					((qName.compareToIgnoreCase("polygon") == 0))) {
				innerBoundaries.clear(); //polygon just started - clear out the last one
			}

			if ((STATE > STATE_FOUND_FEATURE_TAG) && (isMultiGeometryTag(qName))) {
				//in state 4 or a 1000 state and found a start GC (or Multi-geom) event
				if (STATE == STATE_PARSE_GEOM_SIMPLE) {
					// geometry already = recursivegeometry[0]
					STATE = STATE_PARSE_GEOM_NESTED;
				} else {
					STATE++;
					geometry = new ArrayList();
					recursivegeometry.add(geometry);
				}
			}
		} catch (Exception e) {
			throw new SAXException(e.getMessage());
		}
	}
	*/

	////////////////////////////////////////////////////////////////////
	// Error handlers.
	////////////////////////////////////////////////////////////////////
	public void warning(SAXParseException exception) throws SAXException {
		throw exception;
	}

    public Collection<Exception> getExceptions() {
        return exceptions;
    }

}

