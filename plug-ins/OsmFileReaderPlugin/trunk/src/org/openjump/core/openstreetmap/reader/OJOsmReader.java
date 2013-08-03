/*
 *  License: GPL. See LICENSE file for details.
 *  Adapted from JOSM by Stefan Steiniger for use in OpenJUMP [25.July.2013] 
 */
package org.openjump.core.openstreetmap.reader;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.openjump.core.geomutils.algorithm.GeometryConverter;
import org.openjump.core.openstreetmap.model.OjOsmNode;
import org.openjump.core.openstreetmap.model.OjOsmPrimitive;
import org.openjump.core.openstreetmap.model.OjOsmRelation;
import org.openjump.core.openstreetmap.model.OjOsmRelationMember;
import org.openjump.core.openstreetmap.model.OjOsmWay;
import org.openjump.core.openstreetmap.model.Tagged;
import org.openjump.core.openstreetmap.model.User;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.JUMPWorkbench;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.WorkbenchFrame;



/**
 * Parser for the Osm Api. Read from an input stream and construct a dataset out of it.
 *
 * For each xml element, there is a dedicated method.
 * The <code>XMLStreamReader</code> cursor points to the start of the element, when the method is
 * entered, and it must point to the end of the same element, when it is exited.
 * 
 * This class contains methods taken from JOSM's <code>org.openstreetmap.josm.io.OsmReader</code> class.
 * 
 * @author: sstein
 * @date: 5.July.2013
 */
public class OJOsmReader {

    protected XMLStreamReader parser;
    private ArrayList<OjOsmPrimitive> dataset = null;
    HashMap<Long, OjOsmNode> allNodes = new HashMap<Long, OjOsmNode>();
    HashMap<Long, OjOsmWay> allWays = new HashMap<Long, OjOsmWay>();
    HashMap<Long, OjOsmRelation> allRelations = new HashMap<Long, OjOsmRelation>();
    Envelope osmFileEnvelope = null;

	/**
     * constructor (for private and subclasses use only)
     *
     * @see #doParseDataSet(InputStream)
     */
    public OJOsmReader() {
    }
    
    public ArrayList<OjOsmPrimitive> getDataset() {
		return dataset;
	}
    
	public Envelope getOsmFileEnvelope() {
		return osmFileEnvelope;
	}
    
    protected void setParser(XMLStreamReader parser) {
        this.parser = parser;
    }
	
    protected void throwException(String msg) throws XMLStreamException {
        throw new OsmParsingException(msg, parser.getLocation());
    }
    
    /**
     * Does initiate the parsing process and does a bit of post processing,
     * such as creating the way geometries. However, the parsing itself is
     * done in <code>#parseOSM()</code> via <code>#parse()</code> and <code>#parseRoot()</code>.
     * @param source
     * @param monitor for reporting back progress to GUI. Can be null.
     * @param context for reporting to log. Can be null.
     * @return
     * @throws IllegalDataException
     * @see #parse()
     * @see #parseOSM()
     */
    public boolean doParseDataSet(InputStream source, TaskMonitor monitor) throws IllegalDataException {
    	if(source == null) return false;
    	JUMPWorkbench.getInstance().getFrame().log("OJOsmReader.doParseDataSet: start parsing File");
        try {
            InputStreamReader ir = UTFInputStreamReader.create(source, "UTF-8");
            XMLStreamReader parser = XMLInputFactory.newInstance().createXMLStreamReader(ir);
            setParser(parser);
            if(monitor != null){monitor.report("parsing OSM file");}
            parse();
            if(monitor != null){monitor.report("finished parsing");}
        	System.out.println("OJOsmReader.doParseDataSet: finished parsing");
            //prepareDataSet(); // [sstein] : not needed, calls originally dataset assembling methods 
            					//  from org.openstreetmap.josm.io.AbstractReader
            
            //add only nodes that have tags/keys (except for the create_by key)
            //TODO: check/output if there are points that are not used in either a way or a relation
            Iterator<Long> keySetIterator = this.allNodes.keySet().iterator();
            int nodeCount = 0; int maxNodeCount = this.allNodes.size();
            while(keySetIterator.hasNext()){
            	if(monitor != null){
            		monitor.report(nodeCount, maxNodeCount, "Copying nodes with tags.");
            	}
            	Long key = keySetIterator.next();
            	OjOsmNode tempn = this.allNodes.get(key);
            	if(tempn.hasKeys()){
            		// exclude those nodes that have only the "created_by" tag
            		if((tempn.getKeys().size() == 1) && (tempn.hasKey("created_by"))){
            		  // discard
            		}
            		else{
            		  this.dataset.add(tempn);
            		}
            	}
            	nodeCount++;
            }
            //assemble way and relation geometries
            JUMPWorkbench.getInstance().getFrame().log("OJOsmReader.doParseDataSet: checking for invalid node geoms");
        	checkForInvalidNodeGeoms(this.allNodes, monitor);
        	JUMPWorkbench.getInstance().getFrame().log("OJOsmReader.doParseDataSet: assembling way geometries");
            this.createWayGeoms(monitor);
            checkForInvalidWayGeoms(this.allWays, monitor);
            //we can assembley the relations only if have the ways and nodes
            JUMPWorkbench.getInstance().getFrame().log("OJOsmReader.doParseDataSet: assembling relation geometries");
        	this.createRelationGeometries(monitor);
        	JUMPWorkbench.getInstance().getFrame().log("OJOsmReader.doParseDataSet: adding ways and relations to output");
            //add all ways
            if(monitor != null){monitor.report("adding OSM ways to output");}
            Iterator<Long> keySetIterator2 = this.allWays.keySet().iterator();
            while(keySetIterator2.hasNext()){
              Long key = keySetIterator2.next();
              this.dataset.add(this.allWays.get(key));
            }
            //add all relations
            if(monitor != null){monitor.report("adding OSM relations to output");}
            Iterator<Long> keySetIterator3 = this.allRelations.keySet().iterator();
            while(keySetIterator3.hasNext()){
              Long key = keySetIterator3.next();
              this.dataset.add(this.allRelations.get(key));
            }
            //identify major land uses for items in the dataset only
            JUMPWorkbench.getInstance().getFrame().log("OJOsmReader.doParseDataSet: detecting mayor landuses");
            if(monitor != null){monitor.report("detecting OSM object landuse");}
            this.detectMayorLanduses(dataset);
            
            return true;
        } catch(OsmParsingException e) {
            throw new IllegalDataException(e.getMessage(), e);
        } catch(XMLStreamException e) {
            String msg = e.getMessage();
            Pattern p = Pattern.compile("Message: (.+)");
            Matcher m = p.matcher(msg);
            if (m.find()) {
                msg = m.group(1);
            }
            if (e.getLocation() != null)
                throw new IllegalDataException("Line " + e.getLocation().getLineNumber() + ", column " + e.getLocation().getColumnNumber() + " : " + msg, e);
            else
                throw new IllegalDataException(msg, e);
        } catch(Exception e) {
            throw new IllegalDataException(e);
        }
    }	

	protected void parse() throws XMLStreamException {
        int event = parser.getEventType();
        while (true) {
            if (event == XMLStreamConstants.START_ELEMENT) {
                parseRoot();
            } else if (event == XMLStreamConstants.END_ELEMENT)
                return;
            if (parser.hasNext()) {
                event = parser.next();
            } else {
                break;
            }
        }
        parser.close();
    }

    protected void parseRoot() throws XMLStreamException {
        if (parser.getLocalName().equals("osm")) {
            parseOsm();
        } else {
            parseUnknown();
        }
    }
    
    protected void parseUnknown(boolean printWarning) throws XMLStreamException {
        if (printWarning) {
        	JUMPWorkbench.getInstance().getFrame().log("Undefined element <" + parser.getLocalName() + "> found in input stream. Skipping.");
        }
        while (true) {
            int event = parser.next();
            if (event == XMLStreamConstants.START_ELEMENT) {
                parseUnknown(false); /* no more warning for inner elements */
            } else if (event == XMLStreamConstants.END_ELEMENT)
                return;
        }
    }

    protected void parseUnknown() throws XMLStreamException {
        parseUnknown(true);
    }

    /**
     * This method does all the main parsing and primitive type 
     * identification work. It parses each tag of the file and checks 
     * if the tag indicates: (i) bounds, (ii) nodes, (iii) ways, and 
     * (vi) relations.
     * TODO: So far, parsing/assembling of changesets was not transfered from 
     * JOSM OsmReader.java
     * @throws XMLStreamException
     */
    private void parseOsm() throws XMLStreamException {
    	this.dataset = new ArrayList<OjOsmPrimitive>();
    	
        String v = parser.getAttributeValue(null, "version");
        if (v == null) {
            throwException("Missing mandatory attribute ''version''.");
        }
        if (!(v.equals("0.5") || v.equals("0.6"))) {
            throwException("Unsupported version: " + v);
        }
        //ds.setVersion(v);
        String upload = parser.getAttributeValue(null, "upload");
        if (upload != null) {
            //ds.setUploadDiscouraged(!Boolean.parseBoolean(upload));
        }
        String generator = parser.getAttributeValue(null, "generator");
        Long uploadChangesetId = null;
        if (parser.getAttributeValue(null, "upload-changeset") != null) {
            uploadChangesetId = getLong("upload-changeset");
        }
        while (true) {
            int event = parser.next();
            
            /*
            if (cancel) {
                cancel = false;
                throwException("Reading was canceled");
            }
            */
            if (event == XMLStreamConstants.START_ELEMENT) {
                if (parser.getLocalName().equals("bounds")) {
                    this.osmFileEnvelope = parseBounds(generator); //[sstein] don't really need the bounds for OpenJUMP FeatureDatasets
                    						// but we may parse them as well to have an envelope
                } else if (parser.getLocalName().equals("node")) {
                    OjOsmNode node = parseNode();
                    this.allNodes.put(node.getId(), node);
                } else if (parser.getLocalName().equals("way")) {
                    OjOsmWay way = parseWay();
                    this.allWays.put(way.getId(), way);
                } else if (parser.getLocalName().equals("relation")) {//TODO: keep working/debugging from here on
                    OjOsmRelation relation = parseRelation();
                    this.allRelations.put(relation.getId(), relation);
                } else if (parser.getLocalName().equals("changeset")) {
                    //parseChangeset(uploadChangesetId);
                } else {
                    parseUnknown();
                }
            } else if (event == XMLStreamConstants.END_ELEMENT)
                return;
        }
    }
    
    /**
     * 
     * @param generator
     * @return file bounds as JTS Envelope in degrees. Can be NULL. 
     * @throws XMLStreamException
     */
    private Envelope parseBounds(String generator) throws XMLStreamException {
    	Envelope env = null;
        String minlon = parser.getAttributeValue(null, "minlon");
        String minlat = parser.getAttributeValue(null, "minlat");
        String maxlon = parser.getAttributeValue(null, "maxlon");
        String maxlat = parser.getAttributeValue(null, "maxlat");
        String origin = parser.getAttributeValue(null, "origin");
        if (minlon != null && maxlon != null && minlat != null && maxlat != null) {
            if (origin == null) {
                origin = generator;
            }
            double miny = Double.parseDouble(minlat);
            double maxy = Double.parseDouble(maxlat);
            double minx = Double.parseDouble(minlon);
            double maxx = Double.parseDouble(maxlon);
            env = new Envelope(minx, maxx, miny, maxy);
            /*
            Bounds bounds = new Bounds(
                    Double.parseDouble(minlat), Double.parseDouble(minlon),
                    Double.parseDouble(maxlat), Double.parseDouble(maxlon));
//            if (bounds.isOutOfTheWorld()) { // [sstein] removed #.isOutOfTheWorld() method as it requires the projection
//                Bounds copy = new Bounds(bounds);
//                bounds.normalize();
//                System.out.println("Bbox " + copy + " is out of the world, normalized to " + bounds);
//            }
            DataSource src = new DataSource(bounds, origin);
            ds.dataSources.add(src);
            */
        } else {
            throwException(
                    "Missing mandatory attributes on element ''bounds''. Got minlon=" + minlon + 
                    ", minlat=" + minlat + 
                    ", maxlon=" + maxlon + 
                    ", maxlat=" + maxlat +
                    ", origin=" + origin + "."
            );
        }
        jumpToEnd();
        return env;
    }
    
    protected OjOsmNode parseNode() throws XMLStreamException {
    	OjOsmNode n = new OjOsmNode();
        String lat = parser.getAttributeValue(null, "lat");
        String lon = parser.getAttributeValue(null, "lon");
        if (lat != null && lon != null) {
            n.setCoord(new Coordinate(Double.parseDouble(lon), Double.parseDouble(lat)));
        }
        readCommon(n);
        while (true) {
            int event = parser.next();
            if (event == XMLStreamConstants.START_ELEMENT) {
                if (parser.getLocalName().equals("tag")) {
                    parseTag(n);
                } else {
                    parseUnknown();
                }
            } else if (event == XMLStreamConstants.END_ELEMENT)
                return n;
        }
    }
    
    /**
     * Read out the common attributes and put them into current OsmPrimitive.
     */
    private void readCommon(OjOsmPrimitive current) throws XMLStreamException {
        current.setId(getLong("id"));
        if (current.getUniqueId() == 0) {
            throwException("Illegal object with ID=0.");
        }

        String time = parser.getAttributeValue(null, "timestamp");
        if (time != null && time.length() != 0) {
            current.setTimestamp(DateUtils.fromString(time));
        }

        // user attribute added in 0.4 API
        String user = parser.getAttributeValue(null, "user");
        // uid attribute added in 0.6 API
        String uid = parser.getAttributeValue(null, "uid");
        current.setUser(createUser(uid, user));

        // visible attribute added in 0.4 API
        String visible = parser.getAttributeValue(null, "visible");
        if (visible != null) {
            current.setVisible(Boolean.parseBoolean(visible));
        }

        String versionString = parser.getAttributeValue(null, "version");
        int version = 0;
        if (versionString != null) {
            try {
                version = Integer.parseInt(versionString);
            } catch(NumberFormatException e) {
                throwException("Illegal value for attribute ''version'' on OSM primitive with ID "+Long.toString(current.getUniqueId())+". Got: " + versionString);
            }
//            if (ds.getVersion().equals("0.6")){
                if (version <= 0 && current.getUniqueId() > 0) {
                    throwException("Illegal value for attribute ''version'' on OSM primitive with ID "+Long.toString(current.getUniqueId())+". Got: " + versionString);
                } else if (version < 0 && current.getUniqueId() <= 0) {
                    System.out.println("WARNING: Normalizing value of attribute ''version'' of element "+current.getUniqueId()+" to 0, API version is  0.6. Got " + version);
                    version = 0;
                }
//            } else if (ds.getVersion().equals("0.5")) {
//                if (version <= 0 && current.getUniqueId() > 0) {
//                    System.out.println(tr("WARNING: Normalizing value of attribute ''version'' of element {0} to {2}, API version is ''{3}''. Got {1}.", current.getUniqueId(), version, 1, "0.5"));
//                    version = 1;
//                } else if (version < 0 && current.getUniqueId() <= 0) {
//                    System.out.println(tr("WARNING: Normalizing value of attribute ''version'' of element {0} to {2}, API version is ''{3}''. Got {1}.", current.getUniqueId(), version, 0, "0.5"));
//                    version = 0;
//                }
//            } else {
//                // should not happen. API version has been checked before
//                throwException(tr("Unknown or unsupported API version. Got {0}.", ds.getVersion()));
//            }
        } else {
            // version expected for OSM primitives with an id assigned by the server (id > 0), since API 0.6
            //
            if (current.getUniqueId() > 0 /*&& ds.getVersion() != null && ds.getVersion().equals("0.6") */) {
                throwException("Missing attribute ''version'' on OSM primitive with ID " + Long.toString(current.getUniqueId()));
//            } else if (current.getUniqueId() > 0 && ds.getVersion() != null && ds.getVersion().equals("0.5")) {
//                // default version in 0.5 files for existing primitives
//                System.out.println("WARNING: Normalizing value of attribute ''version'' of element {0} to {2}, API version is ''{3}''. Got {1}.", current.getUniqueId(), version, 1, "0.5"));
//                version= 1;
            } else if (current.getUniqueId() <= 0 /*&& ds.getVersion() != null && ds.getVersion().equals("0.5") */) {
                // default version in 0.5 files for new primitives, no warning necessary. This is
                // (was) legal in API 0.5
                version= 0;
            }
        }
        current.setVersion(version);

        String action = parser.getAttributeValue(null, "action");
        if (action == null) {
            // do nothing
        } else if (action.equals("delete")) {
            current.setDeleted(true);
            current.setModified(current.isVisible());
        } else if (action.equals("modify")) {
            current.setModified(true);
        }

        String v = parser.getAttributeValue(null, "changeset");
        if (v == null) {
            current.setChangesetId(0);
        } else {
            try {
                current.setChangesetId(Integer.parseInt(v));
            } catch(NumberFormatException e) {
                if (current.getUniqueId() <= 0) {
                    // for a new primitive we just log a warning
                    System.out.println("Illegal value for attribute ''changeset'' on new object "+current.getUniqueId()+". Got "+v+". Resetting to 0.");
                    current.setChangesetId(0);
                } else {
                    // for an existing primitive this is a problem
                    throwException("Illegal value for attribute ''changeset''. Got " + v);
                }
            }
            if (current.getChangesetId() <=0) {
                if (current.getUniqueId() <= 0) {
                    // for a new primitive we just log a warning
                    System.out.println("Illegal value for attribute ''changeset'' on new object "+current.getUniqueId()+". Got "+v+". Resetting to 0.");
                    current.setChangesetId(0);
                } else {
                    // for an existing primitive this is a problem
                    throwException("Illegal value for attribute ''changeset''. Got " + v);
                }
            }
        }
    }

    protected OjOsmWay parseWay() throws XMLStreamException {
        OjOsmWay wd = new OjOsmWay();
        readCommon(wd);

        ArrayList<Long> nodeIds = new ArrayList<Long>();
        while (true) {
            int event = parser.next();
            if (event == XMLStreamConstants.START_ELEMENT) {
                if (parser.getLocalName().equals("nd")) {
                    nodeIds.add(parseWayNode(wd));
                } else if (parser.getLocalName().equals("tag")) {
                    parseTag(wd);
                } else {
                    parseUnknown();
                }
            } else if (event == XMLStreamConstants.END_ELEMENT) {
                break;
            }
        }
        if (wd.isDeleted() && nodeIds.size() > 0) {
            System.out.println("Deleted way "+wd.getUniqueId()+" contains nodes");
            nodeIds = new ArrayList<Long>();
        }
        wd.setNodeIds(nodeIds);
        return wd;
    }

    private long parseWayNode(OjOsmWay w) throws XMLStreamException {
        if (parser.getAttributeValue(null, "ref") == null) {
            throwException(
                    "Missing mandatory attribute 'ref' on <nd> of way " + w.getUniqueId()
            );
        }
        long id = getLong("ref");
        if (id == 0) {
            throwException(
                    "Illegal value of attribute ''ref'' of element <nd>. Got " + id
            );
        }
        jumpToEnd();
        return id;
    }

    protected OjOsmRelation parseRelation() throws XMLStreamException {
        OjOsmRelation r = new OjOsmRelation();
        readCommon(r);
        
        ArrayList<OjOsmRelationMember> members = new ArrayList<OjOsmRelationMember>();
        while (true) {
            int event = parser.next();
            if (event == XMLStreamConstants.START_ELEMENT) {
                if (parser.getLocalName().equals("member")) {
                    members.add(parseRelationMember(r));
                } else if (parser.getLocalName().equals("tag")) {
                    parseTag(r);
                } else {
                    parseUnknown();
                }
            } else if (event == XMLStreamConstants.END_ELEMENT) {
                break;
            }
        }
        if (r.isDeleted() && members.size() > 0) {
            System.out.println("Deleted relation "+r.getUniqueId()+" contains members");
            members = new ArrayList<OjOsmRelationMember>();
        }
        r.setMembers(members);
        return r;
    }

    private OjOsmRelationMember parseRelationMember(OjOsmRelation r) throws XMLStreamException {
        String role = "";
        int osmPrimitiveType = -1;
        long id = 0;
        String value = parser.getAttributeValue(null, "ref");
        if (value == null) {
            throwException("Missing attribute ''ref'' on member in relation " + r.getUniqueId());
        }
        try {
            id = Long.parseLong(value);
        } catch(NumberFormatException e) {
            throwException("Illegal value for attribute ''ref'' on member in relation "+Long.toString(r.getUniqueId())+". Got " + value);
        }
        value = parser.getAttributeValue(null, "type");
        if (value == null) {
            throwException("Missing attribute ''type'' on member "+Long.toString(id)+" in relation " + Long.toString(r.getUniqueId()));
        }
        try {
        	osmPrimitiveType = OjOsmRelationMember.getOsmPrimitiveTypeFromParsedString(value);
        } catch(IllegalArgumentException e) {
            throwException("Illegal value for attribute ''type'' on member "+Long.toString(id)+" in relation "+Long.toString(r.getUniqueId())+". Got " + value);
        }
        value = parser.getAttributeValue(null, "role");
        role = value;

        if (id == 0) {
            throwException("Incomplete <member> specification with ref=0");
        }
        jumpToEnd();
        return new OjOsmRelationMember(role, osmPrimitiveType, id);
    }

   
    private long getLong(String name) throws XMLStreamException {
        String value = parser.getAttributeValue(null, name);
        if (value == null) {
            throwException("Missing required attribute: " + name);
        }
        try {
            return Long.parseLong(value);
        } catch(NumberFormatException e) {
            throwException("Illegal long value for attribute: " + name + ". Got instead: " + value);
        }
        return 0; // should not happen
    }
    
    /**
     * When cursor is at the start of an element, moves it to the end tag of that element.
     * Nested content is skipped.
     *
     * This is basically the same code as <code>#parseUnknown()</code>, except for the warnings, which
     * are displayed for inner elements and not at top level.
     */
    private void jumpToEnd(boolean printWarning) throws XMLStreamException {
        while (true) {
            int event = parser.next();
            if (event == XMLStreamConstants.START_ELEMENT) {
                parseUnknown(printWarning);
            } else if (event == XMLStreamConstants.END_ELEMENT)
                return;
        }
    }

    private void jumpToEnd() throws XMLStreamException {
        jumpToEnd(true);
    }
    
    private void parseTag(Tagged t) throws XMLStreamException {
        String key = parser.getAttributeValue(null, "k");
        String value = parser.getAttributeValue(null, "v");
        if (key == null || value == null) {
            throwException("Missing key or value attribute in tag.");
        }
        t.put(key.intern(), value.intern());
        jumpToEnd();
    }
    
    private User createUser(String uid, String name) throws XMLStreamException {
        if (uid == null) {
            if (name == null){
                return User.createLocalUser("dummyuser");
            }
            return User.createLocalUser(name);
        }
        try {
            long id = Long.parseLong(uid);
            return User.createOsmUser(id, name);
        } catch(NumberFormatException e) {
            throwException(MessageFormat.format("Illegal value for attribute ''uid''. Got ''{0}''.", uid));
        }
        return null;
    }
    
	private static void checkForInvalidNodeGeoms(HashMap<Long, OjOsmNode> allNodesT, TaskMonitor monitor) {
        Iterator<Long> keySetIterator = allNodesT.keySet().iterator();
        ArrayList<Long> invalidNodeIds = new ArrayList<Long>(); 
        int nodeCount = 0; int maxNodeCount = allNodesT.size();
        while(keySetIterator.hasNext()){
        	if(monitor != null){
        		monitor.report(nodeCount, maxNodeCount, "Checking node geometries.");
        	}
        	Long key = keySetIterator.next();
          	OjOsmNode tempn = allNodesT.get(key);
          	Geometry geom = tempn.getGeom();
          	if(geom.isValid() ==  false){
          		invalidNodeIds.add(key);
          	}
          	nodeCount++;
        }
        if(invalidNodeIds.size() > 0 ){
        	JUMPWorkbench.getInstance().getFrame().log("OjOsmReader.checkForInvalidNodeGeoms() : Found invalid node geometries ...deleting these");
        	for (Iterator iterator = invalidNodeIds.iterator(); iterator.hasNext();) {
				Long id = (Long) iterator.next();
				allNodesT.remove(id);
			}
        }
        else{
        	JUMPWorkbench.getInstance().getFrame().log("OjOsmReader.checkForInvalidNodeGeoms() : All node geometries are valid");
        }
	}
	
	private static void checkForInvalidWayGeoms(HashMap<Long, OjOsmWay> allWaysT, TaskMonitor monitor) {
        Iterator<Long> keySetIterator = allWaysT.keySet().iterator();
        ArrayList<Long> invalidWayIds = new ArrayList<Long>(); 
        int wayCount = 0; int mayWayCount = allWaysT.size();
        while(keySetIterator.hasNext()){
        	if(monitor != null){
        		monitor.report(wayCount, mayWayCount, "Checking way geometries.");
        	}
        	wayCount++;
        	Long key = keySetIterator.next();
        	OjOsmWay tempn = allWaysT.get(key);
        	Geometry geom = tempn.getGeom();
        	boolean valid = false;
        	try{
        	  //we need try-catch, because we may have already now geoms with null points
        	  valid = geom.isValid();
        	}
        	catch(Exception e){
        	  //eat it, because if we get an exception we can't use it anyway
        	}
        	if(valid ==  false){
        	  invalidWayIds.add(key);
        	}
        }
        if(invalidWayIds.size() > 0 ){
        	JUMPWorkbench.getInstance().getFrame().log("OjOsmReader.checkForInvalidWayGeoms() : found invalid way geometries ...deleting these.");
        	for (Iterator iterator = invalidWayIds.iterator(); iterator.hasNext();) {
				Long id = (Long) iterator.next();
				allWaysT.remove(id);
			}
        }
        else{
        	JUMPWorkbench.getInstance().getFrame().log("OjOsmReader.checkForInvalidWayGeoms() : All way geometries are valid");
        }
	}
	
    /**
     * Creates for each way object in <code>allWays</code> the geometry. Usually this is a LineString.
     * However, it can also be an area if the tag/key "area"="yes" exists.
     * Then it calls <code>#createRelationGeometries()</code>.  
     * @param monitor for GUI feedback; can be null.
     * @see #createRelationGeometries()
     */
    private void createWayGeoms(TaskMonitor monitor) {
    	GeometryFactory gf = new GeometryFactory();
    	//iterate over all ways, and retrieve the node Ids with their geometries
    	Iterator<Long> keySetIterator = this.allWays.keySet().iterator();
    	int wayCount = 0; int mayWayCount = this.allWays.size();
    	while(keySetIterator.hasNext()){
        	if(monitor != null){
        		monitor.report(wayCount, mayWayCount, "Creating way geometries.");
        	}
        	wayCount++;
    		Long key = keySetIterator.next();
    		OjOsmWay w = this.allWays.get(key);
    		ArrayList<Long> nodeIds = w.getNodeIds();
    		int numNodes = nodeIds.size();
    		Coordinate[] coords = new Coordinate[numNodes];
    		int i=0;
    		for (Iterator iteratorNodeId = nodeIds.iterator(); iteratorNodeId.hasNext();) {
    			Long nid = (Long) iteratorNodeId.next();
    			try{
	    			OjOsmNode tnode = this.allNodes.get(nid);
	    			coords[i] = tnode.getCoord();
	    			tnode.setUsedInAWay(true);
    			}
    			catch(Exception e){
    				JUMPWorkbench.getInstance().getFrame().log("OjOsmReader.createWayGeoms...(): 'node' for way creation not found. Node Id: " + nid);
    			}
    			i++;
    		}
    		Geometry g;
    		boolean hasAreaTag = false;
    		hasAreaTag = w.hasKey("area");
    		if(coords.length >= 2){
	    		if(w.isClosed() && hasAreaTag){
	    			if(w.get("area").equalsIgnoreCase("yes")){
	    				LinearRing lr = gf.createLinearRing(coords);
	    				g = gf.createPolygon(lr, null);
	    			}
	    			else{
	    				g = gf.createLineString(coords);
	    			}
	    		}
	    		else{
	        		g = gf.createLineString(coords);	
	    		}
    		}
    		else{
    			g = gf.createMultiPoint(coords);
    		}
    		w.setGeom(g); 
    	}
    }
    
    /**
     * Creates for each relation object in <code>allRelations</code> the geometry.
     * This method needs to be called after the way geometries are created, and therefore
     * is called from within the method <code>#createWayGeomsAndRelationGeometries()</code>.
     * @param monitor for GUI messages; can be null.
     * @see createWayGeomsAndRelationGeometries()
     */
    private void createRelationGeometries(TaskMonitor monitor){
    	GeometryFactory gf = new GeometryFactory();
    	//iterate over all ways, and retrieve the node Ids with their geometries
    	Iterator<Long> keySetIterator = this.allRelations.keySet().iterator();
    	int relationCount=0; int maxRelCount = this.allRelations.size();
    	while(keySetIterator.hasNext()){
        	if(monitor != null){
        		monitor.report(relationCount, maxRelCount, "Assembling relation geometry.");
        	}
        	relationCount++;
    		Long key = keySetIterator.next();
    		OjOsmRelation rel = this.allRelations.get(key);
    		//check if there is a multipolygon tag, so we can do some decisions later
    		boolean isMultiPolygon = rel.isMultiPolygon();
    			
    		ArrayList<OjOsmRelationMember> members = rel.getMembers();
    		
    		ArrayList<LineString> innerLS = new ArrayList<LineString>();
    		ArrayList<LineString> outerLS = new ArrayList<LineString>();
    		ArrayList<LineString> wayCollection = new ArrayList<LineString>();
    		ArrayList<Coordinate> nodeCoords = new ArrayList<Coordinate>(); 
    		Geometry g = gf.createGeometryCollection(null); //create a geom that always works
    		boolean hasWays = false;
    		boolean hasNodes = false;
    		for (Iterator iteratorMembers = members.iterator(); iteratorMembers.hasNext();) {
    			OjOsmRelationMember member = (OjOsmRelationMember) iteratorMembers.next();
    			if(member.getOsmPrimitiveType() == OjOsmPrimitive.OSM_PRIMITIVE_WAY){
    				hasWays = true;
    				try{
    					OjOsmWay way = this.allWays.get(member.getMemberId());
    					if(way != null){ //because we may not have a way with that ID stored in this file
    						member.setIdNotFoundInDataset(false);
	    					Coordinate[] wayCoords = way.getGeom().getCoordinates();
							LineString ls = gf.createLineString(wayCoords);
    						/**
    						 * note: even if we have a multi-polygon it may be that we have non-closed lines like the limits of a river
    						 */
	    					if(isMultiPolygon){
			    				if(member.hasRole()){	
			    					if(member.isInnerWay(member.getRole())){
			    						innerLS.add(ls);
			    					}
			    					else if(member.isOuterWay(member.getRole())){
			    						outerLS.add(ls);
			    					}
			    					else{
			    						System.out.println("Relation member with id "+member.getMemberId()+" of type 'way' has an unrecognized role type: "+ member.getRole() +". Adding it to way collection");
				    					wayCollection.add(ls);
			    					}
			    				}
			    				else{
			    					System.out.println("Relation member with id "+member.getMemberId()+" of type 'way' has no role assigned. Adding it to way collection");
			    					wayCollection.add(ls);
			    				}
    						}
    						else{//not a multipolygon, so treat everything as a multi-linestring
		    					wayCollection.add(ls);
    						}
		    				// set flag that this way is actually part of a relation
		    				way.setUsedInARelation(true);
		    			}
    					else{//way == null
    						rel.setMissingMembers(true);
    						member.setIdNotFoundInDataset(true);
    						JUMPWorkbench.getInstance().getFrame().log("OjOsmReader.createRelationGeometries(): relation member of type 'way' not found. Relation ID: " + rel.getId() + ", Member Id: " + member.getMemberId());
    					}
    				}
    				catch(Exception e){
    					JUMPWorkbench.getInstance().getFrame().log("OjOsmReader.createRelationGeometries(): relation member of type 'way' not found. No ways existing in dataset. Relation ID: "+ rel.getId());
    				}
    			}
    			else if(member.getOsmPrimitiveType() == OjOsmPrimitive.OSM_PRIMITIVE_NODE) {
    				hasNodes = true;
    				try{
    					OjOsmNode node = this.allNodes.get(member.getMemberId());
    					if(node != null){
    						member.setIdNotFoundInDataset(false);
	    					nodeCoords.add(node.getCoord());
	    					node.setUsedInARelation(true);
    					}
    					else{
    						rel.setMissingMembers(true);
    						member.setIdNotFoundInDataset(true);
    						JUMPWorkbench.getInstance().getFrame().log("OjOsmReader.createRelationGeometries(): relation member of type 'node' not found. Relation ID: " + rel.getId() + ", Node/Member Id: " + member.getMemberId());
    					}
    				}
    				catch(Exception e){
    					JUMPWorkbench.getInstance().getFrame().log("OjOsmReader.createRelationGeometries(): no list of 'nodes' found. Relation ID: "+ rel.getId());
    				}
    			}
    			else{
    				JUMPWorkbench.getInstance().getFrame().log("Relation member is of unknown type: " + member.getOsmPrimitiveType() + ". Skipping Member.");
    			}
    		}//end iteration over all members
			ArrayList<Geometry> allMemberGeoms = new ArrayList<Geometry>();
    		if(hasWays){
   				// build the relation geometries based on ways
    			
    			if(outerLS.size() > 0){
    				ArrayList<Polygon> outerPolys = this.createPolygonsFromRelationMemberWays(outerLS, wayCollection);
    				//assuming that we can have inner ways only if we have outer way
    				//now check if we have inner polys
    				ArrayList<Polygon> innerPolys = null;
    				ArrayList<Geometry> multiPolys = null;
    				if(innerLS.size() > 0){
    					innerPolys = this.createPolygonsFromRelationMemberWays(innerLS, wayCollection);
	    				multiPolys = this.substractRelationPolygons(outerPolys, innerPolys, wayCollection);
	    				allMemberGeoms.addAll(multiPolys);
	   				}
	   				else{
	   					allMemberGeoms.addAll(outerPolys);
	   				}

    			}// end (outerLS > 0);
    			Geometry gways = gf.createGeometryCollection(null);
    			if(wayCollection.size() > 0){
    				LineString[] lines = wayCollection.toArray(new LineString[wayCollection.size()]);
    				gways = gf.createGeometryCollection(lines);
	    			allMemberGeoms.add(gways);
    			}
    		}
    		if(hasNodes){
    			Coordinate[] pointCoords = nodeCoords.toArray(new Coordinate[nodeCoords.size()]);
    			allMemberGeoms.add(gf.createMultiPoint(pointCoords));
    		}
    		Geometry[] allGeomArray = dissolveGeomCollections(allMemberGeoms);
    		g=gf.createGeometryCollection(allGeomArray);
    		rel.setGeom(g); 
    	}
    }

    /**
     * Given a list of linestrings, i.e. ways that are part of a relation, the method checks which lines
     * are connected using JTS union method - which creates multi-line-strings. Then it is checked if
     * the single linestrings within the multi-linestring are closed. If they are closed, then a polygon 
     * is formed. LineStrings/ways that are not closed are added to the list allWays. Non-closed linestrings
     * may appear if not all elements of a multipolygon relation are found in the file. 
     * The method is used by <code>#createRelationGeometries()</code>.
     * @param outerOrInnerLS
     * @param allWays
     * @return list of polygons
     */
	private ArrayList<Polygon> createPolygonsFromRelationMemberWays(ArrayList<LineString> outerOrInnerLS, ArrayList<LineString> allWays){
		GeometryFactory gf = new GeometryFactory();
		// Union/merge all the single LineStrings
		// so we handle the case when a polygon outline is formed by many single lines
		// the line merger will do the node-ing, etc.

		// Note: I was thinking that I may need tu use the LineMerger and not just noding to
		// concat the different linestrings (see also OSM feature with id 1846627 - that seem to be closed)
		// However, Martin Davis said on the JTS list that both, union and linemerger, can be used.
		Geometry unionGeometry = outerOrInnerLS.get(0);
		for (int j = 1; j < outerOrInnerLS.size(); j++) {
			//check first if we have valid geoms: because we may not
			if(outerOrInnerLS.get(j).isValid()){
				unionGeometry = unionGeometry.union(outerOrInnerLS.get(j));
			}
		}
		// we should have a MultiLineString now, however, we may also get only one
		ArrayList<Polygon> createdPolygons = new ArrayList<Polygon>();
		if(unionGeometry instanceof LineString){
			//check if it is closed
			boolean closed = false;
			try {
				closed = ((LineString) unionGeometry).isClosed();
				if (closed){
					Polygon p = gf.createPolygon(unionGeometry.getCoordinates());
					createdPolygons.add(p);
				}
				else{
					//not closed, so we add it to the ways
					allWays.add((LineString)unionGeometry);
				}
			} catch (Exception e) {
				JUMPWorkbench.getInstance().getFrame().log("OJOsmReader.createPolygonsFromRelationMemberWays: can't evaluate if LineString is closed. LineString: " + unionGeometry.toString());
			}
		}
		else if(unionGeometry instanceof MultiLineString){
			//create polygons from all parts
			MultiLineString lines = ((MultiLineString)unionGeometry);
			for (int j = 0; j < lines.getNumGeometries(); j++) {
				LineString lst = (LineString)lines.getGeometryN(j);
				if(lst.isClosed()){
					Polygon pt = gf.createPolygon(lst.getCoordinates());
					createdPolygons.add(pt);
				}
				else{
					//not closed, so we add to ways
					allWays.add(lst);
				}
			}
		}
		else{
			JUMPWorkbench.getInstance().getFrame().log("unionGeometry of outerLS is neither a single LineString nor a MultiLineString - help!!!");
		}
    	return createdPolygons;
    }
    
	/**
	 * Given the outer and inner ways as polygons in separated list, this method 
	 * finally creates the holes, by checking which inner polygon is part of
	 * which outer polygons. Inner polygons that are not covered by any outer polygon
	 * are added to the wayCollection as LineStrings (not as polygons), so they are not lost.
	 * The method is used by <code>#createRelationGeometries()</code>.
	 * @param outerPolys are the polygons of a relation that may have holes. 
	 * @param innerPolys i.e. the holes to be created in the outer polygons.
	 * @param wayCollection
	 * @return List of (outer) polygons with and without holes.
	 */
	private ArrayList<Geometry> substractRelationPolygons(
			ArrayList<Polygon> outerPolys, ArrayList<Polygon> innerPolys,
			ArrayList<LineString> wayCollection) {
		// Geometry.difference() is not implemented for Geometry collections - so we have to do
		// it by hand and therefore do use the lists.
		// Assuming there are not too many outer and inner lines we do not use an index for 
		// speeding up the search
		GeometryFactory gf = new GeometryFactory();
		ArrayList<Geometry> newOuterPolys = new ArrayList<Geometry>();
		// Loop over the outer polygons
		for (Iterator iterator = outerPolys.iterator(); iterator.hasNext();) {
			Geometry outerPoly = (Polygon) iterator.next();
			int i = 0; 
			while(i < innerPolys.size()){
				Polygon innerPoly = innerPolys.get(i);
				if(outerPoly.covers(innerPoly)){
					outerPoly = outerPoly.difference(innerPoly); //note, a difference may end-up being not a polygon anymore
					// assuming that each innerPoly is covered only by one
					// outerPoly we remove it
					innerPolys.remove(i);
					// in this case we do not raise the list index
				}
				else{//innerPoly is not covered by outerPoly
					i++;
				}
			}
			// store the outer poly - modified or not
			newOuterPolys.add(outerPoly);
		}
		// we may have some of the innerPolys left for which we cannot find a covering polygon
		// lets add them as LineStrings to wayCollection
		if(innerPolys.size() > 0 ){
			//System.out.println("OJOsmReader.substractRelationPolygons(): some inner-Polygons from a Relation are not covered by an outer-Polygon");
			for (Iterator iterator = innerPolys.iterator(); iterator.hasNext();) {
				Polygon pol = (Polygon) iterator.next();
				Coordinate[] coords = pol.getCoordinates();
				wayCollection.add(gf.createLineString(coords));
			}
		}
		return newOuterPolys;
	}
	
	/**
	 * Explodes/dissolves a list that can contain multi-geometries, such as MultiLineString, 
	 * into its single geometries, e.g. a list of single LineStrings. The method is used
	 * by <code>#createRelationGeometries()</code>.
	 * @param allMemberGeoms
	 * @return list of single geometries
	 */
    private static Geometry[] dissolveGeomCollections(ArrayList<Geometry> allMemberGeoms) {
    	ArrayList<Geometry> singleGeoms = new ArrayList<Geometry>();
    	for (Iterator iterator = allMemberGeoms.iterator(); iterator.hasNext();) {
			Geometry g = (Geometry) iterator.next();
			ArrayList<Geometry> collGeoms = GeometryConverter.explodeGeomsIfMultiG(g);
			singleGeoms.addAll(collGeoms);
		}
    	Geometry[] geomArray = singleGeoms.toArray(new Geometry[singleGeoms.size()]);
		return geomArray;
	}
    
    /**
     * Based on the tags the methods will try to identify the major land-use type.
     * For some land-uses (tag: building=yes and natural="...") the geometry of ways will 
     * be changed from LineString to Polygon.
     * @param osmFeatureData
     * @return a new dataset with identified landuses.
     */
    private ArrayList<OjOsmPrimitive> detectMayorLanduses(ArrayList<OjOsmPrimitive> osmFeatureData){

    	ArrayList<OjOsmPrimitive> osmFeaturesWithLuType = new ArrayList<OjOsmPrimitive>();
    	for (Iterator iterator = osmFeatureData.iterator(); iterator.hasNext();) {
			OjOsmPrimitive ojOsmPrimitive = (OjOsmPrimitive) iterator.next();

			GeometryFactory gf = new GeometryFactory();
			/* == Ways ==*/
			if(ojOsmPrimitive instanceof OjOsmWay){
				if (ojOsmPrimitive.hasKey("building")){
					ojOsmPrimitive.setLandUseDescription("building");
					Geometry geom = ojOsmPrimitive.getGeom();
					if((geom instanceof LineString) && ((OjOsmWay) ojOsmPrimitive).isClosed()){
						ojOsmPrimitive.setGeom(gf.createPolygon(geom.getCoordinates()));
					}
				}
				else if (ojOsmPrimitive.hasKey("highway")){
					if(ojOsmPrimitive.hasKey("foot")){
						if(ojOsmPrimitive.get("foot").equalsIgnoreCase("yes")){
							if(ojOsmPrimitive.hasKey("bicyle")){
								if(ojOsmPrimitive.get("bicyle").equalsIgnoreCase("yes")){
									ojOsmPrimitive.setLandUseDescription("path: foot and bicycle");
								}
								else{
									ojOsmPrimitive.setLandUseDescription("footpath");
								}
							}
							else{
								ojOsmPrimitive.setLandUseDescription("footpath");
							}
						}
						else{
							ojOsmPrimitive.setLandUseDescription("path");	
						}
					}//does not have "foot" tag
					else{
							ojOsmPrimitive.setLandUseDescription("road: " + ojOsmPrimitive.get("highway"));
					}
				}
				else if (ojOsmPrimitive.hasKey("railway")){
					ojOsmPrimitive.setLandUseDescription("rail");
				}
				else if (ojOsmPrimitive.hasKey("natural")){
					ojOsmPrimitive.setLandUseDescription(ojOsmPrimitive.get("natural"));
					Geometry geom = ojOsmPrimitive.getGeom();
					if((geom instanceof LineString) && ((OjOsmWay) ojOsmPrimitive).isClosed()){
						ojOsmPrimitive.setGeom(gf.createPolygon(geom.getCoordinates()));
					}
				}
				else if (ojOsmPrimitive.hasKey("parking")){
					ojOsmPrimitive.setLandUseDescription("parking");
					if(ojOsmPrimitive.get("parking").equalsIgnoreCase("surface")){
						Geometry geom = ojOsmPrimitive.getGeom();
						if((geom instanceof LineString) && ((OjOsmWay) ojOsmPrimitive).isClosed()){
							ojOsmPrimitive.setGeom(gf.createPolygon(geom.getCoordinates()));
						}
					}
				}
				else if (ojOsmPrimitive.hasKey("amenity")){
					if(ojOsmPrimitive.get("amenity").equalsIgnoreCase("parking")){
						ojOsmPrimitive.setLandUseDescription("parking");
						Geometry geom = ojOsmPrimitive.getGeom();
						if((geom instanceof LineString) && ((OjOsmWay) ojOsmPrimitive).isClosed()){
							ojOsmPrimitive.setGeom(gf.createPolygon(geom.getCoordinates()));
						}
					}
				}
				else if (ojOsmPrimitive.hasKey("landuse")){
					ojOsmPrimitive.setLandUseDescription(ojOsmPrimitive.get("landuse"));
					Geometry geom = ojOsmPrimitive.getGeom();
					if((geom instanceof LineString) && ((OjOsmWay) ojOsmPrimitive).isClosed()){
						ojOsmPrimitive.setGeom(gf.createPolygon(geom.getCoordinates()));
					}
				}
				else if (ojOsmPrimitive.hasKey("sport")){
					ojOsmPrimitive.setLandUseDescription(ojOsmPrimitive.get("sport"));
					Geometry geom = ojOsmPrimitive.getGeom();
					if((geom instanceof LineString) && ((OjOsmWay) ojOsmPrimitive).isClosed()){
						ojOsmPrimitive.setGeom(gf.createPolygon(geom.getCoordinates()));
					}
				}
				else if (ojOsmPrimitive.hasKey("leisure")){
					ojOsmPrimitive.setLandUseDescription("leisure: "+ ojOsmPrimitive.get("leisure"));
					Geometry geom = ojOsmPrimitive.getGeom();
					if((geom instanceof LineString) && ((OjOsmWay) ojOsmPrimitive).isClosed()){
						ojOsmPrimitive.setGeom(gf.createPolygon(geom.getCoordinates()));
					}
				}
				else if (ojOsmPrimitive.hasKey("waterway")){
					ojOsmPrimitive.setLandUseDescription("waterway: "+ ojOsmPrimitive.get("waterway"));
				}

			}
			/* == Nodes ==*/
			else if(ojOsmPrimitive instanceof OjOsmNode){

				if (ojOsmPrimitive.hasKey("highway")){
					ojOsmPrimitive.setLandUseDescription(ojOsmPrimitive.get("highway"));
				}
				else if (ojOsmPrimitive.hasKey("railway")){
					ojOsmPrimitive.setLandUseDescription(ojOsmPrimitive.get("railway"));
				}
				else if (ojOsmPrimitive.hasKey("barrier")){
					ojOsmPrimitive.setLandUseDescription(ojOsmPrimitive.get("barrier"));
				}
				else if (ojOsmPrimitive.hasKey("amenity")){
					ojOsmPrimitive.setLandUseDescription(ojOsmPrimitive.get("amenity"));
				}
				else if (ojOsmPrimitive.hasKey("tourism")){
					ojOsmPrimitive.setLandUseDescription(ojOsmPrimitive.get("tourism"));
				}
				else if (ojOsmPrimitive.hasKey("building")){
					ojOsmPrimitive.setLandUseDescription("building");
				}
			}
			else if(ojOsmPrimitive instanceof OjOsmRelation){

				if(ojOsmPrimitive.hasKey("type")){
					String relType = ojOsmPrimitive.get("type");
					if(relType.equalsIgnoreCase("multipolygon")){
						relType = "";
					}
					ojOsmPrimitive.setLandUseDescription(relType);
					if (ojOsmPrimitive.hasKey("natural")){
						ojOsmPrimitive.setLandUseDescription(relType +":"+ ojOsmPrimitive.get("natural"));
					}
					if (ojOsmPrimitive.hasKey("route")){
						ojOsmPrimitive.setLandUseDescription(relType +":"+ ojOsmPrimitive.get("route"));
					}
					if (ojOsmPrimitive.hasKey("waterway")){
						ojOsmPrimitive.setLandUseDescription(relType +":"+ ojOsmPrimitive.get("waterway"));
					}
					if (ojOsmPrimitive.hasKey("boundary")){
						ojOsmPrimitive.setLandUseDescription(relType +":"+ ojOsmPrimitive.get("boundary"));
					}
					if (ojOsmPrimitive.hasKey("site")){
						ojOsmPrimitive.setLandUseDescription(relType +":"+ ojOsmPrimitive.get("site"));
					}
					if (ojOsmPrimitive.hasKey("destination_sign")){
						ojOsmPrimitive.setLandUseDescription(relType +":"+ ojOsmPrimitive.get("destination_sign"));
					}
					if (ojOsmPrimitive.hasKey("public_transport")){
						ojOsmPrimitive.setLandUseDescription(relType +":"+ ojOsmPrimitive.get("public_transport"));
					}
				}
			}
			osmFeaturesWithLuType.add(ojOsmPrimitive);
		}
    	return osmFeaturesWithLuType;
    }
}
