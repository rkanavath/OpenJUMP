// License: GPL. See LICENSE file for details.
package org.openjump.core.ui.plugin.file;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;



/**
 * Parser for the Osm Api. Read from an input stream and construct a dataset out of it.
 *
 * For each xml element, there is a dedicated method.
 * The XMLStreamReader cursor points to the start of the element, when the method is
 * entered, and it must point to the end of the same element, when it is exited.
 * 
 * This class contains methods taken from JOSM's org.openstreetmap.josm.io.OsmReader class.
 * 
 * @author: sstein
 * @date: 5.July.2013
 */
public class OJOsmReader {

    protected XMLStreamReader parser;
    private ArrayList<OjOsmPrimitive> dataset = null;
    HashMap<Long, OjOsmNode> allNodes = new HashMap<Long, OjOsmNode>();
    HashMap<Long, OjOsmWay> allWays = new HashMap<Long, OjOsmWay>();
    HashMap<Long, OjOsmWay> allRelations = new HashMap<Long, OjOsmWay>();
    Envelope osmFileEnvelope = null;

	/**
     * constructor (for private and subclasses use only)
     *
     * @see #doParseDataSet(InputStream)
     */
    protected OJOsmReader() {
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
     * such as creating the way geometrys. However, the parsing itself is
     * done in #.parseOSM() via #.parse() and #.parseRoot().
     * @param source
     * @return
     * @throws IllegalDataException
     */
    public boolean doParseDataSet(InputStream source) throws IllegalDataException {
    	if(source == null) return false;
        try {
            InputStreamReader ir = UTFInputStreamReader.create(source, "UTF-8");
            XMLStreamReader parser = XMLInputFactory.newInstance().createXMLStreamReader(ir);
            setParser(parser);
            parse();

            //prepareDataSet(); // [sstein] : not needed, calls originally dataset assembling methods 
            					//  from org.openstreetmap.josm.io.AbstractReader
            
            //add only nodes that have tags/keys (except for the create_by key)
            Iterator<Long> keySetIterator = this.allNodes.keySet().iterator();
            while(keySetIterator.hasNext()){
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
            }
            //assemble way geometries
            createWayGeoms();
            //add all ways
            Iterator<Long> keySetIterator2 = this.allWays.keySet().iterator();
            while(keySetIterator2.hasNext()){
              Long key = keySetIterator2.next();
              this.dataset.add(this.allWays.get(key));
            }
            
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
            System.out.println("Undefined element " + parser.getLocalName() + " found in input stream. Skipping.");
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
                    parseRelation();
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
        //TODO check if type value assignment works
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
     * This is basically the same code as parseUnknown(), except for the warnings, which
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
            if (name == null)
                return null;
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
    
    /**
     * creates for each way object in @allWays the geometry. Usually this is a LineString.
     *  However, it can also be an area if the tag/key "area"="yes" exists.  
     */
    private void createWayGeoms() {
    	GeometryFactory gf = new GeometryFactory();
    	//iterate over all ways, and retrieve the node Ids with their geometries
    	Iterator<Long> keySetIterator = this.allWays.keySet().iterator();
    	while(keySetIterator.hasNext()){
    		Long key = keySetIterator.next();
    		OjOsmWay w = this.allWays.get(key);
    		ArrayList<Long> nodeIds = w.getNodeIds();
    		int numNodes = nodeIds.size();
    		Coordinate[] coords = new Coordinate[numNodes];
    		int i=0;
    		for (Iterator iteratorNodeId = nodeIds.iterator(); iteratorNodeId.hasNext();) {
    			Long nid = (Long) iteratorNodeId.next();
    			OjOsmNode tnode = this.allNodes.get(nid);
    			coords[i] = tnode.getCoord();
    			i++;
    		}
    		Geometry g;
    		boolean hasAreaTag = false;
    		hasAreaTag = w.hasKey("area");
    		//TODO: test this
    		if(hasAreaTag){
    			System.out.println("hasAreaTag");
    		}
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
    		w.setGeom(g); 
    	}
    }
}
