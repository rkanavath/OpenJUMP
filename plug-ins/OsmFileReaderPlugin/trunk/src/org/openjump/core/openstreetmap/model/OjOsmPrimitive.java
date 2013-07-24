package org.openjump.core.openstreetmap.model;

import java.text.MessageFormat;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicLong;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * this class mixes properties and methods from JOSMs 
 *   org.openstreetmap.josm.data.osm.OsmPrimitive
 * and
 *   org.openstreetmap.josm.data.osm.AbstractPrimitive
 * Though, flags here are used as class variables and not as 'int' as in AbstractPrimitive.
 * 
 * @author sstein
 * @data 5.July.2013
 */
public class OjOsmPrimitive implements Tagged{
    
    private static final AtomicLong idCounter = new AtomicLong(0);
    
    /* note: if new types are specified, then do also add the new type
     * to #.getOsmTypeAsString() below
     */
    public static final int OSM_PRIMITIVE_NODE = 1;
    public static final int OSM_PRIMITIVE_WAY = 2;
    public static final int OSM_PRIMITIVE_RELATION = 3;
    public static final int OSM_AREA = 4;

    private boolean hasLandUse = false;
    private String landUseType = "";
	private boolean usedInRelation = false;
    
    static long generateUniqueId() {
        return idCounter.decrementAndGet();
    }
    
    public OjOsmPrimitive() {
		this.id = generateUniqueId();
		GeometryFactory gf = new GeometryFactory();
		this.geom = gf.createGeometryCollection(null);
	}

    public void setId(long id) {
        this.id = id;
    }

    public void setVersion(int version) {
        this.version = version;
    }

	private Geometry geom = null;
	
	public Geometry getGeom(){
		return this.geom;
	}
	
	public void setGeom(Geometry g){
		this.geom = g;
	}
	
	private int osmType = 0;

	public int getOsmType(){
		return this.osmType;
	}
	
	public void setOsmType(int type){
		this.osmType = type;
	}
	
	public String getOsmTypeAsString(){
		String returnType = "";
		if(this.osmType == OjOsmPrimitive.OSM_PRIMITIVE_NODE){
			returnType = "node";
		}
		else if(this.osmType == OjOsmPrimitive.OSM_PRIMITIVE_WAY){
			returnType = "way";
		}
		else if(this.osmType == OjOsmPrimitive.OSM_PRIMITIVE_RELATION){
			returnType = "relation";
		}
		else if(this.osmType == OjOsmPrimitive.OSM_AREA){
			returnType = "area";
		}
		return returnType;
	}
	
    public String getLandUseDescription() {
		return landUseType;
	}

	public void setLandUseDescription(String landUseType) {
		this.landUseType = landUseType;
		//TODO: check how long an empty string is.
		if(landUseType.length()>1){
			this.hasLandUse = true;
		}
	}	
	
	public boolean hasLandUseDescription(){
		return this.hasLandUse;
	}
	
	public boolean isUsedInARelation(){
		return this.usedInRelation;
	}
	
	public void setUsedInARelation(boolean usedInRelation){
		this.usedInRelation = usedInRelation;
	}
	/*------------
     * Keys handling
     ------------*/

    // Note that all methods that read keys first make local copy of keys array reference. This is to ensure thread safety - reading
    // doesn't have to be locked so it's possible that keys array will be modified. But all write methods make copy of keys array so
    // the array itself will be never modified - only reference will be changed

	/**
     * The key/value list for this primitive.
     *
     */
    protected String[] keys;

    /**
     * Replies the map of key/value pairs. Never replies null. The map can be empty, though.
     *
     * @return tags of this primitive. Changes made in returned map are not mapped
     * back to the primitive, use setKeys() to modify the keys
     */
    @Override
    public Map<String, String> getKeys() {
        Map<String, String> result = new HashMap<String, String>();
        String[] keys = this.keys;
        if (keys != null) {
            for (int i=0; i<keys.length ; i+=2) {
                result.put(keys[i], keys[i + 1]);
            }
        }
        return result;
    }

    /**
     * Sets the keys of this primitives to the key/value pairs in <code>keys</code>.
     * Old key/value pairs are removed.
     * If <code>keys</code> is null, clears existing key/value pairs.
     *
     * @param keys the key/value pairs to set. If null, removes all existing key/value pairs.
     */
    @Override
    public void setKeys(Map<String, String> keys) {
        Map<String, String> originalKeys = getKeys();
        if (keys == null || keys.isEmpty()) {
            this.keys = null;
            keysChangedImpl(originalKeys);
            return;
        }
        String[] newKeys = new String[keys.size() * 2];
        int index = 0;
        for (Entry<String, String> entry:keys.entrySet()) {
            newKeys[index++] = entry.getKey();
            newKeys[index++] = entry.getValue();
        }
        this.keys = newKeys;
        keysChangedImpl(originalKeys);
    }

    /**
     * Set the given value to the given key. If key is null, does nothing. If value is null,
     * removes the key and behaves like {@link #remove(String)}.
     *
     * @param key  The key, for which the value is to be set. Can be null, does nothing in this case.
     * @param value The value for the key. If null, removes the respective key/value pair.
     *
     * @see #remove(String)
     */
    @Override
    public void put(String key, String value) {
        Map<String, String> originalKeys = getKeys();
        if (key == null)
            return;
        else if (value == null) {
            remove(key);
        } else if (keys == null){
            keys = new String[] {key, value};
            keysChangedImpl(originalKeys);
        } else {
            for (int i=0; i<keys.length;i+=2) {
                if (keys[i].equals(key)) {
                    keys[i+1] = value;  // This modifies the keys array but it doesn't make it invalidate for any time so its ok (see note no top)
                    keysChangedImpl(originalKeys);
                    return;
                }
            }
            String[] newKeys = new String[keys.length + 2];
            for (int i=0; i< keys.length;i+=2) {
                newKeys[i] = keys[i];
                newKeys[i+1] = keys[i+1];
            }
            newKeys[keys.length] = key;
            newKeys[keys.length + 1] = value;
            keys = newKeys;
            keysChangedImpl(originalKeys);
        }
    }

    /**
     * Remove the given key from the list
     *
     * @param key  the key to be removed. Ignored, if key is null.
     */
    @Override
    public void remove(String key) {
        if (key == null || keys == null) return;
        if (!hasKey(key))
            return;
        Map<String, String> originalKeys = getKeys();
        if (keys.length == 2) {
            keys = null;
            keysChangedImpl(originalKeys);
            return;
        }
        String[] newKeys = new String[keys.length - 2];
        int j=0;
        for (int i=0; i < keys.length; i+=2) {
            if (!keys[i].equals(key)) {
                newKeys[j++] = keys[i];
                newKeys[j++] = keys[i+1];
            }
        }
        keys = newKeys;
        keysChangedImpl(originalKeys);
    }

    /**
     * Removes all keys from this primitive.
     */
    @Override
    public void removeAll() {
        if (keys != null) {
            Map<String, String> originalKeys = getKeys();
            keys = null;
            keysChangedImpl(originalKeys);
        }
    }

    /**
     * Replies the value for key <code>key</code>. Replies null, if <code>key</code> is null.
     * Replies null, if there is no value for the given key.
     *
     * @param key the key. Can be null, replies null in this case.
     * @return the value for key <code>key</code>.
     */
    @Override
    public final String get(String key) {
        String[] keys = this.keys;
        if (key == null)
            return null;
        if (keys == null)
            return null;
        for (int i=0; i<keys.length;i+=2) {
            if (keys[i].equals(key)) return keys[i+1];
        }
        return null;
    }

    public final String getIgnoreCase(String key) {
        String[] keys = this.keys;
        if (key == null)
            return null;
        if (keys == null)
            return null;
        for (int i=0; i<keys.length;i+=2) {
            if (keys[i].equalsIgnoreCase(key)) return keys[i+1];
        }
        return null;
    }

    @Override
    public final Collection<String> keySet() {
        String[] keys = this.keys;
        if (keys == null)
            return Collections.emptySet();
        Set<String> result = new HashSet<String>(keys.length / 2);
        for (int i=0; i<keys.length; i+=2) {
            result.add(keys[i]);
        }
        return result;
    }

    /**
     * Replies true, if the map of key/value pairs of this primitive is not empty.
     *
     * @return true, if the map of key/value pairs of this primitive is not empty; false
     *   otherwise
     */
    @Override
    public final boolean hasKeys() {
        return keys != null;
    }

    /**
     * Replies true if this primitive has a tag with key <code>key</code>.
     *
     * @param key the key
     * @return true, if his primitive has a tag with key <code>key</code>
     */
    public boolean hasKey(String key) {
        String[] keys = this.keys;
        if (key == null) return false;
        if (keys == null) return false;
        for (int i=0; i< keys.length;i+=2) {
            if (keys[i].equals(key)) return true;
        }
        return false;
    }

    /**
     * What to do, when the tags have changed by one of the tag-changing methods.
     */
    protected void keysChangedImpl(Map<String, String> originalKeys) {
    	/*
        updateDirectionFlags();
        updateTagged();
        updateAnnotated();
    	 */
    }

    /**
     * Replies the name of this primitive. The default implementation replies the value
     * of the tag <tt>name</tt> or null, if this tag is not present.
     *
     * @return the name of this primitive
     */
    public String getName() {
        return get("name");
    }

    /*-------------------
     * OTHER PROPERTIES
     *-------------------*/

    /**
     * Unique identifier in OSM. This is used to identify objects on the server.
     * An id of 0 means an unknown id. The object has not been uploaded yet to
     * know what id it will get.
     */
    private long id = 0;

    /**
     * User that last modified this primitive, as specified by the server.
     * Never changed by JOSM.
     */
    private User user = null;

    /**
     * Contains the version number as returned by the API. Needed to
     * ensure update consistency
     */
    private int version = 0;

    /**
     * The id of the changeset this primitive was last uploaded to.
     * 0 if it wasn't uploaded to a changeset yet of if the changeset
     * id isn't known.
     */
    private int changesetId;

    private int timestamp;
    
    /*-------------------
     * FLAGS
     *-------------------*/
    
    protected boolean visible;
    
    protected boolean deleted;
   
    protected boolean incomplete;
    
    protected boolean modified;

    /**
     * Replies the version number as returned by the API. The version is 0 if the id is 0 or
     * if this primitive is incomplete.
     *
     * @see PrimitiveData#setVersion(int)
     */
    public int getVersion() {
        return version;
    }

    /**
     * Replies the id of this primitive.
     *
     * @return the id of this primitive.
     */
    public long getId() {
        long id = this.id;
        return id >= 0?id:0;
    }

    /**
     * Gets a unique id representing this object.
     *
     * @return Osm id if primitive already exists on the server. Unique negative value if primitive is new
     */
    public long getUniqueId() {
        return id;
    }

    /**
     *
     * @return True if primitive is new (not yet uploaded the server, id <= 0)
     */
    public boolean isNew() {
        return id <= 0;
    }

    /**
     * Sets the id and the version of this primitive if it is known to the OSM API.
     *
     * Since we know the id and its version it can't be incomplete anymore. incomplete
     * is set to false.
     *
     * @param id the id. > 0 required
     * @param version the version > 0 required
     * @throws IllegalArgumentException thrown if id <= 0
     * @throws IllegalArgumentException thrown if version <= 0
     * @throws DataIntegrityProblemException If id is changed and primitive was already added to the dataset
     */
    public void setOsmId(long id, int version) {
        if (id <= 0)
            throw new IllegalArgumentException("ID > 0 expected. Got " + id);
        if (version <= 0)
            throw new IllegalArgumentException("Version > 0 expected. Got " + version);
        this.id = id;
        this.version = version;
        //this.setIncomplete(false);
    }

    /**
     * Clears the id and version known to the OSM API. The id and the version is set to 0.
     * incomplete is set to false. It's preferred to use copy constructor with clearId set to true instead
     * of calling this method.
     */
    public void clearOsmId() {
        // Not part of dataset - no lock necessary
        this.id = generateUniqueId();
        this.version = 0;
        this.user = null;
        this.changesetId = 0; // reset changeset id on a new object
        //this.setIncomplete(false);
    }

    /**
     * Replies the user who has last touched this object. May be null.
     *
     * @return the user who has last touched this object. May be null.
     */
    public User getUser() {
        return user;
    }

    /**
     * Sets the user who has last touched this object.
     *
     * @param user the user
     */
    public void setUser(User user) {
        this.user = user;
    }

    /**
     * Replies the id of the changeset this primitive was last uploaded to.
     * 0 if this primitive wasn't uploaded to a changeset yet or if the
     * changeset isn't known.
     *
     * @return the id of the changeset this primitive was last uploaded to.
     */
    public int getChangesetId() {
        return changesetId;
    }

    /**
     * Sets the changeset id of this primitive. Can't be set on a new
     * primitive.
     *
     * @param changesetId the id. >= 0 required.
     * @throws IllegalStateException thrown if this primitive is new.
     * @throws IllegalArgumentException thrown if id < 0
     */
    public void setChangesetId(int changesetId) throws IllegalStateException, IllegalArgumentException {
        if (this.changesetId == changesetId)
            return;
        if (changesetId < 0)
            throw new IllegalArgumentException(MessageFormat.format("Parameter ''{0}'' >= 0 expected, got {1}", "changesetId", changesetId));
        if (isNew() && changesetId > 0)
            throw new IllegalStateException("Cannot assign a changesetId > 0 to a new primitive. Value of changesetId is " + changesetId);

        this.changesetId = changesetId;
    }

//    /**
//     * Replies the unique primitive id for this primitive
//     *
//     * @return the unique primitive id for this primitive
//     */
//    public PrimitiveId getPrimitiveId() {
//        return new SimplePrimitiveId(getUniqueId(), getType());
//    }
//
//    public OsmPrimitiveType getDisplayType() {
//        return getType();
//    }

    public void setTimestamp(Date timestamp) {
        this.timestamp = (int)(timestamp.getTime() / 1000);
    }

    /**
     * Time of last modification to this object. This is not set by JOSM but
     * read from the server and delivered back to the server unmodified. It is
     * used to check against edit conflicts.
     *
     * @return date of last modification
     */
    public Date getTimestamp() {
        return new Date(timestamp * 1000l);
    }

    public boolean isTimestampEmpty() {
        return timestamp == 0;
    }
    
    /**
     * Checks if object is known to the server.
     * Replies true if this primitive is either unknown to the server (i.e. its id
     * is 0) or it is known to the server and it hasn't be deleted on the server.
     * Replies false, if this primitive is known on the server and has been deleted
     * on the server.
     *
     * @return <code>true</code>, if the object is visible on server.
     * @see #setVisible(boolean)
     */
    public boolean isVisible() {
        //return (flags & FLAG_VISIBLE) != 0;
    	return this.visible;
    }

    /**
     * Sets whether this primitive is visible, i.e. whether it is known on the server
     * and not deleted on the server.
     *
     * @see #isVisible()
     * @throws IllegalStateException thrown if visible is set to false on an primitive with
     * id==0
     */
    public void setVisible(boolean visible) throws IllegalStateException{
        if (isNew() && visible == false)
            throw new IllegalStateException("A primitive with ID = 0 cannot be invisible.");
        //updateFlags(FLAG_VISIBLE, visible);
        this.visible = visible;
    }
    
    /**
     * Sets whether this primitive is deleted or not.
     *
     * Also marks this primitive as modified if deleted is true.
     *
     * @param deleted  true, if this primitive is deleted; false, otherwise
     */
    public void setDeleted(boolean deleted) {
        //updateFlags(FLAG_DELETED, deleted);
        //setModified(deleted ^ !isVisible());
    	this.deleted = deleted;
    }
    
    /**
     * Replies <code>true</code>, if the object has been deleted.
     *
     * @return <code>true</code>, if the object has been deleted.
     * @see #setDeleted(boolean)
     */
    public boolean isDeleted() {
        //return (flags & FLAG_DELETED) != 0;
    	return this.deleted;
    }

    /**
     * If set to true, this object is incomplete, which means only the id
     * and type is known (type is the objects instance class)
     */
    protected void setIncomplete(boolean incomplete) {
        //updateFlags(FLAG_INCOMPLETE, incomplete);
        this.incomplete = incomplete;
    }

    public boolean isIncomplete() {
        //return (flags & FLAG_INCOMPLETE) != 0;
        return incomplete;
    }

    /**
     * Marks this primitive as being modified.
     *
     * @param modified true, if this primitive is to be modified
     */
    public void setModified(boolean modified) {
        //updateFlags(FLAG_MODIFIED, modified);
    	this.modified = modified;
    }

    /**
     * Replies <code>true</code> if the object has been modified since it was loaded from
     * the server. In this case, on next upload, this object will be updated.
     *
     * Deleted objects are deleted from the server. If the objects are added (id=0),
     * the modified is ignored and the object is added to the server.
     *
     * @return <code>true</code> if the object has been modified since it was loaded from
     * the server
     */
    public boolean isModified() {
        //return (flags & FLAG_MODIFIED) != 0;
    	return modified;
    }

    public String getAllKeyValueTagsAsOneString(){
    	Map kvmap = this.getKeys();
        String outString = "";
        
        Iterator<String> keySetIterator = kvmap.keySet().iterator();
        while(keySetIterator.hasNext()){
        	if(outString.length() > 1){
        		outString = outString + ",";
        	}
        	String key = keySetIterator.next();
        	String value = (String)kvmap.get(key);
        	outString = outString + key + "=" + value;
        }
        return outString;
    }
    

}
