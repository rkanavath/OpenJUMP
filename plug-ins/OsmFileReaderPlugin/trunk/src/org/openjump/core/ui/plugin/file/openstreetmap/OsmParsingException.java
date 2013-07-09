package org.openjump.core.ui.plugin.file.openstreetmap;

import javax.xml.stream.Location;
import javax.xml.stream.XMLStreamException;

class OsmParsingException extends XMLStreamException {
    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public OsmParsingException() {
        super();
    }

    public OsmParsingException(String msg) {
        super(msg);
    }

    public OsmParsingException(String msg, Location location) {
        super(msg); /* cannot use super(msg, location) because it messes with the message preventing localization */
        this.location = location;
    }

    public OsmParsingException(String msg, Location location, Throwable th) {
        super(msg, th);
        this.location = location;
    }

    public OsmParsingException(String msg, Throwable th) {
        super(msg, th);
    }

    public OsmParsingException(Throwable th) {
        super(th);
    }

    @Override
    public String getMessage() {
        String msg = super.getMessage();
        if (msg == null) {
            msg = getClass().getName();
        }
        if (getLocation() == null)
            return msg;
        msg = msg + "(at line " + getLocation().getLineNumber() + ", column " + getLocation().getColumnNumber() + ")";
        return msg;
    }
}
