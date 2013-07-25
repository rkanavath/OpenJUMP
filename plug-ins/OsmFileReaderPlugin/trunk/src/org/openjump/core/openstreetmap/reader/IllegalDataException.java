/* 
 * License: GPL. See LICENSE file for details.
 * File from JOSM project. 25.July.2013
 */
package org.openjump.core.openstreetmap.reader;

public class IllegalDataException extends Exception{

    public IllegalDataException() {
        super();
    }

    public IllegalDataException(String message, Throwable cause) {
        super(message, cause);
    }

    public IllegalDataException(String message) {
        super(message);
    }

    public IllegalDataException(Throwable cause) {
        super(cause);
    }

}
