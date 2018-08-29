package com.geomaticaeambiente.klemgui.exceptions;

import com.geomaticaeambiente.klemgui.exceptions.MessageTypes.MessageType;

/**
 *
 * @author deluca
 */
public class WarningException extends Exception {

    public WarningException() {
    }

    public WarningException(String message) {
        super(message);
    }
    
    public final MessageType MESSAGETYPE = MessageType.WARNING;
    
}