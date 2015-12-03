package org.openjump.ext.viewmanager.style;

import java.awt.*;

/**
 * Created by UMichael on 13/06/2015.
 */
public class ColorUtil {

    public static Color decode(String color) throws NumberFormatException {
        return color == null ? null : Color.decode(color);
    }

    public static String encode(Color color) {
        return "#" + String.format("%08x", color.getRGB()).substring(2);
    }
}
