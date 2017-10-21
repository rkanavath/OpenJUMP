package org.openjump.core.ui.plugin.colorchooser.utils;

import java.awt.Color;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Scanner;

public class ColorUtils {

    public static String getColorIndexRegistry(String searchQuery) {

        InputStream is = ColorUtils.class.getResourceAsStream("color.txt");
        InputStreamReader isr = new InputStreamReader(is);
        String ColorDef = "";
        Scanner scanner = null;
        String line2 = "";
        try {
            scanner = new Scanner(isr);
            while (scanner.hasNextLine()) {
                scanner.useDelimiter("\\n");
                String line = scanner.nextLine();
                if (line.toLowerCase().contains(
                        "<" + searchQuery.toLowerCase() + ">"))

                {
                    line2 = line.replaceAll("<" + searchQuery.toLowerCase()
                            + ">", "");
                    int start = line2.indexOf('<');
                    int end = line2.indexOf('>', start);
                    String def = line2.substring(start + 1, end);
                    try {

                        // EPSG code between 0 and 32767
                        ColorDef = def.replaceAll("[<\\>]", "").replaceAll(";",
                                "");

                        // Non numeral SRID like INGF
                    } catch (NumberFormatException e) {
                        ColorDef = "";
                    }
                    break;
                } else {

                    ColorDef = "";
                }
            }
        } finally {
            try {
                if (scanner != null)
                    scanner.close();
            } catch (Exception e) {
                System.err.println("Exception while closing scanner "
                        + e.toString());
            }
        }
        return ColorDef;
    }

    /**
     * get hexadecimal color value (es FFFFFF) from color.java class (in this
     * case Color.black)
     * 
     * @param Color
     * @return String
     */

    public static String colorRGBToHex(Color color) {
        if (color == null) {
            return "";
        }
        int r = color.getRed();
        int g = color.getGreen();
        int b = color.getBlue();
        String rHex = Integer.toHexString(r).toUpperCase();
        String gHex = Integer.toHexString(g).toUpperCase();
        String bHex = Integer.toHexString(b).toUpperCase();
        if (rHex.length() < 2)
            rHex = "0" + rHex;
        if (gHex.length() < 2)
            gHex = "0" + gHex;
        if (bHex.length() < 2)
            bHex = "0" + bHex;
        return rHex + gHex + bHex;
    }

    /**
     * get Color.java (ex. color.black) from hexadecimal value (es. FFFFFF)
     * 
     * @param String
     * @return Color
     */
    public static Color hexToColorRGB(String hex) {
        hex = "#" + hex;
        return new Color(Integer.valueOf(hex.substring(1, 3), 16),
                Integer.valueOf(hex.substring(3, 5), 16), Integer.valueOf(
                        hex.substring(5, 7), 16));
    }

    /**
     * Convert hexadecimal String (ex. "FFFFFF") color to RGB String (ex.
     * "0,0,0")
     * 
     * @param String
     * @return String
     */
    public static String hex2Rgb(String hex) {
        hex = "#" + hex;
        Color c = new Color(Integer.valueOf(hex.substring(1, 3), 16),
                Integer.valueOf(hex.substring(3, 5), 16), Integer.valueOf(
                        hex.substring(5, 7), 16));

        StringBuffer sb = new StringBuffer();
        sb.append(c.getRed());
        sb.append(",");
        sb.append(c.getGreen());
        sb.append(",");
        sb.append(c.getBlue());
        return sb.toString();
    }

}
