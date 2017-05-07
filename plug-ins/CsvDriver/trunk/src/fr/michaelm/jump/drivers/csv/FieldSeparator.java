/*
 * Library offering read and write capabilities for dsv formats
 * Copyright (C) 2017 Michaël MICHAUD
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

package fr.michaelm.jump.drivers.csv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * Field separator of a CSV File
 * @author Micha&euml;l MICHAUD
 * @version 0.6 (2012-03-25)
 */
public class FieldSeparator {
    
    public static final FieldSeparator TABULATION  = new FieldSeparator('\t');
    public static final FieldSeparator COMMA       = new FieldSeparator(',');
    public static final FieldSeparator SEMI_COLUMN = new FieldSeparator(';');
    public static final FieldSeparator PIPE        = new FieldSeparator('|');
    public static final FieldSeparator WHITESPACE  = new FieldSeparator(' ');
    
    private char separator;
    private Pattern fieldPattern;

    /**
     * No parameter constructor used for persistence in project files.
     */
    public FieldSeparator() {}

    public FieldSeparator(char separator) {
        this.separator = separator;
    }

    public char getSeparator() {
        return separator;
    }

    public void setSeparator(char sep) {
        this.separator = sep;
    }
    
    public Pattern getFieldPattern() {
        if (fieldPattern == null) {
            String fieldSeparatorPattern = Pattern.quote(""+separator);
            fieldPattern = Pattern.compile(
                // A field starts just after the begining of the string or after a separator
                "(?<=^|" + fieldSeparatorPattern + ")" +
                // A field excludes non printable characters (except ws and   \t)
                // It must be quoted if it includes a double quote or a separator
                // included quotes must be doubled
                "(?:\"((?:[^\\x00-\\x1F\"]|\"\"|\\s)*+)\"|" +
                // If a field is not quoted, it can not include double quote
                "([^\\x00-\\x1F" + fieldSeparatorPattern + "]*))" +
                // A field ends with a separator (not included between double quotes)
                // or with the end of the string
                "(?:" + fieldSeparatorPattern + "|$)"
            );
        }
        return fieldPattern;
    }
    
    public String[] getFields(String line) {
        List<String> tokens = new ArrayList<String>();
        Matcher matcher = getFieldPattern().matcher(line);
        while (!matcher.hitEnd() && matcher.find()) {
            String token = matcher.group(1)!=null ?
                    matcher.group(1).replaceAll("\"\"","\""):
                    matcher.group(2);
            tokens.add(token);
        }
        return tokens.toArray(new String[tokens.size()]);
    }

    public String toString() {
        if (separator == '\t') return "{tab}";
        if (separator == ' ')  return "{whitespace}";
        return "" + separator;
    }

    public static void main(String[] args) {
        System.out.println(new FieldSeparator('\t').getFieldPattern().toString());
        FieldSeparator fs = new FieldSeparator('\t');
        System.out.println(Arrays.toString(fs.getFields("toto\ttata\ttiti")));
        System.out.println(Arrays.toString(fs.getFields("toto 1\ttata 2\ttiti 3")));
        System.out.println(Arrays.toString(fs.getFields("toto 1\ttata \"2\"\ttiti 3")));
        System.out.println(Arrays.toString(fs.getFields("toto 1\t\"tata \"\"2\"\"\"\ttiti 3")));
        System.out.println(Arrays.toString(fs.getFields("toto 1\t\"tata\t2\"\ttiti 3")));
    }

}