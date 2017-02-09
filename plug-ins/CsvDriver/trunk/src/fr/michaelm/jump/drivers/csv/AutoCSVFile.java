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

import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.io.CompressedFile;

import java.io.*;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static fr.michaelm.jump.drivers.csv.FieldSeparator.*;

/**
 * An extension of CSVFile with methods to guess the internal structure of the
 * file to parse.
 * @author Micha&euml;l MICHAUD
 */
public class AutoCSVFile extends CSVFile {

    // Pattern to differenciate a comment line Pattern starting with #
    // from a header line Pattern starting with #FID or #X (old xyz format)
    private final static Pattern SHARP_PATTERN = Pattern.compile("^#(?!(FID|X)[\t,;\\| ])");
    
    // Pattern matching a unquoted, integer, decimal or scientic number
    // A non-comment line containing such a pattern is considered as a data line
    private final static Pattern NUMBER_PATTERN = Pattern.compile("[\\s\\|,;]-?\\d+(\\.\\d+([eE][-\\+]\\d+)?)?[\\s\\|,;]");

    // Pattern matching a WKT string
    // A non-comment line containing such a pattern is considered as a data line
    private final static Pattern WKT_PATTERN = Pattern.compile("(((MULTI)?(POINT|LINESTRING|POLYGON))|GEOMETRYCOLLECTION) ?( EMPTY|\\([\\(\\)\\d,\\. ]*\\))");


    /** No parameter constructor for persitence in a project file.*/
    public AutoCSVFile() {
        super();
    }


    /** Create a AutoCSVFile from the file denoted by this filePath.*/
    public AutoCSVFile(String filePath) throws IOException, CSVFileException {
        super(filePath);
    }


    /**
     * Create a AutoCSVFile from the file species by this filePath and,
     * if filePath specifies a compressed file, by this entryName.
     */
    public AutoCSVFile(String filePath, String entryName) throws IOException, CSVFileException {
        super(filePath, entryName);
    }


    protected void init() throws IOException, CSVFileException {
        // abandon guessEncoding, it is a dangerous guess...
        // setEncoding(guessEncoding());
        setEncoding(Charset.defaultCharset().name());
        readHeaderLines();
        initialized = true;
    }


    /**
     * Test to guess the encoding of a file (taken from
     * neoedmund'editor at http://code.google.com/p/neoeedit/)
     * @Deprecated too dangerous, just use the system default, it can be forced
     * in the command line
     */
    private String guessEncoding() throws IOException {
        // Main multi-bytes encodings
        String local_charset = Charset.defaultCharset().name();
        String[] encodings = {"UTF-8", local_charset, "Shift_JIS", "UTF-16"};
        InputStream in = CompressedFile.openFile(getFilePath(), getEntryName());
        final int defsize = 4096*2;
        int len = Math.min(defsize, (int)new File(getFilePath()).length());
        try {
            byte[] buf = new byte[len];
            len = in.read(buf);
            if (len != defsize) {
                byte[] b2 = new byte[len];
                System.arraycopy(buf, 0, b2, 0, len);
                buf = b2;
            }
            for (String enc : encodings) {                          
                String s = new String(buf, enc);
                // \ufffd is the replacement character used to replace an unknown or unprintable character
                if (new String(s.getBytes(enc), enc).equals(s) && !s.contains("\ufffd")) {
                    return enc;
                }
            }
        } finally {
            in.close();
        }
        return local_charset;
    }
    
    
    private void readHeaderLines() throws IOException, CSVFileException {
        InputStream is = CompressedFile.openFile(getFilePath(), getEntryName());
        BufferedReader br = new BufferedReader(new InputStreamReader(is, getCharset()));
        // line1 will try to get the header line containing column names (if any)
        String line1 = null;
        // line2 will try to get the data type line (if any)
        String line2 = null;
        int count = 0;      // count all read lines
        int nonComment = 0; // count only non comment lines
        boolean pirol = false;
        List<String> lines = new ArrayList<String>();
        // read until first non empty line to guess/set the comment line pattern
        // if 1st non empty line starts with $, read it as a pirol header line
        // if no comment mark is found read first non empty line a header line
        String currentLine;
        while (null != (currentLine=br.readLine())) {
            count++;
            currentLine = currentLine.trim();
            if (currentLine.length()==0) continue;  // while line is empty, continue
            else if (currentLine.startsWith("//")) {
                setCommentLinePattern(Pattern.compile("^//"));
            }
            else if (currentLine.startsWith("--")) {
                setCommentLinePattern(Pattern.compile("^--"));
            }
            else if (SHARP_PATTERN.matcher(currentLine).find()) {
                setCommentLinePattern(SHARP_PATTERN);
            }
            else if (currentLine.startsWith("$")) {
                if (count == 1) {
                    line1 = currentLine.substring(1);
                    continue;
                }
                if (count == 2) continue;
                if (count == 3) {
                    line2 = currentLine.substring(1);
                    setCommentLinePattern(Pattern.compile("^\\$"));
                    pirol = true;
                    nonComment += 2;
                    break;
                }
            }
            else {
                setCommentLinePattern(Pattern.compile("^###COMMENT###$"));
                line1 = currentLine;  // if first non empty line is not a comment line 
                nonComment++;  // it is considered as the column header line
                lines.add(currentLine);
            }
            break; // break after first non empty line
        }
        
        // set line1 as the first non empty, non comment line
        // set line2 as the second non empty, non comment line
        while (null != (currentLine=br.readLine()) && nonComment < 5) {
            count++;
            if (getCommentLinePattern().matcher(currentLine).find()) continue;
            if (nonComment == 0) line1 = currentLine;
            else if (nonComment == 1) line2 = currentLine;
            lines.add(currentLine);
            nonComment++;
        }
        br.close();
        guessFieldSeparator(lines.toArray(new String[lines.size()]));
        setColumns(line1);
        setHeaderLine(hasHeaderLine() && !pirol);
        setAttributeTypes(line2);
        guessGeometryColumns(line2);
    }
    
    /**
     * Tries to tokenize these lines with 5 different separators (tab, comma,
     * semi-column, pipe and whitespace) and returns the separator producing
     * constant length arrays of tokens. If several separators produce
     * constant length arrays, the one producing the greatest number of 
     * tokens is returned.
     * @throws CSVFileException if the fieldSeparator could not be determined
     */
    protected boolean guessFieldSeparator(String[] lines) {
        // if no lines are provided, return the current fieldSeparator 
        if (lines.length == 0) return false;
        FieldSeparator[] separators = new FieldSeparator[]{TABULATION, COMMA, SEMI_COLUMN, PIPE, WHITESPACE};
        List<Set<Integer>> counts = new ArrayList<Set<Integer>>();
        for (int i = 0 ; i < separators.length ; i++) {
            Pattern _fieldPattern = separators[i].getFieldPattern();
            counts.add(new HashSet<Integer>());
            for (String line : lines) {
                if (!getCommentLinePattern().matcher(line).find()) {
                   counts.get(i).add(countFields(line, _fieldPattern));
                }
            }
        }
        int index = -1;
        int fieldNumber = 0;
        for (int i = 0 ; i < counts.size() ; i++) {
            Set<Integer> tokenNumbers = counts.get(i);
            if (tokenNumbers.size() == 1) {
                int number = tokenNumbers.iterator().next();
                if (number > fieldNumber) {
                    index = i;
                    fieldNumber = number;
                }
            }
        }
        if (index == -1) return false;
        setFieldSeparator(separators[index]);
        return true;
    }
    
    /** 
     * Parse a line to compute the field number.</p>
     * Method based on the RFC 4180 BNF of csv format found at 
     * <a href="http://tools.ietf.org/html/rfc4180">http://tools.ietf.org/html/rfc4180</a></p>
     * Note that this parser does not follow 100% the spec as it does not allow 
     * line breaks in a field.
     */
    private static int countFields(String line, Pattern pattern) {
        Matcher matcher = pattern.matcher(line);
        int count = 0;
        while (!matcher.hitEnd() && matcher.find()) {
            count++;
        }
        return count;
    }

    
    protected void setColumns(String line) throws IOException, CSVFileException {

        if (line != null /*&& getFeatureSchema() == null*/) {
            boolean header = !NUMBER_PATTERN.matcher(line).find() &&
                             !WKT_PATTERN.matcher(line).find();
            setHeaderLine(header);
            super.setColumns(line);
        }
    }
    
    /**
     * Try to guess geometry columns from the column names and create the 
     * FeatureSchema
     */ 
    protected void guessGeometryColumns(String line) throws CSVFileException, IOException {
        String[] columns = getColumns();
        if (columns != null && columns.length>0) {
            int wkt = -1; 
            int x = -1;
            int y = -1;
            int z = -1;
            // Try to guess geometry columns from the column names.
            for (int i = 0 ; i < columns.length ; i++) {
                String name = columns[i];
                if (name.matches("(?i)#?X|LON(G(IT(UDE)?)?)?|EAST(ING)?")) x = i;
                else if (name.matches("(?i)#?Y|LAT(IT(UDE)?)?|NORTH(ING)?")) y = i;
                else if (name.matches("(?i)#?Z|ALT(ITUDE)?")) z = i;
                else if (name.matches("(?i)GEOM.*|WKT")) wkt = i;
            }
            if (wkt != -1) setGeometryColumns(new int[]{wkt});
            else if (x != -1 && y != -1) {
                if (z != -1) setGeometryColumns(new int[]{x, y, z});
                else setGeometryColumns(new int[]{x, y});
            }
            FeatureSchema schema = new FeatureSchema();
            AttributeType[] types = getAttributeTypes();
            String[] data = line != null ? tokenize(line) : null;
            for (int i = 0 ; i < columns.length ; i++) {
                if (i == wkt) {
                    schema.addAttribute(columns[i], AttributeType.GEOMETRY);
                }
                else if (i == x && y != -1 && wkt == -1) {
                    schema.addAttribute("GEOMETRY", AttributeType.GEOMETRY);
                }
                else if ((i == y || i == z) && x != -1 && wkt == -1) {
                }
                else if (wkt+x+y == -3 && 
                         data != null && 
                         schema.getGeometryIndex() == -1 &&
                         // no geometry attribute detected in the header
                         // but a wkt string detected in the data
                         WKT_PATTERN.matcher(data[i]).matches()) {
                    schema.addAttribute("WKT", AttributeType.GEOMETRY);
                    setGeometryColumns(new int[]{i});
                }
                else {
                    AttributeType type = (types != null && types.length > i) ? types[i] : AttributeType.STRING;
                    schema.addAttribute(columns[i], type);
                }
            }
            if (schema.getGeometryIndex() == -1) {
                schema.addAttribute("GEOMETRY", AttributeType.GEOMETRY);
            }
            setFeatureSchema(schema);
        }
    }

}
