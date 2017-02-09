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

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKTReader;
import com.vividsolutions.jts.io.WKTWriter;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.io.CompressedFile;
import com.vividsolutions.jump.util.FlexibleDateParser;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;
import java.nio.charset.UnsupportedCharsetException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static fr.michaelm.jump.drivers.csv.FieldSeparator.COMMA;


/**
 * A generic class used to read CSV files containing geographic data.
 * This class can be extended by specialized classes dedicated to specific
 * formats.
 * @author Micha&euml;l MICHAUD
 * @version 0.9 (2014-05-14)
 */
// 0.9 updates to manage compressed files
// 0.6 is a complete re-write of the text driver for OpenJUMP
//     it includes reading of Pirol csv formatted files, and 
//     could read the csvt (vrt ?) files used by OGR tools in
//     the future
public class CSVFile {

    private static final WKTReader WKT_READER = new WKTReader();
    private static final WKTWriter WKT_WRITER = new WKTWriter();
    private static final GeometryFactory GEOMETRY_FACTORY = new GeometryFactory();
    private static final FlexibleDateParser DATE_PARSER = new FlexibleDateParser();
    private static final String LINE_SEPARATOR = System.getProperty("line.separator");
    
    // Main file properties
    private String filePath;
    private String entryName; // if the filePath specifies a compressed file
    private Charset charset = Charset.defaultCharset();
    private Pattern commentLinePattern = Pattern.compile("^#");
    private FieldSeparator fieldSeparator = COMMA;
    
    //int firstDataLine = 0; // First line containing data (1-based)
    private boolean headerLine = false;
    private boolean dataTypeLine = false;
    private String[] columns;
    private AttributeType[] dataTypes;
    private int[] geometryColumns = new int[0];
    //private HashMap columnMapping;
    private FeatureSchema schema;

    boolean initialized = false;

    private final List<Exception> exceptions = new ArrayList<Exception>();
    
    
    /**
     * No argument constructor.</p>
     * Useful for csvt format where the csv file name is read from a file in
     * csvt format, along with other CSV file properties.
     */
    public CSVFile() {
    }
    
    
    /**
     * @param filePath the path of the CSV file to read
     */
    public CSVFile(String filePath) throws IOException, CSVFileException {
        setFilePath(filePath);
        init();
    }

    /**
     * @param filePath the path of the CSV file to read
     */
    public CSVFile(String filePath, String entryName) throws IOException, CSVFileException {
        setFilePath(filePath);
        setEntryName(entryName);
        init();
    }
    
    
    /**
     * This method can be overloaded by derived classes to set one or several
     * parameters from a configuration file or from the file header.
     */
    protected void init() throws IOException, CSVFileException {
        initialized = true;
    }

    
    /**
     * Get the file path of this CSV file to read.
     */
    public String getFilePath() {
        return filePath;
    }


    /**
     * Returns the entryName of this CSV resource if the filePath specifies
     * a compressed file. Return null otherwise.
     */
    public String getEntryName() { return entryName; }

    
    /**
     * Set this CSV file path.
     */
    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }


    /**
     * Set the entryName of this CSV resource in a compressed file.
     */
    public void setEntryName(String entryName) {
        this.entryName = entryName;
    }


    /**
     * Get this CSV driver charset.
     */
    public Charset getCharset() {
        return charset;
    }


    /**
     * Get this CSV driver encoding name.
     */
    public String getEncoding() {
        return charset.name();
    }
    
    
    /**
     * Set this CSV driver encoding to charsetName.
     * @param charsetName the charset, or encoding to use to read this CSV file.
     * It may be one of UTF-8, windows-1252, ISO-8859-1 or any of the charset
     * recognized by the java virtual machine.
     * @throws IllegalCharsetNameException if the given charset name is illegal
     * @throws UnsupportedCharsetException if the given charset name is unknown 
     */
    public void setEncoding(String charsetName) 
            throws IllegalCharsetNameException, UnsupportedCharsetException{
        this.charset = Charset.forName(charsetName);
    }
    
    
    /**
     * Get the comment line Pattern.</p>
     * This pattern can be used with the find method to know if a line of this 
     * CSV file is a comment line or not.
     */
    public Pattern getCommentLinePattern() {
        return commentLinePattern;
    }
    
    
    /**
     * @param commentLinePattern Pattern to use to determine if a line of this 
     * CSV file is a comment line or not.
     */
    public void setCommentLinePattern(Pattern commentLinePattern) {
        this.commentLinePattern = commentLinePattern;
    }
    
    
    /**
     * Get this csv field separator (one of tab, comma, semi-column, pipe or
     * whitespace.
     */
    public FieldSeparator getFieldSeparator() {
        return fieldSeparator;
    }
    
    
    /**
     * @param fieldSeparator single character used as a field separator in this
     * CSV file.
     */
    public void setFieldSeparator(FieldSeparator fieldSeparator) {
        this.fieldSeparator = fieldSeparator;
    }
    
    
    /**
     * Set if this file has a header line containing column names or not.
     */
    public void setHeaderLine(boolean headerLine) {
        this.headerLine = headerLine;
    }
    
    
    /**
     * @return true is the file has a header line containing column names,
     * false otherwise. The column names header line must be the first one after
     * comment lines.
     */
    public boolean hasHeaderLine() {
        return headerLine;
    }


    /** java2xml compatible version of hasHeaderLine. */
    public boolean isHeaderLine() {
        return headerLine;
    }


    /**
     * Set if this file has a line containing column data types or not.
     */
    public void setDataTypeLine(boolean dataTypeLine) {
        this.dataTypeLine = dataTypeLine;
    }
    
    
    /**
     * @return true is the file has a line containing column data types,
     * false otherwise. The column data types must be the first one after
     * the header line containing column names.
     */
    public boolean hasDataTypeLine() {
        return dataTypeLine;
    }


    /** java2xml compatible version of hasDataTypeLine. */
    public boolean isDataTypeLine() {
        return dataTypeLine;
    }



    /**
     * Tokenize the line using this CSVFile fieldSeparator.
     */
    public String[] tokenize(String line) {
        List<String> tokens = new ArrayList<String>();
        Matcher matcher = fieldSeparator.getFieldPattern().matcher(line);
        while (!matcher.hitEnd() && matcher.find()) {
            String token = matcher.group(1)!=null ? matcher.group(1) : 
                           matcher.group(2).replaceAll("\"\"","\"");
            tokens.add(token);
        }
        return tokens.toArray(new String[tokens.size()]);
    }
    
    
    /**
     * Set this CSVFile column names.
     */
    public void setColumns(String[] columns) {
        this.columns = columns;
    }
    
    
    /**
     * Get this CSVFile column names.
     */    
    public String[] getColumns() {
        return columns;
    }
    
    
    public void setAttributeTypes(AttributeType[] dataTypes) {
        this.dataTypes = dataTypes;
    }
    
    
    public AttributeType[] getAttributeTypes() {
        return dataTypes;
    }
    
    
    public void setGeometryColumns(int[] geometryColumns) {
        this.geometryColumns = geometryColumns;
    }
    
    
    public int[] getGeometryColumns() {
        return geometryColumns;
    }
    
    
    /**
     * Set the FeatureSchema from the header.
     */
    public void setFeatureSchema() throws IOException, CSVFileException {
        if (schema != null) return;
        if (!initialized) init();
        InputStream in = null;
        BufferedReader br = null;
        try {
            in = CompressedFile.openFile(getFilePath(), getEntryName());
            br = new BufferedReader(new InputStreamReader(in, getCharset()));
            String line;
            String line1 = null;
            String line2 = null;
            int count = 0;      // count all line
            // read until first non empty line 
            while (null != (line=br.readLine())) {
                if (line.trim().length() == 0) continue;
                if (commentLinePattern.matcher(line).find()) continue;
                count++;
                if (count == 1) {
                    line1 = line;
                    if (!dataTypeLine) break;
                }
                else if (count == 2 && dataTypeLine) {
                    line2 = line;
                    break;
                }
                else break; // count > 2
            }
            if (line1 == null) {
                throw new CSVFileException(CSVFileException.NO_DATA_FOUND, this);
            }
            setColumns(line1);
            setAttributeTypes(line2);
            schema = new FeatureSchema();
            List<Integer> gclist = new ArrayList<>();
            for (int i : geometryColumns) gclist.add(i);
            for (int i = 0 ; i < columns.length ; i++) {
                if (gclist.contains(i)) {
                    if (geometryColumns.length == 1) {
                        schema.addAttribute(columns[i], AttributeType.GEOMETRY);
                    }
                    else if (schema.getGeometryIndex() == -1) {
                        schema.addAttribute("GEOMETRY", AttributeType.GEOMETRY);
                    }
                }
                else {
                    AttributeType type = (dataTypes != null && dataTypes.length > i) ? dataTypes[i] : AttributeType.STRING;
                    schema.addAttribute(columns[i], type);
                }
            }
            // if schema has no geometry attribute, create one which will be filled
            // with empty GeometryCollection
            if (schema.getGeometryIndex() == -1) {
                schema.addAttribute("GEOMETRY", AttributeType.GEOMETRY);
            }
        }
        finally {
            if (br != null) br.close();
            if (in != null) in.close();
        }
    }
    
    
    /**
     * Set column names from the header line or set default names if headerLine
     * is false.
     */
    protected void setColumns(String line) throws IOException, CSVFileException {
        if (line != null) {
            columns = tokenize(line);
            if (!headerLine) {
                for (int i = 0 ; i < columns.length ; i++) {
                    columns[i] = "Col"+(i+1);
                }
            }
        }
    }
    
    
    protected void setAttributeTypes(String line) {
        if (line != null && schema == null) {
            List<AttributeType> typeList = new ArrayList<AttributeType>();
            Matcher matcher = getFieldSeparator().getFieldPattern().matcher(line);
            boolean typed = true;
            while (!matcher.hitEnd() && matcher.find()) {
                String type = matcher.group(1)!=null ? matcher.group(1) : matcher.group(2);
                if (type.matches("(?i)(STRING|CHAR|VARCHAR)(\\([\\d\\.]+\\))?")) {
                    typeList.add(AttributeType.STRING);
                }
                else if (type.matches("(?i)(DOUBLE|DECIMAL|NUMERIC|FLOAT|REAL)(\\([\\d\\.]+\\))?")) {
                    typeList.add(AttributeType.DOUBLE);
                }
                else if (type.matches("(?i)(INT(EGER)?|LONG)(\\([\\d\\.]+\\))?")) {
                    typeList.add(AttributeType.INTEGER);
                }
                else if (type.matches("(?i)(DATE|TIME)")) {
                    typeList.add(AttributeType.DATE);
                }
                else if (type.matches("(?i)(GEOMETRY)")) {
                    typeList.add(AttributeType.GEOMETRY);
                }
                else {
                    typeList.add(AttributeType.STRING);
                    typed = false;
                }
            }
            setAttributeTypes(typeList.toArray(new AttributeType[getColumns().length]));
            if (typed) setDataTypeLine(headerLine);
        }
    }
    
    
    public void setFeatureSchema(FeatureSchema schema) {
        this.schema = schema;
    }
    
    
    public FeatureSchema getFeatureSchema() throws IOException, CSVFileException {
        if (schema == null) setFeatureSchema();
        return schema;
    }
    
    
    /**
     * @return an Iterator iterating over all records of this csv file.</p>
     * Exceptions are not thrown but added to the CSVFile exception list.
     */
    public Iterator<Feature> iterator() throws IOException, CSVFileException {
        if (!initialized) init();
        if (schema == null) {
            throw new CSVFileException(CSVFileException.DRIVER_NOT_CONFIGURED, this);
        }
        exceptions.clear();
        
        return new Iterator<Feature>() {
            String line;
            InputStream in;
            BufferedReader br;
            {
                // Skip comment and header lines
                int headerSize = headerLine ? 1 : 0;
                headerSize = dataTypeLine ? 2 : headerSize;
                in = CompressedFile.openFile(getFilePath(), getEntryName());
                br = new BufferedReader(new InputStreamReader(in, getCharset()));
                int count = 0;
                while (null != (line=br.readLine())) {
                    if (line.trim().length() == 0) continue;
                    if (commentLinePattern.matcher(line).find()) continue;
                    if (++count > headerSize) break;
                }
            }
            
            public boolean hasNext() {
                if (br!=null && line==null) {
                    try {br.close();}
                    catch(IOException e) {}
                }
                if (in!=null && line==null) {
                    try {in.close();}
                    catch(IOException e) {}
                }
                return line != null;
            }
            
            public Feature next() {
                if (line != null) {
                    Feature feature = null;
                    try {
                        feature = getFeature(line);
                    } catch(Exception e) {
                        exceptions.add(e);
                        return null;
                    } finally {
                        try {
                            while (null != (line = br.readLine())) {
                                if (!commentLinePattern.matcher(line).find() &&
                                    line.trim().length() > 0) break;
                            }
                        } catch (IOException e) {/*eat it*/}
                    }
                    return feature;
                }
                else throw new NoSuchElementException();
            }
            
            public void remove() {
                throw new UnsupportedOperationException("Cannot remove a line from a read-only source");
            }
            
        };
        
    }
    

    /** @return a Feature from a record line.*/
    public Feature getFeature(String line) throws com.vividsolutions.jts.io.ParseException, CSVFileException, IOException {
        String[] fields = tokenize(line);
        Feature feature = new BasicFeature(schema);
        feature.setGeometry(getGeometry(fields));
        for (int i = 0 ; i < fields.length ; i++) {
            boolean geom = false; // this is not a geometry column
            for (int j = 0 ; j < geometryColumns.length ; j++) {
                if (geometryColumns[j] == i) {
                    geom = true; 
                    break;
                }
            }
            if (geom) continue; // this is a geometry column
            String name = columns[i];
            if (schema.hasAttribute(name)) {
                AttributeType type = null;
                try {
                    type = schema.getAttributeType(name);
                    if (type == AttributeType.STRING) feature.setAttribute(name, fields[i]);
                    else if (fields[i] == null || fields[i].trim().equals("")) feature.setAttribute(name, null);
                    else if (type == AttributeType.DOUBLE) feature.setAttribute(name, Double.parseDouble(fields[i]));
                    else if (type == AttributeType.INTEGER) feature.setAttribute(name, Integer.parseInt(fields[i]));
                    else if (type == AttributeType.DATE) feature.setAttribute(name, DATE_PARSER.parse(fields[i], false));
                    else feature.setAttribute(name, fields[i]);
                } catch (Exception e) {
                    exceptions.add(new CSVFileException("Feature " + feature.getID() + 
                        ": could not parse \"" + fields[i] + "\" as a " + getType(type)));
                }
            }
        }
        return feature;
    }
    

    /** Get the Geometry from an array of String, using the geometryColumns attribute of this CSVFile.*/
    private Geometry getGeometry(String[] fields) throws com.vividsolutions.jts.io.ParseException, NumberFormatException, ArrayIndexOutOfBoundsException {
        if (geometryColumns.length == 1) {
            if (fields.length <= geometryColumns[0])
                throw new IndexOutOfBoundsException("Field " + geometryColumns[0] + " is needed for geometry but "
                        + Arrays.toString(fields) + " has only " + fields.length + " fields");
            try {
                return WKT_READER.read(fields[geometryColumns[0]]);
            } catch (ParseException e) {
                throw new ParseException("Can't parse string '" + fields[geometryColumns[0]] +
                    "' as double with this CSVFile using fieldSeparator '" + fieldSeparator + "'");
            }
        }
        else if (geometryColumns.length == 2) {
            if (fields.length <= geometryColumns[0] || fields.length <= geometryColumns[1])
                throw new IndexOutOfBoundsException("Fields " + geometryColumns[0] + " and " + geometryColumns[1]
                        + " are needed for geometry but " + Arrays.toString(fields) + " has only " + fields.length + " fields");
            return GEOMETRY_FACTORY.createPoint(new Coordinate(
                Double.parseDouble(fields[geometryColumns[0]]),
                Double.parseDouble(fields[geometryColumns[1]])));
        }
        else if (geometryColumns.length == 3) {
            if (fields.length <= geometryColumns[0] || fields.length <= geometryColumns[1] || fields.length <= geometryColumns[2])
                throw new IndexOutOfBoundsException("Fields " + geometryColumns[0] + " and " + geometryColumns[1]
                        + " and " + geometryColumns[2] + " is needed for geometry but " + Arrays.toString(fields)
                        + " has only " + fields.length + " fields");
            return GEOMETRY_FACTORY.createPoint(new Coordinate(
                Double.parseDouble(fields[geometryColumns[0]]),
                Double.parseDouble(fields[geometryColumns[1]]),
                Double.parseDouble(fields[geometryColumns[2]])));
        }
        else return GEOMETRY_FACTORY.createGeometryCollection(new Geometry[0]); 
    }


    /** Write a line containing column names to writer.*/
    public void writeSchema(Writer writer) throws IOException {
        if (writer != null && headerLine) {
            for (int i = 0 ; i < columns.length ; i++) {
                String name = columns[i];
                // if the field name contains a quote or a fieldSeparator
                boolean quote = name.contains("\"") ||
                        name.indexOf(fieldSeparator.getSeparator()) > -1;
                // -> field name is written between double quotes
                if (quote) name = "\"" + name.replaceAll("\"","\"\"") + "\"";
                writer.write(name);
                if (i < columns.length-1) writer.write(fieldSeparator.getSeparator());
            }
            writer.write(LINE_SEPARATOR);
        }
    }
    

    /** Write a line containing data types to writer.*/
    public void writeDataTypes(Writer writer) throws IOException {
        if (writer != null && dataTypeLine) {
            for (int i = 0 ; i < dataTypes.length ; i++) {
                writer.write(getType(dataTypes[i]));
                if (i < dataTypes.length-1) writer.write(fieldSeparator.getSeparator());
            }
            writer.write(LINE_SEPARATOR);
        }
    }
    
    
    private String getType(AttributeType type) {
        if (type.equals(AttributeType.STRING)) return "STRING";
        else if (type.equals(AttributeType.INTEGER)) return "INTEGER";
        else if (type.equals(AttributeType.DOUBLE)) return "DOUBLE";
        else if (type.equals(AttributeType.DATE)) return "DATE";
        else if (type.equals(AttributeType.GEOMETRY)) return "GEOMETRY";
        else return "STRING";
    }


    /**
     * Write a Feature in the CSV format denoted by this CSVFile object.
     * @param writer the writer to write to
     * @param feature the feature to write
     * @throws IOException
     */
    public void writeFeature(Writer writer, Feature feature) throws IOException {
        WKTWriter wktw = new WKTWriter(3);
        if (writer != null) {
            for (int i = 0 ; i < columns.length ; i++) {
                if (columns[i].equals("X")) {
                    writer.write("" + feature.getGeometry().getCoordinate().x);
                }
                else if (columns[i].equals("Y")) {
                    writer.write("" + feature.getGeometry().getCoordinate().y);
                }
                else if (columns[i].equals("Z")) {
                    writer.write("" + feature.getGeometry().getCoordinate().z);
                }
                else {
                    String value = null;
                    if (feature.getSchema().getAttributeType(columns[i]) == AttributeType.GEOMETRY) {
                        value = wktw.write(feature.getGeometry());
                    } else {
                        value = feature.getString(columns[i]);
                        // if the field name contains a quote or a fieldSeparator
                        boolean quote = value.contains("\"") ||
                                value.indexOf(fieldSeparator.getSeparator()) > -1;
                        // -> field name is written between double quotes
                        if (quote) value = "\"" + value.replaceAll("\"","\"\"") + "\"";
                    }
                    writer.write(value);
                }
                if (i < columns.length-1) writer.write(fieldSeparator.getSeparator());
            }
            writer.write(LINE_SEPARATOR);
        }
    }
    
    
    public boolean hasExceptions() {
        return exceptions.size() > 0;
    }
    

    /** Get all exceptions thrown during the CSV File parsing.*/
    public List<Exception> getExceptions() {
        return exceptions;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("CSVFile Properties={\n");
        sb.append("    filePath        =" + filePath + "\n");
        sb.append("    encoding        =" + charset.displayName() + "\n");
        sb.append("    comment_pattern ='" + commentLinePattern + "'\n");
        sb.append("    field_sep       ='" + fieldSeparator + "'\n");
        sb.append("    hasHeaderLine   ='" + headerLine + "'\n");
        sb.append("    hasDataTypeLine ='" + dataTypeLine + "'\n");
        sb.append("    columns         ='" + Arrays.toString(columns) + "'\n");
        sb.append("    geom_columns    ='" + Arrays.toString(geometryColumns) + "'\n");
        if (schema != null) {
            for (int i = 0 ; i < schema.getAttributeCount() ; i++) {
                sb.append("        " + schema.getAttributeName(i) + "(" + schema.getAttributeType(i) + ")\n");
            }
        }
        sb.append("}");
        return sb.toString();
    }

}
