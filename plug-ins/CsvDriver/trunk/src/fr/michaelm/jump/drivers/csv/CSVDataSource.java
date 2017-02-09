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

import java.io.*;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;

import com.vividsolutions.jump.coordsys.CoordinateSystem;
import com.vividsolutions.jump.coordsys.CoordinateSystemRegistry;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.io.datasource.Connection;
import com.vividsolutions.jump.io.datasource.DataSource;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.Logger;

/**
 * A file containing geographic information in a character delimited format
 * (csv like).</p>
 * Replace the previous XYZDataSource
 * @author Micha&euml;l MICHAUD
 */
// 0.6   complete rewrite of the txt driver, becoming the csv driver
//       XYZDatSource driver (replaced by CSVDataSource)
// 0.5   (2012-03-04)
// 0.2   (2009-10-07) use split(String,-1) instead of split(String)
// 0.1   (2009-09-06)
public class CSVDataSource extends DataSource {

    public static final String CHARSET              = "Charset";
    public static final String COMMENT_LINE_PATTERN = "Comment Line Pattern";
    public static final String FIELD_SEPARATOR      = "Field Separator";
    public static final String HEADER_LINE          = "Header Line";
    public static final String DATA_TYPE_LINE       = "Data Type Line";

    public static final String X_COLUMN             = "X-Column";
    public static final String Y_COLUMN             = "Y-Column";
    public static final String Z_COLUMN             = "Z-Column";
    public static final String WKT_COLUMN           = "WKT-Column";

    public static final String SAVED_AS             = "Saved as";
    public static final String ATTRIBUTE_SELECTION  = "Attribute Selection";
    
    CSVFile csv;
    
    public void init() {
        try {
            csv = (CSVFile)getProperties().get("CSV_FILE");
            
            if (csv == null) {
                
                csv = new CSVFile((String)getProperties().get(FILE_KEY), (String)getProperties().get("CompressedFile"));
                
                Charset encoding = Charset.forName(getProperties().get(CHARSET).toString());
                if (encoding != null) csv.setEncoding(encoding.name());
                
                Pattern commentLinePattern = (Pattern)getProperties().get(COMMENT_LINE_PATTERN);
                if (commentLinePattern != null) csv.setCommentLinePattern(commentLinePattern);
                
                FieldSeparator separator = (FieldSeparator)getProperties().get(FIELD_SEPARATOR);
                if (separator != null) csv.setFieldSeparator(separator);
                System.out.println("sep=" + separator);
                
                Boolean headerLine = (Boolean)getProperties().get(HEADER_LINE);
                if (headerLine != null) csv.setHeaderLine(headerLine);
                
                Boolean dataTypeLine = (Boolean)getProperties().get(DATA_TYPE_LINE);
                if (dataTypeLine != null) csv.setDataTypeLine(dataTypeLine);
                
                int x_index = -1, y_index = -1, z_index = -1, wkt_index = -1;
                // X_COLUMN and Y_COLUMN properties are 1-based while indexes are 0-based
                try {
                    x_index = Integer.parseInt((String)getProperties().get(X_COLUMN)) - 1;
                    y_index = Integer.parseInt((String)getProperties().get(Y_COLUMN)) - 1;
                } catch(Exception e) {}
                try {
                    z_index = Integer.parseInt((String)getProperties().get(Z_COLUMN)) - 1;
                } catch(Exception e) {}
                try {
                    wkt_index = Integer.parseInt((String)getProperties().get(WKT_COLUMN)) - 1;
                } catch(Exception e) {}
                int[] geomColumns;
                if (wkt_index > -1) geomColumns = new int[]{wkt_index};
                else if (x_index > -1 && y_index > -1 && z_index > -1) geomColumns = new int[]{x_index, y_index, z_index};
                else if (x_index > -1 && y_index > -1) geomColumns = new int[]{x_index, y_index};
                else geomColumns = new int[0];
                csv.setGeometryColumns(geomColumns);
            }
        }
        catch(Exception e) {
            e.printStackTrace();
            Logger.error("CSVDataSource#init()", e);
        }
    }


    /**
     * Creates a new Connection to this DataSource.
     */
    public Connection getConnection() {

        init();

        try {

            return new Connection() {

                public FeatureCollection executeQuery(String query, Collection exceptions, TaskMonitor monitor) {

                    try {
                        csv.setFeatureSchema();
                        FeatureCollection fc = new FeatureDataset(csv.getFeatureSchema());
                        Iterator<Feature> iterator = csv.iterator();
                        while (iterator.hasNext()) {
                            try {
                                Feature f = iterator.next();
                                if (f!=null) {
                                    fc.add(f);
                                } else {
                                    System.out.println("Could't read feature");
                                }
                            }
                            catch(Exception e) {
                                e.printStackTrace();
                            }
                        }
                        exceptions.addAll(csv.getExceptions());
                        return fc;

                    } catch (Exception e) {
                        exceptions.add(e);
                        return null;
                    }
                }
                
                public void executeUpdate(String update, FeatureCollection featureCollection, TaskMonitor monitor)
                        throws Exception {

                    csv.setHeaderLine(true);
                    boolean selectAttributes = false;
                    if (getProperties().get(ATTRIBUTE_SELECTION) != null) {
                        selectAttributes = (Boolean)getProperties().get(ATTRIBUTE_SELECTION);
                    }
                    Boolean dataTypeLine = (Boolean)getProperties().get(DATA_TYPE_LINE);
                    if (dataTypeLine != null) csv.setDataTypeLine(dataTypeLine);
                    
                    File output = new File((String)getProperties().get(FILE_KEY));
                    Writer writer = new OutputStreamWriter(new FileOutputStream(output), csv.getCharset());
                    FeatureSchema fs = featureCollection.getFeatureSchema();
                    String[] columns;
                    AttributeType[] types;
                    
                    if (selectAttributes) {
                        AttributeChooser chooser = new AttributeChooser(fs);
                        List<String> attributes = chooser.getSelectedAttributes();
                        columns = attributes.toArray(new String[attributes.size()]);
                        types = new AttributeType[columns.length];
                        int count = 0;
                        for (int i = 0 ; i < columns.length; i++) {
                            if (columns[i].equals("X")) types[i] = AttributeType.DOUBLE;
                            else if (columns[i].equals("Y")) types[i] = AttributeType.DOUBLE;
                            else if (columns[i].equals("Z")) types[i] = AttributeType.DOUBLE;
                            else break;
                            count++;
                        }
                        for (int i = count ; i < columns.length; i++) {
                            types[i] = fs.getAttributeType(fs.getAttributeIndex(columns[i]));
                        }
                    }
                    else {
                        columns = new String[fs.getAttributeCount()];
                        types = new AttributeType[fs.getAttributeCount()];
                        for (int i = 0 ; i < fs.getAttributeCount() ; i++) {
                            columns[i] = fs.getAttributeName(i);
                            types[i] = fs.getAttributeType(i);
                        }
                    }
                    csv.setColumns(columns);
                    csv.setAttributeTypes(types);
                    csv.writeSchema(writer);
                    csv.writeDataTypes(writer);
                    for (Iterator it = featureCollection.iterator() ; it.hasNext() ; ) {
                        Feature feature = (Feature)it.next();
                        csv.writeFeature(writer, feature);
                    }
                    writer.close();

                    // [mmichaud 2013-11-06] resolve a problem of persistence in the project file
                    if (getProperties().get(SAVED_AS) != null && ((Boolean)getProperties().get(SAVED_AS)).booleanValue()) {
                        AutoCSVFile csvFile = new AutoCSVFile(csv.getFilePath(), csv.getEntryName());
                        csv.setEncoding(csv.getCharset().name());
                        csv.setFieldSeparator(csv.getFieldSeparator());
                        csvFile.setDataTypeLine(csv.hasDataTypeLine());
                        getProperties().put("CSV_FILE", csvFile);
                    }
                }
                
                public void close() {}
                
                public FeatureCollection executeQuery(String query, TaskMonitor monitor) throws Exception {
                    ArrayList exceptions = new ArrayList();
                    FeatureCollection featureCollection = executeQuery(query, exceptions, monitor);
                    if (!exceptions.isEmpty()) {
                        throw (Exception) exceptions.iterator().next();
                    }
                    return featureCollection;
                }
            };
        }
        catch(Exception e) {
            Logger.error("CSVDataSource#getConnection()", e);
            return null;
        }
    }
    
    public boolean isWritable() {
        return true;
    }
    
    public FeatureCollection installCoordinateSystem(FeatureCollection queryResult, 
                                                     CoordinateSystemRegistry registry) {
        if (queryResult == null) { return null; }
        String coordinateSystemName;
        try {
            coordinateSystemName = (String) getProperties().get(COORDINATE_SYSTEM_KEY);
        } catch (NullPointerException e){
            return queryResult;
        }
        if (coordinateSystemName == null) { return queryResult; }
        CoordinateSystem coordinateSystem = registry.get(coordinateSystemName);
        if (coordinateSystem == null) { return queryResult; }
        queryResult.getFeatureSchema().setCoordinateSystem(coordinateSystem);
        return queryResult;
    }

}
