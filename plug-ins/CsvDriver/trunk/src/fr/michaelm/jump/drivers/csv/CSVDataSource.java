/*
 * Library offering read and write capabilities for dsv formats
 * Copyright (C) 2012 Michaël MICHAUD
 * michael.michaud@free.fr
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

import java.io.BufferedWriter;
import java.io.Writer;
import java.io.File;
import java.io.FileWriter;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Pattern;
//
import com.vividsolutions.jts.geom.Coordinate;
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

/**
 * A file containing geographic information in a character delimited format
 * (csv like).</p>
 * Replace the previous XYZDataSource
 * @author Micha&euml;l MICHAUD
 * @version 0.6 (2012-03-25)
 */
 // 0.6 complete rewrite of the txt driver, becoming the csv driver
 // XYZDatSource driver (replaced by CSVDataSource) 
 // 0.5 (2012-03-04)
 // 0.2 (2009-10-07) use split(String,-1) instead of split(String)
 // 0.1 (2009-09-06) 
public class CSVDataSource extends DataSource {

    private static final Logger LOG = Logger.getLogger("fr.michaelm.jump.drivers.csv.CSVDataSource");
    
    CSVFile csv;
    
    public void init() {
        try {
            csv = (CSVFile)getProperties().get("CSV_FILE");
            if (csv == null) {
                
                csv = new CSVFile((String)getProperties().get(FILE_KEY));
                
                Charset encoding = (Charset)getProperties().get(I18NPlug.getI18N("drivers.csv.encoding"));
                if (encoding != null) csv.setEncoding(encoding.name());
                
                Pattern commentLinePattern = (Pattern)getProperties().get(I18NPlug.getI18N("drivers.csv.comment-line-pattern"));
                if (commentLinePattern != null) csv.setCommentLinePattern(commentLinePattern);
                
                FieldSeparator separator = (FieldSeparator)getProperties().get(I18NPlug.getI18N("drivers.csv.field-separator"));
                if (separator != null) csv.setFieldSeparator(separator);
                
                Boolean headerLine = (Boolean)getProperties().get(I18NPlug.getI18N("drivers.csv.header-line"));
                if (headerLine != null) csv.setHeaderLine(headerLine);
                
                Boolean dataTypeLine = (Boolean)getProperties().get(I18NPlug.getI18N("drivers.csv.data-type-line"));
                if (dataTypeLine != null) csv.setDataTypeLine(dataTypeLine);
                
                int x_index = -1, y_index = -1, z_index = -1, wkt_index = -1;
                try {
                    x_index = Integer.parseInt((String)getProperties().get(I18NPlug.getI18N("drivers.csv.x"))) - 1;
                    y_index = Integer.parseInt((String)getProperties().get(I18NPlug.getI18N("drivers.csv.y"))) - 1;
                } catch(Exception e) {}
                try {
                    z_index = Integer.parseInt((String)getProperties().get(I18NPlug.getI18N("drivers.csv.z"))) - 1;
                } catch(Exception e) {}
                try {
                    wkt_index = Integer.parseInt((String)getProperties().get(I18NPlug.getI18N("drivers.csv.wkt"))) - 1;
                } catch(Exception e) {}
                int[] geomColumns = null;
                if (wkt_index > -1) geomColumns = new int[]{wkt_index};
                else if (x_index > -1 && y_index > -1 && z_index > -1) geomColumns = new int[]{x_index, y_index, z_index};
                else if (x_index > -1 && y_index > -1) geomColumns = new int[]{x_index, y_index};
                else geomColumns = new int[0];
                csv.setGeometryColumns(geomColumns);
            }
        }
        catch(Exception e) {
            e.printStackTrace();
            LOG.throwing("CSVDataSource","init",e);
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
                        System.out.println(csv);
                        Iterator<Feature> iterator = csv.iterator();
                        while (iterator.hasNext()) {
                            try {
                                Feature f = iterator.next();
                                if (f!=null) {
                                    fc.add(f);
                                } else System.out.println("could't read feature");
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
                    
                    CSVFile csv = new CSVFile();
                    csv.setFieldSeparator((FieldSeparator)getProperties().get(I18NPlug.getI18N("drivers.csv.field-separator")));
                    csv.setHeaderLine(true);
                    csv.setDataTypeLine((Boolean)getProperties().get(I18NPlug.getI18N("drivers.csv.data-type-line")));
                    
                    boolean selectAttributes= (Boolean)getProperties().get(I18NPlug.getI18N("drivers.csv.select-attributes"));
                    int[] geometryColumns;
                    
                    File output = new File((String)getProperties().get(FILE_KEY));
                    Writer writer = new BufferedWriter(new FileWriter(output));
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
                            count++;
                            if (columns[i].equals("X")) types[i] = AttributeType.DOUBLE;
                            else if (columns[i].equals("Y")) types[i] = AttributeType.DOUBLE;
                            else if (columns[i].equals("Z")) types[i] = AttributeType.DOUBLE;
                            else break;
                        }
                        for (int i = count-1 ; i < columns.length; i++) {
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
            LOG.throwing("TextDataSource", "getConnection", e);
            return null;
        }
    }
    
    public boolean isWritable() {
        return true;
    }
    
    public FeatureCollection installCoordinateSystem(FeatureCollection queryResult, 
                                                     CoordinateSystemRegistry registry) {
        if (queryResult == null) { return queryResult; }
        String coordinateSystemName = null;
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
