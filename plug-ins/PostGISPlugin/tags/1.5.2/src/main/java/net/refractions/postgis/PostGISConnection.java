/*
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 */
// Changed:
// 2004.07.12, brent owens
// 2005.07.15, U.D. (Uwe Dalluege, uwe.dalluege@hcu-hamburg.de)
// 2006.06.21, U.D.
// 2006.11.06, U.D.
// 2009.09.25, U.D.
// 2010.03.29, U.D. (add table schema management)
// 2010.11.05, U.D., fixed NaN
// 2011.07.16, M.M. (merged EL and UD changes)
// 2012.04.20, M.M. add date type management
package net.refractions.postgis;

import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.io.WKTReader;
import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.feature.*;
import com.vividsolutions.jump.io.datasource.Connection;
import com.vividsolutions.jump.task.TaskMonitor;
import java.io.Reader;
import java.sql.*;
import java.text.SimpleDateFormat;
import java.util.*;
import javax.swing.*;
import java.math.*;

/**
 * This class represents the actual connection of a PostGIS data source. 
 */
public class PostGISConnection implements Connection {
    
    public static final String KEY = PostGISConnection.class.getName();
  
    private static final String PKG_KEY = "net.refractions.postgis";

    Map properties;
    String server;
    String port;
    String database;
    String table;
    String where;
    String username;
    String password;
    String uniqueCol;
    String method;
    int srid_tmp = 0;
    boolean errGeomEmpty = false;
    
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss.SSS");

    /**
     * Creates an unconfigured connection.
     */
    public PostGISConnection() {
        this(null);
    }
  
    /**
     * Creates a configured connection. See {@link PostGISDataSource} for 
     * property keys.
     */
    public PostGISConnection(Map properties) {
        this.properties = properties;        
    }
  
    /**
     * Configures the connection. See {@link PostGISDataSource} for property
     * keys.
     */
    public void setProperties(Map properties) {
        this.properties = properties;
    }
  
    /**
     * Executes a query against a connection. The result of the query is returned as 
     * a FeatureCollection.
     * @param query SQL statement.
     */
    public FeatureCollection executeQuery(String query) {
        
        FeatureDataset dataSet;
        String geoColName = null;
        readProperties();
        java.sql.Connection conn = connect();
        
        try {
            Statement st = conn.createStatement();
            
            // [mmichaud 2011-07-16] Most of the conversion code could be
            // replaced by OpenJUMP's new framework :
            // see com.vividsolutions.jump.datastore.postgis.PostgisResultSetConverter
            
            ResultSet rs = st.executeQuery(query + " LIMIT 0");
            ResultSetMetaData meta = rs.getMetaData();
            
            boolean geomCol = false;
            FeatureSchema schema = new FeatureSchema();
            StringBuffer sql = new StringBuffer( "SELECT" );
            int num_cols = meta.getColumnCount();
            for( int col_idx = 1; col_idx <= num_cols; col_idx++ ) {
                String attr_name = meta.getColumnName ( col_idx );
                // U.D., uwe.dalluege@hcu-hamburg.de
                // This do not work? What for?
                //        String table_name = meta.getTableName ( col_idx );
                
                AttributeType attr_type = AttributeType.STRING;
                
                // determine the attribute type based on the sql type
                if( meta.getColumnTypeName(col_idx).equalsIgnoreCase("geometry") && !geomCol ) {
                    // found the first geometry column (any extra geometry columns are treated as strings)
                    geomCol = true;
                    attr_type = AttributeType.GEOMETRY;
                    geoColName = attr_name;
                    sql.append( " ST_AsText(" + attr_name + ") AS " + attr_name + "," );
                } 
                else {
                    int sql_type = meta.getColumnType(col_idx);
                    switch(sql_type) {
                        case Types.BIGINT:
                        case Types.INTEGER:
                        case Types.SMALLINT:
                        case Types.TINYINT:
                            attr_type = AttributeType.INTEGER;
                            sql.append( " " + attr_name + "," );
                            break;
                        case Types.DOUBLE:
                        case Types.FLOAT:
                        case Types.REAL:
                        case Types.NUMERIC:
                            attr_type = AttributeType.DOUBLE;
                            sql.append( " " + attr_name + "," );
                            break;
                        case Types.DATE:
                        case Types.TIME:
                        case Types.TIMESTAMP:
                            attr_type = AttributeType.DATE;
                            sql.append( " " + attr_name + "," );
                            break;
                        default:
                            attr_type = AttributeType.STRING;
                            sql.append( " " + attr_name + "," );
                    }
                }
                schema.addAttribute( attr_name, attr_type );
            }
            rs.close();
        
            // U.D., uwe.dalluege@hcu-hamburg.de : read SRID
            // M.M., use new function ST_SRID
            query = "SELECT ST_SRID ( " + geoColName + " ) FROM " + table;
            rs = st.executeQuery ( query + " LIMIT 1" );
            while ( rs.next() ) {
                srid_tmp = rs.getInt ( 1 );
            }
            rs.close(); 
            
            if (!geomCol) {
                st.close();
                conn.close();
                throw new IllegalStateException(I18N.getText(PKG_KEY,KEY+".geometry-missing"));  
            }
            
            sql.deleteCharAt( sql.lastIndexOf( "," ) );
            sql.append( " FROM " + table);
            if (where != null && where != "") {
                sql.append(" WHERE "+ where);
            }
            
            st.execute( "BEGIN" );
            String s = "DECLARE my_cursor CURSOR FOR " + sql.toString();
            
            st.execute( "DECLARE my_cursor CURSOR FOR " + sql.toString() );
            rs = st.executeQuery( "FETCH FORWARD ALL IN my_cursor" );
            dataSet = new FeatureDataset( schema ) ;
            
            // U.D., uwe.dalluege@hcu-hamburg.de : change 0 to srid_tmp
            GeometryFactory factory = 
            new GeometryFactory( new PrecisionModel(), srid_tmp );
          
            WKTReader wktReader = new WKTReader( factory );
            // Main loop reading table records
            while( rs.next() ) {
                Feature f = new BasicFeature( schema );
                for( int attr_idx = 0; attr_idx < schema.getAttributeCount(); attr_idx++ ) {
                    AttributeType attr_type = schema.getAttributeType(attr_idx);
                    if( attr_type.equals(AttributeType.GEOMETRY) ) {
                        Reader wkt = rs.getCharacterStream(schema.getAttributeName(attr_idx)); 
                        Geometry geom;
                        if(wkt == null) {
                            geom = new GeometryCollection(null, factory);
                        } 
                        else {
                            geom = wktReader.read( wkt );
                        } 
                        f.setAttribute( attr_idx, geom );
                    } 
                    else if(attr_type.equals(AttributeType.INTEGER)) {
                        // u.d. to get the null values
                        Object ob = rs.getObject(schema.getAttributeName(attr_idx));
                        if (ob == null) {
                            f.setAttribute(attr_idx, null);
                        }
                        // [mmichaud 2011-07-16] better handling of data types
                        else if (ob instanceof Byte) {
                            f.setAttribute(attr_idx, new Integer(((Byte)ob).intValue()));
                        }
                        else if (ob instanceof Short) {
                            f.setAttribute(attr_idx, new Integer(((Short)ob).intValue()));
                        }
                        else if (ob instanceof Integer) {
                            f.setAttribute(attr_idx, ob);
                        }
                        else if (ob instanceof Long) {
                            f.setAttribute(attr_idx, new Integer(((Long)ob).intValue()));
                        }
                    }
                    else if(attr_type.equals(AttributeType.DOUBLE)) {
                        // u.d. to get the null values
                        Object ob = rs.getObject( schema.getAttributeName( attr_idx ) );
                        if (ob == null) {
                            f.setAttribute(attr_idx, null); 
                        }
                        // [mmichaud 2011-07-16] better handling of data types
                        else if (ob instanceof Float) {
                            f.setAttribute(attr_idx, new Double(((Float)ob).doubleValue()));
                        }
                        else if (ob instanceof Double) {
                            f.setAttribute(attr_idx, ob);
                        }
                        else if (ob instanceof BigDecimal) {
                            f.setAttribute(attr_idx, new Double(((BigDecimal)ob).doubleValue()));
                        } 
                    }
                    else if(attr_type.equals(AttributeType.DATE)) {
                        Object ob = rs.getObject( schema.getAttributeName( attr_idx ) );
                        if (ob == null) {
                            f.setAttribute(attr_idx, null); 
                        }
                        else {
                            f.setAttribute(attr_idx, rs.getDate(schema.getAttributeName(attr_idx)));
                        }
                    } 
                    else if(attr_type.equals(AttributeType.STRING)) {
                        f.setAttribute(attr_idx, rs.getString(schema.getAttributeName(attr_idx)));
                    }
                    // [mmichaud 2011-07-16] better handling of data types
                    // should add Date type handling before this default one
                    else {
                        f.setAttribute(attr_idx, rs.getString(schema.getAttributeName(attr_idx)));
                    }
                }
                // U.D., uwe.dalluege@hcu-hamburg.de  
                f.getGeometry().setSRID (srid_tmp);
                dataSet.add(f);
            }
          
            st.execute( "CLOSE my_cursor" );
            st.execute( "END" );
            st.close();
            conn.close();
        }
        catch(Exception e) {
            if (PostGISPlugIn.DEBUG) e.printStackTrace();
            throw new IllegalStateException(e.getMessage());
        }
        
        return(dataSet);
    }
  
    /**
     * Since any exceptions will cause the query to fail, this function 
     * simply calls executeQuery(String query) and does not ignore any 
     * exceptions.
     */
    public FeatureCollection executeQuery(String query, List exceptions) {
        return(executeQuery(query));
    }
  
    /**
     * Executes an update against the connection.
     * @param query This parameter is ignored, the connection determines everything 
     * it needs to know from setProperties(HashMap properties) and collection.
     * @param collection The updated features.
     */
    public void executeUpdate(String query, FeatureCollection collection) {
  
        // U.D. for exist unique column
        String[] param; 
        boolean unColExists = false;
        
        if (collection.isEmpty()) 
          throw new IllegalStateException(I18N.getText(PKG_KEY, KEY + ".empty-featurecollection"));
        
        int SRID = ((Feature)collection.iterator().next()).getGeometry().getSRID();
        
        // get the feature schema
        FeatureSchema schema = collection.getFeatureSchema();
        HashSet schemaCols = new HashSet(schema.getAttributeCount());
        for( int i = 0; i < schema.getAttributeCount(); i++ ) {
            String name = schema.getAttributeName( i );
            if(schemaCols.contains(name)) {
                throw new UnsupportedOperationException
                    (I18N.getText(PKG_KEY, KEY + ".duplicate-attribute-names"));
            }
            // toLowerCase, ud, 2006.07.29
            schemaCols.add(name.toLowerCase());
        }
        
        java.sql.Connection conn = null;
        Statement stmt = null;
        
        HashSet cols = null;
        StringBuffer sqlBuf = null;
        String sql = null;
        
        readProperties();  
        conn = connect();
        
        // determine if the table already exists or not
        boolean table_exists = false;
        
        try {
            stmt = conn.createStatement();
            ResultSet rs = stmt.executeQuery("SELECT * FROM " + table + " LIMIT 0");
            ResultSetMetaData meta = rs.getMetaData();
            int num_cols = meta.getColumnCount();
            cols = new HashSet( num_cols );
            for( int col_idx = 1; col_idx <= num_cols; col_idx++ ) {
                // toLowerCase, ud, 2006.07.29
                cols.add( meta.getColumnName(col_idx).toLowerCase( ));
            }
            rs.close();
            table_exists = true;
        } 
        catch(SQLException sqle) {
            table_exists = false;
            //if( method == PostGISDataSource.SAVE_METHOD_UPDATE ) {
            //  throw new IllegalStateException( "Save method is set to UPDATE and table does not exist; cannot update a non-existant table" );
            //}       
        }
        
        // begin the transaction
        try {
            // autocommit is set to false to be able to rollback the full transaction
            conn.setAutoCommit(false); // don't know why transactions don't work...
            String geomCol = null;
                ////////////////////////////////////////////////////////////////
                // SAVE_METHOD_DELETE : DROP THE TABLE
                ////////////////////////////////////////////////////////////////
                if (method == PostGISDataSource.SAVE_METHOD_DELETE) {
                    if (table_exists) {
                        param = new String[1];
                        param[0] = table;
                        int opt = JOptionPane.showConfirmDialog(
                            PostGISCommonDriverPanel.driverPanel, 
                            com.vividsolutions.jump.I18N.getMessage(PKG_KEY, KEY+".overwrite.existing", param), 
                            com.vividsolutions.jump.I18N.getMessage(PKG_KEY, KEY+".overwrite", param), 
                            JOptionPane.YES_NO_OPTION);
                        
                        if ( opt == JOptionPane.NO_OPTION ) return;
                        
                        // U.D., 30.03.2010 : use table schema if any
                        String[] schemaAndTable = table.split ( "\\." );
                        String sqlDrop = "Error!";
                        
                        if (schemaAndTable.length == 2) {
                            sqlDrop = "SELECT DropGeometryTable( '" +
                                      schemaAndTable [0] + "', '" +  schemaAndTable [1] + "' )";
                        }
                        else {
                            sqlDrop = "SELECT DropGeometryTable ( '" + table + "' )";
                        }
                        try {
                            stmt.execute( sqlDrop );
                            // If the transaction is rolled back, it's better not
                            // to have dropped the table !!!
                            //stmt.execute ( "COMMIT" );
                            table_exists = false;
                        } 
                        catch( SQLException sqle ) {
                            sqle.printStackTrace();
                            return;
                        }
                    }
                }
                ////////////////////////////////////////////////////////////////
                // SAVE_METHOD_OVERWRITE
                ////////////////////////////////////////////////////////////////
                if (method == PostGISDataSource.SAVE_METHOD_OVERWRITE) {
                    if (table_exists) {
                        //System.out.println 
                        //( "PostGIS Connection table: " + table + " löschen!"); 
                        param = new String[1];
                        param[0] = table;
                        int opt = JOptionPane.showConfirmDialog(
                            PostGISCommonDriverPanel.driverPanel, 
                            com.vividsolutions.jump.I18N.getMessage(PKG_KEY, KEY+".overwrite.existing", param), 
                            com.vividsolutions.jump.I18N.getMessage(PKG_KEY, KEY+".overwrite", param), 
                            JOptionPane.YES_NO_OPTION );
                        
                        if (opt == JOptionPane.NO_OPTION) return;
                        
                        String sqlDrop = "DELETE FROM " + table ;
                        if (where != null && where != "") {
                            sqlDrop = sqlDrop + " WHERE "+ where;
                        }
                        try {
                            stmt.execute(sqlDrop);
                            table_exists = false; // need for other code; normaly true
                        } 
                        catch(SQLException sqle) {
                            sqle.printStackTrace();
                        }
                    }
                    else {
                        // UD, If not exist, it is like SAVE_METHOD_DELETE                    
                        method = PostGISDataSource.SAVE_METHOD_DELETE;
                    }
                }
                
                ////////////////////////////////////////////////////////////////
                // CREATE A NEW TABLE
                ////////////////////////////////////////////////////////////////
                if( !table_exists ) {
                // build the create table statement
                sqlBuf = new StringBuffer( "CREATE TABLE " + table + " (" );
                int num_attrs = schema.getAttributeCount();
                //U.D., 2006.07.28, uwe.dalluege@hcu-hamburg.de 
                if (num_attrs <= 1 &&
                    method != PostGISDataSource.SAVE_METHOD_DELETE &&
                    method != PostGISDataSource.SAVE_METHOD_OVERWRITE) {
                    throw new Exception (I18N.getText(PKG_KEY, KEY + ".error.missing-feature-column"));
                }
                cols = new HashSet(num_attrs);
            
                for(int i = 0; i < num_attrs; i++) {
                    String name = schema.getAttributeName( i );
                    if (name.trim().equalsIgnoreCase(uniqueCol.trim())){
                        unColExists = true;
                    }
                    cols.add(name.trim().toLowerCase());
                    AttributeType type = schema.getAttributeType( i );
                    if(type.equals(AttributeType.INTEGER)) {
                        sqlBuf.append(" " + name + " INT,");
                    } 
                    else if( type.equals( AttributeType.DOUBLE ) ) {
                        sqlBuf.append( " " + name + " FLOAT8," );
                    } 
                    else if( type.equals( AttributeType.GEOMETRY ) ) {
                        // do not add to the query string
                        if(geomCol == null) {
                            // save the name of the geometry column for later
                            geomCol = name;
                        } 
                    } 
                    else { // U.D. from varchar (255) to text-obj
                        sqlBuf.append( " " + name + " TEXT," );
                    }
                }
                int lastIx = sqlBuf.lastIndexOf( "," );
                if (lastIx >= 0)
                    // sqlBuf.deleteCharAt( sqlBuf.lastIndexOf( "," ) );
                    sqlBuf.deleteCharAt( lastIx );
                    // U.D. 21.06.2006 PostgreSQL 8.1 has no default OIDS
                    sqlBuf.append( " ) WITH OIDS" );
                    sql = sqlBuf.toString();  
                    // create the table
                    if (!unColExists && 
                        method != PostGISDataSource.SAVE_METHOD_DELETE &&
                        method != PostGISDataSource.SAVE_METHOD_OVERWRITE) {
                        JOptionPane.showMessageDialog(
                            PostGISCommonDriverPanel.driverPanel, 
                            I18N.getMessage(PKG_KEY, KEY + ".error.unique-column-does-not-exist", new Object[]{uniqueCol}),
                            I18N.getText(PKG_KEY, KEY + ".error.table-will-not-be-saved"), JOptionPane.ERROR_MESSAGE);
                        return;
                    }
                    try {
                        // UD, crazy code! 2006.07.28
                        if (method != PostGISDataSource.SAVE_METHOD_OVERWRITE){
                            stmt.executeUpdate(sql);
                            //stmt.execute ( "COMMIT" ); // Without it doesn't work. u.d., 29.03.2010
                        }
                    } 
                    catch( SQLException sqle ) {
                        if (PostGISPlugIn.DEBUG) { 
                            sqle.printStackTrace();
                        }
                        throw new Exception(I18N.getText(PKG_KEY, KEY + ".error.create-table-failed") + 
                            "\n" + sqle.toString() + "\n" + sql );
                    }
          
                    // add the geometry column
                    String[] schemaAndTable = table.split ( "\\." );
                    if ( schemaAndTable.length == 2 ) { // Schema auf 0, table auf 1
                        sql = "SELECT AddGeometryColumn( '" + schemaAndTable [ 0 ] + "', '" + 
                        schemaAndTable [ 1 ] + 
                        "', '" + geomCol.toLowerCase ( ) + "', " + SRID + ", 'GEOMETRY', 2 )";
                    }
                    else { // nur table
                        sql = "SELECT AddGeometryColumn( '" + table + 
                        "', '" + geomCol.toLowerCase ( ) + "', " + SRID + ", 'GEOMETRY', 2 )";      
                    }
                    try {
                        //System.out.println ( " PGC 443 sql: " + sql ); 
                        // UD, a little crazy! 2006.07.28
                        if (method != PostGISDataSource.SAVE_METHOD_OVERWRITE)
                        stmt.execute(sql);
                        //stmt.execute ( "COMMIT" );
                    }
                    catch( Exception e ) {
                        if (PostGISPlugIn.DEBUG) { 
                            e.printStackTrace();
                        } 
                        throw new Exception(
                            I18N.getText(PKG_KEY, KEY + ".error.addgeometrycolumn-failed") + 
                            "\n" + e.toString() + "\n" + sql
                        );
                    }
                }
        
                /** table creation complete, now move onto the update */
          
                String val = null;
                String uniqueVal = null;
          
                Iterator it = collection.iterator();
                while (it.hasNext()) {
                    uniqueVal = null;
                    Feature f = (Feature) it.next();
              
                // u.d. exist column-names  
                boolean errCol = false;
                boolean errUni = true;
                String errColName = null;
                for (int ii = 0; ii < schema.getAttributeCount(); ii++) {
                    String attrName = schema.getAttributeName(ii);
                    Object[] ocols = cols.toArray();
                    for (int t = 0; t < ocols.length; t++) {
                        //System.out.println ( "PGC 584: ocols " + ocols [ t ] );                           
                    }
                    if (!cols.contains(attrName.toLowerCase ())) {
                        errCol = true;
                        errColName = attrName;
                    }
                    if ( attrName.equalsIgnoreCase(uniqueCol) ) errUni = false;
                }
                // U.D. DB schema and feature schema not the same          
                if (errCol && 
                    method != PostGISDataSource.SAVE_METHOD_DELETE &&
                    method != PostGISDataSource.SAVE_METHOD_OVERWRITE) {
                    JOptionPane.showMessageDialog( 
                        PostGISCommonDriverPanel.driverPanel,
                        I18N.getMessage(PKG_KEY, KEY + ".error.schemas-are-not-equal", new Object[]{errColName}) +
                        "\n" + I18N.getText(PKG_KEY, KEY + ".error.use-lowercases"),
                        I18N.getText(PKG_KEY, KEY + ".error.table-will-not-be-saved"), JOptionPane.ERROR_MESSAGE );
                    stmt.close();
                    conn.close();    
                    return;
                }
                // u.d. unique column not present
                if (errUni && 
                    method != PostGISDataSource.SAVE_METHOD_DELETE &&
                    method != PostGISDataSource.SAVE_METHOD_OVERWRITE) {
                    JOptionPane.showMessageDialog(
                        PostGISCommonDriverPanel.driverPanel, 
                        I18N.getMessage(PKG_KEY, KEY + ".error.unique-column-does-not-exist", new Object[]{uniqueCol}),
                        I18N.getText(PKG_KEY, KEY + ".error.table-will-not-be-saved"), JOptionPane.ERROR_MESSAGE );
                    stmt.close();
                    conn.close();        
                    return;
                }
              
        
                ////////////////////////////////////////////////////////////////
                //If the value of uniqueCol exists we make that the uniqueVal in
                //an UPDATE or INSERT, if does not exists uniqueCol must be
                //AutoIncrement and the value is set by the DBMS in an INSERT.
                //Otherwise is an ERROR.
                //System.out.println ( "PGC 614 count: " + schema.getAttributeCount()); 
                for (int i = 0; i < schema.getAttributeCount(); i++) {
                    String attrName = schema.getAttributeName(i);
                    if (cols.contains(attrName.toLowerCase())) {
                    if (method == PostGISDataSource.SAVE_METHOD_DELETE ||
                        method == PostGISDataSource.SAVE_METHOD_OVERWRITE) {
                        insertFeatureNew (f, stmt, cols, schema, SRID );
                        break;
                    }
                    else if (attrName.equalsIgnoreCase(uniqueCol)) {
                        //Is the value in feature's uniqueCol null?
                        if (f.getAttribute(attrName) == null) {
                            ResultSet rs = stmt.executeQuery(
                                "SELECT * FROM " + table + " LIMIT 0");
                            ResultSetMetaData meta = rs.getMetaData();
                            //if (meta.isAutoIncrement(rs.findColumn(name))) {
                            //The uniqueCol column has no value in the
                            //feature, but is an autoincremental column
                            //so the DBMS assigns this value.
                            // UD, put SRID in the variable-list
                            // insertFeature(f, false, stmt, cols, schema );
                            insertFeature(f, false, stmt, cols, schema, SRID);
                            break;
                        }
                        else  //There a uniqueVal in feature's uniqueCol?
                          //if (f.getString(i).length() > 0) 
                             {
                            AttributeType attrType = schema.getAttributeType(i);
                            if (attrType.equals(AttributeType.INTEGER)) {
                                val = "" + f.getInteger(i);
                            } else if (attrType.equals(AttributeType.DOUBLE)) {
                                val = "" + f.getDouble(i);
                            } else if (
                                attrType.equals(AttributeType.GEOMETRY)) {
                                val = "ST_GeometryFromText( '" +
                                      f.getGeometry().toText() + "', " + SRID + ")";
                            } else {
                                val = "'" + f.getString(i).replaceAll ( "'", "''" ) + "'";
                            }
                            // U.D. if delete methode no need to check val
                            ResultSet rs;
                            if (method == PostGISDataSource.SAVE_METHOD_DELETE ||
                                method == PostGISDataSource.SAVE_METHOD_OVERWRITE) {
                                // u.d. no resultset if delete methode
                                rs = null;
                            }
                            else {
                                rs = stmt.executeQuery("SELECT * FROM " + table
                                    + " WHERE " + attrName + "=" + val);
                                }
                                // Is there a row with that uniqueVal in the DBMS?
                                // U.D. only when not delete methode!
                                if (rs != null) {
                                    if (rs.next()) {
                                        //That feature exists in DBMS.
                                        updateFeature(f, stmt, cols, schema, SRID );
                                        break;
                                    } 
                                    else {
                                        //That feature does not exist in DBMS.
                                        insertFeature(f, true, stmt, cols, schema, SRID );                        
                                        break;
                                    }
                                } // if != null
                                else {
                                    // u.d. if delete method only insert
                                    insertFeature(f, true, stmt, cols, schema, SRID);                        
                                    break;
                                }
                            }
                        } 
                    }
                }
            }
            // end the transaction
            conn.commit(); // don't know why transactions don't work...
            stmt.close();
            conn.close();        
        }
        catch( Exception e ) {
            try {
                conn.rollback();
                stmt.close();
                conn.close();  
            }
            catch(SQLException sqle) {
                if (PostGISPlugIn.DEBUG) sqle.printStackTrace();     
            }
            if (PostGISPlugIn.DEBUG) e.printStackTrace();
            throw new IllegalStateException(e.getMessage());
        }
        // ud. emty geometry not saved  
        if (errGeomEmpty) {
            JOptionPane.showMessageDialog(PostGISCommonDriverPanel.driverPanel, 
                I18N.getText(PKG_KEY, KEY + ".error.layer-has-empty-geometries"),
                I18N.getText(PKG_KEY, KEY + ".error.empty-geometries-are-not-saved"),
                JOptionPane.ERROR_MESSAGE );    
            errGeomEmpty = false;
        }
    
    }
  
    /**
     *Update a feature existent in DBMS identified by uniqueCol/uniqueVal.
     */
    private void updateFeature(Feature f, Statement stmt, HashSet cols,
                             FeatureSchema schema, int SRID ) throws Exception {
        StringBuffer sqlBuf = new StringBuffer("UPDATE " + table + " SET ");
        String val = null;
        String uniqueVal = null;

        for (int i = 0; i < schema.getAttributeCount(); i++) {
            String attrName = schema.getAttributeName(i);
            if (cols.contains(attrName.toLowerCase())) {
                AttributeType type = schema.getAttributeType(i);
                if (type.equals(AttributeType.INTEGER)) {
                    // u.d. find null
                    if (f.getAttribute ( i ) == null) {
                        val = null;
                    }
                    else {
                        Object ob = f.getAttribute ( i );
                        String iType = ob.getClass().getName();
                        if (iType.indexOf ( "Integer" ) >= 0) {
                            val = "" + f.getInteger(i);
                        }
                        else { // Long
                            Long iObj = ( Long ) ob;
                            val = "" +  iObj.longValue( );
                        }
                    }
                }
                else if (type.equals(AttributeType.DOUBLE)) {
                    if ( f.getAttribute ( i ) == null ) { val = null; }                 
                    else {
                        Object ob = f.getAttribute ( i );
                        String dType = ob.getClass ( ).getName( );
                        if (dType.indexOf ("Double") >= 0) {                         
                            val = "" + f.getDouble(i); 
                        }
                        else { // Big Decimal
                            BigDecimal dObj = ( BigDecimal ) ob;
                            val = "" + dObj.doubleValue ( ); 
                        }                                                           
                    }
                }
                else if (type.equals(AttributeType.DATE)) {
                    if ( f.getAttribute ( i ) == null ) { val = null; }                 
                    else {
                        Object ob = f.getAttribute ( i );
                        val = "'" + sdf.format((java.util.Date)ob) + "'";
                    }
                }
                else if (type.equals(AttributeType.GEOMETRY)) {
                    val = "ST_GeometryFromText( '"
                          + f.getGeometry().toText() + "', " + SRID + ")";
                }
                else {
                    val = "'" + f.getString(i).replaceAll ( "'", "''" ) + "'";
                }
                if (attrName.equals(uniqueCol)) {
                    uniqueVal = val;
                }
                else {
                    sqlBuf.append(" " + attrName + "=" + val + ",");
                }
            }
        }
        sqlBuf.deleteCharAt(sqlBuf.lastIndexOf(","));
        sqlBuf.append(" WHERE " + uniqueCol + " = " + uniqueVal);

        String sql = sqlBuf.toString();

        try {
            stmt.executeUpdate(sql);
        } catch (SQLException sqle) {
            throw new Exception(
                I18N.getText(PKG_KEY, KEY + ".error.update-failed") + 
                "\n" + sqle.toString() + "\n" + sql);
        }
    }

    /**
     * Insert feature in table. If there is an uniqueVal this value is used
     * to make the new row's identifier. If there is not an uniqueVal in feature
     * this value is assigned by the DBMS through autoincrement.
     */
    private void insertFeature (Feature f, boolean existsUnique, Statement stmt,
                 HashSet cols, FeatureSchema schema, int SRID ) throws Exception {
        StringBuffer sqlBuf = new StringBuffer("INSERT INTO " + table + " (");
        String sql = null;
        
        for (int i = 0; i < schema.getAttributeCount(); i++) {
            String name = schema.getAttributeName(i);
            if (cols.contains(name.toLowerCase( )) && 
                  (!name.equals(uniqueCol) || existsUnique)) {
                sqlBuf.append(" " + name.toLowerCase ( ) + ",");
            }
        }
        sqlBuf.deleteCharAt(sqlBuf.lastIndexOf(","));
        sqlBuf.append(" ) VALUES (");
        String insertQueryHead = sqlBuf.toString();
        sqlBuf = new StringBuffer(insertQueryHead);
        for (int j = 0; j < schema.getAttributeCount(); j++) {
            String name = schema.getAttributeName(j);
            if (cols.contains(name.toLowerCase( )) && 
                  (!name.equals(uniqueCol) || existsUnique)) {
                AttributeType type = schema.getAttributeType(j);
                
                if (type.equals(AttributeType.INTEGER)) {
                    if (f.getAttribute ( j ) == null) {
                        sqlBuf.append(" " + null + ",");
                    }
                    else {
                         Object ob = f.getAttribute(j);
                         String iType = ob.getClass().getName();
                         if (iType.indexOf ("Integer") >= 0) {
                             sqlBuf.append(" " + f.getInteger(j) + ",");
                         }
                         else { // Long
                             Long iObj = ( Long ) ob;
                             sqlBuf.append(" " + iObj.longValue( ) + ",");
                         }
                     }
                                      
                }
                else if (type.equals(AttributeType.DOUBLE)) {
                    if (f.getAttribute ( j ) == null) {
                        sqlBuf.append(" " + null + ",");
                    }
                    else {
                        Object ob = f.getAttribute (j);
                        String dType = ob.getClass ().getName();
                        if (dType.indexOf("Double") >= 0) {                                       
                            sqlBuf.append(" " + f.getDouble(j) + ","); 
                        }
                        else { // Big Decimal
                            BigDecimal dObj = ( BigDecimal ) ob;
                            sqlBuf.append(" " + dObj.doubleValue ( ) + ","); 
                        }                                       
                    }
                                      
                }
                else if (type.equals(AttributeType.DATE)) {
                    if (f.getAttribute ( j ) == null) {
                        sqlBuf.append(" " + null + ",");
                    }
                    else {
                        Object ob = f.getAttribute (j);
                        sqlBuf.append(" '" + sdf.format((java.util.Date)ob) + "',");
                    }
                }
                else if (type.equals(AttributeType.GEOMETRY)) {
                    sqlBuf.append(
                        " GeometryFromText( '"
                            + f.getGeometry().toText() + "', " + SRID + "),");
                } else {
                    String fString = f.getString(j);
                    sqlBuf.append(" '" + fString.replaceAll( "'", "''" ) + "',");
                }
            }
        }
        sqlBuf.deleteCharAt(sqlBuf.lastIndexOf(","));
        sqlBuf.append(" ) ");
        sql = sqlBuf.toString();
  
        try {
            stmt.executeUpdate(sql);
        } catch (SQLException sqle) {
            throw new Exception(
                I18N.getText(PKG_KEY, KEY + ".error.insert-failed") + 
                "\n" + sqle.toString() + "\n" + sql);
        }
    }


    /**
     * Insert feature in table. No unique column must exist! U.D.
     */
    private void insertFeatureNew (Feature f, Statement stmt, HashSet cols, 
                              FeatureSchema schema, int SRID) throws Exception {
        StringBuffer sqlBuf = new StringBuffer("INSERT INTO " + table + " (");
        String sql = null;
        
        for (int i = 0; i < schema.getAttributeCount(); i++) {
            String name = schema.getAttributeName(i);
            if (cols.contains(name.toLowerCase( ))) {
                sqlBuf.append(" " + name.toLowerCase ( ) + ",");
            }
        }
        sqlBuf.deleteCharAt(sqlBuf.lastIndexOf(","));
        sqlBuf.append(" ) VALUES (");
        String insertQueryHead = sqlBuf.toString();
        sqlBuf = new StringBuffer(insertQueryHead);
        for (int j = 0; j < schema.getAttributeCount(); j++) {
            String name = schema.getAttributeName(j);
            if (cols.contains(name.toLowerCase ( ))) {
                AttributeType type = schema.getAttributeType(j);
                if (type.equals(AttributeType.INTEGER)) {
                    if (f.getAttribute (j) == null) {
                        sqlBuf.append(" " + null + ",");
                    }
                    else {
                        Object ob = f.getAttribute ( j );
                        String iType = ob.getClass ( ).getName( );
                        if (iType.indexOf ("Integer") >= 0) {
                            Integer iObj = (Integer) ob;
                            sqlBuf.append(" " + f.getInteger(j) + ",");
                        }
                        else { // Long
                            Long iObj = (Long)ob;
                            sqlBuf.append(" " + iObj.longValue( ) + ",");
                        }
                    }
                }
                else if (type.equals(AttributeType.DOUBLE)) {
                    if (f.getAttribute ( j ) == null) {
                        sqlBuf.append(" " + null + ",");
                    }
                    else { 
                        Object ob = f.getAttribute ( j );
                        String dType = ob.getClass ( ).getName( );
                        if (dType.indexOf("Double") >= 0) { 
                            // To catch Not A Number; UD. 2010.11.05    
                            if (Double.isNaN(f.getDouble(j))) {
                                sqlBuf.append(" " + "'NaN'" + ",");  
                            }
                            else {
                                sqlBuf.append(" " + f.getDouble(j) + ","); 
                            }
                        }
                        else { // Big Decimal
                            BigDecimal dObj = ( BigDecimal ) ob;
                            sqlBuf.append(" " + dObj.doubleValue ( ) + ","); 
                        }
                    }
                                      
                }
                else if (type.equals(AttributeType.DATE)) {
                    if (f.getAttribute ( j ) == null) {
                        sqlBuf.append(" " + null + ",");
                    }
                    else {
                        Object ob = f.getAttribute ( j );
                        sqlBuf.append(" '" + sdf.format((java.util.Date)ob) + "',"); 
                    }
                }
                else if (type.equals(AttributeType.GEOMETRY)) {
                    // empty geometries are not saves
                    String sGeom = f.getGeometry( ).toText( );
                    if (sGeom.indexOf( "EMPTY" ) >= 0) {
                        errGeomEmpty = true;
                        return;
                    }
                    sqlBuf.append(" ST_GeometryFromText( '"
                            + f.getGeometry().toText()
                            + "', " + SRID + "),");
                }
                else {
                    // ud. for apostrophe in text
                    String fString = f.getString(j);
                    sqlBuf.append(" '" + fString.replaceAll( "'", "''" ) + "',");
                }
            }
        }
        sqlBuf.deleteCharAt(sqlBuf.lastIndexOf(","));
        sqlBuf.append(" ) ");
        sql = sqlBuf.toString();
    
        try {
            stmt.executeUpdate(sql);
        } catch (SQLException sqle) {
            throw new Exception(
                I18N.getText(PKG_KEY, KEY + ".error.insert-failed") + 
                "\n" + sqle.toString() + "\n" + sql);
        }
    }

    /**
     * Reads the query + connection properties into global variables.
     */
    private void readProperties() {
        server = (String)properties.get( PostGISDataSource.SERVER_KEY );
        port = (String)properties.get( PostGISDataSource.PORT_KEY );
        database = (String)properties.get( PostGISDataSource.DATABASE_KEY );
        table = (String)properties.get( PostGISDataSource.TABLE_KEY );
        where = (String)properties.get( PostGISDataSource.WHERE_KEY);
        username = (String)properties.get( PostGISDataSource.USERNAME_KEY );
        password = (String)properties.get( PostGISDataSource.PASSWORD_KEY );
        uniqueCol = (String)properties.get( PostGISDataSource.UNIQUE_COLUMN_KEY );
        method = (String)properties.get( PostGISDataSource.SAVE_METHOD_KEY );
    }
  
    /**
     * Opens a connection to a PostgresSQL database.
     */
    private java.sql.Connection connect() {
        try {
            String jdbcUrl = "jdbc:postgresql://" + server + ":" + port + "/" + database;
            Class.forName( "org.postgresql.Driver" );
            return(DriverManager.getConnection( jdbcUrl, username, password ));
        }
        catch(ClassNotFoundException cnfe) {
            if (PostGISPlugIn.DEBUG) cnfe.printStackTrace();
            throw new IllegalStateException(
                I18N.getText(PKG_KEY, KEY + ".error.could-not-load-driver") + 
                "\n" + cnfe.getMessage());  
        }
        catch(SQLException sqle) {
            if (PostGISPlugIn.DEBUG) sqle.printStackTrace();
            throw new IllegalStateException(
                I18N.getText(PKG_KEY, KEY + ".error.could-not-connect-to-database") + 
                "\n" + sqle.getMessage());
        } 
    }
  
    /**
     * @see Connection#close()
     */
    public void close() {}

    /**
     * @see com.vividsolutions.jump.io.datasource.Connection#executeQuery(java.lang.String, java.util.Collection, com.vividsolutions.jump.task.TaskMonitor)
     */
    public FeatureCollection executeQuery(String query, Collection exceptions, TaskMonitor monitor) {
        return executeQuery(query);
    }

    /**
     * @see com.vividsolutions.jump.io.datasource.Connection#executeQuery(java.lang.String, com.vividsolutions.jump.task.TaskMonitor)
     */
    public FeatureCollection executeQuery(String query, TaskMonitor monitor) throws Exception {
        return executeQuery(query);
    }

    /**
     * @see com.vividsolutions.jump.io.datasource.Connection#executeUpdate(java.lang.String, com.vividsolutions.jump.feature.FeatureCollection, com.vividsolutions.jump.task.TaskMonitor)
     */
    public void executeUpdate(String query, FeatureCollection featureCollection, TaskMonitor monitor) throws Exception {
        executeUpdate(query, featureCollection);
    }

}
 