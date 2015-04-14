package org.jam.openjump.spatialiteplugin;


//import org.sqlite.jdbc.JdbcConnection;
//import org.sqlite.swig.SQLite3;
//import org.sqlite.swig.SWIGTYPE_p_p_char;
import org.sqlite.SQLiteConfig;

import java.io.IOException;
import java.sql.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class SpatialiteDb {

	private  Connection dbcon;
	private  int spatialMetaData;

	public boolean openDataBase(String database){
		try {
            SQLiteConfig config = new SQLiteConfig();
            config.enableLoadExtension(true);
            dbcon = DriverManager.getConnection("jdbc:sqlite:" + database, config.toProperties());
			//dbcon = DriverManager.getConnection("jdbc:sqlite:" + database);
            System.out.println(config);
            System.out.println(dbcon);
			
			if (loadGisExtension()){
				spatialMetaData = queryToInt("select CheckSpatialMetaData()");
                System.out.println("spatialMetaData: " + spatialMetaData);
				if (spatialMetaData == 2) { //FDO/OGR
					queryToInt("select AutoFDOStart()");
                }
				return true;
			} else return false;
		} catch (SQLException e) {
			e.printStackTrace();
			dbcon = null;
			return false;
		} catch (IOException e) {
            e.printStackTrace();
            dbcon = null;
            return false;
        }
	}
	
	public void closeDataBase(){
		closeDbObject(dbcon);
	}

	public void finalize() throws Throwable{
		closeDataBase();
		super.finalize();
	}
	
	public Statement createStatement() throws SQLException{
		return dbcon.createStatement();
	}
	
	public boolean isClosed(){
		if (dbcon ==null) return true;
		try {
            return dbcon.isClosed();
		} catch (SQLException e) {
			e.printStackTrace();
			return true;
		}

	}

	public int getSpatialMetaData(){
		return spatialMetaData;
	}
	
	public int queryToInt(String sql){
		Statement st = null;
		ResultSet rs = null;
		try{
			st = dbcon.createStatement();
			rs = st.executeQuery(sql);
			rs.next();
			return (int)rs.getInt(1);
		} catch (SQLException e) {
			e.printStackTrace();
			return -1;
		}finally{
			closeDbObject(rs);
			closeDbObject(st);
		}
	}
	
	public boolean getTablesNames(Tnode node){
		Map<String,String> vt =new HashMap<String, String>();
		List<TableType> lt = getTablesList();
		for (TableType t : lt) { if (t.virTable!=null) vt.put(t.virTable, t.name);}
		
		for (TableType t : lt) {
			Tnode nc = new Tnode();
			nc.caption=t.name;
			nc.cat=t.type;
			if (vt.containsKey(t.name)){ //is FDO table
				nc.code="SELECT * FROM "+vt.get(t.name);
				nc.des=String.format("Use %s table to access this table ",vt.get(t.name));
			}else nc.code="SELECT * FROM "+t.name;
			node.addChild(nc);
			getFieldsTable(t.name,nc);
			
		}
		node.trimToSize();
		node.sort();
		return lt.size()>0;
	}
	
	private void getFieldsTable(String table,Tnode node){
		ResultSet rsc = null;
		try{
			rsc=dbcon.getMetaData().getColumns(null, null, table, null);
			while (rsc.next()){
				Tnode col=new Tnode(String.format("%s [%s]", rsc.getString(4), rsc.getString(6)), rsc.getString(4) );
				col.cat=Tnode.CAT_FIELD;
				node.addChild(col);
			}
			node.trimToSize();
		} catch (SQLException e) {
			e.printStackTrace();
		}finally {closeDbObject(rsc);}
	}
	
	private List<TableType> getTablesList(){
		String  metadataSQL_0 = "SELECT tbl_name FROM sqlite_master as m"+
				"where (m.type=\'table\' or m.type=\'view\') ";
		
		String metadataSQL_1_2 = "SELECT tbl_name,f_geometry_column,sql FROM sqlite_master as m "+
		"left join geometry_columns as g on g.f_table_name=m.tbl_name "+ 
		"where (m.type=\'table\') ";
		Statement st=null;
		ResultSet rs =null;
		String sql=(spatialMetaData>0) ? metadataSQL_1_2 : metadataSQL_0;
		
		ArrayList<TableType> result= new ArrayList<TableType>();
		
		try{
		
			st=dbcon.createStatement();
			rs=st.executeQuery(sql);
			while (rs.next()){
				TableType tt=new TableType(rs.getString(1));
				result.add(tt);
				if (spatialMetaData==0) tt.type=Tnode.CAT_TABLE;
				else{
					tt.setTableType(spatialMetaData==2, 
							rs.getString(2), rs.getString(3));
				}
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}finally {
			closeDbObject(rs);
			closeDbObject(st);}
		return result;
		
	}


	
	
	public void closeDbObject(Object dbObject){
		if (dbObject == null) return;
		try {
			if (dbObject instanceof ResultSet) {
                ((ResultSet)dbObject).close();
            }
			else if (dbObject instanceof Statement) {
                if (((Statement)dbObject).getConnection().getMetaData() != null) {
                    ((Statement)dbObject).close();
                }
            }
			else if (dbObject instanceof Connection) {
                ((Connection)dbObject).close();
            }
			else System.err.println("Can't close object" + dbObject.toString());
			dbObject = null;
		} catch (SQLException e) {
            System.out.print(e.getMessage());
			//e.printStackTrace();
		}	
	}
	
	public boolean loadGisExtension() throws IOException {
		//SWIGTYPE_p_p_char errmsg = null;
        Properties properties = new Properties();
        properties.load(this.getClass().getResourceAsStream("spatialite.properties"));
        String loadExtension = properties.getProperty("load-extension", "");
		try {
            Statement stmt = dbcon.createStatement();
            stmt.setQueryTimeout(30); // set timeout to 30 sec.

            // loading SpatiaLite
            //stmt.execute("SELECT load_extension('libspatialite-1.dll')");
            System.out.println("sql : " + loadExtension);
            stmt.execute(loadExtension);
            System.out.println(loadExtension);
            // enabling Spatial Metadata
            // using v.2.4.0 this automatically initializes SPATIAL_REF_SYS and GEOMETRY_COLUMNS
            //stmt.execute("SELECT InitSpatialMetadata()");

            /*
            ((JdbcConnection)dbcon).enableLoadExtention();
		    errmsg = SQLite3.new_p_p_char();
            System.out.println(new File(".").getAbsolutePath());
            System.out.println(System.getProperty("java.library.path"));
            System.out.println(System.getProperty("java.class.path"));
            System.out.println(new File("C:\\Temp\\lib\\libspatialite-1.dll").exists());
		    ((JdbcConnection)dbcon).loadExtention("C:\\Temp\\lib\\libspatialite-1.dll", null, errmsg);
		    */
		} catch (SQLException e) {
            System.out.println("Can't load spatialite extension:");
            e.printStackTrace();
			return false;
		} finally{
			//SQLite3.delete_p_p_char(errmsg);
            //try {
                 //if(dbcon != null) dbcon.close();
            //} catch(SQLException e) {
                 //System.err.println(e);
            //}
		}
		return true;
	}
	
	public Object[][] loadData(String sql,List<Object> cols) throws SQLException {

		Statement st = dbcon.createStatement();
		try{
			ResultSet rs = st.executeQuery(sql);
			rs.setFetchDirection(ResultSet.FETCH_FORWARD);
			try{
				ResultSetMetaData md = rs.getMetaData();
				int numCols = md.getColumnCount();
				cols.clear();
				for (int i = 0 ; i < numCols ; i++) cols.add(md.getColumnName(i+1));
				List<Object> list = new ArrayList<Object>();
                //rs.last();
				//Object[][] dat=new Object[rs.getRow()][numCols];
				//rs.beforeFirst();
				while (rs.next()){
                    Object[] obj = new Object[numCols];
					for (int c = 0 ; c < numCols ; c++) {
                        obj[c] = rs.getObject(c+1);
						//dat[rs.getRow()-1][c]=rs.getObject(c+1);
                    }
                    list.add(obj);
				}
				//return dat;
                return list.toArray(new Object[0][0]);
			} finally {closeDbObject(rs);}
		} finally { closeDbObject(st);}
	}
}

final class TableType{
	String name;
	String geom_col=null;;
	String virTable=null;
	int type=-1;	
	
	TableType(String TableName){
		super();
		name=TableName;
		
	}
	
	void setTableType( Boolean isFDO  ,String geometryCol,String SQLdefinition){
		if (geometryCol!=null) {
			type= (isFDO)?Tnode.CAT_GEOTABLE_FDO:Tnode.CAT_GEOTABLE;

		}
		else{
			String sp =".*Virtual(Shape|FDO)\\((.*)\\).*";
			//String sp ="Virtual(Shape)(.*)";
			Pattern pat =Pattern.compile(sp,Pattern.MULTILINE | Pattern.DOTALL );
			Matcher m = pat.matcher(SQLdefinition);
			if (m.find()){
				if ("Shape".equals(m.group(1) )){
					type=Tnode.CAT_GEOTABLE_VSHAPE;
				}
				else{
					type=Tnode.CAT_GEOTABLE_VFDO;
					virTable=m.group(2);
				}
			}
			else{
				type=Tnode.CAT_TABLE;
			}
		}

	}
	

}
