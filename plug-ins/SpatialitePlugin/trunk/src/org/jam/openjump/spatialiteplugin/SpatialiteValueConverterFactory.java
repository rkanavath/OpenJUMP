package org.jam.openjump.spatialiteplugin;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;
import com.vividsolutions.jump.datastore.jdbc.ValueConverter;
import com.vividsolutions.jump.datastore.jdbc.ValueConverterFactory;
import com.vividsolutions.jump.feature.AttributeType;

import java.io.IOException;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;

public class SpatialiteValueConverterFactory {
	  private final ValueConverter WKB_GEOMETRY_MAPPER = new WKBGeometryValueConverter();
	  private final WKBReader wkbReader = new WKBReader();
	  private int geometryCol=-1;
	  
	  public SpatialiteValueConverterFactory(int geometryCol){
		  this.geometryCol=geometryCol;
	  }


	  public ValueConverter getConverter(ResultSetMetaData rsm, int columnIndex)
	      throws SQLException
	  {
		if (geometryCol<0) throw new IndexOutOfBoundsException("No geometry column index");  
	    
	    if (columnIndex ==geometryCol)  return WKB_GEOMETRY_MAPPER;
	    ValueConverter stdConverter = ValueConverterFactory.getConverter(rsm, columnIndex);
	    if (stdConverter != null)   return stdConverter;
	    return ValueConverterFactory.STRING_MAPPER;
	  }
	  
	  class WKBGeometryValueConverter implements ValueConverter
	  {
	    public AttributeType getType() { return AttributeType.GEOMETRY; }
	    public Object getValue(ResultSet rs, int columnIndex)
	        throws IOException, SQLException, ParseException
	    {
			Geometry geom;
			byte[]  wkb = SpatialiteGeometryBlob.getWKB(rs, columnIndex);
			geom =wkbReader.read(wkb);
            //System.out.println(geom);
			return geom;
	    }
	  }


	  
	  
}
