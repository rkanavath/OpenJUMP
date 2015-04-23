/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI 
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * JUMP is Copyright (C) 2003 Vivid Solutions
 *
 * This program implements extensions to JUMP and is
 * Copyright (C) 2010 Jorge Almaraz.
 * 
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
 * For more information, contact:
 *
 * Jukka Rahkonen
 * jukka.rahkonen@latuviitta.fi
 * 
 */
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
