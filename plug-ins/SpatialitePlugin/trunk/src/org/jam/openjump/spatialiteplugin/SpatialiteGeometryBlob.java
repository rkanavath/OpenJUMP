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

import org.apache.log4j.Logger;

import java.io.*;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;

public class SpatialiteGeometryBlob {
	static final Logger logger = Logger.getLogger(SpatialiteGeometryBlob.class);
	private byte endian;
	
	protected int srid;
	protected double mbr_min_x;
	protected double mbr_min_y;
	protected double mbr_max_x;
	protected double mbr_max_y;
	protected int classtype;
	protected byte[] wkb;
	
	void writeBlobTofile(InputStream ins) throws IOException{
		FileOutputStream fos=new FileOutputStream("c:/tmp/sqlite.blob");
		byte[] buf =new byte[ins.available()];
		ins.read(buf);
		fos.write(buf);
		fos.close();
	}
	
	int loadEndianInt(InputStream st,Boolean bigEndian) throws IOException{
		byte[] buf4 =new byte[4];
		st.read(buf4);
		if (bigEndian) return (buf4[3] << 24)+((buf4[2] & 0xFF) << 16)+
							  ((buf4[1] & 0xFF) << 8) + (buf4[0] & 0xFF);
		else  return (buf4[0] << 24)+((buf4[1] & 0xFF) << 16)+
		  ((buf4[2] & 0xFF) << 8) + (buf4[3] & 0xFF);

	}
	double loadEndianDouble(InputStream st,Boolean bigEndian) throws IOException{
		byte[] b =new byte[8];
		st.read(b);
		double d=0.0f;
		if (bigEndian){ 
			d=((b[7]&0xFF)<<56)+
			((b[6]&0xFF)<<48)+
			((b[5]&0xFF)<<40)+
			((b[4]&0xFF)<<32)+
			((b[3]&0xFF)<<24)+
			((b[2]&0xFF)<<16)+
			((b[1]&0xFF)<<8)+
			((b[0]&0xFF));
		}else  for (int i=0;i<8;i++) d=((b[i] & 0xFF)<<8*(7-i));
		return d;
	}
	
	
	/**
	 * @param rs
	 * @param numCol
	 * @return
	 */
	public boolean loadFromResultset(ResultSet rs, int numCol){
		//InputStream iStream;
		
		try {
			//iStream = rs.getBinaryStream(numCol);
            byte[] bytes = rs.getBytes(numCol);
			//writeBlobTofile(rs.getBinaryStream(numCol));
			//if (iStream.available()<59) return false; //minimun size of point blob 65?
            if (bytes.length < 59) return false;
			//DataInputStream di = new DataInputStream(iStream);
			//if (di.readByte()!=0) return false;
            int i = 0;
            if (bytes[i++] != 0) return false;
			//endian = di.readByte();
            endian = bytes[i++];
			//srid = loadEndianInt(di, endian==1);
            srid = loadEndianInt(new ByteArrayInputStream(bytes, i, 4), endian==1);
            i += 4;
			//di.skipBytes(32);
            i += 32;
/*			mbr_min_x=di.readDouble();
			mbr_min_y=di.readDouble();
			mbr_max_x=di.readDouble();
			mbr_max_y=di.readDouble();
*/

/*			mbr_min_x=loadEndianDouble(di, endian==1);
			mbr_min_y=loadEndianDouble(di, endian==1);
			mbr_max_x=loadEndianDouble(di, endian==1);
			mbr_max_y=loadEndianDouble(di, endian==1);
			System.out.println(String.format("mx: %f mf: %f",mbr_min_x, mbr_min_y));
*/
            //new byte[di.available()-1];
            wkb = new byte[bytes.length-i-1];
			System.arraycopy(bytes, i, wkb, 0, wkb.length);
			//di.read(wkb);
            wkb[0] = endian;

            //System.out.println("longueur wkb : " + wkb.length);
            //System.out.println("debut wkb : " + wkb[0] + "-" + wkb[1] + "-" + wkb[2] + "-" + wkb[3] + "-" + wkb[4]);
            //System.out.println("debut wkb : " + wkb[0] + "-" + wkb[1] + "-" + wkb[2] + "-" + wkb[3] + "-" + wkb[4]);
            //System.out.println("nb points : " + wkb[5] + "-" + wkb[6] + "-" + wkb[7] + "-" + wkb[8]);

			//di.close();
			//iStream.close();
			return true;

		} catch (SQLException e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
			return false;
		} catch (IOException e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
			return false;
		}
		
	}//end loadFromResultset
	
	public static  byte[] getWKB(ResultSet rs, int numCol){
		SpatialiteGeometryBlob sgb= new SpatialiteGeometryBlob();
		if (sgb.loadFromResultset(rs, numCol) )
			return sgb.wkb;
		else return null;
	}
	
	public static boolean isGeometryBlob(ResultSet rs , int numCol){
		SpatialiteGeometryBlob sgb= new SpatialiteGeometryBlob();
		return sgb.loadFromResultset(rs, numCol) ;
		
	}
	public static int getGeometryIndex(ResultSet rs){
		ResultSetMetaData md;

			try {
				md = rs.getMetaData();
				int numCols=md.getColumnCount();
				for (int i=0;i<numCols;i++){
					if (isGeometryBlob(rs,i+1)) return i+1;
				}
			} catch (SQLException e) {
				System.out.println(e.getMessage());
				e.printStackTrace();
			}
			return -1;
	}



}
