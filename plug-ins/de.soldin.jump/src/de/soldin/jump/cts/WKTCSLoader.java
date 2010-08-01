/**
 * @(#)WKTCSLoader.java	29.06.2004
 *
 * Copyright 2004 Edgar Soldin
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package de.soldin.jump.cts;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Locale;
import java.util.TreeMap;

import org.geotools.cs.CoordinateSystem;
import org.geotools.cs.CoordinateSystemFactory;
import org.geotools.cs.FactoryException;

/**
 * Derivated from {@link java.util.TreeMap} this class reads
 * a file <code>cs.conf</code>, which must be present in the 
 * classpath. Currently there is no error handeled in case it
 * is missing.
 * <p>
 * The {@link org.geotools.cs.CoordinateSystem}s are filed in
 * the object itself and available by the cs's name as key.
 * </p>
 */
public class WKTCSLoader extends TreeMap{
	private static final String FILE = CSLayerSetExtension.getLibFolder() + "cs.conf";
  
	public WKTCSLoader() throws Exception
	{
		super();
		try{
			// Load file line per line
			LineNumberReader reader = new LineNumberReader(new FileReader(new File(FILE)));
			String line;
			while ((line = reader.readLine()) != null) {
				if (line.startsWith("#") || line.length()<1)
					continue;
				try {
					CoordinateSystem cs = CoordinateSystemFactory.getDefault().createFromWKT(line);
					this.put(cs.getName(Locale.getDefault()),cs);
				} catch (FactoryException e1) {
					e1.printStackTrace();
				}
				
			}
			reader.close();
		}catch (Exception e){
			String msg = " Can't locate/read/write '"+FILE+"'. Make sure it's available and read/writable.";
			System.err.println(msg);
			throw new Exception(msg);
		}

	}
	
	public CoordinateSystem get(String key){
		return (CoordinateSystem)super.get(key);
	}
	
}
