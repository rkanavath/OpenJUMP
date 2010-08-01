package test;

import gnu.io.Loader;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

public class SystemProps {
	public static void main(String[] args) throws Exception {
		Properties props = System.getProperties();
		Properties sorted = new SortedProperties(); sorted.putAll(props);
		for (Object entry : new TreeSet(props.keySet())) {
			System.out.println(entry + " : " + props.getProperty(entry.toString()));
		}
		
		String os = System.getProperty("os.name");
		String sep = System.getProperty("file.separator");
		os = os.trim().substring(0, os.indexOf(' ')).toLowerCase();
		
		System.out.println("'"+os+"'");
		
		File file = new File("Y:/projekte/gps/proj_rxtx/lib/rxtx");
		if ( file.isDirectory() ){
			file = new File( file.getAbsolutePath() + sep + os + sep);
			System.out.println("exist :" + file);
			if ( file.isDirectory()){
				File[] files = file.listFiles();
				for (int i = 0; i < files.length; i++) {
					file = files[i];
					if ( file.isDirectory() )
						System.out.println("subdir : " + file);
				}
			}
		}
	}
}

class SortedProperties extends Properties {
	  /**
	   * Overrides, called by the store method.
	   */
	  @SuppressWarnings("unchecked")
	  public synchronized Enumeration keys() {
	     Enumeration keysEnum = super.keys();
	     Vector keyList = new Vector();
	     while(keysEnum.hasMoreElements()){
	       keyList.add(keysEnum.nextElement());
	     }
	     Collections.sort(keyList);
	     return keyList.elements();
	  }
	  
}
