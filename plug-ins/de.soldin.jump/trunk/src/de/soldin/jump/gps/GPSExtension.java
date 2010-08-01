/*
 * Copyright 2004 - 2010 Edgar Soldin 
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
 * 
 * TODO: - internationalize strings
 *       - add gui support for all other gpsinput methods
 *       - rxtx loader autoincrement through <os>/<arch> folders ?
 *       - track when receive new position instead of time ?
 *       - move crosshair between tracking intervals
 */
package de.soldin.jump.gps;

import de.soldin.jump.*;
import gnu.io.Loader;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;
import java.util.Map.Entry;

import org.apache.log4j.Logger;

import test.LoaderURLTest2;

import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;


/**
 * Installs {@link de.soldin.jump.gps.GPSTrackerPlugin}.
 * 
 * @see com.vividsolutions.jump.workbench.plugin.Extension
 */
public class GPSExtension
	extends Extension
	{
	public static final String NAME = "GPS Extension";
	public static final String VERSION = "0.2rc1";
	public static final String MENUENTRY = "GPS";
	public static final String BASEFOLDER = GPSExtension.class.getCanonicalName() + "-basefolder";
	private WorkbenchContext wbc = null;
	
	public void configure(PlugInContext context) throws Exception {
		this.wbc = context.getWorkbenchContext();
		/*
		JarClassLoader jcl = createClassLoader();
		
		Class clazz = jcl.loadClass("de.soldin.jump.gps.GPSTrackerPlugin");
		PlugIn gps = (PlugIn) clazz.newInstance();
		
		// JCL does not support getResource/getCodeSource, we tell the plugins manually here
		wbc.getBlackboard().put( BASEFOLDER, getBaseFolder() );
		gps.initialize(context);
		*/
		URLClassLoader ecl = createExtClassLoader();
		
		URL[] urls = ecl.getURLs();
		for (int i = 0; i < urls.length; i++) {
			System.out.println("url-"+i+" : "+urls[i]);
		}
		
		/*
		System.out.println(getName()+" gps cl is: " + ecl ) ;
		//System.out.println(getName()+" found: " + ecl.findResource("de/soldin/jump/gps/GPSTrackerPlugin.class") ) ;
		
		Class clazz = ecl.loadClass("de.soldin.jump.gps.GPSTrackerPlugin");
		Class pluginclazz = ecl.loadClass( "com.vividsolutions.jump.workbench.plugin.PlugIn" );
		System.out.println(this.getClass().getName()+" clazz clazz/cl: "+clazz+" / "+clazz.getClassLoader());
		System.out.println(this.getClass().getName()+" clazz plclazz/pl.cl: "+pluginclazz+" / "+pluginclazz.getClassLoader());
		
		//PlugIn clazz3 = (PlugIn) clazz.cast( pluginclazz );
		Object o = clazz.newInstance();
		System.out.println(getName()+" gps o is: " + o + " is PlugIn "+(o instanceof GPSTrackerPlugin?"ja":"nein")+" is "+o.getClass().getCanonicalName()) ;

		Method m = null;
		Method[] ms = o.getClass().getMethods();
		for (int i = 0; i < ms.length; i++) {
			if ( ms[i].getName().endsWith("initialize") ){
				System.out.println(getName()+" m"+i+" : "+ ms[i].toString() ) ;
				m =  ms[i];
			}
		}
		*/
		//Method m = o.getClass().getMethod( "initialize", new Class[]{PlugInContext.class});
		//m.invoke(o, context);
		
		//System.out.println(getName()+" gps cl is: " + gps.getClass().getClassLoader() ) ;
		
		Class clazz = ecl.loadClass("de.soldin.jump.gps.GPSTrackerPlugin");
		PlugIn gps = (PlugIn) clazz.newInstance();
		gps.initialize(context);
		
		System.out.println(getName()+" gps cl is: " + gps.getClass().getClassLoader() ) ;
		
	}
	
	/*private JarClassLoader createClassLoader() throws Exception{
		
		JarClassLoader cl = new JarClassLoader();
		String base = getBase();
		// add gps.jar
		cl.add( base );
		System.out.println(getName()+" base is: "+base);
		// add gps/*.*
		String libFolder = getLibFolder();
		cl.add( libFolder );
		System.out.println(getName()+" libs are in: "+libFolder);
		
		return cl;
	}*/

	private URLClassLoader createExtClassLoader() throws Exception{
		
		ExtClassLoader cl = new ExtClassLoader( this.getClass().getClassLoader(), false );
		
		//EatThisClassLoader2 cl = new EatThisClassLoader2(new URL[]{});
		
		String base = getBase();
		// add gps.jar
		cl.add( base );
		System.out.println(getName()+" base is: "+base);
		// add gps/
		String libFolder = getLibFolder();
		cl.add( libFolder );
		System.out.println(getName()+" libs are in: "+libFolder);
		// add gps/*.jar
		cl.addAllFiles( libFolder );
		/*
		File file = new File( libFolder );
		File[] files = file.listFiles();
		for (int i = 0; files != null && i < files.length; i++) {
			file = files[i];
			if ( file.getName().endsWith(".jar") )
				cl.add( libFolder + file.getName() );
				System.out.println(getName()+" add lib: "+ libFolder + "/" + file.getName());
		}
		*/
		
		System.out.println(getName()+" gps find: " + getClass().getClassLoader().loadClass("de.soldin.jump.cts.CSLayerSetExtension") 
				+ " in " + de.soldin.jump.cts.CSLayerSetExtension.class + " / " 
				+ getLibFolder( de.soldin.jump.cts.CSLayerSetExtension.class, "cts"  )) ;
		
		// find cts extension	//getClass().getClassLoader().loadClass("de.soldin.jump.cts.CSLayerSetExtension");	
		try {
			Class cts = de.soldin.jump.cts.CSLayerSetExtension.class;
			System.out.println(getName()+" cts find: " + cts );
			libFolder = getLibFolder( cts, "cts"  );
			System.out.println(getName()+" libfolder find: " + libFolder );
			cl.addAllFiles( libFolder );
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		
		//cl.addURLs( ((URLClassLoader)this.getClass().getClassLoader()).getURLs() );
		
		return cl;
	}
	
	static public String getBase(){
		return getBase( null );
	}
	
	static public String getBase( Class clazz ){
		
		URL whereami = clazz instanceof Class ?
						clazz.getProtectionDomain().getCodeSource().getLocation() :
						GPSExtension.class.getProtectionDomain().getCodeSource().getLocation();
		System.out.println( NAME + " is in: "+whereami );
		
		if ( whereami == null ) {
			return "";
		}
		
		// postprocessing
		String path = whereami.toString();
		path = path.endsWith("!/") ? path.subSequence(0, path.length()-2).toString() : path ;
		path = path.startsWith("jar:") ? path.subSequence(4, path.length()).toString() : path ;
		path = path.startsWith("file:") ? path.subSequence(5, path.length()).toString() : path ;
		
		// as extension is always in jar, simply get the parent
		// should result in "JUMPHOME/ext/"
		File basefile = new File( path );
		String baseFolder = basefile.getAbsolutePath();
		
		return baseFolder;
	}
	
	static public String getBaseFolder(){
		return new File(getBase()).getParent();
	}
	static public String getLibFolder(){
		return getLibFolder( null, null );
	}
	static public String getLibFolder( Class clazz, String ext_id ){
		String prefix = "lib";
		System.out.println(getStaticName()+" libfolder() params: " + clazz + " / " + ext_id);
		ext_id = ext_id instanceof String ? ext_id : "gps";
		File basefile = new File( getBase( clazz ) );
		System.out.println(getStaticName()+" libfolder() ext: " + basefile + " / " + ext_id);
		String libFolder;
		if ( basefile.getAbsolutePath().endsWith(".jar") )
			libFolder = basefile.getParentFile().getAbsolutePath()+"/"+prefix+"/"+ext_id+"/";
		else // no jar eg. in eclipse
			libFolder = basefile.getParentFile().getAbsolutePath()+"/"+prefix+"/"+ext_id+"/";
		
		return libFolder;
	}
	
	public String getVersion(){ return VERSION; }
	
	public static String getStaticName(){ return GPSExtension.class.getName(); }
}
