package de.soldin.jump;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Vector;

@SuppressWarnings("unchecked")
public class ExtClassLoader extends URLClassLoader {
	private String id = null;
	
	// list of classloaders to ask if we fail ATTENTION: cls.loadclass results in a private classspace
	private Vector<URLClassLoader> cls = new Vector<URLClassLoader>();

	public ExtClassLoader() {
		super(new URL[]{}, null);
		this.id = getCaller();
	}
	public ExtClassLoader(URL[] urls) {
		super(urls, null);
		this.id = getCaller();
	}
	public ExtClassLoader(ClassLoader parent, boolean set_parent ) {
		super(new URL[]{}, set_parent ? parent : null);
		this.id = getCaller();
		if (!set_parent)
			this.addCL( parent );
	}
	public ExtClassLoader(URL[] urls, ClassLoader parent, boolean set_parent) {
		super(urls, set_parent ? parent : null);
		this.id = getCaller();
		if (!set_parent)
			this.addCL( parent );
	}
	public void add(String path) {
		try {
			super.addURL( new File( path ).toURL() );
		} catch (MalformedURLException e) {
			e.printStackTrace();
		}
    }
	public void addURL(URL url) {
		super.addURL(url);
    }
	public void addURLs(URL[] urls) {
		for (int i = 0; i < urls.length; i++) {
			super.addURL(urls[i]);
		}
	}
	public boolean addCL(ClassLoader cl){
		if ( isURLCL(cl) && ! this.cls.contains(cl) ) {
			this.cls.add(0, (URLClassLoader) cl );
			return true;
		}	
		
		return false;
	}
	public boolean addAllFiles(String path){
		return addAllFiles( path, null, false);
	}
	public boolean addAllFilesRecursive(String path){
		return addAllFiles( path, null, true);
	}
	public boolean addAllFiles(String path, String suffix, boolean recursive){
		// initialize null suffix to empty string
		suffix = suffix instanceof String ? suffix : "";
		File file = new File( path );
		File[] files = file.listFiles();
		// replace failed listing with file itself
		if ( ! (files instanceof File[]) ) files = new File[]{ file };
		// iterate through entries
		for (int i = 0; files != null && i < files.length; i++) {
			file = files[i];
			if ( file.getName().endsWith( suffix ) )
				if ( file.isDirectory() ){
					this.addAllFiles( file.getAbsolutePath(), suffix, false);
				}else{
					this.add( file.getAbsolutePath() );
				}
				//System.out.println(this.getClass().getName()+" added lib: "+ file.getAbsolutePath());
		}
		return true;
	}
	public boolean remCL(ClassLoader cl){
		return this.cls.remove( cl );
	}
	
	public Class<?> loadClass(String name) throws ClassNotFoundException 
	{
		
        try {
			return super.loadClass( name, false );
		} catch (ClassNotFoundException e) {
			// search next
		}
		
    	// search parents
		Iterator it = cls.iterator();
		while ( it.hasNext() ) {
			URLClassLoader cl = (URLClassLoader) it.next();
			try {
				return cl.loadClass(name);
			} catch (ClassNotFoundException e) {
				// try next
			}
		}
    			
		throw new ClassNotFoundException(name);
	}
	
	public URL findResource(final String name) {
    	
		URL url = super.findResource(name);

		Iterator it = cls.iterator();
		while ( it.hasNext() && ! (url instanceof URL) ) {
			URLClassLoader cl = (URLClassLoader) it.next();
			url = cl.findResource(name);
		}
		//System.out.println(this.getClass().getName()+" found: "+url);
    	return url;
    }
    
    public Enumeration<URL> findResources(final String name)
	throws IOException
    {
  	
        Vector<Enumeration> enums = new Vector<Enumeration>();
        // add mine
        enums.add( super.findResources(name) );
        // add parents
		Iterator it = cls.iterator();
		while ( it.hasNext() ) {
			URLClassLoader cl = (URLClassLoader) it.next();
			enums.add( cl.findResources(name) );
		}   
        // finalize an iterator for use in Enum
		final Vector enums_final = new Vector( enums );
        
    	return new Enumeration<URL>() {

    	    public URL nextElement() {
    	    	URL url = null;
    	    	Enumeration<URL> urls;
    			Iterator it = enums_final.iterator();
    			while ( it.hasNext() && url == null ) {
    				urls = (Enumeration<URL>) it.next();
    				while ( urls.hasMoreElements() && url == null ) {
    					url = (URL) urls.nextElement();
    				}
    			}
    	    	return url;
    	    }

    	    public boolean hasMoreElements() {
    			Iterator it = enums_final.iterator();
    			boolean more = false;
    			Enumeration<URL> urls;
    			while ( it.hasNext() && ! more) {
    				urls = (Enumeration<URL>) it.next();
    				more = urls.hasMoreElements();
    			}
    			return more;
    	    }
    	};
    }
    
    public URL[] getURLs() {
    	Vector urls = new Vector( Arrays.asList( super.getURLs() ) );

        // add parents
		Iterator it = cls.iterator();
		URL[] parents;
		while ( it.hasNext() ) {
			URLClassLoader cl = (URLClassLoader) it.next();
			if (cl == this) break;
			parents = cl.getURLs();
			urls.addAll( Arrays.asList(parents) );
		}    	
   	  	
    	return (URL[]) urls.toArray( new URL[urls.size()] );
    }
    
    public String toString(){
    	return super.toString() + " --> " + id;
    }
    
	public static boolean isURLCL( ClassLoader cl ){
		return ( cl instanceof URLClassLoader );
	}
	
    public String getCaller() {
	    return Thread.currentThread().getStackTrace()[3].toString(); 
    }

	static public String getBase(){
		return getBase( null );
	}
	
	static public String getBase( Class clazz ){
		
		URL whereami = clazz instanceof Class ?
						clazz.getProtectionDomain().getCodeSource().getLocation() :
						ExtClassLoader.class.getProtectionDomain().getCodeSource().getLocation();
		System.out.println( getStaticName( clazz ) + " is in: "+whereami );
		
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
	/*
	static public String getLibFolder(){
		return getLibFolder( null, null );
	}*/
	static public String getLibFolder( Class clazz, String ext_id ){
		System.out.println(getStaticName( clazz )+" libfolder() params: " + clazz + " / " + ext_id);
		// no extension id delivers the lib root
		ext_id = ext_id instanceof String ? ext_id : ".";
		File basefile = new File( getBase( clazz ) );
		System.out.println(getStaticName( clazz )+" libfolder() base/extid: " + basefile + " / " + ext_id);
		String libFolder;
		String path = basefile.getAbsolutePath();
		// remove trailing slash for check
		if ( path.endsWith("/") )
			path = path.subSequence(0, path.length()-1).toString();
		System.out.println(getStaticName( clazz )+" libfolder() path: " + path);
		// get sep for matching
		String sep = System.getProperty("file.separator");
		// eclipse or what?
		if ( path.endsWith( sep + "bin" ) )
			// no jar but bin folder -> dev mode eg. in eclipse
			libFolder = basefile.getParentFile().getAbsolutePath()+sep+"lib"+sep+ext_id+sep;
		else
			libFolder = basefile.getParentFile().getAbsolutePath()+sep+ext_id+sep;
		
		return libFolder;
	}

	public static String getStaticName(Class clazz) {
		return clazz instanceof Class ? 
				clazz.getName() : 
				ExtClassLoader.class.getName();
	}
}
