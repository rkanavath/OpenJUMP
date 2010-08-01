package gnu.io;

import java.io.File;
import java.net.URI;
import java.net.URL;
import java.security.CodeSource;
import java.util.HashSet;

import sun.reflect.ReflectionFactory.GetReflectionFactoryAction;

public class Loader {

	private static HashSet memory = new HashSet();
	public static String libraryPath;
	
	static {
		System.out.println("RXTX Loader on: '"+System.getProperty("os.name")+"' '"+System.getProperty("os.arch")+"'");		
	}
	
	public static void loadLibrary(String name) {
		// already loaded? return
		if ( memory.contains( "loaded_" + name ) ) return;
		
		// path is set, absolute resolve and try to load it and return
		if ( libraryPath != null ) {
			String absolutePath = (new File(libraryPath)).getAbsolutePath();
			if ( ! memory.contains( "absPathMessage" )) {
				System.out.println("RXTX libraryPath is set to: '"+absolutePath+"'");
				memory.add( "absPathMessage" );
			}
		}	
		// no path? let's try to detect one
		else{
			detectBaseFolder();
		}

		// reset null to "." should result in cwd, worth a try
		if ( ! ( libraryPath instanceof String ) ) setLibraryPath( "." );
		
		// Try user installed libfile in rxtx/
		if ( _load( libraryPath + "/rxtx/" , name ) ) return;
		
		/*//Next try. Detect OS and try to load accordingly
		String os = System.getProperty("os.name").toLowerCase();
		String arch = System.getProperty("os.arch").toLowerCase();
		
		// detect current supported os's set fitting path id
		String id = null;
		if ( os.startsWith( "windows" ) ) {
			id = "win";
		}else if ( os.startsWith( "linux" ) ) {
			id = "linux";
		}
		
		// ignore arch for now, and blindly try both
		if ( id != null ){
			try { // try 32bit version	
				if ( _load( libraryPath + "/rxtx/" + id + "32/", name ) ) return;
			} catch (UnsatisfiedLinkError e32) {
				try { // try 64bit version	
					if ( _load( libraryPath + "/rxtx/" + id + "64/", name ) ) return;
				} catch (UnsatisfiedLinkError e64) {
					// TODO: tell user
				}
			}
		}*/
		
		// os aware load distributed libs v2
		String os = System.getProperty("os.name");
		String sep = System.getProperty("file.separator");
		os = os.trim().substring(0, os.indexOf(' ')).toLowerCase();
		
		File file = new File( libraryPath + sep + "rxtx" + sep );
		if ( file.isDirectory() ){
			file = new File( file.getAbsolutePath() + sep + os + sep);

			if ( file.isDirectory()){
				File[] files = file.listFiles();
				for (int i = 0; i < files.length; i++) {
					file = files[i];
					if ( file.isDirectory() )
						if ( _load( file.getAbsolutePath(), name ) ) return;
				}
			}
		}
		
		
		
		//Last try. Eventually use java.library.path
		//TODO: find some elegant way to tell user if this fails
		System.loadLibrary( name );
	}
	
	
	private static boolean _load( String path, String name ) {
		File libfile = new File( path + "/" + System.mapLibraryName(name) );
		// already loaded?
		if ( memory.contains( libfile ) )
			return true;
		else if ( libfile.exists() ) {
			try {
				System.load( libfile.getAbsolutePath() );
				// no exception? fine, remember it's already loaded
				memory.add( libfile );
				memory.add( "loaded_" +  name );
				System.out.println("RXTX succeeded loading: '"+libfile.getAbsolutePath()+"'");
				return true;
			} catch (UnsatisfiedLinkError e) {
				System.out.println("RXTX unsuccessfully tried: '"+libfile.getAbsolutePath()+"'");
			}
		}else{
			System.out.println("RXTX Loader could not find: " + libfile.getAbsolutePath());
		}
		return false; 
	}


	public static void detectBaseFolder() {
		// detect only once!
		if ( memory.contains("baseFolder_done") ) return;
		memory.add("baseFolder_done");
		
		// get location of class as file url eg. file:/y:/test/foo.jar
		String base = null;
		// try by ProtectionDomain
		try {
			 base = Loader.class.getProtectionDomain().getCodeSource().getLocation().toString();
		}catch (Throwable t) {
			// ignore, try next
		}
		// try by Classloader.getSystemResource
		if (base == null){
			String name = Loader.class.getName().replace('.', '/') + ".class";
			
			/*
			ClassLoader cl = Loader.class.getClassLoader();
			while (cl instanceof ClassLoader) {
				System.out.println(cl.toString() + " : " + cl.getSystemResource(name) );
				cl = cl.getParent();
			}
			*/
			
			URL loc = ClassLoader.getSystemResource( name );
			if (loc instanceof URL) {
				String locstring = loc.toString();
				if (locstring.indexOf(name) != -1)
					locstring = locstring.substring(0, locstring.length() - name.length());
				base = locstring;
			}
		}
		
		// postprocessing, only local paths are supported
		if (base instanceof String){
			base = base.endsWith("!/") ? base.subSequence(0, base.length()-2).toString() : base ;
			base = base.startsWith("jar:") ? base.subSequence(4, base.length()).toString() : base ;
			base = base.startsWith("file:") ? base.subSequence(5, base.length()).toString() : base ;
		}
		
		System.out.println("RXTX detected basefolder as: '"+base+"'");

		try {
			File basefile = new File( base );
			String baseFolder = basefile.getPath();
			if ( baseFolder.endsWith(".jar") )
				baseFolder = basefile.getParent();
			else // no jar eg. in eclipse, search in ../lib/
				baseFolder = basefile.getParent() + "/lib/";
			
			setLibraryPath( baseFolder );
			System.out.println("RXTX libfolder set to: '"+baseFolder+"'");
		} catch (Exception e) {
			System.out.println("RXTX failed to detect libfolder.");
		}
		
	}
	
	public static void setLibraryPath(String libraryPath) {
		Loader.libraryPath = libraryPath;
	}
}
