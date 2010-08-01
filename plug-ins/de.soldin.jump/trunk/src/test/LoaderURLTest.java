package test;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;

import java.security.AccessController;
import java.security.CodeSigner;
import java.security.CodeSource;
import java.security.PermissionCollection;
import java.security.PrivilegedExceptionAction;
import java.security.ProtectionDomain;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Properties;
import java.util.Vector;

import com.vividsolutions.jump.workbench.plugin.PlugIn;


public class LoaderURLTest {
	public static void main(String[] args) throws Exception {
		URL[] urls;
		URL find = null;
		EatThisClassLoader2 pcl = null;
		
		ClassLoader cl = LoaderURLTest.class.getClassLoader();
		if ( cl instanceof URLClassLoader ) {
			URLClassLoader ucl = (URLClassLoader) cl;
			pcl = new EatThisClassLoader2(new URL[]{ new URL("file:/Y:/projekte/gps/proj_testoster/bin/") }, LoaderURLTest.class.getClassLoader(), false);
			//pcl.id = "Mainloader";
			//pcl.addURL( new File("Y:/projekte/gps/proj_test/bin/bin.jar").toURL() );
			//pcl.addURLs( test.getURLs() );
			pcl.addURLs( ucl.getURLs() );
			
			EatThisClassLoader2 test = new EatThisClassLoader2( new URL[]{ new URL("file:/Y:/projekte/gps/proj_test/bin/") }, pcl, true );
		
			//pcl.addCL( test );
			urls = pcl.getURLs();
			
			find = pcl.findResource("Hidden.class");
		}else{
			urls = new URL[]{(new File("no urlclassloader")).toURL()};
		}
				
		for (int i = 0; i < urls.length; i++) {
			System.out.println("url-"+i+" : "+urls[i]);
		}
		System.out.println("Found : "+find);
		
		for (Enumeration el=pcl.findResources("gnu/io/CommPortIdentifier.class"); el.hasMoreElements(); ) {
			System.out.println( el.nextElement() );
		}

		Class clazz = pcl.loadClass("Hidden");
		System.out.println( "Baba : " + LoaderURLTest.class.getClassLoader() );
		System.out.println( "Bubu : " + clazz.getClassLoader() );
		Object hid = clazz.newInstance();
		
	}
}


@SuppressWarnings("unchecked")
class EatThisClassLoader extends URLClassLoader {
	private String id = "no_id";
	
	// list of classloaders to ask if we fail ATTENTION: cls.loadclass results in a private classspace
	private Vector<URLClassLoader> cls = new Vector<URLClassLoader>();
	
	public EatThisClassLoader(URL[] urls) {
		this(urls, null, false);
	}
	public EatThisClassLoader(URL[] urls, ClassLoader parent, boolean set_parent) {
		super(urls, set_parent ? parent : null);
		this.id = getCaller();
		if (!set_parent)
			this.addCL( parent );
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
	public boolean remCL(ClassLoader cl){
		return this.cls.remove( cl );
	}
/*
    public byte[] findClassBytes2(String name){

        try{
        	URL url = this.findResource( name.replace('.', '/').concat(".class") );
        	System.out.println("HAllo : "+url);
            FileInputStream inFile = new
                FileInputStream( new File( url.toURI() ).getAbsoluteFile() );
            byte[] classBytes = new
                byte[inFile.available()];
            inFile.read(classBytes);
            return classBytes;
        }
        catch (Exception x){
            System.out.println(x.getMessage());
        	return null;
        }
    }
    public Class findClass(String name)throws
	    ClassNotFoundException
	    {
	
	    byte[] classBytes = this.findClassBytes2(name);
	    if (classBytes==null){
	        throw new ClassNotFoundException();
	    }
	    else{
	    	CodeSigner[] csgn = null;
	    	CodeSource cs = new CodeSource( super.findResource( name.replace('.', '/').concat(".class") ), csgn );
	    	ProtectionDomain pd = this.getProtectionDomain(cs);
	    	System.out.println( pd.getClassLoader().getClass().getName() );
	        return defineClass(name, classBytes, 0, classBytes.length, pd );
	    }
    }
*/
	public Class<?> loadClass(String name) throws ClassNotFoundException 
	{
        try {
			return super.loadClass( name );
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
    
    /*
     * Returned cached ProtectionDomain for the specified CodeSource.
     
    private ProtectionDomain getProtectionDomain(CodeSource cs) {
		if (cs == null)
		    return null;
	
		ProtectionDomain pd = null;
		PermissionCollection perms = getPermissions(cs);
		pd = new ProtectionDomain(cs, perms, this, null);
	
		return pd;
    }*/

    public String toString(){
    	return super.toString() + " --> " + id;
    }
    
	public static boolean isURLCL( ClassLoader cl ){
		return ( cl instanceof URLClassLoader );
	}
	
    public String getCaller() {
	    return Thread.currentThread().getStackTrace()[3].toString(); 
    }

}

