package test;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;

public class LoaderTest {
	public static void main(String[] args) throws Exception {
		URL[] urls = {(new File("Y:/projekte/gps/proj_test/bin/")).toURL()};
		ClassLoader cl = new URLClassLoader( urls );
		
		Class hidden = cl.loadClass("Hidden");
		hidden.newInstance();
		System.out.println("Done");
	}
}
