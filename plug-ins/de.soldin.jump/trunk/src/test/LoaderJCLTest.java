package test;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;

import org.xeustechnologies.jcl.JarClassLoader;

public class LoaderJCLTest {
	public static void main(String[] args) throws Exception {
		URL[] urls = {(new File("Y:/projekte/gps/proj_test/bin/")).toURL()};
		JarClassLoader cl = new JarClassLoader();
		cl.add("Y:/projekte/gps/proj_test/bin/");
		cl.getSystemLoader().setOrder(1);
		System.out.println(cl.getSystemLoader().getOrder());
		
		Class hidden = cl.loadClass("Hidden");
		hidden.newInstance();
		System.out.println("Done");
	}
}
