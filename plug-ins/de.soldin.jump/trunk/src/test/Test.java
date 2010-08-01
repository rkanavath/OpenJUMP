package test;

import gnu.io.Loader;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;

public class Test {
	public static void main(String[] args) throws Exception {
		System.out.println( Test.class.getName().replace('.', '/')+".class" );
		System.out.println( ClassLoader.getSystemResource("de/soldin/jump/test/Test.class") );
		System.out.println( Test.class.getClassLoader().getResource("de/soldin/jump/test/Test.class") );
		System.out.println( Test.class.getClassLoader().getParent().getResource("de/soldin/jump/test/Test.class") );
		
		ClassLoader cl = Test.class.getClassLoader();
		while (cl instanceof ClassLoader) {
			System.out.println(cl.toString() + " : " + cl.getResource("de/soldin/jump/test/Test.class") );
			cl = cl.getParent();
		}
		
		System.out.println(Test.class.getSimpleName() + ".class" + " 2: " + Test.class.getResource( Test.class.getSimpleName() + ".class" ).toString() );

	}
}
