package test;

import com.vividsolutions.jump.workbench.plugin.*;
import java.util.*;
import java.util.regex.Pattern;

public class I18Test
    extends AbstractPlugIn
{
  // this is the path to the properties-files
  //    myinternational.properties       = language not suported
  //    myinternational_de.properties    = german
  //    myinternational_en.properties    = english
  //    etc.

  String basename = "de.soldin.jump.test.i18test";
  ResourceBundle res;

  public I18Test()
  {
  }

  public void initialize(PlugInContext context) throws Exception
  {

    res = ResourceBundle.getBundle(basename);

    // example 
    System.out.println(getString("Hallo Welt"));
    
    System.out.println("pfad:"+ com.vividsolutions.jts.geom.Geometry.class.getProtectionDomain().getCodeSource().getLocation().toURI());

    //System.out.println(Pattern.compile(",").matcher(System.getProperties().toString()).replaceAll("\n"));
    
    System.out.println(System.getProperty("library.path").toString());
    
    context.getFeatureInstaller().addMainMenuItem(this,
                                                  new String[]
                                                  {getString("Test")},
                                                  getName(), false, null, null);
  }

  public boolean execute(PlugInContext context) throws Exception
  {
    context.getWorkbenchFrame().getOutputFrame().createNewDocument();
    context.getWorkbenchFrame().getOutputFrame().addText(getString("Hallo"));
    context.getWorkbenchFrame().getOutputFrame().surface();
    return true;
  }

  public String getName()
  {
    return getString("Name");
  }

  private String getString(String s)
  {
    try
    {
      return res.getString(s);
    }
    catch (MissingResourceException ex)
    { // no entry 
      ex.printStackTrace();
      return "";
    }
    catch (Exception ex)
    { // no resource file
      ex.printStackTrace();
      return "";
    }

  }
}
