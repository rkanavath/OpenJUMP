package org.openjump.core.ui.plugin.layer.raster;



import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

public class ImageExtension extends Extension
{
  private static final String NAME = "Save Sextante Raster to Image (Giuseppe Aruta - http://sourceforge.net/projects/opensit/)";
  private static final String VERSION = "0.1 (2013-06-02)";

  public String getName()
  {
    return NAME;
}

  public String getVersion()
  {
    return VERSION;
  }

  public void configure(PlugInContext context)
    throws Exception
  {
 
    new SaveImageToRasterPlugIn().initialize(context);
   
    
   
    
  }
}