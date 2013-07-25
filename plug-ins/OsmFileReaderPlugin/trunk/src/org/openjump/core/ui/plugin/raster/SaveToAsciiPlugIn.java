package org.openjump.core.ui.plugin.raster;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.text.NumberFormat;
import java.util.Properties;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

import org.openjump.core.apitools.LayerTools;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.rasterimage.sextante.OpenJUMPSextanteRasterLayer;
import org.openjump.core.rasterimage.sextante.rasterWrappers.GridWrapperNotInterpolated;
import org.openjump.core.ui.plugin.file.open.JFCWithEnterAction;
import org.openjump.core.ui.plugin.layer.pirolraster.LoadSextanteRasterImagePlugIn;

import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GenericNames;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.plugin.FeatureInstaller;

public class SaveToAsciiPlugIn extends AbstractPlugIn{

	protected double[][] data;
	private Properties properties = null;
	private static String propertiesFile = LoadSextanteRasterImagePlugIn.getPropertiesFile();
	private String lastPath;
	NumberFormat cellFormat = null;
	public double defaultNoData = -9999;

	public void initialize(PlugInContext context) throws Exception
	{    

		WorkbenchContext workbenchContext = context.getWorkbenchContext();
		new FeatureInstaller(workbenchContext);

        context.getFeatureInstaller().addMainMenuPlugin(
        		this, 
        		new String[] {MenuNames.RASTER},
        		I18N.get("Save As ASCII Grid File..."), 
				false, 
				null,
				createEnableCheck(context.getWorkbenchContext()));

	}
    
	public boolean execute(PlugInContext context) throws Exception{
		//I am not sure, but I think value that indicates fractions is standardized as a "." for ASCII grids?
		//I.e. in German it is 5,9 while in the US it is 5.9 ???
		cellFormat = NumberFormat.getNumberInstance();
		cellFormat.setMaximumFractionDigits(3);
		cellFormat.setMinimumFractionDigits(0);
		
		JFileChooser fc = new JFCWithEnterAction();

		fc.setFileFilter(new FileFilter() {
			public boolean accept(File f) {
				return (f.isDirectory()) ||
						(f.getName().toLowerCase().endsWith(".asc"));
			}

			public String getDescription() {
				return "Arc/Info ASCII Grid (.asc)";
			}
		});
		this.properties = new Properties();
		
		try {
			FileInputStream fis = new FileInputStream(propertiesFile);
			this.properties.load(fis);
			this.lastPath = this.properties.getProperty(LoadSextanteRasterImagePlugIn.KEY_PATH);
			fis.close();
		}
		catch (FileNotFoundException e) {
			//not sure if it is necessary to show this warning, because the idea is to rather store when there has been no file before.
			context.getWorkbenchFrame().warnUser(I18N.get("org.openjump.core.ui.plugin.layer.pirolraster.SaveRasterImageAsImagePlugIn.File-not-found"));
		}
		catch (IOException e) {
			context.getWorkbenchFrame().warnUser(GenericNames.ERROR);
		}

		if (this.lastPath != null) {
			fc.setCurrentDirectory(new File(this.lastPath));
		}
		
		fc.setMultiSelectionEnabled(false);

		fc.setDialogTitle(getName());
		int returnVal = fc.showSaveDialog(fc);
		if (returnVal == 0) {
			String ascFileName = fc.getSelectedFile().getAbsolutePath();

			if (!ascFileName.toLowerCase().endsWith(".asc".toLowerCase())) {
				ascFileName = ascFileName + ".asc";
			}

			File ascFile = new File(ascFileName);

			FileOutputStream ascOut = new FileOutputStream(ascFile);

			RasterImageLayer rLayer = (RasterImageLayer)LayerTools.getSelectedLayerable(context, RasterImageLayer.class);
			OpenJUMPSextanteRasterLayer rstLayer = new OpenJUMPSextanteRasterLayer();
			rstLayer.create(rLayer);
			
			defaultNoData = rstLayer.getNoDataValue(); //added this

			PrintStream o = new PrintStream(ascOut);
			o.println( "ncols " + rLayer.getOrigImageWidth()  );
			//rstLayer.getNX()  );
			o.println( "nrows " + rLayer.getOrigImageHeight()  );
			//rstLayer.getNY() );
			o.println( "xllcorner " + rLayer.getEnvelope().getMinX() );
			o.println( "yllcorner " + rLayer.getEnvelope().getMinY());
			o.println( "cellsize " + rstLayer.getLayerCellSize() );
			String sNoDataVal = "";
			if( Math.floor(defaultNoData) == defaultNoData){
				sNoDataVal = Integer.toString((int)defaultNoData); 
			}
			else{
				sNoDataVal = Double.toString(defaultNoData);
			}
			o.println( "NODATA_value " + sNoDataVal);


			GridWrapperNotInterpolated gwrapper = new GridWrapperNotInterpolated(rstLayer, rstLayer.getLayerGridExtent());


			int nx = rstLayer.getLayerGridExtent().getNX();
			int ny = rstLayer.getLayerGridExtent().getNY();
			for (int y = 0; y < ny; y++) {//rows
				StringBuffer b = new StringBuffer();
				for (int x = 0; x < nx; x++) {//cols
					double value = gwrapper.getCellValueAsDouble(x, y, 0); //this was before "getCellValueAsFloa()"... not sure why you would want a float?
					if( Double.isNaN(value)) value = defaultNoData;
					// show int as int
					if(Math.floor(value) == value){
						b.append((int)value + " "); //cast to int
					}
					else{
						b.append( value + " ");
					}
				}
				o.println( b );
			}
			o.close();

			rLayer.setImageFileName(ascFileName);
			rLayer.setNeedToKeepImage(false);
		}

		return true;
			}

	public static MultiEnableCheck createEnableCheck(WorkbenchContext workbenchContext)
	{
		EnableCheckFactory checkFactory = new EnableCheckFactory(workbenchContext);
		MultiEnableCheck multiEnableCheck = new MultiEnableCheck();

		multiEnableCheck.add(checkFactory.createExactlyNLayerablesMustBeSelectedCheck(1, RasterImageLayer.class));

		return multiEnableCheck;
	}

}
