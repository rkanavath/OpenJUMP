/* 
 * Kosmo - Sistema Abierto de Información Geográfica
 * Kosmo - Open Geographical Information System
 *
 * http://www.saig.es
 * (C) 2006, SAIG S.L.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation;
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *
 * For more information, contact:
 * 
 * Sistemas Abiertos de Información Geográfica, S.L.
 * Avnda. República Argentina, 28
 * Edificio Domocenter Planta 2ª Oficina 7
 * C.P.: 41930 - Bormujos (Sevilla)
 * España / Spain
 *
 * Teléfono / Phone Number
 * +34 954 788876
 * 
 * Correo electrónico / Email
 * info@saig.es
 *
 */
package org.openjump.core.ui.plugin.layer.raster;


import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.Raster;
import java.awt.image.SampleModel;
import java.awt.image.WritableRaster;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.text.NumberFormat;
import java.util.HashMap;
import java.util.Properties;

import javax.media.jai.PlanarImage;
import javax.swing.Icon;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.apache.log4j.Logger;
import org.openjump.core.apitools.LayerTools;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.rasterimage.WorldFileHandler;
import org.openjump.core.rasterimage.sextante.OpenJUMPSextanteRasterLayer;
import org.openjump.core.rasterimage.sextante.rasterWrappers.GridWrapperNotInterpolated;
import org.openjump.core.ui.plugin.layer.pirolraster.LoadSextanteRasterImagePlugIn;


import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.util.FileUtil;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.ThreadedPlugIn;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.GenericNames;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.images.IconLoader;
import com.vividsolutions.jump.workbench.ui.plugin.FeatureInstaller;

/**
 * Deriving from SaveView plugin from Kosmo SAIG ver. 2
 * "Herramienta que permite salvar la vista actual a un fichero de imagen author S.B. Calvo since 1.0.0"
 * 
 * Modified by Giuseppe Aruta 2013.
 * This plugin allows to save a raster file (loaded into OpenJUP workbench by SextanteRaster plugin)
 * To another type of raster image file
 * Currently supported: PNG, JPG, BMP (via Image I/O), TIF (via common-imaging by Apache)
 * ASC, using a code modified by Stefan Steinger - 2013
 * 
 */
public class SaveImageToRasterPlugIn extends AbstractPlugIn implements ThreadedPlugIn {

    /** Log */
    private final static Logger LOGGER = Logger.getLogger(SaveImageToRasterPlugIn.class);

    /** Plugin name */
    public final static String NAME = I18N.get("org.openjump.core.ui.plugin.layer.pirolraster.SaveRasterImageAsImagePlugIn.Save-Raster-Image-As-Image"); //$NON-NLS-1$


    /** Plugin name */
    public final static String FILE = I18N.get("org.openjump.core.rasterimage.SelectRasterImageFilesPanel.supported-raster-image-formats"); //$NON-NLS-1$

    
    
    /** Plugin icon */
    public final static Icon ICON = IconLoader.icon("disk.png");
   
    private File file;// archivo en el que se va a guardar la imagen

    public final static String MESSAGE = "Current file format is not supported. Do you want to save as tif file format?";
    
    
    
    // Extensiones existentes
    private HashMap extensions;
    public final static String JPEG_EXTENSION = "JPEG"; //$NON-NLS-1$ 
    public final static String BMP_EXTENSION = "BMP"; //$NON-NLS-1$ 
    public final static String TIF_EXTENSION = "TIFF"; //$NON-NLS-1$ 
    public final static String PNG_EXTENSION = "PNG"; //$NON-NLS-1$ 
    public final static String ASC_EXTENSION = "ASC";
    public final static String FLT_EXTENSION = "FLT";
    public final static String GIF_EXTENSION = "GIF";
    public final static String JP2_EXTENSION = "JP2";
    
    protected double[][] data;
    private Properties properties = null;
    private static String propertiesFile = LoadSextanteRasterImagePlugIn.getPropertiesFile();
    NumberFormat cellFormat = null;
    public static final Double DEFAULT_NODATA = -9999.00;
    public double defaultNoData = -9999;

   
    
    
    
    /**
     * 
     */
    public SaveImageToRasterPlugIn() {
        extensions = new HashMap();
        extensions.put("JPG", JPEG_EXTENSION); //$NON-NLS-1$ 
        extensions.put("JPEG", JPEG_EXTENSION); //$NON-NLS-1$
        extensions.put("BMP", BMP_EXTENSION); //$NON-NLS-1$ 
        extensions.put("TIF", TIF_EXTENSION); //$NON-NLS-1$
        extensions.put("TIFF", TIF_EXTENSION); //$NON-NLS-1$
        extensions.put("PNG", PNG_EXTENSION); //$NON-NLS-1$
        extensions.put("ASC", ASC_EXTENSION); //$NON-NLS-1$
        extensions.put("FLT", FLT_EXTENSION); //$NON-NLS-1$
        extensions.put("JP2", JP2_EXTENSION); //$NON-NLS-1$
    }

    @Override
    public String getName() {
        return NAME;
    }

    public Icon getIcon() {
        return ICON;
    }

    
    public void initialize(PlugInContext context)
      throws Exception
    {
      WorkbenchContext workbenchContext = context.getWorkbenchContext();
      new FeatureInstaller(workbenchContext);

      context.getFeatureInstaller().addMainMenuItem(
        this, 
        new String[] { MenuNames.RASTER}, 
       getName() , 
        false, 
        null, 
        createEnableCheck(context.getWorkbenchContext()));
    }
    
    
    
    public boolean execute( PlugInContext context ) throws Exception {
        reportNothingToUndoYet(context);
        // Preguntamos en donde queremos guardar el jpg
        JFileChooser fileChooser = GUIUtil.createJFileChooserWithOverwritePrompting();
        fileChooser
                .setFileFilter(GUIUtil
                        .createFileFilter(
                                 FILE , new String[]{"jpg",   "tif", "png", "bmp", "asc"})); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
        fileChooser.setDialogTitle(NAME); //$NON-NLS-1$
        int option = fileChooser.showSaveDialog(context.getWorkbenchFrame());
        if (JFileChooser.APPROVE_OPTION == option) {
            file = fileChooser.getSelectedFile();
            return true;
        } else {
            return false;
        }
    }

    
 
    
    
    
    public void run( TaskMonitor monitor, PlugInContext context ) throws Exception {

        file = FileUtil.addExtensionIfNone(file, "tif"); //$NON-NLS-1$
        String extension = FileUtil.getExtension(file);

        try {
            // The file is created according to the extension (tif, jpg..)
          String trueExtension = (String) extensions.get(extension.toUpperCase());
          BufferedImage image = null;
          RasterImageLayer rLayer = (RasterImageLayer) LayerTools.getSelectedLayerable(context, RasterImageLayer.class); 
          Raster r = rLayer.getRasterData();
          SampleModel sm = r.getSampleModel();
    	  ColorModel colorModel = PlanarImage.createColorModel(sm);
  		  image = new BufferedImage(colorModel, (WritableRaster) rLayer.getRasterData(), false, null);
	  

        
            
            /**
             * Save to PNG
             */
            if (trueExtension.equalsIgnoreCase(PNG_EXTENSION)) {
                OutputStream out = null;
                try {
                    out = new FileOutputStream(file);
               
                   ImageUtils.writeBufferedImageAsPNG2(out, image);
                    LOGGER.debug( this.getClass());
                } finally {
                    if (out != null) {
                        out.close();
                    }
                }
             
             
             /**
              * Save to JPG converting Buffered Image to RGB
              */
             } else if( trueExtension.equalsIgnoreCase(JPEG_EXTENSION)){
                OutputStream out = null;
                try {
                	
                    out = new FileOutputStream(file);
                    ImageUtils.writeBufferedImageAsJPEG(out, 1.0f, image);
                    LOGGER.debug( this.getClass());
                } finally {
                    if (out != null) {
                        out.close();
                    }
                }
          
                
             /**
              * Save to JPEG2000 actually not working, 
              * an attempt to use jai-imageio-core that allows to write to JP2
              * https://github.com/stain/jai-imageio-core
              */ 
            } else if( trueExtension.equalsIgnoreCase(JP2_EXTENSION)){
                OutputStream out = null;
                try {
                    out = new FileOutputStream(file);
                    ImageUtils.writeBufferedImageAsJPEG2000(out, image);
                    LOGGER.debug( this.getClass());
                } finally {
                    if (out != null) {
                        out.close();
                    }
                } 
                
           /**
            * Save to BMP
            */
            } else if( trueExtension.equalsIgnoreCase(BMP_EXTENSION)){
                OutputStream out = null;
                try {
                    out = new FileOutputStream(file);
                    ImageUtils.writeBufferedImageAsBMP(out,image);
                    LOGGER.debug( this.getClass());
                } finally {
                    if (out != null) {
                        out.close();
                    }
                }
                
             /**
              * Save to ASC, only the 1st band
              */
             } else if( trueExtension.equalsIgnoreCase(ASC_EXTENSION)){
            	
            	 LOGGER.debug( this.getClass());
            	 OutputStream out = null;
            	  try {
                     out = new FileOutputStream(file);
            	 cellFormat = NumberFormat.getNumberInstance();
        		cellFormat.setMaximumFractionDigits(3);
        		cellFormat.setMinimumFractionDigits(0); 
        		
        		 this.properties = new Properties();
        		
        		try {
        			FileInputStream fis = new FileInputStream(propertiesFile);
        			this.properties.load(fis);
        			this.properties.getProperty(LoadSextanteRasterImagePlugIn.KEY_PATH);
        			fis.close();
        		}
        		catch (FileNotFoundException e) {
        			//not sure if it is necessary to show this warning, because the idea is to rather store when there has been no file before.
        			//context.getWorkbenchFrame().warnUser(I18N.get("org.openjump.core.ui.plugin.layer.pirolraster.SaveRasterImageAsImagePlugIn.File-not-found"));
        		}
        		catch (IOException e) {
        			context.getWorkbenchFrame().warnUser(GenericNames.ERROR);
        		}
                    OpenJUMPSextanteRasterLayer rstLayer = new OpenJUMPSextanteRasterLayer();
        			rstLayer.create(rLayer);
        			defaultNoData = rstLayer.getNoDataValue(); //added this
                    PrintStream o = new PrintStream(out);
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
        			} 
                 catch (Exception e) {
                	context.getWorkbenchFrame().warnUser(I18N.get("org.openjump.core.ui.plugin.mousemenu.SaveDatasetsPlugIn.Error-See-Output-Window"));
                 	context.getWorkbenchFrame().getOutputFrame().createNewDocument();
                    context.getWorkbenchFrame().getOutputFrame().addText("SaveImageToRasterPlugIn Exception:" +  "Export Part of FLT/ASC raster to ASC not yet implemented. Please Use Sextante Plugin");
                 	return;
                 }
                finally {
                        if (out != null) {
                            out.close();
                        }
                 }
                
               /**
                 * Save to TIF, using common-imaging library  from Apache
                 */
                 } else {
                	 OutputStream out = null;
                     try {
                         out = new FileOutputStream(file);
                         ImageUtils.writeBufferedImageAsTIF(out,image);
                     }finally {
                          if (out != null) {
                              out.close();
                          }
                      }
                     
                     
               
                 	 
            	
            	/*
            	 // This mathod uses JAI to export Image to  TIF - Not working with
            	 // OpenJDK
            	  
                ParameterBlock pb = new ParameterBlock();
                pb.addSource(image);
                pb.add(file.getAbsolutePath());
                pb.add(trueExtension);
                RenderedOp op = JAI.create("filestore", pb); //$NON-NLS-1$
                op.dispose();*/
            }
            
            
            /**
             * Export World File depending to the extension
             */
           // RasterImageLayer rLayer2 = (RasterImageLayer) LayerTools.getSelectedLayerable(context, RasterImageLayer.class); 
            //Envelope envelope = rLayer2.getEnvelope();
            
           
            Envelope envelope = rLayer.getEnvelope();
            WorldFileHandler worldFileHandler = new WorldFileHandler(file.getAbsolutePath(), false);
			worldFileHandler.writeWorldFile(envelope, image.getWidth(), image.getHeight());
           
			 if (rLayer.getImageFileName() == null){ 
				 /**
				  * If the in_image has no datasource, it loads the out_image to the view
				  */
				
				 rLayer.setImageFileName(file.getAbsolutePath()); 
				 rLayer.setNeedToKeepImage(false);
				 
				 }
			 else{ 
				 //rLayer.setNeedToKeepImage(false);
				 }
			 
	       
   
        } catch (Exception e) {
        	 
        	context.getWorkbenchFrame().warnUser(I18N.get("org.openjump.core.ui.plugin.mousemenu.SaveDatasetsPlugIn.Error-See-Output-Window"));
        	context.getWorkbenchFrame().getOutputFrame().createNewDocument();
            context.getWorkbenchFrame().getOutputFrame().addText("SaveImageToRasterPlugIn Exception:" + new Object[] {e.toString()});
        	
                 
            return;
        }

       
        String NAME_FILE = file.getName();
        
        JOptionPane.showMessageDialog(null,
        		"file '"+NAME_FILE+"' has been successfully created", NAME, JOptionPane.INFORMATION_MESSAGE);
        
        
        
        
           //context.getWorkbenchFrame().setStatusMessage(FILE_CREATED+NAME_FILE);
      

    }
    
   
    
    public static MultiEnableCheck createEnableCheck(WorkbenchContext workbenchContext)
    {
      EnableCheckFactory checkFactory = new EnableCheckFactory(
        workbenchContext);
      MultiEnableCheck multiEnableCheck = new MultiEnableCheck();

      multiEnableCheck.add(checkFactory.createExactlyNLayerablesMustBeSelectedCheck(1, RasterImageLayer.class));

      return multiEnableCheck;
    }

}