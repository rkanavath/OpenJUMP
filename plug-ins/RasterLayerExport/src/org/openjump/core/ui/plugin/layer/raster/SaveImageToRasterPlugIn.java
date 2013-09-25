package org.openjump.core.ui.plugin.layer.raster;

import java.awt.geom.Point2D;
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
import javax.imageio.ImageIO;
import javax.media.jai.PlanarImage;
import javax.swing.Icon;
import javax.swing.JFileChooser;

import org.apache.commons.imaging.ImageWriteException;
import org.apache.log4j.Logger;
import org.openjump.core.apitools.LayerTools;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.rasterimage.WorldFileHandler;
import org.openjump.core.rasterimage.sextante.OpenJUMPSextanteRasterLayer;
import org.openjump.core.rasterimage.sextante.rasterWrappers.GridWrapperNotInterpolated;
import org.openjump.core.ui.plugin.layer.pirolraster.LoadSextanteRasterImagePlugIn;
import com.sun.media.jai.codec.TIFFEncodeParam;
import com.sun.media.jai.codecimpl.TIFFCodec;
import com.sun.media.jai.codecimpl.TIFFImageEncoder;
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
 * 
 * Modified by Giuseppe Aruta 2013. This plugin allows to save a raster file
 * (loaded into OpenJUP workbench by SextanteRaster plugin) To another type of
 * raster image file Currently supported: PNG, JPG, BMP (via Image I/O), TIF
 * (both JAI and common-imaging by Apache) ASC (Esri Ascii grid), GRD (Golden
 * Soft. Surfer Ascii grid ver. 6) and XYZ text file save to ASC using a code
 * modified by Stefan Steinger - 2013
 */
public class SaveImageToRasterPlugIn extends AbstractPlugIn implements
ThreadedPlugIn {

	/** Log */
	private final static Logger LOGGER = Logger
			.getLogger(SaveImageToRasterPlugIn.class);

	/** Plugin name */
	public final static String PLUGINNAME = I18N
			.get("org.openjump.core.ui.plugin.layer.pirolraster.SaveRasterImageAsImagePlugIn.Save-Raster-Image-As-Image"); //$NON-NLS-1$

	/** Plugin name */
	public final static String FILE = I18N
			.get("org.openjump.core.rasterimage.SelectRasterImageFilesPanel.supported-raster-image-formats"); //$NON-NLS-1$

	/** Plugin icon */
	public final static Icon ICON = IconLoader.icon("disk.png");

	private File file;// archivo en el que se va a guardar la imagen
	private File fileINFO;// INFO for grid files
	private File filehdr;// header file for .flt

	public final static String MESSAGE = "Current file format is not supported. Do you want to save as tif file format?";

	// Extensiones existentes
	private HashMap extensions;
	public final static String JPEG_EXTENSION = "JPEG"; //$NON-NLS-1$ 
	public final static String BMP_EXTENSION = "BMP"; //$NON-NLS-1$ 
	public final static String TIF_EXTENSION = "TIF"; //$NON-NLS-1$ 
	public final static String TIFF_EXTENSION = "TIFF";
	public final static String PNG_EXTENSION = "PNG"; //$NON-NLS-1$ 
	public final static String ASC_EXTENSION = "ASC";
	public final static String FLT_EXTENSION = "FLT";
	public final static String GIF_EXTENSION = "GIF";
	public final static String JP2_EXTENSION = "JP2";
	public final static String GRD_EXTENSION = "GRD";
	public final static String TXT_EXTENSION = "TXT";
	public final static String XYZ_EXTENSION = "XYZ";

	protected double[][] data;
	private Properties properties = null;
	private static String propertiesFile = LoadSextanteRasterImagePlugIn
			.getPropertiesFile();
	NumberFormat cellFormat = null;
	public static final Double DEFAULT_NODATA = -9999.00;
	public double defaultNoData = -9999;
	public double surferNoData = 1.70141E+38;
	private String layerName;

	private final static String INFO = I18N
			.get("org.openjump.core.ui.plugin.layer.LayerPropertiesPlugIn.Info"); // information
	private final static String LAYER_NAME = I18N
			.get("org.openjump.core.ui.plugin.layer.LayerPropertiesPlugIn.Layer-Name");
	private final static String DATASOURCE_CLASS = I18N
			.get("org.openjump.core.ui.plugin.layer.LayerPropertiesPlugIn.DataSource-Class"); // class
	// name
	private final static String EXTENT = I18N
			.get("org.openjump.core.ui.plugin.layer.LayerPropertiesPlugIn.extent");
	private final static String XMIN = I18N
			.get("org.openjump.core.ui.plugin.layer.LayerPropertiesPlugIn.xmin");
	private final static String YMIN = I18N
			.get("org.openjump.core.ui.plugin.layer.LayerPropertiesPlugIn.ymin");
	private final static String XMAX = I18N
			.get("org.openjump.core.ui.plugin.layer.LayerPropertiesPlugIn.xmax");
	private final static String YMAX = I18N
			.get("org.openjump.core.ui.plugin.layer.LayerPropertiesPlugIn.ymax");

	private final static String BANDS = I18N
			.get("org.openjump.core.ui.plugin.raster.RasterImageLayerPropertiesPlugIn.file.bands");
	private final static String RASTER_SIZE = I18N
			.get("org.openjump.core.ui.plugin.raster.RasterImageLayerPropertiesPlugIn.file.size");
	private final static String CELL_SIZE = I18N
			.get("org.openjump.core.ui.plugin.raster.RasterImageLayerPropertiesPlugIn.cell.size");
	private final static String CELL_VALUES = I18N
			.get("org.openjump.core.ui.plugin.raster.RasterImageLayerPropertiesPlugIn.cell.values");
	private final static String MAX = I18N
			.get("org.openjump.core.ui.plugin.raster.RasterImageLayerPropertiesPlugIn.cell.max");
	private final static String MIN = I18N
			.get("org.openjump.core.ui.plugin.raster.RasterImageLayerPropertiesPlugIn.cell.min");
	private final static String MEAN = I18N
			.get("org.openjump.core.ui.plugin.raster.RasterImageLayerPropertiesPlugIn.cell.mean");
	private final static String NODATA = I18N
			.get("org.openjump.core.ui.plugin.raster.RasterImageLayerPropertiesPlugIn.cell.nodata");

	/**
	 * 
	 */
	public SaveImageToRasterPlugIn() {
		extensions = new HashMap();
		extensions.put("JPG", JPEG_EXTENSION); //$NON-NLS-1$ 
		extensions.put("JPEG", JPEG_EXTENSION); //$NON-NLS-1$
		extensions.put("BMP", BMP_EXTENSION); //$NON-NLS-1$ 
		extensions.put("TIF", TIF_EXTENSION); //$NON-NLS-1$
		extensions.put("TIFF", TIFF_EXTENSION); //$NON-NLS-1$
		extensions.put("PNG", PNG_EXTENSION); //$NON-NLS-1$
		extensions.put("ASC", ASC_EXTENSION); //$NON-NLS-1$
		extensions.put("FLT", FLT_EXTENSION); //$NON-NLS-1$
		extensions.put("JP2", JP2_EXTENSION); //$NON-NLS-1$
		extensions.put("GRD", GRD_EXTENSION); //$NON-NLS-1$
		extensions.put("TXT", TXT_EXTENSION); //$NON-NLS-1$
		extensions.put("XYZ", XYZ_EXTENSION); //$NON-NLS-1$
	}

	@Override
	public String getName() {
		return PLUGINNAME;
	}

	public Icon getIcon() {
		return ICON;
	}

	public void initialize(PlugInContext context) throws Exception {
		WorkbenchContext workbenchContext = context.getWorkbenchContext();
		new FeatureInstaller(workbenchContext);

		context.getFeatureInstaller().addMainMenuItem(this,
				new String[] { MenuNames.RASTER, MenuNames.TOOLS }, getName(),
				false, null, createEnableCheck(context.getWorkbenchContext()));
	}

	public boolean execute(PlugInContext context) throws Exception {
		reportNothingToUndoYet(context);

		JFileChooser fileChooser = GUIUtil
				.createJFileChooserWithOverwritePrompting();

		fileChooser.setFileFilter(GUIUtil.createFileFilter(FILE, new String[] {
				"asc", "grd", "xyz", "bmp", "jpg", "jp2", "png", "tif" })); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$

		fileChooser.setDialogTitle(PLUGINNAME); //$NON-NLS-1$
		int option = fileChooser.showSaveDialog(context.getWorkbenchFrame());
		if (JFileChooser.APPROVE_OPTION == option) {
			file = fileChooser.getSelectedFile();
			fileINFO = fileChooser.getSelectedFile();
			return true;
		} else {
			return false;
		}
	}

	public void run(TaskMonitor monitor, PlugInContext context)
			throws Exception {

		file = FileUtil.addExtensionIfNone(file, "tif"); //$NON-NLS-1$
		String extension = FileUtil.getExtension(file);
		fileINFO = FileUtil.removeExtensionIfAny(file);
		fileINFO = FileUtil.addExtensionIfNone(fileINFO, "info");
		
		try {
			// The file is created according to the extension (tif, jpg..)

			String trueExtension = (String) extensions.get(extension
					.toUpperCase());
			BufferedImage image = null;
			RasterImageLayer rLayer = (RasterImageLayer) LayerTools
					.getSelectedLayerable(context, RasterImageLayer.class);
			Raster r = rLayer.getRasterData();
			SampleModel sm = r.getSampleModel();
			ColorModel colorModel = PlanarImage.createColorModel(sm);
			image = new BufferedImage(colorModel,
					(WritableRaster) rLayer.getRasterData(), false, null);

			layerName = rLayer.getName();

			/**
			 * Save to PNG
			 */
			if (trueExtension.equalsIgnoreCase(PNG_EXTENSION)) {
				writePng(image, rLayer);
			}

			/**
			 * Save to JPG converting Buffered Image to RGB
			 */
			else if (trueExtension.equalsIgnoreCase(JPEG_EXTENSION)) {
				writeJpg(image, rLayer);
			}
			
			/**
			 * Save to JPEG2000, an attempt to use jai-imageio-core that
			 * allows to write to JP2
			 * https://github.com/stain/jai-imageio-core
			 */
			else if (trueExtension.equalsIgnoreCase(JP2_EXTENSION)) {
				writeJp2(image, rLayer);
			}
			
			/**
			 * Save to BMP
			 */
			else if (trueExtension.equalsIgnoreCase(BMP_EXTENSION)) {
				writeBmp(image, rLayer);
			}
			
			/**
			 * Save to GIF
			 */
			else if (trueExtension.equalsIgnoreCase(GIF_EXTENSION)) {
				writeGif(image, rLayer);
			}
			
			/**
			 * Save to ASC, only the 1st band
			 */
			else if (trueExtension.equalsIgnoreCase(ASC_EXTENSION)) {
				saveAsc(context, rLayer);
			} 
			
			/**
			 * Save to XYZ, only the 1st band
			 */
			else if (trueExtension.equalsIgnoreCase(XYZ_EXTENSION)) {
				saveXyz(context, rLayer);
			}
			
			/**
			 * TODO - Save to FLT, only the 1st band
			 */

			/**
			 * TODO - Save to TXT, only the 1st band
			 */

			/**
			* Save to GRD Golder Surfer ASCII file , only the 1st band
			*/
			else if (trueExtension.equalsIgnoreCase(GRD_EXTENSION)) {
				saveGrd(context, rLayer);
			}
						
			/**
			 * Save to TIF or TIFF If the raster layer is a mono band file,
			 * it uses Oracle JAI library, to preserve GRID datas Else if
			 * the raster layer is NOT a mono band, it uses Apache
			 * commons-imaging library, using common-imaging library from
			 * Apache
			 */
			else {
				saveTiff(image, rLayer, colorModel);

			}

			if (rLayer.getImageFileName() == null) {

				/**
				 * If the in_image has no datasource, it loads the out_image to
				 * the view
				 */

				rLayer.setImageFileName(file.getAbsolutePath());
				rLayer.setNeedToKeepImage(false);

			} else {
				// rLayer.setNeedToKeepImage(false);
			}

		} catch (Exception e) {

			context.getWorkbenchFrame()
			.warnUser(
					I18N.get("org.openjump.core.ui.plugin.mousemenu.SaveDatasetsPlugIn.Error-See-Output-Window"));
			context.getWorkbenchFrame().getOutputFrame().createNewDocument();
			context.getWorkbenchFrame()
			.getOutputFrame()
			.addText(
					"SaveImageToRasterPlugIn Exception:"
							+ new Object[] { e.toString() });

			return;
		}

		String NAME_FILE = file.getName();
		context.getWorkbenchFrame().setStatusMessage(
				"file '" + NAME_FILE + "' has been successfully created");

	}

	private void saveTiff(BufferedImage image, RasterImageLayer rLayer,
			ColorModel colorModel) throws IOException {
		OutputStream out = null;
		try {
			out = new FileOutputStream(file);
			OpenJUMPSextanteRasterLayer rstLayer = new OpenJUMPSextanteRasterLayer();
			rstLayer.create(rLayer);

			// / If original file is monoband it is saved using JAI
			int nrbands = rstLayer.getBandsCount(); //[sstein] I think this is not working correctly
			//int bands2 = rLayer.getRasterData().getNumBands(); //this neither
			
			if (nrbands == 1) {
			//[sstein: 25.Sept.2013] TODO: The JAI code below does not seem to work
			/*
				image = new BufferedImage(colorModel,
						(WritableRaster) rLayer.getRasterData(), false,
						null);
				TIFFEncodeParam param = new TIFFEncodeParam();
				param.setCompression(TIFFEncodeParam.COMPRESSION_NONE);
				TIFFImageEncoder encoder = (TIFFImageEncoder) TIFFCodec
						.createImageEncoder("tif", out, param);
				encoder.encode(image);

				Envelope envelope = rLayer.getEnvelope();
				WorldFileHandler worldFileHandler = new WorldFileHandler(
						file.getAbsolutePath(), false);
				worldFileHandler.writeWorldFile(envelope,
						image.getWidth(), image.getHeight());

			} 
			else {
			*/
				//[sstein: 25.Sept.2013] but this code seems to work, although it is slows + high mem consumption
				// ... and the image is in b-w/grey only (similar to png) 
 				// use Apache commons-imaging
				ImageUtils.writeBufferedImageAsTIF(out, image);
				Envelope envelope = rLayer.getEnvelope();
				WorldFileHandler worldFileHandler = new WorldFileHandler(
						file.getAbsolutePath(), false);
				worldFileHandler.writeWorldFile(envelope,
						image.getWidth(), image.getHeight());
			}
		}
		catch(Exception e){
			e.printStackTrace();
		}
		finally {
			if (out != null) {
				out.close();
			}
		}
	}

	private void saveInfoFile(RasterImageLayer rLayer, OpenJUMPSextanteRasterLayer rstLayer) throws IOException {
		OutputStream out1 = null;

		try {

			out1 = new FileOutputStream(fileINFO);
			PrintStream c = new PrintStream(out1);
			c.println(LAYER_NAME + ": " + layerName + "\n");
			c.println(DATASOURCE_CLASS + ": " + file + "\n");
			c.println(RASTER_SIZE + ": " + rstLayer.getLayerGridExtent().getNX() + " x "+ rstLayer.getLayerGridExtent().getNY() + "\n");
			c.println(EXTENT + ":");
			c.println(XMIN + ": " + rLayer .getEnvelope().getMinX());
			c.println(YMIN + ": " + rLayer.getEnvelope().getMinY());
			c.println(XMAX + ": " + rLayer.getEnvelope().getMaxX());
			c.println(YMAX + ": " + rLayer.getEnvelope().getMaxY() + "\n");
			c.println(BANDS + ": " + rstLayer.getBandsCount() + "\n");
			c.println(CELL_SIZE + ": " + rstLayer.getLayerCellSize()+ "\n");
			c.println(CELL_VALUES + ":");
			c.println(MAX + ": " + rstLayer.getMaxValue());
			c.println(MIN + ": " + rstLayer.getMinValue());
			c.println(MEAN + ": " + rstLayer.getMeanValue());
			c.println(NODATA + ": "+ rstLayer.getNoDataValue());
			// //
			c.close();
		}
		catch(Exception e){
			e.printStackTrace();
		}
		finally {
			if (out1 != null) {
				out1.close();
			}
		}
	}

	private void saveGrd(PlugInContext context, RasterImageLayer rLayer)
			throws IOException {
		OutputStream out = null;

		try {
			OpenJUMPSextanteRasterLayer rstLayer = new OpenJUMPSextanteRasterLayer();
			rstLayer.create(rLayer);
			LOGGER.debug(this.getClass());
			out = new FileOutputStream(file);
			cellFormat = NumberFormat.getNumberInstance();
			cellFormat.setMaximumFractionDigits(3);
			cellFormat.setMinimumFractionDigits(0);
			this.properties = new Properties();
			try {
				FileInputStream fis = new FileInputStream(
						propertiesFile);
				this.properties.load(fis);
				this.properties
				.getProperty(LoadSextanteRasterImagePlugIn.KEY_PATH);
				fis.close();
			} catch (FileNotFoundException e) {
				// not sure if it is necessary to show this warning,
				// because the idea is to rather store when there has
				// been no file before.
				// context.getWorkbenchFrame().warnUser(I18N.get("org.openjump.core.ui.plugin.layer.pirolraster.SaveRasterImageAsImagePlugIn.File-not-found"));
			} catch (IOException e) {
				context.getWorkbenchFrame()
				.warnUser(GenericNames.ERROR);
			}
			defaultNoData = rstLayer.getNoDataValue();

			// The XY min max are adjusted by half the cell size because
			// ESRI grids are based on the center of the grid cell
			// Surfer grids are based on grid lines which equate to the
			// edges of a cell.

			Double xcMin = rLayer.getEnvelope().getMinX()
					+ (0.5 * rstLayer.getLayerCellSize());
			Double ycMin = rLayer.getEnvelope().getMinY()
					+ (0.5 * rstLayer.getLayerCellSize());
			Double xcMax = rLayer.getEnvelope().getMaxX()
					- (0.5 * rstLayer.getLayerCellSize());
			Double ycMax = rLayer.getEnvelope().getMaxY()
					- (0.5 * rstLayer.getLayerCellSize());

			// Write Header

			PrintStream po = new PrintStream(out);
			po.println("DSAA");
			po.println(rLayer.getOrigImageWidth() + " "
					+ rLayer.getOrigImageHeight());
			po.println(xcMin + " " + xcMax);
			po.println(ycMin + " " + ycMax);
			po.println(rstLayer.getMinValue() + " "
					+ rstLayer.getMaxValue());

			GridWrapperNotInterpolated gwrapper = new GridWrapperNotInterpolated(
					rstLayer, rstLayer.getLayerGridExtent());
			int nx = rstLayer.getLayerGridExtent().getNX();
			int ny = rstLayer.getLayerGridExtent().getNY();

			// Start loop for data.
			// Surfer Grid file read first lower line and then the upper
			// ones
			// while ESRI strarts from the upper line and than the lower
			// ones
			for (int y = ny; y >= 0; y--) {// rows
				StringBuffer b = new StringBuffer();
				for (int x = 0; x < nx; x++) {// cols

					double value = gwrapper.getCellValueAsDouble(x, y,
							0); // this was before
					// "getCellValueAsFloa()"... not sure
					// why you would want a float?
					// / To do for Not a number values NaN
					// if( Double.isNaN(value)) value = surferNoData;

					// Try to substitute ESRI (-9999) and SAGA (-99999)
					// NODATA value with Surfer one (1.70141E+38)
					if (value < -9998) {
						b.append("1.70141E+38" + " "); // cast to int
					} else

						// show int as int
						if (Math.floor(value) == value) {
							b.append((int) value + " "); // cast to int
						} else {
							b.append(value + " ");
						}
				}

				// o.println(new StringBuffer(b).reverse().toString());
				po.println(b);
			}
			po.close();
			saveInfoFile(rLayer, rstLayer);
		} catch (Exception e) {
			context.getWorkbenchFrame()
			.warnUser(
					I18N.get("org.openjump.core.ui.plugin.mousemenu.SaveDatasetsPlugIn.Error-See-Output-Window"));
			context.getWorkbenchFrame().getOutputFrame()
			.createNewDocument();
			context.getWorkbenchFrame()
			.getOutputFrame()
			.addText(
					"SaveImageToRasterPlugIn Exception:"
							+ "Export Part of FLT/ASC raster to ASC not yet implemented. Please Use Sextante Plugin");
		} finally {
			if (out != null) {
				out.close();
			}
		}
	}

	private void saveXyz(PlugInContext context, RasterImageLayer rLayer)
			throws IOException {
		OutputStream out = null;

		try {
			OpenJUMPSextanteRasterLayer rstLayer = new OpenJUMPSextanteRasterLayer();
			rstLayer.create(rLayer);
			LOGGER.debug(this.getClass());
			out = new FileOutputStream(file);
			cellFormat = NumberFormat.getNumberInstance();
			cellFormat.setMaximumFractionDigits(3);
			cellFormat.setMinimumFractionDigits(0);
			this.properties = new Properties();
			try {
				FileInputStream fis = new FileInputStream(
						propertiesFile);
				this.properties.load(fis);
				this.properties
				.getProperty(LoadSextanteRasterImagePlugIn.KEY_PATH);
				fis.close();
			} catch (FileNotFoundException e) {
				// not sure if it is necessary to show this warning,
				// because the idea is to rather store when there has
				// been no file before.
				// context.getWorkbenchFrame().warnUser(I18N.get("org.openjump.core.ui.plugin.layer.pirolraster.SaveRasterImageAsImagePlugIn.File-not-found"));
			} catch (IOException e) {
				context.getWorkbenchFrame()
				.warnUser(GenericNames.ERROR);
			}
			// defaultNoData = rstLayer.getNoDataValue(); //added this
			PrintStream po = new PrintStream(out);
			po.println("x" + "	" + "y" + "	" + "z");
			GridWrapperNotInterpolated gwrapper = new GridWrapperNotInterpolated(
					rstLayer, rstLayer.getLayerGridExtent());
			int nx = rstLayer.getLayerGridExtent().getNX();
			int ny = rstLayer.getLayerGridExtent().getNY();
			for (int y = 0; y < ny; y++) {// rows
				StringBuffer b = new StringBuffer();
				for (int x = 0; x < nx; x++) {// cols
					double value = gwrapper.getCellValueAsDouble(x, y,
							0); // this was before
					// "getCellValueAsFloa()"... not sure
					// why you would want a float?
					Point2D pt = rstLayer.getLayerGridExtent()
							.getWorldCoordsFromGridCoords(x, y);
					if (Double.isNaN(value))
						value = defaultNoData;
					// show int as int
					if (Math.floor(value) == value) {
						b.append((int) pt.getX() + "	"
								+ (int) pt.getY() + "	" + (int) value
								+ "\n"); // cast to int
					} else {
						b.append(pt.getX() + "	" + pt.getY() + "	"
								+ value + "\n");
					}
				}
				po.println(b);
			}
			po.close();
			saveInfoFile(rLayer, rstLayer);
		} catch (Exception e) {
			context.getWorkbenchFrame()
			.warnUser(
					I18N.get("org.openjump.core.ui.plugin.mousemenu.SaveDatasetsPlugIn.Error-See-Output-Window"));
			context.getWorkbenchFrame().getOutputFrame()
			.createNewDocument();
			context.getWorkbenchFrame()
			.getOutputFrame()
			.addText(
					"SaveImageToRasterPlugIn Exception:"
							+ "Export Part of FLT/ASC raster to ASC not yet implemented. Please Use Sextante Plugin");
		} finally {
			if (out != null) {
				out.close();
			}
		}
	}

	private void saveAsc(PlugInContext context, RasterImageLayer rLayer)
			throws IOException {
		OutputStream out = null;

		try {
			OpenJUMPSextanteRasterLayer rstLayer = new OpenJUMPSextanteRasterLayer();
			rstLayer.create(rLayer);
			LOGGER.debug(this.getClass());
			out = new FileOutputStream(file);
			cellFormat = NumberFormat.getNumberInstance();
			cellFormat.setMaximumFractionDigits(3);
			cellFormat.setMinimumFractionDigits(0);
			this.properties = new Properties();
			try {
				FileInputStream fis = new FileInputStream(
						propertiesFile);
				this.properties.load(fis);
				this.properties
				.getProperty(LoadSextanteRasterImagePlugIn.KEY_PATH);
				fis.close();
			} catch (FileNotFoundException e) {
				// not sure if it is necessary to show this warning,
				// because the idea is to rather store when there has
				// been no file before.
				// context.getWorkbenchFrame().warnUser(I18N.get("org.openjump.core.ui.plugin.layer.pirolraster.SaveRasterImageAsImagePlugIn.File-not-found"));
			} catch (IOException e) {
				context.getWorkbenchFrame()
				.warnUser(GenericNames.ERROR);
			}
			// defaultNoData = rstLayer.getNoDataValue(); //added this
			PrintStream o = new PrintStream(out);
			o.println("ncols " + rLayer.getOrigImageWidth());// Number
			// of
			// columns
			o.println("nrows " + rLayer.getOrigImageHeight());// Number
			// of
			// rows
			o.println("xllcorner " + rLayer.getEnvelope().getMinX());// the
			// x
			// coordinate
			// of
			// the
			// left
			// edge
			// of
			// the
			// lower
			// left
			// grid
			// cell
			o.println("yllcorner " + rLayer.getEnvelope().getMinY());// the
			// Y
			// coordinate
			// of
			// the
			// left
			// edge
			// of
			// the
			// lower
			// left
			// grid
			// cell
			o.println("cellsize " + rstLayer.getLayerCellSize());// Cell
			// Size
			String sNoDataVal = "";
			if (Math.floor(defaultNoData) == defaultNoData) {
				sNoDataVal = Integer.toString((int) defaultNoData);
			} else {
				sNoDataVal = Double.toString(defaultNoData);
			}
			o.println("NODATA_value " + sNoDataVal);
			GridWrapperNotInterpolated gwrapper = new GridWrapperNotInterpolated(
					rstLayer, rstLayer.getLayerGridExtent());
			int nx = rstLayer.getLayerGridExtent().getNX();
			int ny = rstLayer.getLayerGridExtent().getNY();
			for (int y = 0; y < ny; y++) {// rows
				StringBuffer b = new StringBuffer();
				for (int x = 0; x < nx; x++) {// cols
					double value = gwrapper.getCellValueAsDouble(x, y,
							0); // this was before
					// "getCellValueAsFloa()"... not sure
					// why you would want a float?
					if (Double.isNaN(value))
						value = defaultNoData;
					// show int as int
					if (Math.floor(value) == value) {
						b.append((int) value + " "); // cast to int
					} else {
						b.append(value + " ");
					}
				}
				o.println(b);
			}
			o.close();
			saveInfoFile(rLayer, rstLayer);
		} catch (Exception e) {
			context.getWorkbenchFrame()
			.warnUser(
					I18N.get("org.openjump.core.ui.plugin.mousemenu.SaveDatasetsPlugIn.Error-See-Output-Window"));
			context.getWorkbenchFrame().getOutputFrame()
			.createNewDocument();
			context.getWorkbenchFrame()
			.getOutputFrame()
			.addText(
					"SaveImageToRasterPlugIn Exception:"
							+ "Export Part of FLT/ASC raster to ASC not yet implemented. Please Use Sextante Plugin");
		} finally {
			if (out != null) {
				out.close();
			}
		}
	}

	private void writeGif(BufferedImage image, RasterImageLayer rLayer)
			throws FileNotFoundException, IOException, ImageWriteException {
		OutputStream out = null;
		try {
			out = new FileOutputStream(file);
			File inputFile = new File(rLayer.getName());
			BufferedImage input = ImageIO.read(inputFile);
			ImageUtils.writeBufferedImageAsGIF(out, input);
			Envelope envelope = rLayer.getEnvelope();
			WorldFileHandler worldFileHandler = new WorldFileHandler(
					file.getAbsolutePath(), false);
			worldFileHandler.writeWorldFile(envelope, image.getWidth(),
					image.getHeight());
			LOGGER.debug(this.getClass());
		}
		catch(Exception e){
			e.printStackTrace();
		}
		finally {
			if (out != null) {
				out.close();
			}
		}
	}

	private void writeBmp(BufferedImage image, RasterImageLayer rLayer)
			throws FileNotFoundException, IOException, ImageWriteException {
		OutputStream out = null;
		try {
			out = new FileOutputStream(file);
			ImageUtils.writeBufferedImageAsBMP(out, image);
			Envelope envelope = rLayer.getEnvelope();
			WorldFileHandler worldFileHandler = new WorldFileHandler(
					file.getAbsolutePath(), false);
			worldFileHandler.writeWorldFile(envelope, image.getWidth(),
					image.getHeight());
			LOGGER.debug(this.getClass());
		} finally {
			if (out != null) {
				out.close();
			}
		}
	}

	private void writeJp2(BufferedImage image, RasterImageLayer rLayer)
			throws FileNotFoundException, IOException {
		OutputStream out = null;
		try {
			out = new FileOutputStream(file);
			ImageUtils.writeBufferedImageAsJPEG2000(out, image);
			Envelope envelope = rLayer.getEnvelope();
			WorldFileHandler worldFileHandler = new WorldFileHandler(
					file.getAbsolutePath(), false);
			worldFileHandler.writeWorldFile(envelope, image.getWidth(),
					image.getHeight());
			LOGGER.debug(this.getClass());
		} finally {
			if (out != null) {
				out.close();
			}
		}
	}

	private void writeJpg(BufferedImage image, RasterImageLayer rLayer)
			throws FileNotFoundException, IOException, ImageWriteException {
		OutputStream out = null;
		try {

			out = new FileOutputStream(file);
			ImageUtils.writeBufferedImageAsJPEG(out, 1.0f, image);

			Envelope envelope = rLayer.getEnvelope();
			WorldFileHandler worldFileHandler = new WorldFileHandler(
					file.getAbsolutePath(), false);
			worldFileHandler.writeWorldFile(envelope, image.getWidth(),
					image.getHeight());
			LOGGER.debug(this.getClass());
		} finally {
			if (out != null) {
				out.close();
			}
		}
	}

	private void writePng(BufferedImage image, RasterImageLayer rLayer)
			throws FileNotFoundException, IOException {
		OutputStream out = null;
		try {
			out = new FileOutputStream(file);

			ImageUtils.writeBufferedImageAsPNG2(out, image);

			Envelope envelope = rLayer.getEnvelope();
			WorldFileHandler worldFileHandler = new WorldFileHandler(
					file.getAbsolutePath(), false);
			worldFileHandler.writeWorldFile(envelope, image.getWidth(),
					image.getHeight());

			LOGGER.debug(this.getClass());
		} finally {
			if (out != null) {
				out.close();
			}
		}
	}

	public static MultiEnableCheck createEnableCheck(
			WorkbenchContext workbenchContext) {
		EnableCheckFactory checkFactory = new EnableCheckFactory(
				workbenchContext);
		MultiEnableCheck multiEnableCheck = new MultiEnableCheck();

		multiEnableCheck.add(checkFactory
				.createExactlyNLayerablesMustBeSelectedCheck(1,
						RasterImageLayer.class));

		return multiEnableCheck;
	}

}