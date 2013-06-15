/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI 
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * JUMP is Copyright (C) 2003 Vivid Solutions
 *
 * This program implements extensions to JUMP and is
 * Copyright (C) 2004 Integrated Systems Analysts, Inc.
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * 
 * For more information, contact:
 *
 * Integrated Systems Analysts, Inc.
 * 630C Anchors St., Suite 101
 * Fort Walton Beach, Florida
 * USA
 *
 * (850)862-7321
 * www.ashs.isa.com
 */


package com.isa.jump.plugin;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.FontFactory;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.DefaultFontMapper;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfTemplate;
import com.lowagie.text.pdf.PdfWriter;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.datasource.SaveFileDataSourceQueryChooser;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.LayerManager;
import com.vividsolutions.jump.workbench.model.Layerable;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.*;
import com.vividsolutions.jump.workbench.ui.plugin.PersistentBlackboardPlugIn;
import com.vividsolutions.jump.workbench.ui.plugin.scalebar.ScaleBarRenderer;

import javax.print.DocFlavor;
import javax.print.PrintService;
import javax.print.PrintServiceLookup;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.print.attribute.standard.JobName;
import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import java.awt.*;
import java.awt.print.PageFormat;
import java.awt.print.Paper;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * This class installs a File->Print menu option and
 * performs basic vector or raster printing of the current 
 * Layer View Panel or the area within the fence, if present.
 * The vertical extent may be modified to fit the paper size.
 * @author Larry Becker
 */
public class PrintPlugIn extends AbstractPlugIn {

    I18N I18N_ = I18N.getInstance("print");

    public static final double INCH = 72;
    public static final double MILLIMETER = 72.0/25.4;
    public double UNIT = MILLIMETER;
//	public final int HALF_INCH = 36;

    private final String PRINT_MENU            = I18N_.getText("print","PrintPlugIn.Print");
//	private final String PRINT_AS_GROUP = "Force";
//	private final String PRINT_AS_RASTER = "Force printing as a raster";
//	private final String PRINT_AS_VECTORS = "Force printing as vectors (no transparency)";
    private final String REMOVE_TRANSPARENCY   = I18N_.getText("print","PrintPlugIn.Remove-Transparency");
    private final String REMOVE_BASIC_FILLS    = I18N_.getText("print","PrintPlugIn.Remove-basic-fills");
    private final String REMOVE_THEME_FILLS    = I18N_.getText("print","PrintPlugIn.Remove-theme-fills");
    private final String CHANGE_LINE_WIDTH     = I18N_.getText("print","PrintPlugIn.Change-line-width");
    private final String LINE_WIDTH_PERCENT    = I18N_.getText("print","PrintPlugIn.Line-width-percent");
    private final String LINE_WIDTH_TOOLTIP    = I18N_.getText("print","PrintPlugIn.Line-width-tooltip"); // 0-300
    private final String PDF_PAGE_TOOLTIP      = I18N_.getText("print","PrintPlugIn.PDF-page-tooltip");   // MM
    private final String PRINT_BORDER          = I18N_.getText("print","PrintPlugIn.Print-border");
    private final String PRINT_OPTIONS         = I18N_.getText("print","PrintPlugIn.Print-options");
    private final String PRINTER_NOT_FOUND     = I18N_.getText("print","PrintPlugIn.Printer-not-found");
    private final String RESOLUTION_MULTIPLIER = I18N_.getText("print","PrintPlugIn.Resolution-multiplier");
    //	private final String RESOLUTION_MULTIPLIER = "Image Resolution Multiplier";
    private final String EXPAND_TO_FIT         = I18N_.getText("print","PrintPlugIn.Expand-to-fit");
    private final String PRINT_AREA_IN_FENCE   = I18N_.getText("print","PrintPlugIn.Print-area-in-fence");
    private final String PRINT_AREA_IN_BOUNDS  = I18N_.getText("print","PrintPlugIn.Print-area-in-selection-bounds");
    //	private final String RESOLUTION_MULTIPLIER_TOOLTIP = "1 - 4";
    //  private final String OUT_OF_RANGE          = I18N_.getText("print","out of range");
    private final String FINISHED_MESSAGE      = I18N_.getText("print","PrintPlugIn.Finished-message");
    private final String PRINT_TO_PDF          = I18N_.getText("print","PrintPlugIn.Print-to-PDF");
    private final String SAVE_PDF              = I18N_.getText("print","PrintPlugIn.Save-PDF");
    private final String PDF_FILES              = I18N_.getText("print","PrintPlugIn.PDF-files");
    private final String PDF_PAGE_WIDTH        = I18N_.getText("print","PrintPlugIn.PDF-page-width");
    private final String PDF_PAGE_HEIGHT       = I18N_.getText("print","PrintPlugIn.PDF-page-height");

	private boolean printToPDF = false;
	private PlugInContext pluginContext;
	private PrintService printService = null;;
	private LayerViewPanel printPanel = null;
	private int resolutionFactor = 1; //increase resolution by this factor
//	private boolean forceRaster = false;
//	private boolean forceVector = false;
	private boolean removeTransparency = false;
	private boolean removeThemeFills = false;
	private boolean removeBasicFills = false;
	private boolean changeLineWidth = true;
	private double lineWidthPercent = 25.0f;
	private float lineWidthMultiplier = 0.0f;
	private boolean printBorder = false;
	private boolean expandToFit = true;
	private boolean printFenceArea = false;
	private boolean printBoundsArea = true;
	private boolean doubleImageResolution = false;
	private Envelope printEnvelope;
	PrintRequestAttributeSet attributeSet = new HashPrintRequestAttributeSet();
	private ArrayList printLayerables;
	private Envelope windowEnvelope = null;
	private Geometry fence = null;
	private double pdfPageWidth = 210;
	private double pdfPageHeight = 297;


	//TODO: plugin to set page format (PrintRequestAttributeSet)
	//	printerJob.pageDialog(attributeSet);  
	public PrintRequestAttributeSet getPrintRequestAttributeSet() {
		return attributeSet;
	}
	public void setPrintRequestAttributeSet(PrintRequestAttributeSet attributeSet){
		this.attributeSet = attributeSet;
	}
//	/**
//	 * @param forceRaster new value of forceRaster
//	 * @return old value of forceRaster
//	 */
//	public boolean setForceRaster(boolean forceRaster){
//		boolean old = this.forceRaster;
//		this.forceRaster = forceRaster;
//		if (forceRaster)
//			forceVector = false;
//		return old;
//	}
//	/**
//	 * @param forceVector new value of forceVector
//	 * @return old value of forceVector
//	 */
//	public boolean setForceVector(boolean forceVector){
//		boolean old = this.forceVector;
//		this.forceVector = forceVector;
//		if (forceVector)
//			forceRaster = false;
//		return old;
//	}

    public void initialize(PlugInContext context) throws Exception
    {
        context.getFeatureInstaller().addMainMenuPlugin(this,
                new String[]{MenuNames.FILE}, PRINT_MENU, false, null,
                PrintPlugIn.createEnableCheck(context.getWorkbenchContext()));
    }
    
    public boolean execute(final PlugInContext context) throws Exception
	{   
		reportNothingToUndoYet(context);
        MultiInputDialog dialog = new MultiInputDialog(
        		context.getWorkbenchFrame(), PRINT_OPTIONS, true);
    	fence = context.getLayerViewPanel().getFence();
        printFenceArea = (fence != null);
        dialog.addCheckBox(REMOVE_TRANSPARENCY, removeTransparency);
        dialog.addCheckBox(EXPAND_TO_FIT, expandToFit);
        dialog.addCheckBox(PRINT_AREA_IN_FENCE, printFenceArea);
        dialog.getCheckBox(PRINT_AREA_IN_FENCE).setEnabled(
        		context.getLayerViewPanel().getFence() != null);
        
        dialog.addCheckBox(PRINT_AREA_IN_BOUNDS, printFenceArea);
        dialog.getCheckBox(PRINT_AREA_IN_BOUNDS).setEnabled(
        		context.getLayerViewPanel().getSelectionManager()
        		.getSelectedItems().size() == 1);
        dialog.addCheckBox(PRINT_BORDER,printBorder);
        dialog.addCheckBox(REMOVE_BASIC_FILLS, removeBasicFills);
        dialog.addCheckBox(REMOVE_THEME_FILLS, removeThemeFills);
        dialog.addCheckBox(CHANGE_LINE_WIDTH, changeLineWidth);
        dialog.addDoubleField(LINE_WIDTH_PERCENT,lineWidthPercent,4,LINE_WIDTH_TOOLTIP);
//        dialog.addIntegerField(RESOLUTION_MULTIPLIER,
//        		resolutionFactor,4,RESOLUTION_MULTIPLIER_TOOLTIP);
        dialog.addCheckBox(RESOLUTION_MULTIPLIER, doubleImageResolution);
               
//        dialog.addRadioButton(PRINT_AS_RASTER, PRINT_AS_GROUP, forceRaster,"");
//        dialog.addRadioButton(PRINT_AS_VECTORS, PRINT_AS_GROUP, forceVector,"");
        dialog.addCheckBox(PRINT_TO_PDF, printToPDF);
        dialog.addDoubleField(PDF_PAGE_WIDTH,pdfPageWidth,4,PDF_PAGE_TOOLTIP);
        dialog.addDoubleField(PDF_PAGE_HEIGHT,pdfPageHeight,4,PDF_PAGE_TOOLTIP);
        dialog.setVisible(true);
        if (dialog.wasOKPressed()){
           	removeTransparency = dialog.getBoolean(REMOVE_TRANSPARENCY);
//           	setForceVector(removeTransparency);
         	expandToFit = dialog.getBoolean(EXPAND_TO_FIT);
         	printFenceArea = dialog.getBoolean(PRINT_AREA_IN_FENCE);
         	printBoundsArea = dialog.getBoolean(PRINT_AREA_IN_BOUNDS);
           	//if neither of the following overrides are checked,
           	//  the transparency setting will control
//        	if (dialog.getBoolean(PRINT_AS_RASTER))
//        		setForceRaster(true);        	
//           	if (dialog.getBoolean(PRINT_AS_VECTORS))
//           		setForceVector(true);       	    
           	printBorder = dialog.getBoolean(PRINT_BORDER);
           	removeBasicFills = dialog.getBoolean(REMOVE_BASIC_FILLS);
        	removeThemeFills = dialog.getBoolean(REMOVE_THEME_FILLS);
        	changeLineWidth = dialog.getBoolean(CHANGE_LINE_WIDTH);
        	lineWidthPercent = dialog.getDouble(LINE_WIDTH_PERCENT);
        	lineWidthMultiplier = (float) lineWidthPercent / 100f;
        	doubleImageResolution = dialog.getBoolean(RESOLUTION_MULTIPLIER);
        	if (doubleImageResolution)
        		resolutionFactor = 2;
        	else
        		resolutionFactor = 1;
//         	resolutionFactor = dialog.getInteger(RESOLUTION_MULTIPLIER);
//         	resolutionFactor = dialog.getInteger(RESOLUTION_MULTIPLIER);
         	printToPDF = dialog.getBoolean(PRINT_TO_PDF);
         	pdfPageWidth = dialog.getDouble(PDF_PAGE_WIDTH);
         	pdfPageHeight = dialog.getDouble(PDF_PAGE_HEIGHT);
			new Thread(new Runnable() {  //background the whole printing operation
				public void run() {
					try {
						if (printToPDF)
							pdfCurrentWindow(context);
						else
							printCurrentWindow(context);
		    		} catch (PrinterException e) { 
		    			context.getErrorHandler().handleThrowable(e);
//		    			e.printStackTrace();
		    		}
					context.getLayerViewPanel().repaint();
				}
			}).start();
        }
		return true;
	}
    
    protected void pdfCurrentWindow(PlugInContext context)
    {
    	JFileChooser fileChooser = GUIUtil.createJFileChooserWithOverwritePrompting();
        fileChooser.setDialogTitle(SAVE_PDF);
        fileChooser.setDialogType(JFileChooser.SAVE_DIALOG);
        fileChooser.setMultiSelectionEnabled(false);
        GUIUtil.removeChoosableFileFilters(fileChooser);
        FileFilter fileFilter1 = GUIUtil.createFileFilter(PDF_FILES, new String[]{"pdf"});
        fileChooser.addChoosableFileFilter(fileFilter1);
        fileChooser.setFileFilter(fileFilter1);
        Calendar cal = Calendar.getInstance();
        Date date = cal.getTime();
        SimpleDateFormat df = new SimpleDateFormat("MM-dd-yy_HHmm-ss");
        String dateStr = df.format(date);
        String suggestedFileName = context.getTask().getName() + "_" + dateStr + ".pdf";        
        fileChooser.setSelectedFile(new File(suggestedFileName));
       	if (JFileChooser.APPROVE_OPTION != fileChooser.showSaveDialog(context.getLayerViewPanel()))
       		return;
        String FILE_CHOOSER_DIRECTORY_KEY = SaveFileDataSourceQueryChooser.class.getName() + " - FILE CHOOSER DIRECTORY";
        PersistentBlackboardPlugIn.get(context.getWorkbenchContext()).put(FILE_CHOOSER_DIRECTORY_KEY, fileChooser.getCurrentDirectory().getAbsolutePath());
		String pdfFileName  = fileChooser.getSelectedFile().getPath();
		if (!(pdfFileName.toLowerCase().endsWith(".pdf")))
			pdfFileName = pdfFileName + ".pdf";

        pluginContext = context;
       	printLayerables = new ArrayList(context.getLayerManager()
    			.getLayerables(Layerable.class));  //includes Layers and WMSLayers
    	Collections.reverse(printLayerables);  //print bottom to top
    	//optimize layer styles for print type
    	ArrayList oldStyleList = PrinterDriver.optimizeForVectors(printLayerables, 
    			removeThemeFills, removeBasicFills, 
    			changeLineWidth, (float) lineWidthMultiplier, removeTransparency);
    	//create a printing panel with the current panel's LayerManager
    	final Throwable[] throwable = new Throwable[] { null };
    	printPanel = createLayerPanel(context.getLayerManager(), throwable);
    	PDFDriver.disableDoubleBuffering(printPanel);
    	PDFDriver pdfDriver = new PDFDriver(context, printPanel);
    	
    	ScaleBarRenderer.setEnabled(
    			ScaleBarRenderer.isEnabled(context.getLayerViewPanel())
    			,printPanel);  //transfer scale bar settings to print panel
    	//NorthArrowRenderer.setEnabled(
    	//		NorthArrowRenderer.isEnabled(context.getLayerViewPanel())
    	//		,printPanel);  //transfer North Arrow settings to print panel
    	pdfDriver.setTaskFrame((TaskFrame) context.getWorkbenchFrame().getActiveInternalFrame());

    	pdfDriver.setPrintBorder(printBorder);
    	pdfDriver.setPrintLayerables(printLayerables);
    	windowEnvelope = pluginContext.getLayerViewPanel().getViewport()
    	.getEnvelopeInModelCoordinates();
    	fence = pluginContext.getLayerViewPanel().getFence();

     	try {
      		
    		try {
    	    	// step 1: creation of a document-object
    	    	Document document = new Document(
    	    			new Rectangle((float) (pdfPageWidth * UNIT),
    	    					(float) (pdfPageHeight * UNIT)));

    			// step 2: creation of the writer
    			PdfWriter writer = PdfWriter.getInstance(document, 
    					new FileOutputStream(pdfFileName));
    			writer.setCropBoxSize(
    					new Rectangle(0, 0, ((float) (pdfPageWidth * UNIT)),
    							((float) (pdfPageHeight * UNIT))));
    			writer.setPdfVersion(PdfWriter.VERSION_1_5);
    			writer.setViewerPreferences(PdfWriter.PageModeUseOC);
    			// step 3: we open the document
    			document.open();

    			// step 4: we grab the ContentByte and do some stuff with it

    			// we create a fontMapper and read all the fonts in the font directory
    			DefaultFontMapper mapper = new DefaultFontMapper();
    			FontFactory.registerDirectories();
//    			mapper.insertDirectory("c:\\windows\\fonts");

    			PageFormat pageFormat = new PageFormat();
    			Paper paper = new Paper();
    			double width = pdfPageWidth * UNIT;
    			double height = pdfPageHeight * UNIT;
    			paper.setSize(width, height);
    			paper.setImageableArea(0, 0, width , height );
    			pageFormat.setPaper(paper);
    			double w = pageFormat.getImageableWidth();
    			double h = pageFormat.getImageableHeight();
 
    			PdfContentByte cb = writer.getDirectContent();
    			PdfTemplate tp = cb.createTemplate( (float) w, (float) h);
    			Graphics2D g2 = tp.createGraphics((float) w, (float) h, mapper);
    			tp.setWidth((float) w);
    			tp.setHeight((float) h);
    			
    			pdfDriver.setCb(tp);
    			pdfDriver.setWriter(writer);
    			
    			try  { 
    				initLayerViewPanel(pageFormat); 
       				pdfDriver.setResolutionFactor(resolutionFactor);
    			}
    			catch (Exception e) {
    				String message = (e.getMessage()==null) ? e.toString(): e.getMessage();
    				System.err.println(message);       		
    			}			

    			//The simple method of printing to PDF
//    			try  { 
//    				printPanel.getViewport().zoomToFullExtent();
//    			}
//    			catch (NoninvertibleTransformException e) {
//    				String message = (e.getMessage()==null) ? e.toString(): e.getMessage();
//    				System.err.println(message);       		
//    			}			
//    			printPanel.paintComponent(g2);
    			
    			try  { 
    				pdfDriver.print(g2, pageFormat, 0);
    			} catch (PrinterException e) {
    				String message = (e.getMessage()==null) ? e.toString(): e.getMessage();
    				System.err.println(message);       		
    			}			

    			g2.dispose();
    			tp.sanityCheck(); // all the g2 content is written to tp, not cb
    			cb.addTemplate(tp, 0, 0);
    			cb.sanityCheck();
        		// step 5: we close the document
        		document.close();
    		}
    		catch(DocumentException de) {
    			System.err.println(de.getMessage());
    		}
    		catch(IOException ioe) {
    			System.err.println(ioe.getMessage());
    		}
    	}
    	finally {

    		if (oldStyleList != null) {  // restore the original styles
    			boolean wasFiringEvents = printPanel.getLayerManager().isFiringEvents();
    			printPanel.getLayerManager().setFiringEvents(false);
    			int j = 0;
    			for (Iterator i = printLayerables.iterator(); i.hasNext();) {
    				Object layerable = i.next();
    				if (layerable instanceof Layer) {
    					Layer layer = (Layer) layerable;
    					layer.setStyles( (Collection) oldStyleList.get(j++));
    				}
    			}
    			printPanel.getLayerManager().setFiringEvents(wasFiringEvents);
    		}
    		if (printPanel != null) {
    			PrinterDriver.enableDoubleBuffering(printPanel);
    			printPanel.dispose();
    			printPanel = null;
    		}
    		context.getWorkbenchFrame().setStatusMessage(FINISHED_MESSAGE);
    	}
    }

    
    protected void printCurrentWindow(PlugInContext context)
    throws PrinterException {
    	pluginContext = context;
    	//create a printing panel with the current panel's LayerManager
    	final Throwable[] throwable = new Throwable[] { null };
    	printPanel = createLayerPanel(context.getLayerManager(), throwable);
    	PrinterDriver.disableDoubleBuffering(printPanel);
    	windowEnvelope = pluginContext.getLayerViewPanel().getViewport()
    	.getEnvelopeInModelCoordinates();
    	fence = pluginContext.getLayerViewPanel().getFence();

    	printLayerables = new ArrayList(context.getLayerManager()
    			.getLayerables(Layerable.class));  //includes Layers and WMSLayers
    	Collections.reverse(printLayerables);  //print bottom to top
    	//Create and set up a custom PrinterDriver for use by PrinterJob
    	PrinterDriver printerDriver = new PrinterDriver(context, printPanel);
    	
    	ScaleBarRenderer.setEnabled(
    			ScaleBarRenderer.isEnabled(context.getLayerViewPanel())
    			,printPanel);  //transfer scale bar settings to print panel
    	//NorthArrowRenderer.setEnabled(
    	//		NorthArrowRenderer.isEnabled(context.getLayerViewPanel())
    	//		,printPanel);  //transfer North Arrow settings to print panel
    	printerDriver.setTaskFrame((TaskFrame) context.getWorkbenchFrame().getActiveInternalFrame());

    	printerDriver.setPrintBorder(printBorder);
//    	if (!printerDriver.setResolutionFactor(resolutionFactor)) {
//    		context.getWorkbenchFrame()
//    		.setStatusMessage(RESOLUTION_MULTIPLIER + " " + OUT_OF_RANGE);
//    	}

    	//optimize layer styles for print type
    	ArrayList oldStyleList = PrinterDriver.optimizeForVectors(printLayerables, 
    			removeThemeFills, removeBasicFills, 
    			changeLineWidth, (float) lineWidthMultiplier, removeTransparency);
    	//comment out the following line to test default rendering
    	printerDriver.setPrintLayerables(printLayerables);
    	try {

    		PrinterJob printerJob = PrinterJob.getPrinterJob();
    		attributeSet.add(new JobName(context.getTask().getName(), null));

    		DocFlavor flavor = DocFlavor.SERVICE_FORMATTED.PRINTABLE;
    		if (printService == null) {
    			PrintService[] services = PrintServiceLookup
    			.lookupPrintServices(flavor, attributeSet);
    			if (services.length > 0) {
    				//System.out.println("selected printer " + services[0].getName());
    				printService = services[0];
    			}
    			else { 
    				context.getWorkbenchFrame().warnUser(PRINTER_NOT_FOUND);
    				return;  //exit via finally
    			}
    		}
    		// This is a workaround for the problem of sometimes still getting a
    		// raster even after optimizeForVectors()
//    		try {
//    			if (forceRaster) {
//    				sun.print.RasterPrinterJob.forceRaster = true;
//    				sun.print.RasterPrinterJob.forcePDL = false;
//    			} else if (forceVector) {
//    				sun.print.RasterPrinterJob.forcePDL = true;
//    				sun.print.RasterPrinterJob.forceRaster = false;
//    			}
//    		} catch (Exception e) { 
//    			//just eat any errors that occur if this doesn't exist
//    			context.getWorkbenchFrame().setStatusMessage(e.toString());
//    		}

    		//-------------------- the actual print job --------------------------
    		printerJob.setPrintService(printService);
    		if (printerJob.printDialog(attributeSet)) { //OK pressed		
    			PageFormat pageFormat = PrinterDriver
    				.getPageFormat(attributeSet,printerJob);
    			printerJob.setPrintable(printerDriver, pageFormat);
    			try {
    				initLayerViewPanel(pageFormat);
    				printerDriver.setResolutionFactor(resolutionFactor);
     			} catch (Exception e) {
    	    		String message = (e.getMessage()==null) ? e.toString(): e.getMessage();
    				System.err.println(message);       		
    				throw new PrinterException(message);
    			}
    			printerJob.print(attributeSet);
    		}
    		if (throwable[0] != null) {
    			String message = (throwable[0].getMessage()==null) 
    			? throwable[0].toString(): throwable[0].getMessage();
    			System.err.println(message); 
    			context.getErrorHandler().handleThrowable( 
    					(throwable[0] instanceof Exception) 
    							? (Exception) throwable[0]
    							: new Exception(message));
    		}

    		//-------------------- end actual print job --------------------------
    		printService = printerJob.getPrintService();  //save for next time
    	}
    	finally {

    		if (oldStyleList != null) {  // restore the original styles
    			boolean wasFiringEvents = printPanel.getLayerManager().isFiringEvents();
    			printPanel.getLayerManager().setFiringEvents(false);
    			int j = 0;
    			for (Iterator i = printLayerables.iterator(); i.hasNext();) {
    				Object layerable = i.next();
    				if (layerable instanceof Layer) {
    					Layer layer = (Layer) layerable;
    					layer.setStyles( (Collection) oldStyleList.get(j++));
    				}
    			}
    			printPanel.getLayerManager().setFiringEvents(wasFiringEvents);
    		}
    		if (printPanel != null) {
    			PrinterDriver.enableDoubleBuffering(printPanel);
    			printPanel.dispose();
    			printPanel = null;
    		}
    		printerDriver = null;
    		context.getWorkbenchFrame().setStatusMessage(FINISHED_MESSAGE);
    	}

    }

    public static MultiEnableCheck createEnableCheck(WorkbenchContext workbenchContext)
    {
    	EnableCheckFactory checkFactory = new EnableCheckFactory(workbenchContext);
    	return new MultiEnableCheck()
    	.add(checkFactory.createTaskWindowMustBeActiveCheck())
    	.add(checkFactory.createAtLeastNLayersMustExistCheck(1));        
    }

    /**
     * Construct a printing LayerViewPanel using the PlugInContext's LayerManager
     * @param layerManager
     * @return new LayerViewPanel
     */
    protected LayerViewPanel createLayerPanel(LayerManager layerManager,
    		final Throwable[] throwable){ 
    	LayerViewPanel layerViewPanel = new LayerViewPanel(
    			layerManager, new LayerViewPanelContext() {
    				public void setStatusMessage(String message) {
    				}

    				public void warnUser(String warning) {
    				}

    				public void handleThrowable(Throwable t) {
    					throwable[0] = t;
    				}
    			});
    	return layerViewPanel;
    }

    protected Envelope computePrintPageEnvelope(Envelope windowEnvelope, PageFormat pf) {
    	// keep width of window envelope - adjust height for page format
    	double pageRatio = pf.getImageableHeight()/pf.getImageableWidth();
    	double minX = windowEnvelope.getMinX();
    	double maxX = windowEnvelope.getMaxX();
    	double minY = windowEnvelope.getMinY();
    	double maxY = windowEnvelope.getMaxY();
    	// center and expand up and down option
//  	double centerY = (windowEnvelope.getMaxY() - windowEnvelope.getMinY()) / 2d;
//  	double height2 = (windowEnvelope.getWidth() * pageRatio) / 2d;
//  	minY = centerY - height2; 
//  	maxY = centerY + height2;
    	// top justify and expand down option
    	if (expandToFit){
    		minY = windowEnvelope.getMaxY() - (windowEnvelope.getWidth() * pageRatio);
    		maxY = windowEnvelope.getMaxY();
    	}
    	return new Envelope(minX, maxX, minY, maxY);
    }

    protected void initLayerViewPanel(PageFormat pageFormat) throws Exception {
    	if ((printFenceArea) && (fence != null)) {
    		printEnvelope = computePrintPageEnvelope(fence.getEnvelopeInternal(),
    				pageFormat);
    	} else {
        	if (printBoundsArea) {
        		printEnvelope = computePrintPageEnvelope(((Geometry) (pluginContext.getLayerViewPanel().getSelectionManager()
                		.getSelectedItems().iterator().next())).getEnvelopeInternal(),
        				pageFormat);
        	} else {
        		printEnvelope = computePrintPageEnvelope(windowEnvelope, pageFormat);
        	}
    	}
    	int extentInPixelsX = (int) (pageFormat.getImageableWidth() * resolutionFactor);
    	int extentInPixelsY = (int) (pageFormat.getImageableHeight() * resolutionFactor);
    	if (!expandToFit){
    		double ratio = (printEnvelope.getHeight()/printEnvelope.getWidth());
    		extentInPixelsY = (int) Math.round((ratio * pageFormat.getImageableWidth()) * resolutionFactor);
    	}
    	printPanel.setSize(extentInPixelsX, extentInPixelsY);
    	printPanel.getViewport().zoom(printEnvelope);
    }
}

