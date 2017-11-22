package com.cadplan.jump;

import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.ui.renderer.style.VertexStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.RingVertexStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.util.java2xml.Java2XML;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Point;

import java.awt.geom.Point2D;
import java.util.Collection;
import java.util.Iterator;

/**
 * User: geoff
 * Date: 15/06/2007
 * Time: 14:17:42
 * Copyright 2005 Geoffrey G Roy.
 */
public class VertexSymbols
{

    PlugInContext context;
    Layer [] layers;
    I18NPlug iPlug;

    public VertexSymbols(PlugInContext context, I18NPlug iPlug)
    {
        this.context = context;
        
        this.iPlug = iPlug;
        layers = context.getSelectedLayers();

        if(layers.length > 1)
        {
            VertexParams.singleLayer = false;
        }
        else if (layers.length == 1)
        {
            VertexParams.singleLayer = true;
            VertexParams.selectedLayer = layers[0];
            VertexStyle vertexStyle = layers[0].getVertexStyle();
//            VertexParams.selectedImage = -1;
//            VertexParams.selectedWKT = -1;
            //System.out.println("VertexStyle: "+vertexStyle.getClass().toString());
            if(vertexStyle instanceof ExternalSymbolsImplType)
            {
                VertexParams.size = vertexStyle.getSize();
                VertexParams.attName = ((ExternalSymbolsImplType)vertexStyle).getAttributeName();
                VertexParams.byValue = ((ExternalSymbolsImplType)vertexStyle).getByValue();
                VertexParams.showLine = ((ExternalSymbolsImplType)vertexStyle).getShowLine();
                VertexParams.showFill = ((ExternalSymbolsImplType)vertexStyle).getShowFill();
                VertexParams.dotted = ((ExternalSymbolsImplType)vertexStyle).getDotted();
                VertexParams.orientation = ((ExternalSymbolsImplType)vertexStyle).getOrientation();
                VertexParams.sides = ((ExternalSymbolsImplType)vertexStyle).getNumSides();
                VertexParams.type = VertexParams.EXTERNAL;
                VertexParams.symbolName = ((ExternalSymbolsImplType)vertexStyle).getSymbolName();
                VertexParams.symbolType = ((ExternalSymbolsImplType)vertexStyle).getSymbolType();
                VertexParams.sizeByScale = ((ExternalSymbolsImplType)vertexStyle).getSizeByScale();
                //System.out.println("Initializing params: symbolName="+VertexParams.symbolName+"  type="+VertexParams.symbolType+
                //		"  sizeByScale:"+VertexParams.sizeByScale);
            }
            else if(vertexStyle instanceof PolygonVertexStyle)
            {
                VertexParams.size = vertexStyle.getSize();
                VertexParams.attName = ((PolygonVertexStyle)vertexStyle).getAttributeName();
                VertexParams.byValue = ((PolygonVertexStyle)vertexStyle).getByValue();
                VertexParams.showLine = ((PolygonVertexStyle)vertexStyle).getShowLine();
                VertexParams.showFill = ((PolygonVertexStyle)vertexStyle).getShowFill();
                VertexParams.dotted = ((PolygonVertexStyle)vertexStyle).getDotted();
                VertexParams.orientation = ((PolygonVertexStyle)vertexStyle).getOrientation();
                VertexParams.sides = ((PolygonVertexStyle)vertexStyle).getNumSides();
                VertexParams.type = VertexParams.POLYGON;
                VertexParams.sizeByScale = ((PolygonVertexStyle)vertexStyle).getSizeByScale();

            }
            else if(vertexStyle instanceof StarVertexStyle)
            {
                VertexParams.size = vertexStyle.getSize();
                VertexParams.attName = ((StarVertexStyle)vertexStyle).getAttributeName();
                VertexParams.byValue = ((StarVertexStyle)vertexStyle).getByValue();
                VertexParams.showLine = ((StarVertexStyle)vertexStyle).getShowLine();
                VertexParams.showFill = ((StarVertexStyle)vertexStyle).getShowFill();
                VertexParams.dotted = ((StarVertexStyle)vertexStyle).getDotted();
                VertexParams.orientation = ((StarVertexStyle)vertexStyle).getOrientation();
                VertexParams.sides = ((StarVertexStyle)vertexStyle).getNumSides();
                VertexParams.type = VertexParams.STAR;
                VertexParams.sizeByScale = ((StarVertexStyle)vertexStyle).getSizeByScale();

            }
            else if(vertexStyle instanceof AnyShapeVertexStyle)
            {
                VertexParams.size = vertexStyle.getSize();
                VertexParams.attName = ((AnyShapeVertexStyle)vertexStyle).getAttributeName();
                VertexParams.byValue = ((AnyShapeVertexStyle)vertexStyle).getByValue();
                VertexParams.showLine = ((AnyShapeVertexStyle)vertexStyle).getShowLine();
                VertexParams.showFill = ((AnyShapeVertexStyle)vertexStyle).getShowFill();
                VertexParams.dotted = ((AnyShapeVertexStyle)vertexStyle).getDotted();
                VertexParams.orientation = ((AnyShapeVertexStyle)vertexStyle).getOrientation();
                VertexParams.sides = ((AnyShapeVertexStyle)vertexStyle).getType();
                VertexParams.type = VertexParams.ANYSHAPE;
                VertexParams.sizeByScale = ((AnyShapeVertexStyle)vertexStyle).getSizeByScale();

            }
            else if(vertexStyle instanceof ImageVertexStyle)
            {
                VertexParams.size = vertexStyle.getSize();
                VertexParams.attName = ((ImageVertexStyle)vertexStyle).getAttributeName();
                VertexParams.byValue = ((ImageVertexStyle)vertexStyle).getByValue();
                VertexParams.orientation = ((ImageVertexStyle)vertexStyle).getOrientation();
                String imageName = ((ImageVertexStyle)vertexStyle).getName();
                VertexParams.type = VertexParams.IMAGE;
                VertexParams.sizeByScale = ((ImageVertexStyle)vertexStyle).getSizeByScale();
                try
                {
	                for(int i=0; i < VertexParams.imageNames.length; i++)
	                {
	                    if(VertexParams.imageNames[i].equals(imageName)) VertexParams.selectedImage = i;
	                }
                }
                catch(NullPointerException ex)
                {
                	
                }
                
            }
            else if(vertexStyle instanceof WKTVertexStyle)
            {
                VertexParams.size = vertexStyle.getSize();
                VertexParams.attName = ((WKTVertexStyle)vertexStyle).getAttributeName();
                VertexParams.byValue = ((WKTVertexStyle)vertexStyle).getByValue();
                VertexParams.orientation = ((WKTVertexStyle)vertexStyle).getOrientation();
                VertexParams.showLine = ((WKTVertexStyle)vertexStyle).getShowLine();
                VertexParams.showFill = ((WKTVertexStyle)vertexStyle).getShowFill();
                VertexParams.dotted = ((WKTVertexStyle)vertexStyle).getDotted();                
                String imageName = ((WKTVertexStyle)vertexStyle).getName();
                VertexParams.type = VertexParams.WKT;
                VertexParams.sizeByScale = ((WKTVertexStyle)vertexStyle).getSizeByScale();

                try
                {
	                for(int i=0; i < VertexParams.wktNames.length; i++)
	                {
	                    if(VertexParams.wktNames[i].equals(imageName)) VertexParams.selectedWKT = i;
	                    
	                }
	                System.out.println("Selecting WKT:"+VertexParams.selectedWKT);
                }
                catch(NullPointerException ex)
                {
                	
                }
                

            }
            //identify current images/WKTs for dialog\
            try
            {
	            for(int i=0; i < VertexParams.imageNames.length; i++)
	            {
	                if(VertexParams.imageNames[i].equals(VertexParams.symbolName)) VertexParams.selectedImage = i;
	            }
            }
            catch(NullPointerException ex)
            {
            	
            }
            try
            {
	            for(int i=0; i < VertexParams.wktNames.length; i++)
	            {
	                if(VertexParams.wktNames[i].equals(VertexParams.symbolName)) VertexParams.selectedWKT = i;
	               
	            }
	            //System.out.println("Selecting images: image:"+VertexParams.selectedImage+"  WKT:"+VertexParams.selectedWKT);
            }
            catch(NullPointerException ex)
            {
            	
            }
            
            if(vertexStyle instanceof ExternalSymbolsType) ((ExternalSymbolsType)vertexStyle).presetTextParameters();
        }
        //System.out.println("selectedImage: "+VertexParams.selectedImage);
        VertexDialog vertexDialog = new VertexDialog(iPlug);
        
        //System.out.println("Loading: VertexParams.sizeByScale:"+VertexParams.sizeByScale);

        if(vertexDialog.cancelled) return;
        for (int i=0; i < layers.length; i++)
        {
            changeStyle(layers[i]);
        }

    }

    

    private void changeStyle(Layer layer)
    {
        //int size = layer.getVertexStyle().getSize();
        //System.out.println("***** Changing style for layer: "+layer.getName());
        layer.removeStyle(layer.getVertexStyle());
        VertexStyle newStyle = null;
        if(VertexParams.type == VertexParams.EXTERNAL)
        {
            newStyle = new ExternalSymbolsImplType();
            ((ExternalSymbolsImplType) newStyle).setNumSides(VertexParams.sides);
            ((ExternalSymbolsImplType) newStyle).setOrientation(VertexParams.orientation);
            ((ExternalSymbolsImplType) newStyle).setDotted(VertexParams.dotted);
            ((ExternalSymbolsImplType) newStyle).setShowLine(VertexParams.showLine);
            ((ExternalSymbolsImplType) newStyle).setShowFill(VertexParams.showFill);
            ((ExternalSymbolsImplType) newStyle).setByValue(VertexParams.byValue);
            ((ExternalSymbolsImplType) newStyle).setAttributeName(VertexParams.attName);
            ((ExternalSymbolsImplType) newStyle).setSymbolName(VertexParams.symbolName);
            ((ExternalSymbolsImplType) newStyle).setSymbolType(VertexParams.symbolType);
            ((ExternalSymbolsImplType) newStyle).setSizeByScale(VertexParams.sizeByScale);

            ((ExternalSymbolsType)newStyle).setupTextParameters();
            newStyle.setSize(VertexParams.size);
            //System.out.println("setSize:"+VertexParams.size);
        }
        else if(VertexParams.type == VertexParams.POLYGON )
        {
            newStyle = new PolygonVertexStyle();
            ((PolygonVertexStyle) newStyle).setNumSides(VertexParams.sides);
            ((PolygonVertexStyle) newStyle).setOrientation(VertexParams.orientation);
            ((PolygonVertexStyle) newStyle).setDotted(VertexParams.dotted);
            ((PolygonVertexStyle) newStyle).setShowLine(VertexParams.showLine);
            ((PolygonVertexStyle) newStyle).setShowFill(VertexParams.showFill);
            ((PolygonVertexStyle) newStyle).setByValue(VertexParams.byValue);
            ((PolygonVertexStyle) newStyle).setAttributeName(VertexParams.attName);
            ((PolygonVertexStyle) newStyle).setSizeByScale(VertexParams.sizeByScale);
            
            ((ExternalSymbolsType)newStyle).setupTextParameters();
            newStyle.setSize(VertexParams.size);
        }
        else if(VertexParams.type == VertexParams.STAR)
        {
             newStyle = new StarVertexStyle();
            ((StarVertexStyle) newStyle).setNumSides(VertexParams.sides);
            ((StarVertexStyle) newStyle).setOrientation(VertexParams.orientation);
            ((StarVertexStyle) newStyle).setDotted(VertexParams.dotted);
            ((StarVertexStyle) newStyle).setShowLine(VertexParams.showLine);
            ((StarVertexStyle) newStyle).setShowFill(VertexParams.showFill);
            ((StarVertexStyle) newStyle).setByValue(VertexParams.byValue);
            ((StarVertexStyle) newStyle).setAttributeName(VertexParams.attName);
            ((StarVertexStyle) newStyle).setSizeByScale(VertexParams.sizeByScale);
            
            ((ExternalSymbolsType)newStyle).setupTextParameters();

            newStyle.setSize(VertexParams.size);
            
            
        }
        else if(VertexParams.type == VertexParams.ANYSHAPE)
        {
             newStyle = new AnyShapeVertexStyle();
            ((AnyShapeVertexStyle) newStyle).setType(VertexParams.sides);
            ((AnyShapeVertexStyle) newStyle).setOrientation(VertexParams.orientation);
            ((AnyShapeVertexStyle) newStyle).setDotted(VertexParams.dotted);
            ((AnyShapeVertexStyle) newStyle).setShowLine(VertexParams.showLine);
            ((AnyShapeVertexStyle) newStyle).setShowFill(VertexParams.showFill);
            ((AnyShapeVertexStyle) newStyle).setByValue(VertexParams.byValue);
            ((AnyShapeVertexStyle) newStyle).setAttributeName(VertexParams.attName);
            ((AnyShapeVertexStyle) newStyle).setSizeByScale(VertexParams.sizeByScale);
            ((ExternalSymbolsType)newStyle).setupTextParameters();

            newStyle.setSize(VertexParams.size);
        }
        else if(VertexParams.type == VertexParams.IMAGE)
        {
            newStyle = new ImageVertexStyle();
            int n = VertexParams.selectedImage;
            ((ImageVertexStyle) newStyle).setSize(VertexParams.images[n].getWidth(null),VertexParams.images[n].getHeight(null));
            ((ImageVertexStyle) newStyle).setName(VertexParams.imageNames[n]);
            ((ImageVertexStyle) newStyle).setOrientation(VertexParams.orientation);
            ((ImageVertexStyle) newStyle).setByValue(VertexParams.byValue);
            ((ImageVertexStyle) newStyle).setAttributeName(VertexParams.attName);
            ((ImageVertexStyle) newStyle).setSizeByScale(VertexParams.sizeByScale);
            
            ((ExternalSymbolsType)newStyle).setupTextParameters();

            newStyle.setSize(VertexParams.size);
            //System.out.println("Size set:"+newStyle.getSize());
        }
        else if(VertexParams.type == VertexParams.WKT)
        {
            newStyle = new WKTVertexStyle();
            int n = VertexParams.selectedWKT;
            ((WKTVertexStyle) newStyle).setSize(VertexParams.wktShapes[n].extent,VertexParams.wktShapes[n].extent);
            ((WKTVertexStyle) newStyle).setName(VertexParams.wktNames[n]);
            ((WKTVertexStyle) newStyle).setOrientation(VertexParams.orientation);
            ((WKTVertexStyle) newStyle).setDotted(VertexParams.dotted);
            ((WKTVertexStyle) newStyle).setShowLine(VertexParams.showLine);
            ((WKTVertexStyle) newStyle).setShowFill(VertexParams.showFill);            
            ((WKTVertexStyle) newStyle).setByValue(VertexParams.byValue);
            ((WKTVertexStyle) newStyle).setAttributeName(VertexParams.attName);
            ((WKTVertexStyle) newStyle).setSizeByScale(VertexParams.sizeByScale);
            
            ((ExternalSymbolsType)newStyle).setupTextParameters();
            
            newStyle.setSize(VertexParams.size);
            //System.out.println("Size set:"+newStyle.getSize()+" ::"+VertexParams.size);
        }
        //***XX
        Double viewScale = 1.0/context.getLayerViewPanel().getViewport().getScale();
        VertexParams.actualScale = viewScale;
        //System.out.println("setSize:"+VertexParams.size+"  scale="+viewScale);
        newStyle.setEnabled(true);
        newStyle.initialize(layer);
        layer.addStyle(newStyle);

        layer.fireAppearanceChanged();

        Java2XML conv = new Java2XML();
        try
       {

            //String xmls = conv.write((Object)newStyle,"root");
            //System.out.println("XML:"+xmls);
        }
        catch(Exception ex)
        {
            ex.printStackTrace();
        }
        
    }
}
