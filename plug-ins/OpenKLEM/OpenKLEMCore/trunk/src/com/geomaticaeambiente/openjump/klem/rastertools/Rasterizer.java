package com.geomaticaeambiente.openjump.klem.rastertools;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.Arrays;

/**
 *
 * @author AdL
 */
public class Rasterizer {
    
    public static DoubleBasicGrid rasterize(Geometry[] geoms,
            double[] attributes, Envelope clipEnvelope, double cellSize) throws Exception {
        
        double minX = Double.MAX_VALUE;
        double maxX = -Double.MAX_VALUE;
        double minY = Double.MAX_VALUE;
        double maxY = -Double.MAX_VALUE;        
        
        for(Geometry geom : geoms) {
            minX = Math.min(minX, geom.getEnvelopeInternal().getMinX());
            maxX = Math.max(maxX, geom.getEnvelopeInternal().getMaxX());
            minY = Math.min(minY, geom.getEnvelopeInternal().getMinY());
            maxY = Math.max(maxY, geom.getEnvelopeInternal().getMaxY());
        }       
        
        if(clipEnvelope != null) {
            minX = clipEnvelope.getMinX();
            minY = clipEnvelope.getMinY();
            maxX = clipEnvelope.getMaxX();
            maxY = clipEnvelope.getMaxY();
        }
        
        Coordinate lowerLeftCorner = new Coordinate(minX, minY);
        
        int columnCount = (int) Math.ceil((maxX - minX) / cellSize);
        int rowCount = (int) Math.ceil((maxY - minY) / cellSize);  

        // Iterate through features and rasterize
        //double noData = Double.NaN;
        double noData = -9999.0;
        DoubleBasicGrid outputGrid = new DoubleBasicGrid(new double[rowCount][columnCount],
                cellSize, noData, lowerLeftCorner);
        for(int r=0; r<rowCount; r++){
            Arrays.fill(outputGrid.getData()[r], noData);
        }

        for(int i=0; i<geoms.length; i++) {

            if(!Double.isNaN(attributes[i])){

                DoubleBasicGrid rasterizedGrid = rasterize(
                        geoms[i], lowerLeftCorner, columnCount, rowCount, cellSize, noData);
                for(int r=0; r<rowCount; r++){
                    for(int c=0; c<columnCount; c++){
                        if(rasterizedGrid.getValue(c, r) == 1) {
                            outputGrid.setValue(c, r, attributes[i]);
                        }
                    }
                }
                
            }

        }
        
        return outputGrid;
        
    }

    public static DoubleBasicGrid rasterize(Geometry geom,
            Coordinate lowerLeftCorner, int columnCount, int rowCount, double cellSize, double noData)
            throws Exception, OutOfMemoryError {

        // Array to store outputs
        DoubleBasicGrid outputGrid = new DoubleBasicGrid(new double[rowCount][columnCount],
                cellSize, noData, lowerLeftCorner);

        for(int r=0; r<rowCount; r++){
            Arrays.fill(outputGrid.getData()[r], 0);
        }

        BufferedImage bimage = new BufferedImage(columnCount, rowCount, BufferedImage.TYPE_INT_ARGB);
        bimage.setAccelerationPriority(1.0f);
        Graphics2D graphics = bimage.createGraphics();

        Coordinate[] coord;
        int[] coordGridX;
        int[] coordGridY;

        Color color = new Color(100);
        graphics.setPaint(color);
        graphics.setPaintMode();

        for(int g=0; g<geom.getNumGeometries(); g++){

            // Check if polygons has holes
            if(geom.getGeometryN(g).getGeometryType().equals("Polygon")){
                Polygon polygon = (Polygon) geom.getGeometryN(g);
                java.awt.geom.Area awtArea;
                if(polygon.getNumInteriorRing() > 0){
                    // Holes found
                    // Exterior ring
                    coord = polygon.getExteriorRing().getCoordinates();
                    coordGridX = new int[coord.length];
                    coordGridY = new int[coord.length];

                    // From geographic coords to image coords
                    for(int p=0; p<coord.length; p++){
                        java.awt.Point point = outputGrid.fromCoordinateToCell(coord[p]);
                        coordGridX[p] = point.x;
                        coordGridY[p] = point.y;
                    }

                    java.awt.Polygon awtPolygon = new java.awt.Polygon(coordGridX, coordGridY, coord.length);
                    awtArea = new java.awt.geom.Area(awtPolygon);

                    // Subtract inner rings
                    for(int ir=0; ir<polygon.getNumInteriorRing(); ir++){
                        coord = polygon.getInteriorRingN(ir).getCoordinates();
                        coordGridX = new int[coord.length];
                        coordGridY = new int[coord.length];
                        // From geographic coords to image coords
                        for(int p=0; p<coord.length; p++){
                            java.awt.Point point = outputGrid.fromCoordinateToCell(coord[p]);
                            coordGridX[p] = point.x;
                            coordGridY[p] = point.y;
                        }
                        awtPolygon = new java.awt.Polygon(coordGridX, coordGridY, coord.length);
                        java.awt.geom.Area awtArea2 = new java.awt.geom.Area(awtPolygon);
                        awtArea.subtract(awtArea2);
                    }
                }else{
                    coord = polygon.getCoordinates();
                    coordGridX = new int[coord.length];
                    coordGridY = new int[coord.length];

                    // From geographic coords to image coords
                    for(int p=0; p<coord.length; p++){
                        java.awt.Point point = outputGrid.fromCoordinateToCell(coord[p]);
                        coordGridX[p] = point.x;
                        coordGridY[p] = point.y;
                    }
                    java.awt.Polygon awtPolygon = new java.awt.Polygon(coordGridX, coordGridY, coord.length);
                    awtArea = new java.awt.geom.Area(awtPolygon);
                }

                graphics.setPaint(color);
                graphics.setPaintMode();
                graphics.draw(awtArea);
                graphics.fill(awtArea);

            }else{
                coord = geom.getGeometryN(g).getCoordinates();
                coordGridX = new int[coord.length];
                coordGridY = new int[coord.length];

                // From geographic coords to image coords
                for(int p=0; p<coord.length; p++){
                    java.awt.Point point = outputGrid.fromCoordinateToCell(coord[p]);
                    coordGridX[p] = point.x;
                    coordGridY[p] = point.y;
                }
                
                String geomType = geom.getGeometryN(g).getGeometryType();
                
                if(geomType.equals("LineString") || geomType.equals("MultiLineString")) {
                    graphics.setPaint(color);
                    graphics.setPaintMode();
                    graphics.drawPolyline(coordGridX, coordGridY, coord.length);
                } else if(geomType.equals("Point") || geomType.equals("MultiPoint")) {
                    graphics.setPaint(color);
                    graphics.setPaintMode();
                    graphics.fillRect(coordGridX[0], coordGridY[0], 1, 1);
                }
            }
            
        }

        for(int r=0; r<rowCount; r++){
            for(int c=0; c<columnCount; c++){
                if(bimage.getRGB(c, r) != 0 && bimage.getRGB(c, r) != -1){
                    outputGrid.setValue(c, r, 1);
                }
            }
        }        
        
        return outputGrid;

    }
    
}
