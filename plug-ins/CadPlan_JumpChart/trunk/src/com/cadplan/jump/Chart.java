/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * Copyright (C) 2006 Cadplan
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
 */

package com.cadplan.jump;

import java.awt.Color;
import java.util.HashMap;
import java.util.Map;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.workbench.model.Category;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.renderer.style.BasicStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.ColorThemingStyle;
import com.vividsolutions.jump.workbench.ui.renderer.style.LabelStyle;

/**
 * User: geoff Date: 28/04/2007 Time: 10:04:38 Copyright 2007 Geoffrey G Roy.
 */
public class Chart {
    boolean debug = false;
    PlugInContext context;
    String categoryName = "Jump Charts";
    ChartData chartData;
    ChartValues[] chartValues;
    ChartAttribute[] chartAttributes;
    FeatureDataset dataset;
    double screenScale;
    I18NPlug iPlug;

    public Chart(PlugInContext context, I18NPlug iPlug) {
        this.context = context;
        this.iPlug = iPlug;
        ChartParams.cancelled = false;
        chartData = new ChartData(context, iPlug);

        if (!ChartParams.cancelled) {

            getData();

            for (final ChartValues chartValue : chartValues) {
                // System.out.println("Att i="+i+ "  value="+
                // chartValues[i].toString());

            }

            createContext();
            createFeatures();
            final Layer layer = context
                    .addLayer(categoryName, "Chart", dataset);
            final ColorThemingStyle colorThemingStyle = ColorThemingStyle
                    .get(layer);
            final String actualName = layer.getName();
            final BasicStyle layerStyle = layer.getBasicStyle();
            layerStyle.setAlpha(255);

            final LabelStyle newLabelStyle = layer.getLabelStyle();
            newLabelStyle.setEnabled(true);
            newLabelStyle.setAttribute("Name");
            newLabelStyle.setEnabled(true); // to show labels in legends

            newLabelStyle.setVerticalAlignment(LabelStyle.ON_LINE);

            // System.out.println("Vert Align:"+newLabelStyle.verticalAlignment);
            layer.removeStyle(layer.getLabelStyle());
            layer.addStyle(newLabelStyle);

            final Map<Object, BasicStyle> themeMap = new HashMap<Object, BasicStyle>();
            ;
            final Map<Object, String> labelMap = new HashMap<Object, String>();

            final int numElements = chartAttributes.length;

            for (int i = 0; i < numElements; i++) {
                final BasicStyle style = new BasicStyle();
                final int alpha = 128;
                final Color color = new Color(Color.HSBtoRGB((float) i
                        / (float) numElements, 1.0f, 1.0f), true);
                style.setFillColor(color);
                // themeMap.put(i,style);
                // labelMap.put(i,String.valueOf(i));
                themeMap.put(i, style);
                // [Giuseppe Aruta 2018-04-04] Added Attribute names to layer
                // legend
                labelMap.put(i, chartAttributes[i].name);

            }
            layer.removeStyle(layer.getStyle(ColorThemingStyle.class));
            final BasicStyle basicStyle = new BasicStyle();
            basicStyle.setFillColor(new Color(230, 230, 230));
            basicStyle.setLineColor(Color.BLACK);
            colorThemingStyle.setAttributeName("Index");
            colorThemingStyle.setAttributeValueToBasicStyleMap(themeMap);
            colorThemingStyle.setAttributeValueToLabelMap(labelMap);
            // [Giuseppe Aruta 2018-04-04] Uncomment as it does not work since
            // OpenJUMP 1.6
            // ColorThemingStyle themingStyle = new ColorThemingStyle("Index",
            // themeMap, labelMap, basicStyle);
            colorThemingStyle.setEnabled(true);
            layer.addStyle(colorThemingStyle);

        }
    }

    private void createContext() {

        if (debug) {
            System.out.println("Creating new catagory");
        }
        final Category category = new Category();
        category.setName(categoryName);
        final FeatureSchema featureSchema = new FeatureSchema();
        if (ChartParams.chartType == ChartParams.LABELS) {
            // featureSchema.addAttribute("$FID", AttributeType.INTEGER);
            featureSchema.addAttribute("Geometry", AttributeType.GEOMETRY);
            // featureSchema.addAttribute("Index", AttributeType.INTEGER);

            featureSchema.addAttribute("Name", AttributeType.STRING);
            for (final ChartAttribute chartAttribute : chartAttributes) {
                featureSchema.addAttribute(chartAttribute.toString(),
                        AttributeType.STRING);
            }
        } else {
            featureSchema.addAttribute("Geometry", AttributeType.GEOMETRY);
            featureSchema.addAttribute("Index", AttributeType.INTEGER);
            featureSchema.addAttribute("iValue", AttributeType.INTEGER);
            featureSchema.addAttribute("dValue", AttributeType.DOUBLE);
            featureSchema.addAttribute("Name", AttributeType.STRING);

        }

        dataset = new FeatureDataset(featureSchema);

        screenScale = 1.0 / context.getLayerViewPanel().getViewport()
                .getScale();
        if (debug) {
            System.out.println("Screen scale= " + screenScale);
        }
    }

    private void getData() {
        chartValues = chartData.getValues();
        chartAttributes = chartData.getAttributes();
    }

    private void createFeatures() {
        double maxValue = 0.0;
        double maxSize = 0.0;

        switch (ChartParams.chartType) {
        case ChartParams.PIE_EAST:
            PieChart pieChart = new PieChart(chartAttributes, chartValues,
                    screenScale, 0.0);
            pieChart.addFeatures(dataset);
            maxValue = pieChart.getMaxValue();
            maxSize = pieChart.getMaxSize();
            break;
        case ChartParams.PIE_NORTH:
            pieChart = new PieChart(chartAttributes, chartValues, screenScale,
                    Math.PI / 2.0);
            pieChart.addFeatures(dataset);
            maxValue = pieChart.getMaxValue();
            maxSize = pieChart.getMaxSize();
            break;
        case ChartParams.BAR:
            final BarChart barChart = new BarChart(chartAttributes,
                    chartValues, screenScale);
            barChart.addFeatures(dataset);
            maxValue = barChart.getMaxValue();
            maxSize = barChart.getMaxSize();

            break;
        case ChartParams.COLUMN:
            final ColumnChart columnChart = new ColumnChart(chartAttributes,
                    chartValues, screenScale);
            columnChart.addFeatures(dataset);
            maxValue = columnChart.getMaxValue();
            maxSize = columnChart.getMaxSize();
            break;
        case ChartParams.LABELS:
            final LabelsChart labelsChart = new LabelsChart(chartAttributes,
                    chartValues, screenScale);
            labelsChart.addFeatures(dataset);

            break;
        }
        if (ChartParams.includeLegend
                && ChartParams.chartType != ChartParams.LABELS) {
            createLegend(dataset);
        }
        if (ChartParams.showScale && !ChartParams.uniformSize) {
            final double interval = ChartParams.scaleInterval(maxValue);
            final double scale = maxSize / maxValue;
            if (debug) {
                System.out.println("maxValue=" + maxValue + "  maxSize="
                        + maxSize + "  scale=" + scale);
            }
            createScale(ChartParams.chartType, dataset, interval, scale);
        }
    }

    private void createScale(int type, FeatureCollection dataset,
            double interval, double scale) {
        final FeatureSchema featureSchema = new FeatureSchema();
        featureSchema.addAttribute("Geometry", AttributeType.GEOMETRY);
        featureSchema.addAttribute("dValue", AttributeType.DOUBLE);
        featureSchema.addAttribute("iValue", AttributeType.INTEGER);
        featureSchema.addAttribute("Index", AttributeType.INTEGER);
        featureSchema.addAttribute("Name", AttributeType.STRING);

        final double x0 = context.getLayerViewPanel().getViewport()
                .getEnvelopeInModelCoordinates().getMinX();
        final double y0 = context.getLayerViewPanel().getViewport()
                .getEnvelopeInModelCoordinates().getMinY();

        final double w = 100 * screenScale;
        final double h = 15 * screenScale;
        final double s = 5 * screenScale;
        final double x = x0 + 100 * screenScale * 2;
        final double y = y0;

        switch (type) {
        case ChartParams.PIE_EAST:
        case ChartParams.PIE_NORTH:
            double rad = 5 * scale * interval;
            final double radMax = ChartParams.maxValue * scale;
            // System.out.println("Chart: maxValue="+ChartParams.maxValue+"  radMax="+radMax+"  rad="+rad);
            double rada = rad;

            final int nsides = 32;
            for (int i = 4; i >= 0; i--) {
                rada = rad;
                if (!ChartParams.linearScale) {
                    rada = radMax * Math.sqrt(rad / radMax);
                }
                if (debug) {
                    System.out.println("rada=" + rada);
                }
                final Coordinate[] circlePoints = new Coordinate[nsides + 1];
                for (int k = 0; k < 32; k++) {
                    final double xp = x + rada
                            * Math.cos(k * 2.0 * Math.PI / nsides);
                    final double yp = y + rada + rada
                            * Math.sin(k * 2.0 * Math.PI / nsides);

                    circlePoints[k] = new Coordinate(xp, yp);
                }
                circlePoints[nsides] = circlePoints[0];
                // Geometry geometry = new
                // GeometryFactory().createLineString(circlePoints);
                final LinearRing lr = new GeometryFactory()
                        .createLinearRing(circlePoints);
                final Geometry geometry = new GeometryFactory().createPolygon(
                        lr, null);
                if (debug) {
                    System.out.println("Geometry: " + geometry);
                }
                final Feature feature = new BasicFeature(featureSchema);

                if (debug) {
                    System.out.println("Geometry: " + geometry);
                }
                feature.setGeometry(geometry);
                feature.setAttribute("dValue", 0.0);
                feature.setAttribute("iValue", 0);
                feature.setAttribute("Index", -1);
                feature.setAttribute("Name", "");
                dataset.add(feature);

                final Coordinate[] linePoints = new Coordinate[2];
                linePoints[0] = new Coordinate(x, y + rada * 2.0);
                linePoints[1] = new Coordinate(x + scale * 5.2 * interval, y
                        + rada * 2.0);

                final Geometry linegeometry = new GeometryFactory()
                        .createLineString(linePoints);

                if (debug) {
                    System.out.println("Geometry: " + geometry);
                }
                final Feature linefeature = new BasicFeature(featureSchema);

                if (debug) {
                    System.out.println("Geometry: " + linegeometry);
                }
                linefeature.setGeometry(linegeometry);
                linefeature.setAttribute("dValue", -1.0);
                linefeature.setAttribute("iValue", -1);
                linefeature.setAttribute("Index", -1);
                linefeature.setAttribute("Name", "");
                dataset.add(linefeature);

                // Coordinate [] pointPoints = new Coordinate[1];
                // pointPoints[0] = new Coordinate(x+scale*8.0*interval,
                // y+rad*2.0);
                // Geometry pointgeometry = new
                // GeometryFactory().createMultiPoint(pointPoints);

                final Coordinate[] pointPoints = new Coordinate[2];
                pointPoints[0] = new Coordinate(x + scale * 5.2 * interval, y
                        + rada * 2.0);
                pointPoints[1] = new Coordinate(x + scale * 8.0 * interval, y
                        + rada * 2.0);
                // Geometry pointgeometry = new
                // GeometryFactory().createLineString(pointPoints);
                final Coordinate position = new Coordinate(x + scale * 6.2
                        * interval, y + rada * 2.0);
                final Geometry pointgeometry = new GeometryFactory()
                        .createPoint(position);

                if (debug) {
                    System.out.println("Geometry: " + geometry);
                }
                final Feature pointfeature = new BasicFeature(featureSchema);

                if (debug) {
                    System.out.println("Geometry: " + pointgeometry);
                }
                pointfeature.setGeometry(pointgeometry);
                pointfeature.setAttribute("dValue", -1.0);
                pointfeature.setAttribute("iValue", -1);
                pointfeature.setAttribute("Index", -1);
                pointfeature.setAttribute("Name", format(interval * (i + 1)));
                dataset.add(pointfeature);

                rad = rad - interval * scale;
            }

            break;
        case ChartParams.BAR:
        case ChartParams.COLUMN:
            final double width = 10 * screenScale;
            double yp = y + 5 * interval * scale;
            for (int i = 4; i >= 0; i--) {
                final Coordinate[] points = new Coordinate[5];
                points[0] = new Coordinate(x, yp);
                points[1] = new Coordinate(x + width, yp);
                points[2] = new Coordinate(x + width, yp + interval * scale);
                points[3] = new Coordinate(x, yp + interval * scale);
                points[4] = points[0];
                // Geometry geometry = new
                // GeometryFactory().createLineString(points);
                final LinearRing lr = new GeometryFactory()
                        .createLinearRing(points);
                final Geometry geometry = new GeometryFactory().createPolygon(
                        lr, null);

                if (debug) {
                    System.out.println("Geometry: " + geometry);
                }
                final Feature feature = new BasicFeature(featureSchema);

                if (debug) {
                    System.out.println("Geometry: " + geometry);
                }
                feature.setGeometry(geometry);
                feature.setAttribute("iValue", -1);
                feature.setAttribute("dValue", -1.0);
                feature.setAttribute("Name", "");
                dataset.add(feature);

                final Coordinate[] linePoints = new Coordinate[2];
                linePoints[0] = new Coordinate(x + width, yp + interval * scale);
                linePoints[1] = new Coordinate(x + 2.0 * width, yp + interval
                        * scale);

                final Geometry linegeometry = new GeometryFactory()
                        .createLineString(linePoints);
                if (debug) {
                    System.out.println("Geometry: " + geometry);
                }
                final Feature linefeature = new BasicFeature(featureSchema);

                if (debug) {
                    System.out.println("Geometry: " + linegeometry);
                }
                linefeature.setGeometry(linegeometry);
                linefeature.setAttribute("dValue", -1.0);
                linefeature.setAttribute("iValue", -1);
                linefeature.setAttribute("Index", -1);
                linefeature.setAttribute("Name", "");
                dataset.add(linefeature);

                // Coordinate [] pointPoints = new Coordinate[1];
                // pointPoints[0] = new Coordinate(x+4*width,
                // yp+interval*scale);
                // Geometry pointgeometry = new
                // GeometryFactory().createMultiPoint(pointPoints);

                final Coordinate[] pointPoints = new Coordinate[2];
                pointPoints[0] = new Coordinate(x + 2.0 * width, yp + interval
                        * scale);
                pointPoints[1] = new Coordinate(x + 6.0 * width, yp + interval
                        * scale);
                final Geometry pointgeometry = new GeometryFactory()
                        .createLineString(pointPoints);

                if (debug) {
                    System.out.println("Geometry: " + geometry);
                }
                final Feature pointfeature = new BasicFeature(featureSchema);

                if (debug) {
                    System.out.println("Geometry: " + pointgeometry);
                }
                pointfeature.setGeometry(pointgeometry);
                pointfeature.setAttribute("dValue", -1.0);
                pointfeature.setAttribute("iValue", -1);
                pointfeature.setAttribute("Index", 1);
                pointfeature.setAttribute("Name", format(interval * (i + 1)));

                dataset.add(pointfeature);

                yp = yp - interval * scale;
            }

            break;
        }

    }

    private void createLegend(FeatureCollection dataset) {
        final FeatureSchema featureSchema = new FeatureSchema();
        featureSchema.addAttribute("Geometry", AttributeType.GEOMETRY);
        featureSchema.addAttribute("dValue", AttributeType.DOUBLE);
        featureSchema.addAttribute("iValue", AttributeType.INTEGER);
        featureSchema.addAttribute("Index", AttributeType.INTEGER);
        featureSchema.addAttribute("Name", AttributeType.STRING);

        final double x0 = context.getLayerViewPanel().getViewport()
                .getEnvelopeInModelCoordinates().getMinX();
        final double y0 = context.getLayerViewPanel().getViewport()
                .getEnvelopeInModelCoordinates().getMinY();

        double w = 100 * screenScale;
        final double h = 15 * screenScale;
        final double s = 5 * screenScale;
        double x = x0 + h;
        double y = y0 + h;
        for (int i = chartAttributes.length - 1; i >= 0; i--) {
            final Coordinate[] pointArray = new Coordinate[5];
            pointArray[0] = new Coordinate(x, y);
            pointArray[1] = new Coordinate(x + w, y);
            pointArray[2] = new Coordinate(x + w, y + h);
            pointArray[3] = new Coordinate(x, y + h);
            pointArray[4] = new Coordinate(x, y);
            for (final Coordinate element : pointArray) {
                // System.out.println("Point "+j+":"+pointArray[j]);
            }

            final LinearRing lr = new GeometryFactory()
                    .createLinearRing(pointArray);
            final Geometry geometry = new GeometryFactory().createPolygon(lr,
                    null);

            final Feature feature = new BasicFeature(featureSchema);

            if (debug) {
                System.out.println("Geometry: " + geometry);
            }
            feature.setGeometry(geometry);
            feature.setAttribute("dValue", -1.0);
            feature.setAttribute("iValue", -1);
            feature.setAttribute("Index", i);
            feature.setAttribute("Name", chartAttributes[i].name);
            dataset.add(feature);
            y = y + h + s;

        }
        x = x0 - 5 * screenScale + h;
        y = y0 - 5 * screenScale + h;
        w = w + 10 * screenScale;
        double th = 0;
        for (final ChartAttribute chartAttribute : chartAttributes) {
            th = th + h + s;
        }
        th = th - s + 10 * screenScale;
        final Coordinate[] borderArray = new Coordinate[5];
        borderArray[0] = new Coordinate(x, y);
        borderArray[1] = new Coordinate(x + w, y);
        borderArray[2] = new Coordinate(x + w, y + th);
        borderArray[3] = new Coordinate(x, y + th);
        borderArray[4] = new Coordinate(x, y);

        final Geometry geometry = new GeometryFactory()
                .createLineString(borderArray);
        final Feature feature = new BasicFeature(featureSchema);
        feature.setGeometry(geometry);

        feature.setAttribute("Name", "");
        feature.setAttribute("dValue", 0.0);
        feature.setAttribute("iValue", 0);
        feature.setAttribute("Index", 0);
        final LabelStyle labelStyle = new LabelStyle();

        dataset.add(feature);

        if (!ChartParams.uniformSize) {
            final Coordinate[] labelArray = new Coordinate[5];
            labelArray[0] = new Coordinate(x, y + th);
            labelArray[1] = new Coordinate(x + w, y + th);
            labelArray[2] = new Coordinate(x + w, y + th + h);
            labelArray[3] = new Coordinate(x, y + th + h);
            labelArray[4] = new Coordinate(x, y + th);

            final LinearRing lr = new GeometryFactory()
                    .createLinearRing(labelArray);
            final Geometry labelGeometry = new GeometryFactory().createPolygon(
                    lr, null);

            // Geometry labelGeometry = new
            // GeometryFactory().createLineString(labelArray);
            // System.out.println("labelGeom: "+labelGeometry.toString());
            final Feature labelFeature = new BasicFeature(featureSchema);
            labelFeature.setGeometry(labelGeometry);
            labelFeature.setAttribute("Name",
                    iPlug.get("JumpChart.Legend.ChartType"));
            labelFeature.setAttribute("dValue", -1.0);
            labelFeature.setAttribute("iValue", -1);
            labelFeature.setAttribute("Index", -1);
            dataset.add(labelFeature);
        }

    }

    private String format(double val) {
        return NumberFormatter.format(val);

    }

}
