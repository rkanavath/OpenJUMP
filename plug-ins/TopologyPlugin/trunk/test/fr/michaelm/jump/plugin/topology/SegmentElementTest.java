package fr.michaelm.jump.plugin.topology;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKTReader;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureSchema;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

/**
 * Created by Michaël on 25/05/14.
 */
public class SegmentElementTest {

    static WKTReader reader = new WKTReader();
    static MaxLateralDistanceVertexSnapper snapper = new MaxLateralDistanceVertexSnapper(5.0, 0.0);
    FeatureSchema schema;
    GeometryWrapper wline;
    STRtree index;

    @Before
    public void before() throws Exception {
        schema = new FeatureSchema();
        schema.addAttribute("GEOMETRY", AttributeType.GEOMETRY);

        index = new STRtree();

        Geometry line = reader.read("LINESTRING(0 0, 10 0, 20 0, 30 0)");
        Feature fline = new BasicFeature(schema);
        fline.setGeometry(line);
        wline = new GeometryWrapper.WLineString(fline, index);
    }

    /** Projette plusieurs points sur la première ligne */
    @Test
    public void projectSeveralPointsToOneSegment() throws ParseException {
        BasicFeature source1 = new BasicFeature(schema);
        source1.setGeometry(reader.read("POINT(1 3)"));
        BasicFeature source3 = new BasicFeature(schema);
        source3.setGeometry(reader.read("POINT(3 3)"));
        BasicFeature source2 = new BasicFeature(schema);
        source2.setGeometry(reader.read("POINT(2 3)"));
        BasicFeature source5 = new BasicFeature(schema);
        source5.setGeometry(reader.read("POINT(5 3)"));
        BasicFeature source6 = new BasicFeature(schema);
        source6.setGeometry(reader.read("POINT(15 3)"));
        List<GeometryElement> candidates = index.query(new Envelope(new Coordinate(0,0), new Coordinate(100,100)));
        Projection proj = GeometryElement.projectSingle(source1, snapper, candidates, true);
        if (proj != null) proj.getTargetElement().add(proj);
        proj = GeometryElement.projectSingle(source3, snapper, candidates, true);
        if (proj != null) proj.getTargetElement().add(proj);
        proj = GeometryElement.projectSingle(source2, snapper, candidates, true);
        if (proj != null) proj.getTargetElement().add(proj);
        proj = GeometryElement.projectSingle(source5, snapper, candidates, true);
        if (proj != null) proj.getTargetElement().add(proj);
        proj = GeometryElement.projectSingle(source6, snapper, candidates, true);
        if (proj != null) proj.getTargetElement().add(proj);
        proj.getTargetElement().getGeometryWrapper().insert();
        //System.out.println(proj.getTargetElement().getGeometryWrapper().getFeature().getGeometry());
        Coordinate[] cc = proj.getTargetElement().getGeometryWrapper().getFeature().getGeometry().getCoordinates();
        Assert.assertTrue(cc[0].equals(new Coordinate(0,0)));
        Assert.assertTrue(cc[1].equals(new Coordinate(1,0)));
        Assert.assertTrue(cc[2].equals(new Coordinate(2,0)));
        Assert.assertTrue(cc[3].equals(new Coordinate(3,0)));
        Assert.assertTrue(cc[4].equals(new Coordinate(5,0)));
        Assert.assertTrue(cc[5].equals(new Coordinate(10,0)));
        Assert.assertTrue(cc[6].equals(new Coordinate(15,0)));
        Assert.assertTrue(cc[7].equals(new Coordinate(20,0)));
    }

    /** Projette plusieurs points sur la première ligne */
    @Test
    public void splitWithSeveralPoints() throws ParseException {
        BasicFeature source1 = new BasicFeature(schema);
        source1.setGeometry(reader.read("POINT(1 3)"));
        BasicFeature source3 = new BasicFeature(schema);
        source3.setGeometry(reader.read("POINT(3 3)"));
        BasicFeature source2 = new BasicFeature(schema);
        source2.setGeometry(reader.read("POINT(2 3)"));
        BasicFeature source5 = new BasicFeature(schema);
        source5.setGeometry(reader.read("POINT(5 3)"));
        BasicFeature source6 = new BasicFeature(schema);
        source6.setGeometry(reader.read("POINT(15 3)"));
        List<GeometryElement> candidates = index.query(new Envelope(new Coordinate(0,0), new Coordinate(100,100)));
        Projection proj = GeometryElement.projectSingle(source1, snapper, candidates, true);
        if (proj != null) proj.getTargetElement().add(proj);
        proj = GeometryElement.projectSingle(source3, snapper, candidates, true);
        if (proj != null) proj.getTargetElement().add(proj);
        proj = GeometryElement.projectSingle(source2, snapper, candidates, true);
        if (proj != null) proj.getTargetElement().add(proj);
        proj = GeometryElement.projectSingle(source5, snapper, candidates, true);
        if (proj != null) proj.getTargetElement().add(proj);
        proj = GeometryElement.projectSingle(source6, snapper, candidates, true);
        if (proj != null) proj.getTargetElement().add(proj);
        proj.getTargetElement().getGeometryWrapper().split();
        //System.out.println(proj.getTargetElement().getGeometryWrapper().getFeature().getGeometry());
        MultiLineString mls = (MultiLineString)proj.getTargetElement().getGeometryWrapper().getFeature().getGeometry();
        Assert.assertTrue(mls.getNumGeometries() == 6);
    }

    /** Projette plusieurs points sur la première ligne */
    @Test
    public void splitWithSeveralPoints2() throws ParseException {
        BasicFeature source1 = new BasicFeature(schema);
        source1.setGeometry(reader.read("POINT(1 3)"));
        BasicFeature source10 = new BasicFeature(schema);
        source10.setGeometry(reader.read("POINT(10 3)"));
        List<GeometryElement> candidates = index.query(new Envelope(new Coordinate(0,0), new Coordinate(100,100)));
        Projection proj = GeometryElement.projectSingle(source1, snapper, candidates, true);
        if (proj != null) proj.getTargetElement().add(proj);
        proj = GeometryElement.projectSingle(source10, snapper, candidates, true);
        if (proj != null) proj.getTargetElement().add(proj);
        proj.getTargetElement().getGeometryWrapper().split();
        System.out.println(proj.getTargetElement().getGeometryWrapper().getFeature().getGeometry());
        //MultiLineString mls = (MultiLineString)proj.getTargetElement().getGeometryWrapper().getFeature().getGeometry();
        //Assert.assertTrue(mls.getNumGeometries() == 6);
    }
}
