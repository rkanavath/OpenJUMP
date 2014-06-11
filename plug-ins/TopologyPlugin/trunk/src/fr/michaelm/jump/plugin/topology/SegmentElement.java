package fr.michaelm.jump.plugin.topology;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;

import java.util.Collections;
import java.util.List;

/**
 * Created by Michaël on 22/05/14.
 */
public class SegmentElement extends GeometryElement {

    final Coordinate p0, p1;

    public SegmentElement(GeometryWrapper wGeometry, Coordinate c0, Coordinate c1) {
        super(wGeometry);
        this.p0 = c0;
        this.p1 = c1;
    }

    public int compare(Projection o1, Projection o2) {
        double diff = (d2(p0, o1.getCoord()) - d2(p0, o2.getCoord()));
        return diff > 0 ? 1 : diff < 0 ? -1 : 0;
    }

    public boolean equals(Object obj) {
        return this == obj;
    }

    public void insert(List<CoordinateList> list) {
        if (list.size() == 0) list.add(new CoordinateList());
        CoordinateList cl = list.get(list.size()-1);
        cl.add(p0, false);
        if (projections != null) {
            Collections.sort(projections, this);
            for (Projection projection : projections) {
                cl.add(projection.getCoord(), false);
            }
        }
        cl.add(p1, false);
    }

    public void split(List<CoordinateList> list) {
        if (list.size() == 0) list.add(new CoordinateList());
        CoordinateList cl = list.get(list.size()-1);
        cl.add(p0, false);
        if (projections != null) {
            Collections.sort(projections, this);
            for (Projection projection : projections) {
                //if (!projection.getCoord().equals(p0)) {
                    cl.add(projection.getCoord(), false);
                    cl = new CoordinateList();
                    list.add(cl);
                    cl.add(projection.getCoord(), false);
                //}
            }
        }
        cl.add(p1, false);
    }

}