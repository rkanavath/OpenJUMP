/*
 * (C) 2017 Michaël Michaud
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
 * m.michael.michaud@orange.fr
 */

package fr.michaelm.jump.plugin.match.matcher;

import com.vividsolutions.jts.geom.Geometry;


/**
 * Matcher checking if geometries intersect.
 *
 * @author Michaël Michaud
 */
public class Intersects2DMatcher extends GeometryMatcher {
    
    private static final Intersects2DMatcher INTERSECTS2D =
        new Intersects2DMatcher();
    
    public static Intersects2DMatcher instance() {
        return INTERSECTS2D;
    }
    
    public Intersects2DMatcher() {
    }
    
    /**
     * {@inheritDoc}.
     */
    public double match(Geometry source, Geometry target, Object context) throws Exception {
        Geometry i = source.intersection(target);
        return ((!i.isEmpty()) && i.getDimension() == 2) ? 1.0 : 0.0;
    }
    
}
