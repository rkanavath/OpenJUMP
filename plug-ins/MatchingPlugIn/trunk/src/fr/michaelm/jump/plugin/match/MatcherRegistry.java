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

package fr.michaelm.jump.plugin.match;

import fr.michaelm.jump.plugin.match.matcher.*;
import java.util.LinkedHashMap;
import java.util.Map;


/**
 * Matcher Registry
 * @author Michaël Michaud
 */
public class MatcherRegistry<T extends Matcher> {
    
    public static MatcherRegistry<GeometryMatcher> GEOMETRY_MATCHERS = new MatcherRegistry<> (
        MatchAllMatcher.instance(),
        EqualsExactGeom3dMatcher.instance(),
        EqualsNormalizedGeom3dMatcher.instance(),
        EqualsExactGeom2dMatcher.instance(),
        EqualsNormalizedGeom2dMatcher.instance(),
        EqualsTopologicalGeomMatcher.instance(),
        EqualsWithCoordinateToleranceMatcher.instance(),

        IsWithinMatcher.instance(),
        OverlapsMatcher.instance(),
        OverlappedByMatcher.instance(),

        IntersectsMatcher.instance(),
        Intersects0DMatcher.instance(),
        Intersects1DMatcher.instance(),
        Intersects2DMatcher.instance(),

        MinimumDistanceMatcher.instance(),
        CentroidDistanceMatcher.instance(),
        HausdorffDistanceMatcher.instance(),
        SemiHausdorffDistanceMatcher.instance(),
        ShapeMatcher.instance()
    );
    
    public static MatcherRegistry<StringMatcher> STRING_MATCHERS = new MatcherRegistry<> (
        MatchAllStringsMatcher.instance(),
        StringEqualityMatcher.instance(),
        StringEqualityIgnoreCaseMatcher.instance(),
        StringEqualityIgnoreCaseAndAccentMatcher.instance(),
        LevenshteinDistanceMatcher.instance(),
        DamarauLevenshteinDistanceMatcher.instance()
    );
    
    private Map<String,T> map = new LinkedHashMap<>();
    
    public void register(T matcher) {
        //map.put(matcher.toString(), matcher);
        map.put(matcher.getClass().getSimpleName(), matcher);
    }
    
    public MatcherRegistry(T... matchers) {
        for (T matcher : matchers) register(matcher);
    }
    
    public T get(String name) {
        return map.get(name);
    }
    
    public Map<String,T> getMap() {
        return map;
    }
    
    public static Matcher getMatcher(MatcherRegistry<? extends Matcher> registry, String name) {
        return registry.map.get(name);
    }
    
}
