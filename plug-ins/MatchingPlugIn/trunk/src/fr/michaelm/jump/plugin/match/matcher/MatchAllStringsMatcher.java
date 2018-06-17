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


/**
 * Abstract Matcher implementing common methods
 *
 * @author Michaël Michaud
 */
public class MatchAllStringsMatcher extends StringMatcher {
    
    public static final MatchAllStringsMatcher MATCH_ALL = 
        new MatchAllStringsMatcher();
    
    public static MatchAllStringsMatcher instance() {
        return MATCH_ALL;
    }

    public MatchAllStringsMatcher() {
        super("","");
    }
    
    /**
     * {@inheritDoc}.
     */
    public double match(String source, String target, Object context) throws Exception {
        return 1.0;
    }
    
    //public Index createIndex(final Collection<Feature> features) throws Exception {
    //    return new Index() {
    //        public Set<Feature> query(Object value) {
    //            return new HashSet(features);
    //        }
    //    };   
    //}
    
}
