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

import com.vividsolutions.jump.feature.Feature;

/**
 * A match between two features. A match is oriented from a source feature
 * to a target feature.
 * 
 * @author Michaël Michaud
 */
public class Match implements Comparable<Match> {
    
    private Feature source;
    private Feature target;
    private double score;
    //private double minDistance = Double.NaN;

    /**
     * Create a Match object.
     * @param source the source Feature to match from
     * @param target the target Feature to match to
     * @param score the score of the match
     */
    public Match(Feature source, Feature target, double score) {
        this.source = source;
        this.target = target;
        this.score = score;
    }
    
    public Feature getSource() {
        return source;
    }
    
    public Feature getTarget() {
        return target;
    }
    
    public double getScore() {
        return score;
    }
    
    /**
     * Combine score with another score so that 
     * - one of to scores is 0 -> final score is 0
     * - both score are 1 -> final score is 1
     */
    public double combineScore(double otherScore) {
        return score * otherScore;
        //return this;
    }
    
    /**
     * Compare two matches by comparing their matching score first, then, in
     * case of matching score equality, their source feature ID, and in case
     * of source feature ID equality, their target ID.
     */
     public int compareTo(Match m) {
        if (getScore() > m.getScore()) return -1;
        else if (getScore() < m.getScore()) return 1;
        else {
            if (getSource().getID() < m.getSource().getID()) return -1;
            else if (getSource().getID() > m.getSource().getID()) return 1;
            else {
                if (getTarget().getID() < m.getTarget().getID()) return -1;
                else if (getTarget().getID() > m.getTarget().getID()) return 1;
                else return 0;
            }
        }
    } 
    
    /**
     * Two matches are equal iff their source feature ID, their target feature 
     * ID AND their matching score are equal.
     * It is important that equals is consistent with compareTo
     */
    public boolean equals(Object o) {
        if (o instanceof Match) {
            Match other = (Match)o;
            return source.getID() == other.getSource().getID() && 
                   target.getID() == other.getTarget().getID() && 
                   score == other.getScore();
        }
        return false;
    }
    
    public String toString() {
        return "Match " + source.getID() + " and " + target.getID() + " with score " + score; 
    }

}
