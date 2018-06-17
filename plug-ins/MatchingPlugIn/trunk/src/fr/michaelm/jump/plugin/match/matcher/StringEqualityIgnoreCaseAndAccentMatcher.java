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

import java.text.Collator;

/**
 * String equality matcher to check if two attributes are equals ignoring case
 * and accents.
 *
 * @author Michaël Michaud
 */
public class StringEqualityIgnoreCaseAndAccentMatcher extends StringMatcher {
    
    private static final StringEqualityIgnoreCaseAndAccentMatcher 
        STRING_EQUALITY_IGNORE_CASE_AND_ACCENT =
        new StringEqualityIgnoreCaseAndAccentMatcher("","");
    
    public static StringEqualityIgnoreCaseAndAccentMatcher instance() {
        return STRING_EQUALITY_IGNORE_CASE_AND_ACCENT;
    }
    
    public StringEqualityIgnoreCaseAndAccentMatcher(String sourceAttribute, String targetAttribute) {
        super(sourceAttribute, targetAttribute);
        collator.setStrength(Collator.PRIMARY);
    }
    
    /**
     * Returns 1.0 if attribute A.equalsIgnoreCase(AttributeB), 0.0 otherwise.
     * @param source attribute to match from
     * @param target attribute to match to
     * @param context object containing useful information to check if
     * Attribute a effectively matches Attribute ref
     *
     * @return a double in the range [0-1] representative of the match quality
     * between a and ref. 
     *
     * @throws Exception if input data cannot be processed.
     */
    public double match(String source, String target, Object context) throws Exception {
        if (source == null || target == null) return 0.0;
        return collator.equals(source, target) ? 1.0 : 0.0;
    }

}
