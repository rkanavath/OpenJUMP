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

import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

/**
 * Extension containing matching processing also known as join.
 * @author Michaël Michaud
 * @version 0.8.1 (2018-07-11)
 */ 
// History
// 0.8.1 (2018-07-11) : restore I18N for GeometryMatcher names
// 0.8.0 (2018-06-17) : refactor to use add/getParameter, the OJ aggregation
//                      classes and long and boolean attribute types
// 0.7.5 (2017-03-26) : clean headers and remove dead code before inclusion in
//                      OpenJUMP PLUS version
// 0.7.4 (2017-03-13) : overlapping method could not handle linear geometries
// 0.7.3 (2014-03-27) : layers created by the plugin are now true layers, not 
//                      "views" on the source layer as views can cause severe
//                      bugs if schema of the source layer is changed.
//                      autorise le transfert d'attribut quand il n'y a pas 
//                      d'attribut de type String
// 0.7.2 (2013-07-30) : fix a bug which appears when GeometryMatcher and 
//                      DamarauLevenshteinDistanceMatcher are used simultaneously
// 0.7.1 (2013-04-21) : remember last attribute used if layers did not change
// 0.7.0 (2013-04-07) : add MatchingUpdatePlugIn and MatchEditingPlugIn
// 0.6.2 (2013-03-05) : correction d'une regression empechant tout appariment 1:1
// 0.6.1 (2013-03-02) : option to compute min distance of matched features
//                      improve UI labels/I18N for source and target layers
//                      matchingUpdatePlugIn (work-in-progress, not yet activated)                      
// 0.6.0 (2013-01-29) : performance improvements
//                      improve UI labels/I18N for singleSource and singleTarget options
//                      the threaded process is now interruptable
// 0.5.9 (2012-12-03) : UI labels and I18N
// 0.5.8 (2012-10-07) : fix in 1:N matches wich were only partially found
// 0.5.7 (2012-09-  ) : 
// 0.5.6 (2012-06-28) : small fix in the UI (attribute aggregation)
// 0.5.5 (2012-05-08) : fix a NPE when geometry+attribute+cardinality was used
//                      changed the limit definition of damarau-levenshtein
// 0.5.4 (2012-03-12) : add X_MIN_SCORE to reference dataset + fix link layer name 
// 0.5.3 (2012-01-17) : recompile to be java 1.5 compatible
// 0.5.2 (2012-01-03) : small fixes in i18n
// 0.5.1 (2011-12-04) : small fix in i18n
// 0.5   (2011-12-01) : initial version
public class MatchingExtension extends Extension {

    public String getName() {
        return "Matching Extension (Michaël Michaud)";
    }

    public String getVersion() {
        return "0.8.1 (2018-07-11)";
    }

    public void configure(PlugInContext context) throws Exception {
        new MatchingPlugIn().initialize(context);
        //new MatchingUpdatePlugIn().initialize(context);
        new MatchEditingPlugIn().initialize(context);
    }

}