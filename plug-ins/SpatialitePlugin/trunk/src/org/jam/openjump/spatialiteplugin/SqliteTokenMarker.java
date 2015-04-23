/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI 
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * JUMP is Copyright (C) 2003 Vivid Solutions
 *
 * This program implements extensions to JUMP and is
 * Copyright (C) 2010 Jorge Almaraz.
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
 * Jukka Rahkonen
 * jukka.rahkonen@latuviitta.fi
 * 
 */
package org.jam.openjump.spatialiteplugin;



import jedit.KeywordMap;
import jedit.SQLTokenMarker;
import jedit.Token;

public class SqliteTokenMarker extends SQLTokenMarker {
	
	
	public SqliteTokenMarker(Tnode root){
		super(getKeywordMap(root), true);
	}
	private static void addKeyword(KeywordMap km,Tnode node){
		
		for (int i = 0; i < node.getChildCount(); i++) {
			Tnode child=(Tnode)node.getChildAt(i);
			addKeyword(km,child);
			
			StringBuilder word=  new StringBuilder( child.code);
			if (word.indexOf("()")>0) word.setLength(word.length()-2)  ;
			switch (child.cat) {
				case Tnode.CAT_GROUP: continue;
				case Tnode.CAT_EXPR: km.add(word.toString(), Token.KEYWORD1); break;
				case Tnode.CAT_FUN: km.add(word.toString(),Token.KEYWORD2); break;
				case Tnode.CAT_FIELD: km.add(word.toString(),Token.KEYWORD3); break;

			default:
				km.add(child.caption,Token.KEYWORD3);
				break;
			}

		}
	}
	private static KeywordMap getKeywordMap(Tnode root){
		KeywordMap km =new KeywordMap(true);
		addKeyword(km, root);
		return km;
		
	}

}
