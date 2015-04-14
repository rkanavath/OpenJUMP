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
