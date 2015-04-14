package org.jam.openjump.spatialiteplugin;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;

import javax.swing.tree.TreeNode;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;


public class Tnode  implements TreeNode,Comparable<Tnode> {

	static final int CAT_GROUP=0;
	static final int CAT_EXPR=1;
	static final int CAT_FUN=2;

	static final int CAT_FIELD=5;
	static final int CAT_GEOTABLE=30;
	static final int CAT_GEOTABLE_VSHAPE=31;
	static final int CAT_GEOTABLE_VFDO=32;
	static final int CAT_GEOTABLE_FDO=33;
	static final int CAT_TABLE=40;
	
	private ArrayList<Tnode> childs=null;
	private static final String scat="GROUPEXPR FUNC";
	
	String caption=null;
	String code=null;;
	String des=null;
	Tnode parentNode=null;
	int cat =-1;//0:GROUP;1:EXPR;2:FUN;3:geotable;4:table
	
	private static void addChildNodes(Tnode parent,Element element){
	      NodeList nl= element.getChildNodes();
	      int nc =nl.getLength();
	      parent.childs = new ArrayList<Tnode>();
	      for (int i =0 ;i<nc;i++){
	    	    Node item =nl.item(i);
	    	    if (item.getNodeType()!=Node.ELEMENT_NODE )continue;
	    	    if (item.getNodeName().equals("DES")){
	    	    	parent.des=item.getTextContent();
	    	    	continue;
	    	    }
	        	Element ele =  (Element) item;
	        	Tnode child= new Tnode(((Element) ele).getAttribute("caption"),
	        			((Element) ele).getAttribute("code"));
	        	child.parentNode=parent;
	        	child.cat= (int)scat.indexOf(ele.getNodeName())/5;
	        	parent.childs.add(child);
	        	addChildNodes(child, ele);
	        	
	       }
	      parent.childs.trimToSize();
	}
	 
	static Tnode loadFromXml(){
       DocumentBuilder db;
        Document doc=null;
		try {
			db = DocumentBuilderFactory.newInstance().newDocumentBuilder();
			doc = db.parse(new InputSource(Tnode.class.getResourceAsStream("/org/jam/openjump/spatialiteplugin/funchlp.xml")));
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (SAXException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} 
		
        Element root = doc.getDocumentElement();
        Tnode result =new Tnode("DB","");
        result.cat=Tnode.CAT_GROUP;
        addChildNodes(result,root);
        result.childs.trimToSize();
		return result;
	}


	public Tnode(){
		super();
		this.caption="ROOT" ;
		this.code="";
		this.des="";
	}
	
	public Tnode(String caption,String code){
		super();
		this.caption=caption ;
		this.code=code;
		this.des="";
	}
	public void addChild(Tnode node){
		if (childs==null) childs= new ArrayList<Tnode>();
		childs.add(node);
		node.parentNode=this;
	}
	public Tnode child(int index){
		if (childs!=null) return childs.get(index);
		else return null;
		
	}
	public void removeChilds(){
		if (childs==null)return;
		childs.clear();
		childs=null;
	}
	public void trimToSize(){
		if (childs!=null)childs.trimToSize();
	}
	
	public void sort(){
		Collections.sort(childs);
	}


	@SuppressWarnings("unchecked")
	@Override
	public Enumeration children() {
		return Collections.enumeration(childs);
	}

	@Override
	public boolean getAllowsChildren() {
		return false;
	}

	@Override
	public TreeNode getChildAt(int i) {
		if ((childs!=null)&&(i<childs.size())) return childs.get(i);
		else return null;
	}

	@Override
	public int getChildCount() {
		if (childs!=null) return childs.size();
		else	return 0;
	}

	@Override
	public int getIndex(TreeNode node) {
		if (childs!=null) 	return childs.indexOf(node);
		else return -1;
		
	}

	@Override
	public TreeNode getParent() {
		return parentNode;
	}

	@Override
	public boolean isLeaf() {
		return this.getChildCount()==0;
	}

	@Override
	public int compareTo(Tnode node) {
		if (this.cat==node.cat) return this.caption.compareTo(node.caption);
		else {	return (this.cat<node.cat)?-1:1;}
		
	}

}
