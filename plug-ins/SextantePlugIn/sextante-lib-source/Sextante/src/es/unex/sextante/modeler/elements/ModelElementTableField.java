package es.unex.sextante.modeler.elements;

import es.unex.sextante.core.Sextante;


public class ModelElementTableField
         implements
            IModelElement {

	   public String getTypeDesc() {
		   return (Sextante.getText("Table_Field"));
	   }	
	
}
