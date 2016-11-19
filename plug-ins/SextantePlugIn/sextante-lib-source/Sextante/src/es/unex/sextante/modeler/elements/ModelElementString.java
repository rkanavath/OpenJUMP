package es.unex.sextante.modeler.elements;

import es.unex.sextante.core.Sextante;


public class ModelElementString
         implements
            IModelElement {
	
	
	   public String getTypeDesc() {
		   return (Sextante.getText("String"));
	   }	

}
