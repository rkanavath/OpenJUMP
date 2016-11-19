package es.unex.sextante.modeler.elements;

import es.unex.sextante.core.Sextante;


public class ModelElementBoolean
         implements
            IModelElement {
	
	   public String getTypeDesc() {
		   return (Sextante.getText("Boolean_value"));
	   }	

}
