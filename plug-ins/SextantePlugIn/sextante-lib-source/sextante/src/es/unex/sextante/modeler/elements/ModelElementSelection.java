package es.unex.sextante.modeler.elements;

import es.unex.sextante.core.Sextante;


public class ModelElementSelection
         implements
            IModelElement {
	
	   public String getTypeDesc() {
		   return (Sextante.getText("option_setting"));
	   }	

}
