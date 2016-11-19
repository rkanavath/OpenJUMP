package es.unex.sextante.modeler.elements;

import es.unex.sextante.core.Sextante;


public class ModelElementBand
         implements
            IModelElement {

	   public String getTypeDesc() {
		   return (Sextante.getText("Raster_band"));
	   }	
	
}
