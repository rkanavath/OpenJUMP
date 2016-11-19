package es.unex.sextante.modeler.elements;

import es.unex.sextante.core.Sextante;


public class ModelElementTable
         implements
            IModelElement {

   @Override
   public String toString() {

      return this.getClass().toString();

   }
   
   public String getTypeDesc() {
	   return (Sextante.getText("Table"));
   }   

}
