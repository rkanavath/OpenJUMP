package es.unex.sextante.modeler.elements;

import es.unex.sextante.core.Sextante;


public class ModelElementImageLayer
         implements
            IModelElement {

   public static final int NUMBER_OF_BANDS_UNDEFINED = -1;
   private int             m_iNumberOfBands          = NUMBER_OF_BANDS_UNDEFINED;


   public String getTypeDesc() {
	   return (Sextante.getText("Image_Layer"));
   }   
   
   
   public int getNumberOfBands() {

      return m_iNumberOfBands;

   }


   public void setNumberOfBands(int numberOfBands) {

      if (numberOfBands < 1) {
         numberOfBands = NUMBER_OF_BANDS_UNDEFINED;
      }
      m_iNumberOfBands = numberOfBands;

   }


   @Override
   public String toString() {

      return this.getClass().toString() + "," + Integer.toString(m_iNumberOfBands);

   }

}
