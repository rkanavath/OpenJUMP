package es.unex.sextante.parameters;

import java.awt.geom.Point2D;
import java.io.IOException;
import java.text.DecimalFormat;

import org.kxml2.io.KXmlParser;
import org.kxml2.io.KXmlSerializer;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import es.unex.sextante.additionalInfo.AdditionalInfo;
import es.unex.sextante.additionalInfo.AdditionalInfoNumericalValue;
import es.unex.sextante.additionalInfo.AdditionalInfoString;
import es.unex.sextante.dataObjects.I3DRasterLayer;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.dataObjects.ITable;
import es.unex.sextante.dataObjects.IVectorLayer;
import es.unex.sextante.exceptions.NullParameterAdditionalInfoException;
import es.unex.sextante.exceptions.NullParameterValueException;
import es.unex.sextante.exceptions.WrongParameterTypeException;

/**
 * A parameter representing a String
 * 
 * @author volaya
 * 
 */
public class ParameterString
         extends
            Parameter {

   private static final String DEFAULT = "default";


   @Override
   public String getParameterTypeName() {

      return "String";

   }


   @Override
   public IRasterLayer getParameterValueAsRasterLayer() throws WrongParameterTypeException {

      throw new WrongParameterTypeException();

   }


   @Override
   public IRasterLayer getParameterValueAsImageLayer() throws WrongParameterTypeException {

	   throw new WrongParameterTypeException();

   }
   
   
   @Override
   public I3DRasterLayer getParameterValueAs3DRasterLayer() throws WrongParameterTypeException {

      throw new WrongParameterTypeException();

   }


   @Override
   public ITable getParameterValueAsTable() throws WrongParameterTypeException {

      throw new WrongParameterTypeException();

   }


   @Override
   public IVectorLayer getParameterValueAsVectorLayer() throws WrongParameterTypeException {

      throw new WrongParameterTypeException();

   }


   @Override
   public int getParameterValueAsInt() throws WrongParameterTypeException {

      throw new WrongParameterTypeException();

   }


   @Override
   public double getParameterValueAsDouble() throws WrongParameterTypeException {

      throw new WrongParameterTypeException();

   }


   @Override
   public boolean getParameterValueAsBoolean() throws WrongParameterTypeException {

      throw new WrongParameterTypeException();

   }


   @Override
   public String getParameterValueAsString() throws WrongParameterTypeException, NullParameterValueException {

      if (m_ParameterValue != null) {
         return (String) m_ParameterValue;
      }
      throw new NullParameterValueException();

   }


   @Override
   public Point2D getParameterValueAsPoint() throws WrongParameterTypeException {

      throw new WrongParameterTypeException();

   }


   @Override
   public boolean setParameterAdditionalInfo(final AdditionalInfo additionalInfo) {

      if (additionalInfo instanceof AdditionalInfoString) {
         m_ParameterAdditionalInfo = additionalInfo;
         return true;
      }
      else {
         return false;
      }

   }


   @Override
   public boolean setParameterValue(final Object value) {	  

      if (value instanceof String) {
         m_ParameterValue = value;
         return true;
      }

      /* Some String type params might have been other types originally and then
       * converted (especially for use with GRASS algorithms).
       * So we need to check what types they were originally and convert their
       * values to String. */
      
      if (value instanceof Number) {
    	  /* create string value from numerical value: straight-forward */
    	  if (value instanceof Integer ) {
    		  final Integer iValue = new Integer ((Integer) value);
    		  m_ParameterValue = iValue.toString();
    	  } else {    		  
    		  final Double dValue = new Double ((Double) value);
    		  /* This value has been _passed_ as a Float or Double, but we will not
    		   * store any decimal places that we don't really need. */
    		  if ( dValue % 1 == 0 ) {
    			  final Integer iValue = dValue.intValue();
    			  m_ParameterValue = iValue.toString();
    		  } else {
    			  m_ParameterValue = dValue.toString();
    		  }
    	  }
    	  return true;
      }
      
      if (value instanceof Boolean) {    	  
    	  /* We store boolean value as "0" (false) or "1" (true). */
    	  if ( ((Boolean) value).booleanValue() == true ) {
    		  m_ParameterValue = "1";
    	  } else {
    		  m_ParameterValue = "0";
    	  }
    	  return true;
      }
      
      if (value instanceof Point2D) {
    	  /* Store 2D coordinates as "X.xxxxxx,Y.yyyyyy". */
    	  final Double X = ((Point2D) value).getX(); 
    	  final Double Y = ((Point2D) value).getY();
    	  String coords = new String (	new DecimalFormat("#.######").format(X) + 
    			  						"," + new DecimalFormat("#.######").format(Y));    	  
    	  m_ParameterValue = (coords);
    	  return true;
      }
      
      /* any other value type cannot be converted to a String */      
      return false;

   }


   @Override
   public Class getParameterClass() {

      return String.class;
   }


   @Override
   protected void serializeAttributes(final KXmlSerializer serializer) throws NullParameterAdditionalInfoException, IOException {

      final AdditionalInfoString ais = (AdditionalInfoString) m_ParameterAdditionalInfo;
      if (ais != null) {
         serializer.text("\n");
         serializer.text("\t\t\t");
         serializer.startTag(null, ATTRIBUTE);
         serializer.attribute(null, NAME, DEFAULT);
         serializer.attribute(null, VALUE, ais.getDefaultString());
         serializer.endTag(null, ATTRIBUTE);
      }
      else {
         throw new NullParameterAdditionalInfoException();
      }

   }


   public static Parameter deserialize(final KXmlParser parser) throws XmlPullParserException, IOException {

      String sDefault = null;

      int tag = parser.nextTag();      
      
      boolean bOver = false;
      while (!bOver) {
         switch (tag) {
            case XmlPullParser.START_TAG:
               if (parser.getName().compareTo(ATTRIBUTE) == 0) {
                  final String sName = parser.getAttributeValue("", NAME);
                  if (sName.compareTo(DEFAULT) == 0) {
                     sDefault = parser.getAttributeValue("", VALUE);
                  }
               }
               break;
            case XmlPullParser.END_TAG:
               if (parser.getName().compareTo(INPUT) == 0) {
                  bOver = true;
               }
               break;
            case XmlPullParser.TEXT:
               break;
         }

         if (!bOver) {
            tag = parser.next();
         }

      }

      final ParameterString param = new ParameterString();
      final AdditionalInfoString ai = new AdditionalInfoString();
      ai.setDefaultString(sDefault);
      param.setParameterAdditionalInfo(ai);

      return param;

   }


   @Override
   public String getCommandLineParameter() {

	  if ( m_ParameterValue != null )
		  return "\"" + (String) m_ParameterValue + "\"";
	  else
		  return null;

   }


   @Override
   public boolean isParameterValueCorrect() {

      return true;

   }

}
