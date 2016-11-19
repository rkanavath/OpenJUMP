package es.unex.sextante.parameters;

import java.awt.geom.Point2D;
import java.io.IOException;

import org.kxml2.io.KXmlParser;
import org.kxml2.io.KXmlSerializer;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import es.unex.sextante.additionalInfo.AdditionalInfo;
import es.unex.sextante.additionalInfo.AdditionalInfoBoolean;
import es.unex.sextante.additionalInfo.AdditionalInfoString;
import es.unex.sextante.dataObjects.I3DRasterLayer;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.dataObjects.ITable;
import es.unex.sextante.dataObjects.IVectorLayer;
import es.unex.sextante.exceptions.NullParameterAdditionalInfoException;
import es.unex.sextante.exceptions.NullParameterValueException;
import es.unex.sextante.exceptions.WrongParameterTypeException;

/**
 * A parameter representing a boolean value
 * 
 * @author volaya
 * 
 */
public class ParameterBoolean
         extends
            Parameter {

   private static final String DEFAULT = "default";	
	
   @Override
   public String getParameterTypeName() {

      return "Boolean";

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
   public String getParameterValueAsString() throws WrongParameterTypeException, NullParameterValueException {

      if (m_ParameterValue != null) {
         return ((Boolean) m_ParameterValue).toString();
      }
      throw new NullParameterValueException();

   }


   @Override
   public Point2D getParameterValueAsPoint() throws WrongParameterTypeException {

      throw new WrongParameterTypeException();

   }


   @Override
   public boolean getParameterValueAsBoolean() throws WrongParameterTypeException, NullParameterValueException {

      if (m_ParameterValue != null) {
         return ((Boolean) m_ParameterValue).booleanValue();
      }
      throw new NullParameterValueException();

   }


   @Override
   public boolean setParameterAdditionalInfo(final AdditionalInfo additionalInfo) {

      if (additionalInfo instanceof AdditionalInfoBoolean) {
         m_ParameterAdditionalInfo = additionalInfo;
         return true;
      }
      else {
         return false;
      }

   }


   @Override
   public boolean setParameterValue(final Object value) {

      if (value instanceof Boolean) {
         m_ParameterValue = value;
         return true;
      }
      else {
         return false;
      }

   }


   @Override
   public Class getParameterClass() {

      return Boolean.class;

   }


   @Override
   protected void serializeAttributes(final KXmlSerializer serializer) throws NullParameterAdditionalInfoException, IOException {
	      final AdditionalInfoBoolean aib = (AdditionalInfoBoolean) m_ParameterAdditionalInfo;
	      if (aib != null) {
	         serializer.text("\n");
	         serializer.text("\t\t\t");
	         serializer.startTag(null, ATTRIBUTE);
	         serializer.attribute(null, NAME, DEFAULT);
	         final Boolean bDefault = aib.getDefaultValue();
	         serializer.attribute(null, VALUE, bDefault.toString());
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

	   final ParameterBoolean param = new ParameterBoolean();	   	   
	   final AdditionalInfoBoolean ai = new AdditionalInfoBoolean(new Boolean(sDefault));
	   param.setParameterAdditionalInfo(ai);

	   return param;
   }


   @Override
   public String getCommandLineParameter() {

      final Boolean b = (Boolean) m_ParameterValue;

      if ( b!= null )
    	  return "\"" + b.toString() + "\"";
      else
    	  return null;

   }


   @Override
   public boolean isParameterValueCorrect() {

      return true;

   }

}
