package es.unex.sextante.gui.core;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.math.BigDecimal;

import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.algorithm.AnalysisExtentPanel;

/**
 * A drop down menu that can be attached to any text field and
 * allows to convert the text field's content.
 * It will offer different conversion tools depending on the
 * auto-detected text field contents.
 * 
 * E.g. for numeric fields:
 * 
 * scientific notation -> plain notation 
 * lat/lon distance <-> planar distance
 *  
 * @author benducke
 * 
 */



class MenuActionListener implements ActionListener {

	
	private JTextField field = null;
	private AnalysisExtentPanel extent = null;


	public MenuActionListener(JTextField extField, AnalysisExtentPanel extPanel ) {
		this.setTextField(extField);
		this.setExtentPanel(extPanel);
	}
	
	public void actionPerformed(ActionEvent e) {
		// Functions for integer numbers
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Int_Half"))) {
			TextFieldFuncIntHalf();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Int_Third"))) {
			TextFieldFuncIntThird();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Int_Quarter"))) {
			TextFieldFuncIntQuarter();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Int_Double"))) {
			TextFieldFuncIntDouble();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Int_Triple"))) {
			TextFieldFuncIntTriple();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Int_Log2"))) {
			TextFieldFuncIntLog2();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Int_Pow2"))) {
			TextFieldFuncIntPow2();
		}
		
		// Functions for real numbers
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Round"))) {
			TextFieldFuncRealRound();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Round2"))) {
			TextFieldFuncRealRound2();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Round3"))) {
			TextFieldFuncRealRound3();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Truncate"))) {
			TextFieldFuncRealTruncate();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Half"))) {
			TextFieldFuncRealHalf();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Third"))) {
			TextFieldFuncRealThird();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Quarter"))) {
			TextFieldFuncRealQuarter();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Double"))) {
			TextFieldFuncRealDouble();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Triple"))) {
			TextFieldFuncRealTriple();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Log2"))) {
			TextFieldFuncRealLog2();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_LogE"))) {
			TextFieldFuncRealLogE();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Log10"))) {
			TextFieldFuncRealLog10();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Pow2"))) {
			TextFieldFuncRealPow2();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Sqrt"))) {
			TextFieldFuncRealSqrt();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Real_Plain"))) {
			TextFieldFuncRealPlain();
		}
		
		// Functions for planar/polar conversions
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Planar_To_Polar_Lon"))) {
			TextFieldFuncPlanarToPolarLon();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Planar_To_Polar_Lat"))) {
			TextFieldFuncPlanarToPolarLat();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Polar_Lon_To_Planar"))) {
			TextFieldFuncPolarToPlanarLon();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Polar_Lat_To_Planar"))) {
			TextFieldFuncPolarToPlanarLat();
		}
		
		// Evaluate math expression
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Math"))) {
			TextFieldFuncMath();
		}
		
		// Functions for strings
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_DMSToDecimal"))) {
			TextFieldFuncDMSToDecimal();
		}		
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_DecimalToDMS"))) {
			TextFieldFuncDecimalToDMS();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_String_ToUpper"))) {
			TextFieldFuncStringToUpper();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_String_ToLower"))) {
			TextFieldFuncStringToLower();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_String_Trim"))) {
			TextFieldFuncStringTrim();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_String_Underscore"))) {
			TextFieldFuncStringUnderscore();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_String_DelSpaces"))) {
			TextFieldFuncStringDelSpaces();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_String_AddQuotes"))) {
			TextFieldFuncStringAddQuotes();
		}
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_String_DelQuotes"))) {
			TextFieldFuncStringDelQuotes();
		}
		
		// Functions for any content
		if ( e.getActionCommand().equals(Sextante.getText("Text_Field_Func_Any_Clear"))) {
			TextFieldFuncAnyClear();
		}
		
		this.getTextField().repaint();
	}

	public JTextField getTextField ( ) {
		return (this.field);
	}

	public void setTextField ( JTextField field ) {
		this.field = field;
	}
	
	public AnalysisExtentPanel getExtentPanel () {
		return (this.extent);
	}
	
	public void setExtentPanel ( AnalysisExtentPanel panel ) {
		this.extent = panel;
	}

	public void setText ( String value ) {
		this.getTextField().setText(value);
	}
	
	
	public String getText () {
		return ( this.getTextField().getText() );
	}
	
	public double getXMin () {
		if ( this.extent != null ) {
			return (extent.getXMin());
		}
		return (0.0);
	}
	
	public double getXMax () {
		if ( this.extent != null ) {
			return (extent.getXMax());
		}
		return (0.0);
	}
	
	public double getYMin () {
		if ( this.extent != null ) {
			return (extent.getYMin());
		}
		return (0.0);
	}
	
	public double getYMax () {
		if ( this.extent != null ) {
			return (extent.getYMax());
		}
		return (0.0);
	}

	
	//Text processing functions.
	//INTEGER
	private void TextFieldFuncIntHalf () {
		int oldValue = Integer.parseInt(this.getTextField().getText());
		int newValue = (oldValue/2);		
		this.getTextField().setText(""+newValue);		
	}
	
	
	private void TextFieldFuncIntThird () {
		int oldValue = Integer.parseInt(this.getTextField().getText());
		int newValue = (oldValue/3);		
		this.getTextField().setText(""+newValue);
	}


	private void TextFieldFuncIntQuarter () {
		int oldValue = Integer.parseInt(this.getTextField().getText());
		int newValue = (oldValue/4);
		this.getTextField().setText(""+newValue);
	}
	
	private void TextFieldFuncIntDouble () {
		int oldValue = Integer.parseInt(this.getTextField().getText());
		int newValue = (oldValue*2);		
		this.getTextField().setText(""+newValue);		
	}
	
	private void TextFieldFuncIntTriple () {
		int oldValue = Integer.parseInt(this.getTextField().getText());
		int newValue = (oldValue*3);		
		this.getTextField().setText(""+newValue);
	}
	
	private void TextFieldFuncIntLog2 () {
		int oldValue = Integer.parseInt(this.getTextField().getText());
		if ( oldValue > 0) {
			int newValue = (31 - Integer.numberOfLeadingZeros(oldValue));
			this.getTextField().setText(""+newValue);
		}
	}
	
	private void TextFieldFuncIntPow2 () {
		int oldValue = Integer.parseInt(this.getTextField().getText());
		int newValue = (oldValue*oldValue);		
		this.getTextField().setText(""+newValue);
	}
	
	
	//REAL
	private void TextFieldFuncRealHalf () {
		double oldValue = Double.parseDouble(this.getTextField().getText());
		double newValue = (oldValue/2);
		this.setText(""+newValue);
	}
	
	private void TextFieldFuncRealThird () {
		double oldValue = Double.parseDouble(this.getTextField().getText());
		double newValue = (oldValue/3);		
		this.setText(""+newValue);
	}

	private void TextFieldFuncRealQuarter () {
		double oldValue = Double.parseDouble(this.getTextField().getText());
		double newValue = (oldValue/4);
		this.setText(""+newValue);
	}

	private void TextFieldFuncRealDouble () {
		double oldValue = Double.parseDouble(this.getTextField().getText());
		double newValue = (oldValue*2);
		this.setText(""+newValue);
	}

	private void TextFieldFuncRealTriple () {
		double oldValue = Double.parseDouble(this.getTextField().getText());
		double newValue = (oldValue*3);		
		this.setText(""+newValue);
	}

	private void TextFieldFuncRealPlain () {
		double oldValue = Double.parseDouble(this.getTextField().getText());

		BigDecimal num = new BigDecimal(oldValue);
		String newValue = num.toPlainString();

		this.setText(newValue);
	}
	
	private void TextFieldFuncRealRound () {
		double oldValue = Double.parseDouble(this.getTextField().getText());
		double newValue = (double) (Math.round(oldValue));		
		this.setText(""+newValue);
	}
	
	private void TextFieldFuncRealRound2 () {
		double oldValue = Double.parseDouble(this.getTextField().getText());		
		this.setText(String.format("%.2f", oldValue));
	}
	
	private void TextFieldFuncRealRound3 () {
		double oldValue = Double.parseDouble(this.getTextField().getText());		
		this.setText(String.format("%.3f", oldValue));
	}
	
	private void TextFieldFuncRealTruncate () {
		double oldValue = Double.parseDouble(this.getTextField().getText());
		double newValue = (double) (Math.floor(oldValue));		
		this.setText(String.format("%.0f", newValue));
	}
	
	private void TextFieldFuncRealLog2 () {
		double oldValue = Double.parseDouble(this.getTextField().getText());
		double newValue = (Math.log(oldValue)/Math.log(2));		
		this.setText(""+newValue);
	}
	
	private void TextFieldFuncRealLogE () {
		double oldValue = Double.parseDouble(this.getTextField().getText());
		double newValue = (Math.log(oldValue));		
		this.setText(""+newValue);
	}
	
	private void TextFieldFuncRealLog10 () {
		double oldValue = Double.parseDouble(this.getTextField().getText());
		double newValue = (Math.log(oldValue)/Math.log(10));		
		this.setText(""+newValue);
	}
	
	private void TextFieldFuncRealPow2 () {
		double oldValue = Double.parseDouble(this.getTextField().getText());
		double newValue = (Math.pow(oldValue, 2));		
		this.setText(""+newValue);
	}
	
	private void TextFieldFuncRealSqrt () {
		double oldValue = Double.parseDouble(this.getTextField().getText());
		double newValue = (Math.sqrt(oldValue));		
		this.setText(""+newValue);
	}

	/* 
	 * Convert lat/lon coordinate string in format "DD:MM:SS.ss"
	 * to decimal notation.
	*/
	private void TextFieldFuncDMSToDecimal () {
		String buffer = new String (this.getTextField().getText());
		buffer = buffer.replaceAll("''", "");
		buffer = buffer.replaceAll("'", ":");
		buffer = buffer.replaceAll("°", ":");
		String[] components = buffer.split(":", 3);
		components[0] = components[0].trim(); 
		components[1] = components[1].trim();
		components[2] = components[2].trim();
		final double degs = (double) Double.parseDouble(components[0]);
		final double mins = (double) Double.parseDouble(components[1]);
		final double secs = (double) Double.parseDouble(components[2]);
		
		try {
			double result = (degs+(mins/60)+(secs/3600));
			this.setText(String.format("%.1f", result));
		} catch ( NumberFormatException nfe3 ) {
			return;
		}
		
	}

	/* 
	 * Convert decimal coordinate ton format "DD:MM:SS.ss"
	*/
	private void TextFieldFuncDecimalToDMS () {

		double oldValue = Double.parseDouble(this.getTextField().getText());
		
		final double degs = Math.floor(oldValue);
		final double mins = Math.floor(60*(oldValue-degs));
		final double secs = 3600*(oldValue-degs-(mins/60));
		
		this.getTextField().setText(String.format("%.0f°%.0f'%.8f''", degs, mins, secs));
				
	}

	//NUMERIC TYPE
	
	/**
	 * 
	 * Convert degrees to radians.
	 * 
	 * @param deg
	 * @return
	 */
	private double degToRad ( final double deg )
	{
		return ( deg * ( (2.0 * Math.PI)/360.0 ) );
	}


	/**
	 * Computes length of one degree of longitude, given the current
	 * latitude in degrees. 
	 */
	private double getLengthLon ( final double latInDeg )
	{

		final double lat = degToRad (latInDeg);

		// Calculation constants
		final double m1 = 111412.84;
		final double m2 = -93.5;
		final double m3 = 0.118;

		// Calculate the length of one degree in meters
		final double longLen = (m1 * Math.cos(lat)) + (m2 * Math.cos(3 * lat)) + (m3 * Math.cos(5 * lat));
		
		return ( longLen );
	}
	
	
	/**
	 * Computes length of one degree of latitude, given the current
	 * latitude in degrees. 
	 */
	private double getLengthLat ( final double latInDeg )
	{

		final double lat = degToRad (latInDeg);

		// Calculation constants
		final double m1 = 111132.92;
		final double m2 = -559.82;
		final double m3 = 1.175;
		final double m4 = -0.0023;

		// Calculate the length of one degree in meters
		final double result = m1 + (m2 * Math.cos(2 * lat)) + (m3 * Math.cos(4 * lat)) + (m4 * Math.cos(6 * lat));
		
		return ( result );
	}
	
	
	/**
	 * Extracts the distance token from a "real with latitude" value field.
	 * This is the token after "@".
	 * 
	 * Returns "NaN" if the token could not be extracted and displays an
	 * error message.
	 * 
	 * @return
	 */
	private double getValueDist ()
	{		
		double result = Double.NaN;	
		String content = new String ( this.getTextField().getText() );		
		// We already know that we have either a real or a real with lat
		// field. Let's check which one it is.
		String [] token = content.split("@");
		if ( token != null && token.length == 2 ) {			
			return ( Double.parseDouble(token[0]) ); 
		}		
		// If we get here, then we have a problem.
		JOptionPane.showMessageDialog(null, 
				Sextante.getText("sextante_gui_error_no_real_lat_field"), 
				Sextante.getText("Error"), JOptionPane.ERROR_MESSAGE);
		return ( result );
	}
	
	
	/**
	 * Extracts the latitude token from a "real with latitude" value field.
	 * This is the token after "@".
	 * 
	 * Returns "NaN" if the token could not be extracted and displays an
	 * error message.
	 * 
	 * @return
	 */
	private double getValueLat ()
	{		
		double result = Double.NaN;	
		String content = new String ( this.getTextField().getText() );		
		// We already know that we have either a real or a real with lat
		// field. Let's check which one it is.
		String [] token = content.split("@");
		if ( token != null && token.length == 2 ) {			
			return ( Double.parseDouble(token[1]) ); 
		}		
		// If we get here, then we have a problem.
		JOptionPane.showMessageDialog(null, 
				Sextante.getText("sextante_gui_error_no_real_lat_field"), 
				Sextante.getText("Error"), JOptionPane.ERROR_MESSAGE);
		return ( result );
	}


	/**
	 * Assume that the text field's double value represents a longitudinal
	 * distance and convert that to a planar distance.
	 * Since the length of one unit of longitude depends on the distance
	 * from the equator, the user must also specify an average latitude
	 * for the data using a "@Lat" prefix, e.g.: "1@52" to convert the
	 * length of one degree of longitude at 52° North or South.
	 */
	private void TextFieldFuncPolarToPlanarLon () {
				
		double lat = getValueLat();
		if ( Double.isNaN(lat) ) {
			return;
		}
		
		double oldValue = getValueDist();
		if ( Double.isNaN(oldValue) ) {
			return;
		}

		double newValue = (oldValue*getLengthLon(lat));
		this.getTextField().setText(""+newValue);		
	}


	/**
	 * Assume that the text field's double value represents a latitudinal
	 * distance and convert that to a planar distance.
	 * Since the exact length of one unit of latitude depends on the distance
	 * from the equator (due to the Earth not being a perfect sphere) 
	 * the user must also specify an average latitude
	 * for the data using a "@Lat" prefix, e.g.: "1@52" to convert the
	 * length of one degree of longitude at 52° North or South.
	 */
	private void TextFieldFuncPolarToPlanarLat () {
		
		double lat = getValueLat();
		if ( Double.isNaN(lat) ) {
			return;
		}
		
		double oldValue = getValueDist();
		if ( Double.isNaN(oldValue) ) {
			return;
		}

		double newValue = (oldValue*getLengthLat(lat));
		this.getTextField().setText(""+newValue);
	}


	/**
	 * Assume that the text field's double value represents a metric
	 * distance along the X axis and convert that to a longitudinal distance.
	 * Since the length of one unit of longitude depends on the distance
	 * from the equator, the user must also specify an average latitude
	 * for the data using a "@Lat" prefix, e.g.: "1@52" to convert the
	 * length of one degree of longitude at 52° North or South.
	 */
	private void TextFieldFuncPlanarToPolarLon() {
		double lat = getValueLat();
		if ( Double.isNaN(lat) ) {
			return;
		}
		
		double oldValue = getValueDist();
		if ( Double.isNaN(oldValue) ) {
			return;
		}

		double newValue = (oldValue/getLengthLon(lat));
		this.getTextField().setText(""+newValue);
	}
	
	
	/**
	 * Assume that the text field's double value represents a metric
	 * distance along the X axis and convert that to a latitudinal distance.
	 * Since the exact length of one unit of latitude depends on the
	 * distance from the equator (due to the Earth not being a perfect sphere), 
	 * the user must also specify an average latitude
	 * for the data using a "@Lat" prefix, e.g.: "1@52" to convert the
	 * length of one degree of longitude at 52° North or South.
	 */
	private void TextFieldFuncPlanarToPolarLat() {
		double lat = getValueLat();
		if ( Double.isNaN(lat) ) {
			return;
		}
		
		double oldValue = getValueDist();
		if ( Double.isNaN(oldValue) ) {
			return;
		}

		double newValue = (oldValue/getLengthLat(lat));
		this.getTextField().setText(""+newValue);
	}


	private void TextFieldFuncStringToUpper() {
		this.getTextField().setText(this.getText().toUpperCase());
	}
	
	private void TextFieldFuncStringToLower() {
		this.getTextField().setText(this.getText().toLowerCase());
	}
	
	private void TextFieldFuncStringTrim() {
		this.getTextField().setText(this.getText().trim());
	}

	private void TextFieldFuncStringDelSpaces() {
		this.getTextField().setText(this.getText().replaceAll(" ", ""));
	}
	
	private void TextFieldFuncStringUnderscore() {
		this.getTextField().setText(this.getText().replaceAll(" ", "_"));
	}

	private void TextFieldFuncStringAddQuotes() {
		String buffer = this.getText();
		buffer = buffer.trim();
		if (buffer.startsWith("\"") == false && buffer.endsWith("\"") == false ) {
			this.getTextField().setText("\"" + this.getText() + "\"");
		}
		if (buffer.startsWith("\"") == false && buffer.endsWith("\"") == true ) {
			this.getTextField().setText("\"" + this.getText());
		}
		if (buffer.startsWith("\"") == true && buffer.endsWith("\"") == false ) {
			this.getTextField().setText(this.getText() + "\"");
		}
	}

	private void TextFieldFuncStringDelQuotes() {
		this.setText(this.getText().replaceAll("^\"|\"$", ""));
	}
	
	//ANY TYPE
	private void TextFieldFuncAnyClear () {
		this.getTextField().setText("");		
	}
	
	
	//SIMPLE MATH EXPR EVAL
	private void TextFieldFuncMath () {
		double result = 0.0;
		try {
			result = eval (this.getText());			
		} catch ( RuntimeException re ) {
			return;
		}
		this.getTextField().setText(""+result);
	}
	
	/*
	Supports:
		addition, subtraction, multiplication, division, exponentiation (using ^ symbol).
	Also supports grouping using (...), operator precedence and associativity rules.
	
	This method was originally written by http://stackoverflow.com/users/964243/boann
	and placed in the public domain.
	*/
	public static double eval(final String str) {
	    class Parser {
	        int pos = -1, c;

	        void eatChar() {
	            c = (++pos < str.length()) ? str.charAt(pos) : -1;
	        }

	        void eatSpace() {
	            while (Character.isWhitespace(c)) eatChar();
	        }

	        double parse() {
	            eatChar();
	            double v = parseExpression();
	            if (c != -1) throw new RuntimeException("Expression error: " + (char)c);
	            return v;
	        }

	        // Grammar:
	        // expression = term | expression `+` term | expression `-` term
	        // term = factor | term `*` factor | term `/` factor | term brackets
	        // factor = brackets | number | factor `^` factor
	        // brackets = `(` expression `)`
	        double parseExpression() {
	            double v = parseTerm();
	            for (;;) {
	                eatSpace();
	                if (c == '+') { // addition
	                    eatChar();
	                    v += parseTerm();
	                } else if (c == '-') { // subtraction
	                    eatChar();
	                    v -= parseTerm();
	                } else {
	                    return v;
	                }
	            }
	        }

	        double parseTerm() {
	            double v = parseFactor();
	            for (;;) {
	                eatSpace();
	                if (c == '/') { // division
	                    eatChar();
	                    v /= parseFactor();
	                } else if (c == '*' || c == '(') { // multiplication
	                    if (c == '*') eatChar();
	                    v *= parseFactor();
	                } else {
	                    return v;
	                }
	            }
	        }

	        double parseFactor() {
	            double v;
	            boolean negate = false;
	            eatSpace();
	            if (c == '(') { // brackets
	                eatChar();
	                v = parseExpression();
	                if (c == ')') eatChar();
	            } else { // numbers
	                if (c == '+' || c == '-') { // unary plus & minus
	                    negate = c == '-';
	                    eatChar();
	                    eatSpace();
	                }
	                StringBuilder sb = new StringBuilder();
	                while ((c >= '0' && c <= '9') || c == '.') {
	                    sb.append((char)c);
	                    eatChar();
	                }
	                if (sb.length() == 0) throw new RuntimeException("Expression error: " + (char)c);
	                v = Double.parseDouble(sb.toString());
	            }
	            eatSpace();
	            if (c == '^') { // exponentiation
	                eatChar();
	                v = Math.pow(v, parseFactor());
	            }
	            if (negate) v = -v; // exponentiation has higher priority than unary minus: -3^2=-9
	            return v;
	        }
	    }
	    return new Parser().parse();
	}
	
}


public class TextFieldServices extends MouseAdapter {

	private static final int TEXT_FIELD_TYPE_EMPTY = 0;
	private static final int TEXT_FIELD_TYPE_UNKNOWN = 1;
	private static final int TEXT_FIELD_TYPE_INT = 2;
	private static final int TEXT_FIELD_TYPE_REAL = 3;
	private static final int TEXT_FIELD_TYPE_REAL_WITH_LAT = 4;
	private static final int TEXT_FIELD_TYPE_STRING = 5;
	private static final int TEXT_FIELD_TYPE_STRING_DMS = 6;
	private static final int TEXT_FIELD_TYPE_STRING_MATH = 7;
	
	private static final String[] textFieldTypeNames = {"Text_Field_Type_Empty",
														"Text_Field_Type_Unknown",
														"Text_Field_Type_Integer",
														"Text_Field_Type_Real",
														"Text_Field_Type_Real_With_Lat",
														"Text_Field_Type_String",
														"Text_Field_Type_String_DMS",
														"Text_Field_Type_String_Math"};
	
	private JTextField jTextFieldCurrent = null;	
	private JPopupMenu jPopupServices = null;
	
	private AnalysisExtentPanel extentPanel = null;


	public TextFieldServices() {
		this.setTextField(null);
	}


	public TextFieldServices(JTextField currentField ) {
		this.setTextField(currentField);
	}
	
	
	public TextFieldServices(JTextField currentField, AnalysisExtentPanel currentExtent ) {
		this.setTextField(currentField);
		this.setExtentPanel(currentExtent);
	}	


	@Override
	public void mouseClicked(final MouseEvent e) {		
		if ( e.getButton() == MouseEvent.BUTTON3) {
			if ( this.getTextField() != null ) {
				int i = 0;
				int type = getTextFieldType();
				this.jPopupServices = new JPopupMenu ();
				i = 0; this.jPopupServices.add(Sextante.getText(textFieldTypeNames[type]));
				((Container) this.jPopupServices).getComponent(i).setEnabled(false);
				if ( type != TEXT_FIELD_TYPE_EMPTY ) {
					i ++; this.jPopupServices.addSeparator();
				}
				// Add menu elements depending on field type
				if ( type == TEXT_FIELD_TYPE_INT ) {
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Int_Half"));
					JMenuItem menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Int_Third"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Int_Quarter"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.addSeparator();
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Int_Double"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Int_Triple"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.addSeparator();
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Int_Log2"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.addSeparator();
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Int_Pow2"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					if ( this.getExtentPanel() != null ) {
						i ++; this.jPopupServices.addSeparator();
						i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Planar_To_Polar_Lon"));
						menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
						menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
						i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Polar_Lon_To_Planar"));
						menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
						menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
						i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Planar_To_Polar_Lat"));
						menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
						menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
						i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Polar_Lat_To_Planar"));
						menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
						menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					}
				}
				if ( type == TEXT_FIELD_TYPE_REAL ) {
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Round"));
					JMenuItem menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Round2"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Round3"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Truncate"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.addSeparator();
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Half"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Third"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Quarter"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.addSeparator();
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Double"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Triple"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.addSeparator();
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Log2"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_LogE"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Log10"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.addSeparator();
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Pow2"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Sqrt"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.addSeparator();
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Real_Plain"));					
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.addSeparator();
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_DecimalToDMS"));					
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					
				}
				if ( type == TEXT_FIELD_TYPE_REAL_WITH_LAT || type == TEXT_FIELD_TYPE_REAL ) {
					if ( type == TEXT_FIELD_TYPE_REAL ) {
						i ++; this.jPopupServices.addSeparator();
					}
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Planar_To_Polar_Lon"));
					JMenuItem menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Polar_Lon_To_Planar"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Planar_To_Polar_Lat"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Polar_Lat_To_Planar"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
				}
				if ( type == TEXT_FIELD_TYPE_STRING ) {
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_String_ToUpper"));
					JMenuItem menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_String_ToLower"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.addSeparator();
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_String_Trim"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_String_Underscore"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_String_DelSpaces"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.addSeparator();
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_String_AddQuotes"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_String_DelQuotes"));
					menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
				}
				if ( type == TEXT_FIELD_TYPE_STRING_MATH ) {
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Math"));
					JMenuItem menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
				}
				if ( type == TEXT_FIELD_TYPE_STRING_DMS ) {
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_DMSToDecimal"));
					JMenuItem menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
				}
				if ( type != TEXT_FIELD_TYPE_EMPTY ) {
					if ( i > 1 ) {
						i ++; this.jPopupServices.addSeparator();
					}
					i ++; this.jPopupServices.add(Sextante.getText("Text_Field_Func_Any_Clear"));
					JMenuItem menu = (JMenuItem) ((Container) this.jPopupServices).getComponent(i);
					menu.addActionListener(new MenuActionListener(this.getTextField(), this.getExtentPanel()));
				}				
				this.jPopupServices.doLayout();
				this.jPopupServices.show(this.getTextField(), 0, 0);
			}			
		}
		e.consume();
	}


	public JTextField getTextField ( ) {
		return (this.jTextFieldCurrent);
	}	


	public void setTextField ( JTextField field ) {
		jTextFieldCurrent = field;
	}

	
	public void setExtentPanel ( AnalysisExtentPanel panel ) {
		extentPanel = panel;
	}
	
	
	public AnalysisExtentPanel getExtentPanel ( ) {
		return (this.extentPanel);
	}

	
	public String getText ( ) {
		return (this.jTextFieldCurrent.getText());
	}


	public void setText ( String text ) {
		this.jTextFieldCurrent.setText(text);
	}


	private int getTextFieldType () {
		if ( this.getText() != null && this.getText().trim().length() > 0 ) {
			
			String content = new String (this.getText());
			
			//Check if we have a "Real with Latitude" field:
			//Real@Lat
			//Such fields are used for converting geodesic <-> metric distances.
			boolean isRealWithLat = false;
			String [] token = content.split("@");
			if ( token.length == 2 ) {
				isRealWithLat = true; // We assume this until we know better.
				try {
					String val = token[0].trim();
					if ( Double.valueOf(val) == null ) {
						isRealWithLat = false;
					}
					val = token[1].trim();
					if ( Double.valueOf(val) == null ) {
						isRealWithLat = false;
					}
				} catch ( Exception e ) { // This includes number format exceptions
					isRealWithLat = false;
				}
			}
			
			if ( isRealWithLat == true ) {
				return (TEXT_FIELD_TYPE_REAL_WITH_LAT);
			}
			
			try {
				if ( Integer.valueOf(content) != null ) {
					return (TEXT_FIELD_TYPE_INT);
				}
			} catch ( NumberFormatException nfe ) {
				try {
					if ( Double.valueOf(content) != null ) {
						return (TEXT_FIELD_TYPE_REAL);
					}
				} catch ( NumberFormatException nfe2 ) {
					//See if the string contains a math expression
					String buffer = new String (content);
					buffer = buffer.replaceAll("0", "");
					buffer = buffer.replaceAll("1", "");
					buffer = buffer.replaceAll("2", "");
					buffer = buffer.replaceAll("3", "");
					buffer = buffer.replaceAll("4", "");
					buffer = buffer.replaceAll("5", "");
					buffer = buffer.replaceAll("6", "");
					buffer = buffer.replaceAll("7", "");
					buffer = buffer.replaceAll("8", "");
					buffer = buffer.replaceAll("9", "");
					buffer = buffer.replaceAll("\\.", "");
					buffer = buffer.replaceAll("\\+", "");
					buffer = buffer.replaceAll("-", "");
					buffer = buffer.replaceAll("/", "");
					buffer = buffer.replaceAll("\\*", "");
					buffer = buffer.replaceAll("\\^", "");
					buffer = buffer.replaceAll("\\(", "");
					buffer = buffer.replaceAll("\\)", "");
					buffer = buffer.replaceAll(" ", "");
					if ( buffer.length() == 0 ) {						
						return (TEXT_FIELD_TYPE_STRING_MATH);
					} else {
						//Check if we have a DD:MM:SS.ss string
						buffer = new String (content);
						buffer = buffer.trim();
						buffer = buffer.replaceAll(" ", "");
						buffer = buffer.replaceAll("''", "");
						buffer = buffer.replaceAll("'", ":");
						buffer = buffer.replaceAll("°", ":");
						buffer = buffer.replaceAll(":", "");
						try {
							if ( Double.valueOf(buffer) != null ) {
								return (TEXT_FIELD_TYPE_STRING_DMS);
							}
						} catch ( NumberFormatException nfe3 ) {					
							return (TEXT_FIELD_TYPE_STRING);
						}
						return (TEXT_FIELD_TYPE_STRING);
					}
				}
			}
		} else {
			return (TEXT_FIELD_TYPE_EMPTY);
		}
		return (TEXT_FIELD_TYPE_UNKNOWN);
	} 

}
