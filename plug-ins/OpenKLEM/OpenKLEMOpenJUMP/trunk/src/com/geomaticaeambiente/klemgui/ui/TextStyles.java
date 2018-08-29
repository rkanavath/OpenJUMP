package com.geomaticaeambiente.klemgui.ui;

import java.awt.Color;
import javax.swing.ImageIcon;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

public class TextStyles {
    
    public static StyledDocument addBasicStylesToDocument(StyledDocument doc) {
        
        //Initialize some styles.
        Style defaultStyle = StyleContext.getDefaultStyleContext().
                        getStyle(StyleContext.DEFAULT_STYLE);
 
        Style regular = doc.addStyle(REGULAR, defaultStyle);
        StyleConstants.setFontFamily(regular, "SansSerif");
 
        Style style = doc.addStyle(ITALIC, regular);
        StyleConstants.setItalic(style, true);
        
        style = doc.addStyle(REGULAR_RED, regular);
        StyleConstants.setForeground(style, Color.red);
        
        style = doc.addStyle(BOLD, regular);
        StyleConstants.setBold(style, true);
        
        style = doc.addStyle(TITLE1, regular);
        StyleConstants.setFontSize(style, 18);
        StyleConstants.setBold(style, true);
 
        style = doc.addStyle(TITLE2, regular);
        StyleConstants.setFontSize(style, 15);
        StyleConstants.setBold(style, true);

        return doc;
        
    }    
    
    public StyledDocument addStyle(StyledDocument doc, Style parentStyle, String styleName,
            Integer fontSize, Color foregroundColour, Color backgroundColour, Boolean bold, Boolean italic) {
        
        Style style = doc.addStyle(styleName, parentStyle);
        if(fontSize != null) StyleConstants.setFontSize(style, fontSize);
        if(foregroundColour != null) StyleConstants.setForeground(style, foregroundColour);
        if(backgroundColour != null) StyleConstants.setBackground(style, backgroundColour);
        if(bold != null) StyleConstants.setBold(style, bold);
        if(italic != null) StyleConstants.setItalic(style, italic);

        return doc;
        
    }
    
    public static final String REGULAR = "regular";
    public static final String BOLD = "bold";
    public static final String ITALIC = "italic";
    public static final String REGULAR_RED = "regular_red";
    public static final String TITLE1 = "title1";
    public static final String TITLE2 = "title2";
    
}

