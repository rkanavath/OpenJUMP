package com.geomaticaeambiente.klemgui.utils;

/**
 *
 * @author Geomatica
 */
public class Header {
    
    public Header (String[] headerRows){
        this.headerRows = headerRows;
    }
    
    public Header (String[] headerRows, int rows){
        this.headerRows = headerRows;
        this.rows = rows;
    }
    
    public String[] getHeader(){
        return headerRows;
    }
    
    public int getHeaderRows(){
        return rows;
    }
    
    private String[] headerRows;
    private int rows = 0;
}
