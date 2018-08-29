package com.geomaticaeambiente.klemgui.ui;

import java.awt.Color;
import java.awt.Component;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

/**
 *
 * @author Geomatica
 */
public class YourTableCellRenderer1 extends DefaultTableCellRenderer  {

    private int theRow = -1;
    private int theCol = -1;
    private Color theForeColour = Color.BLACK;
    private Color theBackColour = Color.WHITE;

    @Override
    public Component getTableCellRendererComponent(JTable table,
                                                 Object value,
                                                 boolean isSelected,
                                                 boolean hasFocus,
                                                 int row,
                                                 int col) {

    Component c = super.getTableCellRendererComponent(table, value,
                                          isSelected, hasFocus,
                                          row, col);

        // Only for specific cell
        if(row == theRow) {
    //       c.setFont(/* special font*/);
           // you may want to address isSelected here too
            c.setForeground(theForeColour);
            c.setBackground(theBackColour);
        }else{
            c.setForeground(Color.BLACK);
        }
        return c;
    }

    public void setCol(int col){
        this.theCol = col;
    }

    public void setRow(int row){
        this.theRow = row;
    }

    public void setRowCol(int row, int col){
        this.theRow = row;
        this.theCol = col;
    }

    public void setForeColour(Color colour){
        this.theForeColour = colour;
    }

    public void setBackColour(Color colour){
        this.theBackColour = colour;
    }
}
