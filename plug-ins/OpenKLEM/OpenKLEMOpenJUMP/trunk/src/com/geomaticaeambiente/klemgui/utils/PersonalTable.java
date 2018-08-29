package com.geomaticaeambiente.klemgui.utils;

import com.geomaticaeambiente.klemgui.ui.PersonalComponentAbstract;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.swing.table.DefaultTableModel;

/**
 * Class used in the extensions to implement a ReclassRasterComponents.
 * @author Geomatica
 */
public class PersonalTable extends PersonalComponentAbstract{

    public PersonalTable(DefaultTableModel defaultTableModel, Header header, boolean load, 
            boolean save, boolean add, boolean addFile, boolean multipleFiles, 
            String fileDesc, String[] extensions, boolean remove) {
        this.defaultTableModel = defaultTableModel;
        this.header = header;
        this.load = load;
        this.save = save;
        this.add = add;
        this.addFile = addFile;
        this.multipleFiles = multipleFiles;
        this.fileDesc = fileDesc;
        this.extensions = extensions;
        this.remove = remove;
    }        
    
    public DefaultTableModel getDefaultTableModel(){
        return defaultTableModel;
    } 
   
    public boolean loadIsVisible(){
        return load;
    }
    public boolean saveIsVisible(){
        return save;
    }
    public boolean addIsVisible(){
        return add;
    }
    public boolean addFile() {
        return addFile;
    }
    public boolean multipleFiles() {
        return multipleFiles;
    }
    public String fileDesc() {
        return fileDesc;
    }
    public String[] extensions() {
        return extensions;
    }
    public boolean removeIsVisible(){
        return remove;
    }
    
    //if the printable values are different from the values of the table. For example 
    //for print hydrograph values in hours where in table are in minutes.
    public void setPrintableDefaultTableModel(DefaultTableModel printableDefaultTableModel){
        this.printableDefaultTableModel = printableDefaultTableModel;
    }
    
    public void saveTable(File file) throws IOException, Exception{
        
        //choose defaultTableModel
        DefaultTableModel tableModel;
        if(printableDefaultTableModel!= null) {
            tableModel = printableDefaultTableModel;
        } else {
            tableModel = defaultTableModel;
        }
        	
        int nr = tableModel.getRowCount();
        int nc = tableModel.getColumnCount();
        ArrayList rows = new ArrayList(nr);
        String[] row;
        for (int i = 0; i < nr; ++i) {
            row = new String[nc];
            for(int j=0; j<nc; ++j) {
                row[j] = Double.toString(PluginUtils.getThreeDecimalFormat(Float.valueOf(tableModel.getValueAt(i,j)+"")));
            }
            rows.add(row);
        }

        PrintWriter tf;

        tf = new PrintWriter(new BufferedWriter(new FileWriter(file)));
        Object [] cr;
            
        //write
        if(header != null) {
            for (String headerLine : header.getHeader()) {
                tf.println(headerLine);
            }
        }
            
        for(int i = 0; i < rows.size(); ++i) {
            cr = (Object[])rows.get(i);
            tf.print(PluginUtils.getThreeDecimalFormat(Double.parseDouble(cr[0].toString())) + ","); // retrocompatibilità con formato file .table

            for(int j = 1; j < nc-1; ++j) { //j = 1 the column 0 contain the row count
                tf.print(PluginUtils.getThreeDecimalFormat(Double.parseDouble(cr[j].toString()))+",");
            }
            tf.println(PluginUtils.getThreeDecimalFormat(Double.parseDouble(cr[nc-1].toString())));
        }
        tf.close();
    }
       
    
    public DefaultTableModel loadFromFile(File file) throws Exception{

        //choose defaultTableModel
        DefaultTableModel tableModel;
        if(printableDefaultTableModel!= null) {
            tableModel = printableDefaultTableModel;
        } else {
            tableModel = defaultTableModel;
        }        
        
        int tabCols = tableModel.getColumnCount();
        
        // Empty table
        for(int i = tableModel.getRowCount()-1 ; i>=0; --i) {
            tableModel.removeRow(i);
        }

        // Read file
	//ArrayList<String[]> rows = new ArrayList<String[]>(50);
	String [] vals;
	String line;
	
        BufferedReader tr = new BufferedReader(new FileReader(file));
        //skip header rows
        if(header != null) {
            for (String headerLine : header.getHeader()) {
                tr.readLine();
            }
        }

        while((line = tr.readLine()) != null) {
            vals = line.split(",");
            if(vals.length != tabCols) { // OKKIO: retrocompatibilità con formato file .table
                continue;
            }

            List vals1 = new ArrayList<String>();
            vals1.addAll(Arrays.asList(vals));

            String[] values = (String[]) vals1.toArray(new String[vals1.size()]);


            //rows.add(values);
            tableModel.addRow(values);
        }

        tr.close();
	
        return tableModel;

    }
    
    private final Header header;
    private final DefaultTableModel defaultTableModel;
    private DefaultTableModel printableDefaultTableModel;
    private boolean load = false;
    private boolean save = false;
    private boolean add = false;
    private boolean addFile = false;
    private boolean multipleFiles = false;
    private String fileDesc = null;
    private String[] extensions = null;
    private boolean remove = false;
}
