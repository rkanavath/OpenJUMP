package com.geomaticaeambiente.klemgui.utils;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;

import javax.swing.JTable;

/**
 *
 * @author deluca
 */
public class TextUtils {
    
    public static void exportAsTextFile(String[][] data, File csvFile, String separator) throws IOException {
        
        BufferedWriter buffWriter = new BufferedWriter(new FileWriter(csvFile));
        
        StringBuilder sb = new StringBuilder();
        String newLine = System.getProperty("line.separator");
        for (String[] dataRow : data) {
            for (String dataCell : dataRow) {
                sb.append(dataCell).append(separator);
            }
            sb.replace(sb.length()-1, sb.length(), newLine);
        }
        
        buffWriter.append(sb);
        
        buffWriter.close();
        
    }
    
    
    public static void saveCSV(JTable table, String filename) throws Exception {

        try {
            final File file = new File(filename);
            final BufferedWriter bw = new BufferedWriter(
                    new OutputStreamWriter(new FileOutputStream(
                            file.getAbsoluteFile()), "UTF-8"));

            for (int j = 0; j < table.getColumnCount(); j++) {
                bw.write(table.getModel().getColumnName(j) + ",");
            }
            bw.newLine();
            ;
            for (int i = 0; i < table.getRowCount(); i++) {
                for (int j = 0; j < table.getColumnCount(); j++) {
                    bw.write(table.getModel().getValueAt(i, j) + ",");
                }
                bw.newLine();
            }
            bw.close();
        } catch (final Exception e) {

            //
        }
    }
    
}
