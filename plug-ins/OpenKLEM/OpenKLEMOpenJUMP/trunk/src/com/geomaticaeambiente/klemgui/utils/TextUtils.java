package com.geomaticaeambiente.klemgui.utils;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

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
    
    
}
