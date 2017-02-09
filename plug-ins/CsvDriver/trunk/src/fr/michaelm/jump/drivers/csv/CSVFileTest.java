/*
 * Library offering read and write capabilities for dsv formats
 * Copyright (C) 2017 Michaël MICHAUD
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

package fr.michaelm.jump.drivers.csv;

import com.vividsolutions.jump.feature.Feature;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Iterator;


/**
 * Test class for the csv package.
 * @author Micha&euml;l MICHAUD
 * @version 0.6 (2012-03-15)
 * @since 0.6
 */

public class CSVFileTest {


    
    public static void main(String[] args) {
        try {
            String dir = "./test/resources";
            testDir(dir);
            //testPirolCSVFile(dir + "tab_wkt.pirol");
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
    }
    
    private static void testDir(String dir) throws IOException {
        System.out.println(new File(dir).getAbsolutePath());
        for (File file : new File(dir).listFiles()) {
            if (file.isDirectory()) continue;
            //if (file.getPath().endsWith("xyz") ||
            //    file.getPath().endsWith("wkt") ||
            //    file.getPath().endsWith("txt") ||
            //    file.getPath().endsWith("csv")) {
                testFile(file.getPath());
            //}
        }
    }
    
    private static void testFile(String file) throws IOException {
        try {
            AutoCSVFile csv = new AutoCSVFile(file);
            //csv.setPropertiesAutomatically();
            System.out.println(csv);
            for (Iterator<Feature> it = csv.iterator(); it.hasNext() ;) {
                Feature f = it.next();
                if (f==null) continue;
                System.out.println(f);
                for (int i = 0 ; i < f.getSchema().getAttributeCount() ; i++) {
                    System.out.println("   " + f.getSchema().getAttributeName(i) + "=" + f.getAttribute(i));
                }
            }
            if (csv.hasExceptions()) {
                for (Exception e : csv.getExceptions()) e.printStackTrace();
            }
            System.out.println("");
        } catch (CSVFileException e) {
            e.printStackTrace();
        }
    }
    

}
