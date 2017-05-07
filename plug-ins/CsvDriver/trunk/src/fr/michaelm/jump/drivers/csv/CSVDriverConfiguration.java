/*
 * Library offering read and write capabilities for dsv formats
 * Copyright (C) 2017 Michaël MICHAUD
 * michael.michaud@free.fr
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

import com.vividsolutions.jump.io.datasource.DataSource;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.datasource.DataSourceQueryChooserManager;
import com.vividsolutions.jump.workbench.plugin.Extension;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import org.openjump.core.ui.io.file.DataSourceFileLayerLoader;
import org.openjump.core.ui.io.file.FileLayerLoader;
import org.openjump.core.ui.swing.factory.field.CheckBoxFieldComponentFactory;
import org.openjump.core.ui.swing.factory.field.ComboBoxFieldComponentFactory;
import org.openjump.core.ui.swing.factory.field.FieldComponentFactoryRegistry;

import java.io.IOException;
import java.net.URI;
import java.nio.charset.Charset;
import java.util.*;
import java.util.regex.Pattern;

import static fr.michaelm.jump.drivers.csv.FieldSeparator.*;


/**
 * Extension loading a driver for csv and other character delimited text files
 * @author Micha&euml;l MICHAUD
 * @version 1.0.1 (2017-05-07)
 */
// 1.0.1 (2017-05-07) fix parser to parse correctly non quoted fields including double quotes
// 1.0.0 (2017-02-09) writer : add encoding option
//                    reader : do not keep columns used to make the feature geometry
//                    writer : write WKT with Z values
// 0.9.2 (2014-07-17) abandon the guess encoding method of CSV (auto)
// 0.9.1 (2014-06-16) fix a bug preventing to choose charset and field separator n save as csv
// 0.9.0 (2014-05-14) handle csv resources included in a compressed file
// 0.8.0 (2013-11-07) make csv-driver persistable in an OpenJUMP project (fix a number of problems)
//                    use fixed english keys for persistence instead of I18N
//                    (this change breaks the format of previously saved projects containing csv files)
// 0.7.0 (2013-03-24) make csv-driver persistable in an OpenJUMP project
// 0.6.6 (2012-10-02) make csv-driver compatible with SaveDatasetsPlugIn and 
//       improved error management (+I18N)
// 0.6.5 (2012-08-11) add geometry column with empty geometry collections if
//       geometry is missing
// 0.6.1 (2012-06-19) fix an UnsupportedCharsetException
// 0.6   (2012-04-16) replace 0.5 version of text driver by a completly new one
//       merging pirol and michael's drivers
// 0.5   (2012-03-11) add a JScrollPane for the attribute filter (export)
// 0.4   (2012-01-16) add option to manage datatypes, compile for java 1.5
// 0.3   (2011-07-10) enable attribute filter for export
// 0.2   (2010-04-30) 1 fixes in remove quotes method and in language file
// 0.2   (2009-10-07)

public class CSVDriverConfiguration extends Extension {

    public String getName() {
        return "CSV Driver";
    }

    public String getVersion() {
        return "1.0.0 (2017-02-09)";
    }

    public void configure(PlugInContext context) throws Exception {

        final WorkbenchContext wcontext = context.getWorkbenchContext();

        ////////////////////////////////////////////////////////////////////////
        // Register field components
        ////////////////////////////////////////////////////////////////////////

        FieldComponentFactoryRegistry.setFactory(wcontext,
            "CharsetChooser", new ComboBoxFieldComponentFactory(
                context.getWorkbenchContext(), null, createCommonCharsetArray()));
        
        FieldComponentFactoryRegistry.setFactory(wcontext,
            "CommentLinePatternChooser", new ComboBoxFieldComponentFactory(
                context.getWorkbenchContext(), null,
                new Object[]{Pattern.compile("^###COMMENT###$"),
                             Pattern.compile("^//"),
                             Pattern.compile("^--"),
                             Pattern.compile("^\\$"),
                             Pattern.compile("^#(?!(FID|X)[\t,;\\| ])")}));

        FieldComponentFactoryRegistry.setFactory(
            wcontext,
            "FieldSeparatorChooser",
            new ComboBoxFieldComponentFactory(
                context.getWorkbenchContext(),
                null,
                new Object[]{TABULATION,COMMA,SEMI_COLUMN,PIPE,WHITESPACE}));
        
        FieldComponentFactoryRegistry.setFactory(
            wcontext,
            "HeaderLineOption",
            new CheckBoxFieldComponentFactory(
                context.getWorkbenchContext(),
                null));
        
        FieldComponentFactoryRegistry.setFactory(
            wcontext,
            "DataTypeLineOption",
            new CheckBoxFieldComponentFactory(
                context.getWorkbenchContext(),
                null));
        
        FieldComponentFactoryRegistry.setFactory(wcontext,
            "x column", new ComboBoxFieldComponentFactory(
                wcontext, null, 
                new String[]{"1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","-"}));
        FieldComponentFactoryRegistry.setFactory(wcontext,
            "y column", new ComboBoxFieldComponentFactory(
                wcontext, null, 
                new String[]{"1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","-"}));
        FieldComponentFactoryRegistry.setFactory(wcontext,
            "z column", new ComboBoxFieldComponentFactory(
                wcontext, null, 
                new String[]{"-", "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"}));
        
        FieldComponentFactoryRegistry.setFactory(wcontext,
            "wkt column", new ComboBoxFieldComponentFactory(
                wcontext, null, 
                new String[]{"1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"}));


        ////////////////////////////////////////////////////////////////////////
        // Create configurable csv File Loader for XYZ puntal data
        ////////////////////////////////////////////////////////////////////////
        List<String> csvExtensions = new ArrayList<String>();
        csvExtensions.add("txt");
        csvExtensions.add("xyz");
        csvExtensions.add("csv");

        DataSourceFileLayerLoader csvOptionsFileLoader = new DataSourceFileLayerLoader(
            wcontext, CSVDataSource.class, "csv (set options)", csvExtensions){

            protected Map<String,Object> toProperties(URI uri, Map<String,Object> options) {
                return super.toProperties(uri, toProperties(options));
            }

            // [mmichaud 2013-11-07] change internationalized options to fixed keys properties
            // This is necessary to use persist CSVDataSource in the project file
            private Map<String,Object> toProperties(Map <String,Object> options) {
                Map<String,Object> properties = new HashMap<String,Object>();
                properties.put(CSVDataSource.CHARSET, options.get(I18NPlug.getI18N("drivers.csv.encoding")));
                properties.put(CSVDataSource.COMMENT_LINE_PATTERN, options.get(I18NPlug.getI18N("drivers.csv.comment-line-pattern")));
                properties.put(CSVDataSource.FIELD_SEPARATOR, options.get(I18NPlug.getI18N("drivers.csv.field-separator")));
                properties.put(CSVDataSource.HEADER_LINE, options.get(I18NPlug.getI18N("drivers.csv.header-line")));
                properties.put(CSVDataSource.DATA_TYPE_LINE, options.get(I18NPlug.getI18N("drivers.csv.data-type-line")));
                properties.put(CSVDataSource.X_COLUMN, options.get(I18NPlug.getI18N("drivers.csv.x")));
                properties.put(CSVDataSource.Y_COLUMN, options.get(I18NPlug.getI18N("drivers.csv.y")));
                properties.put(CSVDataSource.Z_COLUMN, options.get(I18NPlug.getI18N("drivers.csv.z")));
                properties.put(CSVDataSource.WKT_COLUMN, options.get(I18NPlug.getI18N("drivers.csv.wkt")));
                return properties;
            }
        };
 
        csvOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.encoding"), 
                "CharsetChooser", Charset.defaultCharset(), true);
        csvOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.comment-line-pattern"), 
                "CommentLinePatternChooser", "^###COMMENT###$", true);
        csvOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.field-separator"),
                "FieldSeparatorChooser", "{tab}", true);
        csvOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.header-line"), 
                "HeaderLineOption", Boolean.TRUE, true);
        csvOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.data-type-line"),
                "DataTypeLineOption", Boolean.FALSE, true);
        csvOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.x"), 
                "x column", "1", true);
        csvOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.y"), 
                "y column", "2", true);
        csvOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.z"), 
                "z column", "-", false);
        
        wcontext.getRegistry().createEntry(FileLayerLoader.KEY, csvOptionsFileLoader);
        
        ////////////////////////////////////////////////////////////////////////
        // Create configurable csv File Loader for WKT data
        ////////////////////////////////////////////////////////////////////////
        List<String> wktExtensions = new ArrayList<String>();
        wktExtensions.add("wkt");

        DataSourceFileLayerLoader wktOptionsFileLoader = new DataSourceFileLayerLoader(
            wcontext, CSVDataSource.class, "wkt (set options)", wktExtensions) {

            protected Map<String,Object> toProperties(URI uri, Map<String,Object> options) {
                return super.toProperties(uri, toProperties(options));
            }

            // [mmichaud 2013-11-07] change internationalized options to fixed keys properties
            // This is necessary to use persist CSVDataSource in the project file
            private Map<String,Object> toProperties(Map <String,Object> options) {
                Map<String,Object> properties = new HashMap<String,Object>();
                properties.put(CSVDataSource.CHARSET, options.get(I18NPlug.getI18N("drivers.csv.encoding")));
                properties.put(CSVDataSource.COMMENT_LINE_PATTERN, options.get(I18NPlug.getI18N("drivers.csv.comment-line-pattern")));
                properties.put(CSVDataSource.FIELD_SEPARATOR, options.get(I18NPlug.getI18N("drivers.csv.field-separator")));
                properties.put(CSVDataSource.HEADER_LINE, options.get(I18NPlug.getI18N("drivers.csv.header-line")));
                properties.put(CSVDataSource.DATA_TYPE_LINE, options.get(I18NPlug.getI18N("drivers.csv.data-type-line")));
                properties.put(CSVDataSource.X_COLUMN, options.get(I18NPlug.getI18N("drivers.csv.x")));
                properties.put(CSVDataSource.Y_COLUMN, options.get(I18NPlug.getI18N("drivers.csv.y")));
                properties.put(CSVDataSource.Z_COLUMN, options.get(I18NPlug.getI18N("drivers.csv.z")));
                properties.put(CSVDataSource.WKT_COLUMN, options.get(I18NPlug.getI18N("drivers.csv.wkt")));
                return properties;
            }
        };
 
        wktOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.encoding"), 
                "CharsetChooser", Charset.defaultCharset(), true);
        wktOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.comment-line-pattern"), 
                "CommentLinePatternChooser", "^###COMMENT###$", true);
        wktOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.field-separator"), 
                "FieldSeparatorChooser", "{tab}", true);
        wktOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.header-line"), 
                "HeaderLineOption", Boolean.TRUE, true);
        wktOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.data-type-line"),
                "DataTypeLineOption", Boolean.FALSE, true);
        wktOptionsFileLoader.addOption(I18NPlug.getI18N("drivers.csv.wkt"), 
                "wkt column", "1", true);
        
        wcontext.getRegistry().createEntry(FileLayerLoader.KEY, wktOptionsFileLoader);
        
        
        ////////////////////////////////////////////////////////////////////////
        // Create Automatic CSV Loader
        ////////////////////////////////////////////////////////////////////////
        csvExtensions.add("wkt");

        DataSourceFileLayerLoader csvAutoFileLoader = new DataSourceFileLayerLoader(
            wcontext, CSVDataSource.class, "csv (auto)", csvExtensions) {

            protected Map<String,Object> toProperties(URI uri, Map<String,Object> options) {

                Map<String,Object> properties = super.toProperties(uri, new HashMap<String, Object>());
                try {
                    final CSVFile csvFile = new AutoCSVFile((String)properties.get(DataSource.FILE_KEY), (String)properties.get("CompressedFile"));
                    properties.put("CSV_FILE", csvFile);
                } catch(IOException e) {
                    e.printStackTrace();
                } catch(CSVFileException e) {
                    e.printStackTrace();
                }
                return properties;
            }
        };
        wcontext.getRegistry().createEntry(FileLayerLoader.KEY, csvAutoFileLoader);

        
        ////////////////////////////////////////////////////////////////////////
        // Create XYZ format saver
        ////////////////////////////////////////////////////////////////////////
        SaveCSVFileDataSourceQueryChooser saveCSVFileDataSourceQC =
            new SaveCSVFileDataSourceQueryChooser(
                CSVDataSource.class,
                "csv",
                new String[]{"csv", "txt", "xyz"},
                wcontext);
        
        DataSourceQueryChooserManager.get(wcontext.getBlackboard())
            .addSaveDataSourceQueryChooser(saveCSVFileDataSourceQC);

    }

    
    public static Charset[] createCommonCharsetArray() {
        // http://stackoverflow.com/questions/8509339/what-is-the-most-common-encoding-of-each-language
        // http://www.w3.org/International/O-charset-lang.html
        SortedMap<String,Charset> availableCharsets = Charset.availableCharsets();
        List<Charset> charsets = new ArrayList<Charset>();
        String[] charsetNames = new String[]{
            "windows-1252", 
            "UTF-8",
            "ISO-8859-1",
            "ISO-8859-2",
            "ISO-8859-5",
            "ISO-8859-6",
            "ISO-8859-9",
            "ISO-8859-15",
            "x-MacRoman",
            "IBM850",
            "EUC-JP",
            "GB2312",
            "Shift_JIS",
            "windows-1250",
            "windows-1251" 
        };
        for (String name : charsetNames) {
            if (availableCharsets.containsKey(name)) {
                charsets.add(availableCharsets.get(name));
            }
        }
        return charsets.toArray(new Charset[charsets.size()]);
    }

}