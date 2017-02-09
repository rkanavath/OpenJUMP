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

public class CSVFileException extends Exception {
    
    public static final String DRIVER_NOT_CONFIGURED = I18NPlug.getI18N("drivers.csv.driver-not-fully-configured");
    public static final String ERROR_READING = I18NPlug.getI18N("drivers.csv.error-reading");
    public static final String NO_DATA_FOUND = I18NPlug.getI18N("drivers.csv.no-data-found");

    public CSVFileException(String message) {
        super(message);
    }
    
    public CSVFileException(String message, CSVFile file) {
        super(message + "\n" + file.getFilePath());
    }

}
