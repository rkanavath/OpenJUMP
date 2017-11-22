/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * Copyright (C) 2006 Cadplan
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 */
package com.cadplan.jump;

import java.io.File;

/**
 * User: geoff
 * Date: 7/01/2007
 * Time: 08:18:33
 * Copyright 2005 Geoffrey G Roy.
 */
public class LayerInfo
{
    public int nameIndexStart;
    public int nameIndexEnd;
    public int locationIndexStart;
    public int locationIndexEnd;
    public int descriptionIndexStart;
    public int descriptionIndexEnd;
    public String name;
    public String dirName;
    public String fileName;
    public String description;
    public int status ;    //  0:unknown, 1:true , 2:false
    public String type;

    public LayerInfo(String name, String description, String dirName,  String fileName, int nameIndexStart, int nameIndexEnd,
                     int descriptionIndexStart, int descriptionIndexEnd,
                     int locationIndexStart, int locationIndexEnd, int status, String type)
    {
        this.name = name;
        this.dirName = dirName;
        this.fileName = fileName;
        this.description = description;
        this.nameIndexStart = nameIndexStart;
        this.nameIndexEnd = nameIndexEnd;
        this.locationIndexStart = locationIndexStart;
        this.locationIndexEnd = locationIndexEnd;
        this.descriptionIndexStart = descriptionIndexStart;
        this.descriptionIndexEnd = descriptionIndexEnd;
        this.status = status;
        this.type = type;
    }

    public String toString()
    {
        return name+"["+nameIndexStart+","+nameIndexEnd+"] "+description+"["+descriptionIndexStart+","+descriptionIndexEnd+"] "+dirName+ File.separator+fileName
                +"["+locationIndexStart+","+locationIndexEnd+"] status:"+status;
    }
}
