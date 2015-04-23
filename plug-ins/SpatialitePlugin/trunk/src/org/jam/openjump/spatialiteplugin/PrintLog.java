/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI 
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * JUMP is Copyright (C) 2003 Vivid Solutions
 *
 * This program implements extensions to JUMP and is
 * Copyright (C) 2010 Jorge Almaraz.
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
 * For more information, contact:
 *
 * Jukka Rahkonen
 * jukka.rahkonen@latuviitta.fi
 * 
 */
package org.jam.openjump.spatialiteplugin;

import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import java.io.OutputStream;

public class PrintLog extends OutputStream {

	private PlugInContext ctx=null;
	
	public PrintLog(PlugInContext context){
		super();
		ctx=context;
	}

	public void write(byte[] b){
		if (b==null) return;
		ctx.getWorkbenchFrame().getOutputFrame().addText(new String(b));
	}

	public void write(byte[] b, int off, int len){
		if (b==null) return;
		ctx.getWorkbenchFrame().getOutputFrame().addText(new String(b,off,len));
	}
	
	public void write(int b){
		byte[] b1 = {(new Integer(b)).byteValue()};
		this.write(b1);
	}

}
