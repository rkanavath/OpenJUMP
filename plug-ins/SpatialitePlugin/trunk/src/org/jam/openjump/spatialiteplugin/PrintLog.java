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
