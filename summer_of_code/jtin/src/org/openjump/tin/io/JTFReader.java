package org.openjump.tin.io;

import org.openjump.tin.ImmutableTin;
import org.openjump.tin.io.JTFLayout;

import com.vividsolutions.jts.geom.Coordinate;

import java.io.InputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.util.ArrayList;
import java.util.List;


/**
 * Reads in a JTF binary file.
 * 
 * @author paradox
 *
 */
public class JTFReader {

	public JTFReader() {
	}
	
	public static ImmutableTin read (InputStream in) throws IOException {
		
		byte[] tmpByteArray;
		ByteBuffer bbuf;
		int numVertices, numTriangles, numBreaklines, numBoundaries, dataStart, 
			srid, tmpByteArraySize;
		String marker;
		Coordinate[] vertices;
		int[][] triTable;
		List<int[]> breaklines, boundaries;
		double tmpX, tmpY, tmpZ;
		
		try {
			// read in file marker
			tmpByteArray = new byte[JTFLayout.MARKER_BYTE_SIZE];
			char[] tmpCharArray = new char[tmpByteArray.length / Character.SIZE];
			
			if (in.read(tmpByteArray) != JTFLayout.MARKER_BYTE_SIZE)
				throw new IOException("JTFReader.read(): read in marker size doesn't equal "+JTFLayout.MARKER_BYTE_SIZE+" bytes.");
			bbuf = ByteBuffer.wrap(tmpByteArray);
			CharBuffer charBuf = bbuf.asCharBuffer();
			marker = bbuf.asCharBuffer().toString();
			System.out.println("JTFReader.read(): marker = "+marker+".");
			if (!marker.equals(JTFLayout.FILE_MARKER))
				throw new IOException("JTFReader.read(): unsupported file version: "+marker+".");
			
			// read in the header int fields
			tmpByteArraySize = JTFLayout.NUM_HEADER_INT_FIELDS * Integer.SIZE  / Byte.SIZE;
			tmpByteArray = new byte[tmpByteArraySize]; 
			if (in.read(tmpByteArray) != tmpByteArraySize)
				throw new IOException("JTFReader.read(): read in header fileds doesn't equal "+tmpByteArraySize+" bytes in size.");
			bbuf = ByteBuffer.wrap(tmpByteArray);
			numVertices = bbuf.getInt();
			numTriangles = bbuf.getInt();
			numBreaklines = bbuf.getInt();
			numBoundaries = bbuf.getInt();
			dataStart = bbuf.getInt();
			srid = bbuf.getInt();
			
			// initialize the large data structures
			vertices = new Coordinate[numVertices];
			triTable = new int[numTriangles][6];
			breaklines = new ArrayList<int[]>(numBreaklines);
			boundaries = new ArrayList<int[]>(numBoundaries);
			
			// read in the vertices
			tmpByteArraySize = numVertices * JTFLayout.NUM_COORDINATES_PER_VERTEX * Double.SIZE / Byte.SIZE;
			tmpByteArray = new byte[tmpByteArraySize];   
			if (in.read(tmpByteArray) != tmpByteArraySize)
				throw new IOException("JTFReader.read(): read in vertex array doesn't equal "+tmpByteArraySize+" bytes in size.");
			bbuf = ByteBuffer.wrap(tmpByteArray);
			for (int i=0; i < numVertices; i++) {
				tmpX = bbuf.getDouble();
				tmpY = bbuf.getDouble();
				tmpZ = bbuf.getDouble();
				vertices[i] = new Coordinate(tmpX, tmpY, tmpZ);
			}
			
			// read in the triangle table
			tmpByteArraySize = numTriangles * JTFLayout.NUM_TRIANGLE_INT_FIELDS * Integer.SIZE / Byte.SIZE;
			tmpByteArray = new byte[tmpByteArraySize]; 
			if (in.read(tmpByteArray) != tmpByteArraySize)
				throw new IOException("JTFReader.read(): read in triangle table doesn't equal "+tmpByteArraySize+" bytes in size.");
			bbuf = ByteBuffer.wrap(tmpByteArray);			
			for (int i=0; i < numTriangles; i++) 
				for (int j=0; j < JTFLayout.NUM_TRIANGLE_INT_FIELDS; j++)
					triTable[i][j] = bbuf.getInt();
			
			// read in breaklines
			for (int i=0; i<numBreaklines; i++) 
				breaklines.add(i, readLineArray(in));
			
			// read in Boundaries
			for (int i=0; i<numBoundaries; i++) 
				boundaries.add(i, readLineArray(in));
		}
		finally {
			in.close();
		}
		
		return new ImmutableTin(vertices, triTable, breaklines, boundaries, srid);
	}
	
	
	/**
	 * 
	 * @param in: the input stream. First integer of the stream is the number
	 * 			of following points. Each point is an integer that represents
	 * 			an index to an external array of points.
	 * @return an array of integers describing the line
	 */
	private static int[] readLineArray (InputStream in) throws IOException{
		int tmpLineLength, bytesPerInt;
		int[] tmpLine;
		byte[] tmpByteArray;
		ByteBuffer bbuf;
		
		bytesPerInt = Integer.SIZE / Byte.SIZE;
		tmpByteArray = new byte[bytesPerInt];
		if (in.read(tmpByteArray) != bytesPerInt)
			throw new IOException("JTFReader.readLineArray(in): read in line length doesn't equal "+bytesPerInt+" bytes in size.");
		tmpLineLength = ByteBuffer.wrap(tmpByteArray).getInt();
		tmpLine = new int[tmpLineLength];
		
		tmpByteArray = new byte[bytesPerInt*tmpLineLength];
		if (in.read(tmpByteArray) != bytesPerInt*tmpLineLength)
			throw new IOException("JTFReader.readLineArray(in): read in line doesn't equal "+bytesPerInt*tmpLineLength+" bytes in size.");
		bbuf = ByteBuffer.wrap(tmpByteArray);			
		for (int j=0; j<tmpLineLength; j++) 
			tmpLine[j] = bbuf.getInt();
		
		return tmpLine;
	}
	
}
