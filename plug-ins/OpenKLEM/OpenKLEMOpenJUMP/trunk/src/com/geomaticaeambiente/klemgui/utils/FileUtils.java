package com.geomaticaeambiente.klemgui.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.channels.FileChannel;

/**
 *
 * @author AdL
 */
public class FileUtils {
    
    public static void copy(File in, File out) throws IOException {
        FileChannel inChannel = new
            FileInputStream(in).getChannel();
        FileChannel outChannel = new
            FileOutputStream(out).getChannel();
        try {
            inChannel.transferTo(0, inChannel.size(),
                    outChannel);
        } 
        catch (IOException e) {
            throw e;
        }
        finally {
            if (inChannel != null) inChannel.close();
            if (outChannel != null) outChannel.close();
        }
    }
    
    public static File stream2file (InputStream in, String prefix, String suffix) throws IOException {
        final File tempFile = File.createTempFile(prefix, suffix);
        tempFile.deleteOnExit();
        
        FileOutputStream out = new FileOutputStream(tempFile);
        FileUtils.copy(in, out);
        
        return tempFile;
    }
    
     private static final int BUF_SIZE = 0x1000; // 4K

    public static long copy(InputStream from, OutputStream to) throws IOException {
        byte[] buf = new byte[BUF_SIZE];
        long total = 0;
        while (true) {
            int r = from.read(buf);
            if (r == -1) {
                break;
            }
            to.write(buf, 0, r);
            total += r;
        }
        from.close();
        to.close();
        return total;
  }
    
}
