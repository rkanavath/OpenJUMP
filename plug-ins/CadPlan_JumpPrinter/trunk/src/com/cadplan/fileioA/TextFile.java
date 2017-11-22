package com.cadplan.fileioA;

import java.io.*;
import java.util.Properties;

/**
 * User: geoff
 * Date: 18/10/2006
 * Time: 09:49:10
 * Copyright 2005 Geoffrey G Roy.
 */
public class TextFile
{
    String fileName;
    String dirName;
    BufferedReader reader = null;
    FileWriter writer = null;

    public TextFile(String dirName, String fileName)
    {
         this.dirName = dirName;
         Properties props = System.getProperties();
         if(dirName == null) this.dirName = props.getProperty("user.dir");
         this.fileName = fileName;
    }

    public boolean exists()
    {
        File file = new File(dirName+File.separator+fileName);
        return file.exists();
    }

    public boolean openRead()
    {
        File file = new File(dirName+File.separator+fileName);

        try
        {
            reader = new BufferedReader(new FileReader(file));
            return true;
        }
        catch (FileNotFoundException ex)
        {
             return false;
        }

    }

    public boolean openWrite()
    {
        File file = new File(dirName+File.separator+fileName);

        try
        {
            writer = new FileWriter(file);
            return true;
        }
        catch (IOException ex)
        {
             return false;
        }
    }

    public String readLine()
    {
        try
        {
            return reader.readLine();
        }
        catch (IOException ex)
        {
           return null;
        }
    }

    public String readAll()
    {
        String line = "";
        StringBuffer sb = new StringBuffer();
        try
        {
            while (line != null)
            {
                line = reader.readLine();
                if(line != null) sb.append(line+"\n");
            }
        }
        catch (IOException ex)
        {
            return sb.toString();
        }
        return sb.toString();
    }

    public boolean write(String s)
    {
        try
        {
            writer.write(s);
            return true;
        }
        catch (IOException ex)
        {
            return false;
        }
    }

    public void close()
    {   try
        {
            if(reader != null) reader.close();
            else if(writer != null)
            {
                writer.flush();
                writer.close();
            }
        }
        catch (IOException ex)
        {

        }
    }
}
