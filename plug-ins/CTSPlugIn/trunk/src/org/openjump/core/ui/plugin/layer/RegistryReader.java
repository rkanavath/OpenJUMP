package org.openjump.core.ui.plugin.layer;

import java.io.*;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Created by Michaël on 01/12/14.
 */
public class RegistryReader {

    static Map<String,String> read(String registry) throws IOException {
        Map<String,String> map = new LinkedHashMap<String,String>();
        InputStream is = null;
        try {
            is = org.cts.registry.Registry.class.getResourceAsStream(registry.toLowerCase());
            BufferedReader br = new BufferedReader(new InputStreamReader(is));
            String line, name = null, code;
            while (null != (line=br.readLine())) {
                if (line.startsWith("#")) {
                    name = line.substring(1).trim();
                } else if (line.startsWith("<")) {
                    code = line.substring(1, line.indexOf(">"));
                    map.put(code, code);
                    if (name != null && name.length() > 0) {
                        map.put(name, code);
                    }
                    name = null;
                } else {
                    name = null;
                }
            }
        } finally {
            if (is != null) try {is.close();} catch(IOException e){}
        }
        return map;
    }
}
