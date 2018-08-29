package com.geomaticaeambiente.openjump.klem.flowdir;

/**
 *
 * @author AdL
 */
public class BitOps {
    
    public static byte setBitValue(byte theByte, int bitPos, boolean bitValue) {
        theByte = (byte) (bitValue ? theByte | (1 << bitPos) : theByte & ~(1 << bitPos));
        return theByte;
    }
    
    public static byte setBitValues(byte theByte, boolean[] values) {
        for (int i=0; i<8; i++) {
            theByte = (byte) (values[i] ? theByte | (1 << i) : theByte & ~(1 << i));
        }
        return theByte;
    }
    
    public static boolean readBitValue(byte theByte, int pos) {
        
        int b = ((theByte >> pos) & 1);
        return b == 1;
        
    }
    
    public static boolean[] readBitValues(byte theByte) {
        
        boolean[] values = new boolean[8];
        for (int i = 0; i < 8; ++i) {
            int b = ((theByte >> i) & 1);
            values[i] = b == 1;
        }
        return values;
        
    }
    
    
}
