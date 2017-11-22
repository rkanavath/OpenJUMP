package com.cadplan.jump;

import java.text.DecimalFormat;

public class NumberFormatter
{
   public static String format(double val)
   {
	        double val0 = val;
	        DecimalFormat df;
	        String result = "";
	        if( Math.abs(val) <= 100000000.0 && Math.abs(val) >= 0.001) 
	        {
	        	val = Math.round(val*10000.0) / 10000.0;
	        	if((val) % 1 == 0 ) result = String.valueOf((int)(val));
	        	else result =  String.valueOf(val);	        	
	        }
	        else
	        {
	           df = new DecimalFormat("0.0000E0");
	           result = df.format(val);
	        }
	        //System.out.println("Formatting: val="+val0+" result="+result);
	        return result;
	    
   }
}
