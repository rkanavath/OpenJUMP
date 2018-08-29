package com.geomaticaeambiente.openjump.klem.parallel;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 *
 * @author AdL
 */
public class ExecutorBuilder {
    
    public ExecutorBuilder(int rowCount) {
        
        int threadCount = 1; //Runtime.getRuntime().availableProcessors();
        executorService = Executors.newFixedThreadPool(threadCount);
        rowPerStripeCount = (int) rowCount / threadCount;
        if(rowPerStripeCount == 0) {
            rowPerStripeCount = 1;
        }
        stripeCount = (int) Math.ceil((double) rowCount / rowPerStripeCount);

    }

    public ExecutorService getExecutorService() {
        return executorService;
    }
    
    public int getRowPerStripeCount() {
        return rowPerStripeCount;
    }

    public int getStripeCount() {
        return stripeCount;
    }

    private final ExecutorService executorService;
    private int rowPerStripeCount;
    private final int stripeCount;
    
}
