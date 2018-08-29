package com.geomaticaeambiente.openjump.klem.parallel2;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import java.util.concurrent.Callable;

/**
 *
 * @author AdL
 */
public abstract class AbstractStripe2 implements Callable<DoubleBasicGrid> {

    protected abstract DoubleStripeGrid2[] getDoubleStripeGridsToUpdate();
    protected abstract IntStripeGrid2[] getIntStripeGridsToUpdate(); 
    protected abstract ByteStripeGrid2[] getByteStripeGridsToUpdate();
    
    protected abstract void plugUpdatedDoubleStripegrids(DoubleStripeGrid2[] newDoubleStripeGrids);
    protected abstract void plugUpdatedIntStripegrids(IntStripeGrid2[] newIntStripeGrids);
    protected abstract void plugUpdatedByteStripegrids(ByteStripeGrid2[] newByteStripeGrids);
    
}
