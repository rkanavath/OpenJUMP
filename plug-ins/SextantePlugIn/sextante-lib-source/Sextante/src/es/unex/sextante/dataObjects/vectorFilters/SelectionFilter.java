package es.unex.sextante.dataObjects.vectorFilters;

import java.util.BitSet;

import es.unex.sextante.dataObjects.IFeature;

/**
 * A filter to use only selected features, in case the underlying library supports selection (usually graphical selection from a
 * GIS)
 * 
 * @author volaya
 * 
 */
public class SelectionFilter
         implements
            IVectorLayerFilter {

   private final BitSet m_BitSet;


   public SelectionFilter(final BitSet bitset) {

      m_BitSet = bitset;

   }


   /**
    * Returns the ID of the last feature in the dataset for which this filter is "true"
    */
   public int getLast() {
	      return ( m_BitSet.length() );
   }   

   
   @Override
   public boolean accept(final IFeature feature,
                         final int iIndex) {

      if (m_BitSet.cardinality() == 0) {
         return true;
      }
      return m_BitSet.get(iIndex);

   }


}
