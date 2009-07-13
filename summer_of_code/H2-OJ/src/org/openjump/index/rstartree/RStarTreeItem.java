package org.openjump.index.rstartree;

import com.vividsolutions.jts.geom.Envelope;

public class RStarTreeItem {

	  private Envelope envelope;
	  private Object item;

	  public RStarTreeItem(Envelope envelope, Object item) {
	    this.envelope = envelope;
	    this.item = item;
	  }

	  public Envelope getEnvelope() {
	    return envelope;
	  }

	  public Object getItem() { return item; }
}
