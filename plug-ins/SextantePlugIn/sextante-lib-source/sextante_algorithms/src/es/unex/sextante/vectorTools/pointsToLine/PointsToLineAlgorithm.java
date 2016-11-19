package es.unex.sextante.vectorTools.pointsToLine;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

import es.unex.sextante.additionalInfo.AdditionalInfoNumericalValue;
import es.unex.sextante.additionalInfo.AdditionalInfoVectorLayer;
import es.unex.sextante.core.GeoAlgorithm;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.dataObjects.IFeature;
import es.unex.sextante.dataObjects.IFeatureIterator;
import es.unex.sextante.dataObjects.IVectorLayer;
import es.unex.sextante.exceptions.GeoAlgorithmExecutionException;
import es.unex.sextante.exceptions.OptionalParentParameterException;
import es.unex.sextante.exceptions.RepeatedParameterNameException;
import es.unex.sextante.exceptions.UndefinedParentParameterNameException;
import es.unex.sextante.outputs.OutputVectorLayer;

public class PointsToLineAlgorithm
extends
GeoAlgorithm {

	public static final String LAYER  = "LAYER";
	public static final String FIELD_GROUP  = "FIELD_GROUP";
	public static final String FIELD_SORT  = "FIELD_SORT";   
	public static final String MODE  = "MODE";
	public static final String VERTICES  = "VERTICES";
	public static final String DIST  = "DIST";      
	public static final String RESULT = "RESULT";
	public static final String SORTED  = "SORTED";
	public static final String REVERSE  = "REVERSE";
	public static final String DATE  = "DATE";
	public static final String FORMAT  = "FORMAT";
	//--
	public static final String MODE_ALL  = Sextante.getText("pts2line_mode_all");
	public static final String MODE_ID  = Sextante.getText("pts2line_mode_group");
	public static final String MODE_DIST  = Sextante.getText("pts2line_mode_dist");
	public static final String MODE_NPTS  = Sextante.getText("pts2line_mode_npts");	
	//--
	private IVectorLayer       	m_Layer;
	private IVectorLayer       	m_Output;
	private IFeatureIterator	m_Iter = null;
	private int 				m_LastPos = -1;
	private boolean				m_DateField = false;
	private String				m_DateFormat = null;

	
	//Class to manage the coordinates, attributes and (if applicable)
	//sort field (numeric or string type) contents of one line vertex (=point).
	private class Vertex implements Comparable<Object> {
		private double X;
		private double Y;
		private double Z;
		private Object[] atts = null;
		private Double numVal = null;
		private String strVal = null;
		private boolean visited = false;
		
		
		public Vertex ( double x, double y, double z, Object[] atts ) {
			make ( x, y, z, atts );
		}
		
		public Vertex ( double val, double x, double y, double z, Object[] atts ) {
			make ( x, y, z, atts );
			setNumVal(new Double (val));
		}
		
		public Vertex ( String val, double x, double y, double z, Object[] atts ) {
			make ( x, y, z, atts );
			setStrVal(new String (val));
		}
		
		private void make ( double x, double y, double z, Object[] atts ) {
			setX(x);
			setY(y);
			setZ(z);
			setAtts (atts);
			setNumVal(null);
			setStrVal(null);
			setVisited ( false );
		}
		
		@Override
		public int compareTo(Object arg0) {
			if ( m_DateField ) {				
				if ( getStrVal() != null && m_DateFormat != null && m_DateFormat.length() > 0 ) {
					//Sort by date field, using given date format.
					try {
						Date d1 = new SimpleDateFormat(m_DateFormat).parse(this.getStrVal());
						Date d2 = new SimpleDateFormat(m_DateFormat).parse(((Vertex)arg0).getStrVal());
						return ( d1.compareTo(d2));
					} catch (ParseException e) {
						return ( 0 );
					}
					
				}				
			} else {
				if ( getNumVal() != null ) {
					return ( this.getNumVal().compareTo(((Vertex)arg0).getNumVal()));
				}
				if ( getStrVal() != null ) {
					return ( this.getStrVal().compareTo(((Vertex)arg0).getStrVal()));
				}
			}
			//Cannot/do not wish to sort:
			return (0);
		}

		public double getX() {
			return X;
		}

		public void setX(double x) {
			X = x;
		}

		public double getY() {
			return Y;
		}

		public void setY(double y) {
			Y = y;
		}

		public double getZ() {
			return Z;
		}

		public void setZ(double z) {
			if ( Double.isNaN(z) || Double.isInfinite(z) ) {
				Z = 0.0;
			} else {
				Z = z;
			}
		}

		public Object[] getAtts() {
			return atts;
		}

		public void setAtts(Object[] atts) {
			this.atts = atts;
		}

		public boolean isVisited() {
			return visited;
		}

		public void setVisited(boolean visited) {
			this.visited = visited;
		}

		private Double getNumVal() {
			return numVal;
		}

		private void setNumVal(Double numVal) {
			this.numVal = numVal;
		}

		private String getStrVal() {
			return strVal;
		}

		private void setStrVal(String strVal) {
			this.strVal = strVal;
		}
		
	}


	/** 
	 * Seeks to the feature specified by an integer position (index)
	 * value. This method updates a global feature iterator.
	 * If the given position is past the current iterator position,
	 * then the iterator will simply be advanced. If it is before
	 * the current position, then the iterator must be reset to the
	 * start of the layer and advanced until it has reached the
	 * desired position.
	 * 
	 * @param pos Index (0..n) of feature to which to seek.
	 * @return The feature at "pos".
	 * @throws GeoAlgorithmExecutionException
	 */
	private IFeature seek ( int pos ) throws GeoAlgorithmExecutionException {
		if ( pos < 0 ) {
			throw new GeoAlgorithmExecutionException(Sextante.getText("feature_err_seek_illegal_pos"));
		}
		if ( m_Iter == null ) {
			m_Iter = m_Layer.iterator();
		}
		if ( pos > m_LastPos ) {
			//Seek forward.
			IFeature feature = null;
			while ( m_LastPos < pos ) {
				if ( !m_Iter.hasNext() ) {
					throw new GeoAlgorithmExecutionException(Sextante.getText("feature_err_seek_past_end"));
				}
				feature = m_Iter.next();
				m_LastPos ++;
			}
			return ( feature );
		} else {
			//Reset iterator, then seek forward.
			m_Iter = m_Layer.iterator();
			m_LastPos = -1;
			return ( seek ( pos ) );
		}
	}


	@Override
	public void defineCharacteristics() {

		setName(Sextante.getText("Points_to_line"));
		setGroup(Sextante.getText("Tools_for_point_layers"));
		setUserCanDefineAnalysisExtent(false);

		try {
			m_Parameters.addInputVectorLayer(LAYER, Sextante.getText("Points"), AdditionalInfoVectorLayer.SHAPE_TYPE_POINT, true);			
			m_Parameters.addTableField(FIELD_GROUP, Sextante.getText("pts2line_field_group"), "LAYER", false);
			m_Parameters.addTableField(FIELD_SORT, Sextante.getText("pts2line_field_sort"), "LAYER", false);
			m_Parameters.addSelection(MODE, Sextante.getText("pts2line_mode"), new String[] { MODE_ALL, MODE_ID, MODE_DIST, MODE_NPTS });
			m_Parameters.addNumericalValue(VERTICES, Sextante.getText("pts2line_num_vertices"), AdditionalInfoNumericalValue.NUMERICAL_VALUE_INTEGER, 2, 2, (double)Integer.MAX_VALUE);
			m_Parameters.addNumericalValue(DIST, Sextante.getText("pts2line_dist"), AdditionalInfoNumericalValue.NUMERICAL_VALUE_DOUBLE, 1.0, 0.0, Double.MAX_VALUE);
			m_Parameters.addBoolean(SORTED, Sextante.getText("pts2line_set_sorted"), false);
			m_Parameters.addBoolean(REVERSE, Sextante.getText("pts2line_set_reverse"), false);
			m_Parameters.addBoolean(DATE, Sextante.getText("pts2line_set_sorted_by_date"), false);
			m_Parameters.addString(FORMAT, Sextante.getText("pts2line_date_format"), "yyyy-MM-dd HH:mm:ss.S");			
			addOutputVectorLayer(RESULT, Sextante.getText("Line"), OutputVectorLayer.SHAPE_TYPE_LINE);
		}
		catch (final RepeatedParameterNameException | UndefinedParentParameterNameException | OptionalParentParameterException e) {
			Sextante.addErrorToLog(e);
		}

	}


	private void checkParameters( int shpCount, String mode, int nPoints, double dist )
			throws GeoAlgorithmExecutionException
	{

		//ALL MODES
		if (shpCount < 2) {
			throw new GeoAlgorithmExecutionException(Sextante.getText("pts2line_err_points_in_lt_2"));
		}

		//MODE: DIST
		if ( mode.equals(MODE_DIST)) {
			if ( dist <= 0.0 ) {
				throw new GeoAlgorithmExecutionException(Sextante.getText("pts2line_err_dist_lt_or_eq_0"));
			}
		}

		//MODE: NPOINTS
		if ( mode.equals(MODE_NPTS)) {
			if ( shpCount < nPoints ) {
				throw new GeoAlgorithmExecutionException(Sextante.getText("pts2line_err_points_in_lt_npoints"));
			}
		}

	}


	//TODO: Needs to handle lat/lon geodesic distances!
	private double dist ( Vertex v1, Vertex v2 )
	{
		double sum = 0;

		final double x1 = v1.getX();
		final double y1 = v1.getY();
		final double z1 = v1.getZ();		
		final double x2 = v2.getX();
		final double y2 = v2.getY();
		final double z2 = v2.getZ();
		
		sum += Math.pow ((x1-x2),2.0);
		sum += Math.pow ((y1-y2),2.0);
		sum += Math.pow ((z1-z2),2.0);
		
		return (Math.sqrt(sum));
	}

	
	private Coordinate vertex2coord ( Vertex v )
	{	
		final double x = v.getX();
		final double y = v.getY();
		final double z = v.getZ();
		final Coordinate coord = new Coordinate ( x, y, z );
		return ( coord );
	}


	@Override
	public boolean processAlgorithm() throws GeoAlgorithmExecutionException
	{
		int iShapeCount = 0;
		int numPoints = 0;
		double tDist = 0.0;
		String mode = null;
		boolean sort = false;
		boolean reverse = false;
		int field_id = -1;
		int field_sort = -1;
		boolean field_id_numeric = false;
		boolean field_sort_numeric = false;
		//-
		IFeature feature = null;		

		numPoints = m_Parameters.getParameterValueAsInt(VERTICES);
		tDist = m_Parameters.getParameterValueAsDouble(DIST);
		mode = m_Parameters.getParameterValueAsString(MODE);
		sort = m_Parameters.getParameterValueAsBoolean(SORTED);
		reverse = m_Parameters.getParameterValueAsBoolean(REVERSE);		
		if ( reverse ) {
			sort = true; //"Sort first" assumed "true" if "Reverse sort" selected.
		}
		m_DateField = m_Parameters.getParameterValueAsBoolean(DATE);
		if ( m_DateField ) {
			sort = true; //"Sort first" assumed "true" if "Sort by date/time" selected.
		}
		m_DateFormat = m_Parameters.getParameterValueAsString(FORMAT);
		
		m_Layer = m_Parameters.getParameterValueAsVectorLayer(LAYER);
		iShapeCount = m_Layer.getShapesCount();
		try {
			field_id = m_Parameters.getParameterValueAsInt(FIELD_GROUP);			
			field_id_numeric = Number.class.isAssignableFrom(m_Layer.getFieldType(field_id));
		} catch (final Exception e) {
			Sextante.addErrorToLog(e);
			field_id = -1;
		}
		try {
			field_sort = m_Parameters.getParameterValueAsInt(FIELD_SORT);
			if ( m_DateField ) {
				field_sort_numeric = false; //Date fields are always imported as strings.
			} else {
				field_sort_numeric = Number.class.isAssignableFrom(m_Layer.getFieldType(field_sort));
			}			
		} catch (final Exception e) {
			Sextante.addErrorToLog(e);
			field_sort = -1;
		}		
				
		checkParameters( iShapeCount, mode, iShapeCount, tDist );		
		
		m_Output = getNewVectorLayer(RESULT, m_Layer.getName(), IVectorLayer.SHAPE_TYPE_LINE, m_Layer.getFieldTypes(),
				m_Layer.getFieldNames());
		
		final ArrayList<Vertex> vertices = new ArrayList<Vertex>();
		
		//Read all features plus sort field contents (if applicable)
		for ( int i = 0; i < iShapeCount; i ++ ) {
			feature = seek (i);
			//Get attribute fields.
			final Object[] atts = feature.getRecord().getValues();
			//Get geometry.
			Geometry geom = feature.getGeometry();
			//Get subgeometries (>1 if multi-point layer):
			for (int j = 0; j < geom.getNumGeometries(); j++) {
				final Geometry subgeom = geom.getGeometryN(j);
				double x = subgeom.getCoordinate().x;
				double y = subgeom.getCoordinate().y;
				double z = subgeom.getCoordinate().z;
				if ( sort ) {//Need to also store contents of field on which to sort!
					if ( field_sort_numeric ) {
						final double val = Double.parseDouble(feature.getRecord().getValue(field_sort).toString());
						Vertex v = new Vertex ( val, x, y, z, atts );
						vertices.add(v);
					} else {
						final String str = feature.getRecord().getValue(field_sort).toString();
						Vertex v = new Vertex ( str, x, y, z, atts );
						vertices.add(v);
					}					
				} else {
					Vertex v = new Vertex ( x, y, z, atts );
					vertices.add(v);
				}
			}
		}		
		//Sort vertices before connecting them?
		if ( sort ) {
			if ( reverse ) {
				Collections.sort(vertices, Collections.reverseOrder());
			} else {
				Collections.sort(vertices);
			}
		}
		
		//
		//Connect vertices according to "MODE"!
		//
		if ( Sextante.getText(MODE_ALL).equals(mode) ) {
			//Connect all points.			
			final ArrayList<Coordinate> array = new ArrayList<Coordinate>();
			//Attributes are copied from first input point.
			final Object[] atts = vertices.get(0).getAtts();
			final GeometryFactory gf = new GeometryFactory();
			for ( int i=0; i < vertices.size() && setProgress(i, vertices.size()) ; i ++ ) {
				array.add(vertex2coord(vertices.get(i)));
			}
			if ( array.size() < 2 ) {
				throw new GeoAlgorithmExecutionException(Sextante.getText("pts2line_err_points_out_lt_2"));
			}
			m_Output.addFeature(gf.createLineString(array.toArray(new Coordinate[0])), atts);
		}
		
		if ( Sextante.getText(MODE_ID).equals(mode) ) {
			//Connect points with same ID.
			int numOut = 0;
			final GeometryFactory gf = new GeometryFactory();
			ArrayList<Coordinate> array = null;
			Object[] atts = null;
			boolean first = false;
			double val1 = 0.0;
			String str1 = null;
			//Step through array with vertices, until all vertices have been processed.
			for ( int i = 0; i < vertices.size() && setProgress(i, vertices.size()); i++ ) {
				if ( vertices.get(i).isVisited() == false ) {
					array = new ArrayList<Coordinate>();
					atts = vertices.get(i).getAtts(); //Copy atts from first point with current ID.
					first = true;
					for ( int j = i; j < vertices.size(); j ++ ) {
						if ( first ) { //First vertex with this ID.							
							if ( field_id_numeric ) {
								val1 = Double.parseDouble(vertices.get(j).getAtts()[field_id].toString());
							} else {
								str1 = vertices.get(j).getAtts()[field_id].toString();
							}							
							array.add(vertex2coord(vertices.get(j))); //Always add.
							vertices.get(j).setVisited(true);
							first = false;
						} else {
							//Add only if ID is the same.
							if ( field_id_numeric ) {
								final double val2 = Double.parseDouble(vertices.get(j).getAtts()[field_id].toString());
								if ( val1 == val2 ) {
									array.add(vertex2coord(vertices.get(j)));
									vertices.get(j).setVisited(true);
								}
							} else {
								final String str2 = vertices.get(j).getAtts()[field_id].toString();
								if ( str1.equals(str2) ) {
									array.add(vertex2coord(vertices.get(j)));
									vertices.get(j).setVisited(true);
								}
							}
						}
					}
					//If we have > 2 points with current ID, then we connect them into a new line geom:
					if ( array.size() >= 2 ) {
						m_Output.addFeature(gf.createLineString(array.toArray(new Coordinate[0])), atts);
						numOut ++;
					}
				}
			}
			//Check that we have actually produced any output!
			if ( numOut < 1 ) {
				throw new GeoAlgorithmExecutionException(Sextante.getText("pts2line_err_lines_out_lt_1"));
			}
		}

		if ( Sextante.getText(MODE_DIST).equals(mode) ) {
			boolean first = true;
			int numOut = 0;
			//Connect points that are not more than "dist" apart.
			final GeometryFactory gf = new GeometryFactory();
			Object[] atts = null;
			ArrayList<Coordinate> array = null;
			for ( int i = 0; i < vertices.size() && setProgress(i, vertices.size()) ;i ++ ) {
				if ( first == false ) {
					//Check if distance to previous point is within threshold
					if ( dist (vertices.get(i), vertices.get(i-1)) <= tDist ) {
						//Within threshold: add vertex
						array.add(vertex2coord(vertices.get(i)));
					} else {
						//Distance threshold exceeded: close linestring.
						if ( array.size() > 1 ) {
							m_Output.addFeature(gf.createLineString(array.toArray(new Coordinate[0])), atts);
							numOut ++;
						}
						first = true;
					}
				}
				if ( first == true ) {
					//Copy attributes from current (first along this line) point.
					atts = vertices.get(i).getAtts();
					//First vertex must always be stored
					array = new ArrayList<Coordinate>();
					array.add(vertex2coord(vertices.get(i)));
					first = false;
				}	
			}
			//Connect last remaining group (if any)
			if ( array!=null && array.size() > 1 ) {
				m_Output.addFeature(gf.createLineString(array.toArray(new Coordinate[0])), atts);
				numOut ++;
			}
			//Check that we have actually produced any output!
			if ( numOut < 1 ) {
				throw new GeoAlgorithmExecutionException(Sextante.getText("pts2line_err_lines_out_lt_1"));
			}
		}
		
		if ( Sextante.getText(MODE_NPTS).equals(mode) ) {
			//Always connect n points.
			int current = 0;
			final GeometryFactory gf = new GeometryFactory();
			Object[] atts = null;
			ArrayList<Coordinate> array = null;
			for ( int i=0; i < vertices.size() && setProgress(i, vertices.size()) ; i ++ ) {				
				if ( current == 0 ) {
					//Copy attributes from first of n points.
					atts = vertices.get(i).getAtts();					
					array = new ArrayList<Coordinate>();
				}
				array.add(vertex2coord(vertices.get(i)));
				current ++;
				if ( current == numPoints ) {
					//Point count reached: connect and reset counter.
					m_Output.addFeature(gf.createLineString(array.toArray(new Coordinate[0])), atts);
					current = 0;					
				}
			}
		}

		return !m_Task.isCanceled();
	}

}
