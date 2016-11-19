package es.unex.sextante.rasterize.rasterizeTracks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

import es.unex.sextante.core.AnalysisExtent;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.dataObjects.IFeature;
import es.unex.sextante.dataObjects.IRecord;
import es.unex.sextante.exceptions.GeoAlgorithmExecutionException;
import es.unex.sextante.rasterize.rasterizeTracks.TrackPartIterable.TrackPart;

public class TrackPartIterable implements Iterable<TrackPart[]> {

	private int sensorcount;
	private int rowsAfter;
	private int valueCol;
	private int sensorCol;
	private int trackCol;
	private AnalysisExtent extent;
	private ArrayList<IFeature> data;
	private int timestampCol;
	private static GeometryFactory geometryFactory = new GeometryFactory();
	private boolean reversed=false;
	private boolean useSensorIDs;
	private boolean useRowInBetweenInterpolation;
	private Polygon bounds;
	private ArrayList<Sensor> left = new ArrayList<Sensor>();
	private ArrayList<Sensor> right = new ArrayList<Sensor>();
	private Sensor[] completeBounds;
	private boolean m_storeAllPoints;
	private boolean ignoreTimestamps;

	public TrackPartIterable(ArrayList<IFeature> features, int sensorcount,
			int rowsAfter, int valueCol, int sensorCol, int trackCol, int timestampCol, boolean useSensorIDs, boolean m_bUseRowInBetweenInterpolation, AnalysisExtent extent, boolean storeOuterPoints, boolean ignoreTimestamps) {
		this.data=features;
		this.sensorcount = sensorcount;
		this.rowsAfter = rowsAfter;
		this.valueCol = valueCol;
		this.sensorCol = sensorCol;
		this.trackCol = trackCol;
		this.timestampCol=timestampCol;
		this.useSensorIDs=useSensorIDs;
		this.useRowInBetweenInterpolation=m_bUseRowInBetweenInterpolation;
		this.extent = extent;
		this.ignoreTimestamps=ignoreTimestamps;
		m_storeAllPoints = storeOuterPoints; 
		
	}
	
	

	public class TrackPartIterator implements Iterator<TrackPart[]> {
		private Iterator<IFeature> iFeatureIterator;
		private TrackPart[] data = new TrackPart[rowsAfter + 1];
		short track = Short.MIN_VALUE;
		private IFeature lastStep;
		private int line=0;
		private double stepDist=Double.POSITIVE_INFINITY;
		
		public TrackPartIterator(Iterator<IFeature> iterator) {
			this.iFeatureIterator = iterator;
		}
		
		public void init() throws GeoAlgorithmExecutionException{
			if(!iFeatureIterator.hasNext()&&!reversed){
				throw new GeoAlgorithmExecutionException("Given Track has no data");
			}
			lastStep = iFeatureIterator.next();
			
			for (int row = 1; row < rowsAfter + 1; row++) {
				TrackPart nextstep = computeNextDataStep(iFeatureIterator);
				data[row] = nextstep;
			}
			if(m_storeAllPoints&&bounds==null){
				for(int i=0; i<data[1].sensors.length; i++){
					if(data[1].sensors[i]!=null){
						left.add(data[1].sensors[i]);
					}
				}
			}
		}
		/**
		 * Computes the next sensor row out of a given iterator.
		 * - split sensor row by given timestamp
		 * - compute bounds (missing sensor ids on the begin or end)
		 * - fix gaps in between of the gaps
		 * @return the next Trackpart (sensor row)
		 * @throws TrackPartIteratorExeption
		 */
		private TrackPart computeNextDataStep(Iterator<IFeature> featureIterator) throws GeoAlgorithmExecutionException {

			IFeature step = lastStep;	//lastSensor, is first sensor in current row 
			Sensor[] sensors = new Sensor[sensorcount];
			IRecord record = step.getRecord();
			
			double timestamp0 = Double.parseDouble(record.getValue(timestampCol).toString());
			double sensor0 = Double.parseDouble(record.getValue(sensorCol).toString())-1;
			short track0 = Short.parseShort(record.getValue(trackCol).toString());
			track=track0;
			int pos=0;
			while(featureIterator.hasNext()){
				record = step.getRecord();
				double timestamp = Double.parseDouble(record.getValue(timestampCol).toString());
				if(ignoreTimestamps){
					int sensor = Integer.parseInt(record.getValue(sensorCol).toString()) - 1;
					//Sextante.addInfoToLog("pos:"+pos+"\tsensor0:"+sensor0+"\tsensor:"+sensor);
					if(sensor>=pos){
						double value = Double.parseDouble(record.getValue(valueCol).toString());
						sensors[sensor] = new Sensor(step.getGeometry().getCoordinate(),value);
						step=featureIterator.next();
						lastStep=step;
					}else{
						break;
					}
				}else if(timestamp==timestamp0){
					double value = Double.parseDouble(record.getValue(valueCol).toString());
					int sensor = pos;
					if(useSensorIDs){
					sensor = Integer.parseInt(record.getValue(sensorCol).toString()) - 1;
					}
					sensors[sensor] = new Sensor(step.getGeometry().getCoordinate(),value);
					step=featureIterator.next();
					lastStep=step;
				}else{
					break;
				}				
				//pos=(pos+1)%sensorcount;
				pos++;
			};
			//Sextante.addInfoToLog(Arrays.toString(sensors));
			if(reversed){
				line--;
			}else{
				line++;
			}
			
			//check bounds (regarding the missing sensors in the begin/end of the sensor row)
			//Sextante.addInfoToLog("check Line "+line+" of Track "+track0);
			int left = 0;
			for(; left < sensorcount; left++ ){
				if(sensors[left]!=null) break;
			}
			int right = sensorcount-1;
			for(; right >= 0; right-- ){
				if(sensors[right]!=null) break;
			}
			
			if(left>right&&!reversed) {
				throw new GeoAlgorithmExecutionException("Line "+line+" of Track "+track0+" has no sensordata");
			}
			if(left<=right){
				repairInternalRow(sensors, left, right,extent);
			}
			return new TrackPart(sensors, track0,left,right,timestamp0);

		}
		

		@Override
		public boolean hasNext() {

			return iFeatureIterator.hasNext()||data[0]!=data[data.length-1];
			//return iFeatureIterator.hasNext();
		}

		@Override
		public TrackPart[] next(){
			// do {
			for (int row = 0; row < data.length - 1; row++) {
				data[row] = data[row + 1];
			}
			if(iFeatureIterator.hasNext()){
				try {
					data[data.length - 1] = computeNextDataStep(iFeatureIterator);
					//if(!reversed){
						double dist = data[data.length - 1].distance(data[data.length - 2]);
						this.stepDist = dist<this.stepDist?dist:this.stepDist;
					//}
					
					if(useRowInBetweenInterpolation
							&&data[data.length - 3]!=null&&data[data.length - 1]!=null
							&&data[data.length - 3].distance(data[data.length - 1])<=2*getStepDist()){
						//Sextante.addInfoToLog("repairRow:"+Arrays.toString(data));
						int innerLeft = Math.max(data[data.length - 1].getLeft(), data[data.length - 3].getLeft());
						int innerRight = Math.min(data[data.length - 1].getRight(), data[data.length - 3].getRight());
						if(data[data.length - 2]==null){
							Sensor[] sensors = new Sensor[sensorcount]; 
							data[data.length - 2]= new TrackPart(sensors, data[data.length - 3].getTrackid(), innerLeft, innerRight,(data[data.length - 1].timestamp+data[data.length - 3].timestamp)/2.0 );
							for(int dx = innerLeft; dx<=innerRight; dx++){
								repairRow(dx);
							}
							data[data.length - 2].update(innerLeft, innerRight);
						}else{
							boolean repaired = false;
							for(int dx = innerLeft; dx<Math.min(data[data.length - 2].getLeft(), innerRight); dx++){
								repairRow(dx);
								repaired = true;
							}
							for(int dx = Math.max(data[data.length - 2].getRight()+1,innerLeft); dx<=innerRight; dx++){
								repairRow(dx);
								repaired = true;
							}
							if(repaired){
								data[data.length - 2].update(Math.min(data[data.length - 2].getLeft(),innerLeft), Math.max(data[data.length - 2].getRight(),innerRight));
								
							}
						}
						//Sextante.addInfoToLog("repaired Row:"+Arrays.toString(data));
						
					}
					if(m_storeAllPoints&&bounds==null){
						//for(int step=0; step<data.length; step++){
						int step = 1;
						if(data[step].getSensors()[0]!=null){
							left.add(data[step].sensors[0]);
						}else{
							for(int i=data[step].left; i>0; i--){
								if(data[step].getSensors()[i]!=null&&data[step].getSensors()[i-1]==null){
									left.add(data[step].sensors[i]);
									break;
								}
							}
						}
						if(data[step].getSensors()[data[step].getSensors().length-1]!=null){
							right.add(data[step].getSensors()[data[step].getSensors().length-1]);
						}else{
							for(int i=data[step].right; i<data[step].getSensors().length-1; i++){
								if(data[step].getSensors()[i]!=null&&data[step].getSensors()[i+1]==null){
									right.add(data[step].sensors[i]);
									break;
								}
							}
						}
						//}
						//Sextante.addInfoToLog("new Left bounds"+left.get(left.size()-1)+"new Right bounds"+right.get(right.size()-1));
					}
				} catch (GeoAlgorithmExecutionException e) {
					Sextante.addErrorToLog(e);
					e.printStackTrace();
				}
			}else if(m_storeAllPoints&&bounds==null){ 
				//store lastLine
				//Sextante.addInfoToLog("save last line and combine bounds");
				for(int i=0; i<data[2].sensors.length; i++){
					if(data[2].sensors[i]!=null){
						left.add(data[2].sensors[i]);
					}
				}
				completeBounds = new Sensor[left.size()+right.size()+1];
				for(int i=0; i<left.size(); i++){
					completeBounds[i]=left.get(i);
				}
				Collections.reverse(right);
				for(int i=0; i<right.size(); i++){
					completeBounds[i+left.size()]=right.get(i);
				}
				completeBounds[completeBounds.length-1]=left.get(0);
				LinearRing ring = geometryFactory.createLinearRing(completeBounds);
				if(m_storeAllPoints){
					bounds = geometryFactory.createPolygon(ring, null);
				}
			}
			// } while (hasSingleTrack(data) && hasNext());
			if(!hasSingleTrack(data))return null;
			return data;
		}
		private void repairRow(int dx) {
			if(data[data.length - 2].sensors[dx]!=null)return;
			
			//Sextante.addInfoToLog("dx:"+dx);
				double x = 		data[data.length - 1].getSensors()[dx].x * 0.5
						+ 		data[data.length - 3].getSensors()[dx].x * 0.5;
				double y = 		data[data.length - 1].getSensors()[dx].y * 0.5
						+ 		data[data.length - 3].getSensors()[dx].y * 0.5;
				double z = 		data[data.length - 1].getSensors()[dx].z * 0.5
						+ 		data[data.length - 3].getSensors()[dx].z * 0.5;
				double value = 	data[data.length - 1].getSensors()[dx].value * 0.5
						+ 		data[data.length - 3].getSensors()[dx].value * 0.5;
				data[data.length - 2].getSensors()[dx]=new Sensor(new Coordinate(x,y,z), value);
		}

		public int getLine() {
			return line;
		}
		public short getTrack() {
			return track;
		}
		
		public double getStepDist() {
			return stepDist;
		}
		
		
	}

	public class TrackPart {
		private Sensor[] sensors;
		private short trackid;
		private int right;
		private int left;
		private double timestamp;
		public TrackPart(Sensor[] sensors, short trackid, int left, int right, double timestamp) {
			this.sensors=sensors;
			this.trackid = trackid;
			this.left = left;
			this.right = right;
			this.timestamp=timestamp;
		}
		public void update(int left, int right) {
			this.left = left;
			this.right = right;
			Sensor[] sensors2 = new Sensor[sensors.length];
			boolean has_gap = false;
			for (int i = left; i <= right; i++) {
				if(sensors[i]==null){
					//Sextante.addInfoToLog("left:"+left+"\tright:"+right+"\tdata:"+Arrays.toString(getGeometries()));
					has_gap=true;
					continue;
				}
				sensors2[i]=sensors[i];
			}
			if(has_gap){
				repairInternalRow(sensors2, left, right,extent);
				sensors=sensors2;
				update(left, right);
			}
			sensors=sensors2;
		}
		public int getLeft() {
			return left;
		}
		public int getRight() {
			return right;
		}
		public short getTrackid() {
			return trackid;
		}
		public Sensor[] getSensors() {
			return sensors;
		}
		
		@Override
		public String toString() {
		
			return "Sensordata "+left+" - "+right;
		}
		
		public double distance(TrackPart tp2){
			return 0;//Math.abs(timestamp-tp2.timestamp);
		}
	}
	
	/**
	 * iterates through a given Array and checks if all data is from the same track
	 * @param parts
	 * @return
	 */
	public static boolean hasSingleTrack(TrackPart[] parts) {
		short trackid = parts[0].getTrackid();
		for (int i = 0; i < parts.length; i++) {
			if (trackid != parts[i].getTrackid()) {
				return false;
			}
		}
		return true;
	}
	
	public ArrayList<IFeature> getData() {
		return data;
	}
	
	public void reverse(){
		this.reversed=!this.reversed;
		Collections.reverse(data);
	}
	
	@Override
	public TrackPartIterator iterator() {
		return new TrackPartIterator(data.iterator());
	}
	
	//find and fix gaps in between of the computed bounds
	private static void repairInternalRow(Sensor[] sensors2, int left, int right, AnalysisExtent extent) {
		for (int leftGap = left; leftGap <= right; leftGap++) {
			if (sensors2[leftGap] == null) {
				int rightGap = leftGap + 1;
				for (; rightGap <= right; rightGap++) {
					if (sensors2[rightGap] != null) {
						rightGap--;
						break;
					}
				}
				if (rightGap != right) {
					repair(leftGap, rightGap, sensors2,extent);
					leftGap = rightGap;
				} else {
					break;
				}
			}
		}
		
	}

	/**
	 * repairs a given gap by using a linear interpolation
	 * 
	 * @param gapBegin
	 *            - the index of the first sensor which is missing
	 * @param gapEnd
	 *            - the index of the last sensor which is missing
	 * @param sensors2
	 *            - the array of given sensor geometries
	 * @param extent 
	 */
	private static void repair(int gapBegin, int gapEnd, Sensor[] sensors2, AnalysisExtent extent) {
		gapBegin--;
		gapEnd++;
		double diff = gapEnd - gapBegin;
		// Sextante.addInfoToLog("repair between"+gapBegin+"-"+gapEnd+" with a
		// diff of "+ diff);
		for (int i = 0; i <= diff; i++) {
			// Sextante.addInfoToLog("compute mid3(i):" +i);
			double x = sensors2[gapBegin].x * (1 - (i / diff))
					+ sensors2[gapEnd].x * (i / diff);
			double y = sensors2[gapBegin].y * (1 - (i / diff))
					+ sensors2[gapEnd].y * (i / diff);
			double z = sensors2[gapBegin].z * (1 - (i / diff))
					+ sensors2[gapEnd].z * (i / diff);
			double value = sensors2[gapBegin].value * (1 - (i / diff)) + sensors2[gapEnd].value * (i / diff);
			// Sextante.addInfoToLog("mid3 x:"+x+"\ty:"+y+"\tvalue:"+value);
			sensors2[gapBegin + i] = new Sensor(new Coordinate(x, y,z), value);
		}
		
		
	}
	
	public int getSensorCol() {
		return sensorCol;
	}
	public int getTrackCol() {
		return trackCol;
	}
	public int getTimestampCol() {
		return timestampCol;
	}
	public int getSensorcount() {
		return sensorcount;
	}
	public int getValueCol() {
		return valueCol;
	}

	public void setData(ArrayList<IFeature> data) {
		this.data=data;
	}
	
	public Polygon getBounds() {
		return bounds;
	}
	
	public Sensor[] getCompleteBounds() {
		completeBounds = new Sensor[left.size()+right.size()+1];
		for(int i=0; i<left.size(); i++){
			completeBounds[i]=left.get(i);
		}
		Collections.reverse(right);
		for(int i=0; i<right.size(); i++){
			completeBounds[i+left.size()]=right.get(i);
		}
		completeBounds[completeBounds.length-1]=left.get(0);
		return completeBounds;
	}
}
