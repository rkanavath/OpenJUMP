package org.openjump.datastore.h2;

import java.sql.Connection;
import java.sql.SQLException;

import com.vividsolutions.jump.datastore.AdhocQuery;
import com.vividsolutions.jump.datastore.DataStoreConnection;
import com.vividsolutions.jump.datastore.DataStoreException;
import com.vividsolutions.jump.datastore.DataStoreMetadata;
import com.vividsolutions.jump.datastore.FilterQuery;
import com.vividsolutions.jump.datastore.Query;
import com.vividsolutions.jump.io.FeatureInputStream;
import com.vividsolutions.jump.datastore.SpatialReferenceSystemID;


public class H2DataStoreConnection implements DataStoreConnection {

	private H2DataStoreMetadata dbMetadata;
	private Connection connection;

	public H2DataStoreConnection(Connection conn) {
		connection = conn;
		dbMetadata = new H2DataStoreMetadata(this);
	}

	public Connection getConnection()
	{
		return connection;
	}

	public FeatureInputStream execute(Query query) {
		if (query instanceof FilterQuery) {
			try {
				return executeFilterQuery((FilterQuery) query);
			} catch (SQLException e) {
				throw new RuntimeException(e);
			}
		}
		if (query instanceof AdhocQuery) {
			return executeAdhocQuery((AdhocQuery) query);
		}
		throw new IllegalArgumentException("Unsupported Query type");
	}

	/**
	 * Executes a filter query.
	 *
	 * The SRID is optional for queries - it will be determined automatically
	 * from the table metadata if not supplied.
	 *
	 * @param query the query to execute
	 * @return the results of the query
	 * @throws SQLException
	 */
	public FeatureInputStream executeFilterQuery(FilterQuery query) throws SQLException
	{
		SpatialReferenceSystemID srid = dbMetadata.getSRID(query.getDatasetName(), query.getGeometryAttributeName());
		String[] colNames = dbMetadata.getColumnNames(query.getDatasetName());

		H2SQLBuilder builder = new H2SQLBuilder(srid, colNames);
		String queryString = builder.getSQL(query);

		H2FeatureInputStream ifs = new H2FeatureInputStream(connection, queryString);
		return ifs;
	}

	public FeatureInputStream executeAdhocQuery(AdhocQuery query)
	{
		String queryString = query.getQuery();
		H2FeatureInputStream ifs = new H2FeatureInputStream(connection, queryString);
		return ifs;
	}

	public void close()
	throws DataStoreException
	{
		try {
			connection.close();
		}
		catch (Exception ex) { throw new DataStoreException(ex); }
	}

	public boolean isClosed() throws DataStoreException {
		try {
			return connection.isClosed();
		} catch (SQLException e) {
			throw new DataStoreException(e);
		}
	}

	public DataStoreMetadata getMetadata() {
		return dbMetadata;
	}

}

