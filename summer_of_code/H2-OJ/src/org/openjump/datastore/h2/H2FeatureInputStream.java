package org.openjump.datastore.h2;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.io.BaseFeatureInputStream;

public class H2FeatureInputStream 
extends BaseFeatureInputStream {

	private FeatureSchema featureSchema;
	private Connection conn;
	private String queryString;
	private boolean initialized = false;
	private Exception savedException;

	private Statement stmt = null;
	private ResultSet rs = null;
	private H2ResultSetConverter mapper;

	int geometryColIndex = -1;

	public H2FeatureInputStream(Connection conn, String queryString) {
		this.conn = conn;
		this.queryString = queryString;
	}

	/**
	 * @return The underlaying {@link Connection}.
	 */
	public Connection getConnection()  {return conn;}


	private void init()
	throws SQLException
	{
		if (initialized)
			return;
		initialized = true;

		//conn.setDefaultRowPrefetch(100);
		stmt = conn.createStatement();
		String parsedQuery = queryString;
		//String parsedQuery = QueryUtil.parseQuery(queryString);
		rs = stmt.executeQuery(parsedQuery);
		mapper = new H2ResultSetConverter(conn, rs);
		featureSchema = mapper.getFeatureSchema();
	}

	protected Feature readNext()
	throws Exception
	{
		if (savedException != null)
			throw savedException;
		if (! initialized)
			init();
		if (rs == null)
			return null;
		if (! rs.next())
			return null;
		return getFeature();
	}

	private Feature getFeature()
	throws Exception
	{
		return mapper.getFeature();
	}

	public void close() throws SQLException {
		if (rs != null) {
			rs.close();
		}
		if (stmt != null) {
			stmt.close();
		}
	}

	public FeatureSchema getFeatureSchema()
	{
		if (featureSchema != null)
			return featureSchema;

		try {
			init();
		}
		catch (SQLException ex)
		{
			savedException = ex;
		}
		if (featureSchema == null)
			featureSchema = new FeatureSchema();
		return featureSchema;
	}

}
