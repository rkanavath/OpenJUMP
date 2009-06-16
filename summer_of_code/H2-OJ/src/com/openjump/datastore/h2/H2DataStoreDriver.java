package com.openjump.datastore.h2;

import com.vividsolutions.jump.datastore.DataStoreConnection;
import com.vividsolutions.jump.datastore.DataStoreDriver;
import com.vividsolutions.jump.parameter.ParameterList;
import com.vividsolutions.jump.parameter.ParameterListSchema;
import org.h2.Driver;

import java.sql.*;

/**
 * A driver for supplying {@link H2DataStoreConnection}s
 */
public class H2DataStoreDriver implements DataStoreDriver {

	public static final String DRIVER_NAME = "H2Spatial";
	public static final String JDBC_CLASS = "org.h2.Driver";
	public static final String URL_PREFIX = "jdbc:H2:tcp";
	//public static final String URL_PREFIX = "jdbc:H2:";
	public static final String OPTIONS = ";AUTO_SERVER=TRUE";
	public static final String PARAM_Server = "Server";
	public static final String PARAM_Port = "Port";
	public static final String PARAM_Instance = "Instance";
	public static final String PARAM_User = "User";
	public static final String PARAM_Password = "Password";

	private static final String[] paramNames = new String[] {
		PARAM_Server,
		PARAM_Port,
		PARAM_Instance,
		PARAM_User,
		PARAM_Password
	};
	private static final Class[] paramClasses = new Class[]
	                                                      {
		String.class,
		Integer.class,
		String.class,
		String.class,
		String.class
	};
	private final ParameterListSchema schema = new ParameterListSchema(paramNames, paramClasses);;


	public DataStoreConnection createConnection(ParameterList params)
		throws Exception
	{
		String host = params.getParameterString(PARAM_Server);
		int port = params.getParameterInt(PARAM_Port);
		String database = params.getParameterString(PARAM_Instance);
		String user = params.getParameterString(PARAM_User);
		String password = params.getParameterString(PARAM_Password);

		String url
		= String.valueOf(new StringBuffer(URL_PREFIX).append
				(host).append
				(":").append
				//(port).append
				("//").append
				//(database).append(OPTIONS));
				(database));


		Driver driver = (Driver) Class.forName(JDBC_CLASS).newInstance();
		DriverManager.registerDriver(driver);

		Connection conn = DriverManager.getConnection(url, user, password);
		return new H2DataStoreConnection(conn);
	}

	public boolean isAdHocQuerySupported() {
		return true;
	}

	public ParameterListSchema getParameterListSchema() {
	    return schema;
	}

	public String getName() {
	    return DRIVER_NAME;

	}

}
