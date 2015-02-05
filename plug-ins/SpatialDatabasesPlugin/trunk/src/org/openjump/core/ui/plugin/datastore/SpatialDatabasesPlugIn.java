/*
 * created on 		04.10.2005
 * last modified: 	05.10.2005 comments added
 * 
 * author:			sstein
 * 
 **/
package org.openjump.core.ui.plugin.datastore;

import com.vividsolutions.jump.datastore.DataStoreDriver;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import java.sql.Driver;
import org.openjump.core.ui.plugin.datastore.oracle.*;
/**
 * @description: testplugin for jump<p>
 * shows hello world - window in Jump
 *
 * @author sstein
 *
 */
public class SpatialDatabasesPlugIn extends AbstractPlugIn {

    public SpatialDatabasesPlugIn() {
        // empty constructor
    }

    public void initialize(PlugInContext context) throws Exception {
        // registers the OracleDataStore driver to the system:
        try {
            Driver driver = (Driver) Class.forName(OracleDataStoreDriver.JDBC_CLASS).newInstance();
            context.getWorkbenchContext().getRegistry().createEntry(DataStoreDriver.REGISTRY_CLASSIFICATION,
                new OracleDataStoreDriver());
            System.out.println("Oracle Spatial Data Store added");
        } catch (Exception e) {
            // TODO: replace by log ?
            System.out.println("oracle driver not found: " + e.toString() + ". Oracle Spatial Data Store NOT added");
        }

    }
}
