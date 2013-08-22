package prologConnector;

import filesAndPaths.FilesMgmt;

public class PlConnectionPool {



	
	public static CiaoPrologConnectionClass getConnection() throws Exception {
		




		CiaoPrologConnectionClass connection = new CiaoPrologConnectionClass();
		if (connection == null) throw new Exception("No connection with Prolog.");
		return connection;
	}
}
