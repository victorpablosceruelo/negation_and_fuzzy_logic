package prologConnector;

import filesAndPaths.FilesMgmt;

public class ConnectionsPool {


	private static String plServerPath = null;

	
	public static CiaoPrologConnectionClass getConnection() throws Exception {
		



		if (plServerPath == null) {
			plServerPath = FilesMgmt.returnPlServerValidPath(plServerValidSubPaths, LOG);
			LOG.info("plServerPath: " + plServerPath);
		}

		CiaoPrologConnectionClass connection = new CiaoPrologConnectionClass();
		if (connection == null) throw new Exception("No connection with Prolog.");
		return connection;
	}
}
