package prologConnector;

import constants.KConstants;
import filesAndPaths.FilesAndPathsException;

public class PlConnectionsPool {

	private static PlConnectionEnvelope[] connections = null;

	private synchronized static void initializeConnectionsPool() throws PlConnectionEnvelopeException, FilesAndPathsException {
		if (connections == null) {
			connections = new PlConnectionEnvelope[KConstants.PlConnectionsPool.maxNumOfConnections];
			for (int i = 0; i < KConstants.PlConnectionsPool.maxNumOfConnections; i++) {
				connections[i] = new PlConnectionEnvelope(i);
			}
		}
	}

	private static PlConnectionEnvelope getConnection() throws PlConnectionEnvelopeException, FilesAndPathsException {
		boolean found = false;
		int i = 0;
		PlConnectionEnvelope connection = null;

		while (!found) {
			if (i >= KConstants.PlConnectionsPool.maxNumOfConnections) {
				i = 0;
			}

			connection = connections[i];
			if (connection != null) {
				found = connection.testAndSet(true);
			}
			
			if (!found) {
				i++;
			}

		}
		return connection;
	}

	public static void launchQuery(CiaoPrologQueryInterface query) throws PlConnectionEnvelopeException, CiaoPrologConnectorException,
			FilesAndPathsException {

		if (connections == null) {
			initializeConnectionsPool();
		}

		PlConnectionEnvelope connection = getConnection();
		try {
			connection.runPrologQuery(query);
		} finally {
			connection.testAndSet(true); // Free the connection.
		}

	}
}
