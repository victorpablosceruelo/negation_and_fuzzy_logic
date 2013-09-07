package prologConnector;

import constants.KConstants;
import filesAndPaths.PathsMgmtException;

public class PlConnectionsPool {

	private static PlConnectionEnvelope[] connections = null;

	private synchronized static void initializeConnectionsPool() {
		if (connections == null) {
			connections = new PlConnectionEnvelope[KConstants.PlConnectionsPool.maxNumOfConnections];
			for (int i = 0; i < KConstants.PlConnectionsPool.maxNumOfConnections; i++) {
				connections[i] = null;
			}
		}
	}

	private synchronized static PlConnectionEnvelope initializeConnection(int i) throws PlConnectionEnvelopeException, PathsMgmtException {
		if (connections == null) {
			throw new PlConnectionEnvelopeException("connections is null.");
		}

		if (connections[i] == null) {
			connections[i] = new PlConnectionEnvelope(i);
		}
		return connections[i];
	}

	private static PlConnectionEnvelope getConnection() throws PlConnectionEnvelopeException, PathsMgmtException {
		boolean found = false;
		int i = 0;
		PlConnectionEnvelope connection = null;

		while (!found) {
			if (i >= KConstants.PlConnectionsPool.maxNumOfConnections) {
				i = 0;
			}

			connection = connections[i];
			if (connection == null) {
				connection = initializeConnection(i);
			}

			found = connection.testAndSet(true);
			if (!found) {
				i++;
			}
		}
		return connection;
	}

	public static void launchQuery(CiaoPrologQueryInterface query) throws PlConnectionEnvelopeException, CiaoPrologTermInJavaException,
			CiaoPrologQueryException, PathsMgmtException, CiaoPrologQueryAnswerException {

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
