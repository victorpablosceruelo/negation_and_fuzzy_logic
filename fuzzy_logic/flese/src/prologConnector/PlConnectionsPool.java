package prologConnector;

import constants.KConstants;
import filesAndPaths.PathsMgmtException;

public class PlConnectionsPool {

	private static PlConnectionEnvelope[] connections = null;

	private PlConnectionsPool() throws PlConnectionEnvelopeException, PathsMgmtException {
		connections = new PlConnectionEnvelope[KConstants.PlConnectionsPool.maxNumOfConnections];
		for (int i = 0; i < KConstants.PlConnectionsPool.maxNumOfConnections; i++) {
			if (connections[i] == null) {
				connections[i] = new PlConnectionEnvelope(i);
			}
		}
	}

	public static void launchQuery(CiaoPrologQueryInterface query) throws PlConnectionEnvelopeException, AnswerTermInJavaClassException,
			CiaoPrologQueryException, PathsMgmtException {

		PlConnectionEnvelope connection = null;
		boolean found = false;
		int i = 0;

		while (!found) {
			if (i >= KConstants.PlConnectionsPool.maxNumOfConnections)
				i = 0;

			connection = connections[i];
			found = connection.testAndSet(true);
			if (!found)
				i++;
		}

		connection.runPrologQuery(query);
	}
}
