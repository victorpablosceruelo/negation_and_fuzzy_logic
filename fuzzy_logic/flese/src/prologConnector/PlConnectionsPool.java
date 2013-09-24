package prologConnector;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import constants.KConstants;
import filesAndPaths.FilesAndPathsException;

public class PlConnectionsPool {

	final static private Log LOG = LogFactory.getLog(PlConnectionEnvelope.class);

	private static PlConnectionEnvelope[] connections = null;
	private static MgmtThread mgmtThread = null;

	private static class MgmtThread extends Thread {

		public void run() {
			boolean continueLooping = true;
			while (continueLooping) {
				reinitilizeConnections();
				continueLooping = sleep();
			}
		}

		private boolean sleep() {
			long millis = 1000; // 1 second.
			try {
				sleep(millis);
			} catch (InterruptedException e) {
				e.printStackTrace();
				return false;
			}
			return true;
		}

		private void reinitilizeConnections() {
			LOG.info("Reinitializing connections. ");
			String result = "";
			for (int i = 0; i < KConstants.PlConnectionsPool.maxNumOfConnections; i++) {
				if (connections[i] != null) {
					boolean reserved = connections[i].reservate();
					if (reserved) {
						result += connections[i].reinitializeConnection();
						connections[i].free(false);
					}
				}
			}
			LOG.info("Reinitialized connections: " + result);
		}
	}

	private synchronized static void initializeConnectionsPool() throws PlConnectionEnvelopeException, FilesAndPathsException {
		if (connections == null) {
			connections = new PlConnectionEnvelope[KConstants.PlConnectionsPool.maxNumOfConnections];
			for (int i = 0; i < KConstants.PlConnectionsPool.maxNumOfConnections; i++) {
				if (connections[i] == null) {
					connections[i] = new PlConnectionEnvelope(i);
				}
			}
			mgmtThread = new MgmtThread();
			mgmtThread.start();
		}
	}

	private static PlConnectionEnvelope getConnection() throws PlConnectionEnvelopeException, FilesAndPathsException {
		boolean found = false;
		int i = 0;
		PlConnectionEnvelope connectionEnvelope = null;

		while (!found) {
			if (i >= KConstants.PlConnectionsPool.maxNumOfConnections) {
				i = 0;
			}

			connectionEnvelope = connections[i];
			if (connectionEnvelope != null) {
				found = connectionEnvelope.reservate();
				if (found) {
					LOG.info("connection " + connectionEnvelope.getConnectionId() + " has been reserved.");
				} else {
					LOG.info("connection " + connectionEnvelope.getConnectionId() + " has NOT been reserved.");
				}
			}

			if (!found) {
				i++;
			}

		}
		return connectionEnvelope;
	}

	public static void launchQuery(CiaoPrologQueryInterface query) throws PlConnectionEnvelopeException, CiaoPrologConnectorException,
			FilesAndPathsException {

		if (connections == null) {
			initializeConnectionsPool();
		}

		PlConnectionEnvelope connectionEnvelope = getConnection();
		try {
			PLConnectionUtils.runPrologQuery(query, connectionEnvelope.getPlConnection());
		} finally {
			connectionEnvelope.free(true); // Free the connection.
			LOG.info("connection " + connectionEnvelope.getConnectionId() + " has been freed.");
		}

	}
}
