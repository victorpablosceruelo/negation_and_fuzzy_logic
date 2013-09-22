package prologConnector;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.PLConnection;
import CiaoJava.PLException;
import constants.KConstants;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.PathsMgmt;

public class PlConnectionEnvelope {

	final static private Log LOG = LogFactory.getLog(PlConnectionEnvelope.class);

	private int connectionId = 0;
	private PLConnection plConnection = null;
	private boolean isReserved = true;
	PathsMgmt pathsMgmt = null;

	private static class Constants {
		public static int query = 0;
		public static int reserve = 1;
		public static int free = 2;
	}

	public PlConnectionEnvelope(int connectionId) throws PlConnectionEnvelopeException, FilesAndPathsException {
		if (connectionId < 0) {
			throw new PlConnectionEnvelopeException("connectionId cannot be < 0");
		}
		if (connectionId >= KConstants.PlConnectionsPool.maxNumOfConnections) {
			throw new PlConnectionEnvelopeException("connectionId cannot be >= " + KConstants.PlConnectionsPool.maxNumOfConnections);
		}

		this.isReserved = false;
		this.connectionId = connectionId;
		this.pathsMgmt = new PathsMgmt();
		this.plConnection = null;
	}

	private void createPlConnection() throws PlConnectionEnvelopeException, FilesAndPathsException {
		String[] argv = new String[1];
		argv[0] = this.pathsMgmt.getPlServerPath();

		try {
			this.plConnection = new PLConnection(argv);
		} catch (IOException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("IOException: " + e.getMessage());
		} catch (PLException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("PLException: " + e.getMessage());
		}
	}

	private void destroyPlConnection() throws PlConnectionEnvelopeException {
		if (this.plConnection != null) {
			try {
				this.plConnection.stop();
			} catch (InterruptedException e) {
				e.printStackTrace();
				throw new PlConnectionEnvelopeException("InterruptedException: " + e.getMessage());
			} catch (IOException e) {
				e.printStackTrace();
				throw new PlConnectionEnvelopeException("IOException: " + e.getMessage());
			} catch (PLException e) {
				e.printStackTrace();
				throw new PlConnectionEnvelopeException("PLException: " + e.getMessage());
			}
		}
	}

	public boolean reservate() {
		return testOrSetStatus(Constants.reserve);
	}

	public boolean free(boolean destroyConnection) {
		if (testOrSetStatus(Constants.query)) {
			if (destroyConnection) {
				try {
					destroyPlConnection();
				} catch (PlConnectionEnvelopeException e) {
					e.printStackTrace();
				}
				this.plConnection = null;
			}
		}
		return testOrSetStatus(Constants.free);
	}

	synchronized private boolean testOrSetStatus(int action) {
		if (Constants.query == action) {
			return this.isReserved;
		}
		if (Constants.reserve == action) {
			if (!this.isReserved) {
				// Capture it
				this.isReserved = true;
				// LOG.info("connection " + getConnectionId() +
				// " has been reserved.");
				return true;
			}
		}
		if (Constants.free == action) {
			if (this.isReserved) {
				// Free it
				this.isReserved = false;
				// LOG.info("connection " + getConnectionId() +
				// " has been freed.");
				return true;
			}
		}
		return false;
	}

	public int getConnectionId() {
		return this.connectionId;
	}

	public PLConnection getPlConnection() throws PlConnectionEnvelopeException, FilesAndPathsException {
		if (!testOrSetStatus(Constants.query)) {
			return null;
		}

		if (this.plConnection == null) {
			LOG.info("Creating connection for envelope with id: " + getConnectionId());
			createPlConnection();
		}
		return this.plConnection;
	}

	public void reinitializeConnection() {
		if (!testOrSetStatus(Constants.query)) {
			return;
		}

		try {
			destroyPlConnection();
		} catch (PlConnectionEnvelopeException e) {
			e.printStackTrace();
		}

		this.plConnection = null;

		try {
			createPlConnection();
		} catch (PlConnectionEnvelopeException e) {
			e.printStackTrace();
			this.plConnection = null;
		} catch (FilesAndPathsException e) {
			e.printStackTrace();
			this.plConnection = null;
		}

	}

}
