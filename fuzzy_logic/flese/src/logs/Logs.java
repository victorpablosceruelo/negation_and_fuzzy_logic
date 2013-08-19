package logs;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;

public class Logs {

	private static Boolean previouslyLogged = false;

	public static void logServerInfoClass(HttpServletRequest request, Log LOG) {
		if (!previouslyLogged) {
			// Returns the host name of the server to which the request was
			// sent.
			LOG.info("request.getServerName(): " + request.getServerName());
			// Returns the host name of the server to which the request was
			// sent.
			LOG.info("request.getServerPort()" + request.getServerPort());
			// Returns the host name of the Internet Protocol (IP) interface on
			// which the request was received.
			LOG.info("request.getLocalName()" + request.getLocalName());
			// Returns the Internet Protocol (IP) port number of the interface
			// on which the request was received.
			LOG.info("request.getLocalPort()" + request.getLocalPort());

			previouslyLogged = true;
		}
	}
}
