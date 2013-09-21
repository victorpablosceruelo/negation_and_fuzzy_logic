package urls;

import javax.servlet.http.HttpServletRequest;

public class AppUrl {

	private static final class PrivateConstants {
		public static final String appSubPath = "/flese/";
		public static final String appSubPathInTest = "/fleseTest/";

		public static final String httpsPrefix = "https://";
		public static final String httpPrefix = "http://";

		/**
		 * This are the invalid ends for the serverUrl computed. Be carefull:
		 * removal order matters.
		 */
		public static final String[] invalidTailsStarts = { "?", "index.jsp", "WEB-INF", "servlets", "images", "js_and_css" };

	}

	private static class ServerAndAppUrlsMaintenance {
		private static class hiddenVariables {
			private static String serverUrl = "";
			private static String appUrl = "";
		}

		public static String getServerUrl() {
			return getOrSetServerUrl(null);
		}

		public static void setServerUrl(String url) {
			getOrSetServerUrl(url);
		}

		public static String getAppUrl() {
			return getOrSetAppUrl(null);
		}

		public static void setAppUrl(String url) {
			getOrSetAppUrl(url);
		}

		private static synchronized String getOrSetServerUrl(String url) {
			if (url != null) {
				hiddenVariables.serverUrl = url;
			}
			return hiddenVariables.serverUrl;
		}

		private static synchronized String getOrSetAppUrl(String url) {
			if (url != null) {
				hiddenVariables.appUrl = url;
			}
			return hiddenVariables.appUrl;
		}

	}

	public static String getAppUrl() {
		return getAppUrl(null);
	}

	public static String getAppUrl(HttpServletRequest request) {
		if ("".equals(ServerAndAppUrlsMaintenance.getAppUrl())) {
			computeAppAndServerUrls(request);
		}

		return ServerAndAppUrlsMaintenance.getAppUrl();
	}

	public static String getAppFullUrl() {
		return getAppUrl(null);
	}

	public static String getAppFullUrl(HttpServletRequest request) {
		String appUrl = getAppUrl(request);
		String serverUrl = ServerAndAppUrlsMaintenance.getServerUrl();

		return ("".equals(appUrl)) ? "" : serverUrl + appUrl;
	}

	private static void computeAppAndServerUrls(HttpServletRequest request) {

		if (!"".equals(ServerAndAppUrlsMaintenance.getAppUrl())) {
			return;
		}

		if (!"".equals(ServerAndAppUrlsMaintenance.getServerUrl())) {
			return;
		}

		if (request == null) {
			return;
		}

		String serverName = request.getServerName();
		if ((serverName == null) || ("".equals(serverName))) {
			return;
		}

		String requestUrl = request.getRequestURL().toString();
		if ((requestUrl == null) || ("".equals(requestUrl))) {
			return;
		}

		boolean isHTTPS = requestUrl.startsWith(PrivateConstants.httpsPrefix);
		boolean isHTTP = requestUrl.startsWith(PrivateConstants.httpPrefix);
		if (isHTTP) {
			requestUrl = requestUrl.substring(PrivateConstants.httpPrefix.length());
		}
		if (isHTTPS) {
			requestUrl = requestUrl.substring(PrivateConstants.httpsPrefix.length());
		}

		while (requestUrl.startsWith("/")) {
			requestUrl = requestUrl.substring(1);
		}

		int index = 0;
		// Remove the invalid ends.
		requestUrl = removeInvalidTails(requestUrl);

		String serverUrl = "";
		// Now remove the server name, but save it !!!
		index = requestUrl.indexOf(serverName);
		if (index > -1) {
			index = index + serverName.length();
			serverUrl = requestUrl.substring(0, index);
			requestUrl = requestUrl.substring(index);
		}

		// We consider the server port as part of the server name.
		index = requestUrl.indexOf(':');
		if (index > -1) {
			index = requestUrl.indexOf('/');
			if (index > -1) {
				serverUrl += requestUrl.substring(0, index);
				requestUrl = requestUrl.substring(index);
			}
		}

		ServerAndAppUrlsMaintenance.setServerUrl(serverUrl);
		ServerAndAppUrlsMaintenance.setAppUrl(requestUrl);

	}

	/**
	 * Looks for the invalid ends defined in invalidEnds and removes the tail of
	 * the string, including the invalid end.
	 * 
	 * @param url
	 *            is the input string.
	 * @return the url without the invalid ends.
	 */
	private static String removeInvalidTails(String url) {
		int index = -1;
		String invalidEnd = null;
		for (int i = 0; i < PrivateConstants.invalidTailsStarts.length; i++) {
			invalidEnd = PrivateConstants.invalidTailsStarts[i];
			index = url.indexOf(invalidEnd);
			if (index > -1) {
				url = url.substring(0, index);
			}
		}
		return url;
	}
}
