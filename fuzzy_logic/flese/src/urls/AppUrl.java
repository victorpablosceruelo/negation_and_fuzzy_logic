package urls;

import javax.servlet.http.HttpServletRequest;

public class AppUrl {

	private static String appUrl = null;
	private static String appPath = null;
	private static String httpsPrefix = "https://";
	private static String httpPrefix = "http://";

	private static final class PrivateConstants {
		public static final String appPath = "/flese/";
		public static final String appPathInTest = "/fleseTest/";
	}

	public static String getAppUrl(String requestUrl, String serverName) {
		if ((appUrl == null) || ("".equals(appUrl))) {
			computeAppUrl(requestUrl, serverName);
		}

		if (appUrl == null)
			appUrl = ""; // Better an empty stream than a null pointer !!!
		return appUrl;
	}

	private static synchronized void computeAppUrl(String requestUrl, String serverName) {
		if ((appUrl == null) || ("".equals(appUrl))) {
			// String requestUrl = request.getRequestURL().toString();
			// String queryString = request.getQueryString(); // d=789
			// String serverName = request.getServerName()

			if (requestUrl != null) {
				// appPath = http://.../page
				Integer index = requestUrl.lastIndexOf(getAppPath(requestUrl));
				appUrl = requestUrl.substring(0, index);
			}

			if ((serverName == null) || (!("localhost".equals(serverName)))) {
				if (!appUrl.startsWith(httpsPrefix)) {
					if (appUrl.startsWith(httpPrefix)) {
						appUrl = appUrl.substring(httpPrefix.length());
					}

					while (appUrl.startsWith("/")) {
						appUrl = appUrl.substring(1);
					}

					appUrl = httpsPrefix + appUrl;
				}
			}
		}
	}

	public static String getAppPath() {
		if (appPath == null) {
			return PrivateConstants.appPath;
		}
		return appPath;
	}

	public static String getAppPath(HttpServletRequest request) {
		if (appPath == null) {
			computeAppPath(request);
		}
		return getAppPath();
	}
	
	public static String getAppPath(String requestUrl) {
		if (appPath == null) {
			computeAppPath(requestUrl);
		}
		return getAppPath();
	}

	private static synchronized void computeAppPath(HttpServletRequest request) {
		if (request == null) {
			return;
		}
		StringBuffer requestUrlSB = request.getRequestURL();
		if (requestUrlSB == null) {
			return;
		}
		
		String requestUrl = requestUrlSB.toString();
		computeAppPath(requestUrl);
	}
	
	private static synchronized void computeAppPath(String requestUrl) {
		if (appPath != null) {
			return;
		}
		
		if ((requestUrl == null) || "".equals(requestUrl)) {
			return;
		}

		if (requestUrl.contains(PrivateConstants.appPath)) {
			appPath = PrivateConstants.appPath;
			return;
		}
		if (requestUrl.contains(PrivateConstants.appPathInTest)) {
			appPath = PrivateConstants.appPathInTest;
			return;
		}
	}

}
