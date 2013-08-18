package urls;

import constants.KConstants;

public class AppUrl {

	private static String appUrl = null;
	private static String httpsPrefix = "https://";
	private static String httpPrefix = "http://";

	public static String getAppUrl(String requestUrl, String serverName) {
		if ((appUrl == null) || ("".equals(appUrl))) {
			// String requestUrl = request.getRequestURL().toString();
			// String queryString = request.getQueryString(); // d=789
			// String serverName = request.getServerName()

			if (requestUrl != null) {
				Integer index = requestUrl.lastIndexOf(KConstants.appPath); // http://
																			// ...
																			// /page
				appUrl = requestUrl.substring(0, index + KConstants.appPath.length());
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

		if (appUrl == null)
			appUrl = ""; // Better an empty stream than a null pointer !!!
		return appUrl;
	}
}
