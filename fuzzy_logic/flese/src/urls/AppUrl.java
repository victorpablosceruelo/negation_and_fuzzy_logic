package urls;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;

import constants.KConstants;

public class AppUrl {

	private static String appUrl = null;
	private static String httpsPrefix = "https://";
	private static String httpPrefix = "http://";
	
	public static String getAppUrl(HttpServletRequest request, Log LOG) {
	    if ((appUrl == null) || ("".equals(appUrl))) { 
		    String requestUrl = request.getRequestURL().toString();
		    // String queryString = request.getQueryString();   // d=789
		    		    
	    	if (requestUrl != null) {
	    		Integer index = requestUrl.lastIndexOf(KConstants.appPath); // http:// ... /page
	    		appUrl = requestUrl.substring(0, index + KConstants.appPath.length());
	    	}
	    	
	    	if ((request.getServerName() == null) || (! ("localhost".equals(request.getServerName())))) {
	    		if (! appUrl.startsWith(httpsPrefix)) {
	    			if (appUrl.startsWith(httpPrefix)) {
	    				appUrl = appUrl.substring(httpPrefix.length());
	    			}
	    			
	    			while (appUrl.startsWith("/")) {
	    				appUrl = appUrl.substring(1);
	    			}
	    			
	    			appUrl = httpsPrefix + appUrl;
	    		}
	    	}
	    	
		    if (LOG != null) {
		    	String logMsg = "";
		    	if (requestUrl != null) logMsg += "\n getAppUrl: requestUrl: " + requestUrl;
		    	// if (queryString != null) logMsg += "\n getUrlFromRequest: queryString: " + queryString;
	    		if (appUrl != null) logMsg += "\n getAppUrl: appUrl: " + appUrl;
	    		LOG.info(logMsg);
		    }

	    }
	    
	    if (appUrl == null) appUrl = ""; // Better an empty stream than a null pointer !!!
	    return appUrl;
	}
}
