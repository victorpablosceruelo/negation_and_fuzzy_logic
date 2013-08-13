package auxiliar;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;

import javax.servlet.RequestDispatcher;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import constants.KConstants;
import constants.UrlMappingClass;
import constants.UrlMappingsClass;


public class ServletsAuxMethodsClass {

	final static Log LOG = LogFactory.getLog(ServletsAuxMethodsClass.class);
	private final static String appPath = "flese/";
	private static String appUrl = null;
	
	/**
	 * Checks if an user name is valid.
	 * 
	 * @param     localUserName is the name of the user that we are checking.
	 * @exception LocalUserNameFixesClassException if localUserName is empty, null or invalid.
	 */
	public static boolean checkUserNameIsValid(String userName) throws Exception {

		if (userName == null) throw new Exception("userName is null"); 
		if ("".equals(userName)) throw new Exception("userName is empty"); 
		if (userName.contains("\\s")) throw new Exception("userName contains \\s");
		if (userName.contains("\\@")) throw new Exception("userName contains \\@");
		if (userName.contains("\\.")) throw new Exception("userName contains \\.");
		if (userName.contains("/")) throw new Exception("userName contains /.");
		
		return true;
	}

	/**
	 * Executes default actions when an exception is thrown.
	 * @param where is the where we are redirected whe an exception occurs.
	 * @param addToUri is the string added to the where, for adding additional parameters to the request.
	 * @param e is the exception thrown.
	 * @param request is the HttpServletRequest.
	 * @param response is the HttpServletResponse.
	 * @param LOG is the Log facility. Can be null.
	 */
	static public void actionOnException(int where, String addToUri, Exception e, HttpServletRequest request, HttpServletResponse response, Log LOG) {
		try {
			actionOnExceptionAux(where, addToUri, e, request, response, LOG);
		}
		catch (java.lang.IllegalStateException e1) {
			LOG.error("-------------------------------------------------------------------");
			LOG.error("Exception thrown inside actionOnException: " + e);
			e.printStackTrace();
			LOG.error("-------------------------------------------------------------------");
		}
		catch (Exception e2) {
			LOG.error("-------------------------------------------------------------------");
			LOG.error("Exception thrown inside actionOnException: " + e);
			e.printStackTrace();
			LOG.error("-------------------------------------------------------------------");
			
			try {
				actionOnExceptionAux(UrlMappingsClass.Pages.SignOutRequest, "", e, request, response, LOG);
			}
			catch (Exception e3) {
				LOG.error("-------------------------------------------------------------------");
				LOG.error("Exception thrown inside actionOnException: " + e);
				e.printStackTrace();
				LOG.error("-------------------------------------------------------------------");
			}
		}
	}
	
	static private void actionOnExceptionAux(int where, String addToUri, Exception e, HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws Exception {
		if (e != null) {
			if (LOG != null) {
				LOG.error("Exception thrown: " + e);
				LOG.error("Exception trace: ");
			}
			e.printStackTrace();
			if (e.getMessage() != null) {
				ServletsAuxMethodsClass.addMessageToTheUser(request, e.getMessage(), LOG);
			}
			else {
				ServletsAuxMethodsClass.addMessageToTheUser(request, e.toString(), LOG);
			}
		}
		else ServletsAuxMethodsClass.addMessageToTheUser(request, "Internal problem: thrown exception is null.", LOG);
		
		if (request == null) throw new Exception("request is null.");
		if (response == null) throw new Exception("response is null.");
		ServletsAuxMethodsClass.forward_to(where, addToUri, request, response, LOG);
	}

	/**
	 * Adds a msg to the request session attribute msgs.
	 * 
	 * @param request is the HttpServletRequest
	 * @param msg is the message to be added. Cannot be null.
	 * @param LOG is the servlet logging facility. Can be null (but it is not recommended).
	 */
	public static void addMessageToTheUser(HttpServletRequest request, String msg, Log LOG) {
		if (request != null) {
			String [] currentMsgs = (String[]) request.getAttribute("msgs");
			String [] newMsgs;
			if (currentMsgs != null) {
				newMsgs = new String[currentMsgs.length +1];
				for (int i=0; i<currentMsgs.length; i++) {
					newMsgs[i] = currentMsgs[i];
				}
				newMsgs[currentMsgs.length] = msg;
				// Remove the old messages array.
				request.removeAttribute("msgs");
			}
			else {
				newMsgs = new String[1];
				newMsgs[0] = msg;
			}
			// Save the new messages array.
			request.setAttribute("msgs", newMsgs);
			// Log
			if (LOG != null) LOG.info("Added to the msgs session attribute in the request the msg \'" + msg + "\'");
		}
		else {
			if (LOG != null) LOG.error("request is null. ERROR");
		}
	}
	
	public static String getCurrentDate () {
		DateFormat dateFormat = new SimpleDateFormat("yyyyMMdd_HHmmss");
		Date date = new Date();
		// System.out.println(dateFormat.format(date));
		return (dateFormat.format(date));
	}
	
	public static String requestParametersToString(HttpServletRequest request) {
		
		String result = "\n ";
		
		if ((request != null) && (LOG != null)) {
			// Get the values of all request parameters
			Enumeration<String> parametersEnum = request.getParameterNames();
			String parameterName;

			if (parametersEnum != null) {
				while (parametersEnum.hasMoreElements()) {
					// Get the name of the request parameter
					parameterName = (parametersEnum.nextElement()).toString();
					result += "\n - Values for parameter: " + parameterName + " :: ";

					String[] values = request.getParameterValues(parameterName);
					if (values != null) {
						for (int i=0; i<values.length; i++) {
							result += "[" + i + "]: " + values[i];
						}
					}
					else result += "values list is null.";
				}
				result += "\n - <END>";
			}
			else result += "\n - parametersEnum is null.";
		}
		else {
			result += "\n - request is null.";
		}
		result += "\n";
		return result;
	}
	
	public static String requestParametersToString (HttpServletRequest request, Log LOG) {
		Enumeration<String> parametersEnum = request.getParameterNames();
		String parameterName;
		String retString = "";
		Integer counter = 0;

		while (parametersEnum.hasMoreElements()) {
			// Get the name of the request parameter
			parameterName = parametersEnum.nextElement().toString();
			LOG.info("Parameter name: " + parameterName);
			
			String[] values = request.getParameterValues(parameterName);
			for (int i=0; i<values.length; i++) {
				LOG.info("Parameter name and value: " + parameterName + ": " + values[i]);
				if (counter==0) {
					retString += "?";
				}
				else {
					retString += "&";
				}
				retString += parameterName+"="+values[i];
				counter++;
			}
		}
		LOG.info("<end of attributes list>");
		return retString;
	}
	
	private static String getAppUrlFromRequest(HttpServletRequest request, Log LOG) {
		
	    if ((appUrl == null) || ("".equals(appUrl))) { 
		    String requestUrl = request.getRequestURL().toString();
		    // String queryString = request.getQueryString();   // d=789
		    		    
	    	if (requestUrl != null) {
	    		Integer index = requestUrl.lastIndexOf(appPath); // http:// ... /page
	    		appUrl = requestUrl.substring(0, index + appPath.length());
	    	}
	    	
	    	if ((request.getServerName() == null) || (! ("localhost".equals(request.getServerName())))) {
	    		String httpsPrefix = "https://";
	    		String httpPrefix = "http://";
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
		    	if (requestUrl != null) logMsg += "\n getUrlFromRequest: requestUrl: " + requestUrl;
		    	// if (queryString != null) logMsg += "\n getUrlFromRequest: queryString: " + queryString;
	    		if (appUrl != null) logMsg += "\n getUrlFromRequest: appUrl: " + appUrl;
	    		LOG.info(logMsg);
		    }

	    }
	    
	    if (appUrl == null) appUrl = ""; // Better an empty stream than a null pointer !!!
	    return appUrl;
	}
	

	
	/**
	 * Returns the app mapping for uriNickName.
	 * @param UriNickName is the uri Nick Name.
	 * @return the app mapping nickname introduced.
	 * @throws Exception when the nick name is unknown.
	 */
	private static String appMappingForUriNickName(int uriNickName) throws Exception {

		UrlMappingClass urlsMapping = UrlMappingsClass.getMapping(uriNickName);

		if ((urlsMapping == null) ||
			(urlsMapping.getKeyString() == null) ||
			(urlsMapping.getUrl() == null) ||
			(("".equals(urlsMapping.getUrl())) && (uriNickName != UrlMappingsClass.Pages.TheSamePage))) {
			throw new Exception("Unknown UriNickName: " + uriNickName);
		}
		// LOG.info("uriNickName: " +uriNickName+ " -> " + retVal);
		return urlsMapping.getUrl();
	}
	
	public static int uriNickNameForOpValue (String op) throws Exception {
		
		if (op == null) throw new Exception("op value cannot be null.");
		if ("".equals(op)) throw new Exception("op value cannot be empty string.");
		
		UrlMappingClass [] urlsMappings = urlsMappings();
		boolean found = false;
		int i = 0;
		while ((! found) && (i < urlsMappings.length)) {
			if (urlsMappings[i].getOpValue().equals(op)) found = true;
			else i++;
		}
		if (! found) throw new Exception("op value is not recognized. opValue: " + op); 
		return i;
	}
	
	/**
	 * Returns the full url for the nickname introduced.
	 * @param UriNickName
	 * @param request is the HttpServletRequest
	 * @param LOG is the Log facility (can be null).
	 * @return the full url for the nickname introduced.
	 * @throws Exception when the nick name is unknown.
	 */
	public static String getFullPathForUriNickName(int UriNickName, HttpServletRequest request, Log LOG) 
			throws Exception {
		String appUrl = ServletsAuxMethodsClass.getAppUrlFromRequest(request, LOG);
		String nickNameUrl = ServletsAuxMethodsClass.appMappingForUriNickName(UriNickName);
		String url = "";
		if (! "".equals(nickNameUrl)) url = appUrl + nickNameUrl;
		if (LOG != null) LOG.info("url: " + url);
		return url;
	}
	
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	
	/**
	 * Server side change of the requested page into a new one.
	 * It keeps the current context so you can share the same session.
	 * 
	 * @param where is the new page to which we are going.
	 * @param addToUri is the string added to the where, for adding additional parameters to the request.
	 * @param request is the servlet request parameter.
	 * @param response is the servlet response parameter.
	 * @param LOG is the LOG object (can be null).
	 * @throws Exception when the nick name is unknown.
	 */
	public static void forward_to(int where, String addToUri, HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws Exception {
		String url = appMappingForUriNickName(where);
		if (addToUri != null) url += addToUri;
		if (LOG != null) LOG.info("forwarding_to: " + url);
		RequestDispatcher dispatcher = request.getRequestDispatcher(url);
		dispatcher.forward(request, response);
	}
	
	/**
	 * Client side change of the requested page into a new one.
	 * It generates a different context so you can not share the same session.
	 * 
	 * @param where is the new page to which we are going.
	 * @param addToUri is the string added to the where, for adding additional parameters to the request.
	 * @param request is the servlet request parameter.
	 * @param response is the servlet response parameter.
	 * @param LOG is the LOG object (can be null).
	 * @throws Exception when the nick name is unknown or the operation is not possible.
	 */
	public static void redirect_to(int where, String addToUri, HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws Exception {
		String url = getFullPathForUriNickName(where, request, LOG);
		if (addToUri != null) url += addToUri;
		if (LOG != null) LOG.info("redirecting_to: " + url);
		response.sendRedirect( url );
	}
	
	/**
	 * Client side change of the requested page into a new one.
	 * It keeps the context by using encodeRedirectURL so you can not share the same session.
	 * 
	 * @param where is the new page to which we are going.
	 * @param addToUri is the string added to the where, for adding additional parameters to the request.
	 * @param request is the servlet request parameter.
	 * @param response is the servlet response parameter.
	 * @param LOG is the LOG object (can be null).
	 * @throws Exception when the nick name is unknown or the operation is not possible.
	 */
	public static void redirect_to_with_session(int where, String addToUri, HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws Exception {
		String url = getFullPathForUriNickName(where, request, LOG);
		if (addToUri != null) url += addToUri;
		if (LOG != null) LOG.info("encodeRedirectURL: redirecting_to: " + url);
		response.encodeRedirectURL( url );
	}
}
