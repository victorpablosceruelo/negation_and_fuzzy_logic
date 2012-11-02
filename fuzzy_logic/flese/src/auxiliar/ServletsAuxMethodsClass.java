package auxiliar;

import java.io.IOException;
import java.util.Enumeration;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.brickred.socialauth.AuthProvider;
//import org.apache.commons.logging.LogFactory;
import org.brickred.socialauth.SocialAuthManager;

public class ServletsAuxMethodsClass {

	private final static String appPath = "flese/";
	private static String appUrl = null;

	/**
	 * Tests if the client session has been authenticated.
	 * If it has not been then redirects the user client to the logout servlet.
	 * 
	 * @param request is the HttpServletRequest
	 * @param response is the HttpServletResponse
	 * @param LOG is the servlet logging facility. Can be null (but it is not recommended).
	 * @return true if it has been authenticated; false if not.
	 * @throws Exception 
	 */
	public static boolean clientSessionIsAuthenticated(HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws Exception {
		if (request == null) throw new Exception("request is null");
		if (response == null) throw new Exception("response is null");
		
		HttpSession session = request.getSession(false);
		if (session == null) throw new Exception("session is null");

		String testingMode = (String) session.getAttribute("testingMode");
		if (testingMode == null) {
			SocialAuthManager manager = (SocialAuthManager) session.getAttribute("authManager");
			if (manager == null) throw new Exception("manager is null");

			AuthProvider provider = (AuthProvider) session.getAttribute("provider");
			if (provider == null) throw new Exception("provider is null");
		}		
		return true;
	}

	/**
	 * Executes de default actions when an exception is thrown.
	 * @param e is the exception thrown.
	 * @param request is the HttpServletRequest.
	 * @param response is the HttpServletResponse.
	 * @param LOG is the Log facility.
	 */
	static public void actionOnException(Exception e, HttpServletRequest request, HttpServletResponse response, Log LOG) {
		LOG.error("Exception thrown: ");
		LOG.error(e);
		e.printStackTrace();
		ServletsAuxMethodsClass.addMessageToTheUser(request, e.getMessage(), LOG);
		try{
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.SocialAuthServletSignOut, request, response, LOG);
		}
		catch (Exception e2) {
			actionOnException(e, request, response, LOG);
		}
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
			HttpSession session = request.getSession(false);
			if (session != null) {
				String [] currentMsgs = (String []) session.getAttribute("msgs");
				String [] newMsgs;
				if (currentMsgs != null) {
					newMsgs = new String[currentMsgs.length +1];
					for (int i=0; i<currentMsgs.length; i++) {
						newMsgs[i] = currentMsgs[i];
					}
					newMsgs[currentMsgs.length] = msg;
				}
				else {
					newMsgs = new String[1];
					newMsgs[0] = msg;
				}
				session.removeAttribute("msgs");
				session.setAttribute("msgs", newMsgs);
				if (LOG != null) LOG.error("Added to the msgs session attribute in the request the msg \'" + msg + "\'");
			}
			else {
				if (LOG != null) LOG.error("Session is null.");
			}
		}
		else {
			if (LOG != null) LOG.error("request is null. ERROR");
		}
	}
	

	public static void logRequestParameters(HttpServletRequest request, Log LOG) throws IOException {
		// Get the values of all request parameters
		Enumeration<String> parametersEnum = request.getParameterNames();
		String parameterName;

		while (parametersEnum.hasMoreElements()) {
			// Get the name of the request parameter
			parameterName = (parametersEnum.nextElement()).toString();
			LOG.info("Parameter name: " + parameterName);
			
			String[] values = request.getParameterValues(parameterName);
			for (int i=0; i<values.length; i++) {
				LOG.info("Parameter name and value: " + parameterName + "[" + i + "]: " + values[i]);
			}
		}
		LOG.info("<end of attributes list>");
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
	    if (appUrl == null) {
		    String requestUrl = request.getRequestURL().toString();
		    String queryString = request.getQueryString();   // d=789
		    
		    if (LOG != null) {
		    	if (requestUrl != null) LOG.info("getUrlFromRequest: requestUrl: " + requestUrl);
		    	if (queryString != null) LOG.info("getUrlFromRequest: queryString: " + queryString);
		    }
		    
	    	if (requestUrl != null) {
	    		Integer index = requestUrl.lastIndexOf(appPath); // http:// ... /page
	    		appUrl = requestUrl.substring(0, index + appPath.length());
	    		if (appUrl != null) {
	    			if (LOG != null) LOG.info("getUrlFromRequest: appUrl: " + appUrl);
	    		}
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
	    }
	    
	    if (appUrl == null) return ""; // Better an empty stream than a null pointer !!!
	    else return appUrl;
	}
	
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	public static final int errorPage = 0;
	public static final int IndexPage = 1;
	public static final int SocialAuthServletSignIn = 2;
	public static final int SocialAuthServletSignOut = 3;
	public static final int SocialAuthServletUserInfo = 4;
	public static final int SocialAuthSignInPage = 5;
	public static final int SocialAuthUserInfoPage = 6;
	public static final int SocialAuthCallBackServlet = 7;
	public static final int FilesMgmtServlet = 8;
	public static final int FilesMgmtIndexPage = 9;
	public static final int FilesMgmtFileViewPage = 10;
	public static final int QueryServletBuildQuery = 11;
	public static final int QueryServletRunQuery = 12;
	public static final int BuildQueryPage = 13;
	public static final int RunQueryPage = 15;
	
	
	private static String appMappingForUriNickName(int UriNickName, Log LOG) {
		String retVal = null;
		switch (UriNickName) {
		case errorPage: retVal = "error.jsp";
				break;
		case IndexPage: retVal = "index.jsp";
				break;
		case SocialAuthServletSignIn: retVal = "SocialAuthServlet?op=signin";
				break;
		case SocialAuthServletSignOut: retVal = "SocialAuthServlet?op=signout";
				break;
		case SocialAuthServletUserInfo: retVal = "SocialAuthServlet?op=userInfo";
				break;
		case SocialAuthSignInPage: retVal = "WEB-INF/authentication.jsp";
				break;
		case SocialAuthUserInfoPage: retVal = "WEB-INF/authenticatedUserInfo.jsp";
				break;
		case SocialAuthCallBackServlet: retVal = "SocialAuthCallBackServlet";
				break;
		case FilesMgmtServlet: retVal = "FilesMgmtServlet";
				break;
		case FilesMgmtIndexPage: retVal = "WEB-INF/filesMgmt.jsp";
				break;
		case FilesMgmtFileViewPage: retVal = "WEB-INF/filesMgmtFileView.jsp";
				break;
		case QueryServletBuildQuery: retVal = "QueryServlet?op=buildQuery";
				break;
		case QueryServletRunQuery: retVal = "QueryServlet?op=runQuery";
				break;
		case BuildQueryPage: retVal = "WEB-INF/fleseBuildQuery.jsp";
				break;
		case RunQueryPage: retVal = "WEB-INF/fleseRunQuery.jsp";
				break;
		default: retVal = "error.jsp";
				break;
		}
		if (LOG != null) LOG.info("appURIsMappingFor " + UriNickName + " is " + retVal);
		return retVal;
	}
	
	/**
	 * Returns the full url for the nickname introduced.
	 * @param UriNickName
	 * @param request is the HttpServletRequest
	 * @param LOG is the Log facility (can be null).
	 * @return the full url for the nickname introduced.
	 */
	public static String getFullPathForUriNickName(int UriNickName, HttpServletRequest request, Log LOG) {
		String appUrl = ServletsAuxMethodsClass.getAppUrlFromRequest(request, LOG);
		return appUrl + appMappingForUriNickName(UriNickName, LOG);
	}
	
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	
	/**
	 * Server side change of the requested page into a new one.
	 * It keeps the current context so you can share the same session.
	 * 
	 * @param where is the new page to which we are going.
	 * @param request is the servlet request parameter.
	 * @param response is the servlet response parameter.
	 * @param LOG is the LOG object (can be null).
	 * @throws ServletException if getRequestDispatcher launches it
	 * @throws IOException if getRequestDispatcher launches it
	 */
	public static void forward_to(int where, HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws ServletException, IOException {
		String url = appMappingForUriNickName(where, LOG);
		if (LOG != null) LOG.info("forward_to: " + url);
		RequestDispatcher dispatcher = request.getRequestDispatcher(url);
		dispatcher.forward(request, response);
	}
	
	/**
	 * Client side change of the requested page into a new one.
	 * It generates a different context so you can not share the same session.
	 * 
	 * @param where is the new page to which we are going.
	 * @param request is the servlet request parameter.
	 * @param response is the servlet response parameter.
	 * @param LOG is the LOG object (can be null).
	 * @throws IOException if sendRedirect launches it.
	 */
	public static void redirect_to(int where, HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws IOException {
		String url = getFullPathForUriNickName(where, request, LOG);
		if (LOG != null) LOG.info("redirect_to: " + url);
		response.sendRedirect( url );
	}
	
	/**
	 * Client side change of the requested page into a new one.
	 * It keeps the context by using encodeRedirectURL so you can not share the same session.
	 * 
	 * @param where is the new page to which we are going.
	 * @param request is the servlet request parameter.
	 * @param response is the servlet response parameter.
	 * @param LOG is the LOG object (can be null).
	 * @throws IOException if sendRedirectURL launches it.
	 */
	public static void redirect_to_with_session(int where, HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws IOException {
		String url = getFullPathForUriNickName(where, request, LOG);
		
		if (LOG != null) LOG.info("encodeRedirectURL: " + url);
		response.encodeRedirectURL( url );
	}
}
