package auxiliar;

import java.io.IOException;
import java.util.Date;
import java.util.Enumeration;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
//import org.apache.commons.logging.LogFactory;

public class ServletsAuxMethodsClass {

	public final static String appPath = "/fuzzy-search/";

	/**
	 * Tests if the client session has been authenticated.
	 * If it has not been then redirects the user client to the logout servlet.
	 * 
	 * @param request is the HttpServletRequest
	 * @param response is the HttpServletResponse
	 * @param LOG is the servlet logging facility. Can be null (but it is not recommended).
	 * @return true if it has been authenticated; false if not.
	 * @throws IOException if forward_to launches it.
	 * @throws ServletException if forward_to launches it.
	 */
	public static boolean clientSessionIsAuthenticated(HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws IOException, ServletException {
		if ((request != null) && (response != null)) {
			HttpSession session = request.getSession(false);

			if ((session == null) || (session.getAttribute("authenticated") == null) || 
					(! (Boolean) session.getAttribute("authenticated"))) {

				if (LOG != null) LOG.info("no session. logout.");
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.AuthenticationServletSignout, request, response, LOG);
				return false;
			}
			else {
				if (LOG != null) LOG.info("valid session. Session id: " + session.getId() + 
						" Creation Time" + new Date(session.getCreationTime()) + 
						" Time of Last Access" + new Date(session.getLastAccessedTime()));
				return true;
			}
		}
		else {
			LOG.error("request or response is null. ERROR");
			return false;
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
			session.setAttribute("msgs", newMsgs);
			if (LOG != null) LOG.error("Added to the msgs session attribute in the request the msg \'" + msg + "\'");
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
	    String requestUrl = request.getRequestURL().toString();
	    String queryString = request.getQueryString();   // d=789
	    
	    if (LOG != null) {
	    	if (requestUrl != null) LOG.info("getUrlFromRequest: requestUrl: " + requestUrl);
	    	if (queryString != null) LOG.info("getUrlFromRequest: queryString: " + queryString);
	    }
	    
	    if (requestUrl != null) {
	    	Integer index = requestUrl.lastIndexOf("/"); // http:// ... /page
	    	String appUrl = requestUrl.substring(0, index);
	    	if (appUrl != null) {
	    		if (LOG != null) LOG.info("getUrlFromRequest: appUrl: " + appUrl);
	    		return appUrl;
	    	}
	    }
	    return ""; // Better an empty stream than a null pointer !!!
	}
	
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	public static final int error_Page = 0;
	public static final int IndexServlet = 1;
	public static final int IndexPage = 2;
	public static final int AuthenticationServletSignin = 3;
	public static final int AuthenticationServletSignout = 4;
	public static final int AuthenticationServletSigned = 5;
	public static final int FilesMgmtServlet = 6;
	public static final int DataBasesMenu_Page = 7;
	public static final int DataBaseQueryServlet_Page = 8;
	public static final int DataBaseQuery_Page = 9;
	public static final int DataBaseQueryResults_Page = 10;
	public static final int UserInfoServlet_Page = 11;
	public static final int UserInfo_Page = 12;
	public static final int FileView_Page = 13;
	
	
	private static String appMappingForUriNickName(int UriNickName, Log LOG) {
		String retVal = null;
		switch (UriNickName) {
		case error_Page: retVal = "/error.jsp";
				break;
		case IndexServlet_Page: retVal = "/IndexServlet";
				break;
		case Index_Page: retVal = "/WEB-INF/appIndex.jsp";
				break;
		case AuthenticationServletSignin: retVal = "/SocialAuthServlet?op=signin";
				break;
		case AuthenticationServletSignout: retVal = "/SocialAuthServlet?op=signout";
				break;
		case AuthenticationServletSigned: retVal = "/SocialAuthServlet?op=signed";
				break;
		case DataBasesMenuServlet_Page: retVal = "/DataBasesMenuServlet";
				break;
		case DataBasesMenu_Page: retVal = "/WEB-INF/dataBasesMenu.jsp";
				break;
		case QueryServlet: retVal = "/DataBaseQueryServlet";
				break;
		case NormalQuery_Page: retVal = "/DataBaseQueryServlet";
				break;
		case DataBaseQuery_Page: retVal = "/WEB-INF/dataBaseQuery.jsp";
				break;
		case DataBaseQueryResults_Page: retVal = "/WEB-INF/dataBaseQueryResults.jsp";
				break;
		case UserInfoServlet_Page: retVal = "/UserInfoServlet";
				break;
		case UserInfo_Page: retVal = "/WEB-INF/userInfo.jsp";
				break;
		case FileView_Page: retVal = "/WEB-INF/fileView.jsp";
				break;
		default: retVal = "/error.jsp";
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
