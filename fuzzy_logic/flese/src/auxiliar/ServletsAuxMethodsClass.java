package auxiliar;

import java.util.Enumeration;

import javax.servlet.RequestDispatcher;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class ServletsAuxMethodsClass {

	final static Log LOG = LogFactory.getLog(QueryConversorClass.class);
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
				actionOnExceptionAux(ServletsAuxMethodsClass.SignOutRequest, "", e, request, response, LOG);
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
	

	public static void logRequestParameters(HttpServletRequest request, Log LOG) {
		if ((request != null) && (LOG != null)) {
			// Get the values of all request parameters
			Enumeration<String> parametersEnum = request.getParameterNames();
			String parameterName;
			String msg = "";
			LOG.info("---------------------------------------------------------------------------------------------");

			if (parametersEnum != null) {
				while (parametersEnum.hasMoreElements()) {
					// Get the name of the request parameter
					parameterName = (parametersEnum.nextElement()).toString();
					msg += "\n - Parameter name: " + parameterName;

					String[] values = request.getParameterValues(parameterName);
					if (values != null) {
						for (int i=0; i<values.length; i++) {
							msg += "\n   Parameter name and value: " + parameterName + "[" + i + "]: " + values[i];
						}
					}
					else msg += "\n   Parameter values is null.";
				}
				LOG.info(msg + "\n - <END of parameters list>");
			}
			else LOG.info("parametersEnum is null.");
			LOG.info("---------------------------------------------------------------------------------------------");
		}
		else {
			if (LOG != null) LOG.info("request is null.");
		}
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
	
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	public static final int theSamePage = 1;
	public static final int errorPage = 2;
	public static final int IndexPage = 3;
	
	public static final int SocialAuthenticationCallBackRequest = 4;
	public static final int SignInRequest = 5;
	public static final int SignOutRequest = 6;
	public static final int SignedInAnswer = 7;
	public static final int SignedOutAnswer = 8;
	public static final int UserOptionsRequest = 9;
	public static final int UserOptionsAnswer = 10;
	
	public static final int FilesListRequest = 11;
	public static final int FilesListAnswer = 12;
	public static final int FileUploadRequest = 13;
	public static final int FileUploadAnswer = 14;
	public static final int FileViewRequest = 15;
	public static final int FileViewAnswer = 16;
	public static final int FileDownloadRequest = 17;
	public static final int FileDownloadAnswer = 18;
	public static final int FileRemoveRequest = 19;
	public static final int FileRemoveAnswer = 20;
	
	public static final int ProgramFileIntrospectionRequest = 21;
	public static final int ProgramFileIntrospectionAnswer = 22;
	public static final int RunQueryRequest = 23;
	public static final int RunQueryAnswer = 24;

	public static final int ListProgramFuzzificationsRequest = 25;
	public static final int ListProgramFuzzificationsAnswer = 26;	
	public static final int SaveProgramFuzzificationRequest  = 27;
	public static final int SaveProgramFuzzificationAnswer  = 28;
	
	public static String [] [] urlsMappings() {
		String [][] urlsMappings = new String[29][2];
		
		urlsMappings[0] = new String[]{null, null};
		urlsMappings[theSamePage] = new String[]{"theSamePage", ""};
		urlsMappings[errorPage] = new String[]{"errorPage", "error.jsp"};
		urlsMappings[IndexPage] = new String[]{"IndexPage", "index.jsp"};
		
		urlsMappings[SocialAuthenticationCallBackRequest] = new String[]{"SocialAuthenticationCallBackRequest", "SocialAuthCallBackServlet"};
		urlsMappings[SignInRequest] = new String[]{"SignInRequest", "SocialAuthCallBackServlet?op=signin"};
		urlsMappings[SignOutRequest] = new String[]{"SignOutRequest", "SocialAuthCallBackServlet?op=signout"};
		urlsMappings[SignedInAnswer] = new String[]{"SignedInAnswer", "WEB-INF/signedIn.jsp"};
		urlsMappings[SignedOutAnswer] = new String[]{"SignedOutAnswer", "WEB-INF/signedOut.jsp"};
		urlsMappings[UserOptionsRequest] = new String[]{"UserOptionsRequest", "DispatcherServlet?op=userInfo"};
		urlsMappings[UserOptionsAnswer] = new String[]{"UserOptionsAnswer", "WEB-INF/userOptions.jsp"};
		
		urlsMappings[FilesListRequest] = new String[]{"FilesListRequest", "DispatcherServlet?op=filesList"};
		urlsMappings[FilesListAnswer] = new String[]{"FilesListAnswer", "WEB-INF/filesList.jsp"};
		urlsMappings[FileUploadRequest] = new String[]{"FileUploadRequest", "DispatcherServlet?op=fileUpload"};
		urlsMappings[FileUploadAnswer] = new String[]{"FileUploadAnswer", "WEB-INF/fileUpload.jsp"};
		urlsMappings[FileViewRequest] = new String[]{"FileViewRequest", "DispatcherServlet?op=fileView"};
		urlsMappings[FileViewAnswer] = new String[]{"FileViewAnswer", "WEB-INF/fileView.jsp"};
		urlsMappings[FileDownloadRequest] = new String[]{"FileDownloadRequest", "DispatcherServlet?op=fileDownload"};
		urlsMappings[FileDownloadAnswer] = new String[]{"FileDownloadAnswer", "WEB-INF/fileDownload.jsp"};
		urlsMappings[FileRemoveRequest] = new String[]{"FileRemoveRequest", "DispatcherServlet?op=fileRemove"};
		urlsMappings[FileRemoveAnswer] = new String[]{"FileRemoveAnswer", "WEB-INF/fileRemove.jsp"};
		
		urlsMappings[ProgramFileIntrospectionRequest] = new String[]{"ProgramFileIntrospectionRequest", "DispatcherServlet?op=programFileIntrospection"};
		urlsMappings[ProgramFileIntrospectionAnswer] = new String[]{"ProgramFileIntrospectionAnswer", "WEB-INF/programFileIntrospection.jsp"};
		urlsMappings[RunQueryRequest] = new String[]{"RunQueryRequest", "DispatcherServlet?op=runQuery"};
		urlsMappings[RunQueryAnswer] = new String[]{"RunQueryAnswer", "WEB-INF/runQuery.jsp"};
		
		urlsMappings[ListProgramFuzzificationsRequest] = new String[]{"ListProgramFuzzificationsRequest", "DispatcherServlet?op=listProgramFuzzifications"};
		urlsMappings[ListProgramFuzzificationsAnswer] = new String[]{"ListProgramFuzzificationsAnswer", "WEB-INF/runQuery.jsp"};
		urlsMappings[SaveProgramFuzzificationRequest] = new String[]{"SaveProgramFuzzificationRequest", "DispatcherServlet?op=saveProgramFuzzification"};
		urlsMappings[SaveProgramFuzzificationAnswer] = new String[]{"SaveProgramFuzzificationAnswer", "WEB-INF/runQuery.jsp"};
		
		return urlsMappings;
	}
	
	/**
	 * Returns the app mapping for uriNickName.
	 * @param UriNickName is the uri Nick Name.
	 * @return the app mapping nickname introduced.
	 * @throws Exception when the nick name is unknown.
	 */
	private static String appMappingForUriNickName(int uriNickName) throws Exception {

		String [] [] urlsMappings = urlsMappings();

		if ((uriNickName > urlsMappings.length) || 
			(urlsMappings[uriNickName][0] == null) ||
			(urlsMappings[uriNickName][1] == null) ||
			(("".equals(urlsMappings[uriNickName][1])) && (uriNickName != theSamePage))) {
			throw new Exception("Unknown UriNickName: " + uriNickName);
		}
		// LOG.info("uriNickName: " +uriNickName+ " -> " + retVal);
		return urlsMappings[uriNickName][1];
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
