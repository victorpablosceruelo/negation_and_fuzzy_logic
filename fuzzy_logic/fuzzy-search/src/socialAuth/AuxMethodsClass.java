package socialAuth;

import java.io.IOException;
import java.util.Enumeration;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class AuxMethodsClass {

	public static void log_request_parameters(HttpServletRequest request, Log LOG) throws IOException {
		// Get the values of all request parameters
		Enumeration<String> parametersEnum = request.getParameterNames();
		String parameterName;

		while (parametersEnum.hasMoreElements()) {
			// Get the name of the request parameter
			parameterName = parametersEnum.nextElement().toString();
			LOG.info("Parameter name: " + parameterName);
			
			String[] values = request.getParameterValues(parameterName);
			for (int i=0; i<values.length; i++) {
				LOG.info("Parameter name and value: " + parameterName + ": " + values[i]);
//				System.out.print(values[i]);
//				System.out.print(", ");
			}

			// LOG.info(to_be_printed);
		}
		LOG.info("<end of attributes list>");
	}
	
	public static String request_parameters(HttpServletRequest request, Log LOG) {
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
//				System.out.print(values[i]);
//				System.out.print(", ");
			}

			// LOG.info(to_be_printed);
		}
		LOG.info("<end of attributes list>");
		return retString;
	}
	
	public static String getAppUrlFromRequest(HttpServletRequest request) {
	    String requestUrl = request.getRequestURL().toString();
	    String queryString = request.getQueryString();   // d=789
	    
	    if (requestUrl != null) {
	    	System.out.print("getUrlFromRequest: requestUrl: " + requestUrl);
	    }
	    if (queryString != null) {
	    	System.out.print("getUrlFromRequest: queryString: " + queryString);
	    }

	    if (requestUrl != null) {
	    	Integer index = requestUrl.lastIndexOf("/"); // http:// ... /page
	    	String appUrl = requestUrl.substring(0, index);
	    	if (appUrl != null) {
	    		System.out.print("getUrlFromRequest: appUrl: " + appUrl);
	    		return appUrl;
	    	}
	    }
	    return ""; // Better an empty stream than a null pointer !!!
	}
	
	public static void redirectToAuthenticationIndex(HttpServletRequest request, HttpServletResponse response) throws IOException {
		String appUrl = getAppUrlFromRequest(request);
		response.sendRedirect( appUrl + "/index-authentication.jsp");
	}
	
	public static void redirectToSearch(HttpServletRequest request, HttpServletResponse response) throws IOException {
		String appUrl = getAppUrlFromRequest(request);
		response.sendRedirect( appUrl + "/search-menu.jsp" );
	}
	
	public static void redirectToLogout(HttpServletRequest request, HttpServletResponse response) throws IOException {
		String appUrl = getAppUrlFromRequest(request);
		response.sendRedirect( appUrl + "/SocialAuthLogInAndOutServlet?mode=logout" );
	}
	
	public static void redirectToError(HttpServletRequest request, HttpServletResponse response) throws IOException {
		final Log LOG = LogFactory.getLog(SocialAuthUpdateStatusServlet.class);
		AuxMethodsClass.log_request_parameters(request, LOG);
		String retString = AuxMethodsClass.request_parameters(request, LOG);

		String appUrl = getAppUrlFromRequest(request);
		response.sendRedirect( appUrl + "/error.jsp" + retString );
	}
	
	public static void forward_to(String where, HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		final Log LOG = LogFactory.getLog(SocialAuthUpdateStatusServlet.class);
		LOG.info("forward_to " + where);
		RequestDispatcher dispatcher = request.getRequestDispatcher(where);
		dispatcher.forward(request, response);
	}
	
}
