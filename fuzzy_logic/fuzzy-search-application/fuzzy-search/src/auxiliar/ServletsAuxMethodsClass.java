package auxiliar;

import java.io.IOException;
import java.util.Enumeration;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
//import org.apache.commons.logging.LogFactory;

public class ServletsAuxMethodsClass {
	
	public static Boolean client_session_is_not_authenticated(HttpSession session) {
		return ((session == null) || (session.getAttribute("authenticated") == null) || (! (Boolean) session.getAttribute("authenticated")));
	}

	public static void log_request_parameters(HttpServletRequest request, Log LOG) throws IOException {
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
	
	public static String getAppUrlFromRequest(HttpServletRequest request, Log LOG) {
	    String requestUrl = request.getRequestURL().toString();
	    String queryString = request.getQueryString();   // d=789
	    
	    if (LOG != null) {
	    	if (requestUrl != null) {
	    		LOG.info("getUrlFromRequest: requestUrl: " + requestUrl);
	    	}
	    	if (queryString != null) {
	    		LOG.info("getUrlFromRequest: queryString: " + queryString);
	    	}
	    }
	    
	    if (requestUrl != null) {
	    	Integer index = requestUrl.lastIndexOf("/"); // http:// ... /page
	    	String appUrl = requestUrl.substring(0, index);
	    	if (appUrl != null) {
	    		if (LOG != null) {
	    			LOG.info("getUrlFromRequest: appUrl: " + appUrl);
	    		}
	    		return appUrl;
	    	}
	    }
	    return ""; // Better an empty stream than a null pointer !!!
	}
	
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	
	public static void goToAppIndex(HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws ServletException, IOException {
		ServletsAuxMethodsClass.forward_to("/IndexServlet", request, response, LOG);
	}	
	
	public static void goToAuthenticationSignin(HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws ServletException, IOException {
		ServletsAuxMethodsClass.forward_to("/SocialAuthServlet?mode=signin", request, response, LOG);
	}	
	
	public static void goToAuthenticationSignout(HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws ServletException, IOException {
		ServletsAuxMethodsClass.forward_to("/SocialAuthServlet?mode=signout", request, response, LOG);
	}

	public static void goToDataBasesMenu(HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws ServletException, IOException {
		// ServletsAuxMethodsClass.forward_to("/SearchServlet", request, response, LOG);
		ServletsAuxMethodsClass.redirect_to("/DataBasesMenuServlet", request, response, LOG);
	}
	
	public static void goToDataBaseQuery(HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws ServletException, IOException {
		// ServletsAuxMethodsClass.forward_to("/SearchServlet", request, response, LOG);
		ServletsAuxMethodsClass.redirect_to("/DataBaseQueryServlet", request, response, LOG);
	}
	
	public static void goToError(HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws ServletException, IOException {
		ServletsAuxMethodsClass.forward_to("/error.jsp", request, response, LOG);
	}
	
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	
	public static void forward_to(String where, HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws ServletException, IOException {
		if (LOG != null) {
			LOG.info("forward_to " + where);
		}
		RequestDispatcher dispatcher = request.getRequestDispatcher(where);
		dispatcher.forward(request, response);
	}
	
	public static void redirect_to(String where, HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws IOException 
		{
		if (LOG != null) {
			LOG.info("redirect_to " + where);
		}
		String appUrl = ServletsAuxMethodsClass.getAppUrlFromRequest(request, LOG);
		response.sendRedirect( appUrl + where);
	}
	
}
