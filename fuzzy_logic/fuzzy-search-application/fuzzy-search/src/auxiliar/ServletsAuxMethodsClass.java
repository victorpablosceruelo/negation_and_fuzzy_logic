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
	public static final int error_Page = 0;
	public static final int IndexServlet_Page = 1;
	public static final int Index_Page = 2;
	public static final int AuthenticationSignin_Page = 3;
	public static final int AuthenticationSignout_Page = 4;
	public static final int DataBasesMenuServlet_Page = 5;
	public static final int DataBasesMenu_Page = 6;
	public static final int DataBaseQueryServlet_Page = 7;
	public static final int DataBaseQuery_Page = 8;
	public static final int UserInfoServlet_Page = 9;
	public static final int UserInfo_Page = 10;
	
	private static String pagesDatabase(int NickName) {
		String retVal = null;
		switch (NickName) {
		case error_Page: retVal = "/error.jsp";
				break;
		case IndexServlet_Page: retVal = "/IndexServlet";
				break;
		case Index_Page: retVal = "/WEB-INF/appIndex.jsp";
				break;
		case AuthenticationSignin_Page: retVal = "/SocialAuthServlet?mode=signin";
				break;
		case AuthenticationSignout_Page: retVal = "/SocialAuthServlet?mode=signout";
				break;
		case DataBasesMenuServlet_Page: retVal = "/DataBasesMenuServlet";
				break;
		case DataBasesMenu_Page: retVal = "/WEB-INF/dataBasesMenu.jsp";
				break;
		case DataBaseQueryServlet_Page: retVal = "/DataBaseQueryServlet";
				break;
		case DataBaseQuery_Page: retVal = "/WEB-INF/dataBaseQuery.jsp";
				break;
		case UserInfoServlet_Page: retVal = "/UserInfoServlet";
				break;
		case UserInfo_Page: retVal = "/WEB-INF/userInfo.jsp";
				break;				
		default: retVal = "/error.jsp";
				break;
		}
				
		return retVal;
	}
	
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------------------
	
	public static void forward_to(int where, HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws ServletException, IOException {
		String realWebPageName = pagesDatabase(where);
		if (LOG != null) {
			LOG.info("forward_to " + where + " -> " + realWebPageName);
		}
		RequestDispatcher dispatcher = request.getRequestDispatcher(realWebPageName);
		dispatcher.forward(request, response);
	}
	
	public static void redirect_to(int where, HttpServletRequest request, HttpServletResponse response, Log LOG) 
			throws IOException {
		String realWebPageName = pagesDatabase(where);
		
		if (LOG != null) {
			LOG.info("redirect_to " + where + " -> " + realWebPageName);
		}
		String appUrl = ServletsAuxMethodsClass.getAppUrlFromRequest(request, LOG);
		response.sendRedirect( appUrl + realWebPageName);
	}
	
}
