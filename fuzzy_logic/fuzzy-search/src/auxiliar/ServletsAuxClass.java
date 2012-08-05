package auxiliar;

import java.io.IOException;
import java.util.Enumeration;

import javax.servlet.http.HttpServletRequest;
import org.apache.commons.logging.Log;

public class ServletsAuxClass {

	public static void show_request_parameters(HttpServletRequest request, Log LOG) throws IOException {
		// Get the values of all request parameters
		Enumeration<String> parametersEnum = request.getParameterNames();
		String attrName;

		while (parametersEnum.hasMoreElements()) {
			// Get the name of the request parameter
			attrName = parametersEnum.nextElement().toString();
			System.out.print(attrName);
			System.out.print(": ");
			
			String[] values = request.getParameterValues(attrName);
			for (int i=0; i<values.length; i++) {
				System.out.print(values[i]);
				System.out.print(", ");
			}

			// LOG.info(to_be_printed);
		}
		LOG.info("<end of attributes list>");
	}
	
	public static String getAppUrlFromRequest(HttpServletRequest request, Log LOG) {
	    String requestUrl = request.getRequestURL().toString();
	    String queryString = request.getQueryString();   // d=789
	    //if (queryString != null) {
	    //    requestUrl += "?"+queryString;
	    //}
	    LOG.info("getUrlFromRequest: requestUrl: " + requestUrl);
	    LOG.info("getUrlFromRequest: queryString: " + queryString);
	    return requestUrl;
	}
		
}
