
package servlets;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

// import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import auxiliar.ServletsAuxMethodsClass;

@WebServlet("/SocialAuthUserInfoServlet")
public class SocialAuthUserInfoServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final Log LOG = LogFactory.getLog(SocialAuthUserInfoServlet.class);

	public void doGet(HttpServletRequest request, HttpServletResponse response) {
		LOG.info("--- doGet invocation ---");
		doGetAndDoPost(request, response);
		LOG.info("--- doGet end ---");
	}
	
	public void doPost(HttpServletRequest request, HttpServletResponse response) {
		LOG.info("--- doPost invocation ---");
		doGetAndDoPost(request, response);
		LOG.info("--- doPost end ---");	
	}
	
	private void doGetAndDoPost(HttpServletRequest request, HttpServletResponse response) {
		try {
			if (ServletsAuxMethodsClass.clientSessionIsAuthenticated(request, response, LOG)) {
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.SocialAuthUserInfoPage, request, response, LOG);
			}
		} catch (Exception e) {
			ServletsAuxMethodsClass.actionOnException(e, request, response, LOG);
		}
	}	
}




//// EOF


